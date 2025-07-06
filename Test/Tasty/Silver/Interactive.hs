{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Golden test management, interactive mode. Runs the tests, and asks
-- the user how to proceed in case of failure or missing golden standard.

module Test.Tasty.Silver.Interactive
  (
  -- * Command line helpers
    defaultMain
  , defaultMain1

  -- * The ingredient
  , interactiveTests
  , Interactive (..)

  -- * Programmatic API
  , runTestsInteractive
  , DisabledTests
  )
  where

import Prelude

import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.STM
import Control.Monad.State

import Data.Char
import Data.Maybe
import Data.Monoid    ( Any(..) )
import Data.Proxy
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ( Semigroup(..) )
#endif
import Data.Tagged
import Data.Text      ( Text )
import Data.Text.Encoding
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Options.Applicative hiding (Failure, Success)

import System.Console.ANSI
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Silently (silence)
import System.IO.Temp
import System.Process
import System.Process.ByteString as PS
import qualified System.Process.Text as ProcessText

import Text.Printf

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Runners
import Test.Tasty.Silver.Filter
import Test.Tasty.Silver.Interactive.Run
import Test.Tasty.Silver.Internal

type DisabledTests = TestPath -> Bool

-- | Like @defaultMain@ from the main tasty package, but also includes the
-- golden test management capabilities.

defaultMain :: TestTree -> IO ()
defaultMain = defaultMain1 []


defaultMain1 :: [RegexFilter] -> TestTree -> IO ()
defaultMain1 filters =
    defaultMainWithIngredients
        [ listingTests
        , interactiveTests (checkRF False filters)
        ]

-- | Option for interactive mode.

newtype Interactive = Interactive Bool
  deriving (Eq, Ord, Typeable)

instance IsOption Interactive where
  defaultValue   = Interactive False
  parseValue     = fmap Interactive . safeRead
  optionName     = return "interactive"
  optionHelp     = return "Run tests in interactive mode."
  optionCLParser = flagCLParser (Just 'i') (Interactive True)

data ResultType = RTSuccess | RTFail | RTIgnore
  deriving (Eq)

data FancyTestException
  = Mismatch GoldenResultI
  | Disabled
  deriving (Show, Typeable)

instance Exception FancyTestException

getResultType :: Result -> ResultType
getResultType (Result { resultOutcome = Success}) = RTSuccess
getResultType (Result { resultOutcome = (Failure (TestThrewException e))}) =
  case fromException e of
    Just Disabled -> RTIgnore
    _ -> RTFail
getResultType (Result { resultOutcome = (Failure _)}) = RTFail


interactiveTests :: DisabledTests
    -> Ingredient
interactiveTests dis = TestManager
    [ Option (Proxy :: Proxy Interactive)
    , Option (Proxy :: Proxy HideSuccesses)
    , Option (Proxy :: Proxy AnsiTricks)
    , Option (Proxy :: Proxy UseColor)
    , Option (Proxy :: Proxy NumThreads)
    , Option (Proxy :: Proxy ExcludeFilters)
    , Option (Proxy :: Proxy IncludeFilters)
    , Option (Proxy :: Proxy AcceptTests)
    ] $
  \opts tree ->
      Just $ runTestsInteractive dis opts (filterWithRegex opts tree)

runSingleTest ::  IsTest t => DisabledTests -> TestPath -> TestName -> OptionSet -> t -> (Progress -> IO ()) -> IO Result
runSingleTest dis tp _ _ _ _ | dis tp =
  return $ (testFailed "")
    { resultOutcome = (Failure $ TestThrewException $ toException Disabled) }
runSingleTest _ _ _ opts t cb = do
  case (cast t :: Maybe Golden) of
    Nothing -> run opts t cb
    Just g -> do
        (r, gr) <- runGolden g

        -- we may be in a different thread here than the main ui.
        -- force evaluation of actual value here, as we have to evaluate it before
        -- leaving this test.
        gr' <- forceGoldenResult gr
        case gr' of
            GREqual -> return r
            grd -> return $ r { resultOutcome = (Failure $ TestThrewException $ toException $ Mismatch grd) }

-- | A simple console UI.
runTestsInteractive :: DisabledTests -> OptionSet -> TestTree -> IO Bool
runTestsInteractive dis opts tests = do
  let tests' = wrapRunTest (runSingleTest dis) tests

  launchTestTree opts tests' $ \smap -> do
    isTerm <- hSupportsANSI stdout

    (\k -> if isTerm
      then (do hideCursor; k) `finally` showCursor
      else k) $ do

      hSetBuffering stdout NoBuffering

      let
        whenColor = lookupOption opts
        HideSuccesses hideSuccesses = lookupOption opts
        AnsiTricks ansiTricks = lookupOption opts

      let
        ?colors = useColor whenColor isTerm

      outp <- produceOutput opts tests

      stats <- case () of { _
        | hideSuccesses && isTerm && ansiTricks ->
            consoleOutputHidingSuccesses outp smap
        | hideSuccesses && not isTerm ->
            streamOutputHidingSuccesses outp smap
        | otherwise -> consoleOutput outp smap
      }

      return $ \time -> do
            printStatistics stats time
            return $ statFailures stats == 0


-- | Show diff using available external tools.

printDiff :: TestName -> GDiff -> IO ()
printDiff = showDiff_ False

-- | Like 'printDiff', but uses @less@ if available.

showDiff_ :: Bool -> TestName -> GDiff -> IO ()
showDiff_ _       _ Equal                   = error "Can't show diff for equal values."
showDiff_ True    n (ShowDiffed _ t)        = showInLess n t
showDiff_ False   _ (ShowDiffed _ t)        = TIO.putStrLn t
showDiff_ useLess n (DiffText _ tGold tAct) =
  ifM (doesCmdExist "wdiff" `and2M` haveColorDiff) colorDiff $ {-else-}
  ifM (doesCmdExist "git") gitDiff {-else-} noDiff
  where

  -- Display diff using `git diff`.
  gitDiff = do
    withDiffEnv n tGold tAct $ \ fGold fAct -> do
      -- Unless we use `less`, we simply call `git` directly.
      if not useLess
        then do
          (out, err) <- callGitDiff [ fGold, fAct ]
          TIO.putStrLn err
          TIO.putStrLn out
        else callCommand $ unwords
            [ "git"
            , unwords gitDiffArgs
            , "--color=always"
            , toSlashesFilename fGold
            , toSlashesFilename fAct
            , "| less -r > /dev/tty"
              -- Option -r: display control characters raw (e.g. sound bell instead of printing ^G).
              -- Thus, ANSI escape sequences will be interpreted as that.
              -- /dev/tty is "terminal where process started"  ("CON" on Windows?)
            ]

  -- Display diff using `wdiff | colordiff`.
  colorDiff = do
    withDiffEnv n tGold tAct $ \ fGold fAct -> do
      let cmd = unwords
            [ "wdiff"
            , toSlashesFilename fGold
            , toSlashesFilename fAct
            , "| colordiff"
              -- E.g.
            , if useLess then "| less -r > /dev/tty" else ""
              -- Option -r: display control characters raw (e.g. sound bell instead of printing ^G).
              -- Thus, ANSI escape sequences will be interpreted, e.g. as coloring.
              -- /dev/tty is "terminal where process started"  ("CON" on Windows?)
            ]
      ifM (doesCmdExist "colordiff")
        -- If `colordiff` is treated as executable binary, we do not indirect via `sh`,
        -- but can let the default shell do the piping for us.
        {-then-} (callCommand cmd)
        -- Otherwise, let `sh` do the piping for us.  (Needed e.g. for Cygwin.)
        {-else-} (callProcess "sh" [ "-c", cmd ])

      -- Alt:
      --   -- We have to pipe ourselves; don't use `less` then.
      --   callProcessText "wdiff" [fGold, fAct] T.empty >>=
      --     void . callProcessText "colordiff" []
      --   -- TODO: invoke "colordiff" through callCommand

    -- Newline if we didn't go through less
    unless useLess $ putStrLn ""

  -- No diff tool: Simply print both golden and actual value.
  noDiff = do
    putStrLn "`git diff` not available, cannot produce a diff."
    putStrLn "Golden value:"
    TIO.putStrLn tGold
    putStrLn "Actual value:"
    TIO.putStrLn tAct

-- | Call external tool @"git" 'gitDiffArgs'@ with given extra arguments, returning its output.
--   If @git diff@ prints to @stderr@ or returns a exitcode indicating failure, throw exception.

callGitDiff
  :: [String]
       -- ^ File arguments to @git diff@.
  -> IO (Text, Text)
       -- ^ @stdout@ and @stderr@ produced by the call.
callGitDiff args = do
  ret@(exitcode, stdOut, stdErr) <-
    ProcessText.readProcessWithExitCode
      "git" (gitDiffArgs ++ args) T.empty
  let done = return (stdOut, stdErr)
  case exitcode of
    ExitSuccess   -> done
    -- With option --no-index, exitcode 1 indicates that files are different.
    ExitFailure 1 -> done
    -- Other failure codes indicate that something went wrong.
    ExitFailure _ -> gitFailed $ show ret
  where
  gitFailed msg = fail $ "Call to `git diff` failed: " ++ msg

gitDiffArgs :: [String]
gitDiffArgs = [ "diff", "--no-index", "--text" ]

-- #16: filenames get mangled under Windows, backslashes disappearing.
-- We only use this function on names of tempfiles, which do not contain spaces,
-- so it should be enough to hackily replace backslashes by slashes.
-- | Turn backslashes to slashes, which can also be path separators on Windows.
toSlashesFilename :: String -> String
toSlashesFilename = map $ \ c -> case c of
  '\\' -> '/'
  c    -> c

-- | Look for a command on the PATH.  If @doesCmdExist cmd@, then
--   @callProcess cmd@ should be possible.
--
--   Note that there are OS-specific differences.
--   E.g. on @cygwin@, only binaries (@.exe@) are deemed to exist,
--   not scripts.  The latter also cannot be called directly with
--   @callProcess@, but need indirection via @sh -c@.
--   In particular, @colordiff@, which is a @perl@ script, is not
--   found by @doesCmdExist@ on @cygwin@.
--
--   On @macOS@, there isn't such a distinction, so @colordiff@
--   is both found by @doesCmdExist@ and can be run by @callProcess@.
doesCmdExist :: String -> IO Bool
doesCmdExist cmd = isJust <$> findExecutable cmd

-- | Since @colordiff@ is a script, it may not be found by 'findExecutable'
-- e.g. on Cygwin.  So we try also to find it using @which@.
haveColorDiff :: IO Bool
haveColorDiff = orM
  [ doesCmdExist "colordiff"
  , andM
    [ haveSh
    , silence $ exitCodeToBool <$> rawSystem "which" [ "colordiff" ]
    ]
  ]

exitCodeToBool :: ExitCode -> Bool
exitCodeToBool ExitSuccess   = True
exitCodeToBool ExitFailure{} = False

-- Stores the golden/actual text in two files, so we can use it for git diff.
withDiffEnv :: TestName -> T.Text -> T.Text -> (FilePath -> FilePath -> IO ()) -> IO ()
withDiffEnv n tGold tAct cont = do
  withSystemTempFile (n <.> "golden") $ \ fGold hGold -> do
    withSystemTempFile (n <.> "actual") $ \ fAct hAct -> do
      hSetBinaryMode hGold True
      hSetBinaryMode hAct True
      BS.hPut hGold (encodeUtf8 tGold)
      BS.hPut hAct (encodeUtf8 tAct)
      hClose hGold
      hClose hAct
      cont fGold fAct


printValue :: TestName -> GShow -> IO ()
printValue _ (ShowText t) = TIO.putStrLn t

showValue :: TestName -> GShow -> IO ()
showValue n (ShowText t) = showInLess n t

showInLess :: String -> T.Text -> IO ()
showInLess _ t = do
  ifNotM useLess
    {-then-} (TIO.putStrLn t)
    {-else-} $ do
      ret <- PS.readCreateProcessWithExitCode (shell "less > /dev/tty") $ encodeUtf8 t
      case ret of
        ret@(ExitFailure _, _, _) -> error $ show ret
        _ -> return ()

-- | Should we use external tool @less@ to display diffs and results?
useLess :: IO Bool
useLess = andM [ hIsTerminalDevice stdin, hSupportsANSI stdout, doesCmdExist "less" ]

-- | Is @sh@ available to take care of piping for us?
haveSh :: IO Bool
haveSh = doesCmdExist "sh"

-- | Ask user whether to accept a new golden value, and run action if yes.

tryAccept
  :: String   -- ^ @prefix@ printed at the beginning of each line.
  -> IO ()    -- ^ Action to @update@ golden value.
  -> IO Bool  -- ^ Return decision whether to update the golden value.
tryAccept prefix update = do
  -- Andreas, 2021-09-18
  -- Accepting by default in batch mode is not the right thing,
  -- because CI may then falsely accept broken tests.
  --
  -- --   If terminal is non-interactive, just assume "yes" always.
  -- termIsInteractive <- hIsTerminalDevice stdin
  -- if not termIsInteractive then do
  --   putStr prefix
  --   putStr "Accepting actual value as new golden value."
  --   update
  --   return True
  -- else do
    isANSI <- hSupportsANSI stdout
    when isANSI showCursor
    putStr prefix
    putStr "Accept actual value as new golden value? [yn] "
    let
      done b = do
        when isANSI hideCursor
        putStr prefix
        return b
      loop = do
        ans <- getLine
        case ans of
          "y" -> do update; done True
          "n" -> done False
          _   -> do
            putStr prefix
            putStrLn "Invalid answer."
            loop
    loop


--------------------------------------------------
-- TestOutput base definitions
--------------------------------------------------
-- {{{
-- | 'TestOutput' is an intermediary between output formatting and output
-- printing. It lets us have several different printing modes (normal; print
-- failures only; quiet).
data TestOutput
  = HandleTest
      {- test name, used for golden lookup #-} (TestName)
      {- print test name   -} (IO ())
      {- print test result -} (Result -> IO Statistics)
  | PrintHeading (IO ()) TestOutput
  | Skip
  | Seq TestOutput TestOutput

instance Semigroup TestOutput where
  (<>) = Seq

-- The monoid laws should hold observationally w.r.t. the semantics defined
-- in this module
instance Monoid TestOutput where
  mempty = Skip
  mappend = (<>)

type Level = Int

produceOutput :: (?colors :: Bool) => OptionSet -> TestTree -> IO TestOutput
produceOutput opts tree = do
  let
    -- Do not retain the reference to the tree more than necessary
    !alignment = computeAlignment opts tree
    Interactive isInteractive = lookupOption opts
    AcceptTests accept        = lookupOption opts
    -- We always print timing in non-interactive mode
    forceTime = not isInteractive
  -- In batch mode, we never use 'less' to show result.
  useLess <- if isInteractive then useLess else pure False

  let
    handleSingleTest
      :: (IsTest t, ?colors :: Bool)
      => OptionSet -> TestName -> t -> Ap (Reader Level) TestOutput
    handleSingleTest _opts name _test = Ap $ do
      level <- ask

      let
        align = replicate (alignment - indentSize * level - length name) ' '
        pref = indent level ++ replicate (length name) ' ' ++ "  " ++ align
        printTestName =
          printf "%s%s: %s" (indent level) name align

        printResultLine result = do
          -- use an appropriate printing function
          let
            resTy = getResultType result
            printFn = case resTy of
                RTSuccess -> ok
                RTIgnore -> warn
                RTFail -> failure
          case resTy of
            RTSuccess -> printFn "OK"
            RTIgnore -> printFn "DISABLED"
            RTFail -> printFn "FAIL"
          -- print time only if it's significant
          when (resultTime result >= 0.01 || forceTime) $
            printFn (printf " (%.2fs)" $ resultTime result)
          printFn "\n"

        possiblyAccept msgPass msgFail update = do
          isUpd <- if isInteractive then tryAccept pref update else do
            putStr pref
            when accept update
            pure accept
          let r =
                if isUpd
                then ( testPassed msgPass
                     , mempty { statCreatedGolden = 1 } )
                else ( testFailed msgFail
                     , mempty { statFailures = 1 } )
          printResultLine (fst r)
          return r

        handleTestResult result = do
          (result', stat') <- case resultOutcome result of
            Failure (TestThrewException e) ->
              case fromException e of

                Just (Mismatch (GRNoGolden (Identity a) shw (Just upd))) -> do
                  if isInteractive then do
                    printf "Golden value missing. Press <enter> to show actual value.\n"
                    _ <- getLine
                    showValue name =<< shw a
                  else do
                    infoFail $ printf "%sActual value is:\n" pref
                    hsep
                    printValue name =<< shw a
                    hsep
                  possiblyAccept "Created golden value." "Golden value missing." $
                    upd a

                Just (Mismatch (GRDifferent _ a diff (Just upd))) -> do
                  printf "Golden value differs from actual value.\n"
                  unless useLess hsep
                  showDiff_ useLess name diff
                  unless useLess hsep
                  possiblyAccept "Updated golden value." "Golden value does not match actual output." $
                    upd a

                Just (Mismatch (GRDifferent _ _ diff Nothing)) -> do
                  printResultLine result
                  infoFail $ printf "%sDiff between actual and golden value:\n" pref
                  hsep
                  printDiff name diff
                  hsep
                  return (testFailed "", mempty { statFailures = 1 })

                Just (Mismatch _) -> error "Impossible case!"
                Just Disabled -> do
                  printResultLine result
                  return ( result
                         , mempty { statDisabled = 1 } )
                Nothing -> do
                  printResultLine result
                  return (result, mempty {statFailures = 1})
            Success -> do
              printResultLine result
              return (result, mempty { statSuccesses = 1 })
            Failure _ -> do
              printResultLine result
              return (result, mempty { statFailures = 1 })

          let result'' = result' { resultTime = resultTime result }

          rDesc <- formatMessage $ resultDescription result''
          when (not $ null rDesc) $ (case getResultType result'' of
            RTSuccess -> infoOk
            RTIgnore -> infoWarn
            RTFail -> infoFail) $
              printf "%s%s\n" pref (formatDesc (level+1) rDesc)

          return stat'

      return $ HandleTest name printTestName handleTestResult

    handleGroup :: OptionSet -> TestName -> [Ap (Reader Level) TestOutput] -> Ap (Reader Level) TestOutput
    handleGroup _ name grp = Ap $ do
      level <- ask
      let
        printHeading = printf "%s%s\n" (indent level) name
        printBody = runReader (getApp $ mconcat grp) (level + 1)
      return $ PrintHeading printHeading printBody


  return $ flip runReader 0 $ getApp $
      foldTestTree
        trivialFold
          { foldSingle = handleSingleTest
#if MIN_VERSION_tasty(1,5,0)
          , foldGroup = \ opts name ts -> handleGroup opts name ts
#else
          , foldGroup = \ opts name t  -> handleGroup opts name [t]
#endif
          }
          opts tree

hsep :: IO ()
hsep = putStrLn (replicate 40 '=')

foldTestOutput
  :: (?colors :: Bool, Monoid b)
  => (IO () -> IO Result
    -> (Result -> IO Statistics)
    -> b)
  -> (IO () -> b -> b)
  -> TestOutput -> StatusMap -> b
foldTestOutput foldTest foldHeading outputTree smap =
  flip evalState 0 $ getApp $ go outputTree where
  go (HandleTest _ printName handleResult) = Ap $ do
    ix <- get
    put $! ix + 1
    let
      statusVar =
        fromMaybe (error "internal error: index out of bounds") $
        IntMap.lookup ix smap
      readStatusVar = getResultFromTVar statusVar
    return $ foldTest printName readStatusVar handleResult
  go (PrintHeading printName printBody) = Ap $
    foldHeading printName <$> getApp (go printBody)
  go (Seq a b) = mappend (go a) (go b)
  go Skip = mempty

-- }}}

--------------------------------------------------
-- TestOutput modes
--------------------------------------------------
-- {{{
consoleOutput :: (?colors :: Bool) => TestOutput -> StatusMap -> IO Statistics
consoleOutput outp smap =
  getApp . fst $ foldTestOutput foldTest foldHeading outp smap
  where
    foldTest printName getResult handleResult =
      (Ap $ do
        _ <- printName
        r <- getResult
        handleResult r
      , Any True)
    foldHeading printHeading (printBody, Any nonempty) =
      (Ap $ do
        when nonempty $ printHeading
        stats <- getApp printBody
        return stats
      , Any nonempty )

consoleOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO Statistics
consoleOutputHidingSuccesses outp smap =
  snd <$> (getApp $ foldTestOutput foldTest foldHeading outp smap)
  where
    foldTest printName getResult handleResult =
      Ap $ do
          _ <- printName
          r <- getResult
          if resultSuccessful r
            then do
                clearThisLine
                return (Any False, mempty { statSuccesses = 1 })
            else do
                stats <- handleResult r
                return (Any True, stats)

    foldHeading printHeading printBody =
      Ap $ do
        _ <- printHeading
        b@(Any failed, _) <- getApp printBody
        unless failed clearAboveLine
        return b

    clearAboveLine = do cursorUpLine 1; clearThisLine
    clearThisLine = do clearLine; setCursorColumn 0

streamOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> IO Statistics
streamOutputHidingSuccesses outp smap =
  snd <$> (flip evalStateT [] . getApp $
    foldTestOutput foldTest foldHeading outp smap)
  where
    foldTest printName getResult handleResult =
      Ap $ do
          r <- liftIO $ getResult
          if resultSuccessful r
            then return (Any False, mempty { statSuccesses = 1 })
            else do
              stack <- get
              put []

              stats <- liftIO $ do
                sequence_ $ reverse stack
                _ <- printName
                handleResult r

              return (Any True, stats)

    foldHeading printHeading printBody =
      Ap $ do
        modify (printHeading :)
        b@(Any failed, _) <- getApp printBody
        unless failed $
          modify $ \stack ->
            case stack of
              _:rest -> rest
              [] -> [] -- shouldn't happen anyway
        return b

-- }}}

--------------------------------------------------
-- Statistics
--------------------------------------------------
-- {{{

data Statistics = Statistics
  { statSuccesses :: !Int
  , statUpdatedGolden :: !Int
  , statCreatedGolden :: !Int
  , statFailures :: !Int
  , statDisabled :: !Int
  }

instance Semigroup Statistics where
  Statistics a1 b1 c1 d1 e1 <> Statistics a2 b2 c2 d2 e2 = Statistics (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2) (e1 + e2)


instance Monoid Statistics where
  mempty = Statistics 0 0 0 0 0
  mappend = (<>)

printStatistics :: (?colors :: Bool) => Statistics -> Time -> IO ()
printStatistics st time = do
  printf "\n"

  let total = statFailures st + statUpdatedGolden st + statCreatedGolden st + statSuccesses st

  when (statCreatedGolden st > 0) (printf "Created %d golden values.\n" (statCreatedGolden st))
  when (statUpdatedGolden st > 0) (printf "Updated %d golden values.\n" (statUpdatedGolden st))
  when (statDisabled st > 0) (printf "Ignored %d disabled tests.\n" (statDisabled st))

  case statFailures st of
    0 -> do
      ok $ printf "All %d tests passed (%.2fs)\n" total time

    fs -> do
      failure $ printf "%d out of %d tests failed (%.2fs)\n" fs total time

data FailureStatus
  = Unknown
  | Failed
  | OK

instance Semigroup FailureStatus where
  Failed  <> _      = Failed
  _       <> Failed = Failed
  OK      <> OK     = OK
  _       <> _      = Unknown

instance Monoid FailureStatus where
  mempty = OK
  mappend = (<>)

-- }}}

--------------------------------------------------
-- Console test reporter
--------------------------------------------------

-- | Report only failed tests
newtype HideSuccesses = HideSuccesses Bool
  deriving (Eq, Ord, Typeable)

instance IsOption HideSuccesses where
  defaultValue   = HideSuccesses False
  parseValue     = fmap HideSuccesses . safeRead
  optionName     = return "hide-successes"
  optionHelp     = return "Do not print tests that passed successfully"
  optionCLParser = flagCLParser Nothing (HideSuccesses True)

newtype AnsiTricks = AnsiTricks Bool
   deriving Typeable

instance IsOption AnsiTricks where
  defaultValue = AnsiTricks True
  parseValue   = fmap AnsiTricks . safeReadBool
  optionName   = return "ansi-tricks"
  optionHelp   = return $
    -- Multiline literals don't work because of -XCPP.
    "Enable various ANSI terminal tricks. " ++
    "Can be set to 'true' (default) or 'false'."

-- | When to use color on the output
data UseColor
  = Never | Always | Auto
  deriving (Eq, Ord, Typeable)

-- | Control color output
instance IsOption UseColor where
  defaultValue   = Auto
  parseValue     = parseUseColor
  optionName     = return "color"
  optionHelp     = return "When to use colored output. Options are 'never', 'always' and 'auto' (default: 'auto')"
  optionCLParser =
    option parse
      (  long name
      <> help (untag (optionHelp :: Tagged UseColor String))
      )
    where
      name = untag (optionName :: Tagged UseColor String)
      parse = str >>=
        maybe (readerError $ "Could not parse " ++ name) pure <$> parseValue

-- | @useColor when isTerm@ decides if colors should be used,
--   where @isTerm@ denotes where @stdout@ is a terminal device.
useColor :: UseColor -> Bool -> Bool
useColor cond isTerm =
  case cond of
    Never  -> False
    Always -> True
    Auto   -> isTerm

parseUseColor :: String -> Maybe UseColor
parseUseColor s =
  case map toLower s of
    "never"  -> return Never
    "always" -> return Always
    "auto"   -> return Auto
    _        -> Nothing

-- }}}

--------------------------------------------------
-- Various utilities
--------------------------------------------------
-- {{{

{-getResultWithGolden :: StatusMap -> GoldenStatusMap -> TestName -> Int -> IO (Result, ResultStatus)
getResultWithGolden smap gmap nm ix = do
  r <- getResultFromTVar statusVar

  gr <- atomically $ readTVar gmap
  case nm `M.lookup` gr of
    Just g@(GRDifferent {}) -> return (r, RMismatch g)
    Just g@(GRNoGolden {})  -> return (r, RMismatch g)
    _ | resultSuccessful r  -> return (r, RPass)
    _ | resultOutcome r
    _ | otherwise           -> return (r, RFail)
  where statusVar =
            fromMaybe (error "internal error: index out of bounds") $
            IntMap.lookup ix smap
-}

getResultFromTVar :: TVar Status -> IO Result
getResultFromTVar statusVar = do
  atomically $ do
    status <- readTVar statusVar
    case status of
      Done r -> return r
      _ -> retry



-- }}}

--------------------------------------------------
-- Formatting
--------------------------------------------------
-- {{{

indentSize :: Int
indentSize = 2

indent :: Int -> String
indent n = replicate (indentSize * n) ' '

-- handle multi-line result descriptions properly
formatDesc
  :: Int -- indent
  -> String
  -> String
formatDesc n desc =
  let
    -- remove all trailing linebreaks
    chomped = reverse . dropWhile (== '\n') . reverse $ desc

    multiline = '\n' `elem` chomped

    -- we add a leading linebreak to the description, to start it on a new
    -- line and add an indentation
    paddedDesc = flip concatMap chomped $ \c ->
      if c == '\n'
        then c : indent n
        else [c]
  in
    if multiline
      then paddedDesc
      else chomped

data Maximum a
  = Maximum a
  | MinusInfinity

instance Ord a => Semigroup (Maximum a) where
  Maximum a <> Maximum b = Maximum (a `max` b)
  MinusInfinity <> a = a
  a <> MinusInfinity = a

instance Ord a => Monoid (Maximum a) where
  mempty = MinusInfinity
  mappend = (<>)

-- | Compute the amount of space needed to align "OK"s and "FAIL"s.
--
computeAlignment :: OptionSet -> TestTree -> Int
computeAlignment opts =
  fromMonoid . foldTestTree f opts
  where
    fromMonoid :: (Int -> Maximum Int) -> Int
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

    f :: TreeFold (Int -> Maximum Int)
    f = trivialFold
      { foldSingle = \ _opts  name _test level -> addName   name level
      , foldGroup  = \ _opts _name group level -> addIndent level group
      }

    addName :: TestName -> Int -> Maximum Int
    addName name level = Maximum $ length name + level

#if MIN_VERSION_tasty(1,5,0)
    addIndent :: Int -> [Int -> Maximum Int] -> Maximum Int
    addIndent level = foldMap ($ (level + indentSize))
#else
    addIndent :: Int -> (Int -> Maximum Int) -> Maximum Int
    addIndent level = id      ($ (level + indentSize))
#endif

-- (Potentially) colorful output
ok, warn, failure, infoOk, infoWarn, infoFail :: (?colors :: Bool) => String -> IO ()
ok       = output NormalIntensity Dull  Green
warn     = output NormalIntensity Dull  Yellow
failure  = output BoldIntensity   Vivid Red
infoOk   = output NormalIntensity Dull  White
infoWarn = output NormalIntensity Dull  White
infoFail = output NormalIntensity Dull  Red

output
  :: (?colors :: Bool)
  => ConsoleIntensity
  -> ColorIntensity
  -> Color
  -> String
  -> IO ()
output bold intensity color st
  | ?colors =
    (do
      setSGR
        [ SetColor Foreground intensity color
        , SetConsoleIntensity bold
        ]
      putStr st
    ) `finally` setSGR []
  | otherwise = putStr st

-- }}}
