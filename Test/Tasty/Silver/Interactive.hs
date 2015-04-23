{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Golden test management, interactive mode. Runs the tests, and asks
-- the user how to proceed in case of failure or missing golden standard.
module Test.Tasty.Silver.Interactive
  (
  -- * Command line helpers
    defaultMain

  -- * The ingredient
  , interactiveTests
  , Interactive (..)

  -- * Programmatic API
  , runTestsInteractive
  )
  where

import Prelude hiding (fail)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Options
import Test.Tasty.Silver.Filter
import Test.Tasty.Silver.Internal
import Test.Tasty.Silver.Interactive.Run
import Data.Typeable
import Data.Tagged
import Data.Maybe
import Data.Monoid
import qualified Data.Text.IO as TIO
#if __GLASGOW_HASKELL__ < 708
import Data.Foldable (foldMap)
#endif
import Data.Char
import qualified Data.IntMap as IntMap
#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif
import Control.Monad.State hiding (fail)
import Control.Monad.STM
import Control.Monad.Reader hiding (fail)
import Control.Monad.Identity hiding (fail)
import Control.Concurrent.STM.TVar
import Control.Exception
import Text.Printf
import qualified Data.Text as T
import Data.Text.Encoding
import Options.Applicative
import System.Process.ByteString as PS
import System.Process
import qualified Data.ByteString as BS
import System.IO
import System.IO.Temp
import System.FilePath
import Test.Tasty.Providers
import qualified Data.Map as M
import System.Console.ANSI
import qualified System.Process.Text as PTL


-- | Like @defaultMain@ from the main tasty package, but also includes the
-- golden test management capabilities.
defaultMain :: TestTree -> IO ()
defaultMain = defaultMainWithIngredients [listingTests, interactiveTests]

newtype Interactive = Interactive Bool
  deriving (Eq, Ord, Typeable)
instance IsOption Interactive where
  defaultValue = Interactive False
  parseValue = fmap Interactive . safeRead
  optionName = return "interactive"
  optionHelp = return "Run tests in interactive mode."
  optionCLParser = flagCLParser (Just 'i') (Interactive True)


data ResultStatus = RPass | RFail | RMismatch GoldenResultI

type GoldenStatus = GoldenResultI

type GoldenStatusMap = TVar (M.Map TestName GoldenStatus)

interactiveTests :: Ingredient
interactiveTests = TestManager
    [ Option (Proxy :: Proxy Interactive)
    , Option (Proxy :: Proxy HideSuccesses)
    , Option (Proxy :: Proxy UseColor)
    , Option (Proxy :: Proxy NumThreads)
    , Option (Proxy :: Proxy ExcludeFilters)
    , Option (Proxy :: Proxy IncludeFilters)
    ] $
  \opts tree ->
      Just $ runTestsInteractive opts (filterWithRegex opts tree)

runSingleTest ::  IsTest t => GoldenStatusMap -> TestName -> OptionSet -> t -> (Progress -> IO ()) -> IO Result
runSingleTest gs n opts t cb = do
  case (cast t :: Maybe Golden) of
    Nothing -> run opts t cb
    Just g -> do

        (r, gr) <- runGolden g

        -- we may be in a different thread here than the main ui.
        -- force evaluation of actual value here, as we have to evaluate it before
        -- leaving this test.
        gr' <- forceGoldenResult gr
        atomically $ modifyTVar gs (M.insert n gr')
        return r


-- | A simple console UI
runTestsInteractive :: OptionSet -> TestTree -> IO Bool
runTestsInteractive opts tests = do
  gmap <- newTVarIO M.empty
  let tests' = wrapRunTest (runSingleTest gmap) tests

  r <- launchTestTree opts tests' $ \smap ->
    do
    isTerm <- hSupportsANSI stdout

    (\k -> if isTerm
      then (do hideCursor; k) `finally` showCursor
      else k) $ do

      hSetBuffering stdout NoBuffering

      let
        whenColor = lookupOption opts
        HideSuccesses hideSuccesses = lookupOption opts

      let
        ?colors = useColor whenColor isTerm

      let
        outp = produceOutput opts tests

      stats <- case () of { _
        | hideSuccesses && isTerm ->
            consoleOutputHidingSuccesses outp smap gmap
        | hideSuccesses && not isTerm ->
            streamOutputHidingSuccesses outp smap gmap
        | otherwise -> consoleOutput outp smap gmap
      }

      return $ \time -> do
            printStatistics stats time
            return $ statFailures stats == 0



  return r


printDiff :: TestName -> GDiff -> IO ()
printDiff n (DiffText _ tGold tAct) = withDiffEnv
  (\fGold fAct -> do
        (_, stdOut, _) <- PTL.readProcessWithExitCode "sh" ["-c", "git diff --no-index --text " ++ fGold ++ " " ++ fAct] T.empty
        TIO.putStrLn stdOut

  )
  n tGold tAct
printDiff _ (ShowDiffed _ t) = TIO.putStrLn t
printDiff _ Equal = error "Can't print diff for equal values."

showDiff :: TestName -> GDiff -> IO ()
showDiff n (DiffText _ tGold tAct) = withDiffEnv
  (\fGold fAct -> callProcess "sh"
        ["-c", "git diff --color=always --no-index --text " ++ fGold ++ " " ++ fAct ++ " | less -r > /dev/tty"])
  n tGold tAct
showDiff n (ShowDiffed _ t) = showInLess n t
showDiff _ Equal = error "Can't show diff for equal values."

-- Stores the golden/actual text in two files, so we can use it for git diff.
withDiffEnv :: (FilePath -> FilePath -> IO ()) -> TestName -> T.Text -> T.Text -> IO ()
withDiffEnv cont n tGold tAct = do
  withSystemTempFile (n <.> "golden") (\fGold hGold -> do
    withSystemTempFile (n <.> "actual") (\fAct hAct -> do
      hSetBinaryMode hGold True
      hSetBinaryMode hAct True
      BS.hPut hGold (encodeUtf8 tGold)
      BS.hPut hAct (encodeUtf8 tAct)
      hClose hGold
      hClose hAct
      cont fGold fAct
      )
    )


printValue :: TestName -> GShow -> IO ()
printValue _ (ShowText t) = TIO.putStrLn t

showValue :: TestName -> GShow -> IO ()
showValue n (ShowText t) = showInLess n t

showInLess :: String -> T.Text -> IO ()
showInLess _ t = do
  -- TODO error handling...
  _ <- PS.readProcessWithExitCode "sh" ["-c", "less > /dev/tty"] inp
  return ()
  where inp = encodeUtf8 t

tryAccept :: String -> TestName -> (a -> IO ()) -> a -> IO Bool
tryAccept pref nm upd new = do
  isTerm <- hSupportsANSI stdout
  when isTerm showCursor
  _ <- printf "%sAccept actual value as new golden value? [yn] " pref
  ans <- getLine
  case ans of
    "y" -> do
        upd new
        when isTerm hideCursor
        printf "%s" pref
        return True
    "n" -> do
        printf "%s" pref
        when isTerm hideCursor
        return False
    _   -> do
        printf "%sInvalid answer.\n" pref
        tryAccept pref nm upd new


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
      {- print test result -} ((Result, ResultStatus) -> IO Statistics)
  | PrintHeading (IO ()) TestOutput
  | Skip
  | Seq TestOutput TestOutput

-- The monoid laws should hold observationally w.r.t. the semantics defined
-- in this module
instance Monoid TestOutput where
  mempty = Skip
  mappend = Seq

type Level = Int

produceOutput :: (?colors :: Bool) => OptionSet -> TestTree -> TestOutput
produceOutput opts tree =
  let
    -- Do not retain the reference to the tree more than necessary
    !alignment = computeAlignment opts tree
    Interactive isInteractive = lookupOption opts

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
        hsep = putStrLn (replicate 40 '=')
        printResultLine success time forceTime = do
          -- use an appropriate printing function
          let
            printFn =
              if success
                then ok
                else fail
          if success
            then printFn "OK"
            else printFn "FAIL"
          -- print time only if it's significant
          when (time >= 0.01 || forceTime) $
            printFn (printf " (%.2fs)" time)
          printFn "\n"


        handleTestResult (result, resultStatus) = do
          -- non-interactive mode. Uses different order of printing,
          -- as using the interactive layout doesn't go that well
          -- with printing the diffs to stdout.
          --
          printResultLine (resultSuccessful result) (resultTime result) True

          rDesc <- formatMessage $ resultDescription result
          when (not $ null rDesc) $
            (if resultSuccessful result then infoOk else infoFail) $
              printf "%s%s\n" pref (formatDesc (level+1) rDesc)

          stat' <- case resultStatus of
            RMismatch (GRNoGolden a shw _) -> do
                infoFail $ printf "%sActual value is:\n" pref
                let a' = runIdentity a
                shw' <- shw a'
                hsep
                printValue name shw'
                hsep
                return ( mempty { statFailures = 1 } )
            RMismatch (GRDifferent _ _ diff _) -> do
                infoFail $ printf "%sDiff between actual and golden value:\n" pref
                hsep
                printDiff name diff
                hsep
                return ( mempty { statFailures = 1 } )
            RMismatch _ -> error "Impossible case!"
            RPass -> return ( mempty { statSuccesses = 1 } )
            RFail -> return ( mempty { statFailures = 1 } )

          return stat'

        handleTestResultInteractive (result, resultStatus) = do
          (result', stat') <- case resultStatus of
            RMismatch (GRNoGolden a shw upd) -> do
                printf "Golden value missing. Press <enter> to show actual value.\n"
                _ <- getLine
                let a' = runIdentity a
                shw' <- shw a'
                showValue name shw'
                isUpd <- tryAccept pref name upd a'

                return (
                    if isUpd
                    then ( testPassed "Created golden value."
                         , mempty { statCreatedGolden = 1 } )
                    else ( testFailed "Golden value missing."
                         , mempty { statFailures = 1 } )
                    )
            RMismatch (GRDifferent _ a diff upd) -> do
                printf "Golden value differs from actual value.\n"
                showDiff name diff
                isUpd <- tryAccept pref name upd a
                return (
                    if isUpd
                    then ( testPassed "Updated golden value."
                         , mempty { statUpdatedGolden = 1 } )
                    else ( testFailed "Golden value does not match actual output."
                         , mempty { statFailures = 1 } )
                    )
            RMismatch _ -> error "Impossible case!"
            RPass -> return (result, mempty { statSuccesses = 1 })
            RFail -> return (result, mempty { statFailures = 1 })
          rDesc <- formatMessage $ resultDescription result'

          printResultLine (resultSuccessful result') (resultTime result) False

          when (not $ null rDesc) $
            (if resultSuccessful result' then infoOk else infoFail) $
              printf "%s%s\n" pref (formatDesc (level+1) rDesc)

          return stat'

      let handleTestResult' = (if isInteractive then handleTestResultInteractive else handleTestResult)
      return $ HandleTest name printTestName handleTestResult'

    handleGroup :: TestName -> Ap (Reader Level) TestOutput -> Ap (Reader Level) TestOutput
    handleGroup name grp = Ap $ do
      level <- ask
      let
        printHeading = printf "%s%s\n" (indent level) name
        printBody = runReader (getApp grp) (level + 1)
      return $ PrintHeading printHeading printBody

  in
    flip runReader 0 $ getApp $
      foldTestTree
        trivialFold
          { foldSingle = handleSingleTest
          , foldGroup = handleGroup
          }
          opts tree

foldTestOutput
  :: (?colors :: Bool, Monoid b)
  => (IO () -> IO (Result, ResultStatus)
    -> ((Result, ResultStatus) -> IO Statistics)
    -> b)
  -> (IO () -> b -> b)
  -> TestOutput -> StatusMap -> GoldenStatusMap -> b
foldTestOutput foldTest foldHeading outputTree smap gmap =
  flip evalState 0 $ getApp $ go outputTree where
  go (HandleTest nm printName handleResult) = Ap $ do
    ix <- get
    put $! ix + 1
    let
      readStatusVar = getResultWithGolden smap gmap nm ix
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
consoleOutput :: (?colors :: Bool) => TestOutput -> StatusMap -> GoldenStatusMap -> IO Statistics
consoleOutput outp smap gmap =
  getApp . fst $ foldTestOutput foldTest foldHeading outp smap gmap
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

consoleOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> GoldenStatusMap -> IO Statistics
consoleOutputHidingSuccesses outp smap gmap =
  snd <$> (getApp $ foldTestOutput foldTest foldHeading outp smap gmap)
  where
    foldTest printName getResult handleResult =
      Ap $ do
          _ <- printName
          r <- getResult
          if resultSuccessful (fst r)
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

streamOutputHidingSuccesses :: (?colors :: Bool) => TestOutput -> StatusMap -> GoldenStatusMap -> IO Statistics
streamOutputHidingSuccesses outp smap gmap =
  snd <$> (flip evalStateT [] . getApp $
    foldTestOutput foldTest foldHeading outp smap gmap)
  where
    foldTest printName getResult handleResult =
      Ap $ do
          r <- liftIO $ getResult
          if resultSuccessful (fst r)
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
  }

instance Monoid Statistics where
  Statistics s1 ug1 cg1 f1 `mappend` Statistics s2 ug2 cg2 f2 = Statistics (s1 + s2) (ug1 + ug2) (cg1 + cg2) (f1 + f2)
  mempty = Statistics 0 0 0 0

printStatistics :: (?colors :: Bool) => Statistics -> Time -> IO ()
printStatistics st time = do
  printf "\n"

  let total = statFailures st + statUpdatedGolden st + statCreatedGolden st + statSuccesses st

  when (statCreatedGolden st > 0) (printf "Created %d golden values.\n" (statCreatedGolden st))
  when (statUpdatedGolden st > 0) (printf "Updated %d golden values.\n" (statUpdatedGolden st))

  case statFailures st of
    0 -> do
      ok $ printf "All %d tests passed (%.2fs)\n" total time

    fs -> do
      fail $ printf "%d out of %d tests failed (%.2fs)\n" fs total time

data FailureStatus
  = Unknown
  | Failed
  | OK

instance Monoid FailureStatus where
  mappend Failed _ = Failed
  mappend _ Failed = Failed

  mappend OK OK = OK

  mappend _ _ = Unknown

  mempty = OK

-- }}}

--------------------------------------------------
-- Console test reporter
--------------------------------------------------

-- | Report only failed tests
newtype HideSuccesses = HideSuccesses Bool
  deriving (Eq, Ord, Typeable)
instance IsOption HideSuccesses where
  defaultValue = HideSuccesses False
  parseValue = fmap HideSuccesses . safeRead
  optionName = return "hide-successes"
  optionHelp = return "Do not print tests that passed successfully"
  optionCLParser = flagCLParser Nothing (HideSuccesses True)

-- | When to use color on the output
data UseColor
  = Never | Always | Auto
  deriving (Eq, Ord, Typeable)

-- | Control color output
instance IsOption UseColor where
  defaultValue = Auto
  parseValue = parseUseColor
  optionName = return "color"
  optionHelp = return "When to use colored output. Options are 'never', 'always' and 'auto' (default: 'auto')"
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

getResultWithGolden :: StatusMap -> GoldenStatusMap -> TestName -> Int -> IO (Result, ResultStatus)
getResultWithGolden smap gmap nm ix = do
  r <- getResultFromTVar statusVar

  gr <- atomically $ readTVar gmap
  case nm `M.lookup` gr of
    Just g@(GRDifferent {}) -> return (r, RMismatch g)
    Just g@(GRNoGolden {})  -> return (r, RMismatch g)
    _ | resultSuccessful r  -> return (r, RPass)
    _ | otherwise           -> return (r, RFail)
  where statusVar =
            fromMaybe (error "internal error: index out of bounds") $
            IntMap.lookup ix smap

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

instance Ord a => Monoid (Maximum a) where
  mempty = MinusInfinity

  Maximum a `mappend` Maximum b = Maximum (a `max` b)
  MinusInfinity `mappend` a = a
  a `mappend` MinusInfinity = a

-- | Compute the amount of space needed to align "OK"s and "FAIL"s
computeAlignment :: OptionSet -> TestTree -> Int
computeAlignment opts =
  fromMonoid .
  foldTestTree
    trivialFold
      { foldSingle = \_ name _ level -> Maximum (length name + level)
      , foldGroup = \_ m -> m . (+ indentSize)
      }
    opts
  where
    fromMonoid m =
      case m 0 of
        MinusInfinity -> 0
        Maximum x -> x

-- (Potentially) colorful output
ok, fail, infoOk, infoFail :: (?colors :: Bool) => String -> IO ()
fail     = output BoldIntensity   Vivid Red
ok       = output NormalIntensity Dull  Green
infoOk   = output NormalIntensity Dull  White
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
