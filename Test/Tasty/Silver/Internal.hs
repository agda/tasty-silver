{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Silver.Internal where

import Control.Exception
import Control.Monad.Identity

import Data.ByteString as SB
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T

import System.IO.Error

import Test.Tasty.Providers
import Test.Tasty.Options

-- | See 'Test.Tasty.Silver.Advanced.goldenTest1' for explanation of the fields.

data Golden =
  forall a .
    Golden
        (IO (Maybe a))    -- Get golden value.
        (IO a)            -- Get actual value.
        (a -> a -> IO GDiff)                       -- Compare/diff.
        (a -> IO GShow)                            -- How to produce a show.
        (Maybe (a -> IO ()))                       -- Update golden value.


-- | This option, when set to 'True', specifies that we should run in the
-- «accept tests» mode.

newtype AcceptTests = AcceptTests Bool
  deriving (Eq, Ord)

instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeRead
  optionName = return "accept"
  optionHelp = return "Accept current results of golden tests"
  optionCLParser =  flagCLParser Nothing (AcceptTests True)

-- | Read the file if it exists, else return 'Nothing'.
-- Useful for reading golden files.

readFileMaybe :: FilePath -> IO (Maybe SB.ByteString)
readFileMaybe path = catchJust
    (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
    (Just <$> SB.readFile path)
    (const $ return Nothing)


-- | The comparison/diff result.

data GDiff
  = Equal
      -- ^ Values are equal.
  | DiffText { gReason :: (Maybe String), gActual :: T.Text, gExpected :: T.Text }
      -- ^ The two values are different, show a diff between the two given texts.
  | ShowDiffed { gReason :: (Maybe String), gDiff :: T.Text }
      -- ^ The two values are different, just show the given text to the user.

-- | How to show a value to the user.

data GShow
  = ShowText T.Text     -- ^ Show the given text.

instance IsTest Golden where
  run opts golden _ = do
    (r, gr) <- runGolden golden
    let (AcceptTests accept) = lookupOption opts :: AcceptTests
    case gr of
      GRNoGolden act _ (Just upd) | accept -> do
            act >>= upd
            return $ testPassed "Created golden file."
      GRDifferent _ act _ (Just upd) | accept -> do
            upd act
            return $ testPassed "Updated golden file."
      _ -> return r

  testOptions = return [Option (Proxy :: Proxy AcceptTests)]

type GoldenResult = GoldenResult' IO
type GoldenResultI = GoldenResult' Identity

data GoldenResult' m
  = GREqual
  | forall a . GRDifferent
        (a)     -- golden
        (a)     -- actual
        (GDiff) -- diff
        (Maybe (a -> IO ())) -- update
  | forall a . GRNoGolden
        (m a) -- compute actual (we don't want to compute it if it is not used)
        (a -> IO GShow) --show
        (Maybe (a -> IO ())) -- update

runGolden :: Golden -> IO (Result, GoldenResult)
runGolden (Golden getGolden getActual cmp shw upd) = do
  ref' <- getGolden
  case ref' of
    Nothing -> return (testFailed "Missing golden value.", GRNoGolden getActual shw upd)
    Just ref -> do
      new <- getActual
      -- Output could be arbitrarily big, so don't even try to say what wen't wrong.
      cmp' <- cmp ref new
      case cmp' of
        Equal -> return (testPassed "", GREqual)
        d -> let r = fromMaybe "Result did not match golden value." (gReason d)
              in return (testFailed r, GRDifferent ref new cmp' upd)

forceGoldenResult :: GoldenResult -> IO GoldenResultI
forceGoldenResult gr = case gr of
            (GRNoGolden act shw upd) -> do
                act' <- act
                return $ GRNoGolden (Identity act') shw upd
            (GRDifferent a b c d) -> return $ GRDifferent a b c d
            (GREqual) -> return GREqual

instance Show (GoldenResult' m) where
  show GREqual = "GREqual"
  show (GRDifferent {}) = "GRDifferent"
  show (GRNoGolden {}) = "GRNoGolden"


-- * Generic utilites

-- | Monadic @if@.

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mc mt me = do
  c <- mc
  if c then mt else me

-- | Monadic @if (not ...) ...@.

ifNotM :: Monad m => m Bool -> m a -> m a -> m a
ifNotM mc = flip $ ifM mc

-- | Short-cutting version of @'liftM2' (&&)@.

and2M :: Monad m => m Bool -> m Bool -> m Bool
and2M ma mb = ifM ma mb $ return False

-- | Short-cutting version of @'and' . 'sequence'@.

andM :: Monad m => [m Bool] -> m Bool
andM = Prelude.foldl and2M (return True)

-- | Short-cutting version of @'liftM2' (||)@.

or2M :: Monad m => m Bool -> m Bool -> m Bool
or2M ma mb = ifM ma (return True) mb

-- | Short-cutting version of @'or' . 'sequence'@.

orM :: Monad m => [m Bool] -> m Bool
orM = Prelude.foldl or2M (return False)
