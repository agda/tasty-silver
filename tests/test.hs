{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent.MVar
import Control.Exception      ( catch, IOException, SomeException )
import Control.Monad          ( unless )
import Control.Monad.IO.Class ( liftIO )
import Data.List (sort)
#if !(MIN_VERSION_base(4,8,0))
import Data.Functor           ( (<$) )
import Data.Monoid            ( mempty )
#endif

import System.Directory
import System.FilePath
import System.IO.Silently     ( capture )
import System.IO.Temp

import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Silver
import Test.Tasty.Silver.Advanced
import Test.Tasty.Silver.Filter (checkRF)
import Test.Tasty.Silver.Interactive
import Test.Tasty.Silver.Internal

touch :: FilePath -> IO ()
touch f = writeFile f ""

main :: IO ()
main = defaultMain $ testGroup "tests" $
  testShowDiff :
  testFindByExt :
  testWithResource :
  testCheckRF :
  []

testFindByExt :: TestTree
testFindByExt =
  testCase "findByExtension" $
    withSystemTempDirectory "golden-test" $ \basedir -> do

      setCurrentDirectory basedir

      createDirectory ("d1")
      createDirectory ("d1" </> "d2")
      touch ("d1" </> "d2" </> "h1.c")      -- match
      touch ("d1" </> "d2" </> "h1.exe")
      touch ("d1" </> "d2" </> "h1")
      touch ("d1" </> "g1.c")               -- match
      touch ("f1.c")                        -- match
      touch ("f2.h")                        -- match
      touch ("f2.exe")

      files <- findByExtension [".c", ".h"] "."
      sort files @?= sort
        [ "./d1/d2/h1.c"
        , "./d1/g1.c"
        , "./f1.c"
        , "./f2.h"
        ]

testShowDiff :: TestTree
testShowDiff =
  testCase "showDiff" $
    withSystemTempDirectory "golden-test" $ \ dir -> do
      let
        goldenFile  = dir </> "golden.txt"
        goldenValue = "golden value"
        actualFile  = dir </> "actual.txt"
        actualValue = "actual value"
        tree        = goldenVsFile "showDiff-internal" goldenFile actualFile $
                        writeFile actualFile actualValue
      writeFile goldenFile goldenValue
      writeFile actualFile actualValue
      do
          -- let io = defaultMain tree
          let io = runTestsInteractive (const False) mempty tree
          (out, success) <- capture $ catch (True <$ io) $ \ (e :: SomeException) -> do
             print e
             return False
          -- unless success $ putStr out
          putStr out
          assertBool "Test should succeed." success

-- | Check if resources are properly initialized.
testWithResource :: TestTree
testWithResource =
  testCase "withResource" $
    -- The @AcceptTests True@ option automatically replaces the golden value.
    case tryIngredients [consoleTestReporter] (singleOption $ AcceptTests True) tree of
      Just r' -> do
        (out, success) <- capture r'
        unless success $ putStr out
        assertBool "Test should succeed." success
      Nothing -> assertFailure "Test broken"
  where
    tree   = withResource acq free test
    -- Resource acquisition.
    acq    = newMVar True
    -- Resource release.
    free v = swapMVar v False >> return ()
    -- Test using resource.
    test v = goldenTest1 "check res"
               (return Nothing)           -- get the golden value
               (liftIO $ testAction v)    -- get the tested value
               (\ _ _ -> Equal)           -- comparison function (always succeed)
               (\ _   -> ShowText mempty) -- show the golden/actual value
               (\ x   -> assertBool "Incorrect result" x)  -- update the golden file
    -- Actual test: check whether the MVar contains True.
    testAction v = do
      assertBool "Resource not initialized." =<< readMVar =<< v
      return True

testCheckRF :: TestTree
testCheckRF =
  testGroup "Filter.checkRF"
    [ testCase "empty1a" $ checkRF True  [] "/"     @?= True
    , testCase "empty1b" $ checkRF False [] "/"     @?= False
    , testCase "empty2a" $ checkRF True  [] "/sdfg" @?= True
    , testCase "empty2b" $ checkRF False [] "/sdfg" @?= False
    ]
