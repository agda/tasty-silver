{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{- |
This module provides a simplified interface. If you want more, see "Test.Tasty.Silver.Advanced".

=== Note about filenames

They are looked up in the usual way, thus relative
names are relative to the processes current working directory.
It is common to run tests from the package's root directory (via @cabal
test@ or @stack test@), so if your test files are under
the @tests\/@ subdirectory, your relative file names should start with
@tests\/@ (even if your @test.hs@ is itself under @tests\/@, too).

=== Note about line endings

The best way to avoid headaches with line endings
(when running tests both on UNIX and Windows) is to treat your golden files
as binary, even when they are actually textual.

This means:

* When writing output files from Haskell code, open them in binary mode
(see 'System.IO.openBinaryFile', 'System.IO.withBinaryFile' and 'System.IO.hSetBinaryMode'). This will
disable automatic @\\n -> \\r\\n@ conversion on Windows.
When using 'Data.ByteString.ByteString', note that
"Data.ByteString" and "Data.ByteString.Lazy" use binary mode for
@'writeFile'@, while "Data.ByteString.Char8" and "Data.ByteString.Lazy.Char8"
use text mode.

* Tell your version control not to do any newline conversion for golden files. For
 git, check in a @.gitattributes@ file with the following contents (assuming
 your golden files have @.golden@ extension):

>*.golden	-text

On its side, `tasty-silver` reads and writes files in binary mode, too.

Why not let Haskell/git do automatic conversion on Windows? Well, for
instance, @tar@ will not do the conversion for you when unpacking a release
tarball, so when you run e.g. @stack install your-package --tests@, the
tests will be broken.

As a last resort, you can strip all @\\r@ characters from both arguments in your
comparison function when necessary. But most of the time treating the files
as binary does the job.
-}

module Test.Tasty.Silver
  ( goldenVsFile
  , goldenVsProg
  , goldenVsAction

  , printProcResult

  , findByExtension
  )
  where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding

import System.Directory
import System.Exit
import System.FilePath
import System.Process.Text as PT

import Test.Tasty.Providers (TestTree, TestName)
import Test.Tasty.Silver.Advanced

-- | Compare a given file contents against the golden file contents. Assumes that both text files are utf8 encoded.
goldenVsFile
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> FilePath -- ^ path to the output file
  -> IO () -- ^ action that creates the output file
  -> TestTree -- ^ the test verifies that the output file contents is the same as the golden file contents
goldenVsFile name ref new act =
  goldenTest1
    name
    (fmap decodeUtf8 <$> readFileMaybe ref)
    (act >> (decodeUtf8 <$> BS.readFile new))
    textLikeDiff
    textLikeShow
    (upd)
  where upd = BS.writeFile ref . encodeUtf8

-- | Compares a given file with the output (exit code, stdout, stderr) of a program. Assumes
-- that the program output is utf8 encoded.
goldenVsProg
  :: TestName   -- ^ test name
  -> FilePath   -- ^ path to the golden file
  -> FilePath   -- ^ executable to run.
  -> [String]   -- ^ arguments to pass.
  -> T.Text     -- ^ stdin
  -> TestTree
goldenVsProg name ref cmd args inp =
  goldenVsAction name ref runProg printProcResult
  where runProg = PT.readProcessWithExitCode cmd args inp

-- | Compare something text-like against the golden file contents.
-- For the conversion of inputs to text you may want to use the Data.Text.Encoding
-- or/and System.Process.Text modules.
goldenVsAction
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO a -- ^ action that returns a text-like value.
  -> (a -> T.Text) -- ^ Converts a value to it's textual representation.
  -> TestTree -- ^ the test verifies that the returned textual representation
              --   is the same as the golden file contents
goldenVsAction name ref act toTxt =
  goldenTest1
    name
    (fmap decodeUtf8 <$> readFileMaybe ref)
    (toTxt <$> act)
    textLikeDiff
    textLikeShow
    (BS.writeFile ref . encodeUtf8)

textLikeShow :: T.Text -> GShow
textLikeShow = ShowText

textLikeDiff :: T.Text -> T.Text -> GDiff
textLikeDiff x y | x == y    = Equal
textLikeDiff x y | otherwise =  DiffText Nothing x y


-- | Converts the output of a process produced by e.g. System.Process.Text to a textual representation.
-- Stdout/stderr are written seperately, any ordering relation between the two streams
-- is lost in the translation.
printProcResult :: (ExitCode, T.Text, T.Text) -> T.Text
-- first line is exit code, then out block, then err block
printProcResult (ex, a, b) = T.unlines (["ret > " `T.append` T.pack (show ex)]
                            ++ addPrefix "out >" a ++ addPrefix "err >" b)
    where addPrefix _    t | T.null t  = []
          addPrefix pref t | otherwise = map (f pref) (T.splitOn "\n" t)
          -- don't add trailing whitespace if line is empty. git diff will mark trailing whitespace
          -- as error, which looks distracting.
          f pref ln | T.null ln = pref
          f pref ln | otherwise = pref `T.append` " " `T.append` ln



-- | Find all files in the given directory and its subdirectories that have
-- the given extensions.
-- It is typically used to find all test files and produce a golden test
-- per test file.
--
-- The returned paths use forward slashes (@'/'@) to separate path components,
-- /even on Windows/. Thus if the file name ends up in a golden file, it
-- will not differ when run on another platform.
--
-- The semantics of extensions is the same as in 'takeExtension'. In
-- particular, non-empty extensions should have the form @".ext"@.
--
-- This function may throw any exception that 'getDirectoryContents' may
-- throw.
--
-- It doesn't do anything special to handle symlinks (in particular, it
-- probably won't work on symlink loops).
-- Nor is it optimized to work with huge directory trees (you'd probably
-- want to use some form of coroutines for that).
findByExtension
  :: [FilePath] -- ^ extensions
  -> FilePath -- ^ directory
  -> IO [FilePath] -- ^ paths
findByExtension extsList = go
  where
  exts = Set.fromList extsList
  go dir = do
    allEntries <- getDirectoryContents dir
    let entries = filter (not . (`elem` [".", ".."])) allEntries
    liftM concat $ forM entries $ \e -> do
      let path = dir ++ "/" ++ e         -- NOT </>! Slash accepted even on Windows.
      isDir <- doesDirectoryExist path
      if isDir
        then go path
        else return [ path | takeExtension path `Set.member` exts ]
