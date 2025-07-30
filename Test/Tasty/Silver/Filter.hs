{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regex filtering for test trees.
module Test.Tasty.Silver.Filter
  ( filterWithRegex
  , checkRF
  , RegexFilter (..)
  , IncludeFilters (..)
  , ExcludeFilters (..)
  , TestPath
  )
  where

import Prelude hiding (fail)

import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup ( (<>) )
#endif
import Data.Tagged
import qualified Data.List as L

import Options.Applicative

import qualified Text.Regex.TDFA.String as RS
import qualified Text.Regex.TDFA as R

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Options
import Test.Tasty.Runners

-- | Path into the 'TestTree'.  Separator is the slash character(@'/'@).
type TestPath = String

-- Andreas, 2025-07-30, TODO:  The following comment is without substance,
-- since all types have Typeable since GHC-7.10:
-- we have to store the regex as String, as there is no Typeable instance
-- for the Regex data type with GHC < 7.8
data RegexFilter
  = RFInclude String -- ^ Include tests that match.
  | RFExclude String -- ^ Exclude tests that match.

-- | Tests to completely exclude, treating them like they do not exist.
newtype ExcludeFilters = ExcludeFilters [RegexFilter]

-- | Tests to completely include, treating all other tests like they do not exist.
newtype IncludeFilters = IncludeFilters [RegexFilter]

instance IsOption ExcludeFilters where
  defaultValue = ExcludeFilters []
  parseValue = fmap ExcludeFilters . parseValue1 RFExclude
  optionName = return "regex-exclude"
  optionHelp = return "Exclude tests matching a regex (experimental)."
  optionCLParser = parseFilter RFExclude ExcludeFilters

instance IsOption IncludeFilters where
  defaultValue = IncludeFilters []
  parseValue = fmap IncludeFilters . parseValue1 RFInclude
  optionName = return "regex-include"
  optionHelp = return "Include only tests matching a regex (experimental)."
  optionCLParser = parseFilter RFInclude IncludeFilters

compileRegex :: String -> Maybe RS.Regex
compileRegex = either (const Nothing) Just . RS.compile R.defaultCompOpt R.defaultExecOpt

parseFilter :: forall v . IsOption v => (String -> RegexFilter) -> ([RegexFilter] -> v) -> Parser v
parseFilter mkRF mkV = mkV <$> some ( option parse ( long name <> help helpString))
  where
    name = untag (optionName :: Tagged v String)
    helpString = untag (optionHelp :: Tagged v String)
    parse = (str >>=
        either (\err -> readerError $ "Could not parse " ++ name ++ ": " ++ err) (\_ -> mkRF <$> str)
        <$> RS.compile R.defaultCompOpt R.defaultExecOpt)

parseValue1 :: (String -> RegexFilter) -> String -> Maybe [RegexFilter]
parseValue1 f x = fmap (const [f x]) $ compileRegex x

filterWithRegex :: OptionSet -> TestTree -> TestTree
filterWithRegex opts =
  -- Andreas, 2023-10-20: Since @filterWithPred (const True)@ is not the identity
  -- when the test tree contains 'WithResource' etc.,
  -- we skip it if it does not actually filter out anything.
  if null filters
    then id
    else filterWithPred (checkRF True filters)
  where
    ExcludeFilters excRgxs = lookupOption opts
    IncludeFilters incRgxs = lookupOption opts
    filters = excRgxs ++ incRgxs

-- | Check if the given path should be kept using regex filters.
-- A Tree leaf is retained if the following conditions
-- are met:
-- 1. At least one RFInclude matches.
-- 2. No RFExclude filter matches.
checkRF :: Bool -- ^ If 'True', ignore first condition if no 'RFInclude' is given.
    -> [RegexFilter]
    -> TestPath
    -> Bool
checkRF ignNoInc rf tp =
  ((null incRgxs && ignNoInc) || any regexMatches incRgxs)
    && (not $ any regexMatches excRgxs)
  where (incRgxs, excRgxs) = L.partition (isInclude) rf
        isInclude (RFInclude _) = True
        isInclude (RFExclude _) = False

        -- | Returns if the regex matches the test path.
        -- Does NOT differentiate between exclude and include
        -- filters!
        regexMatches :: RegexFilter -> Bool
        regexMatches (RFInclude rgx) = R.matchTest (fromJust $ compileRegex rgx) tp
        regexMatches (RFExclude rgx) = R.matchTest (fromJust $ compileRegex rgx) tp


filterWithPred :: (TestPath -> Bool) -> TestTree -> TestTree
filterWithPred f tree = fromMaybe emptyTest $ filter' "/" tree
  where
    filter' :: TestPath -> TestTree -> Maybe TestTree
    filter' path = \case
      SingleTest n t      -> if f (path <//> n) then Just $ SingleTest n t else Nothing
      TestGroup n ts      -> Just $ TestGroup n $ mapMaybe (filter' $ path <//> n) ts
      PlusTestOptions o t -> PlusTestOptions o <$> filter' path t
      -- we don't know at tree construction time what the tree wrapped inside an AskOptions/WithResource
      -- is going to look like. We always return something, and just return an empty test group
      -- if later on we see that the child subtree was excluded.
      WithResource r t    -> Just $ WithResource r $ \ x -> fromMaybe emptyTest $ filter' path $ t x
      AskOptions t        -> Just $ AskOptions     $ \ o -> fromMaybe emptyTest $ filter' path $ t o

    x <//> y = x ++ "/" ++ y

    emptyTest = testGroup "" []
