{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Regex filtering for test trees.
module Test.Tasty.Silver.Filter
  ( filterWithRegex
  , filterWithRegex1
  , RegexFilter (..)
  , IncludeFilters (..)
  , ExcludeFilters (..)
  )
  where

import Prelude hiding (fail)
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Runners
import Test.Tasty.Options
import Data.Tagged
import Data.Typeable
import Data.Maybe
import Data.Monoid
#if __GLASGOW_HASKELL__ < 708
import Data.Foldable (foldMap)
#endif
#if __GLASGOW_HASKELL__ < 708
import Data.Proxy
#endif
import Options.Applicative
import qualified Text.Regex.TDFA.String as RS
import qualified Text.Regex.TDFA as R

-- we have to store the regex as String, as there is no Typeable instance
-- for the Regex data type with GHC < 7.8
data RegexFilter
  = RFInclude String -- include tests that match
  | RFExclude String -- exclude tests that match
  deriving (Typeable)

newtype ExcludeFilters = ExcludeFilters [RegexFilter]
  deriving (Typeable)

newtype IncludeFilters = IncludeFilters [RegexFilter]
  deriving (Typeable)

compileRegex :: String -> Maybe RS.Regex
compileRegex = either (const Nothing) Just . RS.compile R.defaultCompOpt R.defaultExecOpt

parseFilter :: forall v . IsOption v => (String -> RegexFilter) -> ([RegexFilter] -> v) -> Parser v
parseFilter mkRF mkV = mkV <$> many ( option parse ( long name <> help helpString))
  where
    name = untag (optionName :: Tagged v String)
    helpString = untag (optionHelp :: Tagged v String)
    parse = (str >>=
        either (\err -> readerError $ "Could not parse " ++ name ++ ": " ++ err) (\_ -> mkRF <$> str)
        <$> RS.compile R.defaultCompOpt R.defaultExecOpt)

parseValue1 :: (String -> RegexFilter) -> String -> Maybe [RegexFilter]
parseValue1 f x = fmap (const $ [f x]) $ compileRegex x

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


filterWithRegex :: OptionSet -> TestTree -> TestTree
filterWithRegex opts tree = foldl (filterWithRegex1 opts) tree (excRgxs ++ incRgxs)
  where ExcludeFilters excRgxs = lookupOption opts
        IncludeFilters incRgxs = lookupOption opts


filterWithRegex1 :: OptionSet -> TestTree -> RegexFilter -> TestTree
filterWithRegex1 opts tree rf = case rf of
    RFInclude rgx -> filter' (R.matchTest (fromJust $ compileRegex rgx))
    RFExclude rgx -> filter' (not . R.matchTest (fromJust $ compileRegex rgx))
  where x <//> y = x ++ "/" ++ y
        filter' :: (String -> Bool) -> TestTree
        filter' pred' =
            let alg :: TreeFold [String -> Maybe TestTree]
                alg = trivialFold
                    { foldSingle = \_ nm t -> [\pth -> if pred' (pth <//> nm) then Just (SingleTest nm t) else Nothing]
                    , foldGroup  = \nm chlds -> [\pth -> Just $ TestGroup nm (catMaybes $ map (\x -> x (pth <//> nm)) chlds)]
                    }
                [root] = foldTestTree alg opts tree
             in maybe (testGroup "" []) (id) (root "")

