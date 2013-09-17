module Abbreviation
       (replaceAbbreviations)
where

import           Prelude
import           Map (Map)
import qualified Map as M
import           Hakyll

markAbbreviations :: (Map String String) -> String -> String
markAbbreviations abbrMap text =
  replaceAll "%[a-zA-Z0-9_]*" replacement
    where replacement matched = case M.lookup (tail matched) abbrMap of
            Nothing -> matched
            Just a -> "<abbr title=\"" ++ a ++ "\">" ++ matched ++ "</abbr>"

testAbbrMap :: Map String String
testAbbrMap = M.fromList
              [ ("test", "Just a stupid simple test")
              ]

testText = "Hello, World, this %test is really stupid."

main = print $ markAbbreviations testAbbrMap testText
