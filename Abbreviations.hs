module Abbreviation
       (markAbbreviations)
where

import           Prelude
import           Data.Map (Map)
import qualified Data.Map as M
import           Hakyll

markAbbreviations :: (Map String String) -> String -> String
markAbbreviations abbrMap text =
  replaceAll "%[a-zA-Z0-9_]*" replacement text
    where replacement matched = case M.lookup (tail matched) abbrMap of
            Nothing -> matched
            Just a -> "<abbr title=\"" ++ a ++ "\">" ++ (tail matched) ++ "</abbr>"

testAbbrMap :: Map String String
testAbbrMap = M.fromList
              [ ("test", "Just a stupid simple test")
              ]

testText = "Hello, World!  This %test is really stupid."

main = print $ markAbbreviations testAbbrMap testText
