module ZeitLens.Blog.Abbreviations
       (markAbbreviations)
where

import           Prelude
import           Data.Monoid
import qualified Data.Map as M

import           Hakyll (replaceAll)

markAbbreviations :: String -> String
markAbbreviations text =
  replaceAll "%[a-zA-Z0-9_]*" replacement text
    where replacement matched = case M.lookup (tail matched) abbrMap of
            Nothing -> matched
            Just (classes, meaning) ->
                mconcat [ "<abbr "
                        , classAttr classes
                        , " "
                        , titleAttr meaning
                        , ">"
                        , (tail matched)
                        , "</abbr>"
                        ]
          titleAttr meaning = "title=\"" ++ meaning ++ "\""
          classAttr classes = maybe "" (\c -> "class=\"" ++ c ++ "\"") classes

abbrMap :: M.Map String (Maybe String, String)
abbrMap = M.fromList
          -- Abbr     extra classes  meaning
          [ ("AGPLv3",(Nothing          , "GNU Affero General Public License, version 3"))
          , ("CCC",   (Just "initialism", "Chaos Computer Club"))
          , ("CSS",   (Just "initialism", "Cascading Style Sheets"))
          , ("CSS2",  (Just "initialism", "Cascading Style Sheets, second revision"))
          , ("CSS3",  (Just "initialism", "Cascading Style Sheets, third revision"))
          , ("DRY",   (Just "initialism", "Don't Repeat Yourself"))
          , ("GRDDL", (Just "initialism", "Gleaning Resource Descriptions from Dialects of Languages"))
          , ("HTML",  (Just "initialism", "Hypertext Markup Language"))
          , ("HTML5", (Just "initialism", "Hypertext Markup Language, fifth revision"))
          , ("HTTP",  (Just "initialism", "Hypertext Transfer Protocol"))
          , ("IE",    (Just "initialism", "Internet Explorer"))
          , ("JS",    (Nothing          , "JavaScript"))
          , ("Sass",  (Nothing          , "Syntactically Awesome StyleSheets"))
          , ("SEO",   (Just "initialism", "Search Engine Optimisation"))
          , ("SVG",   (Just "initialism", "Scalable Vector Graphics"))
          , ("RDF",   (Just "initialism", "Resource Description Framework"))
          , ("W3C",   (Nothing          , "World Wide Web Consortium"))
          , ("XHTML", (Just "initialism", "eXtensible Hypertext Markup Language"))
          , ("XML",   (Just "initialism", "eXtensible Markup Language"))
          , ("XSLT",  (Just "initialism", "Extensible Stylesheet Language Transformations"))
          ]
