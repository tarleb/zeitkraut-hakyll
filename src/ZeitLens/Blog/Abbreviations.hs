module ZeitLens.Blog.Abbreviations
       (markAbbreviations)
where

import           Prelude
import           Data.Map (Map)
import qualified Data.Map as M
import           Hakyll

markAbbreviations :: String -> String
markAbbreviations text =
  replaceAll "%[a-zA-Z0-9_]*" replacement text
    where replacement matched = case M.lookup (tail matched) abbrMap of
            Nothing -> matched
            Just (classes, meaning) ->
                "<abbr class=\"" ++ (maybe "" id classes) ++ "\""
                                     ++ " title=\"" ++ meaning ++ "\">"
                                     ++ (tail matched) ++ "</abbr>"

abbrMap :: Map String (Maybe String, String)
abbrMap = M.fromList
          -- Abbr     extra classes  meaning
          [ ("CCC",   (Just "initialism",  "Chaos Computer Club"))
          , ("CSS",   (Just "initialism",  "Cascading Style Sheets"))
          , ("CSS2",  (Just "initialism",  "Cascading Style Sheets, second revision"))
          , ("CSS3",  (Just "initialism",  "Cascading Style Sheets, third revision"))
          , ("DRY",   (Just "initialism",  "Don't Repeat Yourself"))
          , ("GRDDL", (Just "initialism",  "Gleaning Resource Descriptions from Dialects of Languages"))
          , ("HTML",  (Just "initialism",  "HyperText Markup Language"))
          , ("HTML5", (Just "initialism",  "HyperText Markup Language, fifth revision"))
          , ("IE",    (Just "initialism",  "Internet Explorer"))
          , ("Sass",  (Nothing          , "Syntactically Awesome StyleSheets"))
          , ("SEO",   (Just "initialism",  "Search Engine Optimisation"))
          , ("SVG",   (Just "initialism",  "Scalable Vector Graphics"))
          , ("RDF",   (Just "initialism",  "Resource Description Framework"))
          , ("W3C",   (Nothing          ,  "World Wide Web Consortium"))
          , ("XHTML", (Just "initialism",  "eXtensible Hypertext Markup Language"))
          , ("XML",   (Just "initialism",  "eXtensible Markup Language"))
          , ("XSLT",  (Just "initialism",  "Extensible Stylesheet Language Transformations"))
          ]
