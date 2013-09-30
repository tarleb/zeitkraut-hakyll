module Abbreviations
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
            Just a -> "<abbr title=\"" ++ a ++ "\">" ++ (tail matched) ++ "</abbr>"

abbrMap :: Map String String
abbrMap = M.fromList
          [ ("CCC",   "Chaos Computer Club")
          , ("CSS",   "Cascading Style Sheets")
          , ("CSS2",  "Cascading Style Sheets, second revision")
          , ("CSS3",  "Cascading Style Sheets, third revision")
          , ("DRY",   "Don't Repeat Yourself")
          , ("GRDDL", "Gleaning Resource Descriptions from Dialects of Languages")
          , ("HTML",  "HyperText Markup Language")
          , ("HTML5", "HyperText Markup Language, fifth revision")
          , ("IE",    "Internet Explorer")
          , ("Sass",  "Syntactically Awesome StyleSheets")
          , ("SEO",   "Search Engine Optimisation")
          , ("SVG",   "Scalable Vector Graphics")
          , ("RDF",   "Resource Description Framework")
          , ("W3C",   "World Wide Web Consortium")
          , ("XHTML", "eXtensible Hypertext Markup Language")
          , ("XML",   "eXtensible Markup Language")
          , ("XSLT",  "Extensible Stylesheet Language Transformations")
          ]
