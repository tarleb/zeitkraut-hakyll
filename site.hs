{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Hakyll.Web.Tags
import Control.Applicative
import Hakyll.Core.Identifier
import Data.Maybe (fromMaybe)
import Data.Monoid

import Abbreviations

config :: Configuration
config =
  defaultConfiguration {
    deployCommand = "rsync --checksum --delete -ave 'ssh' "
                    ++ "_site/ krewinkel@moltkeplatz.de:/var/www/zeitlinse.moltkeplatz.de/"
  }

main :: IO ()
main =
  hakyllWith config $ do

    -- compile templates
    match "templates/*" $ compile templateCompiler

    -- copy static files
    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    -- styles
    privateSassDependency <- makePatternDependency $
                            "css/_settings.scss" .||.
                            "css/syntac.scss"    .||.
                            "css/graphics.scss"
    let publicSassFiles = "css/zeitlens.scss" .||. "css/zeitlens-deck.scss"
    rulesExtraDependencies [privateSassDependency] $ match publicSassFiles $ do
        let sassCompiler =
              getResourceString
              >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
              >>= return . fmap compressCss
        route $ setExtension "css"
        compile sassCompiler

    match ("favicon.ico" .||. "robots.txt") $ do
      route idRoute
      compile copyFileCompiler

    match ("decks/**" .||. "fonts/**") $ do
      route idRoute
      compile copyFileCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "scripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- base and post contexts
    let baseCtx   = defaultContext
    let postCtx    = mconcat
                     [ dateField "date" "%B %e, %Y"
                     , dateField "datetime" "%Y-%m-%d"
                     , defaultContext
                     ]
    -- applying the base template
    let applyBase item = loadAndApplyTemplate "templates/base.html" baseCtx item
                         >>= withItemBody (return . markAbbreviations)

    -- root level static pages
    match ("about.html" .||. "contact.html") $ do
        route idRoute
        compile $ do
            getResourceBody
                  >>= applyAsTemplate postCtx
                  >>= applyBase
                  >>= relativizeUrls

    -- render each of the individual posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= applyBase
            >>= relativizeUrls

    -- create a full list of all posts
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let postListCtx = mconcat
                  [ field "posts" (\_ -> postList postCtx recentFirst)
                  , baseCtx ]

            let basePostMetaCtx = mconcat
                  [ constField "metadescription" "ZeitLens post archive"
                  , baseCtx ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" postListCtx
                >>= loadAndApplyTemplate "templates/base.html"  basePostMetaCtx
                >>= relativizeUrls


    -- home page
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                  postList postCtx $ fmap (take 3) . recentFirst
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= applyBase
                >>= relativizeUrls

    -- Render RSS feed
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedContext = postCtx <> bodyField "description"
        let recentPosts = fmap (take 10) . recentFirst
        posts <- recentPosts =<< loadAllSnapshots "posts/*" "content"
        renderAtom feedConfiguration feedContext posts


postList :: Context String
            -> ([Item String] -> Compiler [Item String])
            -> Compiler String
postList ctx sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl ctx posts

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration {
      feedTitle = "ZeitLens Blog"
    , feedDescription = "ZeitLens Blog -- Science, Technologie, Privacy"
    , feedAuthorName = "Albert Krewinkel"
    , feedAuthorEmail = "albert+feed@zeitlens.com"
    , feedRoot = "http://zeitlens.com"
  }
