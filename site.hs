{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Hakyll.Web.Tags
import Control.Applicative
import Hakyll.Core.Identifier
import Data.Maybe (fromMaybe)
import Data.Monoid

main :: IO ()
main =
  hakyll $ do

    -- compile templates
    match "templates/*" $ compile templateCompiler

    -- copy static files
    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/zeitlinse.scss" $ do
        let sassCompiler =
              getResourceString
              >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
              >>= return . fmap compressCss
        route $ setExtension "css"
        compile sassCompiler

    match ("favicon.ico" .||. "robots.txt") $ do
      route idRoute
      compile copyFileCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "scripts/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- base context and template
    let baseCtx   = defaultContext
    let applyBase = loadAndApplyTemplate "templates/base.html" baseCtx

    -- context for individual posts
    let postCtx    = defaultPostCtx

    -- root level static pages
    match ("about.markdown" .||. "imprint.markdown") $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
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
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            let postListCtx = mconcat
                  [ field "posts" (\_ -> postList recentFirst)
                  , baseCtx ]

            let basePostMetaCtx = mconcat
                  [ constField "metadescription" "ZeitLinse post archive"
                  , baseCtx ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postListCtx
                >>= loadAndApplyTemplate "templates/base.html"  basePostMetaCtx
                >>= relativizeUrls


    -- home page
    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                  postList $ fmap (take 5) . recentFirst
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= applyBase
                >>= relativizeUrls

    -- Render RSS feed
    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedContext = defaultPostCtx <> bodyField "description"
        let recentPosts = fmap (take 10) . recentFirst
        posts <- recentPosts =<< loadAllSnapshots "posts/*" "content"
        renderAtom (feedConfiguration "Recent Posts") feedContext posts


defaultPostCtx :: Context String
defaultPostCtx = mconcat
  [ dateField "date" "%B %e, %Y"
  , dateField "datetime" "%Y-%m-%d"
  , defaultContext
  ]

postList :: ([Item String] -> Compiler [Item String])
            -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl defaultPostCtx posts

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title =
  FeedConfiguration {
      feedTitle = "ZeitLinse Blog -- " ++ title
    , feedDescription = "ZeitLinse Blog -- Science, Technologie, Privacy"
    , feedAuthorName = "Albert Krewinkel"
    , feedAuthorEmail = "zeitlinse+feed@moltkeplatz.de"
    , feedRoot = "http://zeitlinse.moltkeplatz.de"
  }
