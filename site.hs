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
    match "css/*" $ do
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
                  [ constField "metadescription" "Zeit Linse post archive"
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
                  postList $ fmap (take 10) . recentFirst
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= applyBase
                >>= relativizeUrls


defaultPostCtx :: Context String
defaultPostCtx = mconcat
  [ dateField "date" "%B %e, %Y"
  , defaultContext
  ]

postList :: ([Item String] -> Compiler [Item String])
            -> Compiler String
postList sortFilter = do
    posts   <- sortFilter =<< loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts
