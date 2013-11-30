{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Hakyll.Web.Tags
import           Control.Applicative
import           Hakyll.Core.Identifier
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid

import Abbreviations

config :: Configuration
config =
  defaultConfiguration {
    deployCommand = "rsync --checksum --delete -ave 'ssh' "
                    ++ "_site/ krewinkel@moltkeplatz.de:/var/www/zeitlinse.moltkeplatz.de/"
  }

logoTemplateIdentifier = "templates/img/zeitlens-logo-template.svg"

main :: IO ()
main =
  hakyllWith config $ do

    -- compile templates
    match "templates/**" $ compile templateCompiler

    -- --------------------
    -- STATIC FILES
    -- --------------------
    match ("favicon.ico"
           .||. "robots.txt"
           .||. "decks/**"
           .||. "fonts/**"
           .||. "img/*.jpg"
           .||. "scripts/*") $ do
      route idRoute
      compile copyFileCompiler

    -- --------------------
    -- SCRIPTS
    -- --------------------
    match "scripts/components/*" $ do
      compile $ getResourceBody >>= saveSnapshot "script-component"

    -- --------------------
    -- STYLES
    -- --------------------

    -- copy static files
    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    let logoStyles = M.fromList [ ("brand",    "css/zeitlens-logo-brand.scss")
                                , ("default",  "css/zeitlens-logo-default.scss")
                                , ("inverted", "css/zeitlens-logo-inverted.scss")
                                ]
    privateSassDependency <- makePatternDependency . fromList $
                             [ "css/_settings.scss"
                             , "css/syntax.scss"
                             ] ++ M.elems logoStyles
    let publicSassFiles = "css/zeitlens.scss" .||. "css/zeitlens-deck.scss"
    rulesExtraDependencies [privateSassDependency] $ match publicSassFiles $ do
        route $ setExtension "css"
        compile sassCompiler


    -- --------------------
    -- LOGOS
    -- --------------------

    match (fromList $ M.elems logoStyles) $ do
      compile $ sassCompiler >>= saveSnapshot "logo-css"

    match "img/zeitlens-logo.svg" $ do
      compile $ getResourceBody >>= saveSnapshot "logo-svg"

    createLogoWithScss "img/zeitlens-logo-default.svg"
                       "css/zeitlens-logo-default.scss"

    createLogoWithScss "img/zeitlens-logo-inverted.svg"
                       "css/zeitlens-logo-inverted.scss"

    createLogoWithScssAndScript
                      "img/zeitlens-logo-animated.svg"
                      "css/zeitlens-logo-default.scss"
                      "scripts/components/animate-logo.js"


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

sassCompiler = getResourceString
               >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
               >>= return . fmap compressCss

createLogoWithScss imgRoute scssPattern =
    create [imgRoute] $ do
      route idRoute
      compile $ do
        css <- fmap itemBody $ loadSnapshot scssPattern "logo-css"
        img <- fmap itemBody $ loadSnapshot "img/zeitlens-logo.svg" "logo-svg"
                      >>= applyAsTemplate (imgCtx (styleTags css) "")
        makeItem img

createLogoWithScssAndScript imgRoute scssPattern jsPattern =
    create [imgRoute] $ do
      route idRoute
      compile $ do
        css <- fmap itemBody $ loadSnapshot scssPattern "logo-css"
        js  <- fmap itemBody $ loadSnapshot jsPattern "script-component"
        let ctx = imgCtx (styleTags css) (scriptTags js)
        img <- fmap itemBody $ loadSnapshot "img/zeitlens-logo.svg" "logo-svg"
                      >>= applyAsTemplate ctx
        makeItem img

-- Contexts
imgCtx defs script =
    field "logo" $ \item -> do
      let ctx = mconcat
                [ field "logo-defs" $ \_ -> return defs
                , field "logo-script" $ \_ -> return script
                , defaultContext ]
      logo <- loadBody logoTemplateIdentifier
      fmap itemBody $ applyTemplate logo ctx item

baseCtx   = (imgCtx "" "") <> defaultContext

postCtx   = mconcat
            [ dateField "date" "%B %e, %Y"
            , dateField "datetime" "%Y-%m-%d"
            , baseCtx
            ]

styleTags content = "<style type=\"text/css\">" ++ content ++ "</style>"

scriptTags script =
    "<script type=\"text/javascript\">//<![CDATA[\n" ++ script ++ "\n//]]></script>"
