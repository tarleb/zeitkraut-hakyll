{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid

import           Hakyll
import           Hakyll.Core.Identifier

import           ZeitKraut.Blog.Abbreviations


data LogoStyle = DefaultLogoStyle
               | InvertedLogoStyle
               | BrandLogoStyle

logoStyleFile :: LogoStyle -> Identifier
logoStyleFile DefaultLogoStyle  = "css/zeitlens-logo-default.scss"
logoStyleFile InvertedLogoStyle = "css/zeitlens-logo-inverted.scss"
logoStyleFile BrandLogoStyle    = "css/zeitlens-logo-brand.scss"

main :: IO ()
main =
  hakyllWith config $ do

    -- compile templates
    match "templates/**" $ compile templateCompiler

    -- --------------------
    -- STATIC FILES
    -- --------------------
    match (     "robots.txt"
           .||. "decks/**"
           .||. "img/*.jpg") $ do
      route idRoute
      compile copyFileCompiler


    -- --------------------
    -- SCRIPTS
    -- --------------------
    match "scripts/*" $ do
      route idRoute
      compile copyFileCompiler

    match "scripts/components/*" $ do
      compile $ getResourceBody >>= saveSnapshot "script-component"


    -- --------------------
    -- BOWER COMPONENTS
    -- --------------------
    match "_components/jquery/jquery.min.*" $ do
      route $ gsubRoute "_components/jquery" (const "scripts")
      compile copyFileCompiler

    match "_components/bootstrap/dist/js/bootstrap.min.js" $ do
      route $ constRoute "scripts/bootstrap.min.js"
      compile copyFileCompiler

    match "_components/bootstrap/dist/css/bootstrap.min.css" $ do
      route $ constRoute "css/bootstrap.min.css"
      compile copyFileCompiler

    match "_components/components-font-awesome/css/font-awesome.min.css" $ do
      route $ constRoute "css/font-awesome.min.css"
      compile copyFileCompiler

    match "_components/components-font-awesome/fonts/*" $ do
      route $ gsubRoute "_components/components-font-awesome/" (const "")
      compile copyFileCompiler

    -- --------------------
    -- STYLES
    -- --------------------

    let logoStyles = fromList . map logoStyleFile $
                     [ DefaultLogoStyle, InvertedLogoStyle, BrandLogoStyle ]
    sassDependencies <- mapM makePatternDependency
                        [ "css/_settings.scss"
                        , "css/syntax.scss"
                        , logoStyles
                        ]
    rulesExtraDependencies sassDependencies $ match "css/zeitlens.scss" $ do
      route $ setExtension "css"
      compile sassCompiler

    match "css/deck.js/zeitlens-deck.scss" $ do
      route $ constRoute "decks/zeitlens-deck.css"
      compile sassCompiler

    match logoStyles $ do
      compile $ sassCompiler >>= saveSnapshot "logo-css"


    -- --------------------
    -- LOGOS AND ICONS
    -- --------------------

    match "img/zeitlens-logo.svg" $ do compile $ templateCompiler

    createLogo "img/zeitlens-logo-default.svg"  DefaultLogoStyle
    createLogo "img/zeitlens-logo-inverted.svg" InvertedLogoStyle

    createLogoWithScript "img/zeitlens-logo-animated.svg"
                         DefaultLogoStyle
                         "scripts/components/animate-logo.js"

    createAppleTouchIcon "57x57"
    createAppleTouchIcon "72x72"
    createAppleTouchIcon "114x114"
    createAppleTouchIcon "144x144"

    createFavicon "favicon.ico"


    -- --------------------
    -- DEFAULT PAGES
    -- --------------------

    -- applying the base template
    let applyBase item = loadAndApplyTemplate "templates/base.html" baseCtx item
                         >>= withItemBody (return . markAbbreviations)

    -- root level static pages
    match "contact.html" $ do
      route idRoute
      compile $ do
        getResourceBody
                >>= applyAsTemplate postCtx
                >>= saveSnapshot "raw-contact"
                >>= loadAndApplyTemplate "templates/page.html" postCtx
                >>= applyBase

    match "privacy-policy.md" $ do
      route $ setExtension "html"
      compile $ do
        pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" postCtx
                >>= applyBase

    match "colophon.html" $ do
      route idRoute
      compile $ do
        contactInfo <- loadSnapshotBody "contact.html" "raw-contact"
        let contactCtx = constField "contact" contactInfo
        getResourceBody
                >>= applyAsTemplate (contactCtx <> postCtx)
                >>= loadAndApplyTemplate "templates/page.html" postCtx
                >>= applyBase


    -- --------------------
    -- TAGS
    -- --------------------
    tags <- buildTags "posts/*" $ fromCapture "*.html"

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged ‘" ++ tag ++ "’"
        let tagsCtx = constField "title" title <> defaultContext
        route $ customRoute (("tags/" <>) . toFilePath)
        compile $ do
          postList postCtx recentFirst pattern
                  >>= makeItem
                  >>= loadAndApplyTemplate "templates/post-list.html" tagsCtx
                  >>= loadAndApplyTemplate "templates/page.html" tagsCtx
                  >>= applyBase

    create ["tags.html"] $ do
      route idRoute
      compile $ do
        let cloudCtx = constField "title" "Tags" <> defaultContext
        renderTagCloud 100 300 tags
                >>= makeItem
                >>= loadAndApplyTemplate "templates/page.html" cloudCtx
                >>= applyBase

    -- --------------------
    -- POSTS AND POST LISTS
    -- --------------------

    -- home page
    match "index.html" $ do
      route idRoute
      compile $ do
        let recentPosts = fmap (take 3) . recentFirst
        posts <- postList postCtx recentPosts "posts/*"
        let idxCtx = constField "posts" posts <> baseCtx
        getResourceBody
                >>= applyAsTemplate idxCtx
                >>= loadAndApplyTemplate "templates/page.html" idxCtx
                >>= applyBase

    -- render each of the individual posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile $
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (taggedPostCtx tags)
            >>= saveSnapshot "content"
            >>= applyBase

    -- create a full list of all posts
    create ["archive.html"] $ do
      let archiveCtx = mconcat [ constField "title" "Archive"
                               , constField "description" "ZeitKraut post archive"
                               , baseCtx
                               ]
      route idRoute
      compile $ do
        postList postCtx recentFirst "posts/*"
                >>= makeItem
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/page.html" archiveCtx
                >>= loadAndApplyTemplate "templates/base.html" archiveCtx

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
            -> Pattern
            -> Compiler String
postList ctx sortFilter posts = do
    posts   <- sortFilter =<< loadAll posts
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl ctx posts


-- --------------------
-- Compilers
-- --------------------

sassCompiler :: Compiler (Item String)
sassCompiler = getResourceString
               >>= withItemBody (unixFilter "sass" ["-s", "--scss"])
               >>= return . fmap compressCss

-- Run input bytestring through imagemagick
imagemagickFilter :: String
                  -> String
                  -> LazyBS.ByteString
                  -> Compiler LazyBS.ByteString
imagemagickFilter format dimension =
    unixFilterLBS "convert" ["-" , "-resize" , dimension , format <> ":-"]

-- --------------------
-- Rules
-- --------------------

createLogoWithScript :: Identifier -> LogoStyle -> Identifier -> Rules ()
createLogoWithScript imgRoute logoStyle jsPattern =
    create [imgRoute] $ do
      route idRoute
      compile $ do
        logoCompiler logoStyle (Just jsPattern) >>= makeItem

createLogo :: Identifier -> LogoStyle -> Rules ()
createLogo imgRoute logoStyle =
    create [imgRoute] $ do
      route idRoute
      compile $ do
        logoCompiler logoStyle Nothing >>= makeItem

createAppleTouchIcon :: String -> Rules ()
createAppleTouchIcon dimension =
    create [fromFilePath $ "img/zeitlens-icon-" ++ dimension ++ ".png"] $ do
      route idRoute
      compile $ do
        fmap toByteString invertedLogo
                 >>= imagemagickFilter "png" dimension
                 >>= makeItem
  where
    toByteString = Builder.toLazyByteString . Builder.stringUtf8
    invertedLogo = logoCompiler InvertedLogoStyle Nothing

createFavicon :: Identifier -> Rules ()
createFavicon filename =
    create [filename] $ do
      route idRoute
      compile $ do
        fmap toByteString defaultLogo
                 >>= imagemagickFilter "ico" "32x32"
                 >>= makeItem
  where
    toByteString = Builder.toLazyByteString . Builder.stringUtf8
    defaultLogo = logoCompiler DefaultLogoStyle Nothing

logoCompiler :: LogoStyle -> Maybe Identifier -> Compiler String
logoCompiler logoStyle jsPattern = do
  css <- loadSnapshotBody (logoStyleFile logoStyle) "logo-css"
  js  <- maybe (return "")
               (fmap scriptTags . flip loadSnapshotBody "script-component")
               jsPattern
  let ctx = imgCtx (styleTags css) js
  fmap itemBody $ makeItem ""
           >>= loadAndApplyTemplate "templates/img/zeitlens-logo.svg" ctx


-- --------------------
-- Contexts
-- --------------------

imgCtx :: String -> String -> Context String
imgCtx defs script =
    field "logo" $ \item -> do
      let ctx = mconcat
                [ field "logo-defs" $ \_ -> return defs
                , field "logo-script" $ \_ -> return script
                , defaultContext ]
      logo <- loadBody "templates/img/zeitlens-logo-template.svg"
      fmap itemBody $ applyTemplate logo ctx item

baseCtx :: Context String
baseCtx   = (imgCtx "" "") <> defaultContext

postCtx :: Context String
postCtx   = mconcat
            [ dateField "date" "%B %e, %Y"
            , dateField "datetime" "%Y-%m-%d"
            , baseCtx
            ]

taggedPostCtx :: Tags -> Context String
taggedPostCtx tags = tagsField "tags" tags `mappend` postCtx

-- --------------------
-- MISC
-- --------------------

styleTags :: String -> String
styleTags content = "<style type=\"text/css\">" ++ content ++ "</style>"

scriptTags :: String -> String
scriptTags script =
    "<script type=\"text/javascript\">//<![CDATA[\n" ++ script ++ "\n//]]></script>"


-- --------------------
-- CONFIGURATION
-- --------------------

config :: Configuration
config =
  defaultConfiguration {
    deployCommand =
        "rsync --checksum --delete -ave 'ssh' "
        ++ "_site/ krewinkel@moltkeplatz.de:/var/www/zeitlinse.moltkeplatz.de/"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration {
      feedTitle = "ZeitKraut Blog"
    , feedDescription = "ZeitKraut Blog -- Science, Technologie, Privacy"
    , feedAuthorName = "Albert Krewinkel"
    , feedAuthorEmail = "albert+feed@zeitlens.com"
    , feedRoot = "https://zeitkraut.de"
  }
