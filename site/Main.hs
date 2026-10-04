{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import Hakyll.Images ( loadImage
                     , compressJpgCompiler
                     , scaleImageCompiler
                     )
import Data.Default ( def )
import Data.List ( sortBy, isSuffixOf, isPrefixOf, stripPrefix )
import Data.Ord ( comparing, Down(..) )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA          ( (=~) )
import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Definition   ( Block(..), Pandoc )
import Text.Pandoc.Highlighting ( Style, kate, styleToCss )
import Text.Pandoc.Options      ( ReaderOptions (..)
                                , WriterOptions (..) )
import Text.Pandoc.Walk         ( walk )
import System.Directory         ( listDirectory )
import System.FilePath          ( takeBaseName, takeFileName )
import System.Process
import System.Exit
import System.IO
import GHC.Exts ( fromString )

-- configuration

data SiteConfiguration = SiteConfiguration
  { siteName :: String
  , siteRoot :: String
  } deriving (Show)

siteConfiguration :: SiteConfiguration
siteConfiguration =
  SiteConfiguration
  { siteName = "caret"
  , siteRoot = "https://maxvargas.github.io/caret"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
  { feedTitle = "lem"
  , feedDescription = "huh"
  , feedAuthorName = "max"
  , feedAuthorEmail = "blackhole@email.com"
  , feedRoot = "https://maxvargas.github.io/caret/Miscellany"
  }

hakyllConfiguration :: Configuration
hakyllConfiguration =
  defaultConfiguration
    { destinationDirectory = "plop"
    , ignoreFile = ignoreFile'
    , previewHost = "127.0.0.1"
    , previewPort = 8000
    , providerDirectory = "./"
    , storeDirectory = "_cache"
    , tmpDirectory = "_tmp"
    }
  where
    ignoreFile' path
      | ".DS_Store" == fileName           = True
      | "."    `isSuffixOf` fileName = False
      | "#"    `isSuffixOf` fileName = True
      | "~"    `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path

main :: IO ()
main = hakyllWith hakyllConfiguration $ do

  -- NOTE: Maybe the top bar can have ABOUT ; SERIAL ; BUNDLES ; PICS (Note, no HOME)

  -- Copy non-content files
  match "templates/default.html" $ compile templateBodyCompiler
  match "templates/directory.html" $ compile templateBodyCompiler
  match "templates/branchdir.html" $ compile templateBodyCompiler

  match "fonts/**" $ do
    route $ idRoute
    compile $ copyFileCompiler

  match "robots.txt" $ do
    route $ idRoute
    compile $ copyFileCompiler

  match "css/style.scss" $ do
    route $ constRoute "css/style.css"
    compile $ compileSass

  match "css/katex.min.css" $ do
    route $ constRoute "css/katex.min.css"
    compile copyFileCompiler

  match "favicon/favicon-32x32.png" $ do
    route $ gsubRoute "^favicon/" (const "")
    compile $ copyFileCompiler

  create ["css/syntax.css"] $ do
    route idRoute
    compile $ makeItem (styleToCss pandocCodeStyle)

  -- Everything in roam/ is copied into _site/
  -- Start with .html files

  match "roam/index.html" $ do
    route $ gsubRoute "^roam/" (const "")
    compile $ do
      body <- getResourceBody
      rendered <- recompilingUnsafeCompiler $
        replaceMath (T.pack (itemBody body))
      subbed <- recompilingUnsafeCompiler $
        substituteHrefs (rendered)
      makeItem (T.unpack subbed)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "roam/**.jpg" $ do
    route $ gsubRoute "^roam/" (const "")
    compile $ loadImage
      >>= scaleImageCompiler 450 225
      >>= compressJpgCompiler 80

  -- jpg reference handling is werird...
  match "roam/Photos.html" $ do
    route $ constRoute "Photos/index.html"
    compile $ do
      body <- getResourceBody
      makeItem (T.unpack $ transformAttachmentLink $ T.pack (itemBody body) )
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  notes ["Functional", "NixOS"]
  mvDir "NixOS" "notes/"
  nixos
  mvDir "Functional" "notes/"
  functional

  mvDir "" ""
  miscellany

miscellany :: Rules ()
miscellany = dirIndex "Miscellany" ""

functional :: Rules ()
functional = dirIndex "Functional" "notes/"

nixos :: Rules ()
nixos = dirIndex "NixOS" "notes/"

dirIndex :: String -> String -> Rules ()
dirIndex dirname prefix = do
  match (fromGlob ("directories/" <> dirname <> "/index.html")) $ do
    route $ constRoute (prefix <> dirname <> "/index.html")
    compile $ do
      posts <- recentPosts <$> loadPosts (fromGlob ("roam/" <> dirname <> "/*.html"))
      content <- getResourceBody
      let ctx =
            constField "content" (itemBody content)
            <> listField "posts"
              (postCtx prefix)
              (return (map (\post -> Item (postIdentifier post) post) posts))
            <> defaultContext
      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/directory.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

notes :: [String] -> Rules ()
notes subdirs = do
  match (fromGlob ("directories/notes/index.html")) $ do
    route $ constRoute ("notes/index.html")
    compile $ do
      content <- getResourceBody
      let ctx =
            constField "content" (itemBody content)
            <> listField "directories"
              directoryCtx
              (return (map (\subdir -> Item (fromString subdir) subdir) subdirs))
            <> defaultContext
      getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/branchdir.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

-- This isn't actually correct. But morally... needs fixing
-- mvDirs :: [String] -> String -> [Rules ()]
-- mvDirs subdirs dirname = map (\s -> mvDir s dirname) subdirs

-- freaking relativizeUrls with images... >:(
mvRoot :: Rules ()
mvRoot = do
  match (fromGlob ("roam/**.html")) $ do
    route $ composeRoutes
      (gsubRoute "^roam/" (const ""))
      (gsubRoute ".html$" (const "/index.html"))
    compile $ do
      body <- getResourceBody
      rendered <- recompilingUnsafeCompiler $
        replaceMath (T.pack (itemBody body))
      subbed <- recompilingUnsafeCompiler $
        substituteHrefs (rendered)
      highlighted <- recompilingUnsafeCompiler $
        replaceCode (subbed)
      makeItem (T.unpack $ transformAttachmentLink highlighted)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

mvDir :: String -> String -> Rules ()
mvDir subdir dirname = do
  match (fromGlob ("roam/" <> subdir <> "/**.html")) $ do
    route $ composeRoutes
      (gsubRoute "^roam/" (const dirname))
      (gsubRoute ".html$" (const "/index.html"))
    compile $ do
      body <- getResourceBody
      rendered <- recompilingUnsafeCompiler $
        replaceMath (T.pack (itemBody body))
      subbed <- recompilingUnsafeCompiler $
        substituteHrefs (rendered)
      highlighted <- recompilingUnsafeCompiler $
        replaceCode (subbed)
      localHreffed <- recompilingUnsafeCompiler $
        transformInternalHref (highlighted)
      makeItem (T.unpack $ transformAttachmentLink localHreffed)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match (fromGlob ("roam/" <> subdir <> "/**/*.svg")) $ do
    route $ gsubRoute "^roam/" (const dirname)
    compile $ do
      body <- getResourceBody
      makeItem $ T.unpack $ transformTikZ (T.pack (itemBody body))

  match (fromGlob ("roam/" <> subdir <> "/**")) $ do
    route $ gsubRoute "^roam/" (const dirname)
    compile $ copyFileCompiler

renderKatex :: Bool -> T.Text -> IO T.Text
renderKatex display math = do
  let args =
        if display
        then ["-F", "html", "-d", "-t"]
        else ["-F", "html", "-t"]

  (Just hin, Just hout, _, ph) <-
    createProcess
      (proc "katex" args)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        }

  TIO.hPutStr hin math
  hClose hin

  result <- TIO.hGetContents hout
  _ <- waitForProcess ph

  pure result

replaceBetween :: (T.Text -> IO T.Text) -> T.Text -> T.Text -> T.Text -> IO T.Text
replaceBetween transformFn begin end = go
  where
    go text =
      case findNext text of
        Nothing -> pure text
        Just (before, inner, after) -> do
          subbed <- transformFn inner
          rest <- go after
          pure $ before <> subbed <> rest

    findNext text =
      case (T.breakOn begin text) of
        (before, onwards)
          | T.null onwards ->
            Nothing
          | otherwise ->
            findEnd before onwards

    findEnd before onwards =
      let (bgn, content) = T.splitAt (T.length begin) onwards
          (inn, closing) = T.breakOn end content
      in
        if T.null closing
        then Nothing
        else
          Just
            ( before <> bgn
            , inn
            , closing
            )

replaceMath :: T.Text -> IO T.Text
replaceMath = do
  subbedInline <- replaceBetween (renderKatex False) "\\(" "\\)"
  subbedDisplay <- replaceBetween (renderKatex True) "\\[" "\\]"
  return subbedDisplay

compileSass :: Compiler (Item String)
compileSass = do
  css <- recompilingUnsafeCompiler $ do
    (exitCode, stdout, stderr) <-
      readProcessWithExitCode
        "sass"
        [ "--no-source-map"
        , "--style=expanded"
        , "css/style.scss"
        ]
        ""

    case exitCode of
      ExitSuccess ->
        pure stdout
      ExitFailure n ->
        error $
          "sass failed with exit code "
          ++ show n
          ++ ":\n"
          ++ stderr

  makeItem css

substituteHrefs :: T.Text -> IO T.Text
substituteHrefs = replaceBetween substituteHref "href=\"" "\""

substituteHref :: T.Text -> IO T.Text
substituteHref ref = do
  subbed <- maybeSubbed ref
  pure subbed

  where
    maybeSubbed text =
      case (T.breakOn "http" text) of
        (before, after)
          | T.null after && T.count "Functional" before > 0
            -> pure $ ("notes/" <> fst (T.breakOn ".html" before))
          | T.null after && T.count "NixOS" before > 0
            -> pure $ ("notes/" <> fst (T.breakOn ".html" before))
          | T.null after
            -> pure $ fst (T.breakOn ".html" before)
          | otherwise
            -> pure text

extractDate :: String -> Maybe String
extractDate html =
  case (html =~ ("<time>(.*)</time>" :: String)) :: (String, String, String, [String]) of
    (_, _, _, [date]) -> Just date
    _ -> Nothing

extractTitle :: String -> Maybe String
extractTitle html =
  case (html =~ ("<h1>(.*)</h1>" :: String)) :: (String, String, String, [String]) of
    (_, _, _, [date]) -> Just date
    _ -> Nothing

filePathToIndex :: String -> String -> String
filePathToIndex prefix = (replace "roam/" prefix) . (replace ".html" "")

replace :: String -> String -> String -> String
replace x y = T.unpack . (T.replace (T.pack x) (T.pack y)) . T.pack

-- This breaks a more common pattern to use `Item`s
-- However, that case needed metadata to be formatted in a particular way,
-- compatible with .md, but not so much .html
-- I'd rather stick with .html, in case I ever dislike hakyll...
data Post = Post
  { postIdentifier :: Identifier
  , postBody       :: String
  , postDate       :: Maybe String
  , postTitle      :: Maybe String
  }

recentPosts :: [Post] -> [Post]
recentPosts =
  sortBy $ comparing (Down . postDate)

loadPosts :: Pattern -> Compiler [Post]
loadPosts pattern = do
  identifiers <- getMatches pattern
  mapM loadPost identifiers

loadPost :: Identifier -> Compiler Post
loadPost identifier = do
  body <- unsafeCompiler $ readFile (toFilePath identifier)

  pure Post
    { postIdentifier = identifier
    , postBody       = body
    , postDate       = extractDate body
    , postTitle      = extractTitle body
    }

directoryCtx :: Context String
directoryCtx =
  field "title" (\item ->
    pure $ maybe "" id (Just $ itemBody item))
  <> field "url" (\item ->
    pure $ toUrl ("notes/" <> (toFilePath (itemIdentifier item))))

postCtx :: String -> Context Post
postCtx prefix =
  field "date" (\item ->
    pure $ maybe "" id (postDate (itemBody item)))
  <> field "title" (\item ->
    pure $ maybe "" id (postTitle (itemBody item)))
  <> field "url" (\item ->
    pure $ toUrl (filePathToIndex prefix (toFilePath (postIdentifier (itemBody item)))))

replaceCode :: T.Text -> IO T.Text
replaceCode = go
  where
    go text =
      case T.breakOn "<pre class=\"src " text of
        (_, "") ->
          pure text
        (before, rest) -> do
          let (block, after) = T.breakOn "</pre>" rest

          if T.null after
            then pure text
            else do
              highlighted <- highlightCode block
              rest' <- go (T.drop (T.length "</pre>") after)
              pure $ before <> highlighted <> rest'

pandocCodeStyle :: Style
pandocCodeStyle = kate

highlightCode :: T.Text -> IO T.Text
highlightCode block = do
  let prefix = "<pre class=\"src "
      fragment = T.drop (T.length prefix) block
      (className, rest) = T.breakOn "\"><code>" fragment
      language = T.drop (T.length "src-") className
      codeStart = T.drop (T.length "\"><code>") rest
      codeHtml = fst (T.breakOn "</code>" codeStart)

  result <- runIO $ do
    decoded <- readHtml def ("<pre><code>" <> codeHtml <> "</code></pre>")
    let code = extractCode decoded
    writeHtml5String
      def
        { writerHighlightStyle = Just pandocCodeStyle
        }
      (Pandoc nullMeta [CodeBlock ("", [language], []) code])

  case result of
    Left _ ->
      pure $ block
    Right output -> do
      pure output

extractCode :: Pandoc -> T.Text
extractCode (Pandoc _ blocks) =
  case blocks of
    [CodeBlock _ text] -> text
    _                    -> ""

-- TikZ coloring
tikzColor :: T.Text
tikzColor = "#97522c"

transformTikZ :: T.Text -> T.Text
transformTikZ svg =
  T.replace "<g id='page1'>"
    ("<g id='page1' fill='" <> tikzColor <> "'>")
  $ T.replace "#000" tikzColor svg

-- Hacky photo url fix
transformAttachmentLink :: T.Text -> T.Text
transformAttachmentLink =
  (T.replace "src=\"attachments" "src=\"../attachments") .
  (T.replace "src=\"tikz" "src=\"../tikz")

-- Hacky internal ref fix
transformInternalHrefPure :: T.Text -> T.Text
transformInternalHrefPure =
  T.pack . go . T.unpack
  where
    go s =
      case s =~ ("href=\"([^\"]+)\"" :: String) :: (String,String,String,[String]) of
        (before, "", _, _)  -> before
        (before, match, after, [url])
          | any (`isPrefixOf` url) ["../", "./", "/", "#", "http"] -> before ++ match ++ go after
          | otherwise -> before ++ "href=\"../" ++ url ++ "/\"" ++ go after

transformInternalHref :: T.Text -> IO T.Text
transformInternalHref text = pure (transformInternalHrefPure text)
