--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified System.FilePath as FP
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "thumbnails/**/*.jpg" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler

    match "paintings/akryl.html" $ do
        route   idRoute
        compile $ do
          paintings <- alphaLast =<< loadAll ("paintings/akryl/*.jpg" .&&. hasNoVersion)
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (galleryCtx "akryl" paintings)
            >>= loadAndApplyTemplate "templates/default.html" (galleryCtx "akryl" paintings)
            >>= relativizeUrls

    match "paintings/olej.html" $ do
        route   idRoute
        compile $ do
          paintings <- alphaLast =<< loadAll ("paintings/olej/*.jpg" .&&. hasNoVersion)
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (galleryCtx "olej" paintings)
            >>= loadAndApplyTemplate "templates/default.html" (galleryCtx "olej" paintings)
            >>= relativizeUrls

    match "paintings/ostatni.html" $ do
        route   idRoute
        compile $ do
          paintings <- alphaLast =<< loadAll ("paintings/ostatni/*.jpg" .&&. hasNoVersion)
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (galleryCtx "ostatni" paintings)
            >>= loadAndApplyTemplate "templates/default.html" (galleryCtx "ostatni" paintings)
            >>= relativizeUrls

    match "paintings/pastel.html" $ do
        route   idRoute
        compile $ do
          paintings <- alphaLast =<< loadAll ("paintings/pastel/*.jpg" .&&. hasNoVersion)
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (galleryCtx "pastel" paintings)
            >>= loadAndApplyTemplate "templates/default.html" (galleryCtx "pastel" paintings)
            >>= relativizeUrls

    match "paintings/**/*.jpg" $ version "raw" $ do
        route   idRoute
        compile copyFileCompiler

    match "paintings/**/*.jpg" $ do
        route   $ setExtension "html"
        compile $ do
            getResourceBody
              >>= loadAndApplyTemplate "templates/painting.html" paintingCtx
              >>= loadAndApplyTemplate "templates/default.html" paintingCtx
              >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
          getResourceBody
              >>= loadAndApplyTemplate "templates/default.html"
                    (constField "home" "true" <> defaultContext)
              >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
paintingCtx =
    defaultContext <> (
      field "image-url" $ \item -> do
        url <- getRoute (itemIdentifier item)
        return $ maybe "" (\fp -> '/' : FP.replaceExtension fp "jpg") url
      ) <> (
      field "thumbnail-url" $ \item -> do
        url <- getRoute (itemIdentifier item)
        return $ maybe "" (\fp ->
          "/thumbnails" FP.</> FP.replaceExtension (FP.joinPath $ drop 1 $ FP.splitPath fp) "jpg") url
      ) <> (
      field "technique-akryl" $ \item -> do
        technique <- getMetadataField (itemIdentifier item) "technika"
        if technique /= Just "akryl"
        then fail "neni akryl"
        else return "akryl"
      ) <> (
      field "technique-olej" $ \item -> do
        technique <- getMetadataField (itemIdentifier item) "technika"
        if technique /= Just "olej"
        then fail "neni olej"
        else return "olej"
      ) <> (
      field "technique-pastel" $ \item -> do
        technique <- getMetadataField (itemIdentifier item) "technika"
        if technique /= Just "pastel"
        then fail "neni pastel"
        else return "pastel"
      ) <> (
      field "technique-ostatni" $ \item -> do
        technique <- getMetadataField (itemIdentifier item) "technika"
        if    technique /= Just "pastel"
           && technique /= Just "olej"
           && technique /= Just "akryl"
        then return "ostatni"
        else fail "neni ostatni"
      ) <> (
      field "dostupnost" $ \item -> do
        dostupnost <- getMetadataField (itemIdentifier item) "dostupnost"
        case dostupnost of
          Just "ano" -> return "ano"
          _ -> fail "nedostupne"
      )

galleryCtx technique paintings =
    defaultContext
    <> listField "paintings" paintingCtx (return paintings)
    <> constField ("technique-" <> technique) technique

alphaFirst :: [Item a] -> Compiler [Item a] 
alphaFirst items = return $ 
    sortBy (comparing (FP.takeBaseName . toFilePath . itemIdentifier)) items

alphaLast :: [Item a] -> Compiler [Item a] 
alphaLast items = reverse <$> alphaFirst items
