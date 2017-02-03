{-# LANGUAGE OverloadedStrings #-}

module Memegen.Lib
    ( memegenEntry
    ) where

import qualified Snap as S
import           Snap.Core (Method(..), rqPostParams, getRequest, writeBS, getParam, method)
-- import for hello
import qualified Data.ByteString as B
-- import for upload
import qualified Snap.Util.FileUploads as S
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Control.Monad.State (liftM, liftIO)
import           System.FilePath ((</>))
import           Data.Maybe (fromJust)
-- import for creating upload directory
import           System.Directory (createDirectoryIfMissing)
-- import for module Db
import qualified Memegen.Db as Db
import qualified Snap.Snaplet.SqliteSimple as S
import           Memegen.App (AppState(..), db)
import           Control.Concurrent (withMVar)
import           Control.Lens ((^#))
-- import for storing the meme metadata at the upload time.
import           Data.Map.Lazy ((!))
-- import for list
import qualified Data.ByteString.Lazy as BL
import           Data.Aeson (encode)
-- import for meme viewer
import qualified Snap.Util.FileServe as S
-- import for Image gd
import           Memegen.Img (createMeme)
-- import for File size limit
import           Data.Int (Int64(..))

uploadDir :: String
uploadDir = "upload"

maxFileSize :: Int64
maxFileSize = 2^(24::Int)  -- 16MB

memegenEntry :: IO ()
memegenEntry = S.serveSnaplet (S.setBind "127.0.0.1" S.defaultConfig) appInit

-- changed signiture to "AppState", was "a ()" or "a b ()"
appInit :: S.SnapletInit AppState AppState
appInit = S.makeSnaplet "memegen" "Meme generator." Nothing $ do
    S.addRoutes routes
    -- initialize database
    d <- S.nestSnaplet "db" db S.sqliteInit

    -- Grab the DB connection pool from the sqlite snaplet and call
    -- into the Model to create all the DB tables if necessary.
    let c = S.sqliteConn $ d ^# S.snapletValue
    liftIO $ withMVar c $ \conn -> Db.createTables conn

    -- creating upload directory at the application initialization time:
    liftIO $ createDirectoryIfMissing True uploadDir

    return $ AppState d

routes :: [(B.ByteString, S.Handler AppState AppState ())]
routes = [ ("/", S.ifTop $ S.sendFile "resources/index.html")
         , ("hello/:echoparam", method GET $ echoHandler)
         , ("upload", method POST $ uploadHandler)
         , ("upload", S.sendFile "resources/upload.html")
         , ("list", method GET $ listHandler)
         , ("image", S.serveDirectoryWith S.fancyDirectoryConfig "upload")
         ]

-- handler for hello
echoHandler :: S.Handler AppState AppState ()
echoHandler = do
  Just param <- S.getParam "echoparam"
  writeBS $ B.append "Hello " param

-- handler for showing meme
listHandler :: S.Handler AppState AppState ()
listHandler = do
  memes <- S.withTop db $ Db.listMemes
  writeBS $ BL.toStrict $ encode memes

-- handler for upload
uploadHandler :: S.Handler AppState AppState ()
uploadHandler = do
  -- Files are sent as HTTP multipart form entries.
  files <- S.handleMultipart uploadPolicy $ \part -> do
    content <- liftM B.concat EL.consume
    return (part, content)
  let (imgPart, imgContent) = head files
  let fileName = fromJust (S.partFileName imgPart)

  req <- S.getRequest
  let params = rqPostParams req
  let topText = decodeUtf8 $ head (params ! "top")
  let bottomText = decodeUtf8 $ head (params ! "bottom")

  -- Create meme
  memeContent <- liftIO $
    createMeme imgContent (T.unpack topText) (T.unpack bottomText)
  -- Store meme metadata into DB
  S.withTop db $ Db.saveMeme topText bottomText (decodeUtf8 fileName)
  -- Store the image in upload directory.
  -- writeFile operates inside IO monad. Snap handlers run inside Snap
  -- monad, which provides an access to IO monad. We use liftIO to
  -- execute a function inside IO monad and return to Snap monad.
  liftIO $ B.writeFile
    (uploadDir </> (T.unpack $ decodeUtf8 fileName)) memeContent

  -- Show the uploaded image. (was imgContent)
  writeBS memeContent

  where
    uploadPolicy :: S.UploadPolicy
    uploadPolicy =
     -- 2^24 is the maximum allowed upload file size (16MiB)
     S.setMaximumFormInputSize (maxFileSize) S.defaultUploadPolicy
