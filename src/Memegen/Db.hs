{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Memegen.Db
   ( Meme(..)
  --  Define the database schema, and create it if it doesn't exist:
   , tableExists
   , createTables
  --  Write functions to read and write database data:
   , listMemes
   , saveMeme
   ) where

import qualified Data.Text as T
import qualified Snap.Snaplet.SqliteSimple as L
-- import for defining database schema
import qualified Database.SQLite.Simple as D
import           Control.Monad.State (unless)
-- import for functions to read and write data
import Memegen.App (AppState(..))
import Snap.Snaplet (Handler(..))
-- import for meme in JSON
import Data.Aeson
import Data.Aeson.TH

data Meme = Meme
 {
   memeId :: Int
 , topText :: T.Text
 , bottomText :: T.Text
 , imageFilepath :: T.Text
 } deriving (Show)

$(deriveJSON defaultOptions ''Meme)

instance L.FromRow Meme where
 fromRow = Meme <$> L.field <*> L.field <*> L.field <*> L.field

-- functions for defining database schema
tableExists :: D.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- D.query conn "SELECT name FROM sqlite_master \
                    \WHERE type='table' AND name=?" (L.Only tblName)
  case r of
    [L.Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: D.Connection -> IO ()
createTables conn = do
  schemaCreated <- tableExists conn "memes"
  unless schemaCreated $
    D.execute_ conn
      (D.Query $ "CREATE TABLE memes (\
                 \id INTEGER PRIMARY KEY, \
                 \top_text TEXT, \
                 \bottom_text TEXT, \
                 \image_filepath TEXT)")

-- functions for reading and writing database data
-- | Retrieve all memes.
listMemes :: Handler AppState L.Sqlite [Meme]
listMemes = L.query "SELECT id, top_text, bottom_text, image_filepath \
                   \FROM memes ORDER BY id DESC" ()

-- | Save a new meme
saveMeme :: T.Text -> T.Text -> T.Text -> Handler AppState L.Sqlite ()
saveMeme top bottom filepath =
  L.execute "INSERT INTO memes (top_text, bottom_text, image_filepath) \
            \VALUES (?, ?, ?)" (top, bottom, filepath)
