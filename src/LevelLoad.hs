module LevelLoad
  ( loadLevel
  ) where

import Data.Aeson (eitherDecodeStrict')
import qualified Data.ByteString as BS

import GameData
  ( Level (..)
  , LevelFile (..)
  , LevelId
  , levelPath
  )

-- | UTF-8 BOM (EF BB BF) that Windows editors often prepend.
bom :: BS.ByteString
bom = BS.pack [0xEF, 0xBB, 0xBF]

stripBom :: BS.ByteString -> BS.ByteString
stripBom bs
  | bom `BS.isPrefixOf` bs = BS.drop (BS.length bom) bs
  | otherwise              = bs

-- | Load a level JSON file for the given LevelId.
loadLevel :: LevelId -> IO Level
loadLevel lid = do
  let path = levelPath lid
  raw <- BS.readFile path
  case eitherDecodeStrict' (stripBom raw) of
    Left err ->
      ioError (userError ("Failed to load " ++ path ++ ": " ++ err))
    Right (LevelFile sts pls) ->
      pure Level
        { lvlId      = lid
        , lvlStatics = sts
        , lvlPlayers = pls
        }
