{-# LANGUAGE OverloadedStrings #-}

module Hoogle.Searching
    ( createSearchedEmbed
    )
where

import           Control.DeepSeq                ( NFData )

import qualified Data.Text                     as T

import           Discord
import           Discord.Types

import           Hoogle

dbPath :: FilePath
dbPath = "resources/hoogle-db.hoo"

dbArg :: String
dbArg = "--database=" ++ dbPath

withLocalDatabase :: NFData a => (Database -> IO a) -> IO a
withLocalDatabase = withDatabase dbPath

getFirstTarget :: Database -> String -> Maybe Target
getFirstTarget db pattern = case searchDatabase db pattern of
    (target : _) -> Just target
    _            -> Nothing

createEmbedTarget :: Maybe Target -> CreateEmbed
createEmbedTarget Nothing = def
    { createEmbedAuthorName = "Haskell Companion"
    , createEmbedTitle = "Sorry, couldn't find anything matching your phrase."
    }
createEmbedTarget (Just target) = def
    { createEmbedAuthorName  = "Haskell Companion"
    , createEmbedTitle       = T.pack $ targetResultDisplay False target
    , createEmbedUrl         = T.pack $ targetURL target
    , createEmbedDescription = T.pack $ targetInfo target
    }

createSearchedEmbed :: String -> IO CreateEmbed
createSearchedEmbed pattern = do
    target <- withLocalDatabase (\db -> pure $ getFirstTarget db pattern)
    pure $ createEmbedTarget target
