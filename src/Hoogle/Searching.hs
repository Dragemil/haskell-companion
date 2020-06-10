{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Hoogle.Searching
Description : A module responsible for searching
              within local Hoogle database.

Here are the functions responsible for forwarding given search pattern
to the local Hoogle database and then wrapping it in the Discord bot
embedded message.
-}
module Hoogle.Searching
    ( createSearchedEmbed
    )
where

import           Control.DeepSeq                ( NFData )

import qualified Data.Text                     as T

import           Discord
import           Discord.Types

import           Hoogle

-- | A path to our local Hoogle database.
dbPath :: FilePath
dbPath = "resources/hoogle-db.hoo"

-- | Perform an operation with the local database.
-- Similar to @withFile@
withLocalDatabase :: NFData a => (Database -> IO a) -> IO a
withLocalDatabase = withDatabase dbPath

getFirstTarget :: Database -> String -> Maybe Target
getFirstTarget db pattern = case searchDatabase db pattern of
    (target : _) -> Just target
    _            -> Nothing

createEmbedTarget :: Maybe Target -> CreateEmbed
createEmbedTarget Nothing = def
    { createEmbedAuthorName = "Hoogle"
    , createEmbedAuthorUrl = "https://hoogle.haskell.org/"
    , createEmbedTitle = "Sorry, couldn't find anything matching your phrase."
    }
createEmbedTarget (Just target) = def
    { createEmbedAuthorName  = "Hoogle"
    , createEmbedAuthorUrl   = "https://hoogle.haskell.org/"
    , createEmbedTitle       = T.pack $ targetResultDisplay False target
    , createEmbedUrl         = T.pack $ targetURL target
    , createEmbedDescription = T.pack $ targetInfo target
    }

-- | Based on a provided search pattern, creates
-- a Discord embedded message with result.
createSearchedEmbed :: String -> IO CreateEmbed
createSearchedEmbed pattern = do
    target <- withLocalDatabase (\db -> pure $ getFirstTarget db pattern)
    pure $ createEmbedTarget target
