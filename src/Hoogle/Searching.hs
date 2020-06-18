{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Hoogle.Searching
Description : Searching within local Hoogle database.

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
import           Data.String                    ( IsString
                                                , fromString
                                                )

import           Discord
import           Discord.Types

import           Hoogle

-- | A path to local Hoogle database.
dbPath :: FilePath
dbPath = "resources/hoogle-db.hoo"

-- | Perform an operation with the local database.
-- Similar to @withFile@
withLocalDatabase :: NFData a => (Database -> IO a) -> IO a
withLocalDatabase = withDatabase dbPath

findTarget :: Database -> String -> Int -> (Maybe Target, Int)
findTarget db pattern which = (, cnt) $ if which > cnt || which < 1
    then Nothing
    else Just $ targets !! (which - 1)
  where
    targets = searchDatabase db pattern
    cnt     = length targets

createCountMessage :: IsString a => Int -> a
createCountMessage 0 = ""
createCountMessage cnt =
    fromString $ "Found " ++ show cnt ++ " results for your phrase."

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
createSearchedEmbed :: IsString a => String -> Int -> IO (a, CreateEmbed)
createSearchedEmbed pattern which = do
    (target, cnt) <- withLocalDatabase
        (\db -> pure $ findTarget db pattern which)
    pure (createCountMessage cnt, createEmbedTarget target)
