{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Hoogle.Searching
Description : Searching within local Hoogle database.

Here are the functions responsible for forwarding given search pattern
to the local Hoogle database and then wrapping it in the Discord bot
embed message.
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

{- |
The only exported function.
Based on the command input, the local database is searched and proper
embed message and casual message is prepared for the user.
-}
createSearchedEmbed :: IsString a => String -> Int -> IO (a, CreateEmbed)
createSearchedEmbed pattern 0 = do
    targets <- withLocalDatabase $ pure . (flip searchDatabase) pattern
    pure
        ( createCountMessage $ length targets
        , createEmbedTargetsTitles $ take 10 targets
        )
createSearchedEmbed pattern which = do
    (target, cnt) <- withLocalDatabase
        (\db -> pure $ findTarget db pattern which)
    pure $ (createCountMessage cnt, ) $ if which >= 0 && which <= cnt
        then createEmbedTarget target
        else createEmbedInputWarning cnt

-- | Creates embed message with detailed info about the target or information
-- about the absence of the target if its @Nothing@.
createEmbedTarget :: Maybe Target -> CreateEmbed
createEmbedTarget Nothing =
    embedFromHoogle { createEmbedTitle = nothingFoundMessage }
createEmbedTarget (Just target) = embedFromHoogle
    { createEmbedTitle       = T.pack $ targetResultDisplay False target
    , createEmbedUrl         = T.pack $ targetURL target
    , createEmbedDescription = T.pack $ targetInfo target
    }

-- | Creates embed message with titles of the targets.
-- Also adds some metadata to the titles, so that user may be able to chose
-- one in order to get more detailed information.
createEmbedTargetsTitles :: [Target] -> CreateEmbed
createEmbedTargetsTitles [] =
    embedFromHoogle { createEmbedTitle = nothingFoundMessage }
createEmbedTargetsTitles targets = embedFromHoogle
    { createEmbedTitle       = "Some best matches:"
    , createEmbedDescription = T.pack $ displayTitles
    }
  where
    displayTitles =
        concat $ zipWith (\num tar -> show num ++ ". " ++ tar) [1 ..] $ map
            ((++ "\n") . targetResultDisplay False)
            targets

-- | Creates message informing about the wrong search number.
createEmbedInputWarning :: Int -> CreateEmbed
createEmbedInputWarning cnt = embedFromHoogle
    { createEmbedTitle = T.pack
                         $  "Please provide search number from 0 to "
                         ++ show cnt
                         ++ "."
    }

-- | A template of embed message with author set as Hoogle.
embedFromHoogle :: CreateEmbed
embedFromHoogle = def { createEmbedAuthorName = "Hoogle"
                      , createEmbedAuthorUrl  = "https://hoogle.haskell.org/"
                      }

-- | A string informing about the absence of the results.
nothingFoundMessage :: IsString a => a
nothingFoundMessage = "Sorry, couldn't find anything matching your phrase."

-- | Creates message informing about the number of found results.
-- Empty message is created if no results are found, since there will be
-- information in the embed message.
createCountMessage :: IsString a => Int -> a
createCountMessage 0 = ""
createCountMessage cnt =
    fromString $ "Found " ++ show cnt ++ " results for your phrase."

-- | Looks for pattern within the database and then
-- returns @which@ one or @Nothing@ if nothing has been found
-- or @which@ is incorrect.
findTarget :: Database -> String -> Int -> (Maybe Target, Int)
findTarget db pattern which = (, cnt) $ if which > cnt || which < 1
    then Nothing
    else Just $ targets !! (which - 1)
  where
    targets = searchDatabase db pattern
    cnt     = length targets

-- | Perform an operation with the local database.
-- Similar to @withFile@.
withLocalDatabase :: NFData a => (Database -> IO a) -> IO a
withLocalDatabase = withDatabase dbPath

-- | A path to local Hoogle database.
dbPath :: FilePath
dbPath = "resources/hoogle-db.hoo"
