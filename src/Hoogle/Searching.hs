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

embedFromHoogle :: CreateEmbed
embedFromHoogle = def { createEmbedAuthorName = "Hoogle"
                      , createEmbedAuthorUrl  = "https://hoogle.haskell.org/"
                      }

createEmbedInputWarning :: Int -> CreateEmbed
createEmbedInputWarning cnt = embedFromHoogle
    { createEmbedTitle = T.pack
                         $  "Please provide search number from 0 to "
                         ++ show cnt
                         ++ "."
    }

createEmbedTargetsTitles :: [Target] -> CreateEmbed
createEmbedTargetsTitles [] = embedFromHoogle
    { createEmbedTitle = "Sorry, couldn't find anything matching your phrase."
    }
createEmbedTargetsTitles targets = embedFromHoogle
    { createEmbedTitle       = "Some best matches:"
    , createEmbedDescription = T.pack $ displayTitles
    }
  where
    displayTitles =
        concat $ zipWith (\num tar -> show num ++ ". " ++ tar) [1 ..] $ map
            ((++ "\n") . targetResultDisplay False)
            targets

createEmbedTarget :: Maybe Target -> CreateEmbed
createEmbedTarget Nothing = embedFromHoogle
    { createEmbedTitle = "Sorry, couldn't find anything matching your phrase."
    }
createEmbedTarget (Just target) = embedFromHoogle
    { createEmbedTitle       = T.pack $ targetResultDisplay False target
    , createEmbedUrl         = T.pack $ targetURL target
    , createEmbedDescription = T.pack $ targetInfo target
    }

-- | Based on a provided search pattern, creates
-- a Discord embedded message with result.
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
