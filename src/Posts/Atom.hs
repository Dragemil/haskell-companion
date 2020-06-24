{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Posts.Atom
Description : Fetching random blog posts from the planet haskell.

Here are the functions responsible for fetching random blog posts from
the [planet haskell](http://planet.haskell.org/).
-}
module Posts.Atom
  ( getRandomPostEmbed
  )
where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text                     as T
import           Data.String                    ( IsString )
import           Data.Maybe

import           Discord
import           Discord.Types

import           Text.Feed.Import
import           Text.Feed.Types
import           Text.Feed.Query
import qualified Text.XML                      as TX

import           Network.HTTP.Simple
import           System.Random                  ( randomRIO )

type MaybeIO = MaybeT IO

{- |
The only exported function.
Fetches random blog post from the [planet haskell](http://planet.haskell.org/),
parses it from Atom, and embeds it into a Discord embedded message.
-}
getRandomPostEmbed :: IO CreateEmbed
getRandomPostEmbed = fmap itemToEmbed $ runMaybeT $ do
  feed <- MaybeT (extractFeed <$> responseBody haskellPlanetAtomURL)
  randomFeedItem feed

{- |
Embeds given feed item into a Discord embedded message.
Creates a sample failure message if no item was given.
-}
itemToEmbed :: Maybe Item -> CreateEmbed
itemToEmbed Nothing = def
  { createEmbedAuthorName = "Haskell Companion"
  , createEmbedTitle      =
    "Sorry, something went wrong while I was fetching articles"
  }
itemToEmbed (Just item) = def
  { createEmbedAuthorName  = fromMaybe "" (getItemAuthor item)
  , createEmbedAuthorUrl   = fromMaybe "" (getItemFeedLink item)
  , createEmbedTitle       = fromMaybe "No title was given" (getItemTitle item)
  , createEmbedUrl         = fromMaybe "" (getItemLink item)
  , createEmbedDescription = fromMaybe "" (getItemSummary item)
  , createEmbedFooterText  = "Created on " `T.append` fromMaybe
                               ""
                               (getItemPublishDateString item)
  }

-- | Returns @Just@ random feed item if there is any or @Nothing@ if there are none.
randomFeedItem :: Feed -> MaybeIO Item
randomFeedItem = randomElement . feedItems

-- | Tries to parse a @ByteString@ into a @Feed@.
-- Either an Atom or a RSS 2.x standard may be parsed correctly.
extractFeed :: BS.ByteString -> Maybe Feed
extractFeed = parseFeedSource

-- | Sends HTTP request and returns its body.
responseBody :: Request -> IO BS.ByteString
responseBody req = liftIO $ getResponseBody <$> httpLBS req

haskellPlanetAtomURL :: IsString a => a
haskellPlanetAtomURL = "http://planet.haskell.org/atom.xml"

-- | Returns @Just@ random element from the list or @Nothing@ if the list is empty.
randomElement :: [a] -> MaybeIO a
randomElement [] = mzero
randomElement l  = do
  i <- lift $ randomRIO (0, length l - 1)
  return $ l !! i
