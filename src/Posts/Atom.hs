{-# LANGUAGE OverloadedStrings #-}

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

getRandomPostEmbed :: IO CreateEmbed
getRandomPostEmbed = fmap itemToEmbed $ runMaybeT $ do
  feed <- MaybeT (extractFeed <$> responseBody haskellPlanetAtomURL)
  randomFeedItem feed


itemToEmbed :: Maybe Item -> CreateEmbed
itemToEmbed Nothing = def
  { createEmbedAuthorName = "Haskell Companion"
  , createEmbedTitle      =
    "Sorry, something went wrong while I was fetching articles"
  }
itemToEmbed (Just item) = def
  { createEmbedAuthorName  = fromMaybe "" (getItemAuthor item)
  , createEmbedAuthorUrl   = fromMaybe "" (getItemFeedLink item)
  , createEmbedTitle = fromMaybe "Maybe check this out:" (getItemTitle item)
  , createEmbedUrl         = fromMaybe "" (getItemLink item)
  , createEmbedDescription = fromMaybe "" (getItemSummary item)
  , createEmbedFooterText  = "Created on " `T.append` fromMaybe
                               ""
                               (getItemPublishDateString item)
  }

randomFeedItem :: Feed -> MaybeIO Item
randomFeedItem = randomElement . feedItems

extractFeed :: BS.ByteString -> Maybe Feed
extractFeed bs = parseAtom $ TX.parseLBS TX.def bs
 where
  parseAtom (Left  _  ) = Nothing
  parseAtom (Right lbs) = readAtom . TX.toXMLElement . TX.documentRoot $ lbs

responseBody :: Request -> IO BS.ByteString
responseBody req = liftIO $ getResponseBody <$> httpLBS req

haskellPlanetAtomURL :: IsString a => a
haskellPlanetAtomURL = "http://planet.haskell.org/atom.xml"

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

randomElement :: [a] -> MaybeIO a
randomElement [] = mzero
randomElement l  = do
  i <- lift $ randomRIO (0, length l - 1)
  return $ l !! i
