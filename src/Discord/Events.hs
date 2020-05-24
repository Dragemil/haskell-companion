{-# LANGUAGE OverloadedStrings #-}

module Discord.Events
    ( eventHandler
    )
where

import           Control.Concurrent
import           Control.Monad

import           Data.Char                      ( toLower )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests              as R

import           Hoogle.Searching

import           Posts.Atom

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis (MessageCreate m)
    | fromBot m = pure ()
    | startWithPrefix checkPrefix m = do
        embed <- createSearchedEmbed $ getPattern m
        void $ restCall dis (R.CreateMessageEmbed (messageChannel m) "" embed)
    | startWithPrefix amuseMePrefix m = do
        embed <- getRandomPostEmbed
        void $ restCall dis (R.CreateMessageEmbed (messageChannel m) "" embed)
    | toBot m = do
        restCall dis (R.CreateReaction (messageChannel m, messageId m) "wave")
        threadDelay (1 * 10 ^ 6)
        restCall dis (R.CreateMessage (messageChannel m) (greeting m))
        pure ()
    | otherwise = pure ()
eventHandler _ _ = pure ()

greeting :: Message -> T.Text
greeting m = "Hi " `T.append` userName (messageAuthor m) `T.append` "!"

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

toBot :: Message -> Bool
toBot = ("bot" `T.isInfixOf`) . T.map toLower . messageText

checkPrefix :: IsString a => a
checkPrefix = "!check "

amuseMePrefix :: IsString a => a
amuseMePrefix = "!amuseme"

getPattern :: Message -> String
getPattern = T.unpack . T.drop (T.length checkPrefix) . messageText

startWithPrefix :: T.Text -> Message -> Bool
startWithPrefix pref = (pref `T.isPrefixOf`) . T.map toLower . messageText
