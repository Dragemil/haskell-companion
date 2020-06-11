{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Discord.Events
Description : Handling events for the Discord bot.

Every user input in the Discord server is parsed in this module
and if a command is recognized an appropriate response is prepared.
Known commands are:

     * /!check \<pattern\>/ - for checking /\<pattern\>/ in the local Hoogle
     documentation database,
     * /!amuseme/ - for fetching random blog post from the [planet haskell](http://planet.haskell.org/),
     * A message containing /bot/ which is considered as a greeting to the bot.
     Bot tries to be polite and greets back, while giving :wave: reaction under
     the users greeting.
-}
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

{- |
The only exported function.
Based on Discord server event, a proper response is prepared.
The message is parsed, and if it is from bot or no command is recognized, our bot
will remain silent.
If an event handler throws an exception, discord-haskell will continue to run.
-}
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis (MessageCreate m)
    | fromBot m = pure ()
    | startsWithPrefix checkPrefix m = do
        embed <- createSearchedEmbed $ getPattern m
        void $ restCall dis $ R.CreateMessageEmbed (messageChannel m) "" embed
    | startsWithPrefix amuseMePrefix m = do
        embed <- getRandomPostEmbed
        void $ restCall dis $ R.CreateMessageEmbed (messageChannel m)
                                                   "Maybe check this out:"
                                                   embed
    | toBot m = do
        restCall dis (R.CreateReaction (messageChannel m, messageId m) "wave")
        threadDelay (1 * 10 ^ 6)
        restCall dis (R.CreateMessage (messageChannel m) (greeting m))
        pure ()
    | otherwise = pure ()
eventHandler _ _ = pure ()

-- | Prepares a personal greeting.
greeting :: Message -> T.Text
greeting m = "Hi " `T.append` userName (messageAuthor m) `T.append` "!"

-- | Checks whether the message comes from another bot.
fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

-- | Checks whether the message contains phrase /bot/.
toBot :: Message -> Bool
toBot = ("bot" `T.isInfixOf`) . T.map toLower . messageText

-- | A prefix for the docs command.
checkPrefix :: IsString a => a
checkPrefix = "!check "

-- | A prefix for the random post command.
amuseMePrefix :: IsString a => a
amuseMePrefix = "!amuseme"

-- | Extracts search pattern for the docs command.
getPattern :: Message -> String
getPattern = T.unpack . T.drop (T.length checkPrefix) . messageText

-- | Checks whether message starts with given prefix.
startsWithPrefix :: T.Text -> Message -> Bool
startsWithPrefix pref = (pref `T.isPrefixOf`) . T.map toLower . messageText
