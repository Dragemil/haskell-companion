{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : Discord.Events
Description : Handling events for the Discord bot.

Every user input in the Discord server is parsed in this module
and if a command is recognized an appropriate response is prepared.
Known commands are:

     * /!help/ - for information about the bots commands,
     * /!check \<number\> \<pattern\>/ - for checking /\<pattern\>/ in the local Hoogle
     documentation database, the \<number\> result is presented or first 10 if
     the \<number\> is 0,
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
import           Data.Maybe                     ( maybe )
import           Data.String                    ( IsString )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests              as R

import           Hoogle.Searching

import           Posts.Atom
import           Text.Read                      ( readMaybe )

{- |
The only exported function.
Based on Discord server event, a proper response is prepared.
The message is parsed, and if it is from a bot or no command is recognized, our bot
will remain silent.
If an event handler throws an exception, discord-haskell will continue to run.
-}
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis (MessageCreate m)
    | fromBot m = pure ()
    | startsWithPrefix helpPrefix m = do
        restCall
            dis
            (R.CreateReaction (messageChannel m, messageId m) "ok_hand")
        restCall dis (R.CreateMessage (messageChannel m) helpMessage)
        pure ()
    | startsWithPrefix checkPrefix m = do
        request <-
            maybe
                    (pure $ R.CreateMessage (messageChannel m)
                                            checkCommandReminder
                    )
                    (uncurry createSearchedEmbed >=> pure . uncurry
                        (R.CreateMessageEmbed (messageChannel m))
                    )
                $ getPattern m
        void $ restCall dis request
    | startsWithPrefix amuseMePrefix m = do
        embed <- getRandomPostEmbed
        void $ restCall dis $ R.CreateMessageEmbed (messageChannel m)
                                                   "Maybe check this out:"
                                                   embed
    | toBot m = do
        restCall dis (R.CreateReaction (messageChannel m, messageId m) "wave")
        restCall dis (R.CreateMessage (messageChannel m) (greeting m))
        pure ()
    | otherwise = pure ()
eventHandler _ _ = pure ()

-- | Prepares a personal greeting.
greeting :: Message -> T.Text
greeting m = "Hi " `T.append` userName (messageAuthor m) `T.append` "!"

-- | A message reminding of the structure of the docs command.
checkCommandReminder :: IsString a => a
checkCommandReminder = "The command is _!check <number> <pattern>_"

-- | A message to display on help command.
helpMessage :: IsString a => a
helpMessage =
    "\
\ Here are all my commands, through which I can help you:\n\
\ _!help_ - I'll show you this message,\n\
\ _!amuseme_ - I'll bring you one random\
\ http://planet.haskell.org/ post,\n\
\ _!check <number> <pattern>_ - I'll find you the Hoogle docs matching\
\ _<pattern>_. If the _<number>_ is 0 I'll show you brief info about what I've\
\ found, otherwise you'll get the details about the result.\n\
\"

-- | Checks whether the message comes from another bot.
fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

-- | Checks whether the message contains phrase /bot/.
toBot :: Message -> Bool
toBot = ("bot" `T.isInfixOf`) . T.map toLower . messageText

-- | A prefix for the help command.
helpPrefix :: IsString a => a
helpPrefix = "!help"

-- | A prefix for the docs command.
checkPrefix :: IsString a => a
checkPrefix = "!check "

-- | A prefix for the random post command.
amuseMePrefix :: IsString a => a
amuseMePrefix = "!amuseme"

-- | Extracts search pattern and the number for the docs command.
getPattern :: Message -> Maybe (String, Int)
getPattern message = (pat, ) <$> which
  where
    pat =
        dropWhile (== ' ')
            . dropWhile (/= ' ')
            . dropWhile (== ' ')
            $ withoutPrefix
    which = readMaybe . takeWhile (/= ' ') . dropWhile (== ' ') $ withoutPrefix
    withoutPrefix =
        drop (length (checkPrefix :: String)) . T.unpack . messageText $ message

-- | Checks whether the message starts with given prefix.
startsWithPrefix :: T.Text -> Message -> Bool
startsWithPrefix pref = (pref `T.isPrefixOf`) . T.map toLower . messageText
