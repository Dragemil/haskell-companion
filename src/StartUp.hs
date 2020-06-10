{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module      : StartUp
Description : A module responsible for configuring and starting up the bot.

Here are the functions responsible for configuring and starting up the bot.
-}
module StartUp
  ( runBot
  )
where

import           Control.Monad
import           Control.Concurrent

import           Data.String                    ( IsString )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

import           Discord
import           Discord.Events
import           Discord.Types
import qualified Discord.Requests              as R

-- Greets on start and when someone says "bot"
runBot :: IO ()
runBot = do
  tok <- token

  void . runDiscord $ def { discordToken   = tok
                          , discordOnStart = startHandler
                          , discordOnEnd   = putStrLn "The bot went off"
                          , discordOnEvent = eventHandler
                          , discordOnLog   = \s -> TIO.putStrLn s
                          }

-- | Token for authentication is read from a file which is ignored by git.
token :: IO T.Text
token = TIO.readFile "./secrets/bot-auth-token"

-- | Makes the bot say hello on every text channel there is on a server.
-- If the start handler throws an exception, discord-haskell will gracefully shutdown.
startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  Right partialGuilds <- restCall dis R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall dis $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall dis $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c : _) -> void $ restCall dis $ R.CreateMessage
        (channelId c)
        "Hello! At your service."
      _ -> pure ()

-- | Checks whether Discord channel is a text channel.
isTextChannel :: Channel -> Bool
isTextChannel ChannelText{} = True
isTextChannel _             = False
