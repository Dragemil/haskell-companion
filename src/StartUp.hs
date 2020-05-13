{-# LANGUAGE OverloadedStrings #-}

module StartUp
    ( sayHello
    ) where

import Control.Monad
import Control.Concurrent
import Data.Char (toLower)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- Greets on start and when someone says "bot"
sayHello :: IO ()
sayHello = do
  tok <- token

  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = putStrLn "Bye, I got to go"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^6)
  TIO.putStrLn t

token :: IO T.Text
token = TIO.readFile "./secrets/bot-auth-token"

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  Right partialGuilds <- restCall dis R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall dis $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall dis $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> void $ restCall dis $ R.CreateMessage (channelId c) "Hello! At your service"
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
      MessageCreate m -> when (not (fromBot m) && isToBot m) $ do
        restCall dis (R.CreateReaction (messageChannel m, messageId m) "wave")
        threadDelay (1 * 10^6)
        restCall dis (R.CreateMessage (messageChannel m) (greeting m))
        pure ()
      _ -> pure ()

greeting :: Message -> T.Text
greeting m = "Hi " `T.append` userName (messageAuthor m) `T.append` "!"

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isToBot :: Message -> Bool
isToBot = ("bot" `T.isInfixOf`) . T.map toLower . messageText
