---
title: "Haskell Companion - szkic projektu"
author: "Emil Dragańczuk"
output: pdf
date: "9 Maja 2020"
---

# Funkcjonalności
Projekt ma za zadanie stworzyć bota do serwerów Discorda. Będzie on wyszukiwał dokumentację Haskella z Hoogle na komendę oraz dostarczał losowe artykuły z [planet haskell][1] na komendę lub okresowo samodzielnie.

# Architektura
Baza bota będzie wzięta z biblioteki. Hoogle również ma swoją bibliotekę, więc skorzystamy z niej aby wydobywać z niego dokumentację. Do wydobywania postów z [planet haskell][1] wykorzystamy to że są zapisane w formacie Atom oraz RSS, które sparsuje nam kolejna biblioteka. Posty w odpowiednim formacie uzyskamy z żądania HTTP. Do okresowego wyświetlania blogów również wykorzystamy bibliotekę. Wszystkie wymagane biblioteki znajdują się poniżej.

# Biblioteki do wykorzystania

* [discord-haskell](https://hackage.haskell.org/package/discord-haskell) - podstawowe API Discordowego bota w Haskellu
* [hoogle](http://hackage.haskell.org/package/hoogle-5.0.17.15) - API do dokumentacji Hoogle
* [feed](https://hackage.haskell.org/package/feed) - parsowanie RSS, w którym są blogi z [planet haskell][1]
* [http-conduit](https://hackage.haskell.org/package/http-conduit) - do zapytań do [planet haskell][1]
* [scheduler](https://hackage.haskell.org/package/scheduler) - do regularnie wyświetlanych blogów z [planet haskell][1]

# Schemat działania bota

Poniżej zarys funkcji przekazującej dokumentację z Hoogle na komendę użytkownika:

``` Haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do userFacingError <- runDiscord $ def
        { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"
        , discordOnEvent = eventHandler }
    TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
    MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (4 * 10^6)
        _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
        pure ()
    _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
```

[1]: http://planet.haskell.org/
