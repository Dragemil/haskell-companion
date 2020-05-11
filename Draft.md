# Haskell Companion - szkic projektu

## Funkcjonalności

Projekt ma za zadanie stworzyć bota do serwerów Discorda. Będzie on wyszukiwał dokumentację Haskella z Hoogle na komendę oraz dostarczał losowe artykuły z [planet haskell][1] na komendę lub okresowo samodzielnie.

## Architektura

Baza bota będzie wzięta z biblioteki. Hoogle również ma swoją bibliotekę, więc skorzystamy z niej aby wydobywać z niego dokumentację. Na start wygenerujemy do pliku lokalną bazę danych funkcji z Hoogle, a każde zapytanie o dokumentację będzie korzystało z tej bazy. Do wydobywania postów z [planet haskell][1] wykorzystamy to że są zapisane w formacie Atom oraz RSS, które sparsuje nam kolejna biblioteka. Posty w odpowiednim formacie uzyskamy z żądania HTTP. Do okresowego wyświetlania blogów również wykorzystamy bibliotekę. Wszystkie wymagane biblioteki znajdują się poniżej.

## Biblioteki do wykorzystania

* [discord-haskell](https://hackage.haskell.org/package/discord-haskell) - podstawowe API Discordowego bota w Haskellu
* [hoogle](http://hackage.haskell.org/package/hoogle-5.0.17.15) - API do dokumentacji Hoogle
* [feed](https://hackage.haskell.org/package/feed) - parsowanie RSS, w którym są blogi z [planet haskell][1]
* [http-conduit](https://hackage.haskell.org/package/http-conduit) - do zapytań do [planet haskell][1]
* [scheduler](https://hackage.haskell.org/package/scheduler) - do regularnie wyświetlanych blogów z [planet haskell][1]

<div style="page-break-after: always;"></div>

## Schemat działania bota

Poniżej zarys funkcji przekazującej dokumentację z Hoogle na komendę użytkownika:

``` Haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import Hoogle

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- | Prints Hoogle package link on command "!check <pattern>"
hoogleExample :: IO ()
hoogleExample = do
    token <- TIO.readFile "./secrets/auth-token.secret"
    userFacingError <- runDiscord $ def
        { discordToken = token
        , discordOnEvent = eventHandler }
    TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = withDatabase databaseFile (db -> case event of
    MessageCreate m -> when (not (isFromBot m) && isCheck (messageText m)) $ do
        _ <- restCall dis (R.CreateMessage
            (messageChannel m)
            (getFirstLinkIfFound (getPattern $ messageText m) db))
        pure ()
    _ -> pure ())

getFirstLinkIfFound :: Text -> Database -> Text
getFirstLinkIfFound pattern db = case (searchDatabase db pattern) of
    (package:_) -> targetUrl package
    _ -> noEntry

getPattern :: Text -> Text
getPattern = drop 7 -- Lenght of "!check"

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

isCheck :: Text -> Bool
isCheck = ("!check " `isPrefixOf`) . toLower

noEntry :: Text
noEntry = "No entry with such pattern found in the database"

databaseFile :: FilePath
databaseFile = undefined
```

[1]: http://planet.haskell.org/
