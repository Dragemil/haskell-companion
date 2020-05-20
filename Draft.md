# Haskell Companion - szkic projektu

## Funkcjonalności

Projekt ma za zadanie stworzyć bota do serwerów Discorda. Będzie on wyszukiwał dokumentację Haskella z Hoogle na komendę oraz dostarczał losowe artykuły z [planet haskell][1] na komendę lub okresowo samodzielnie.

## Architektura

Baza bota będzie wzięta z biblioteki. Hoogle również ma swoją bibliotekę, więc skorzystamy z niej aby wydobywać z niego dokumentację. Na start wygenerujemy do pliku lokalną bazę danych funkcji z Hoogle, a każde zapytanie o dokumentację będzie korzystało z tej bazy. Do wydobywania postów z [planet haskell][1] wykorzystamy to że są zapisane w formacie Atom oraz RSS, które sparsuje nam kolejna biblioteka. Posty w odpowiednim formacie uzyskamy z żądania HTTP, które początkowo sparsujemy do XML a dopiero potem do Atom. Do okresowego wyświetlania blogów również wykorzystamy bibliotekę. Wymagane biblioteki znajdują się poniżej.

## Biblioteki do wykorzystania

* [discord-haskell](https://hackage.haskell.org/package/discord-haskell) - podstawowe API Discordowego bota w Haskellu
* [hoogle](http://hackage.haskell.org/package/hoogle-5.0.17.15) - API do dokumentacji Hoogle
* [feed](https://hackage.haskell.org/package/feed) - parsowanie standardu Atom, w którym udostępniane są blogi z [planet haskell][1]
* [xml-conduit](http://hackage.haskell.org/package/xml-conduit-1.9.0.0) - żądanie do [planet haskell][1] na początku będzie parsowane do XML, a dopiero potem do Atom
* [http-conduit](https://hackage.haskell.org/package/http-conduit) - do zapytań do [planet haskell][1] o blogi
* [scheduler](https://hackage.haskell.org/package/scheduler) - do regularnie wyświetlanych blogów z [planet haskell][1]

<div style="page-break-after: always;"></div>

## Schemat działania bota

Poniżej zarys funkcji przekazującej dokumentację z Hoogle na komendę użytkownika.
Bot sczytuje wiadomości na kanale textowym, a te zaczynające się odpowiednią frazą interpretuje jako zapytania o dokumentacje, które sprawdzi w lokalnie zaimportowanej bazie dokumentacji Hoogle, a następnie wyśle link do pierwszej z nich na kanał tekstowy.

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

<div style="page-break-after: always;"></div>

Poniżej prototyp żądania do [planet haskell][1], które potem będzie parsowane do XML, a z XML do Atoma, co umożliwi wydobycie różnych danych z postów.
``` Haskell
{-# LANGUAGE OverloadedStrings #-}

module Draft.ImportPosts
    ( getFeed
    )
where

import           Control.Monad.IO.Class
import qualified Text.Atom.Feed                as Atom
import qualified Text.Atom.Feed.Import         as Import
import           Network.HTTP.Simple
import           Text.XML
--import           Data.XML.Types
import           Data.Text.Lazy.Encoding
import           Data.Text.Lazy                 ( toStrict )

--elementFeed

httpXML :: MonadIO m => Request -> m Document
httpXML req = parseText_ def . decodeUtf8 . getResponseBody <$> httpLBS req

planetHaskellRequest :: Request
planetHaskellRequest = addRequestHeader "Accept" "application/atom+xml"
    $ parseRequest_ "http://planet.haskell.org/atom.xml"

getFeed :: MonadIO m => m (Maybe Atom.Feed)
getFeed = do
    doc <- httpXML planetHaskellRequest
    return . Import.elementFeed $ documentRoot doc
```

[1]: http://planet.haskell.org/
