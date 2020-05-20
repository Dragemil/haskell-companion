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
