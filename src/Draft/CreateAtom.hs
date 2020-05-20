{-# LANGUAGE OverloadedStrings #-}

module Draft.CreateAtom
    ( atomFeed
    )
where

import qualified Text.Atom.Feed                as Atom
import qualified Text.Atom.Feed.Export         as Export

import           Data.Text                     as T
import           Data.Text.Lazy                 ( toStrict )

atomFeed :: Maybe Text
atomFeed = renderFeed examplePosts

data Post = Post
  { _postedOn :: Text
  , _url :: Text
  , _content :: Text
  }

examplePosts :: [Post]
examplePosts =
    [ Post "2000-02-02T18:30:00Z" "http://example.com/2" "Bar."
    , Post "2000-01-01T18:30:00Z" "http://example.com/1" "Foo."
    ]

toEntry :: Post -> Atom.Entry
toEntry (Post date url content) = (Atom.nullEntry
                                      url -- The ID field. Must be a link to validate.
                                      (Atom.TextString (T.take 20 content)) -- Title
                                      date
                                  )
    { Atom.entryAuthors = [Atom.nullPerson { Atom.personName = "J. Smith" }]
    , Atom.entryLinks   = [Atom.nullLink url]
    , Atom.entryContent = Just (Atom.HTMLContent content)
    }

feed :: [Post] -> Atom.Feed
feed posts = Atom.nullFeed
    "http://example.com/atom.xml" -- ID
    (Atom.TextString "Example Website") -- Title
    (case posts of
        Post latestPostDate _ _ : _ -> latestPostDate
        _                           -> "" -- Updated
    )

-- renderFeed :: Atom.Feed -> Maybe Text
-- renderFeed f = toStrict <$> Export.textFeed f

renderFeed :: [Post] -> Maybe Text
renderFeed posts =
    toStrict
        <$> (Export.textFeed $ (feed posts)
                { Atom.feedEntries = fmap toEntry posts
                , Atom.feedLinks   = [Atom.nullLink "http://example.com/"]
                }
            )


