#! /usr/bin/env runghc

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Protolude

import Data.Aeson (ToJSON, object, (.=))
import qualified Data.ByteString.Base64.URL as B64
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (String)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, iso8601DateFormat, parseTimeM)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.Human (humanReadableTime')
import Lens.Micro.Platform ((^..), (<&>), traversed)
import Lens.Micro.Aeson (_Integral, _String, _Value, key, values)
import Network.HTTP.Client (HttpException, Manager, httpLbs, parseRequest, requestHeaders, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (getFeedItems, getItemLink, getItemTitle, getItemPublishDate)
import Text.Microstache (compileMustacheText, renderMustache)
import Text.RawString.QQ (r)

-- TODO
-- youtube
-- twitter

main :: IO ()
main = do
  httpManager <- newTlsManager
  let
    eTmpl = compileMustacheText "html" htmlTemplate
  when (isLeft eTmpl) (panic $ show eTmpl)
  let
    Right tmpl = eTmpl

  eSections <- mapM (runFetch httpManager . fetchSection) purescript
  let
    errs = lefts eSections
    sections = rights eSections

  putLText . renderMustache tmpl $ object ["sections" .= sections]

  unless (null errs) $ do
    putText (mconcat . replicate 72 $ "-")
    mapM_ (putText . show) errs

runFetch :: Manager -> ReaderT Manager (ExceptT Error IO) a -> IO (Either Error a)
runFetch m f =
  runExceptT (runReaderT f m)

fetchSection :: FetchM m => Section -> m DisplaySection
fetchSection Section{title,sources} = do
  currentTime <- liftIO getCurrentTime
  ss <- mapM fetch sources
        <&> mconcat
        <&> Seq.sortBy (comparing $ Down . datetime)
        <&> Seq.take 10
        <&> fmap (\(Item t l d) -> DisplayItem t l d (humanReadableTime' currentTime d))
  pure (DisplaySection title ss)

httpGet :: FetchM m => Text -> m LByteString
httpGet url = do
  manager <- ask
  let
    requestE
      = parseRequest (toS url)

  when (isLeft requestE) (throwError (InvalidURL url))

  let
    Right request
      = requestE
    req
      = request { requestHeaders = [("user-agent", "joncfoo-news")] }
  responseE <-
    (liftIO . try) (httpLbs req manager)
  case responseE of
    Left e ->
      throwError (HttpE e)
    Right response ->
      pure (responseBody response)

fetch :: FetchM m => Source Text -> m (Seq Item)
fetch (Github keyword) = do
  let
    sevenDaysAgo =
      60 * 60 * 24 * (-7)
  date <- liftIO getCurrentTime
          <&> addUTCTime sevenDaysAgo
          <&> formatTime defaultTimeLocale "%Y-%m-%d"
          <&> toS
  let
    url = "https://api.github.com/search/repositories?q=pushed:%3E"
          <> date <> "+language:" <> keyword <> "&sort=updated&order=desc"
  json <- httpGet url
  let
    parseTime :: String -> Maybe UTCTime
    parseTime =
      parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
    results =
      json ^.. key "items" . values
    titles =
      results ^.. traversed . _Value . key "name" . _String
    links =
      results ^.. traversed . _Value . key "html_url" . _String
    dates =
      results ^.. traversed . _Value . key "updated_at" . _String
      & fmap (parseTime . toS)
      & catMaybes

  unless (length titles == length links && length links == length dates) $
    throwError (GithubE "length of pieces were not the same")

  pure (mkItems titles links dates)

fetch (Reddit subreddit) = do
  let
    url = "https://www.reddit.com/r/" <> subreddit <> "/new/.json"
  json <- httpGet url
  let
    results =
      json ^.. key "data" . key "children" . values . key "data"
    titles =
      results ^.. traversed . key "title" . _String
    links =
      results ^.. traversed . key "permalink" . _String
      & fmap ("https://www.reddit.com" <>)
    dates
      = results ^.. traversed . key "created_utc" . _Integral
        & fmap (posixSecondsToUTCTime . fromInteger)

  unless (length titles == length links && length links == length dates) $
    throwError (RedditE "length of pieces were not the same")

  pure (mkItems titles links dates)

fetch PSReleases = do
  fromFeed "https://github.com/purescript/purescript/releases.atom"
  >>= (pure . fmap addlink)
  where
    addlink (Item t l d) = Item t ("https://github.com" <> l) d

fetch (Stackoverflow keyword) =
  fromFeed ("https://stackoverflow.com/feeds/tag?tagnames=" <> keyword <> "&sort=newest")

fetch (Googlegroup keyword) =
  fromFeed ("https://groups.google.com/forum/feed/" <> keyword <> "/topics/atom.xml?num=10")

fetch (RSS url) =
  fromFeed url

fromFeed :: FetchM m => Text -> m (Seq Item)
fromFeed url = do
  mFeed <- parseFeedSource <$> httpGet url

  when (isNothing mFeed) (throwError . FeedE $ "Failed to parse feed: " <> url)

  let
    Just feed = mFeed
    toItem (Just t, Just l, Just dt) = Just (Item t l dt)
    toItem _ = Nothing
    items' =
      feed
      & getFeedItems
      & fmap (\i -> (getItemTitle i, getItemLink i, join $ getItemPublishDate i))
      & fmap toItem
      & catMaybes
      & Seq.fromList
  pure items'

-- nasty! :D
mkItems :: [Text] -> [Text] -> [UTCTime] -> Seq Item
mkItems titles links dates =
  zipWith Item titles links
  & flip (zipWith identity) dates
  & Seq.fromList

type FetchM m
  = ( MonadReader Manager m
    , MonadError Error m
    , MonadIO m
    )

data Error
  = HttpE HttpException
  | InvalidURL Text
  | FeedE Text
  | GithubE Text
  | RedditE Text
  deriving Show

purescript :: [Section]
purescript =
  [ Section "Blog Posts"
    [ RSS "http://blog.functorial.com/feed.rss"
    , RSS "https://qiita.com/kimagure/feed.atom"
    , RSS "http://mutanatum.com/atom.xml"
    ]
  , Section "Github" [Github "purescript"]
  , Section "Reddit" [Reddit "purescript"]
  , Section "Stack Overflow" [Stackoverflow "purescript"]
  , Section "Google Groups" [Googlegroup "purescript"]
  , Section "Releases" [PSReleases]
  ]

haskell :: [Section]
haskell =
  [ Section "Blog Posts"
    [ RSS "https://kseo.github.io/atom.xml"
    ]
  , Section "Reddit" [Reddit "haskell"]
  , Section "Stack Overflow" [Stackoverflow "haskell"]
  ]

data Source a
  = Github a
  | Googlegroup a
  | Pursuit
  | Reddit a
  | PSReleases
  | RSS a
  | Stackoverflow a
  | Twitter a
  | Youtube a

data Section =
  Section
  { title :: Text
  , sources :: [Source Text]
  }

data Item
  = Item
  { title    :: Text
  , link     :: Text
  , datetime :: UTCTime
  }

data DisplaySection =
  DisplaySection
  { title :: Text
  , items :: Seq DisplayItem
  } deriving (Generic, ToJSON)

data DisplayItem =
  DisplayItem
  { title        :: Text
  , link         :: Text
  , time         :: UTCTime
  , readableTime :: String
  } deriving (Generic, ToJSON)

htmlTemplate :: LText
htmlTemplate = [r|<!doctype html>
<html>
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>News</title>
<link rel="stylesheet" href="styles.css">
</head>
<body class="purescript">
<header>
  <a class="nav selected" data-value="purescript" href="#">PureScript</a>
  <span class="pipe">|</span>
  <a class="nav" data-value="haskell" href="#">Haskell</a>
</header>
<main>
  {{#sections}}
    <section>
      <h3>{{title}}</h3>
      <ul>
        {{#items}}
          <li><a href="{{{link}}}">{{title}}</a> <time pubdate="{{time}}">&mdash; {{readableTime}}</time></li>
        {{/items}}
      </ul>
    </section>
  {{/sections}}
</main>
</body>
</html>
|]
