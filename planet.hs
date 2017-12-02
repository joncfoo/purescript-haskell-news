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
import Data.List (zipWith4)
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
import Text.Feed.Query (getFeedItems, getItemEnclosure, getItemLink, getItemTitle, getItemPublishDate)
import Text.Microstache (compileMustacheText, renderMustache)
import Text.RawString.QQ (r)

-- TODO
-- youtube
-- twitter

purescript :: Section
purescript =
  Section "purescript" $ Seq.fromList
  [ SubSection "Blog Posts"
    [ RSS "http://blog.functorial.com/feed.rss"
    , RSS "https://qiita.com/kimagure/feed.atom"
    , RSS "http://mutanatum.com/atom.xml"
    ]
  , SubSection "Github" [Github "purescript"]
  , SubSection "Reddit" [Reddit "purescript"]
  , SubSection "Stack Overflow" [Stackoverflow "purescript"]
  , SubSection "Google Groups" [Googlegroup "purescript"]
  , SubSection "Releases" [PSReleases]
  ]

haskell :: Section
haskell =
  Section "haskell" $ Seq.fromList
  [ SubSection "Blog Posts"
    [ RSS "https://kseo.github.io/atom.xml"
    ]
  , SubSection "Reddit" [Reddit "haskell"]
  , SubSection "Stack Overflow" [Stackoverflow "haskell"]
  ]

podcasts :: Section
podcasts =
  Section "podcasts" $ Seq.fromList
  [ SubSection "FP Podcasts"
    [ Podcast "http://feeds.soundcloud.com/users/soundcloud:users:239787249/sounds.rss"  -- LambdaCast
    , Podcast "https://www.functionalgeekery.com/feed/mp3/" -- Functional Geekery
    , Podcast "http://www.magicreadalong.com/episode?format=rss" -- Magic Read Along
    , Podcast "http://www.haskellcast.com/feed.xml" -- Haskell Cast
    ]
  , SubSection "World Stories"
    [ Podcast "https://www.npr.org/rss/podcast.php?id=510324" -- NPR: Rough translation
    , Podcast "http://podcasts.files.bbci.co.uk/p02nq0lx.rss" -- BBC: The Documentary
    , Podcast "http://www.radiolab.org/feeds/podcast/"  -- RadioLab
    ]
  , SubSection "Music"
    [ Podcast "http://www.mennodejong.com/cloudcast/podcast.xml"  -- Menno de Jong
    , Podcast "https://www.thisisdistorted.com/repository/xml/paulvandyk1446481004.xml"  -- Paul van Dyk
    , Podcast "http://www.kyauandalbert.com/EuphonicSessions-Podcast-KyauandAlbert.xml"  -- Kyau and Albert
    , Podcast "http://jaytechmusic.com/jaytechmusic/jaytechmusic.xml"  -- Jaytech
    , Podcast "http://feeds.feedburner.com/SilkRoyalShowcase"  -- Silk Music Showcase
    , Podcast "https://www.thisisdistorted.com/repository/xml/SolarstonePureTrance1463143743.xml" -- Solarstone
    , Podcast "http://john00fleming.co.uk/mixes/podcasts/Nov08/podcast.xml"  -- John 00 Fleming
    , Podcast "http://internationaldepartures.podbean.com/feed/"  -- Shane 54
    , Podcast "https://www.thisisdistorted.com/repository/xml/1421850521.xml"  -- Roger Shah
    ]
  ]

main :: IO ()
main = do
  httpManager <- newTlsManager
  let
    eTmpl = compileMustacheText "html" htmlTemplate
  when (isLeft eTmpl) (panic $ show eTmpl)
  let
    Right tmpl
      = eTmpl
    sections =
      [purescript, haskell, podcasts]
    titles
      = fmap (\Section{title}-> object $ ["title" .= (title :: Text)]) sections

  eResult <- (runFetch httpManager . mapM fetchSection) sections
  case eResult of
    Left errs ->
      panic (show errs)
    Right displaySections ->
      putLText . renderMustache tmpl $ object [ "titles" .= titles
                                              , "sections" .= rotate' 1 displaySections]
runFetch :: Manager -> ReaderT Manager (ExceptT Error IO) a -> IO (Either Error a)
runFetch m f =
  runExceptT (runReaderT f m)

fetchSection :: FetchM m => Section -> m DisplaySection
fetchSection Section{title,subsections} = do
  dss <- mapM fetchSubSection subsections
  pure (DisplaySection title dss)

fetchSubSection :: FetchM m => SubSection -> m DisplaySubSection
fetchSubSection SubSection{title,sources} = do
  currentTime <- liftIO getCurrentTime
  ss <- mapM fetch sources
        <&> mconcat
        <&> Seq.sortBy (comparing $ Down . datetime)
        <&> Seq.take 10
        <&> fmap (\(Item t l e d) -> DisplayItem t l e d (humanReadableTime' currentTime d))
  pure (DisplaySubSection title ss)

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
  fromFeed "https://github.com/purescript/purescript/releases.atom" False
  >>= (pure . fmap addlink)
  where
    addlink (Item t l e d) = Item t ("https://github.com" <> l) e d

fetch (Stackoverflow keyword) =
  fromFeed ("https://stackoverflow.com/feeds/tag?tagnames=" <> keyword <> "&sort=newest") False

fetch (Googlegroup keyword) =
  fromFeed ("https://groups.google.com/forum/feed/" <> keyword <> "/topics/atom.xml?num=10") False

fetch (RSS url) =
  fromFeed url False

fetch (Podcast url) =
  fromFeed url True

fromFeed :: FetchM m => Text -> Bool -> m (Seq Item)
fromFeed url enclosure = do
  mFeed <- parseFeedSource <$> httpGet url

  when (isNothing mFeed) (throwError . FeedE $ "Failed to parse feed: " <> url)

  let
    Just feed =
      mFeed
    toItem (Just t, Just l, e, Just dt) =
      Just (Item t l (if enclosure then e else Nothing) dt)
    toItem _ =
      Nothing
    fromEnclosure i =
      case getItemEnclosure i of
        Just (uri, _, _) -> Just uri
        _ -> Nothing
    items' =
      feed
      & getFeedItems
      & fmap (\i -> (getItemTitle i, getItemLink i, fromEnclosure i, join $ getItemPublishDate i))
      & fmap toItem
      & catMaybes
      & Seq.fromList
  pure items'

-- nasty! :D
mkItems :: [Text] -> [Text] -> [UTCTime] -> Seq Item
mkItems titles links dates =
  Seq.fromList $ zipWith4 Item titles links (repeat Nothing) dates

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs

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

data Source a
  = Github a
  | Googlegroup a
  | Pursuit
  | Reddit a
  | Podcast a
  | PSReleases
  | RSS a
  | Stackoverflow a
  | Twitter a
  | Youtube a

data SubSection =
  SubSection
  { title   :: Text
  , sources :: [Source Text]
  }

data Section =
  Section
  { title       :: Text
  , subsections :: Seq SubSection
  }

data Item
  = Item
  { title     :: Text
  , link      :: Text
  , enclosure :: Maybe Text
  , datetime  :: UTCTime
  }

data DisplaySection =
  DisplaySection
  { title       :: Text
  , subsections :: Seq DisplaySubSection
  } deriving (Generic, ToJSON)

data DisplaySubSection =
  DisplaySubSection
  { title :: Text
  , items :: Seq DisplayItem
  } deriving (Generic, ToJSON)

data DisplayItem =
  DisplayItem
  { title        :: Text
  , link         :: Text
  , enclosure    :: Maybe Text
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
<body>
<header>
  <nav>
    {{#titles}}
    <a href="#{{title}}">{{title}}</a>
    <div class="pipe"></div>
    {{/titles}}
    </nav>
</header>
<main role="main">
  {{#sections}}
  <div id="{{title}}" class="view">
    {{#subsections}}
    <section>
      <h3>{{title}}</h3>
      <ul>
        {{#items}}
          <li><a href="{{{link}}}" data-enclosure="{{enclosure}}">{{title}}</a> <time pubdate="{{time}}">&mdash; {{readableTime}}</time></li>
        {{/items}}
      </ul>
    </section>
    {{/subsections}}
  </div>
  {{/sections}}
</main>
</body>
</html>
|]
