#! /usr/bin/env runghc

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

import Protolude

import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Sequence              as Seq

import Data.Aeson              (FromJSON, ToJSON, object, (.=))
import Data.List               (zipWith4)
import Data.Sequence           (Seq)
import Data.String             (String, fromString)
import Data.Time               (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime, iso8601DateFormat,
                                parseTimeM)
import Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import Data.Time.Format.Human  (humanReadableTime')
import Lens.Micro.Aeson        (key, values, _Integral, _String, _Value)
import Lens.Micro.Platform     (traversed, (<&>), (^..))
import Network.HTTP.Client     (HttpException, Manager, httpLbs, parseRequest, requestHeaders, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Text.Feed.Import        (parseFeedSource)
import Text.Feed.Query         (getFeedItems, getItemEnclosure, getItemLink, getItemPublishDate, getItemTitle)
import Text.Microstache        (compileMustacheText, renderMustache)
import Text.RawString.QQ       (r)

purescript :: Page
purescript =
  Page "purescript" $ Seq.fromList
  [ Group "Blog Posts"
    [ RSS "http://blog.functorial.com/feed.rss"
    , RSS "https://qiita.com/kimagure/feed.atom"
    , RSS "http://mutanatum.com/atom.xml"
    ]
  , Group "Github" [Github "purescript"]
  , Group "Reddit" [Reddit "purescript"]
  , Group "Stack Overflow" [Stackoverflow "purescript"]
  , Group "Google Groups" [Googlegroup "purescript"]
  , Group "Releases" [PSReleases]
  ]

haskell :: Page
haskell =
  Page "haskell" $ Seq.fromList
  [ Group "Blog Posts"
    [ RSS "https://kseo.github.io/atom.xml"
    ]
  , Group "Reddit" [Reddit "haskell"]
  , Group "Stack Overflow" [Stackoverflow "haskell"]
  ]

podcasts :: Page
podcasts =
  Page "podcasts" $ Seq.fromList
  [ Group "FP Podcasts"
    [ Podcast "http://feeds.soundcloud.com/users/soundcloud:users:239787249/sounds.rss"  -- LambdaCast
    , Podcast "https://www.functionalgeekery.com/feed/mp3/" -- Functional Geekery
    , Podcast "http://www.magicreadalong.com/episode?format=rss" -- Magic Read Along
    , Podcast "http://www.haskellcast.com/feed.xml" -- Haskell Cast
    ]
  , Group "World Stories"
    [ Podcast "https://www.npr.org/rss/podcast.php?id=510324" -- NPR: Rough translation
    , Podcast "http://podcasts.files.bbci.co.uk/p02nq0lx.rss" -- BBC: The Documentary
    , Podcast "http://www.radiolab.org/feeds/podcast/"  -- RadioLab
    ]
  , Group "Music"
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
  css <- readFile "output/styles.css"
  javascript <- readFile "js/snippet.js"
  httpManager <- newTlsManager
  let
    eTmpl
      = compileMustacheText "html" htmlTemplate
  when (isLeft eTmpl) (panic $ show eTmpl)
  let
    Right tmpl
      = eTmpl
    pages
      = [purescript, haskell, podcasts]
    titles
      = (\Page{title}-> object $ ["title" .= (title :: Text)])
        <$> pages

  eResult <- (runFetch httpManager . mapM fetchSection) pages
  case eResult of
    Left errs ->
      panic (show errs)
    Right displaySections ->
      -- rotate pages since the CSS needs the first page to actually be last (due to how selectors work)
      object [ "titles" .= titles
             , "pages" .= rotate' 1 displaySections
             , "css" .= css
             , "javascript" .= javascript
             ]
      & renderMustache tmpl
      & putLText

runFetch :: Manager -> ReaderT Manager (ExceptT Error IO) a -> IO (Either Error a)
runFetch m f =
  runExceptT (runReaderT f m)

fetchSection :: Fetch m => Page -> m DisplayPage
fetchSection Page{title,groups} = do
  dss <- mapM fetchSubSection groups
  pure (DisplayPage title dss (title == "podcasts"))

fetchSubSection :: Fetch m => Group -> m DisplayGroup
fetchSubSection Group{title,sources} = do
  currentTime <- liftIO getCurrentTime
  ss <- mapM fetch sources
        <&> mconcat
        <&> Seq.sortBy (comparing $ Down . datetime)
        <&> Seq.take 10
        <&> fmap (\(Item t l e d) -> DisplayItem t l e d (humanReadableTime' currentTime d))
  pure (DisplayGroup title ss)

httpGet :: Fetch m => Text -> m LByteString
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

fetch :: Fetch m => Source -> m (Seq Item)
fetch (Github (Keyword language)) = do
  let
    sevenDaysAgo =
      60 * 60 * 24 * (-7)
  date <- liftIO getCurrentTime
          <&> addUTCTime sevenDaysAgo
          <&> formatTime defaultTimeLocale "%Y-%m-%d"
          <&> toS
  let
    url
      = "https://api.github.com/search/repositories?q=pushed:%3E"
        <> date <> "+language:" <> language <> "&sort=updated&order=desc"
  json <- httpGet url
  let
    parseTime :: String -> Maybe UTCTime
    parseTime
      = parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ"))
    results
      = json ^.. key "items" . values
    titles
      = results ^.. traversed . _Value . key "name" . _String
    links
      = results ^.. traversed . _Value . key "html_url" . _String
    dates
      = results ^.. traversed . _Value . key "updated_at" . _String
        & fmap (parseTime . toS)
        & catMaybes

  unless (length titles == length links && length links == length dates) $
    throwError (GithubE "length of pieces were not the same")

  pure (mkItems titles links dates)

fetch (Reddit (Keyword subreddit)) = do
  let
    url
      = "https://www.reddit.com/r/" <> subreddit <> "/new/.json"
  json <- httpGet url
  let
    results
      = json ^.. key "data" . key "children" . values . key "data"
    titles
      = results ^.. traversed . key "title" . _String
    links
      = results ^.. traversed . key "permalink" . _String
        <&> ("https://www.reddit.com" <>)
    dates
      = results ^.. traversed . key "created_utc" . _Integral
        <&> (posixSecondsToUTCTime . fromInteger)

  unless (length titles == length links && length links == length dates) $
    throwError (RedditE "length of pieces were not the same")

  pure (mkItems titles links dates)

fetch PSReleases =
  fromFeed "https://github.com/purescript/purescript/releases.atom" False
  <&> fmap addlink
  where
    addlink (Item t l e d) = Item t ("https://github.com" <> l) e d

fetch (Stackoverflow (Keyword tag)) =
  let
    url
      = URL $ "https://stackoverflow.com/feeds/tag?tagnames=" <> tag <> "&sort=newest"
  in
    fromFeed url False

fetch (Googlegroup (Keyword ggroup)) =
  let
    url
      = URL $ "https://groups.google.com/forum/feed/" <> ggroup <> "/topics/atom.xml?num=10"
  in
    fromFeed url False

fetch (RSS url) =
  fromFeed url False

fetch (Podcast url) =
  fromFeed url True

fromFeed :: Fetch m => URL -> Bool -> m (Seq Item)
fromFeed (URL url) enclosure = do
  mFeed <- parseFeedSource <$> httpGet url

  when (isNothing mFeed) $
    (throwError . FeedE $ "Failed to parse feed: " <> url)

  let
    Just feed
      = mFeed
    toItem (Just t, Just l, e, Just dt)
      = Just (Item t l (if enclosure then e else Nothing) dt)
    toItem _
      = Nothing
    fromEnclosure i
      = case getItemEnclosure i of
          Just (uri, _, _) -> Just uri
          _                -> Nothing
    items'
      = getFeedItems feed
        <&> (\i -> (getItemTitle i, getItemLink i, fromEnclosure i, join $ getItemPublishDate i))
        <&> toItem
        & catMaybes
        & Seq.fromList
  pure items'

mkItems :: [Text] -> [Text] -> [UTCTime] -> Seq Item
mkItems titles links dates =
  zipWith4 Item titles links (repeat Nothing) dates
  & Seq.fromList

rotate' :: Int -> [a] -> [a]
rotate' _ [] = []
rotate' n xs = zipWith const (drop n (cycle xs)) xs

type Fetch m
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

newtype Keyword = Keyword Text
  deriving (Generic, FromJSON)

instance IsString Keyword where
  fromString = Keyword . toS

newtype URL = URL Text
  deriving (Generic, FromJSON)

-- this is kinda not cool?
instance IsString URL where
  fromString = URL . toS

data Source
  = Github Keyword
  | Googlegroup Keyword
  | Pursuit
  | Reddit Keyword
  | Podcast URL
  | PSReleases
  | RSS URL
  | Stackoverflow Keyword
  | Twitter Text
  | Youtube Text
  deriving (Generic, FromJSON)

data Page =
  Page
  { title  :: Text
  , groups :: Seq Group
  } deriving (Generic, FromJSON)

data Group =
  Group
  { title   :: Text
  , sources :: [Source]
  } deriving (Generic, FromJSON)

data Item
  = Item
  { title     :: Text
  , link      :: Text
  , enclosure :: Maybe Text
  , datetime  :: UTCTime
  } deriving (Generic, FromJSON)

data DisplayPage =
  DisplayPage
  { title  :: Text
  , groups :: Seq DisplayGroup
  , isPodcasts :: Bool
  } deriving (Generic, ToJSON)

data DisplayGroup =
  DisplayGroup
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
<style>
{{{css}}}
</style>
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
  {{#pages}}
  <div id="{{title}}" class="view">
    {{#isPodcasts}}
    <div id="player">
        <label>Playback Rate
            <select id="playbackRate">
                <option value="1" selected>1x</option>
                <option value="1.2">1.2x</option>
                <option value="1.3">1.3x</option>
                <option value="1.4">1.4x</option>
                <option value="1.5">1.5x</option>
                <option value="1.6">1.6x</option>
                <option value="1.7">1.7x</option>
                <option value="1.8">1.8x</option>
                <option value="1.9">1.9x</option>
                <option value="2">2x</option>
            </select>
        </label>
        <audio controls></audio>
    </div>
    {{/isPodcasts}}
    {{#groups}}
    <section>
      <h3>{{title}}</h3>
      <ul>
        {{#items}}
          <li><a href="{{{link}}}" data-enclosure="{{enclosure}}">{{title}}</a> <time pubdate="{{time}}">&mdash; {{readableTime}}</time></li>
        {{/items}}
      </ul>
    </section>
    {{/groups}}
  </div>
  {{/pages}}
</main>
<script>
{{{javascript}}}
</script>
</body>
</html>
|]
