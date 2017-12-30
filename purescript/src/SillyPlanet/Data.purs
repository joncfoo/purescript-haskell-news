module SillyPlanet.Data
  ( Page(..)
  , Group(..)
  , Item(..)
  ) where

import Data.Generic (class Generic)
import Data.Newtype (class Newtype)

newtype Page
  = Page
    { title  :: String
    , groups :: Array Group
    }

derive instance genericPage :: Generic Page
derive instance newtypePage :: Newtype Page _

newtype Group
  = Group
    { title :: String
    , items :: Array Item
    }

derive instance genericGroup :: Generic Group
derive instance newtypeGroup :: Newtype Group _

data Item
  = Link
    { title    :: String
    , link     :: String
    , time     :: String
    , niceTime :: String
    }
  | Podcast
    { title     :: String
    , link      :: String
    , enclosure :: String
    , time      :: String
    , niceTime  :: String
    }

derive instance genericItem :: Generic Item
