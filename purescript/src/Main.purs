module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Classy.Event (preventDefault, toEvent)
import DOM.Event.Event (Event)
import Data.Argonaut.Generic.Aeson (decodeJson)
import Data.Array (concatMap, find, head)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Network.HTTP.Affjax as Affjax
import Network.RemoteData (RemoteData(..), fromEither, withDefault)
import SillyPlanet.Data (Group(..), Item(..), Page(..))

type Effect =
  forall e. Eff
  ( console :: CONSOLE
  , dom :: DOM
  , ajax :: AJAX
  , exception :: EXCEPTION
  , ref :: REF
  , avar :: AVAR | e) Unit

main :: Effect
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI pages unit body

type Input = Unit

type State =
  { pages       :: RemoteData String (Array Page)
  , currentPage :: Maybe Page
  }

data Query a
  = Initialize a
  | SetCurrentPage String a
  | PreventDefault Event (Query a)
  | AddPage a
  | AddGroup a
  | AddItem Group a

data Message = Unit

type UIEff eff =
  Aff
  ( ajax :: AJAX
  , dom :: DOM
  | eff
  )

pages :: forall eff. H.Component HH.HTML Query Input Message (UIEff eff)
pages =
  H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }
  where
    initialState :: State
    initialState =
      { pages: NotAsked
      , currentPage: Nothing
      }

    eval :: Query ~> H.ComponentDSL State Query Message (UIEff eff)
    eval = case _ of
      Initialize next -> do
        H.modify (_ { pages = Loading })
        res <- H.liftAff $ Affjax.get "/api/feed.json"
        let pages_ = fromEither (decodeJson res.response)
        H.modify (_ { pages = pages_
                    , currentPage = head $ withDefault [] pages_
                    }
                 )
        pure next

      SetCurrentPage selectedTitle next -> do
        state <- H.get
        case find (\(Page p) -> p.title == selectedTitle) (withDefault [] state.pages) of
          Nothing ->
            pure unit
          Just p ->
            H.modify (_ { currentPage = Just p })
        pure next

      PreventDefault ev q -> do
        H.liftEff $ preventDefault ev
        eval q

      AddPage next -> pure next
      AddGroup next -> pure next
      AddItem group next -> pure next


    render :: State -> H.ComponentHTML Query
    render state =
      case state.pages of
        NotAsked ->
          HH.text "Initializing..."
        Loading ->
          HH.text "Loading..."
        Failure err ->
          HH.text $ "Failed to load: " <> err
        Success ps ->
          HH.div_
          [ HH.header_ [ HH.nav_ $ (concatMap mkNavLink ps) <> [HH.a [HP.href "#", onClick AddPage] [HH.text "Add page..."]] ]
          , HH.main [HP.attr (HH.AttrName "role") "main"]
            ([HH.a [HP.href "#", onClick AddGroup] [HH.text "Add group..."]] <> (maybe [] mkPage state.currentPage))
          ]
      where
        onClick action =
          HE.onClick (\e -> Just $ PreventDefault (toEvent e) (H.action action))
        mkNavLink (Page{title}) =
          [ HH.a [HP.href "#", onClick (SetCurrentPage title)] [HH.text title]
          , HH.div [HP.class_ (HH.ClassName "pipe")] []
          ]
        mkPage (Page{title,groups}) =
          [ HH.div [HP.id_ title, HP.class_ $ HH.ClassName "view"]
            (concatMap mkGroup groups)
          ]
        mkGroup group@(Group{title,items}) =
          [ HH.section_
            [ HH.h3_ [HH.text title]
            , HH.a [HP.href "#", onClick (AddItem group)] [HH.text "Add item..."]
            , HH.ul_ (concatMap mkItem items)
            ]
          ]
        mkItem =
          case _ of
            Link{title,link,time,niceTime} ->
              [ HH.li_ [ HH.a [HP.href link]
                         [HH.text $ title <> " "]
                       , HH.time [HP.attr (HH.AttrName "pubdate") time]
                         [HH.text $ "— " <> niceTime]
                       ]
              ]
            Podcast{title,link,time,niceTime,enclosure} ->
              [ HH.li_ [ HH.a [HP.href link, HP.attr (HH.AttrName "data-enclosure") enclosure]
                         [HH.text $ title <> " "]
                       , HH.time [HP.attr (HH.AttrName "pubdate") time]
                         [HH.text $ "— " <> niceTime]
                       ]
              ]
