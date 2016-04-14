module DOMUtil where

import Prelude (Unit(), (<<<))

import Control.Monad.Eff (Eff())

import DOM
import DOM.Node.Element (setAttribute)
import DOM.Node.Node (setTextContent)
import DOM.Node.Types

foreign import removeAttribute :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit

setText :: forall eff. String -> Element -> Eff (dom::DOM | eff) Unit
setText text = setTextContent text <<< elementToNode

disableElement :: forall eff. Element -> Eff (dom :: DOM | eff) Unit
disableElement = setAttribute "disabled" "true"

enableElement :: forall eff. Element -> Eff (dom :: DOM | eff) Unit
enableElement = removeAttribute "disabled" 
