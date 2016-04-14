module DOMUtil where

import Prelude (Unit(), pure, unit, (<<<))

import Control.Monad.Eff (Eff())

import Data.Maybe (Maybe, maybe)

import DOM
import DOM.Node.Node (setTextContent)
import DOM.Node.Types

foreign import removeAttribute :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit

setText :: forall e. String -> Maybe Element -> Eff (dom::DOM | e) Unit
setText text = maybe (pure unit) (setTextContent text <<< elementToNode)