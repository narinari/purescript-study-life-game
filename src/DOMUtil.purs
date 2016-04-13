module DOMUtil where

import Prelude (Unit())

import Control.Monad.Eff (Eff())

import DOM
import DOM.Node.Types

foreign import removeAttribute :: forall eff. String -> Element -> Eff (dom :: DOM | eff) Unit