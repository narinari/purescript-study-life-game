module Main where

import Prelude
import Math (max)
import Data.Array ((..), (:), zipWith, head, length, concat, take, drop, replicate)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Foldable (sequence_, fold, foldl)
import Data.Nullable (toMaybe)
import Control.Bind (join)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Cont.Trans (runContT)
import Control.Monad.ST (ST, STRef, readSTRef, modifySTRef, newSTRef)
import Control.Parallel (inParallel, runParallel)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (click)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (Element, elementToEventTarget)
import DOM.RequestAnimationFrame (requestAnimationFrame)
import Graphics.Canvas as C
import Graphics.Canvas.Free
import Color (toHexString, rgb)
import LifeGame (FieldSize, Field, next)
import LifeGame.Data (gliderGun)
import DOMUtil (setText, enableElement, disableElement)

data TimeState = Running | Stopped | Stopping
type GameState = { timeState :: TimeState
                 , stepRate :: Int
                 , startButton:: Maybe Element
                 , stopButton :: Maybe Element
                 , generationScore :: Maybe Element
                 , rateIndicator :: Maybe Element}
 
main :: forall e h. Eff (st :: ST h, console :: CONSOLE, canvas :: C.Canvas, dom :: DOM | e) Unit
main = do
  document <- htmlDocumentToParentNode <$> (window >>= document)
  gameNodes <- {timeState: Stopped, stepRate: 3, startButton:_, stopButton:_, generationScore:_, rateIndicator:_ }
                <$> (toMaybe <$> querySelector "#start" document)
                <*> (toMaybe <$> querySelector "#stop" document)
                <*> (toMaybe <$> querySelector "#generation" document)
                <*> (toMaybe <$> querySelector "#rate" document)
  rateIncButton <- toMaybe <$> querySelector "#rateInc" document
  rateDecButton <- toMaybe <$> querySelector "#rateDec" document
  
  disableElement =<<= gameNodes.stopButton
  showGeneration 0 gameNodes
  showStepRateIndicator gameNodes
  
  state <- newSTRef gameNodes
  registerListener click (startLoop state) gameNodes.startButton
  registerListener click (stopLoop state) gameNodes.stopButton
  registerListener click (rateIncrement state) rateIncButton
  registerListener click (rateDecrement state) rateDecButton
  
  where
    registerListener event listener = (=<<=) (addEventListener event (eventListener listener) true <<< elementToEventTarget) 

rateIncrement :: forall e h. STRef h GameState -> Event -> Eff (st :: ST h, dom :: DOM | e) Unit
rateIncrement state _ = do
  updateStepRate state \s -> if s.stepRate > 1 then s { stepRate=s.stepRate - 1 } else s
  
rateDecrement :: forall e h. STRef h GameState -> Event -> Eff (st :: ST h, dom :: DOM | e) Unit
rateDecrement state _ = do
  updateStepRate state \s -> if s.stepRate < 100 then s { stepRate=s.stepRate + 1 } else s

updateStepRate :: forall e h. STRef h GameState -> (GameState -> GameState) -> Eff (st :: ST h, dom :: DOM | e) Unit
updateStepRate state updater = modifySTRef state updater >>= showStepRateIndicator

stopLoop :: forall e h. STRef h GameState -> Event -> Eff (st :: ST h, dom :: DOM | e) Unit
stopLoop state _ = do
  gameState <- readSTRef state
  case gameState.timeState of
    Running -> void $ do
      modifySTRef state \g -> g { timeState=Stopping }
      enableElement =<<= gameState.startButton
      disableElement =<<= gameState.stopButton
    _ -> pure unit

startLoop :: forall e h. STRef h GameState -> Event -> Eff (st :: ST h, console :: CONSOLE, canvas :: C.Canvas, dom :: DOM | e) Unit
startLoop state _ = do
  gameState <- readSTRef state
  startLoop' gameState
  where
    startLoop' gs@{timeState: Stopped} = do
      modifySTRef state \g -> g { timeState=Running }
      
      disableElement =<<= gs.startButton
      enableElement =<<= gs.stopButton
      
      C.getCanvasElementById "canvas" >>= mainLoop
    startLoop' _ = pure unit
    
    mainLoop Nothing       = pure unit
    mainLoop (Just canvas) = do
      ctx <- C.getContext2D canvas
      dimensions <- C.getCanvasDimensions canvas
      let initialField = concat [gliderGun, gliderGun]
          size = { height: length initialField
                 , width: fromMaybe 0 $ length <$> head initialField}
          cellSize = max (toNumber $ size.height) (toNumber size.width)
          cellRect = {x: 0.0, y: 0.0, w: dimensions.width / cellSize, h: dimensions.height / cellSize}
          ranges = chunks 2 $ 0..(size.height - 1)
          loop generation field = do
            s <- readSTRef state
            loop' s
            where
              loop' {timeState: Stopping} = void $ modifySTRef state \g -> g { timeState=Stopped }
              loop' s@{timeState: Running} = do
                showGeneration generation s
                runGraphics ctx $ do
                  clearRect {x: 0.0, y: 0.0, w: dimensions.width, h: dimensions.height}
                  drawCells cellRect size field
                flip runContT (delayFrame (s.stepRate - 1) <<< loop (generation + 1)) $
                  runParallel $ fold <$> (for ranges (inParallel <<< pure <<< next size field))
              loop' _ = pure unit
              delayFrame frameNum = foldl (<<<) requestAnimationFrame $ replicate frameNum requestAnimationFrame
      loop 0 initialField

--
-- GUI
--
drawCells :: C.Rectangle -> FieldSize -> Field -> Graphics Unit
drawCells  cellRect size field =
  sequence_ $ join $ zipWith (\row cols -> zipWith (draw row) (0..size.width) cols) (0..size.height) field
  where
    draw row col target
      | target == 0 = pure unit
      | otherwise = do
        color target
        void $ fillRect $ cellRect {x = (toNumber col) * cellRect.w, y = toNumber row * cellRect.h}
    color age = rgb (255 - age) age 140 # toHexString # setFillStyle

showStepRateIndicator :: forall e. GameState -> Eff (dom :: DOM | e) Unit
showStepRateIndicator gs = setText ("step forward per " ++ show gs.stepRate ++ " frame") =<<= gs.rateIndicator

showGeneration :: forall e. Int -> GameState -> Eff (dom :: DOM | e) Unit
showGeneration generation gs = setText (show generation ++ " Generation") =<<= gs.generationScore

--
-- Utils
--
chunks :: forall a. Int -> Array a -> Array (Array a)
chunks 0 arr = []
chunks 1 arr = [arr]
chunks i arr = chunks' (length arr / i) arr where
  chunks' i [] = []
  chunks' i l  = take i l : chunks' i (drop i l)

infixl 9 =<<=
(=<<=) :: forall a b f. (Applicative f) => (a -> f b) -> Maybe a -> f Unit
(=<<=) f (Just a) = void $ f a
(=<<=) _ Nothing = pure unit