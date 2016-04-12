module LifeGame (
  FieldSize
  , Field
  , Cell
  , next
) where

import Prelude
import Data.Array ((..), (!!), slice, head, last, catMaybes, zipWith)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Control.Plus (empty)

type Cell = Int
type Field = Array (Array Cell)
type FieldSize = { width :: Int, height :: Int }

next :: FieldSize -> Field -> Array Int -> Field
next {width: w, height: h} prev range = fromMaybe [] $
  zipWith (\row cols -> zipWith (rule row) (0..(w - 1)) cols) range <$>
  (slice <$> head range <*> ((+1) <$> last range) <*> pure prev)
  where
    rule y x target =
      if target == 0 then
        if aroundLives == 3 then 1 else 0
      else
        if aroundLives == 2 || aroundLives == 3 then min (target + 1) 255 else 0 
      where
        aroundLives = sum $ catMaybes $ cell <$> aroundCells x y w h
        cell (Tuple x' y') = prev !! y' >>= (!! x')
          >>= \n -> return $ case n of
            0 -> 0
            _ -> 1
    min :: Int -> Int -> Int
    min a b = if a < b then a else b

aroundCells :: Int -> Int -> Int -> Int -> Array (Tuple Int Int)            
aroundCells cx cy xmax ymax = do
  y <- [cy - 1, cy, cy + 1]
  x <- [cx - 1, cx, cx + 1]
  if x < 0 || y < 0 || x >= xmax || y >= ymax || (cx == x && cy == y) then empty else return (Tuple x y)