module LifeGame (
  Game
  , Field
  , Cell
  , cross
  , next
) where

import Prelude
import Data.Array ((..), zip, (!!), catMaybes)
import Data.Tuple (Tuple(..))
import Data.Foldable (sum)
import Control.Plus (empty)

type Cell = Int
type Field = Array (Array Cell)
type Game = { w :: Int, h :: Int, field :: Field }

next :: Game -> Game
next prev = prev { field = map rule $ cross (Tuple id (zip (0..999))) <$> zip (0..999) prev.field }
  where
    rule (Tuple row cols) = map rule' cols
      where
        rule' (Tuple col target) =
          if target == 0 then
            if aroundLives == 3 then 1 else 0
          else
            if aroundLives == 2 || aroundLives == 3 then min (target + 1) 255 else 0 
          where
            aroundLives = sum $ catMaybes $ cell <$> aroundCells col row prev.w prev.h
            cell (Tuple col row) = prev.field !! row >>= (!! col)
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

cross :: forall a b c d. Tuple (a->c) (b->d) -> Tuple a b -> Tuple c d
cross (Tuple f g) (Tuple a b) = Tuple (f a) (g b)