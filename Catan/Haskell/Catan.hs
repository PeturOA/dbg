{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ParallelListComp #-}

module Catan where

import qualified Data.Map as M
import System.Random
import Control.Monad
import Data.List hiding (init)
import Prelude hiding (init)
import Data.Ord

type Income = Int

data Resource = Brick | Sheep | Rock | Tree | Wheat 
  deriving (Eq, Ord)
           
instance Show Resource where
  show Brick = "B"
  show Sheep = "S"
  show Rock  = "R"
  show Tree  = "T"
  show Wheat = "W"

-- data Either a b = Left a | Right b

-- Einsmóta við 'Either Bool (Resource, Income)'
data Hex = Desert
         | Normal Resource Income

instance Show Hex where
  show Desert                   = "D"
  show (Normal resource income) = show resource ++ show income

-- instance Show Hex where
--   show (MkHex resource Nothing) = undefined

shuffle :: [a] -> IO [a]
shuffle xs = do
  pairList <- forM xs $ \str -> do
    rnd :: Double <- randomIO
    return (str, rnd)

  let sorted = sortBy (comparing snd) pairList
  return (map fst sorted)

resources :: [Resource]
resources = take 18 (cycle [Tree, Sheep, Wheat, Brick, Rock])

incomes :: [Income]
incomes = take 18 (cycle [3,4,5,6,8,9,10,11,2,12])

type Board = M.Map Pos Hex
type Pos = (Int, Int)

hexHexGrid :: Int -> [Pos]
hexHexGrid r = [ (x, y) | x <- [-r+1..r-1], y <- f x]
  where f x = if x < 0 then [1-r-x .. r-1] else [1-r .. r-1-x]


catanGrid :: [Pos]
catanGrid = hexHexGrid 3

makeHexes :: IO [Hex]
makeHexes = do
  is <- shuffle incomes
  shuffle $ Desert : [ Normal resource i | resource <- resources | i <- is ]

makeBoard :: [Pos] -> [Hex] -> Board
makeBoard grids hexes = M.fromList (zipWith (,) grids hexes)

initt :: IO Board
initt = do
  hexes <- makeHexes
  return (makeBoard catanGrid hexes)

splitOnChange :: [(Pos, Hex)] -> [[(Pos, Hex)]]
splitOnChange = groupBy (\a b -> fst (fst a) == fst (fst b))

