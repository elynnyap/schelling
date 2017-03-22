{-
  File      :  Satisfaction.hs
  Copyright : (c) E-Lynn Yap, 02/17/17
  Contains methods to determine the satisfaction of agents in a given grid
  based on a provided threshold.
-}

module Satisfaction
(
  getDissatisfied,
  isDissatisfied
) where

import Grid

-- Returns a list of dissatisfied agents and their coordinates
getDissatisfied :: (Grid Spot) -> Int -> [(Spot, Coord)]
getDissatisfied x t = getSpotCoords x (isDissatisfied x t)

-- Takes in a grid, threshold, and a spot plus its coordinates
-- and returns a boolean indicating if there is a dissatisfied agent
-- in that spot.
isDissatisfied :: (Grid Spot) -> Int -> Spot -> Coord -> Bool
isDissatisfied x t spot coord
  | spot == Empty = False
  | neighborCount == 0 = True
  | similarPercent < ((fromIntegral t)/100.0) = True
  | otherwise = False
  where similarPercent = similarNum / neighborCount
        similarNum = fromIntegral (length $ filter (== spot) neighbors)
        neighborCount = fromIntegral (length $ filter (/= Empty) neighbors)
        neighbors = getNeighbors x coord