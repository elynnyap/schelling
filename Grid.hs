{-
  File      :  Grid.hs
  Copyright : (c) E-Lynn Yap, 02/17/17
  Contains data types relevant to the grid for the Schelling simulation
  as well as methods to interact with the grid.
-}

module Grid
(
  Grid(..),
  Spot(..),
  Coord,
  getRandomGrid,
  getSpotCoords,
  moveAgents,
  getNeighbors
) where

import RandomSelect

-- Defines a grid with a fixed number of rows and cols
data Grid a = Grid {nRows :: Int, nCols :: Int, grid :: [[a]]}

-- Make Grid an instance of Functor
instance Functor Grid where
  fmap f (Grid nRows nCols gridData) = Grid nRows nCols 
    $ fmap (\row -> fmap f row) gridData

-- Make Grid an instance of Show so that it can be printed
instance (Show a) => Show (Grid a) where
  show g = concat $ map (\row -> concat row ++ "\n") $ grid (fmap show g)

-- Defines the three possible values that a spot in a grid can take
data Spot = AgentA | AgentB | Empty deriving Eq

-- Make Spot an instance of Show so it can be printed
instance Show Spot where
  show AgentA = "X"
  show AgentB = "O"
  show Empty = "E"

-- Type synonym for the coordinates of a given spot in a grid
type Coord = (Int, Int)

-- Takes 4 arguments: number of rows, number of columns, percentage of spots
-- being AgentA, percentage of spots being AgentB, and returns a randomly
-- generated grid.
getRandomGrid :: Int -> Int -> Int -> Int -> IO (Grid Spot)
getRandomGrid rows cols aPercent bPercent = do
  let totalCount = rows * cols
  let agentACount = round $ (fromIntegral totalCount) * 
                            (fromIntegral aPercent) / 100
  let agentBCount = round $ (fromIntegral totalCount) * 
                            (fromIntegral bPercent) / 100
  let emptyCount = totalCount - agentACount - agentBCount
  let spotVals = map (const AgentA) [1..agentACount] ++
                 map (const AgentB) [1..agentBCount] ++
                 map (const Empty) [1..emptyCount]
  (g, _) <- generateGrid rows cols ([], spotVals)
  return (Grid rows cols g)

-- Takes in number of rows r, number of cols c, a tuple consisting of
-- a list of lists of spots and a list of spots to choose from, and
-- returns the data for a randomly generated rxc grid as a list of lists
-- of spots
generateGrid :: Int -> Int -> ([[Spot]], [Spot]) -> IO ([[Spot]], [Spot])
generateGrid rows cols (grid, spotVals) = do
  case rows of
    0 -> return (grid, spotVals)
    _ -> do
      (newRow, remSpots) <- generateRandomRow cols ([], spotVals)
      generateGrid (rows-1) cols (newRow:grid, remSpots)

-- Given an integer n, and a tuple consisting of a) a randomly
-- chosen list of spots and b) a list of spots to choose from,
-- returns a tuple consisting of a) a randomly chosen list of
-- n spots and b) a list of spots not chosen from the original list
generateRandomRow :: Int -> ([Spot], [Spot]) -> IO ([Spot], [Spot])
generateRandomRow n (chosen, remaining) = do
  case n of
    0 -> return (chosen, remaining)
    _ -> do
      (newSpot, remSpots) <- getRandomElement remaining
      generateRandomRow (n-1) (newSpot:chosen, remSpots)

-- Takes in a grid and a list of dissatisfied agents, where each agent
-- is represented by a tuple consisting of its value and coordinates.
-- Moves dissatisfied agents into empty spots, returning a new grid
moveAgents :: IO (Grid Spot) -> [(Spot, Coord)] -> IO (Grid Spot)
moveAgents grid agents = foldr moveAgent grid agents

-- Takes an agent represented by a tuple and a grid.
-- Moves an agent into a random empty spot in the grid.
moveAgent :: (Spot, Coord) -> IO (Grid Spot) -> IO (Grid Spot)
moveAgent agent g = do
  g' <- g
  emptySpot <- getEmptySpot g'
  return $ swapCoords agent emptySpot g'

-- Examines each spot in a grid to determine if it fulfills a given condition
-- and returns a list of tuples corresponding to those spots and their coords
-- that fulfill that condition
getSpotCoords :: (Grid Spot) -> (Spot -> Coord -> Bool) -> [(Spot, Coord)]
getSpotCoords x cond = fst $ foldr f ([], ((nRows x) + 1, nCols x)) g
  where g = grid x
        f row (lst, (r, c)) = foldr f' (lst, (r - 1, nCols x)) row
        f' spot (lst, (r, c))
                          | cond spot (r, c) = ((spot, (r, c)):lst, nextCoord)
                          | otherwise = (lst, nextCoord)
                          where nextCoord = (r, c - 1)

-- Returns the coordinates of a randomly chosen empty spot in a grid
getEmptySpot :: (Grid Spot) -> IO (Spot, Coord)
getEmptySpot x = do
  (emptySpot, _) <- getRandomElement $ getSpotCoords x (\spot _ -> spot == Empty)
  return emptySpot

-- Swaps the coordinates of two Spots
swapCoords :: (Spot, Coord) -> (Spot, Coord) -> (Grid Spot) -> (Grid Spot)
swapCoords spot1 spot2 g = newGrid
  where g' = placeInGrid g (fst spot1) (snd spot2)
        newGrid = placeInGrid g' (fst spot2) (snd spot1)

-- Places a value in a particular coordinate in the grid
placeInGrid :: (Grid Spot) -> Spot -> Coord -> (Grid Spot)
placeInGrid x newSpot (targetX, targetY) = Grid (nRows x) (nCols x) newGrid
  where newGrid = fst $ foldr f ([], (nRows x, nCols x)) g
        g = grid x
        f row (g', coord) = ((newRow row coord):g', ((fst coord) - 1, nCols x))
        newRow r c = fst $ foldr f' ([], c) r
        f' currSpot (row', (currX, currY))
                                  | currX == targetX && currY == targetY
                                      = (newSpot:row', nextCoord)
                                  | otherwise = (currSpot:row', nextCoord)
                                  where nextCoord = (currX, currY - 1)

-- Returns the neighbors for a given coordinate (as a list of Spots)
getNeighbors :: (Grid Spot) -> Coord -> [Spot]
getNeighbors x c = map fst $ getSpotCoords x (areAdjacent c)

-- Checks if two coordinates are adjacent to each other. Returns false
-- if the two coordinates are identical.
areAdjacent :: Coord -> Spot -> Coord -> Bool
areAdjacent (row1, col1) _ (row2, col2)
  | (row1 == row2) && (col1 == col2) = False
  | (rowDiff <= 1 && colDiff <= 1) = True
  | otherwise = False
  where rowDiff = abs (row1 - row2)
        colDiff = abs (col1 - col2)