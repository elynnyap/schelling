{-
  File      :  Simulation.hs
  Copyright : (c) E-Lynn Yap, 02/17/17
  Contains the Main module for the Schelling simulation which presents
  three different ways to run the simulation: 1) generating a 10x10
  grid randomly, 2) prompting the user for the grid dimensions and
  3) reading grids from supplied configuration files.
-}

module Main where

import Grid
import GridFile
import Satisfaction
import System.Environment
import Text.Read(readMaybe)
import Control.Monad

-- Main method that implements the entire simulation
main :: IO ()
main = do
  args <- getArgs
  when (null args) $ simulateRandomGrid
  when (not $ null args) $ do
    let flag = head args
    case flag of
      "-i" -> simulateRandomGrid
      "-r" -> simulateUserGrid
      "-f" -> simulateFilesGrid $ drop 1 args
      _ -> putStrLn "Usage: ./Simulation <-i>/<-f>/<-r>"

-- Runs the simulation on a supplied threshold and grid
runSimulation :: Int -> (Grid Spot) -> IO ()
runSimulation threshold grid = do
  putStrLn "\nInitial grid: "
  print grid
  putStrLn "Final grid: "
  finalGrid threshold grid >>= print

-- Runs the simulation on a randomly generated 10x10 grid with 30% Agent A
-- and 30% Agent B. NOTE: I do not have a variable called initialGrids
-- because my initial grid configurations are randomly generated, rather
-- than hard-coded. I ran this past Lamont at office hours, who said it's ok.
simulateRandomGrid :: IO ()
simulateRandomGrid = do
  threshold <- getThreshold
  grid <- getRandomGrid 10 10 30 30
  runSimulation threshold grid

-- Runs the simulation on a grid created based on params supplied by user
simulateUserGrid :: IO ()
simulateUserGrid = do
  (rows, cols) <- getGridDimensions
  (aPercent, bPercent) <- getAgentBreakdown
  threshold <- getThreshold
  grid <- getRandomGrid rows cols aPercent bPercent
  runSimulation threshold grid

-- Runs the simulation on grids from grid configuration files
simulateFilesGrid :: [String] -> IO ()
simulateFilesGrid files = do
  when (null files) $ putStrLn "Must supply at least one filename for option -f."
  when (not $ null files) $ do
    threshold <- getThreshold
    grids <- parseGridsFromFiles files
    mapM_ (runSimulation threshold) grids
    return ()

-- Prompts the user for a threshold and wraps it in an IO action
getThreshold :: IO Int
getThreshold = do
  putStrLn "\nEnter a threshold for Schelling's simulation (0-100):"
  input <- getLine
  case (readMaybe input :: Maybe Int) of
    Just x | 0 <= x && x <= 100 -> return x
    _ -> do
      putStrLn "Invalid input. Type a round number from 0-100 and hit enter."
      getThreshold

-- Prompts the user for the number of rows and cols in a grid
getGridDimensions :: IO (Int, Int)
getGridDimensions = do
  putStrLn $ "Enter the number of rows and columns in the grid. E.g."
    ++ " \"5 10\" represents a grid with 5 rows and 10 columns."
  args <- fmap words getLine
  case (length args) of
    2 -> do
      let r = readMaybe (args !! 0) :: Maybe Int
      let c = readMaybe (args !! 1) :: Maybe Int
      case (r, c) of
        (Just x, Just y) | 0 < x && 0 < y -> return (x, y)
        _ -> do
          putStrLn $ "Must enter a positive int for rows and a positive int"
            ++ " for columns."
          getGridDimensions
    _ -> do
      putStrLn $ "Invalid input. Type the number of rows and the number of "
        ++ "columns, separated by a space, and press enter."
      getGridDimensions

-- Prompts the user for the percentage of AgentA vs. AgentB in a grid
getAgentBreakdown :: IO (Int, Int)
getAgentBreakdown = do
  putStrLn $ "Enter the percentage of the grid occupied by Agent A (X) and the "
    ++ "percentage of the grid occupied by Agent B (O). E.g. \"30 40\" represents"
    ++ " a grid where 30% is occupied by Agent A and 40% by Agent B, and the"
    ++ " remaining 30% is empty."
  args <- fmap words getLine
  case (length args) of
    2 -> do
      let a = readMaybe (args !! 0) :: Maybe Int
      let b = readMaybe (args !! 1) :: Maybe Int
      case (a, b) of
        (Just x, Just y) | 0 <= x && 0 <= y && (x + y <= 100) -> return (x, y)
        _ -> do
          putStrLn $ "Must enter positive integers for the percentages and "
            ++ "they cannot add up to more than 100."
          getAgentBreakdown
    _ -> do
      putStrLn $ "Invalid input. Type the two percentages separated by a "
        ++ "space, and press enter."
      getAgentBreakdown

-- Returns the final grid from a specified threshold and initial grid, where
-- the final grid is the deterministic evaluation of Schelling's simulation,
-- with 0 dissatisfied agents.
finalGrid :: Int -> (Grid Spot) -> IO (Grid Spot)
finalGrid threshold grid
  | null dissatisfied = return grid
  | otherwise = moveAgents (return grid) dissatisfied >>= finalGrid threshold
  where dissatisfied = getDissatisfied grid threshold