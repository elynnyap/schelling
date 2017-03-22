{-
  File      :  GridFile.hs
  Copyright : (c) E-Lynn Yap, 02/24/17
  Contains methods for parsing a text file to produce a grid.
-}

module GridFile
(
  parseGridsFromFiles
) where

import Grid
import Text.Read(readMaybe)
import Data.List(isSuffixOf)
import Data.Maybe(isNothing, fromJust)
import Data.Char(isSpace)
import System.IO

-- Takes a list of filenames and returns a list of valid grids
-- Prints an error message for each invalid file
parseGridsFromFiles :: [String] -> IO [(Grid Spot)]
parseGridsFromFiles filenames =
  foldr parseGridFromFile ((pure []) :: IO [(Grid Spot)]) filenames

-- Takes a single filename and parses it to produce a valid grid if possible
-- If file is bad, prints an error message
parseGridFromFile :: String -> IO [(Grid Spot)] -> IO [(Grid Spot)]
parseGridFromFile filename g = do
  newGrid <- fmap extractGrid $ readFile filename
  case newGrid of
    Nothing -> do
      putStrLn $ "\n" ++ filename ++ " does not contain a valid grid."
      g
    Just grid -> do
      fmap ((:) grid) g

-- Takes in the data in a file as a string and returns a grid wrapped in a Maybe
-- if the file contains a valid grid, otherwise returns Nothing.
extractGrid :: String -> Maybe (Grid Spot)
extractGrid fileData
  | invalidNumberOfLines = Nothing -- fewer than 3 lines in file
  | invalidDimensions = Nothing -- first two lines do not contain positive integers
  | invalidGridData = Nothing -- spot values are invalid/don't match # rows & cols
  | otherwise = Just newGrid
  where invalidNumberOfLines = length gridStrings < 3
        gridStrings = filter (not . all isSpace) $ lines fileData
        invalidDimensions = isNothing dimensions
        dimensions = parseDimensions $ (rowString, colString)
        (rowString, colString) = (gridStrings !! 0, gridStrings !! 1)
        (rows, cols) = fromJust dimensions
        invalidGridData = isNothing grid
        grid = parseGrid rows cols $ drop 2 gridStrings
        newGrid = fromJust grid

-- Takes in a tuple of 2 strings and parses them for nRows and nCols
-- Returns Nothing unless both nRows and nCols are valid positive integers,
-- in which case it returns Just (nRows, nCols)
parseDimensions :: (String, String) -> Maybe (Int, Int)
parseDimensions (nRows, nCols) = case (r, c) of
  (Just rows, Just cols) -> case (rows > 0 && cols > 0) of
                              True -> Just (rows, cols)
                              False -> Nothing
  _ -> Nothing
  where (r, c) = (readMaybe nRows :: Maybe Int, readMaybe nCols :: Maybe Int)

-- Takes in a grid's nRows and nCols, and an array of Strings constituting a grid
-- Returns Nothing unless the array of Strings is consistent with a valid grid,
-- in which case it returns Just newGrid, where newGrid is the grid produced
parseGrid :: Int -> Int -> [String] -> Maybe (Grid Spot)
parseGrid rows cols lines
  | (numOfRowsCols == expectedNumOfRowsCols) && (validSpots spotValues) =
      Just (Grid rows cols $ convertToSpots spotValues)
  | otherwise = Nothing
  where numOfRowsCols = map length spotValues
        expectedNumOfRowsCols = map (const cols) [1..rows]
        spotValues = map words lines

-- Takes in a list of lists of strings and checks if the strings are all valid
-- grid values i.e. "X", "O" or "E"
validSpots :: [[String]] -> Bool
validSpots spotValues = null invalidStrings
  where invalidStrings = filter invalid $ concat spotValues
        invalid str = case str of
          "X" -> False
          "O" -> False
          "E" -> False
          _ -> True

-- Takes in a list of lists of strings and converts each string to its corresponding
-- Spot value to return the data for a grid.
convertToSpots :: [[String]] -> [[Spot]]
convertToSpots spotValues = map (map getSpot) spotValues

-- Returns the corresponding Spot for a valid string
getSpot :: String -> Spot
getSpot str = case str of
  "X" -> AgentA
  "O" -> AgentB
  "E" -> Empty
  _ -> error $ str ++ " does not represent a valid Spot"