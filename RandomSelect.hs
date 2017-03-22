{-
  File      :  Random.hs
  Copyright : (c) E-Lynn Yap, 02/17/17
  Contains method to select an element from a list randomly.
-}

module RandomSelect
(
  getRandomElement
) where

import System.Random

-- Returns a tuple where first element is the element randomly chosen
-- and second element is the original list minus that element
getRandomElement :: [a] -> IO (a, [a])
getRandomElement [] = error "Empty list"
getRandomElement lst = do
  randomIndex <- randomRIO (0, length lst - 1)
  let randomElement = lst !! randomIndex
  let lst' = removeAt lst randomIndex
  return (randomElement, lst')

-- Removes element from a list at the given index, returning reduced list
removeAt :: [a] -> Int -> [a]
removeAt lst idx = fst $ foldr f ([], length lst - 1) lst
  where f x (currList, currIdx)
                              | currIdx == idx = (currList, currIdx - 1)
                              | otherwise = (x:currList, currIdx - 1)