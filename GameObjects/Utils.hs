module GameObjects.Utils
( calculateAmountFromPercentage
, calculatePercentageFromAmount
, maxDirtOnSquare
, generateRandomDir
, matrixIndexAccess
, add
, isValid
, removeFromList
) where

import System.Random

calculateAmountFromPercentage :: Float -> Int -> Int
calculateAmountFromPercentage percentage total = round (percentage * t / 100)
    where t = fromIntegral total :: Float

calculatePercentageFromAmount :: Int -> Int -> Float
calculatePercentageFromAmount x total = 100 * ( a / b )
    where a = fromIntegral x :: Float
          b = fromIntegral total :: Float

maxDirtOnSquare :: Int -> Int
maxDirtOnSquare n
    | n == 1 = 1
    | n == 2 = 3
    | n >= 3 = 6
    | otherwise = error "Unexpected number of children"

generateRandomDir :: StdGen -> ((Int, Int), StdGen)
generateRandomDir gen =
    let directions = [(0, 0), (1, 0), (-1, 0), (0, 1), (0, -1)];
        (idx, newGen) = randomR (0, 4) gen;
        dir = directions !! idx
    in (dir, newGen)

matrixIndexAccess :: [[a]] -> (Int, Int) -> a
matrixIndexAccess matrix (i, j) = (matrix !! (i - 1)) !! (j - 1)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


generate :: Int -> (StdGen -> (a, StdGen)) -> StdGen -> [a]
generate 0 _ _ = []
generate n f gen = let (x, gen') = f gen in x : generate (n - 1) f gen'


-- Returns true if idx is a valid position of list (1-indexed)
inRange :: Int -> [a] -> Bool
inRange idx list = (idx >= 1) && (idx <= length list)

-- Returns true if (i, j) is a valid position of matrix (1-indexed)
-- It is assumed that all rows of matrix have the same amount of elements
isValid :: (Int, Int) -> [[a]] -> Bool
isValid (i, j) matrix = inRange i matrix && inRange j (head matrix)


removeFromList :: Int -> [a] -> [a]
removeFromList i list = let (first, h : second) = splitAt i list in first ++ second


capital :: [(Int, Int, Int)] -> String
capital [] = "empty list !!"
capital all@(x@(a, b, _):xs) = "The first element of " ++ show all ++ " is " ++ show x ++ ". The rest of the list is " ++ show xs