module GameObjects.Children
( move
) where

import System.Random
import GameObjects.Board
import GameObjects.Utils


move :: Board -> StdGen -> (Board, StdGen)
move board gen =
    let children = collectState B board ++ collectState RBB board
    in moveChild children board gen

moveChild:: [Position] -> Board -> StdGen -> (Board, StdGen)
moveChild [] board gen = (board, gen)
moveChild (pos:others) board gen =
    let (dirtAmount, gen') = generateDirtAmount board gen pos;
        (dir, gen'') = generateRandomDir gen';
        newPos = add pos dir;
        board' = canMove pos newPos dir board;
        (newBoard, newGen) = generateDirtInSquare board' gen pos dirtAmount
    in moveChild others newBoard newGen


generateDirtAmount :: Board -> StdGen -> Position -> (Int, StdGen)
generateDirtAmount board gen pos =
    let childrenNearBy = length [ x | row <- board, x <- row, (fst x == B) || (fst x == RBB), isClose (snd x) pos];
        maxDirt = maxDirtOnSquare childrenNearBy;
    in randomR (0, maxDirt) gen

isClose :: Position -> Position -> Bool
isClose (x1, y1) (x2, y2) = 
    let v1 = abs (x1 - x2);
        v2 = abs (y1 - y2)
    in (v1 <= 1) && (v2 <= 1)


canMove :: Position -> Position -> Position -> Board -> Board
canMove _ _ (0, 0) board = board
canMove pos newPos dir board
    | not (isValid newPos board) = board
    | isOccupied childrenOccupied x = board
    | isOccupied obstacleOccupied x = 
        let (flag, emptyPos) = lookForEmpty newPos dir board;
        in if flag
            then let board' = putCell O Empty newPos emptyPos board
                 in putCell B (stateToLeave pos board') pos newPos board'
            else board
    | otherwise = putCell B (stateToLeave pos board) pos newPos board
    where x = matrixIndexAccess board newPos
    
stateToLeave :: Position -> Board -> State
stateToLeave pos board
    | state == B = Empty
    | state == RBB = BR
    | otherwise = error "Unexpected state!!!"
    where (state, _) = matrixIndexAccess board pos


-- Returns true if a child can not move to this cell
childrenOccupied :: Cell -> Bool
childrenOccupied (state, _) = (state /= Empty) && (state /= O)

-- Returns true if there is an obstacle on cell
obstacleOccupied :: Cell -> Bool
obstacleOccupied (state, _) = state == O

-- Checks if there is a space for obstacles to be moved by children
-- In case there is, it returns it
lookForEmpty :: Position -> Position -> Board -> (Bool, Position)
lookForEmpty pos dir board
    | not (isValid pos board) = (False, pos)
    | fst x == Empty = (True, pos)
    | isOccupied obstacleOccupied x = lookForEmpty newPos dir board
    | otherwise = (False, pos)
    where x = matrixIndexAccess board pos
          newPos = add pos dir

    
-- Returns the board after replacing cell in newPos with (state, newPos) and cell in pos with (stateToLeave, pos)
putCell :: State -> State -> Position -> Position -> Board -> Board
putCell state newState pos newPos board =
    let board1 = addCell board (state, newPos);
        board2 = addCell board1 (newState, pos);
    in board2


-- This method will collect the empty cells in the 3x3 square centered in pos
-- These cells will be where the new dirt is going to be generated
-- It returns the board after randomly generating the new dirt in these cells
generateDirtInSquare :: Board -> StdGen -> Position -> Int -> (Board, StdGen)
generateDirtInSquare board gen pos n =
    let emptyCells = [ snd x | row <- board, x <- row, fst x == Empty, isClose pos (snd x)]
    in generateDirt board gen emptyCells n


-- This method will try to generate n cells of dirt
-- The cells where the dirt will go will be generated randomly and will be from emptyCells
-- It returns the board after adding the new dirty cells
generateDirt :: Board -> StdGen -> [Position] -> Int -> (Board, StdGen)
generateDirt board gen _ 0 = (board, gen)
generateDirt board gen [] _ = (board, gen)
generateDirt board gen emptyCells n =
    let (idx, newGen) = randomR (0, length emptyCells - 1) gen;
        dirtyCell = emptyCells !! idx;
        newBoard = addCell board (D, dirtyCell);
        emptyCellsLeft = removeFromList idx emptyCells
    in generateDirt newBoard newGen emptyCellsLeft (n - 1)