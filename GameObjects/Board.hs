module GameObjects.Board
( generateBoard
, dirtPercentage
, isOccupied
, addCell
, collectState
, shuffleBoard
, State(..)
, Position
, Cell
, Board
) where

import System.Random
import GameObjects.Utils

data State = Empty | O | B | D | R | C | BR | BC | RC | RD | BRC | RDB | RBB deriving (Show, Eq)
type Position = (Int, Int)
type Cell = (State, Position)
type Board = [[Cell]]


generateBoard :: Int ->  Int ->  Int ->  Int ->  Int -> Float -> StdGen -> (Board, StdGen)
generateBoard row col o b r d gen =
    let emptyBoard = createBoard row col 1;
        (boardC, gen1) = addCorral emptyBoard b gen row col [];
        (boardCO, gen2) = addObstacles boardC o gen1 row col;
        (boardCOB, gen3) = addChildren boardCO b gen2 row col;
        (boardCOBR, gen4) = addRobots boardCOB r gen3 row col;
        dirtAmount = calculateDirtAmount boardCOBR d;
        (boardCOBRD, gen5) = addDirt boardCOBR dirtAmount gen4 row col
    in (boardCOBRD, gen5)


createBoard :: Int -> Int -> Int -> Board
createBoard 0 _ _ = []
createBoard row col i = createRow i col 1 : createBoard (row - 1) col (i + 1)

createRow :: Int -> Int -> Int -> [Cell]
createRow _ 0 _ = []
createRow i col j = (Empty, (i, j)) : createRow i (col - 1) (j + 1)

addCorral :: Board -> Int -> StdGen -> Int -> Int -> [Cell] -> (Board, StdGen)
addCorral board 0 gen _ _ _ = (board, gen)
addCorral board count gen row col currentCorrals =
    let (pos, newGen) = generateAdyacentPosition board row col gen currentCorrals;
        newCell = (C, pos)
        newBoard = addCell board newCell
    in addCorral newBoard (count - 1) newGen row col (newCell : currentCorrals)


generatePosition :: Int -> Int -> StdGen -> (Position, StdGen)
generatePosition row col gen = 
    let (i, gen') = randomR (1, row) gen;
        (j, gen'') = randomR (1, col) gen'
    in ((i, j), gen'')


addCell :: Board -> Cell -> Board
addCell board (state, (i, j)) = 
    let cell = (state, (i, j)); 
        (first, h : second) = splitAt (i - 1) board;
        newRow = addCellInRow h cell j;
    in first ++ (newRow : second)


addCellInRow :: [Cell] -> Cell -> Int -> [Cell]
addCellInRow list cell idx =
    let (first, _ : second) = splitAt (idx - 1) list
    in first ++ (cell : second)


generateAdyacentPosition :: Board -> Int -> Int -> StdGen -> [Cell] -> (Position, StdGen)
generateAdyacentPosition board row col gen [] = generatePosition row col gen
generateAdyacentPosition board row col gen currentCorrals =
    let (newPos, newGen) = generatePosition row col gen;
        flag = isValidPosition board newPos currentCorrals
    in if flag
        then (newPos, newGen)
        else generateAdyacentPosition board row col newGen currentCorrals


isValidPosition :: Board -> Position -> [Cell] -> Bool
isValidPosition board pos list =
    let cell = findCell board pos;
        flag1 = isOccupied notEmpty cell;
        flag2 = isAdyacent pos list
    in not flag1 && flag2


findCell :: Board -> Position -> Cell
findCell board (i, j) =
    let (first, h : second) = splitAt (i - 1) board;
        (first', h': second') = splitAt (j - 1) h
    in h'


isOccupied :: (Cell -> Bool) -> Cell -> Bool
isOccupied f = f


notEmpty :: Cell -> Bool
notEmpty (state, pos) = state /= Empty


isAdyacent :: Position -> [Cell] -> Bool
isAdyacent _ [] = False
isAdyacent pos (x : xs) = isAdyacentToCell pos x || isAdyacent pos xs

isAdyacentToCell :: Position -> Cell -> Bool
isAdyacentToCell (x1, y1) (_, (x2, y2)) =
    let v1 = abs (x1 - x2);
        v2 = abs (y1 - y2);
    in (v1 <= 1) && (v2 <= 1) && (v1 * v2 == 0)


addObject :: State -> Board -> Int -> StdGen -> Int -> Int -> (Board, StdGen)
addObject _ board 0 gen _ _ = (board, gen)
addObject state board count gen row col = 
    let (pos, newGen) = generateNewValidPosition row col gen board notEmpty;
        cell = (state, pos);
        newBoard = addCell board cell
    in addObject state newBoard (count - 1) newGen row col


generateNewValidPosition :: Int -> Int -> StdGen -> Board -> (Cell -> Bool) -> (Position, StdGen)
generateNewValidPosition row col gen board notEmpty =
    let (pos, newGen) = generatePosition row col gen;
        cell = findCell board pos
    in if not (isOccupied notEmpty cell)
        then (pos, newGen)
        else generateNewValidPosition row col newGen board notEmpty

addObstacles :: Board -> Int -> StdGen -> Int -> Int -> (Board, StdGen)
addObstacles = addObject O

addChildren :: Board -> Int -> StdGen -> Int -> Int -> (Board, StdGen)
addChildren = addObject B

addRobots :: Board -> Int -> StdGen -> Int -> Int -> (Board, StdGen)
addRobots = addObject R

addDirt :: Board -> Int -> StdGen -> Int -> Int -> (Board, StdGen)
addDirt = addObject D

calculateDirtAmount :: Board -> Float -> Int
calculateDirtAmount board percentage = calculateAmountFromPercentage percentage (getAmountState Empty board)


dirtPercentage :: Board -> Float
dirtPercentage board =
    let emptyCells = getAmountState Empty board;
        dirtyCells = getAmountState D board
    in calculatePercentageFromAmount dirtyCells (dirtyCells + emptyCells)


collectState :: State -> Board -> [Position]
collectState state board = [ snd x | row <- board, x <- row, fst x == state]

getAmountState :: State -> Board -> Int
getAmountState state board = length (collectState state board)

collectCorralAmount :: Board -> Int
collectCorralAmount board =
    let amount1 = getAmountState C board;
        amount2 = getAmountState BC board;
        amount3 = getAmountState RC board;
        amount4 = getAmountState BRC board
    in amount1 + amount2 + amount3 + amount4

shuffleBoard :: Int -> Int -> Board -> StdGen -> (Board, StdGen)
shuffleBoard row col board gen =
    let emptyBoard = createBoard row col 1;
        
        corralAmount = collectCorralAmount board;
        (boardC, gen1) = addCorral emptyBoard corralAmount gen row col [];
        corrals = collectState C boardC;
        corralWithChildAmount = getAmountState BC board;
        (boardCwithB, corralsLeft, gen2) = 
            addObjectFromList boardC BC corralWithChildAmount corralAmount corrals gen1;
        corralWithRobotAmount = getAmountState RC board;
        corralsLeftAmount = corralAmount - corralWithChildAmount;
        (boardCwithR, corralsLeft1, gen3) =
            addObjectFromList boardCwithB RC corralWithRobotAmount corralsLeftAmount corralsLeft gen2;
        corralWithBRAmount = getAmountState BRC board;
        corralsLeft1Amount = corralsLeftAmount - corralWithRobotAmount;
        (boardCWithBR, _, gen4) =
            addObjectFromList boardCwithR BRC corralWithBRAmount corralsLeft1Amount corralsLeft1 gen3;

        obstaclesAmount = getAmountState O board;
        (boardO, gen5) = addObstacles boardCWithBR obstaclesAmount gen4 row col;

        childrenAmount = getAmountState B board;
        (boardB, gen6) = addChildren boardO childrenAmount gen5 row col;

        robotsAmount = getAmountState R board;
        (boardR, gen7) = addRobots boardB robotsAmount gen6 row col;

        dirtAmount = getAmountState D board;
        (boardD, gen8) = addDirt boardR dirtAmount gen7 row col;

        robotAndChhildAmount = getAmountState BR board;
        (boardBR, gen9) = addObject BR boardD robotAndChhildAmount gen8 row col;

        robotAndDirtAmount = getAmountState RD board;
        (boardRD, gen10) = addObject RD boardBR robotAndDirtAmount gen9 row col;

        robotDirtAndChildAmount = getAmountState RDB board;
        (boardRDB, gen11) = addObject RDB boardRD robotDirtAndChildAmount gen10 row col;

        robotAndTwoChildrenAmount = getAmountState RBB board;
        (boardRBB, gen12) = addObject RBB boardRDB robotAndTwoChildrenAmount gen11 row col
    in (boardRBB, gen12)


addObjectFromList :: Board -> State -> Int -> Int -> [Position] -> StdGen -> (Board, [Position], StdGen)
addObjectFromList board _ 0 _ list gen = (board, list, gen)
addObjectFromList board state count total list gen =
    let (i, newGen) = randomR (0, total - 1) gen;
        pos = list !! i;
        newBoard = addCell board (state, pos);
        newList = removeFromList i list
    in addObjectFromList newBoard state (count - 1) (total - 1) newList newGen