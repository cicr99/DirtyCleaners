module GameObjects.Robots
( move
, generate
, shuffle
, Robot
, Plan
) where

import System.Random
import GameObjects.Board
import GameObjects.Utils


-- AGENT TYPE: 1 reactive, 2 proactive

type Robot = (Position, Bool, Plan)
type Plan = [Position]

-- It returns a list filled with robots created accordingly to the ones in the board
generate :: Board -> [Robot]
generate board =
    let robotPositions = collectState R board
    in constructRobots robotPositions False []

-- It creates the robot object using the list of position, the flag to say if it is carrying a child or not and the plan
constructRobots :: [Position] -> Bool -> [Position] -> [Robot]
constructRobots [] _ _ = []
constructRobots (pos:rest) flag plan = (pos, flag, plan): constructRobots rest flag plan


-- It returns the list of robots after shuffling the board
shuffle :: Board -> Board -> [Robot] -> [Robot]
shuffle oldBoard board robots =
    let onlyRobots = collectState R board;
        newRobots1 = reassignRobots onlyRobots R robots oldBoard [];

        robotAndDirt = collectState RD board;
        newRobots2 = reassignRobots robotAndDirt RD robots oldBoard newRobots1;

        robotAndChild = collectState BR board;
        newRobots3 = reassignRobots robotAndChild BR robots oldBoard newRobots2;

        robotAndCorral = collectState RC board;
        newRobots4 = reassignRobots robotAndCorral RC robots oldBoard newRobots3;

        robotChildAndCorral = collectState BRC board;
        newRobots5 = reassignRobots robotChildAndCorral BRC robots oldBoard newRobots4;

        robotChildAndDirt = collectState RDB board;
        newRobots6 = reassignRobots robotChildAndDirt RDB robots oldBoard newRobots5;
        
        robotTwoChildren = collectState RBB board;
        newRobots7 = reassignRobots robotTwoChildren RBB robots oldBoard newRobots6
    in newRobots7

-- It returns a new list where the robots with the given state are repositioned
reassignRobots :: [Position] -> State -> [Robot] -> Board -> [Robot] -> [Robot]
reassignRobots [] _ _ _ robots = robots
reassignRobots _ _ [] _ _ = error "Not enough robots!!!"
reassignRobots posList@(x:rest) state ((pos, flag, plan):robots) board newRobots =
    let (st, _) = matrixIndexAccess board pos
    in if st == state
        then reassignRobots rest state robots board ((x, flag, plan):newRobots)
        else reassignRobots posList state robots board newRobots


-- This method will invoke the move action of the corresponding robot agent
move :: Board -> Int -> [Robot] -> (Board, [Robot])
move board agentType robots
    | agentType == 1 = moveReactive board robots
    | agentType == 2 = moveProactive board robots
    | otherwise = error "Invalid agent number!!!"


-- this method describes the movement of reactive robots
moveReactive :: Board -> [Robot] -> (Board, [Robot])
moveReactive board [] = (board, [])
moveReactive board robots@(robot@(pos, flag, _):rest)
    | isDirty cell = 
        let board' = clean cell board;
            (newBoard, newRobots) = moveReactive board' rest
        in (newBoard, robot:newRobots)
    | flag && reachableC = moveToTarget headToC pathToC board robot rest
    | not flag && reachableB = moveToTarget headToB pathToB board robot rest
    | reachableD = moveToTarget headToD pathToD board robot rest
    | otherwise = 
        let (newBoard, newRobotsList) = moveReactive board rest
        in (newBoard, robot:newRobotsList)
    where cell = matrixIndexAccess board pos
          (reachableC, pathToC) = canReach C [(pos, (0, 0))] [] board
          (reachableB, pathToB) = canReach B [(pos, (0, 0))] [] board
          (reachableD, pathToD) = canReach D [(pos, (0, 0))] [] board




moveToTarget :: ([Position] -> Board -> Robot -> (Board, Robot)) -> [Position] -> Board -> Robot -> [Robot] -> (Board, [Robot])
moveToTarget headToTarget pathToTarget board robot rest = 
    let (board', newRobot) = headToTarget pathToTarget board robot;
        (newBoard, newRobotsList) = moveReactive board' rest
    in (newBoard, newRobot:newRobotsList)




-- It returns true if the cell where the robot is, is dirty
isDirty :: Cell -> Bool
isDirty (state, _) = state == RD || state == RDB || state == D

-- It returns a new board with the specified cell without dirt
clean :: Cell -> Board -> Board
clean (state, pos) board =
    if state == RD
    then addCell board (R, pos)
    else addCell board (BR, pos)

-- Parameters:
-- state :: State -> Desired destination
-- queue :: [(Position, Position)] -> The nodes to visit <(node, parent)>
-- visited :: [(Position, Position)] -> The (nodes, parent) that have been visited already
-- It returns wether there is a path to the desired state and the path to it
canReach :: State -> [(Position, Position)] -> [(Position, Position)] -> Board -> (Bool, [Position])
canReach _ [] _ _ = (False, [])
canReach state ((node, parent):queue) visited board =
    let cell = matrixIndexAccess board node
    in if fst cell == state
       then (True, reverse (getPath node parent visited))
       else let ady = getValidNotVisitedAdyacent node visited queue board [(1, 0), (0, 1), (- 1, 0), (0, - 1)]
            in canReach state (queue ++ ady) ((node,parent):visited) board


getPath :: Position -> Position -> [(Position, Position)] -> [Position]
getPath node (0, 0) _ = []
getPath node parent visited = node : getPath parent (getParent parent visited) visited


getParent :: Position -> [(Position, Position)] -> Position
getParent _ [] = (0, 0)
getParent pos ((node, parent):visited) = if pos == node then parent else getParent pos visited


getValidNotVisitedAdyacent :: Position -> [(Position, Position)] -> [(Position, Position)] -> Board -> [Position] -> [(Position, Position)]
getValidNotVisitedAdyacent _ _ _ _ [] = []
getValidNotVisitedAdyacent node visited inQueue board (dir:ady) =
    let pos = add node dir
    in if isValid pos board && robotValidCell pos board && notVisited pos visited && notVisited pos inQueue
        then (pos, node):getValidNotVisitedAdyacent node visited inQueue board ady
        else getValidNotVisitedAdyacent node visited inQueue board ady


robotValidCell :: Position -> Board -> Bool
robotValidCell pos board = (state == Empty) || (state == B) || (state == D) || (state == C)
    where (state, _) = matrixIndexAccess board pos


notVisited :: Position -> [(Position, Position)] -> Bool
notVisited _ [] = True
notVisited pos ((node,_):rest) = (pos /= node) && notVisited pos rest


headToC :: [Position] -> Board -> Robot -> (Board, Robot)
headToC [] _ _ = error "There is no path!!!"
headToC [x] board (pos,_,_) = moveR board pos x BRC False []
headToC (x:y:rest) board (pos, flag, _)
    | isDirty cell = moveR board pos x RDB True []
    | null rest = moveR board pos y BRC False []
    | otherwise = moveR board pos y (stateToMove board y flag) True []
    where cell = matrixIndexAccess board x
        

moveR :: Board -> Position -> Position -> State -> Bool -> Plan -> (Board, Robot)
moveR board oldPos pos state flag plan =
    let board' = addCell board (state, pos);
        newBoard = addCell board' (stateToLeave board oldPos, oldPos)
    in (newBoard, (pos, flag, plan))


stateToLeave :: Board -> Position -> State
stateToLeave board pos
    | state == R = Empty
    | state == BR = Empty
    | state == RC = C
    | state == RD = Empty
    | state == RDB = Empty
    | state == RBB = B
    | state == BRC = BC
    | otherwise = error "Unexpected state!!!"
    where (state, _) = matrixIndexAccess board pos

stateToMove :: Board -> Position -> Bool -> State
stateToMove board pos flag
    | state == Empty = if flag then BR else R
    | state == B = if flag then RBB else BR
    | state == D = if flag then RDB else RD
    | state == C = if flag then BRC else RC
    | otherwise = error "Unexpected state!!!"
    where (state, _) = matrixIndexAccess board pos


headToB :: [Position] -> Board -> Robot -> (Board, Robot)
headToB [] _ _ = error "There is no path!!!"
headToB [x] board (pos,_,_) = moveR board pos x BR True []
headToB (x:_) board (pos,flag,_) = moveR board pos x (stateToMove board x flag) False []


headToD :: [Position] -> Board -> Robot -> (Board, Robot)
headToD [] _ _ = error "There is no path!!!"
headToD (x:_) board (pos,flag,_) = moveR board pos x (stateToMove board x flag) False []






-- this method describes the movement of proactive robots
moveProactive :: Board -> [Robot] -> (Board, [Robot])
moveProactive board robots = (board, robots)