import System.Random
import GameObjects.Board
import qualified GameObjects.Robots as Robots
import qualified GameObjects.Children as Children

main = do
    putStrLn "Hello, welcome to Dirty Cleaners Simulations"
    
    putStrLn "Please input the number of rows of your board:"
    inputR <- getLine
    let row = read inputR :: Int

    putStrLn "Please input the number of columns of your board:"
    inputC <- getLine
    let col = read inputC :: Int

    putStrLn "Please input the amount of time needed for a random change in the game:"
    inputT <- getLine
    let t = read inputT :: Int

    putStrLn "Please input the number of obstacles in your board:"
    inputO <- getLine
    let o = read inputO :: Int

    putStrLn "Please input the number of children in your board:"
    inputB <- getLine
    let b = read inputB :: Int

    putStrLn "Please input the number of robots in your board:"
    inputR <- getLine
    let r = read inputR :: Int

    putStrLn "Please pick the strategy you would like for the robots (1 for reactive, 2 for proactive):"
    inputA <- getLine
    let a = read inputA :: Int

    putStrLn "Please input the initial percentage of dirt in your board:"
    inputD <- getLine
    let d = read inputD :: Float

    putStrLn "Please input the maximum amount of turns you would like the simulation to last:"
    inputM <- getLine
    let m = read inputM :: Int

    putStrLn "Please input the number of simulations you would like to run:"
    inputS <- getLine
    let s = read inputS :: Int

    gen <- getStdGen
    let (lostGames, wonGames, totalDirt) = gameSimulation row col t o b r a d m s gen
    let simNumber = fromIntegral s :: Float
    let average = totalDirt / simNumber
    putStrLn "Simulation Results:"
    putStrLn $ "Total simulations where the robots couldn't keep the house clean: " ++ show lostGames
    putStrLn $ "Total simulations where the robots managed to keep the children from generating dirt: " ++ show wonGames
    putStrLn $ "Average dirt percentage at the end: " ++ show average


gameSimulation :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Float -> Int -> Int -> StdGen -> (Int, Int, Float)
gameSimulation _ _ _ _ _ _ _ _ _ 0 _ = (0, 0, 0)
gameSimulation row col t o b r a d m s gen =
    let (board, newGen) = generateBoard row col o b r d gen ;
        robots = Robots.generate board;
        (loseGame, winGame, dirt, newGen') = gameLoop row col t t m newGen board a robots;
        (lostGames, wonGames, totalDirt) = gameSimulation row col t o b r a d m (s - 1) newGen';
        vLose = if loseGame then 1 else 0;
        vWin = if winGame then 1 else 0;
    in (vLose + lostGames, vWin + wonGames, dirt + totalDirt) 


gameLoop :: Int -> Int -> Int -> Int -> Int -> StdGen -> Board -> Int -> [Robots.Robot] -> (Bool, Bool, Float, StdGen)
gameLoop _ _ _ _ 0 gen board _ _ = let dirt = dirtPercentage board in (False, False, dirt, gen)
gameLoop row col t 0 turnsLeft gen board a robots = 
    let (newBoard, newGen) = shuffleBoard row col board gen;
        newRobots = Robots.shuffle board newBoard robots
    in gameLoop row col t t turnsLeft newGen newBoard a newRobots
gameLoop row col t tLeft turnsLeft gen board a robots = 
    let (board', newRobots) = Robots.move board a robots;
        (board'', gen') = Children.move board' gen;
        dirt = dirtPercentage board'';
        childrenAmount = getAmountState C board''
    in if childrenAmount == 0
        then (False, True, dirt, gen')
        else if dirt > 60
            then (True, False, dirt, gen')
            else gameLoop row col t (tLeft - 1) (turnsLeft - 1) gen' board'' a newRobots