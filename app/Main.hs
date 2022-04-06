module Main where
import LSystem
import Lib

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    --putStrLn "Enter axiom > "
    --axiom <- getLine
    let axiom = "F+F+F"
    --putStrLn "Enter rules > "
    --rules <- getContents
    let rules = [('F', "F-F+F")]
    --putStrLn "Axiom=" ++ axiom ++ "Rules=" ++ rules
    let ls = lSystem axiom rules
    putStrLn $ "LSystem=" ++ show ls

    putStrLn "\n--- getIteration ---"
    let f p q n = getIteration (lSystem p q) n
    print $ f "F+F+F" [('F', "F-F+F")] 0
    print $ f "F+F+F" [('F', "F-F+F")] 1
    print $ f "F+F+F" [('F', "F-F+F")] 2
    print $ getIteration (lSystem "F+F+F" [('F',"F-F+F")]) 1
    print $ getIteration ls 1

    putStrLn "\n--- getIterationWithIndex---"
    print $ getIterationWithIndex ls 0  
    print $ getIterationWithIndex ls 1
    print $ getIterationWithIndex ls 2
    print $ getIterationWithIndex ls 3
    print $ getIterationWithIndex ls 4
    --print $ getIterationWithIndex ls 5

    putStrLn "\n--- getIterationWithIndex2---"
    print $ getIterationWithIndex ls 0  
    print $ getIterationWithIndex ls 1
    print $ getIterationWithIndex ls 2

    putStrLn "\n--- getIterationsUntil---"
    print $ getIterationsUntil ls 0
    print $ getIterationsUntil ls 1
    print $ getIterationsUntil ls 2
    print $ getIterationsUntil ls 3

    putStrLn "\n--- getIterationsUntilWithIndex---"
    print $ getIterationsUntilWithIndex ls 0
    print $ getIterationsUntilWithIndex ls 1
    print $ getIterationsUntilWithIndex ls 2
    print $ getIterationsUntilWithIndex ls 3

    putStrLn ""
