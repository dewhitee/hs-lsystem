import LSystem
import Lib
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let axiom = "F+F+F"
    let rules = [('F', "F-F+F")]
    let ls = lSystem axiom rules
    putStrLn $ "LSystem=" ++ show ls

    putStrLn "\n--- getIteration ---"
    let f p q n = getSymbols (lSystem p q) n
    print $ f "F+F+F" [('F', "F-F+F")] 0
    print $ f "F+F+F" [('F', "F-F+F")] 1
    print $ f "F+F+F" [('F', "F-F+F")] 2
    print $ getSymbols (lSystem "F+F+F" [('F',"F-F+F")]) 1
    print $ getSymbols ls 1

    putStrLn "\n--- getSymbolsWithIndex ---"
    print $ getSymbolsWithIndex ls 0
    print $ getSymbolsWithIndex ls 1
    print $ getSymbolsWithIndex ls 2
    print $ getSymbolsWithIndex ls 3
    print $ getSymbolsWithIndex ls 4

    putStrLn "\n--- getIteration ---"
    print $ getIteration ls 0
    print $ getIteration ls 1
    print $ getIteration ls 2

    putStrLn "\n--- getIterationsUntil ---"
    print $ getSymbolsUntil ls 0
    print $ getSymbolsUntil ls 1
    print $ getSymbolsUntil ls 2
    print $ getSymbolsUntil ls 3

    putStrLn "\n--- getIterationsUntilWithIndex ---"
    print $ getSymbolsUntilWithIndex ls 0
    print $ getSymbolsUntilWithIndex ls 1
    print $ getSymbolsUntilWithIndex ls 2
    print $ getSymbolsUntilWithIndex ls 3

    putStrLn "\n--- Encoding ---"
    let iter0 = getIteration ls 0
    let iter1 = getIteration ls 1
    let iter2 = getIteration ls 2
    let iter3 = getIteration ls 3
    let iter4 = getIteration ls 4
    print $ Iteration 1 "a"
    print iter0
    print iter1
    print iter2
    print iter3
    print iter4
    print $ encode (Iteration 1 "a")
    appendIterationToFile iter0 ""
    appendIterationToFile iter1 "iter.json"
    appendIterationToFile iter2 "iter.json"

    putStrLn "\n--- Decoding ---"
    it1 <- loadIterationFromFile ""
    it2 <- loadIterationFromFile "iter.json"
    print $ fromMaybe iter0 it1
    print $ fromMaybe iter0 it2
    print "a"

    saveIterationsToFile [iter0, iter1, iter2, iter3, iter4] "its.json"

    C.putStrLn $ encode [ iter0, iter1, iter2, iter3, iter4 ]
    C.putStrLn $ encode [ iter0, iter1 ]
    its <- loadIterationsFromFile "its.json"
    print $ fromMaybe [] its
    putStrLn ""
