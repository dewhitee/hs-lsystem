module Main where
import LSystem
import Data.Aeson
import Data.Maybe
import System.IO
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Data.IORef
import Data.List
import Data.List.Split

newtype LSystemRef = LSystemRef { ls :: IORef LSystem }

initialLSystem  = lSystem "F+F+F" [('F', "F-F+F")]
lSystem1        = lSystem "X" [('X', "F-[[X]+X]+F[+FX]-X"), ('F', "FF")]
lSystem2        = lSystem "F+F+F+F" [('F', "FF+F-F+F+FF")]
lSystem3        = lSystem "Y" [('X', "X[-FFF][+FFF]FX"), ('Y', "YFX[+Y][-Y]")]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    mainLoop
    putStrLn "Finished."

mainLoop :: IO ()
mainLoop = do
    putStrLn " 0 - Create new lsystem\n\
             \ 1 - Show examples\n\
             \ 2 - Load iterations from .json or .yaml file\n\
             \ 9 - Exit"
    putStr " > "
    input <- getLine
    let choice = read input :: Int
    menuChoice choice
    when (choice /= 9) mainLoop

menuChoice :: Int -> IO ()
menuChoice choice =
    case choice of
        0 -> createNewLSystem
        1 -> showExamples
        2 -> loadFromFile
        _ -> return()

createNewLSystem :: IO ()
createNewLSystem = do
    putStr "Enter axiom (e.g. F+F+F)> "
    axiom <- getLine
    putStr "Enter count of rules > "
    rulesCount <- getLine
    rules <- enterRules (read rulesCount :: Int)
    let ls = lSystem axiom rules
    putStrLn "\nNew LSystem was successfully created: "
    print ls
    putStr "\nDo you want to save iterations to .json or .yaml file? 0 (NO) or 1 (YES, PLEASE) > "
    saveChoice <- getLine
    case read saveChoice :: Int of
        1 -> saveToFile ls
        _ -> return()
    return()

enterRules :: Int -> IO [(Symbol, [Symbol])]
enterRules count = mapM (const enterSingleRule) [0..count-1]

enterSingleRule :: IO (Symbol, [Symbol])
enterSingleRule = do
    putStr "\tEnter symbol > "
    symbol <- getLine
    putStr "\tEnter rule for this symbol (e.g. F-F+F) > "
    rule <- getLine
    return (head symbol, rule)

showExamples :: IO ()
showExamples = do
    putStrLn "\nChoose LSystem you want to see."
    mapM_ (\(x, y) -> putStrLn $ show x ++ " - " ++ show y) [(0, initialLSystem), (1, lSystem1), (2, lSystem2), (3, lSystem3)]
    putStr " > "
    exampleChoice <- getLine

    putStr "\tHow many iterations you want to see? > "
    iterationCount <- getLine
    let count = (read iterationCount :: Int) - 1
    case read exampleChoice of
        0 -> printIterations $ getIterationsUntil initialLSystem count
        1 -> printIterations $ getIterationsUntil lSystem1 count
        2 -> printIterations $ getIterationsUntil lSystem2 count
        3 -> printIterations $ getIterationsUntil lSystem3 count
        _ -> print ""
    putStrLn ""

getFileExt :: String -> String
getFileExt fileName = "." ++ last (wordsBy (=='.') fileName)

saveToFile :: LSystem -> IO ()
saveToFile ls = do
    putStr "\tEnter filename (.json or .yaml) > "
    fileName <- getLine
    let fileExtension = getFileExt fileName
    putStr "\tHow many iterations you want to save? > "
    iterationCount <- getLine
    let iterations = getIterationsUntil ls ((read iterationCount :: Int) - 1)
    case fileExtension of
        ".json" -> do
            saveIterationsToFile iterations fileName
            putStrLn $ "Iterations saved to the .json " ++ fileName ++ " file."
        ".yaml" -> do
            saveIterationsToYaml iterations fileName
            putStrLn $ "Iterations saved to the .yaml " ++ fileName ++ " file."
        _       -> do return()
    putStrLn ""

loadFromFile :: IO ()
loadFromFile = do
    putStr "\tEnter filename (.json or .yaml) > "
    fileName <- getLine
    let fileExtension = getFileExt fileName
    iterations <- case fileExtension of
        ".json" -> loadIterationsFromFile fileName
        ".yaml" -> loadIterationsFromYaml fileName
        _       -> loadIterationsFromFile fileName
    putStrLn $ "\tIterations loaded from the " ++ fileName ++ " file:"
    printIterations $ fromMaybe [] iterations
    putStrLn ""
        