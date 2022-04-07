{-# LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}

module LSystem where
import Data.Map
import Data.Aeson
import GHC.Generics
import Data.Maybe
import Data.Foldable
import Control.Monad
import System.Directory
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as C
--import qualified Data.Attoparsec.ByteString.Lazy as Atto
import qualified Data.Aeson.Yaml as AesonYaml
import qualified Data.Yaml as Yaml

type Symbol = Char
type Axiom = [Symbol]
type Rules = Map Symbol Axiom

-- Data structure that holds index and symbols of a specific iteration of LSystem
data Iteration = Iteration {
    index :: Int,
    symbols :: [Symbol]
} deriving (Eq, Show, Generic)

instance FromJSON Iteration
instance ToJSON Iteration where
    toJSON (Iteration index symbols) = object
        [
            "index" .= index
        ,   "symbols" .= symbols
        ]

data LSystem = LSystem Axiom Rules deriving (Eq, Show)
lSystem :: Axiom -> [(Symbol, [Symbol])] -> LSystem
lSystem axiom rules = LSystem axiom (fromList rules)

getRule :: Rules -> Symbol -> [Symbol]
getRule rules symbol = findWithDefault [symbol] symbol rules

applyRule :: [Symbol] -> Rules -> [Symbol]
applyRule axiom rules = concatMap (getRule rules) axiom

getSymbols :: LSystem -> Int -> [Symbol]
getSymbols (LSystem axiom rules) iteration =
    case iteration of
        0 -> axiom
        _ -> getSymbols (LSystem currentSymbols rules) (iteration - 1)
        where
            currentSymbols = applyRule axiom rules

getSymbolsWithIndex :: LSystem -> Int -> (Int, [Symbol])
getSymbolsWithIndex (LSystem axiom rules) iteration =
    case iteration of
        0 -> (iteration, axiom)
        _ -> getSymbolsWithIndexHelper (LSystem currentSymbols rules) (iteration - 1) initialIteration
        where
            currentSymbols = applyRule axiom rules
            initialIteration = iteration

getSymbolsWithIndexHelper :: LSystem -> Int -> Int -> (Int, [Symbol])
getSymbolsWithIndexHelper (LSystem axiom rules) iteration initialIteration =
    case iteration of
        0 -> (initialIteration, axiom)
        _ -> getSymbolsWithIndexHelper (LSystem currentSymbols rules) (iteration - 1) initialIteration
        where
            currentSymbols = applyRule axiom rules

getSymbolsUntil :: LSystem -> Int -> [[Symbol]]
getSymbolsUntil (LSystem axiom rules) iteration =
    case iteration of
        0 -> [axiom]
        _ -> Prelude.map (getSymbols (LSystem axiom rules)) [0..iteration]

getSymbolsUntilWithIndex :: LSystem -> Int -> [(Int, [Symbol])]
getSymbolsUntilWithIndex (LSystem axiom rules) iteration =
    case iteration of
        0 -> [(0, axiom)]
        _ -> Prelude.map (getSymbolsWithIndex (LSystem axiom rules)) [0..iteration]

-- Iteration
getIteration :: LSystem -> Int -> Iteration
getIteration (LSystem axiom rules) iteration =
    case iteration of
        0 -> Iteration iteration axiom
        _ -> getIterationHelper (LSystem currentSymbols rules) (iteration - 1) initialIteration
        where
            currentSymbols = applyRule axiom rules
            initialIteration = iteration

getIterationHelper :: LSystem -> Int -> Int -> Iteration
getIterationHelper (LSystem axiom rules) iteration initialIteration =
    case iteration of
        0 -> Iteration initialIteration axiom
        _ -> getIterationHelper (LSystem currentSymbols rules) (iteration - 1) initialIteration
        where
            currentSymbols = applyRule axiom rules

getIterationsUntil :: LSystem -> Int -> [Iteration]
getIterationsUntil (LSystem axiom rules) iteration =
    case iteration of
        0 -> [Iteration 0 axiom]
        _ -> Prelude.map (getIteration (LSystem axiom rules)) [0..iteration]

printIterations :: [Iteration] -> IO ()
printIterations = mapM_ print

-- File management
defaultJsonFileName :: String
defaultJsonFileName = "it.json"

defaultYamlFileName :: String
defaultYamlFileName = "it.yaml"

saveIterationToFile :: Iteration -> String -> IO ()
saveIterationToFile it "" = BS.writeFile defaultJsonFileName (encode it)
saveIterationToFile it fileName = BS.writeFile fileName (encode it)

saveIterationToFileSep :: Iteration -> String -> IO ()
saveIterationToFileSep it "" = BS.appendFile defaultJsonFileName $ encode it <> ","
saveIterationToFileSep it fileName = BS.appendFile fileName $ encode it <> ","

appendIterationToFile :: Iteration -> String -> IO ()
appendIterationToFile it "" = BS.appendFile defaultJsonFileName (encode it)
appendIterationToFile it fileName = BS.appendFile fileName (encode it)

saveIterationsToFile :: [Iteration] -> String -> IO ()
saveIterationsToFile iterations ""          = BS.writeFile defaultJsonFileName (encode iterations)
saveIterationsToFile iterations fileName    = BS.writeFile fileName (encode iterations)

saveIterationsToYaml :: [Iteration] -> String -> IO ()
saveIterationsToYaml iterations ""          = BS.writeFile defaultJsonFileName (AesonYaml.encode iterations)
saveIterationsToYaml iterations fileName    = BS.writeFile fileName (AesonYaml.encode iterations)

appendIterationsToFile :: [Iteration] -> String -> IO ()
appendIterationsToFile iterations ""        = BS.appendFile defaultJsonFileName (encode iterations)
appendIterationsToFile iterations fileName  = BS.appendFile fileName (encode iterations)

appendIterationsToYaml :: [Iteration] -> String -> IO ()
appendIterationsToYaml iterations ""        = BS.appendFile defaultYamlFileName (AesonYaml.encode iterations)
appendIterationsToYaml iterations fileName  = BS.appendFile fileName (AesonYaml.encode iterations)

loadIterationFromFile :: String -> IO (Maybe Iteration)
loadIterationFromFile "" = do
    json <- BS.readFile defaultJsonFileName
    return (decode json)
loadIterationFromFile fileName = do
    json <- BS.readFile fileName
    return (decode json)

loadIterationFromYaml :: String -> IO (Maybe Iteration)
loadIterationFromYaml "" = do
    yaml <- BS.readFile defaultYamlFileName
    return (Yaml.decode $ BS.toStrict yaml)
loadIterationFromYaml fileName = do
    yaml <- BS.readFile fileName
    return (Yaml.decode $ BS.toStrict yaml)

loadIterationsFromFile :: String -> IO (Maybe [Iteration])
loadIterationsFromFile "" = do
    json <- BS.readFile defaultJsonFileName
    return (decode json)
loadIterationsFromFile fileName = do
    json <- BS.readFile fileName
    return (decode json)

loadIterationsFromYaml :: String -> IO (Maybe [Iteration])
loadIterationsFromYaml "" = do
    yaml <- BS.readFile defaultYamlFileName
    return (Yaml.decode $ BS.toStrict yaml)
loadIterationsFromYaml fileName = do
    yaml <- BS.readFile fileName
    return (Yaml.decode $ BS.toStrict yaml)

loadIterationsArray :: String -> IO [Iteration]
loadIterationsArray "" = do
    json <- loadIterationsFromFile defaultJsonFileName
    return $ fromMaybe [] json
loadIterationsArray fileName = do
    json <- loadIterationsFromFile fileName
    return $ fromMaybe [] json
