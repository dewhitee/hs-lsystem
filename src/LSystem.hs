module LSystem where
import Data.Map
import Data.Aeson

-- https://hackage.haskell.org/package/hls-0.15/docs/src/LSystem-LSystem.html#lSystem

type Symbol = Char
type Axiom = [Symbol]
type Rules = Map Symbol Axiom

-- Data structure that holds index and symbols of a specific iteration of LSystem
data Iteration = Iteration { index :: Int, symbols :: [Symbol] } deriving (Eq, Show)
--instance FromJSON Iteration
--instance ToJSON Iteration

data LSystem = LSystem Axiom Rules deriving (Eq, Show)
lSystem :: Axiom -> [(Symbol, [Symbol])] -> LSystem
lSystem axiom rules = LSystem axiom (fromList rules)

getRule :: Rules -> Symbol -> [Symbol]
getRule rules symbol = findWithDefault [symbol] symbol rules

applyRule :: [Symbol] -> Rules -> [Symbol]
applyRule axiom rules = concatMap (getRule rules) axiom

getIteration :: LSystem -> Int -> [Symbol]
getIteration (LSystem axiom rules) iteration =
    case iteration of
        0 -> axiom
        _ -> getIteration (LSystem currentSymbols rules) (iteration - 1)
        where
            currentSymbols = applyRule axiom rules

getIterationWithIndex :: LSystem -> Int -> (Int, [Symbol])
getIterationWithIndex (LSystem axiom rules) iteration =
    case iteration of
        0 -> (iteration, axiom)
        _ -> getIterationWithIndexHelper (LSystem currentSymbols rules) (iteration - 1) initialIteration
        where
            currentSymbols = applyRule axiom rules
            initialIteration = iteration

getIterationWithIndexHelper :: LSystem -> Int -> Int -> (Int, [Symbol])
getIterationWithIndexHelper (LSystem axiom rules) iteration initialIteration =
    case iteration of
        0 -> (initialIteration, axiom)
        _ -> getIterationWithIndexHelper (LSystem currentSymbols rules) (iteration - 1) initialIteration
        where
            currentSymbols = applyRule axiom rules

getIterationsUntil :: LSystem -> Int -> [([Symbol])]
getIterationsUntil (LSystem axiom rules) iteration =
    case iteration of
        0 -> [axiom]
        _ -> Prelude.map (\x -> getIteration (LSystem axiom rules) x) [0..iteration]

getIterationsUntilWithIndex :: LSystem -> Int -> [(Int, [Symbol])]
getIterationsUntilWithIndex (LSystem axiom rules) iteration =
    case iteration of
        0 -> [(0, axiom)]
        _ -> Prelude.map (\x -> getIterationWithIndex (LSystem axiom rules) x) [0..iteration]
