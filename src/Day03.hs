{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
module Day03 where


import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Function
import Data.List
import Data.List.Split
import Data.Char
import Text.Read
import Data.Monoid
import Data.Ord


newtype Coord = Coord (Int,Int)
    deriving Semigroup  via (Sum Int, Sum Int)
    deriving Monoid     via (Sum Int, Sum Int)
    deriving Show
    deriving Eq
    deriving Ord
    
data Dir = U | D | R | L deriving (Show,Read)
data Path = Path Dir Int  deriving (Show,Read)

type Paths = ([Path],[Path])
type Steps = Sum Int

getDir :: Dir -> Coord
getDir U = Coord (0,1)
getDir D = Coord (0,-1)
getDir R = Coord (1,0)
getDir L = Coord (-1,0)



parse :: String -> Paths
parse = toTuple . map parsePaths . lines
    where
        toTuple [x,y] = (x,y)
        toTuple _ = error "bad input"

        parsePaths = map (read . ("Path " ++) . intercalate " " . groupBy (on (==) isDigit)) . splitOn ","
        



exec :: Path -> (Coord, Steps) -> Map Coord Steps -> ((Coord, Steps), Map Coord Steps)
exec (Path dir n) (pos,steps) grid = (last newSteps, Map.unionWith min grid newMap)
    where
        newMap   = Map.fromList newSteps
        newSteps = take (n+1) $ iterate (<> (getDir dir, Sum 1)) (pos, steps)

createMap :: [Path] -> Map Coord Steps
createMap paths = snd $ foldl' f mempty paths 
    where 
        f (state,grid) p = exec p state grid

findIntersection :: Paths -> Map Coord Steps
findIntersection (p1, p2) = Map.intersectionWith (+) (createMap p1) (createMap p2)

manDist :: Coord -> Int
manDist (Coord (p1,p2)) = abs p1 + abs p2

calcIntersections :: Paths -> Map Coord Steps
calcIntersections = Map.delete (Coord (0,0)) . findIntersection

solveA :: Paths -> Int
solveA = manDist . minimumBy (comparing manDist) . Map.keys . calcIntersections

solveB :: Paths -> Int
solveB = getSum . minimum . calcIntersections


        
main :: IO ()
main = do
    contents <- readFile "data/day03.txt"

    let parsed = parse contents

    print $ solveA parsed
    print $ solveB parsed
    

    
    

