{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
module Day03 where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Lens
import Data.List
import Data.List.Split
import Data.Char
import Text.Read
import Data.Monoid
import Data.Ord
import Data.Function

-- showSet s = case Set.size s of
--     0 -> "."
--     1 -> "*"
--     n -> "X"

newtype Coord = Coord (Int,Int)
    deriving Semigroup  via (Sum Int, Sum Int)
    deriving Monoid     via (Sum Int, Sum Int)
    deriving Show
    deriving Eq
    deriving Ord

data Dir = U | D | R | L deriving (Show,Read)
data Path = Path Dir Int  deriving (Show,Read)

-- f "U7" = 

-- parse :: String -> Map (Int,Int) (Set Int)
parse = map parsePaths . lines
    where
        parsePaths = map (read @Path . ("Path " ++) . intercalate " " . groupBy f) . splitOn ","
        f x y = isDigit x && isDigit y || not (isDigit x) && not (isDigit y)

getDir U = Coord (0,1)
getDir D = Coord (0,-1)
getDir R = Coord (1,0)
getDir L = Coord (-1,0)



exec (Path dir n) pos grid = (last ss, foldr Set.insert grid newSet)
    where
        newSet = Set.fromList ss
        ss = take (n+1) $ iterate (<> getDir dir) pos

createSet paths = snd $ foldl' f start paths 
    where 
        start = (Coord (0,0), Set.empty)
        f (pos,grid) p = exec p pos grid

findIntersection (p1, p2)= Set.intersection (createSet p1) (createSet p2)

-- manDist (Coord (p1,p2)) (Coord (q1,q2)) = abs (p1-q1) + abs (p2-q2)
manDist (Coord (p1,p2)) = abs p1 + abs p2

solveA = manDist . minimumBy (comparing manDist) . Set.delete mempty . findIntersection 


solveB = undefined


        
main :: IO ()
main = do
    contents <- readFile "data/day03.txt"
    -- let contents = "R8,U5,L5,D3\nU7,R6,D4,L4"
    -- let contents = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
    let parsed = parse contents

    let [p1,p2] = parsed
    
    -- print $ createSet p1
    -- print $ createSet p2
    print $ solveA (p1, p2)
    -- print $ exec (Path U 5) (Coord (1,1)) Set.empty
    -- print $ solveA parsed
    -- print $ solveB parsed
    
    return ()
    
    

