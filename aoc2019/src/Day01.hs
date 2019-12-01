module Day01 where

import Data.Function (fix)


parse :: String -> [Int]
parse = fmap read . lines


calcMass :: Int -> Int
calcMass x = div x 3 - 2

calcRecMass :: (Int -> Int) -> Int -> Int
calcRecMass r x = 
    let
        res = calcMass x
    in
        if res >= 0 then res + r res else 0
    

solveA :: [Int] -> Int
solveA = sum . fmap calcMass
        

solveB :: [Int] -> Int
solveB = sum . fmap (fix calcRecMass)


        
main :: IO ()
main = do
    contents <- readFile "data/day1.txt"
    let parsed = parse contents

    print $ solveA parsed
    print $ solveB parsed
    
    
    

