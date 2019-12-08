{-# LANGUAGE ViewPatterns #-}

module Day07 where

import           Control.Lens
import qualified Data.Vector as Vector
import Data.Vector (Vector, fromList ,(!),(//))
import           Data.List.Split                ( splitOn )
import           Safe                           (headMay)
import Data.Strings
import Data.List
import Control.Monad.Reader
import Control.Monad.Writer
import Debug.Trace

parse :: String -> Vector Int
parse = fromList . fmap read . splitOn ","

data Mode = Pos | Inter deriving Show

getVal :: Mode -> Vector Int -> Int -> Int
getVal Inter  _ val = val
getVal Pos   ar val = ar ! val

toMode :: Char -> Mode
toMode '0' = Pos
toMode '1' = Inter
toMode x = error $ "toMode " ++ show x

parseCode :: Int -> (Mode, Mode, Mode, Int)
parseCode (show -> code) = (toMode a, toMode b, toMode c, read op :: Int)
    where
      (c:b:a:op) = strPadLeft '0' 5 code

interpretPos :: Int -> Vector Int -> Reader [Int] ([Int], Vector Int)
interpretPos pos ar
  | 99 <- ar ! pos = return ([],ar)

  | (am,_,_,3) <- parseCode $ ar ! pos
  , inputPos <- ar ! (pos+1)  
  = do
    input <- asks head
    local tail $ interpretPos (pos+2) (set (ix inputPos) input ar)
  
  | (am,_,_,4) <- parseCode $ ar ! pos 
  , v <- ar ! (pos+1)
  = do
    (xs,resar) <- interpretPos (pos+2) ar

    return (getVal am ar v:xs, resar)

  | (am,bm,cm,op) <- parseCode $ ar ! pos
  , [a,b,stored] <- fmap (ar !) [pos+1..pos+3]
  = case op of 
      1 -> interpretPos (pos+4) (doOp (+) ar (getVal am ar a) (getVal bm ar b) stored) 
      2 -> interpretPos (pos+4) (doOp (*) ar (getVal am ar a) (getVal bm ar b) stored)
      5 -> if getVal am ar a /= 0 then interpretPos (getVal bm ar b) ar else interpretPos (pos+3) ar
      6 -> if getVal am ar a == 0 then interpretPos (getVal bm ar b) ar else interpretPos (pos+3) ar
      7 -> interpretPos (pos+4) $ store (<) (getVal am ar a) (getVal bm ar b) stored ar
      8 -> interpretPos (pos+4) $ store (==) (getVal am ar a) (getVal bm ar b) stored ar
      _ -> error "?"
  | otherwise = 
    error "opsie"

store :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Vector Int -> Vector Int
store p a b c ar = if p a b then set (ix c) 1 ar else set (ix c) 0 ar 

doOp :: (Int -> Int -> Int) -> Vector Int -> Int -> Int -> Int -> Vector Int
doOp (#) ar a b (ix -> lens)  
    = set lens (a # b) ar

f :: Int -> ([Int],Vector Int) -> Writer [Int] ([Int],Vector Int)
f phase (inp,ar)  = do
  tell inp
  return $ flip runReader (phase : inp) $ interpretPos 0 ar

--mm :: Int -> [Int]
mm phases = foldr1 (>=>) funcs
    where
        funcs = map f phases

solveA ar = maximum $ map start combs
  where 
    start = getThrust ([0],ar)
    combs = permutations [0,1,2,3,4]
    getThrust tup ar = head $ fst $ fst $ runWriter $ mm ar tup

ss phases = foldr1 (>=>) (take 2 funcs)
  where
      funcs = map bf phases


bf :: Int -> ([Int],Vector Int) -> Writer [Int] ([Int],Vector Int)
bf phase (inp,ar)  = do
  tell inp
  return $ flip runReader (phase : inp) $ interpretPos 0 ar


-- solveB :: Vector Int -> [Int]
-- solve phaseNums fu ar = maximum $ map (doOne fu ar) combs
--   where 
--     combs = permutations phaseNums
  
-- doOne :: Vector Int -> [Int] -> Int
doOne ar phaseNums = getThrust (cycle [0],ar)
  where 
    getThrust tup = fst $ fst $ runWriter $ ss phaseNums tup

main :: IO ()
main = do
  contents <- readFile "data/day07.txt"
  let ar = parse contents

  -- let ar = parse "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

  -- print $ getThrust ([0],ar) [4,3,2,1,0]

  -- let ar = parse "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
  -- print $ getThrust ([0],ar) [0,1,2,3,4]

  -- let ar = parse "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
  -- print $ getThrust ([0],ar) [1,0,4,3,2]
  -- print $ solveB ar 
  -- print $ solveA ar
  -- print $ solve [5..9] ar
  
  let ar = parse "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
  print $ doOne ar [9,8,7,6,5]
  -- print $ getThrust ([0],ar) [9,8,7,6,5]