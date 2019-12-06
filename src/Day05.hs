{-# LANGUAGE ViewPatterns #-}

module Day05 where

import           Control.Lens
import qualified Data.Vector as Vector
import Data.Vector (Vector, fromList ,(!),(//))
import           Data.List.Split                ( splitOn )
import           Safe                           (headMay)
import Data.Strings
import Debug.Trace

parse :: String -> (Vector Int, [Int])
parse xs = (fromList res, res)
  where res = fmap read . splitOn "," $ xs

data Mode = Pos | Inter deriving Show

getVal Inter  _ val = val
getVal Pos   ar val = ar ! val


toMode '0' = Pos
toMode '1' = Inter
toMode x = error $ "toMode " ++ show x


shit (show -> code) = (toMode a, toMode b, toMode c, read op :: Int)
    where
      --ORDER!?
      (c:b:a:op) = strPadLeft '0' 5 code

input = 1
-- input = undefined

interpretPos :: Int -> Vector Int -> [Int]
-- interpretPos pos ar = []
interpretPos pos ar
  -- | traceShow (length ar, ar ! pos, ar ! pos + 1, "pos", pos, "op", ar ! pos, shit $ ar ! pos) False = undefined

  | 99 <- ar ! pos = []

  | (am,_,_,3) <- shit $ ar ! pos
  , inputPos <- ar ! (pos+1)  
  = interpretPos (pos+2) (set (ix inputPos) 1 ar)

  
  | (am,_,_,4) <- shit $ ar ! pos 
  , v <- ar ! (pos+1)
  = getVal am ar v : interpretPos (pos+2) ar




  | (am,bm,cm,op) <- shit $ ar ! pos
  , [a,b,stored] <- fmap (ar !) [pos+1..pos+3]
  = case op of 
      1 -> interpretPos (pos+4) (doOp (+) ar (getVal am ar a) (getVal bm ar b) stored) 
      2 -> interpretPos (pos+4) (doOp (*) ar (getVal am ar a) (getVal bm ar b) stored) 
      _ -> error $ show (pos,Vector.take 15 ar,op)
  | otherwise = 
    error "opsie"

doOp :: (Int -> Int -> Int) -> Vector Int -> Int -> Int -> Int -> Vector Int
doOp (#) ar a b (ix -> lens)  
    = set lens (a # b) ar


solveA :: Vector Int -> [Int] -> [Int]
solveA ar list = interpretPos 0 ar


-- solveB :: Vector Int -> [Int] -> Maybe Int
-- solveB ar list = fmap calc $ headMay $ filter (checkTarget . f) ((,) <$> [1..1000] <*> [1..1000])
--     where
--         f (one,two) =  (! 0) <$> interpret list (ar // [(1,one),(2,two)])
--         checkTarget x = Just 19690720 == x
--         calc (x,y) = 100*x + y

main :: IO ()
main = do
  contents <- readFile "data/day05.txt"
  let (ar,list) = parse contents



  print $ solveA ar list
  -- print $ solveA ar list
  -- print $ solveB ar list