{-# LANGUAGE ViewPatterns #-}

module Day05 where

import           Control.Lens
import qualified Data.Vector as Vector
import Data.Vector (Vector, fromList ,(!),(//))
import           Data.List.Split                ( splitOn )
import           Safe                           (headMay)
import Data.Strings
import Control.Monad.Reader

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

interpretPos :: Int -> Vector Int -> Reader Int [Int]
interpretPos pos ar
  | 99 <- ar ! pos = return []

  | (am,_,_,3) <- parseCode $ ar ! pos
  , inputPos <- ar ! (pos+1)  
  = do
      input <- ask
      interpretPos (pos+2) (set (ix inputPos) input ar)
  
  | (am,_,_,4) <- parseCode $ ar ! pos 
  , v <- ar ! (pos+1)
  = (getVal am ar v:) <$> interpretPos (pos+2) ar

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


solveA :: Vector Int -> [Int]
solveA = flip runReader 1 . interpretPos 0

solveB :: Vector Int -> [Int]
solveB = flip runReader 5 . interpretPos 0


main :: IO ()
main = do
  contents <- readFile "data/day05.txt"
  let ar = parse contents

  print $ solveA ar 
  print $ solveB ar 
  