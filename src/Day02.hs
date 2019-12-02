{-# LANGUAGE ViewPatterns #-}

module Day02 where

import           Control.Lens
import           Control.Monad
import           Data.Vector hiding (length,filter)
import           Data.List.Split                ( splitOn )
import           Safe                           (headMay)


parse :: String -> (Vector Int, [Int])
parse xs = (fromList res, res)
  where res = fmap read . splitOn "," $ xs


interpret :: [Int] -> Vector Int -> Maybe (Vector Int)
interpret ops ar =
    case ops of 
        99:_  -> Just ar
        1:a:b:stored:rest -> interpret rest =<< doOp (+) ar a b stored 
        2:a:b:stored:rest -> interpret rest =<< doOp (*) ar a b stored
        _  -> Nothing

doOp :: (Int -> Int -> Int) -> Vector Int -> Int -> Int -> Int -> Maybe (Vector Int)
doOp (#) ar ((ar !?) -> Just a) ((ar !?) -> Just b) (ix -> lens)  
    = Just $ set lens (a # b) ar
doOp _ _ _ _ _ = Nothing


solveA :: Vector Int -> [Int] -> Maybe Int
solveA ar list =  interpret list (ar // [(1,12),(2,2)]) >>= (!? 0)


solveB :: Vector Int -> [Int] -> Maybe Int
solveB ar list = fmap calc $ headMay $ filter (checkTarget . f) pairs
    where
        pairs = (,) <$> [1..100] <*> [1..100]
        f (one,two) = interpret list (ar // [(1,one),(2,two)]) >>= (!? 0)
        checkTarget x = Just 19690720 == x
        calc (x,y) = 100*x + y

main :: IO ()
main = do
  contents <- readFile "data/day02.txt"
  let (ar,list) = parse contents

  print $ solveA ar list
  print $ solveB ar list



