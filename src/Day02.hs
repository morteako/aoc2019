module Day02 where

import           Control.Lens
import           Data.Array
import           Data.List.Split                ( splitOn )
import           Safe


parse :: String -> (Array Int Int, [Int])
parse xs = (listArray (0, length res - 1) res, res)
  where res = fmap read . splitOn "," $ xs


interpret :: [Int] -> Array Int Int -> Maybe (Array Int Int)
interpret ops ar =
    case ops of 
        99:_  -> Just ar
        1:a:b:stored:rest -> interpret rest (doOp (+) a b stored ar) 
        2:a:b:stored:rest -> interpret rest (doOp (*) a b stored ar)
        _  -> Nothing

doOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> Array Int Int -> Array Int Int
doOp (#) a b stored ar = set (ix stored) (av # bv) ar
    where
        av = ar ! a
        bv = ar ! b


solveA :: Array Int Int -> [Int] -> Maybe Int
solveA ar list = fmap (! 0) $ interpret list (ar // [(1,12),(2,2)])


solveB :: Array Int Int -> [Int] -> Maybe Int
solveB ar list = fmap calc $ headMay $ filter (checkTarget . f) ((,) <$> [1..1000] <*> [1..1000])
    where
        f (one,two) =  (! 0) <$> interpret list (ar // [(1,one),(2,two)])
        checkTarget x = Just 19690720 == x
        calc (x,y) = 100*x + y

main :: IO ()
main = do
  contents <- readFile "data/day02.txt"
  let (ar,list) = parse contents

  print $ solveA ar list
  print $ solveB ar list



