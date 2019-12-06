{-# LANGUAGE TupleSections #-}

module Day06 where

import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map
                                                , (!?)
                                                )
import           Control.Lens
import           Data.List.Split
import           Data.Map.Lens
import           Data.Maybe

parse :: String -> Map String [String]
parse = Map.fromListWith (++) . fmap (f . splitOn ")") . lines
 where
  f [x, y] = (x, [y])
  f er     = error $ show er



sort graph cur
  | Just res <- graph !? cur, s <- sort graph <$> res = length res + sum s
  | otherwise = 0

solveA graph = sum $ fmap (sort graph) (Map.keys graph)

makeUndirected = Map.unionWith (++) <*> Map.fromList . concatMap g . Map.toList
 where
  g :: (String, [String]) -> [(String, [String])]
  g (x, xs) = fmap (, [x]) xs

shortest :: Set String -> Map String [String] -> String -> [Int]
shortest visited graph cur
  | Just True <- elem "SAN" <$> graph !? cur
  = [0]
  | Set.member cur visited || Map.notMember cur graph
  = []
  | Just vals <- graph !? cur, newVis <- Set.insert cur visited
  = succ <$> concatMap (shortest newVis graph) vals
  | otherwise
  = error "should not happen"

solveB graph = pred <$> shortest Set.empty (makeUndirected graph) "YOU"



main :: IO ()
main = do
  contents <- readFile "data/day06.txt"
  let parsed = parse contents

  print $ solveA parsed
  print $ solveB parsed

