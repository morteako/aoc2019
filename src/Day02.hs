{-# LANGUAGE ScopedTypeVariables #-}
module Day02 where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.List.Split (splitOn)
import qualified Data.Map as M

parse :: String -> M.Map Int Int
parse = M.fromList . zip [0..] . fmap read . splitOn ","

-- exec :: Int -> M.Map Int Int -> Maybe (M.Map Int Int)
-- exec pos tape = do
--   a <- M.lookup (pos+1) tape >>= flip M.lookup tape
--   b <- M.lookup (pos+2) tape >>= flip M.lookup tape
--   c <- M.lookup (pos+3) tape
--   case Nothing of
--     Just 1 -> exec (pos+4) (M.insert c (a+b) tape)
--     Just 2 -> exec (pos+4) (M.insert c (a*b) tape)
--     Just 99 -> return tape
    -- _       -> Nothing

exec' :: Int -> ReaderT (M.Map Int Int) Maybe Int
exec' pos = do
  a <- asks (M.lookup (pos+1)) >>= lift >>= ReaderT . M.lookup 
  b <- asks (M.lookup (pos+2)) >>= lift >>= ReaderT . M.lookup 
  c <- asks (M.lookup (pos+3)) >>= lift
  res <- ReaderT $ M.lookup pos
  case res of
    1  -> local (M.insert c (a+b)) $ exec' (pos+4)
    2  -> local (M.insert c (a*b)) $ exec' (pos+4)
    99 -> ReaderT $ M.lookup 0
    _  -> lift Nothing

exec :: Int -> ReaderT (M.Map Int Int) Maybe Int
exec pos = do
  let look x = asks (M.lookup x) >>= lift
  [a,b] <- traverse (look >=> ReaderT . M.lookup) [pos+1,pos+2] 
  [op,c] <- traverse look [pos,pos+3]
  case op of
    1  -> local (M.insert c (a+b)) $ exec (pos+4)
    2  -> local (M.insert c (a*b)) $ exec (pos+4)
    99 -> look 0
    _  -> lift Nothing

solveA = runReaderT (exec 0) . M.insert 2 2 . M.insert 1 12

-- solveB noun verb = exec 0 . M.insert 2 verb . M.insert 1 noun

main :: IO ()
main = do
    contents <- readFile "data/day02.txt"
    let parsed = parse contents

    print $ solveA parsed
    -- print 1

    -- By testing a few instances for solveB, we quickly see that the output is
    -- increased by one whenever the noun is increased by one, and increased by
    -- 216000 whenever the verb is increased by one. The output is 1114711 the
    -- output when both the noun and the verb is 0. Solving the equation
    --
    --     1114711 + noun + verb*216000 = 19690720
    --
    -- over the integers, with the additional requirement of the noun being
    -- smaller than the verb gives the solution (see extra/day02.smt2). The
    -- additional requirement is just to avoid ridiculous solutions like the
    -- noun being 19690720 - 1114711 = 18576009 and the verb being zero.

    -- print $ Just $ 86 * 100 + 9

    return ()