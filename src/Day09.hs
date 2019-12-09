{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}

module Day09 where

import           Control.Lens
import qualified Data.Map.Strict as Map
import Data.Map (Map, fromList)
import           Data.List.Split                ( splitOn )
import           Safe                           (headMay)
import Data.Strings
import Control.Monad.Reader


parse :: String -> Map Integer Integer
parse = fromList . zip [0..] . fmap read . splitOn ","

data Mode = Pos | Inter | Rel deriving Show

m !@ k = Map.findWithDefault 0 k m


getVal :: Mode -> Map Integer Integer -> Integer -> Base -> Integer
getVal Inter  _ val base = val
getVal Pos   ar val base = ar !@ val
getVal Rel   ar val (Base b) = ar !@ (val+b)

toMode :: Char -> Mode
toMode '0' = Pos
toMode '1' = Inter
toMode '2' = Rel
toMode x = error $ "toMode " ++ show x

parseCode :: Integer -> (Mode, Mode, Mode, Integer)
parseCode (show -> code) = (toMode a, toMode b, toMode c, read op :: Integer)
    where
      (c:b:a:op) = strPadLeft '0' 5 code

newtype Base = Base Integer 
    deriving Show
    deriving Num via Integer

getNoe Inter a base = a
getNoe Pos a base = a
getNoe Rel a (Base b) = a+b


interpretPos :: Base -> Integer -> Map Integer Integer -> Reader Integer [Integer]
interpretPos base@(Base b) pos ar
  | 99 <- ar !@ pos = return []

  | (am,_,_,3) <- parseCode $ ar !@ pos
  , a <- ar !@ (pos+1)  
  , inputPos <- getNoe am a base
  = do
      input <- ask
      interpretPos base (pos+2) (Map.insert inputPos input ar)
  
  | (am,_,_,4) <- parseCode $ ar !@ pos 
  , a <- ar !@ (pos+1)
  , out <- getVal am ar a base
  = (out:) <$> interpretPos base (pos+2) ar

  | (am,_,_,9) <- parseCode $ ar !@ pos 
  , a <- ar !@ (pos+1)
  = interpretPos (Base (getVal am ar a base) + base) (pos+2) ar


  | (am,bm,cm,op) <- parseCode $ ar !@ pos
  , [a,b,stored] <- fmap (ar !@) [pos+1..pos+3]
  , aVal <- getVal am ar a base
  , bVal <- getVal bm ar b base
  , cVal <- getNoe cm stored base
  , (pos3,pos4) <- (pos+3,pos+4)
  = 
    case op of 
      1 -> interpretPos base pos4 (doOp (+) ar aVal bVal (getNoe cm stored base)) 
      2 -> 
        interpretPos base pos4 (doOp (*) ar aVal bVal (getNoe cm stored base)) 
      5 -> 
        if getVal am ar a base /= 0 then interpretPos base bVal ar else interpretPos base pos3 ar
      6 -> if getVal am ar a base == 0 then interpretPos base bVal ar else interpretPos base pos3 ar
      7 -> interpretPos base pos4 $ store (<) aVal bVal cVal ar
      8 -> interpretPos base pos4 $ store (==) aVal bVal (getNoe cm stored base) ar
      _ -> error "?"
  | otherwise = 
    error "ops"

store :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Integer -> Map Integer Integer -> Map Integer Integer
store p a b c ar = if p a b then Map.insert c 1 ar else Map.insert c 0 ar 

doOp :: (Integer -> Integer -> Integer) -> Map Integer Integer -> Integer -> Integer -> Integer -> Map Integer Integer
doOp (#) ar a b k 
    = Map.insert k (a # b) ar


solveA :: Map Integer Integer -> [Integer]
solveA = flip runReader 1 . interpretPos (Base 0) 0

solveB :: Map Integer Integer -> [Integer]
solveB = flip runReader 2 . interpretPos (Base 0) 0

test = print . solveA . parse

main :: IO ()
main = do
  contents <- readFile "data/day09.txt"
  let parsed = parse contents
  print $ solveA parsed
  print $ solveB parsed

  