{-# LANGUAGE TupleSections #-}

module Day08 where


import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.List
import Data.List.Split
import Data.List.Extra (minimumOn)
import Data.Monoid
import Data.Semigroup



parseA :: (Int,Int) -> String -> [String]
parseA (wide,tall) = chunksOf (wide*tall)

solveA :: [String] -> Maybe (Product Int)
solveA xss = do
    let ms = map (Map.fromListWith (+) . map (,1)) xss
    let mi = minimumOn (Map.findWithDefault 0 '0') ms
    foldMap (fmap Product . flip Map.lookup mi) ['1','2']

parseB :: (Int,Int) -> String -> [[Pixel]]
parseB (wide,tall) = chunksOf (wide*tall) . map toPixel

data Pixel = White | Black | Transparent

toPixel '0' = White
toPixel '1' = Black
toPixel '2' = Transparent
toPixel c = error $ show c

instance Show Pixel where 
    show White = " "
    show Black = "1"
    show Transparent = " "

instance Semigroup Pixel where
    White <> _ = White
    Black <> _ = Black
    Transparent <> r = r

instance Monoid Pixel where mempty = Transparent


solveB :: (Int,Int) -> [[Pixel]] -> [[Pixel]]
solveB (wide,tall) = chunksOf wide . fmap mconcat . transpose
--solveB (wide,tall) = chucnksOf wide . getAp . getZipList . foldMap (Ap . ZipList)
        
main :: IO ()
main = do
    contents <- readFile "data/day08.txt"
    let parsedA = parseA (25,6) contents
    
    
    print $ solveA parsedA

    let dims = (25,6)
    let parsedB = parseB dims contents
    
    mapM_ print $ solveB dims parsedB
    
    
    
    

