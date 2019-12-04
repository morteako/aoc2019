module Day04 where


import           Data.List


twoSame :: (Int -> Int -> Bool) -> String -> Bool
twoSame op = any (op 2 . length) . group

isSorted :: String -> Bool
isSorted = all (uncurry (<=)) . (zip <*> tail)

rangeMin, rangeMax :: Int
rangeMin = 156218
rangeMax = 652527

solve :: (Int -> Int -> Bool) -> Int
solve op =
  length
    . filter ((&&) <$> twoSame op <*> isSorted)
    . map show
    $ [rangeMin .. rangeMax]

solveA, solveB :: Int
solveA = solve (>=)
solveB = solve (==)


main :: IO ()
main = do
  print $ solveA
  print $ solveB

