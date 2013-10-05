module Main where
import Data.List


conWay :: Int -> Int -> [Int]
conWay r l = iterate next' [r] !! (l-1)
next' = concatMap (\xs -> [length xs, head xs]) . group

main = do
  r <- readLn :: IO Int
  l <- readLn :: IO Int
  putStrLn . unwords . map show $ conWay r l
