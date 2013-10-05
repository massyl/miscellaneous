module CodingGame where
import System.IO.Error hiding(catch)
import Prelude hiding (catch)
import Control.Exception
import Data.List

closest i x y |distx == disty = max x y
              |distx > disty  = y
              |otherwise = x
              where distx = abs(x - i)
                    disty = abs(y - i)

lowertemperature :: [Int] -> Int
lowertemperature [] = 0
lowertemperature (x:xs) = foldr (closest 0) x xs

main = do
  readLn :: IO Int
  line <- catch getLine (\e -> if isEOFError e then return "" else ioError e)
  putStrLn . show . lowertemperature . map read $ words line
