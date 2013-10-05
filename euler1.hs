module Euler where


divBy xs x = any (== 0) . map (x `mod`) $ xs
euler n = sum . filter (divBy [3,5]) $ [0..n]

divBy' x = x `mod` 3 == 0 || x `mod` 5 == 0
euler' n = sum . filter divBy' $ [0..n]

euler'' xs n = sum . filter (divBy xs) $ [0..n]
