

listLength :: (Num a) => [a] -> a
listLength (_ : tail) = listLength tail + 1
listLength _ = 0






distance :: (Double, Double) -> (Double, Double) -> Double
distance p1 p2 = let
  dx = fst p1 - fst p2
  dy = snd p1 - snd p2
  in sqrt (dx^2 + dy^2)


filterByDist :: Double -> [(Double, Double)] -> [(Double, Double)]
filterByDist maxDist ((x, y):tail) = let
  dist = distance (x, y) (0, 0)
  tailResult = filterByDist maxDist tail
  in if (dist > maxDist)
    then [(x, y)] ++ tailResult
    else tailResult
filterByDist _ [] = []

--main = print(filterByDist 10 [(0.1, 0.1), (3.5, 10.2), (-1, -2), (3,4), (10, 11)])


inStock :: [(String, Int)] -> [(String, Int)]
inStock [] = []
inStock ((name, count):tail) = let
  tailRes = inStock tail
  in if (count > 0)
    then [(name, count)] ++ tailRes
    else tailRes

--main = print(inStock [("Apple", 2), ("Coffee", 0), ("Chocolate", 5), ("Cheese", 0)])


--sumNotDivided :: [Int] -> [(Int, Int)]
--sumNotDivided [] = []
--sumNotDivided (n : ns) = let
--  currentSuitable = [x | x <- ns, (((x + n) `mod` n) /= 0) && (((x + n) `mod` x) /= 0)]
--  resTail = sumNotDivided ns
--  in (map(\j -> (n, j)) currentSuitable) ++ resTail

sumNotDivided :: [Int] -> [(Int, Int)]
sumNotDivided xs = [(x, y) | x <- xs, y <- xs, (((x + y) `mod` y) /= 0) && (((x + y) `mod` x) /= 0), x < y]

main = print(sumNotDivided [2..5])

