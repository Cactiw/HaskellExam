
-- A4
type Env a = [(String, a)]

--instance Foldable Env where
--  foldr _ res [] = res
--  foldr f res (key x) = append (f (key x) res)
  

--eqVars :: (Eq a) => a -> Env a -> [String]
--eqVars _ [] = []
--eqVars x [(key, x):tail] = append x & (eqVars x tail)
--eqVars x [(key, e):tail] = eqVars x tail


eqVars :: (Eq a) => a -> Env a -> [String]
eqVars elem = map fst . filter (\(key, value) -> (value == elem))


--main = print (eqVars 'a' [("x", 'a'), ("y", '1'), ("foo", 'a'), ("bar", '3'), ("abc", 'a')])

fact :: (Num a, Eq a) => a -> a
fact 1 = 1
fact n = n * fact (n - 1)

expCoefs :: [Float]
expCoefs = [((-1) ** (x - 1)) / (fact (2 * x - 1)) | x <- [1, 2..]]

calc :: (Integral a, Fractional x) => a -> x -> [x] -> x
calc n x coeffs = calcWork n 1 x coeffs

calcWork :: (Integral a, Fractional x) => a -> a -> x -> [x] -> x
calcWork n curN x (curCoeff:tailCoeffs)
 | n == curN = curCoeff * (x ^^ (2 * n - 1))
 | otherwise = curCoeff * (x ^^ (2 * curN - 1)) + calcWork n (curN + 1) x tailCoeffs

main = do
  print (take 5 expCoefs)
  print (calc 100 1 expCoefs)
