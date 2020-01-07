#! /usr/bin/runhaskell

-- main = print (take 10 [(x, y) | x <- [1, 2..], x > 5, y <- [1, 2..], y >= x * 2])
main = print (take 10 [(x, x * 2) | x <- [1, 2..], x > 5])
