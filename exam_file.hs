#! /usr/bin/runhaskell

data BiTree a = Empty | Leaf a | BiTree a (BiTree a) (BiTree a) deriving (Show)

isPosVert :: (Num a, Ord a) => BiTree a -> Bool
isPosVert (Empty) = True
isPosVert (Leaf v) = True
isPosVert (BiTree v l r) | isOneSubTree (BiTree v l r) = isPosVert l && (isPosVert r) && (v > 0)
                         | otherwise = (isPosVert l) && (isPosVert r)

isOneSubTree :: BiTree a -> Bool
isOneSubTree Empty = False
isOneSubTree (Leaf a) = False
isOneSubTree (BiTree v Empty Empty) = False
isOneSubTree (BiTree v Empty _) = True
isOneSubTree (BiTree v _ Empty) = True
isOneSubTree (BiTree _ _ _) = False

main = print (isPosVert (BiTree (-1) (Leaf 3) Empty))
