#! /usr/bin/runhaskell

data BiTree a = Leaf a | BiTree a (BiTree a) (BiTree a) deriving (Eq, Show)

inc :: (Num a) => BiTree a -> BiTree a
inc (Leaf l) = Leaf (l + 1)
inc (BiTree i l r) = BiTree (i + 1) (inc l) (inc r)

minLeaf :: (Ord a) => BiTree a -> a
minLeaf (Leaf l) = l
minLeaf (BiTree v l r) = minimum [v, (minLeaf l), (minLeaf r)]

main = print (minLeaf (BiTree 1 (Leaf 2) (Leaf 3)))
