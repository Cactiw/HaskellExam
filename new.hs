#! /usr/bin/runhaskell

data BiTree a = Leaf a | BiTree a (BiTree a) (BiTree a)

leafSum :: (Num a) => BiTree a -> a
leafSum (Leaf v) = v
leafSum (BiTree v l r) = v + (leafSum l) + (leafSum r)

main = print (leafSum (Leaf 8))
