#! /usr/bin/runhaskell

--data BiTree a = Empty | Node a | Node a (BiTree a) (BiTree a) deriving (Eq, Show)
data BiTree a = Empty | Node a (BiTree a) (BiTree a) deriving (Eq, Show)

-- A1
zipTree :: BiTree a -> BiTree b -> BiTree (a, b)
zipTree Empty _ = Empty
zipTree _ Empty = Empty
zipTree (Node v1 l1 r1) (Node v2 l2 r2) = let
  lRes = zipTree l1 l2
  rRes = zipTree r1 r2
  in Node (v1, v2) lRes rRes


--main = print (zipTree (Node 'a' Empty (Node 'c' Empty Empty))
--                       (Node 1 Empty (Node 2 Empty Empty)))

main = print (zipTree (Node 'a' (Node 'c' Empty Empty) Empty)
               (Node 1 Empty (Node 2 Empty Empty)))




