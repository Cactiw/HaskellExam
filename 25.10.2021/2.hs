
data BiTree a = Empty | Node a (BiTree a) (BiTree a) deriving (Eq)

-- B1
instance Show a => Show (BiTree a) where
  show Empty = ""
  show (Node a l r) = showTree "" (Node a l r)


showTree :: (Show a) => String -> BiTree a -> String
showTree (offset) (Node a l r) = let
   lRes = showTree (offset ++ "  ") l
   rRes = showTree (offset ++ "  ") r

   in (offset ++ (show a) ++ "\n" ++ lRes ++ rRes)
showTree _ empty = ""


--main = putStr(show (Node 1
--        (Node 2 Empty (Node 3 Empty Empty))
--        (Node 4 (Node 5 Empty Empty) Empty)))


main = putStr(show (Node 1
        (Node 2 (Node 6 (Node 7 Empty Empty) Empty) (Node 3 Empty Empty))
        (Node 4 (Node 5 Empty Empty) Empty)))

