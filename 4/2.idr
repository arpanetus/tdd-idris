module Main

data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) ->
                        (val : elem) ->
                        (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
        = case compare x val of
               LT => Node (insert x left) val right
               EQ => orig
               GT => Node left val (insert x right)


listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = let insertedTree = listToTree xs in
                           insert x insertedTree

treeToList : Ord a => BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ [val] ++ (treeToList right)

pshow : (Show x) => BSTree x -> String
pshow Empty = "Empty"
pshow (Node left val right) = let sleft = (pshow left) ++ " "
                                  sval = (show val) ++ " "
                                  sright = pshow right in
                                  "Node (" ++ sleft ++ sval ++ sright ++ ")"


main : IO()
main = do
  putStrLn (pshow (listToTree [1,4,3,5,2]))
  putStrLn (show (treeToList (listToTree [4,1,8,7,2,3,9,5,6])))
