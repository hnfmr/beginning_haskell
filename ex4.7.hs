-- ex4.7.hs
import Tree
                  
treeFind :: Ord a => a -> BinaryTree a -> Maybe a
treeFind t (Node v l r) = case compare t v of
                            EQ -> Just v
                            LT -> treeFind t l
                            GT -> treeFind t r
treeFind _ Leaf         = Nothing

-- n for node, t for tree
treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert n t@(Node v l r) = case compare n v of
                                EQ -> t
                                LT -> Node v (treeInsert n l) r
                                GT -> Node v l (treeInsert n r)
treeInsert n Leaf           = Node n Leaf Leaf

flattenTree :: (BinaryTree a) -> [a]
flattenTree Leaf = []
flattenTree (Node v l r) =
  let subTreeList = (concat $ flattenTree l) ++ (concat $ flattenTree r) in
  v : subTreeList

concatTree :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
concatTree t1 t2 =
  let l = flattenTree t1 in
  foldr f t2 l
  where f e t = treeInsert e t
                   