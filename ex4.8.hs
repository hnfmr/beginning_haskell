-- ex4.8.hs

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf
                  deriving Show

newtype Maybe' a = Maybe' (Maybe a)

instance Functor Maybe' where
  fmap f (Maybe' (Just x)) = Maybe' (Just $ f x)
  fmap f ((Maybe' Nothing)) = Maybe' Nothing
  
instance Functor BinaryTree where
  fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)
  fmap f Leaf         = Leaf
  