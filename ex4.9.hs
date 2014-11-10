-- ex4.9.hs
-- make Maybe and BinaryTree foldable

import Tree
import Data.Foldable
import Data.Maybe
import Data.Monoid

newtype Maybe' a = Maybe' (Maybe a)

instance Foldable Maybe' where
  foldr _ z (Maybe' Nothing) = z
  foldr f z (Maybe' (Just x)) = f x z
  
  foldl _ z (Maybe' Nothing) = z
  foldl f z (Maybe' (Just x)) = f z x

--data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
--                  | Leaf
--                  deriving Show
instance Foldable BinaryTree where
  foldMap f (Node x l r) = f x `mappend` foldMap f l `mappend` foldMap f r
  foldMap _ Leaf = mempty
  