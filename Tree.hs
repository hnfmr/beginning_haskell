-- Tree.hs
module Tree where

data TravelGuide = TravelGuide { title :: String, authors :: [String],
                                 price :: Double } deriving (Show, Eq, Ord)
                                 
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf
                  deriving Show
                