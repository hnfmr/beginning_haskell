-- Chapter4 Tree.hs

data TravelGuide = TravelGuide { title :: String, authors :: [String],
                                 price :: Double } deriving (Show, Eq, Ord)
                                 
data BinaryTree a = Node a BinaryTree BinaryTree
                  | Leaf
                  deriving Show
                