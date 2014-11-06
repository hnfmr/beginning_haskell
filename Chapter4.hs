-- Tree.hs
module Tree where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Tree as T

data TravelGuide = TravelGuide { title :: String, authors :: [String],
                                 price :: Double } deriving (Show, Eq, Ord)

newtype TravelGuidePrice = TravelGuidePrice TravelGuide deriving Eq

instance Ord TravelGuidePrice where
  (TravelGuidePrice (TravelGuide t1 a1 p1)) <=
    (TravelGuidePrice(TravelGuide t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))
                                 
data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Leaf
                  deriving Show

-- Binary Tres with Monoidal Cache
data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c)
                     | Leaf3
                     deriving (Show, Eq, Ord)
                     
treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c (Node3 v2 c2 l r) =
  case compare v v2 of
    EQ -> Node3 v2 c2 l r
    LT -> let newLeft   = treeInsert4 v c l
              newCached = c2 <> cached newLeft <> cached r
          in Node3 v2 newCached newLeft r
    GT -> let newRight  = treeInsert4 v c r
              newCached = c2 <> cached l <> cached newRight
          in Node3 v2 newCached l newRight
treeInsert4 v c Leaf3 = Node3 v c Leaf3 Leaf3

cached :: Monoid c => BinaryTree3 v c -> c
cached (Node3 _ c _ _) = c
cached Leaf3 = mempty

newtype Min = Min Double deriving Show

instance Monoid Min where
  mempty = Min infinity where infinity = 1/0
  mappend (Min x) (Min y) = Min $ min x y
  
modifyTravelGuidePrice :: Double -> [TravelGuide] -> [TravelGuide]
modifyTravelGuidePrice m = map (\tg -> tg { price = m * price tg })

modifyTravelGuidePriceMap :: Double -> M.Map a TravelGuide -> M.Map a TravelGuide
modifyTravelGuidePriceMap m = M.map (\tg -> tg { price = m * price tg })

modifyTravelGuidePriceTree :: Double -> T.Tree TravelGuide -> T.Tree TravelGuide
modifyTravelGuidePriceTree m = fmap (\tg -> tg { price = m * price tg })

modifyTravelGuidePrice' :: Functor f => Double -> f TravelGuide -> f TravelGuide
modifyTravelGuidePrice' m = fmap (\tg -> tg { price = m * price tg })