-- ex7.7.hs

-- TODO: maybe use LogicT, i.e. pathsLFair

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader

paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do (e_start, e_end) <- edges
                   guard $ e_start == start
                   subpath <- paths edges e_end end
                   return $ start : subpath
  in if start == end
     then return [end] `mplus` e_paths
     else e_paths

graph1 :: [(Int, Int)]
graph1 = [(2013, 501), (2013, 1004), (501, 2558)]

graph2 :: [(Int, Int)]
graph2 = [(2013,501),(501,2558),(501,1004),(1004,501),(2013,2558)]
type Graph = [(Int, Int)]

pathsReaderWriter :: (MonadReader Graph m, MonadWriter [[Int]] m) => Int -> Int -> m ()
pathsReaderWriter start end =
  do
    g <- ask
    let epaths = paths g start end
      in tell epaths
      
      