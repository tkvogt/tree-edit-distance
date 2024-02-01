{-# LANGUAGE BangPatterns, OverloadedStrings, FlexibleContexts, DeriveGeneric, DeriveAnyClass, Strict, 
    StrictData, MultiParamTypeClasses, FlexibleInstances, InstanceSigs, TupleSections, MagicHash, UnboxedTuples #-}
{-|
Module      :  TreeEditDistance
Copyright   :  (C) 2024 Tillmann Vogt

License     :  BSD-style (see the file LICENSE)
Maintainer  :  Tillmann Vogt <tillk.vogt@gmail.com>
Stability   :  provisional
Portability :  POSIX
-}
module TreeEditDistance
    ( editDistance, zhangShasha, addPostOrderIndexing, addLeftMosts, leftMostNodes, keyRoots, dEdge, extractNodes, fromAdj, show',
      AGraph, Label, EdgeLabel, EdgeAttribute
    ) where

import           Data.List (groupBy, sortBy, foldl')
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe(catMaybes, maybeToList)
import qualified Data.Vector.Unboxed as VU
import           Data.Word(Word32)
import           Graph.IntMap (Graph(..), Edge8(..), EdgeAttribute, ExtractNodeType, adjustNode, children, lookupNode)
import qualified Graph.IntMap as Graph
import Data.Array
-- import Debug.Trace

-- | If there is a faster/better algorithm, it will be put here
editDistance :: AGraph -> AGraph -> Int
editDistance tree0 tree1 = zhangShasha tree0 tree1

type AGraph = Graph Label EdgeLabel
data Label =
  Label { name :: String,
          indx :: Word32, -- a second index where by construction the left node is always smaller than the right node
          leftMost :: Word32
        } deriving (Show)
data EdgeLabel = EdgeLabel String deriving (Show)


-- Adds and index to every node int the order: left child, right child, parent, 
-- see https://youtu.be/hwiks-n7vso?t=410  . That way the index of the parent is
-- always bigger than its children and the index of the right node is always higher than the left node
addPostOrderIndexing :: Word32 -> (Word32, AGraph) -> (Word32, AGraph)
addPostOrderIndexing n (count, graph)
    | null cs = inc (count, graph)
    | otherwise = inc (foldr addPostOrderIndexing (count, graph) (reverse cs))
  where cs = VU.toList (children graph n (EdgeLabel ""))
        inc (c,g) = (c+1, adjustNode (\nl -> nl { indx = c + 1 }) n g)

-- Adds to every node the left most descendant leaf node
--           n
--         / | \
--        /  |  \
--       /   |   \
--      /    |    \
--     /     |     \
--    /      |      \
--   /       |       \
--  /        |        \
-- /         |         \   cs = children
--(head cs) (tail cs)  ..
--firstNode  rest      ..   -- applying addLeftMosts recursively
-- (leftMostNode, leftMostGraph) (_,restGraph)
--
addLeftMosts :: Word32 -> (Word32, AGraph) -> (Word32, AGraph)
addLeftMosts n (_, graph) -- Why _ ? node is not used when input
    | null cs = (n, adjust graph n n)
    | otherwise = (leftMostNode, adjust restGraph n leftMostNode)
  where cs = VU.toList (children graph n (EdgeLabel ""))
        adjust g node leftMn = adjustNode (\nl -> nl { leftMost = leftMn }) node g

        -- children are split into the first node and the rest
        (leftMostNode, leftMostGraph) -- first node
          | null cs = (0, adjust graph n n) -- Why 0? node is not used when tuple is input
          | otherwise = addLeftMosts (head cs) (0, graph)
        (_,restGraph) | null cs = (0, adjust graph n n) -- rest
                                                 -- Why 0? node is not used
                      | otherwise = foldr addLeftMosts (0, leftMostGraph) (tail cs)

-- walks through the tree in the same way as addLeftMosts
leftMostNodes :: Word32 -> AGraph -> [Word32]
leftMostNodes n graph
    | null cs = nList
    | otherwise = (concatMap (\c -> leftMostNodes c graph) cs) ++ nList -- nList at the end ~> post order
  where cs = VU.toList (children graph n (EdgeLabel ""))
        nList = maybeToList (fmap leftMost (lookupNode n graph))

keyRoots :: [Word32] -> [Word32]
keyRoots l = [fromIntegral i + 1 | (i, x) <- zip [0..] l, notElem x (drop (i + 1) l)]

type DistArray = Array (Word32, Word32) Int

-- A good explanation: https://www.youtube.com/watch?v=6Ur8B35xCj8
zhangShasha :: AGraph -> AGraph -> Int
zhangShasha tree0 tree1 = -- Debug.Trace.trace (" finalArray\n" ++ show' finalArray) $
                          finalArray ! (flen l0, flen l1)
  where
    lmGraph tree = snd (addLeftMosts root (addPostOrderIndexing root (0,tree)))
    root = 0

    lmGraph0 = lmGraph tree0
    lmGraph1 = lmGraph tree1

    l0 = leftMostNodes root lmGraph0
    l1 = leftMostNodes root lmGraph1

    li ngraph ls = catMaybes (map nList ls)
      where nList n = fmap indx (lookupNode n ngraph)

    l0i = li lmGraph0 l0
    l1i = li lmGraph1 l1

    roots = [(k0,k1) | k0 <- keyRoots l0, k1 <- keyRoots l1]

    finalArray :: DistArray
    finalArray = foldl' (treeDist tree0 tree1 l0i l1i) startArray roots

    startArray :: DistArray
    startArray = listArray ((0,0),(flen l0, flen l1)) (take (le0 * le1) (repeat 0))
      where le0 = (length l0) + 1
            le1 = (length l1) + 1

flen :: [Word32] -> Word32
flen = fromIntegral . length


treeDist :: AGraph -> AGraph -> [Word32] -> [Word32] -> DistArray -> (Word32, Word32) -> DistArray
treeDist tree0 tree1 l0 l1 ar (i, j) = tdAr // [((i,j), forestAr ! (i,j))]
  where ts = [(i1,j1) | i1 <- [(l0 !! (fromIntegral i -1))..i] ,
                        j1 <- [(l1 !! (fromIntegral j -1))..j]]
        (tdAr, forestAr) = foldl' forestDist (ar, startForestArray) ts

        startForestArray = listArray ((0,0),(i, j)) l
          where l = firstRow ++
                    (concat (take (fromIntegral i) (map firstColumn firstCol)))
        firstColumn c = c : (take (fromIntegral j) (repeat 0))

        firstRow = (take j1' (repeat 0)) ++ (take (fromIntegral j + 1 - j1') [1..])
        firstCol = (take (i1'-1) (repeat 0)) ++ (take (fromIntegral i - i1' + 1) ([1..]))

        i1' = fromIntegral (l0 !! (fromIntegral i - 1))
        j1' = fromIntegral (l1 !! (fromIntegral j - 1))

        forestDist :: (DistArray , DistArray) -> (Word32, Word32) -> (DistArray, DistArray)
        forestDist (td, fArr) (i1,j1)
            | l0 !! (fromIntegral i1 - 1) == l0 !! (fromIntegral i - 1) &&
              l1 !! (fromIntegral j1 - 1) == l1 !! (fromIntegral j - 1) =
                          (td // [((i1,j1), m)], fArr // [((i1,j1), m)])
            | otherwise = (td, fArr // [((i1,j1), minimum [fd0, fd1, fd3])])
          where (delete,insert,relabel) = (1,1,1)

                fd0 = (fArr ! (i_temp, j1)) + delete
                fd1 = (fArr ! (i1, j_temp)) + insert
                fd2 = (fArr ! (i_temp, j_temp)) + cost
                m = minimum [fd0, fd1, fd2]
                fd3 = (fArr ! (i_temp2, j_temp2)) + (td ! (i1,j1))

                i_temp = if l0 !! (fromIntegral i - 1) > i1 - 1 then 0 else i1 - 1
                j_temp = if l1 !! (fromIntegral j - 1) > j1 - 1 then 0 else j1 - 1

                i1_temp = l0 !! (fromIntegral i1 - 1) - 1
                j1_temp = l1 !! (fromIntegral j1 - 1) - 1

                i_temp2 = if l0 !! (fromIntegral i - 1) > i1_temp then 0 else i1_temp
                j_temp2 = if l1 !! (fromIntegral j - 1) > j1_temp then 0 else j1_temp

                cost | fmap name (lookupNode (i1-1) tree0) ==
                       fmap name (lookupNode (j1-1) tree1) = 0
                     | otherwise = relabel


---------------------------------------------------------------------------------------------------

instance EdgeAttribute EdgeLabel where
  fastEdgeAttr (EdgeLabel _) = 0
  edgeFromAttr = Map.fromList [(0, EdgeLabel "")]
  show_e _ = "Edge"
  bases _ = [Edge8 0]

instance Enum Label where
  toEnum n = Label (show n) 0 0 
  fromEnum (Label n _ _) = read n

instance ExtractNodeType Label where
    extractNodeType (Label _ _ _) = ""

-- | Extract nodes from both starting node and adjacent nodes
extractNodes :: [(Word32, [Word32], e)] -> Map.Map Word32 Label
extractNodes adj = Map.fromList (map labelNode nodes)
  where
    nodes = (map sel1 adj) ++ (concat (map sel2 adj))
    sel1 (x, _, _) = x
    sel2 (_, y, _) = y
    labelNode n = ( n, Label (show n) 0 0)

dEdge :: EdgeLabel
dEdge = EdgeLabel ""

fromAdj :: Map Word32 nl -> [(Word32, [Word32], EdgeLabel)] -> Graph nl EdgeLabel
fromAdj nodesMap adj = foldl (newNodes nodesMap) Graph.empty adj
  where
    newNodes :: Map Word32 nl -> Graph nl EdgeLabel -> (Word32, [Word32], EdgeLabel) -> Graph nl EdgeLabel
    newNodes nm g (n,ns,eLabel) = Graph.insertEdges (Just True) edges $
                                  Graph.insertNodes lookedUpNodes g
      where
        lookedUpNodes = catMaybes $ map addLabel (n:ns)
        addLabel n1 = fmap (\label -> (n1,label)) (Map.lookup n1 nm)
        edges = zip es edgeLbls
        es = zipWith (\n0 n1 -> (n0, n1)) (repeat n) ns
        edgeLbls = repeat eLabel

show' :: Array (Word32, Word32) Int -> String
show' a = concat (map ((++ "\n") . show . map snd) (groupBy groupx sorted))
  where sorted = sortBy (\((x,_),_) ((y,_),_) -> compare x y) (assocs a)
        groupx ((x0,_),_) ((x1,_),_) = x0 == x1
