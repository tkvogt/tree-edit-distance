module Main (main, zhangShasha, sArray, testGraph0, testGraph1, testGraph2, testGraph3) where

import TreeEditDistance
import Data.Array

main :: IO ()
main = do putStrLn ("testGraph0 testGraph1" ++ show (zhangShasha testGraph0 testGraph1))
          putStrLn ("testGraph1 testGraph2" ++ show (zhangShasha testGraph1 testGraph2))

------------------------------------------------------------------------------------------------

sArray :: [Int] -> [Int] -> (Array (Int, Int) Int, Int, Int, [Int])
sArray l0 l1 = (listArray ((0,0),(length l0, length l1)) (take (((length l0)+1) * ((length l1)+1)) (repeat 0)),
               (length l0)+1,
               (length l1)+1,
               take (((length l0)+1) * ((length l1)+1)) (repeat 0))

--labels = Map.fromList []

testGraph0 :: AGraph
testGraph0 = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (0, [1,2], dEdge),
        (1, [3,4], dEdge),
        (4, [5], dEdge)
      ]

testGraph1 :: AGraph
testGraph1 = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (0, [1,2], dEdge),
        (1, [3], dEdge),
        (3, [4,5], dEdge)
      ]

testGraph2 :: AGraph
testGraph2 = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (0, [1,2], dEdge),
        (1, [3,4,5], dEdge)
      ]

testGraph3 :: AGraph
testGraph3 = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (0, [1,2], dEdge),
        (1, [3,4], dEdge),
        (4, [5], dEdge),
        (2, [6], dEdge),
        (6, [7,8], dEdge),
        (8, [9,10], dEdge)
      ]
