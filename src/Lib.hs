module Lib where

replaceElement :: Int -> [a] -> a -> [a]
replaceElement index list newValue =
    (fst $ splitAt index list) ++ [ newValue ] ++ (snd $ splitAt (index + 1) list)

pointInRect :: (Float, Float) -> ((Int, Int), (Int, Int)) -> Bool
pointInRect (x, y) ((rX1, rY1), (rX2, rY2)) =
    x >= fromIntegral rX1 && x <= fromIntegral rX2 && y >= fromIntegral rY1 && y <= fromIntegral rY2
