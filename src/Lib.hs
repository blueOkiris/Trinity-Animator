module Lib where

-- Replace a singular element in a list
replaceElement :: Int -> [a] -> a -> [a]
replaceElement index list newValue =
    (fst $ splitAt index list) ++ [ newValue ] ++ (snd $ splitAt (index + 1) list)

-- Delete a specific element in a list
deleteElement :: Int -> [a] -> [a]
deleteElement index list =
    (fst $ splitAt index list) ++ (snd $ splitAt (index + 1) list)

-- Check if a point is inside of a rect defined by its upper left corner and lower right corner
pointInRect :: (Float, Float) -> ((Int, Int), (Int, Int)) -> Bool
pointInRect (x, y) ((rX1, rY1), (rX2, rY2)) =
    x >= fromIntegral rX1 && x <= fromIntegral rX2 && y >= fromIntegral rY1 && y <= fromIntegral rY2

-- Implement a ternary operator
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y
