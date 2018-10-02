-- 计算一个列表元素的个数
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 计算列表的平均值
myAverage :: (Fractional a) => [a] -> a
myAverage xs = let sum = foldl (+) 0 xs 
                   len = fromIntegral (length xs)
               in  sum / len

-- 将一个列表变成回文序列
myBackto :: [a] -> [a]
myBackto xs = xs ++ reverse xs

-- 确定输入的列表是否是一个回文序列
isBackto :: (Eq a) => [a] -> Bool
isBackto [] = True
isBackto (_:[]) = True
isBackto xs = case head xs == last xs of
              False -> False
              True -> isBackto ms
                where
                  ms = tail . init $ xs
                  
-- 排序一个包含许多列表的列表，其排序规则基于他的子列表的长度．
import Data.List
sortfun :: [a] -> [a] -> Ordering
sortfun xs ys
          | length xs > length ys = GT
          | length xs == length ys = EQ
          | length xs < length ys = LT

sortByLength :: [[a]] -> [[a]]
sortByLength xss = sortBy sortfun xss

-- 定义一个函数，其用一个分隔符将一个包含许多列表的列表连接在一起．
intersperse1 :: a -> [[a]] -> [a]
intersperse1 _ (x:[]) = x
intersperse1 s xss = foldl (\retult xs -> retult ++ [s] ++ xs) (head xss) (tail xss)

intersperse2 :: a -> [[a]] -> [a]
intersperse2 s xs
            | null xs = []
            | length xs == 1 = head xs
            | otherwise = head xs ++ [s] ++ intersperse2 s (tail xs)

-- 求二叉树的高度
treeHeight Empty = 0
treeHeight (Node a l r)
              | lh > rh = lh + 1
              | rh >= lh = rh + 1
                where lh = treeHeight l
                      rh = treeHeight r
