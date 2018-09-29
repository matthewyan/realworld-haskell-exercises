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
                  