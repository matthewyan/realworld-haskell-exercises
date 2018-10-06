-- 实现一组安全函数
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ xs !! 0

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ xs !! (length xs - 1)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ take (length xs - 1) xs

-- 写一个和words功能近似的函数splitWith，要求带一个谓词和一个任意类型元素组成的列表，在使谓词返回False的元素处分割这个列表。
myFind :: (a -> Bool) -> [a] -> Int
myFind pre [] = 0
myFind pre (x:xs)
            | pre x = 1 + myFind pre xs
            | otherwise = 0
                
mySplitWith :: (a -> Bool) -> [a] -> [[a]]
mySplitWith pre [] = []
mySplitWith pre xs = [take n xs] ++ (mySplitWith pre (drop (n+1) xs))
                        where n = myFind pre xs
