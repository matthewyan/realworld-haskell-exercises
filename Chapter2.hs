
-- 几种last的实现方式
myLast :: [a] -> a
myLast [] = error "error"
myLast xs = let cnt = length xs
            in xs!!(cnt-1)

myLast1 :: [a] -> a
myLast1 [] = error "error"
myLast1 (x:[]) = x
myLast1 (x:xs) = myLast1 xs

myLast2 :: [a] -> a
myLast2 [] = error "error"
myLast2 xs | length xs == 1 = xs!!0
myLast2 (x:xs) = myLast1 xs

-- 写一个函数 lastButOne, 返回列表倒数第二个元素.
lastButOne :: [a] -> a
lastButOne [] = error "error"
lastButOne (x:[]) = x
lastButOne (x:y:[]) = x
lastButOne (x:xs) = lastButOne xs

lastButOne1 xs = if null xs
                 then error "error"
                 else head . drop ((length xs) - 2) $ xs
                 