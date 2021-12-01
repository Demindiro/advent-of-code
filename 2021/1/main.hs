import Input

count [_]     = 0
count (x:ls@(y:_))
  | y > x     = 1 + count ls
  | otherwise =     count ls

count3 [_, _, _] = 0
count3 (x:ls@(y:z:w:_))
  | b > a        = 1 + count3 ls
  | otherwise    =     count3 ls
  where
    a = x + y + z
    b = y + z + w

main = print $ count3 input
