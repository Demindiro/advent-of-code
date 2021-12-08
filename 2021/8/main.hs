import Data.List (sort, group)

--  3
-- 4 0
--  5
-- 6 1
--  2

patterns 2 = [1]
patterns 3 = [7]
patterns 4 = [4]
patterns 5 = [2, 3, 5]
patterns 6 = [0, 6, 9]
patterns 7 = [8]

segments 2 _ = 1
segments 3 _ = 7
segments 4 _ = 4
segments 7 _ = 8
segments 5 [True , False, _, _, False, _, True ] = 2
segments 5 [True , True , _, _, False, _, False] = 3
segments 5 [False, True , _, _, True , _, False] = 5
segments 6 [True, _, _, _, _, False, True] = 0
segments 6 [False, _, _, _, _, True, True] = 6
segments 6 [True, _, _, _, _, True, False] = 9
segments _ _  = -1

intersection a b = [x | x <- a, y <- b, x == y]
difference   a b = [x | x <- a, [() | y <- b, x == y] == []]

indexWireMap s = case l of
  2 -> [s, s, e, e, e, e, e] -- 1
  3 -> [s, s, e, s, e, e, e] -- 7
  4 -> [s, s, e, e, s, s, e] -- 4
  5 -> [d, d, d, d, d, s, d] -- 2, 3, 5
  6 -> [d, s, s, s, s, d, d] -- 0, 6, 9
  7 -> [s, s, s, s, s, e, s] -- 8
  where
    l = length s
    d = ['a'..'g']
    e = []

d = difference ['a'..'g']
numWireMap n s = case n of
  0 -> [s, s, s, s, s, d, s]
  1 -> [s, s, d, d, d, d, d]
  2 -> [s, d, s, s, d, s, s]
  3 -> [s, s, s, s, d, s, d]
  4 -> [s, s, d, d, s, s, d]
  5 -> [d, s, s, s, s, s, d]
  6 -> [d, s, s, s, s, s, s]
  7 -> [s, s, d, s, d, d, d]
  8 -> [s, s, s, s, s, s, s]
  9 -> [s, s, s, s, s, s, d]
  where
    d = difference ['a'..'g'] s


reduce l = [foldl intersection "abcdefg" (f i l') | i <- [0..6]]
  where
    l'  = map indexWireMap l
    f i = filter (/= []) . map (!! i)

reduce' f ol [] = f
reduce' f ol (s:ss)
  | n < 0     = reduce' f ol ss
  | otherwise = let f' = map (uncurry intersection) $ zip f $ numWireMap n s
                in  reduce' f' ol $ if   f' == f
                                    then ss
                                    else ol
  where
    n = uncurry segments (toSegment f s)

uniques = map head . filter (\x -> 1 == length x)
uniquefy' l []       = []
uniquefy' l ([a]:as) = [a] : uniquefy' l as
uniquefy' l (a:as)   = difference a (uniques l) : uniquefy' l as
uniquefy l = let l' = uniquefy' l l
             in  if l == l'
                 then l
                 else uniquefy l'

toSegment l s = (length s, [intersection l' s /= [] | l' <- l])

parseLine t = let [a, b] = map words $ lines [if c == '|' then '\n' else c | c <- t] in (a, b)
parseLines = map parseLine . lines

digitCombinations = map head . group . sort . map sort

part1 t = length $ filter (\x -> length (patterns (length x)) == 1) t

part2 a b = let t = digitCombinations $ a ++ b
                e = uniquefy $ reduce' (reduce t) t t
                [x,y,z,w] = map (uncurry segments) $ map (toSegment e) b
            in x * 1000 + y * 100 + z * 10 + w

main = readFile "input.txt"
   >>= \x -> let l = parseLines x
             in  (print $ sum $ map (part1 . snd) l)
             >>  (print $ sum $ map (uncurry part2) l)
