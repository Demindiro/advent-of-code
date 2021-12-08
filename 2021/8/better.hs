import Data.List (sort, group)

segments 2 _ = 1
segments 3 _ = 7
segments 4 _ = 4
segments 7 _ = 8
segments 5 [_, False, True , _, True , False, _] = 2
segments 5 [_, False, True , _, False, True , _] = 3
segments 5 [_, True , False, _, False, True , _] = 5
segments 6 [_, _, True , False, True , _, _] = 0
segments 6 [_, _, False, True , True , _, _] = 6
segments 6 [_, _, True , True , False, _, _] = 9

intersection a b = [x | x <- a, y <- b, x == y]
difference   a b = [x | x <- a, [() | y <- b, x == y] == []]
union        a b = map head $ group $ sort $ a ++ b

intersections (a:s) = foldl intersection a s
differences    a s  = foldl difference a s
unions        (a:s) = foldl union a s

toSegment l s = (length s, [intersection l' s /= [] | l' <- l])

parseLine t = let [a, b] = map words $ lines [if c == '|' then '\n' else c | c <- t] in (a, b)
parseLines = map parseLine . lines

--   0_23_56789         aaaa
-- 0_          01      b    c
-- __          23      b    c
-- 45          4_       dddd
-- 6_          _7      e    f
-- 89          89      e    f
--   __23456_89         gggg
-- 0_          01
-- 2_          _3
-- __          45
-- 6_          67
-- 8_          89
--   0_23_56_89
decode s = [a, b, c, d, e, f, g]
  where
    xf n = filter ((== n) . length) s
    [x1] = xf 2
    [x7] = xf 3
    [x4] = xf 4
    x235 = xf 5
    x069 = xf 6
    i235 = intersections x235
    i069 = intersections x069
    u235 = unions x235
    u069 = unions x069
    fid i d = differences (intersections i) d
    a = fid [i069, i235, x7]         [x1, x4]
    b = fid [i069, x4, u235]         [x1, x7]
    c = fid [u069, x1, u235, x4, x7] []
    d = fid [i235, x4, u069]         [x1, x7]
    e = fid [u069, u235]             [x1, x4, x7]
    f = fid [i069, x1, u235, x4, x7] []
    g = fid [i069, i235]             [x1, x4, x7]

uniquefy l = if l == l' then l else uniquefy l'
  where
    u  = map head $ filter ((== 1) . length) l
    l' = map (\x -> if length x == 1 then x else difference x u) l

part1 = length . filter (\x -> x == 2 || x == 3 || x == 4 || x == 7) . map length

part2 a b = let e = decode a
                [x,y,z,w] = map (uncurry segments . toSegment e) b
            in  x * 1000 + y * 100 + z * 10 + w

main = readFile "input.txt"
   >>= \x -> let l = parseLines x
             in  (print $ sum $ map (part1 . snd) l)
             >>  (print $ sum $ map (uncurry part2) l)
