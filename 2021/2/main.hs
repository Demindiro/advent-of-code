splitLines ""        = []
splitLines ('\n':ls) = splitLines ls
splitLines ls        = l : splitLines ls'
  where
    (l, ls') = break (== '\n') ls

parse []           = []
parse (c:cs)
  | l == "forward" = (v,  0) : parse cs
  | l == "down"    = (0,  v) : parse cs
  | l == "up"      = (0, -v) : parse cs
  where
    (l, _:s) = break (== ' ') c
    v        = read s

points = parse . splitLines

final = foldr (\ (x, y) (x', y') -> (x + x', y + y')) (0, 0)

aim a []         = []
aim a ((x, y):l) = (x, x * a) : aim (y + a) l

part1 x = points x
part2 x = aim 0 $ points x

main = readFile "input.txt"
   >>= print . (\(x, y) -> x * y) . final . part2
