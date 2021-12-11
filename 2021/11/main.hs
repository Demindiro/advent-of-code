parseInput = foldl (++) []
           . map (\(y, l) -> map (\(x, c) -> ((x, y), read [c])) l)
           . zip [0..]
           . map (zip [0..])
           . lines

powerup = map (\(p, i) -> (p, i + 1))

flash l = l >>= (\(p, i) -> if i >= 10 then [p] else [])

flashN l = l >>= neighbours

neighbours (x, y) = [ (x + x', y + y')
                    | x' <- [-1..1]
                    , y' <- [-1..1]
                    , (x', y') /= (0, 0)
                    ]

count p = length . filter (==p)

ripple n = map (\(p, i) -> (p, if i /= 0 then i + count p n else i))

drain = map (\(p, i) -> (p, if i >= 10 then 0 else i))

step l = snd $ until ((==[]) . fst) m (f', r')
  where
    l'       = powerup l
    f'       = flashN $ flash l'
    r'       = ripple f' (drain l')
    m (_, r) = let f = flash r
               in  (f, ripple (flashN f) (drain r))

part1 x = fst $ foldl f (0, x) [1..100]
  where
    f (s, p) _ = let p' = step p
                 in  (s + count 0 (map snd p'), p')

part2 x = fst $ until (all (== 0) . map snd . snd)
                      (\(x, y) -> (x + 1, step y))
                      (0, x)

main = fmap parseInput (readFile "input.txt")
   >>= mapM_ print . sequence [part1, part2]
