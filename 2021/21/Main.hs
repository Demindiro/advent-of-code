chunks3 :: [a] -> [(a,a,a)]
chunks3 (a:b:c:s) = (a,b,c) : chunks3 s

score :: Int -> Int
score = (+) 1 . (`mod` 10) . (+) (-1)

-- It's ugly but it works!
part1 = let c f = map snd $ filter (f . fst) $ zip [0..] $ chunks3 [1..]
            c :: (Int -> Bool) -> [(Int, Int, Int)]
            d = \n (x,y,z) -> x+y+z+n
            e = takeWhile (< 1000)
            f n = scanl (+) 0 . tail . map score . scanl d n
            a = f 7 $ c even
            b = f 6 $ c odd
            an = e a
            bn = e b
            m n = 2 * n - 3 
            x = m $ 3 * (length $ zip an bn)
            y = uncurry min $ last $ zip an bn
        in  x * y

part2 = uncurry max $ recA (0,0,7) (0,0,6)
  where
    step n (w, s, p) = (w, s + score (p + n), p + n)
    sum' [a,b,c,d,e,f,g] = a + b * 3 + c * 6 + d * 7 + e * 6 + f * 3 + g
    sum'' l = let (a, b) = unzip l in (sum' a, sum' b)
    recA a@(wa, sa, pa) b@(wb, sb, pb)
      | sb >= 21 = (wa, wb + 1)
      | otherwise = sum'' $ map (\x -> recB (step x a) b) [3..9]
    recB a@(wa, sa, pa) b@(wb, sb, pb)
      | sa >= 21 = (wa + 1, wb)
      | otherwise = sum'' $ map (\x -> recA a (step x b)) [3..9]

main = mapM_ print [part1, part2]
