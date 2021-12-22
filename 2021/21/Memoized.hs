import qualified Data.Map as M
import Debug.Trace

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

part2 = uncurry max $ memRecA (0,7) (0,6)
  where
    step n (s, p) = (s + score (p + n), score (p + n))
    sum' [a,b,c,d,e,f,g] = a + b * 3 + c * 6 + d * 7 + e * 6 + f * 3 + g
    sum'' l = let (a, b) = unzip l in (sum' a, sum' b)
    index a@(sa, pa) b@(sb, pb) = ((sa * 10 + (pa - 1)) * 32 + sb) * 10 + (pb - 1)
    memRecA a b = M.fromList [(((sa,pa), (sb,pb)), recA (sa, pa) (sb, pb))
                             | sa <- [0..31], pa <- [1..10], sb <- [0..31], pb <- [1..10]
                             ] M.! (a, b)
    memRecB a b = M.fromList [(((sa,pa), (sb,pb)), recB (sa, pa) (sb, pb))
                             | sa <- [0..31], pa <- [1..10], sb <- [0..31], pb <- [1..10]
                             ] M.! (a, b)
    recA a@(sa, pa) b@(sb, pb)
      | sb >= 21 = (0, 1)
      | otherwise = sum'' $ map (\x -> memRecB (step x a) b) [3..9]
      | otherwise = sum'' $ map (\x -> recB (step x a) b) [3..9]
    recB a@(sa, pa) b@(sb, pb)
      | sa >= 21 = (1, 0)
      | otherwise = sum'' $ map (\x -> memRecA a (step x b)) [3..9]
      | otherwise = sum'' $ map (\x -> recA a (step x b)) [3..9]

main = mapM_ print [part1, part2]
