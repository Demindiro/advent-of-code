import qualified Data.Array as A
import Debug.Trace

parse = (\(y,x,l) -> A.array ((0, 0), (y, x)) l)
      . (\l -> (maximum $ map (fst . fst) l, maximum $ map (snd . fst) l, l))
      . foldMap (\(y,l) -> map (\(x,v) -> ((y, x), v)) l)
      . zip [0..]
      . map (zip [0..])
      . lines

steps m = loop $ trace ("\27[2J" ++ toStr (0,0) m) m
  where
    (my,mx) = snd $ A.bounds m
    loop m | m == m' = 1
           | m /= m' = 1 + (loop $ trace ("\27[2J" ++ toStr (0,0) m') m')
      where
        n  = m A.// stepEW (0, 0)
        m' = n A.// stepNS (0, 0)
        stepEW p@(y, x)
          | y > my = []
          | x > mx = stepEW (y + 1, 0)
          | m A.! p == '>' && m A.! r == '.' = (r, '>') : (p, '.') : ns
          | otherwise = ns
          where r = (y, succ x `mod` succ mx)
                ns = stepEW (y, x + 1)
        stepNS p@(y, x)
          | y > my = []
          | x > mx = stepNS (y + 1, 0)
          | n A.! p == 'v' && n A.! d == '.' = (d, 'v') : (p, '.') : ns
          | otherwise = ns
          where d = (succ y `mod` succ my, x)
                ns = stepNS (y, x + 1)
    toStr p@(y, x) m
      | y > my = "\n"
      | x > mx = '\n' : toStr (y + 1, 0) m
      | otherwise = m A.! p : toStr (y, x + 1) m

main = parse <$> readFile "input.txt" >>= print . steps
