import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.Array      as A
import Debug.Trace

type Grid  = A.Array Pos Risk
type Flood = M.Map Pos Risk
type Scan  = S.Set Pos
type Pos   = (Int, Int)
type Risk  = Int

parseInput :: String -> Grid
--parseInput :: String -> [(Pos, Risk)]
parseInput t = A.array ((0, 0), s t) $ g t
  where s t = ((length . lines) t - 1, (length . head . lines) t - 1)
        g   = foldMap (\(x,l) -> map (\(y,v) -> ((y, x), read [v])) l)
            . zip [0..] . map (zip [0..])
            . lines

bfs :: Grid -> Risk
bfs grid = loop (M.singleton (0, 0) 0) (S.singleton (0, 0))
  where
    end@(ex, ey) = snd $ A.bounds grid
    loop :: Flood -> Scan -> Risk
    loop flood scan
      | scan' == S.empty = flood M.! end
      | otherwise        = loop flood' $ scan'
      where
        (scan', flood') = S.foldr update (S.empty, flood) scan
        update :: Pos -> (Scan, Flood) -> (Scan, Flood)
        update pos (scan, flood) = (scan', flood')
          where
            flood' = foldr (uncurry M.insert) flood pr
            scan' = foldr S.insert scan $ map fst pr
            nb = neighbours pos
            rs = map ((+ val) . (grid A.!)) nb
            pr = filter (\(k, v) -> get k > v) $ zip nb rs
            val = get pos
            get :: Pos -> Risk
            get = maybe maxBound id . (flip M.lookup) flood
        set k = M.insert k
        neighbours (x, y) = filter inRange
                          $ [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
        inRange (x, y) = 0 <= x && x <= ex && 0 <= y && y <= ey

enlarge :: Int -> Grid -> Grid
enlarge n grid = A.array ((0, 0), (sx * n - 1, sy * n - 1)) a
  where
    a = [ ((mx * sx + x, my * sy + y), f mx my x y)
        | x <- [0..ex]
        , y <- [0..ey]
        , mx <- [0..n - 1]
        , my <- [0..n - 1]
        ]
    f mx my x y = (mx + my + grid A.! (x, y) - 1) `mod` 9 + 1
    (ex, ey) = snd $ A.bounds grid
    (sx, sy) = (ex + 1, ey + 1)

main = parseInput <$> readFile "input.txt"
   >>= mapM_ print . sequence [bfs, bfs . enlarge 5]
