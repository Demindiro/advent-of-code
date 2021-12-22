import Data.List (foldl')
import Data.List.Ordered (nubSort)
import Data.List.Split (splitOn)
import qualified Data.Array.Unboxed as A
--import qualified Data.STUArray as STU
import qualified Data.Set   as S
import qualified Data.IntMap.Strict as M
import Debug.Trace

--data Bool = True | False deriving (Eq, Show)
type Point = (Int, Int, Int)
-- Areas are half-open!
type Area  = (Point, Point)
type Step  = (Bool, Area)
type Grid  = A.Array Point Bool

parse :: String -> Step
parse s | ('o':'n':' ':s')     <- s = (True , f s')
        | ('o':'f':'f':' ':s') <- s = (False, f s')
  where
    f :: String -> Area
    f s = ((lx,ly,lz),(hx+1,hy+1,hz+1))
      where [[lx,hx],[ly,hy],[lz,hz]] = map g $ splitOn "," s
    g :: String -> [Int]
    g = map (read . snd . break h) . splitOn ".."
      where h x = '0' <= x && x <= '9' || x == '-'

intersection :: Area -> Area -> Area
intersection (a, b) (l, h) = (f max a l, f min b h)
  where f g (a,b,c) (x,y,z) = (g a x, g b y, g c z)

valid ((lx,ly,lz),(hx,hy,hz)) = lx < hx && ly < hy && lz < hz

size ((a,b,c),(d,e,f)) = product [d-a,e-b,f-c]

main = do
  steps' <- map parse <$> lines <$> readFile "input.txt"
  --let steps = filter (valid . snd)
  --          $ map (\(o,a) -> (o, intersection bounds a)) steps'
  let steps = steps'
  let (x',y',z') = unzip3 $ foldMap (\(_,(a,b)) -> [a,b]) steps
  let [x, y, z ] = map nubSort [x', y', z']
  let [mx,my,mz] = map M.fromList [zip x [0..], zip y [0..], zip z [0..]]
  let grid = A.listArray ((0,0,0),(length x,length y,length z)) $ repeat False
      grid :: A.UArray Point Bool
  let step (o,((lx,ly,lz),(hx,hy,hz))) g = g'
        where [sx, sy, sz] =          [mx M.! lx, my M.! ly, mz M.! lz]
              [ex, ey, ez] = map pred [mx M.! hx, my M.! hy, mz M.! hz]
              l  = [((x,y,z),o) | x <- [sx..ex], y <- [sy..ey], z <- [sz..ez]]
              g' = A.accum seq g l :: A.UArray Point Bool
  let size (ix,iy,iz) = product [ex - sx, ey - sy, ez - sz]
        where [sx, sy, sz] = [x !!      ix, y !!      iy, z !!      iz]
              [ex, ey, ez] = [x !! succ ix, y !! succ iy, z !! succ iz]
  let count = sum . map (size . fst) . filter ((==True) . snd) . A.assocs
  print $ count $ foldl' (flip step) grid steps
  where
    bounds = ((-50,-50,-50),(51,51,51))
