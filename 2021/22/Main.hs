{-# LANGUAGE BangPatterns #-}

import Data.List.Ordered (nubSort)
import Data.List.Split (splitOn)
import qualified Data.Array.Unboxed  as A
import qualified Data.IntMap.Strict  as M

type Point = (Int, Int, Int)
-- Areas are half-open!
type Area  = (Point, Point)
type Step  = (Bool, Area)

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

main = do
  steps <- map parse <$> lines <$> readFile "input.txt"
  let steps' = filter (valid . snd) $ map bounds steps
  let (x',y',z') = unzip3 $ foldMap (\(_,(a,b)) -> [a,b]) steps
  let [x, y, z ] = map nubSort [x', y', z']
  let ![ax,ay,az] = let f l = A.listArray (0, length l - 1) l
                        f :: [Int] -> A.UArray Int Int
                    in  [f x, f y, f z]
  let ![mx,my,mz] = map M.fromList [zip x [0..], zip y [0..], zip z [0..]]
  let grid = A.listArray ((0,0,0),(length x,length y,length z)) $ repeat False
      grid :: A.UArray Point Bool
  let step g (o,((lx,ly,lz),(hx,hy,hz))) = g'
        where [sx, sy, sz] =          [mx M.! lx, my M.! ly, mz M.! lz]
              [ex, ey, ez] = map pred [mx M.! hx, my M.! hy, mz M.! hz]
              l  = [((x,y,z),o) | x <- [sx..ex], y <- [sy..ey], z <- [sz..ez]]
              g' = A.accum seq g l :: A.UArray Point Bool
  let size (ix,iy,iz) = product [ex - sx, ey - sy, ez - sz]
        where [sx, sy, sz] = [ax A.!      ix, ay A.!      iy, az A.!      iz]
              [ex, ey, ez] = [ax A.! succ ix, ay A.! succ iy, az A.! succ iz]
  -- This long version reduces runtime from ~58 sec to ~12
  -- A.assocs is very slow :(
  let count :: A.UArray Point Bool -> Int
      count g = sum [ size (x,y,z)
                    | x <- [0..length x-1]
                    , y <- [0..length y-1]
                    , z <- [0..length z-1]
                    , g A.! (x,y,z)
                    ]
  mapM_ (print . count . foldl step grid) [steps', steps]
  where
    bounds (o,a) = (o, intersection ((-50,-50,-50),(51,51,51)) a)
