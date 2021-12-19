{-# LANGUAGE BangPatterns #-}

import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Scanner  = S.Set Beacon
type Beacon   = (Int, Int, Int)
type Delta    = (Int, Int, Int)
type Matrix   = (Delta, Delta, Delta)
type Rotation = Int

parseBeacon :: String -> Beacon
parseBeacon s = (read l, read m, read r)
  where [l, m, r] = splitOn "," s

-- Translate a single scanner's points
translate :: Delta -> Scanner -> Scanner
translate (dx,dy,dz) = S.map (\(x,y,z) -> (x+dx,y+dy,z+dz))

-- All possible rotations of a scanner
rotations :: [Matrix]
rotations = [id $! matMul b a | a <- rot_a, b <- rot_b]
  where
    dot (ax,ay,az) (bx,by,bz) = ax * bx + ay * by + az * bz
    col 0 ((x,_,_), (y,_,_), (z,_,_)) = (x, y, z)
    col 1 ((_,x,_), (_,y,_), (_,z,_)) = (x, y, z)
    col 2 ((_,_,x), (_,_,y), (_,_,z)) = (x, y, z)
    matMul :: Matrix -> Matrix -> Matrix
    matMul (a0, a1, a2) b = ( (f a0 0, f a0 1, f a0 2)
                            , (f a1 0, f a1 1, f a1 2)
                            , (f a2 0, f a2 1, f a2 2) )
      where f m n = dot m $ col n b
    rot_a = [ (( 1,  0,  0), ( 0,  1,  0), ( 0,  0,  1))
            , (( 0,  0,  1), ( 0,  1,  0), (-1,  0,  0))
            , ((-1,  0,  0), ( 0,  1,  0), ( 0,  0, -1))
            , (( 0,  0, -1), ( 0,  1,  0), ( 1,  0,  0)) ]
    rot_b = [ (( 1,  0,  0), ( 0,  1,  0), ( 0,  0,  1))
            , ((-1,  0,  0), ( 0, -1,  0), ( 0,  0,  1))
            , (( 0,  1,  0), (-1,  0,  0), ( 0,  0,  1))
            , (( 0, -1,  0), ( 1,  0,  0), ( 0,  0,  1))
            , (( 1,  0,  0), ( 0,  0, -1), ( 0,  1,  0))
            , matMul ((-1, 0, 0), (0, 1,  0), (0, 0, -1))
                     (( 1, 0, 0), (0, 0, -1), (0, 1,  0)) ]

-- Rotate a single scanner's points
rotate :: Matrix -> Scanner -> Scanner
rotate (a,b,c) = S.map (\p -> (f p a, f p b, f p c))
  where f (x,y,z) (u,v,w) = u*x + v*y + z*w

-- Try to merge two scanners
merge :: Scanner -> Scanner -> Maybe Scanner
merge a b = if   S.size (S.intersection a b) >= 12
            then Just $! S.union a b
            else Nothing

-- Merge with any scanner in the list, returning the composite scanner,
-- the location of the scanner and the remaining scanners
mergeAny :: Scanner -> [Scanner] -> (Scanner, Delta, [Scanner])
mergeAny !a (b:bs) | ((d,!s):_) <- bm            = (s, d, bs )
                   | (s,d,r)    <- mergeAny a bs = (s, d, b:r)
  where
    -- All translations of all rotations of B
    bt = [ let !d = (x'-x,y'-y,z'-z)
           in  merge a (translate d b') >>= \s -> pure $! (d, s)
         | (x',y',z') <- S.toList a
         , b'         <- map ((flip rotate) b) $! rotations
         , (x, y, z ) <- S.toList b'
         ]
    -- All successful merges
    bm = catMaybes $ bt

-- Create a composite scanner and take note of the location of each scanner
-- as well
composite :: [Scanner] -> (Scanner, [Delta])
composite (a:s) = loop a s []
  where
    loop :: Scanner -> [Scanner] -> [Delta] -> (Scanner, [Delta])
    loop !c [] d = (c, (0,0,0):reverse d)
    loop !c !s d = loop c' s' (d':d)
      where (c',d',s') = mergeAny c s

-- Determine the maximum Manhattan distance between any two points in a list
maxManhattan :: [Delta] -> Int
maxManhattan s
  | [a,b]        <- s = d a b
  | (a:s'@(b:_)) <- s = max (m a s') (maxManhattan s')
  where
    m :: Delta -> [Delta] -> Int
    m a [b]    = d a b
    m a (b:bs) = max (d a b) $ m a bs
    d :: Delta -> Delta -> Int
    d (x,y,z) (u,v,w) = sum $ map abs [x-u, y-v, z-w]

main = map (S.fromList . map parseBeacon . tail)
    <$> filter (/= [])
    <$> splitOn [""]
    <$> lines
    <$> readFile "input.txt"
   >>= \s -> let (s', d) = composite s
             in  mapM_ print [S.size s', maxManhattan d]
