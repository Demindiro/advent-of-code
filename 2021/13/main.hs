{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.List (nub)

data Fold = FoldX Int | FoldY Int deriving (Show)
data Point = Point Int Int deriving (Eq, Show)

parseInput :: T.Text -> ([Point], [Fold])
parseInput = (\(x, y) -> (map parseCrd x, map parseFold y))
           . (\x -> let (l, (_:r)) = break (=="") x in (l, r))
           . T.lines
  where
    parseCrd t = Point x y
      where [x, y] = map (read . T.unpack) $ T.splitOn "," t
    parseFold t | f == "x" = (FoldX . read . T.unpack) v
                | f == "y" = (FoldY . read . T.unpack) v
                where [f, v] = T.splitOn "=" $ last $ T.words t

fold :: [Point] -> [Fold] -> [Point]
fold = foldl (flip (\f' -> map (`f` f')))
  where f (Point x y) v | (FoldX u) <- v, x > u = Point (2 * u - x) y
                        | (FoldY u) <- v, y > u = Point x (2 * u - y)
                        | otherwise             = Point x y

printPoints :: [Point] -> IO ()
printPoints pl = mapM_ putStr l'
  where maxX = maximum [x | (Point x _) <- pl]
        maxY = maximum [y | (Point _ y) <- pl]
        l'   = [ (if elem (Point x y) pl then "#" else " ")
               ++ if x == maxX then "\n" else ""
               | y <- [0..maxY]
               , x <- [0..maxX]
               ]

main = parseInput <$> T.readFile "input.txt"
   >>= \(pl, fl) -> (print $ length $ nub $ fold pl [head fl])
                 >> (printPoints $ fold pl fl)
