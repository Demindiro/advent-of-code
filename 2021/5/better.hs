import Data.List (group, sort)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Control.Monad

readInput f = Text.readFile f
          >>= let parsePoint  = map . Text.splitOn
                  splitLine t = let (l,   r) = Text.break (== ',') $ t
                                    (x, _:y) = (Text.unpack l, Text.unpack r)
                                in (read x, read y)
              in  return . (map $ map splitLine)
                         . (map $ Text.splitOn $ Text.pack " -> ")
                         . Text.lines

points d [(x, y), (x', y')] = let b a b' = [a, a + signum (b' - a) .. b']
                              in  if   x == x' || y == y'
                                  then if   x == x'
                                       then zip (repeat x) (b y y')
                                       else zip (b x x') (repeat y)
                                  else if   d && abs (x - x') == abs (y - y')
                                       then zip (b x x') (b y y')
                                       else []

count' = length . filter (> 1) . map length . group . sort

part12 d f = (readInput f :: IO [[(Int, Int)]])
         >>= return . count' . concat . map (points d)

main = (part12 False "input.txt" >>= print)
    >> (part12 True  "input.txt" >>= print)
