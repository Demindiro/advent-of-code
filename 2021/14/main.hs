import qualified Data.Map.Strict as Map
import Data.List (sort, group)
import Control.Arrow ((&&&))

type Template = Map.Map String Char
type FreqMap  = Map.Map String Int
type CountMap = Map.Map Char   Int

parseInput :: String -> (String, Template)
parseInput l = (start, Map.fromList $ map f patterns)
  where (start:_:patterns) = lines l
        f p = let [l, _, r] = words p in (l, head r)

freqMap :: String -> Template -> FreqMap
freqMap [_] m = Map.empty
freqMap (l:s@(r:_)) m = add [l, r] 1 $ freqMap s m

step :: [(String, Int)] -> Template -> FreqMap -> CountMap -> (FreqMap, CountMap)
step [] templ freq count = (freq, count)
step ((p@[l,r],i):ss) templ freq count = step ss templ freq' count'
  where x   = templ Map.! p
        p'l = [l, x]
        p'r = [x, r]
        freq'   = add p'r i $ add p'l i $ sub p i freq
        count'  = add x i count

get k = maybe 0 id . Map.lookup k
add k n m = Map.insert k (get k m + n) m
sub k n m = Map.insert k (get k m - n) m

main = parseInput <$> readFile "input.txt"
   >>= \i -> mapM_ print [f 10 i, f 40 i]
  where
    f n (s, templ) = diff $ snd $ foldl (const . g) (freq, count) [1..n]
      where
        freq = freqMap s templ
        count = Map.fromList $ map (head &&& length) $ group $ sort s
        g (s, x) = step (Map.toList s) templ s x
        diff m = maximum l - minimum l
          where l = Map.elems m
