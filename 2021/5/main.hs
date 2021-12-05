import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import Control.Monad

readInput f = Text.readFile f
          >>= let parsePoint  = Prelude.map $ Text.splitOn
                  splitLine t = let (l,   r) = Text.break (== ',') $ t
                                    (x, _:y) = (Text.unpack l, Text.unpack r)
                                in (read x, read y)
              in  return . (map $ map $ \x -> splitLine x)
                         . (map $ Text.splitOn $ Text.pack " -> ")
                         . Text.lines

points d [(x, y), (x', y')] = let b a b' = [a, a + signum (b' - a) .. b']
                              in  if   x == x' || y == y'
                                  then if   x == x'
                                       then Prelude.zip (repeat x) (b y y')
                                       else Prelude.zip (b x x') (repeat y)
                                  else if   d && abs (x - x') == abs (y - y')
                                       then Prelude.zip (b x x') (b y y')
                                       else []

has' _ []     = False
has' p (p':l) = p == p' || has' p l

has _ []     = False
has p (p':l) = (if p == p' then has' else has) p l

count'' [] l     = 0
count'' (p:ps) l = (if has p l then 1 else 0) + count'' ps l

count' x = let s = (Set.toList $ Set.fromList $ x)
           in  count'' s x

part12 d f = (readInput f :: IO [[(Int, Int)]])
         >>= \x -> let s = join . Prelude.map (points d)
                   in  return $ count' $ s x

main = (part12 False "input.txt" >>= print)
    >> (part12 True  "input.txt" >>= print)
