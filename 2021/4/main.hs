import Control.Applicative
import Control.Monad
import Data.Maybe

parseText f = readFile f
          >>= \c -> let (s:_:sl) = lines c
                    in  return (parseNumbers s, parseBoards sl)

parseBoards []     = []
parseBoards ("":l) = parseBoards l
parseBoards l      = let (b, bl) = break (== "") l
                     in  parseBoard b : parseBoards bl

parseBoard b = map parseRow b

parseRow ""      = []
parseRow (' ':l) = parseRow l
parseRow l       = let (e, el) = break (== ' ') l
                   in  read e : parseRow el

parseNumbers ""      = []
parseNumbers (',':l) = parseNumbers l
parseNumbers l       = let (n, nl) = break (== ',') l
                       in  read n : parseNumbers nl

checkRows s []    = False
checkRows s (r:l) = let c = [() | x <- r, y <- s, x == y]
                    in  length c == length r || checkRows s l

transpose ([]:_) = []
transpose m      = map head m : transpose (map tail m)

checkColumns s b = checkRows s $ transpose b

score s@(n:_) b = sum [x | x <- join b, [() | y <- s, x == y] == []] * n

checkWin []       b = Nothing
checkWin s@(_:nl) b = checkWin nl b <|> if   checkRows s b || checkColumns s b
                                        then Just $ (s, b)
                                        else Nothing

checkWins s []     = []
checkWins s (b:bl) = checkWin s b : checkWins s bl

scoreOfWinner s b op = let wo       = catMaybes $ checkWins s b
                           bs'      = [(length n, (n, b)) | (n, b) <- wo]
                           (bs, b') = snd $ op bs'
                       in  score bs b'

main = parseText "input.txt"
   >>= \(s, b) -> let s'   = reverse s
                      f op = print $ scoreOfWinner s' b op
                  in  f minimum >> f maximum
