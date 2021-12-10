import Data.List (sort)

data Status = Complete | Incomplete [Char] | Corrupted Int
  deriving (Show)

traverse' :: [Char] -> [Char] -> Status
traverse' [] []     = Complete
traverse' s  []     = Incomplete s
traverse' [] (c:cs) = traverse' [c] cs
traverse' sl@(s:ss) (c:cs)
  | c == ')'  = check '(' 3
  | c == ']'  = check '[' 57
  | c == '}'  = check '{' 1197
  | c == '>'  = check '<' 25137
  | otherwise = traverse' (c:sl) cs
  where
    check e x = if s /= e then Corrupted x else traverse' ss cs

part1 y = sum [x | (Corrupted x) <- map (traverse' []) y]

score' :: Int -> [Char] -> Int
score' m [] = m
score' m (c:cs)
  | c == '(' = score' (m * 5 + 1) cs
  | c == '[' = score' (m * 5 + 2) cs
  | c == '{' = score' (m * 5 + 3) cs
  | c == '<' = score' (m * 5 + 4) cs

part2 y = let l = reverse [x | (Incomplete x) <- map (traverse' []) y]
          in  (sort $ map (score' 0) l) !! (length l `div` 2)

main = fmap lines (readFile "input.txt")
   >>= \x -> print (part1 x) >> print (part2 x)
