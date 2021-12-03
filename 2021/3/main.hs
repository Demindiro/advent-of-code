gamma l
  | l !! 0 == "" = []
  | a > b        = '0' : gamma cs 
  | otherwise    = '1' : gamma cs 
  where
    l' = map (splitAt 1) l
    c  = map fst l'
    cs = map snd l'
    a = length $ filter (== "0") c
    b = length $ filter (== "1") c

epsilon l = [if c == '0' then '1' else '0' | c <- gamma l]

bin2dec []     = 0
bin2dec (b:bs) = (if b == '1' then 2 ^ length bs else 0) + bin2dec bs

filter' :: Int -> (Int -> Int -> Bool) -> [String] -> String
filter' i op [e] = e
filter' i op l
  | op (length a) (length b) = filter' k op a
  | otherwise                = filter' k op b
  where
    k = i + 1
    a = [c | c <- l, c !! i == '0']
    b = [c | c <- l, c !! i == '1']

part1 = (\x -> (bin2dec $ gamma x) * (bin2dec $ epsilon x)) . lines

part2 = let f op = bin2dec . filter' 0 op
        in  (\x -> (f (>) x) * (f (<=) x)) . lines
    
main = readFile "input.txt" >>= \x -> print $ (part1 x, part2 x)
