solve ::Int -> [Int] -> [[Int]]
solve n c | n == 0            = [[]]
          | n <  0 || c == [] = []
          | otherwise         = concat $ map f [0..length c - 1]
  where f i = let (_, (e:es)) = splitAt i c
              in  map (e:) $ solve (n - e) es

main = solve 150 <$> map read <$> lines <$> readFile "input.txt"
   >>= \p -> mapM_ (print . length)
           $ sequence [id, filter ((== minimum (map length p)) . length)] p
