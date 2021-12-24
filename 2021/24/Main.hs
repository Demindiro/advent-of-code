import qualified Data.IntMap.Strict as M

fn (a,b,c) w z = let x = fromEnum $ z `mod` 26 + b /= w
                 in  z `div` a * (25 * x + 1) + (w + c) * x

param = [ (1,13,3)
        , (1,11,12)
        , (1,15,9)
        , (26,-6,12)
        , (1,15,2)
        , (26,-8,1)
        , (26,-4,1)
        , (1,15,13)
        , (1,10,1)
        , (1,11,6)
        , (26,-11,2)
        , (26,0,11)
        , (26,-8,10)
        , (26,-7,3)
        ]

branchingEval h = best . (flip loop) (M.singleton 0 [])
  where
    loop :: [(Int,Int,Int)] -> M.IntMap [Int] -> M.IntMap [Int]
    loop []     = id
    loop (p:ps) = loop ps . M.fromListWith h . M.foldMapWithKey g
      where
        g :: Int -> [Int] -> [(Int, [Int])]
        g z l = [(fn p w z, w:l) | w <- [1..9]]
    best s | z == 0 = l
           | z /= 0 = best s'
      where
        (Just ((z, l), s')) = M.maxViewWithKey s

main = f max >> putStrLn "" >> f min >> putStrLn ""
  where f h = mapM_ (putStr . show) (reverse $ branchingEval h param)
