step (vx, vy) (x, y) = ((vx - signum vx, vy - 1), (x + vx, y + vy))

inArea ((lx, ly), (ux, uy)) (x, y) = f lx ux x && f ly uy y
  where f a b v = a <= v && v <= b

simulate t@((_, ly), (ux, _)) vel = (inArea t p, (v, p))
  where cond (_, p@(x, y)) = inArea t p || x > ux || y < ly
        (v, p) = until cond (uncurry step) (vel, (0, 0))

testVels = [(x, y) | x <- [1..], y <- [x^2+1, x^2..(-(x^2)-1)]]

solve a@(_, (ux, _)) = map (snd . fst)
                       $ filter (fst . snd)
                       $ takeWhile c
                       $ map (\p -> (p, simulate a p)) testVels
  where c (_, (_, (_, (x, _)))) = x <= ux * 2

maxHeight y = (y + 1) * y `div` 2

test  = ((20, -10), (30, -5))
input = ((128, -142), (160, -88))

main = mapM_ print $ sequence [maxHeight . maximum, length] $ solve input
