import Data.Array.IO

--init_mem = [1, 0, 0, 0, 99]
--init_mem = [2, 3, 0, 3, 99]
--init_mem = [1,1,1,4,99,5,6,0,99]
init_mem = [1, 0, 0, 3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,10,19,23,2,9,23,27,1,6,27,31,2,31,9,35,1,5,35,39,1,10,39,43,1,10,43,47,2,13,47,51,1,10,51,55,2,55,10,59,1,9,59,63,2,6,63,67,1,5,67,71,1,71,5,75,1,5,75,79,2,79,13,83,1,83,5,87,2,6,87,91,1,5,91,95,1,95,9,99,1,99,6,103,1,103,13,107,1,107,5,111,2,111,13,115,1,115,6,119,1,6,119,123,2,123,13,127,1,10,127,131,1,131,2,135,1,135,5,0,99,2,14,0,0]

-- Perform a single operation
op' :: (Int -> Int -> Int) -> IOArray Int Int -> Int -> IO ()
op' op mem i = do
  x <- readArray mem (i + 1)
  y <- readArray mem (i + 2)
  z <- readArray mem (i + 3)
  x <- readArray mem x
  y <- readArray mem y
  writeArray mem z (op x y)

-- Parse a sequence of operations
parse :: IOArray Int Int -> Int -> IO ()
parse mem i = do
  op <- readArray mem i
  case op :: Int of
    1 -> do
      op' (+) mem i
      parse mem (i + 4)
    2 -> do
      op' (*) mem i
      parse mem (i + 4)
    99 -> return ()
    _ -> error "invalid op"

-- Create a mutable array from init_array
load' :: IO (IOArray Int Int)
load' = do
  mem <- newArray (0, (length init_mem) - 1) 0 :: IO (IOArray Int Int)
  sequence_ $ do
    (i, x) <- zip [0..] init_mem
    return $ writeArray mem i x
  return mem

main = do

  -- Part 1
  mem <- load'
  writeArray mem 1 12
  writeArray mem 2 2
  parse mem 0

  -- Dump result
  readArray mem 0 >>= print

  -- Bruteforce attempt to find 'search'
  let search = 19690720
  sequence_ $ do
    x <- [0..100]
    y <- [0..100]
    return $ do
      mem <- load'
      writeArray mem 1 x
      writeArray mem 2 y
      parse mem 0
      z <- readArray mem 0
      if z == search
        then print (x, y, z, 100 * x + y)
        else return ()
