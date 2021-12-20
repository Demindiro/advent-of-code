import Data.List.Split (splitOn)
import qualified Data.Vector as V
import qualified Data.Set as S

data Pixel = Light | Dark deriving (Eq, Show)
data Image = Image Int Int (S.Set Point) Pixel
type Enhancer = V.Vector Pixel
type Point = (Int, Int)

instance Show Image where
  show (Image r c im b) = foldr ((++) . ('\n':)) "" f
    where f   = map g [-3..r+2]
          g y = [ if S.member (x, y) im then v else v' | x <- [-3..c+2]]
          [v, v'] = if b == Dark then "#." else ".#"

inv Light = Dark
inv Dark  = Light

binToDec :: [Pixel] -> Int
binToDec = f 0
  where f n []      = n
        f n (Dark :s) = f (n * 2 + 0) s
        f n (Light:s) = f (n * 2 + 1) s

parseEnhancer :: String -> Enhancer
parseEnhancer = V.fromList . f
  where f []       = []
        f ('.':es) = Dark  : f es
        f ('#':es) = Light : f es

sample :: Point -> Image -> Int
sample (x, y) (Image _ _ im bs) = binToDec $ reverse l
  where f p = if S.member p im then inv bs else bs
        l   = [f (x-x',y-y') | y' <- [-1..1], x' <- [-1..1]]

enhance :: Enhancer -> Image -> Image
enhance eh im@(Image r c _ bs) = Image (r + 2) (c + 2) (S.fromList im') bs'
  where im' = [ (x,y)
              | y <- [0..r + 2]
              , x <- [0..c + 2]
              , eh V.! sample (x - 1, y - 1) im /= bs'
              ]
        bs' = if eh V.! 0 == Dark then bs else inv bs

pixelsInImage (Image _ _ im _) = S.size im

enhance' 0 eh im = im
enhance' i eh im = enhance' (i - 1) eh $ enhance eh im

main = f <$> splitOn "\n\n" <$> readFile "input.txt"
   >>= \(eh,im) -> mapM_ (print . pixelsInImage)
                         [enhance' 2 eh im, enhance' 50 eh im]
  where f [eh, im] = (parseEnhancer eh, g $ lines im)
        g im@(r:_)  = Image (length im) (length r) (h im) Dark
          where h = S.fromList
                  . map (\(y,x,_) -> (x,y))
                  . filter (\(_,_,v) -> v == '#')
                  . foldMap (\(y,l) -> map (\(x,v) -> (y,x,v)) l)
                  . zip [0..]
                  . map (zip [0..])
