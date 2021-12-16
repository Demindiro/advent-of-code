import Data.Char (ord)

type Opcode  = Int
type Version = Int
data Message = Number Version Int | Operator Version Opcode [Message]
  deriving (Show)
type PartialDecode = (Message, String)

hexToBin' = g . f
  where
    f c | '0' <= c && c <= '9' = ord c - ord '0'
        | 'A' <= c && c <= 'F' = ord c - ord 'A' + 10
    g = (!!) [[d,c,b,a] | d <- "01", c <- "01", b <- "01", a <- "01"]

hexToBin = foldr ((++) . hexToBin') ""

binToNum :: String -> Int
binToNum = f 0
  where
    f n []      = n
    f n ('0':s) = f (n * 2 + 0) s
    f n ('1':s) = f (n * 2 + 1) s

decode :: String -> PartialDecode
decode (v0:v1:v2:i0:i1:i2:ms)
  | id == "100" = let (d,e) = decodeNum ms in (Number ver $ binToNum d, e)
  | otherwise   = let (d,e) = decodeOp  ms in (Operator ver op d, e)
  where
    ver = binToNum [v0,v1,v2]
    id  = [i0,i1,i2]
    op  = binToNum id
    decodeNum :: String -> (String, String)
    decodeNum ('0':a:b:c:d:s) = (a:b:c:d : [], s)
    decodeNum ('1':a:b:c:d:s) = (a:b:c:d : n, s')
      where (n, s') = decodeNum s
    decodeOp :: String -> ([Message], String)
    decodeOp ('0':a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:s) = (loop r, z)
      where
        n' = binToNum [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o]
        (r, z) = splitAt n' s
        loop :: String -> [Message]
        loop s | all (=='0') s = []
               | otherwise     = let (d,e) = decode s in d : loop e
    decodeOp ('1':a:b:c:d:e:f:g:h:i:j:k:s) = loop s n'
      where
        n' = binToNum [a,b,c,d,e,f,g,h,i,j,k]
        loop :: String -> Int -> ([Message], String)
        loop s n | n == 0    = ([], s)
                 | otherwise = let (d , e ) = decode s
                                   (dl, s') = loop e (n - 1)
                               in  (d : dl, s')

decodeHex = decode . hexToBin

sumVersions :: Message -> Int
sumVersions (Number   v _  ) = v
sumVersions (Operator v _ m) = v + sum (map sumVersions m)

parse :: Message -> Int
parse (Number   _ n  ) = n
parse (Operator _ 0 m) = sum     $ map parse m
parse (Operator _ 1 m) = product $ map parse m
parse (Operator _ 2 m) = minimum $ map parse m
parse (Operator _ 3 m) = maximum $ map parse m
parse (Operator _ 5 m) = let [l,r] = map parse m in fromEnum $ l >  r
parse (Operator _ 6 m) = let [l,r] = map parse m in fromEnum $ l <  r
parse (Operator _ 7 m) = let [l,r] = map parse m in fromEnum $ l == r

testDecode = mapM_ (print . decodeHex) l
  where l = ["D2FE28", "38006F45291200", "EE00D40C823060"]
testVerSum = mapM_ (print . sumVersions . fst . decodeHex) l
  where l = ["8A004A801A8002F478"]
testParse = mapM_ (print . parse . fst . decodeHex) l
  where l = ["C200B40A82"]

main = fst <$> decodeHex <$> readFile "input.txt"
   >>= mapM_ print . sequence [sumVersions, parse]
