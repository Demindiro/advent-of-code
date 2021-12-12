import qualified Data.Set   as Set
import qualified Data.Graph as Graph
import Data.List (sort, group)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

type V = Graph.Vertex
type N = String

parseInput = Graph.graphFromEdges
           . group''
           . sort
           . concat
           . sequence [id, map swap]
           . map (\x -> let (l,(_:r)) = break (=='-') x in (l, r))
           . lines
  where
    group' :: N -> [N] -> [(N, N)] -> [((), N, [N])]
    group' k v [] = [((), k, v)]
    group' k v ((k',v'):l) | k == k' = group' k (v':v) l
                           | k /= k' = ((), k, v) : group' k' [v'] l
    group'' :: [(N, N)] -> [((), N, [N])]
    group'' ((k, v):l) = group' k [v] l

visit :: (V -> (N, [N])) -> (N -> Maybe V) -> V -> V -> V -> Bool -> Set.Set V -> [[V]]
visit nfv vfk current start end twice visited
  = map (current:) $ foldMap (visitNode) nodes'
  where
    (curr, nodes) = nfv current
    nodes' = map (fromJust . vfk) nodes
    visitNode :: V -> [[V]]
    visitNode node
      | node == end = [[end]]
      | Set.member node visited && (twice' || node == start) = []
      | otherwise = visit nfv vfk node start end twice' insert
    insert | small      = Set.insert current visited
           | otherwise  = visited
    small = "a" <= curr && curr <= "z"
    twice' = twice || Set.member current visited

main = readFile "input.txt"
   >>= \x -> let (graph, nodeFromVertex, vertexFromKey) = parseInput x
                 nfv   = (\((), x, y) -> (x, y)) . nodeFromVertex
                 vfk   = vertexFromKey
                 start = fromJust $ vfk "start"
                 end   = fromJust $ vfk "end"
             in  print $ length
                       $ visit nfv vfk start start end False Set.empty

-- Debugging functions

printPaths nfv = mapM_ (putStrLn . pathToString nfv)

pathToString nfv = tail . foldr ((++) . (',':)) "" . map f
  where f = (\(_, l, _) -> l) . nfv
