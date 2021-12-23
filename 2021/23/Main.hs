import qualified Data.Array  as A
import qualified Data.IntMap as M
import qualified Data.Set    as S
import qualified Data.Heap   as H
import Data.Maybe (isJust, isNothing)
import Data.Maybe (catMaybes, fromJust)

data Amphipod = A | B | C | D deriving (Eq, Ord, Show)
-- #01x2x3x4x56#
type Hallway  = A.Array Int (Maybe Amphipod)
data Room     = Z
              | F Amphipod
              | L Amphipod
              | FF Amphipod Amphipod
              | LL Amphipod Amphipod
              | FFF Amphipod Amphipod Amphipod
              | LLL Amphipod Amphipod Amphipod
              | FFFF Amphipod Amphipod Amphipod Amphipod
              | LLLL Amphipod Amphipod Amphipod Amphipod
              deriving (Eq, Ord, Show)
type Rooms    = A.Array Int Room
data Burrow   = Burrow Hallway Rooms deriving (Ord, Eq, Show)
type RoomId     = Int
type HallwayPos = Int
type Cost = Int

cost A = 1
cost B = 10
cost C = 100
cost D = 1000

room (a,b,c,d) = FFFF a b c d

burrow a b c d = Burrow (A.listArray (0,6) $ repeat Nothing)
                        (A.listArray (0,3) $ map room [a,b,c,d])
         
input = burrow (D,D,D,B) (C,B,C,B) (A,A,B,C) (A,C,A,D)
test  = burrow (A,D,D,B) (D,B,C,C) (C,A,B,B) (A,C,A,D)

pushRoom e (Z        ) = Just (L    e      )
pushRoom e (F   a    ) = Just (LL   a e    )
pushRoom e (L   a    ) = Just (LL   a e    )
pushRoom e (FF  a b  ) = Just (LLL  a b e  )
pushRoom e (LL  a b  ) = Just (LLL  a b e  )
pushRoom e (FFF a b c) = Just (LLLL a b c e)
pushRoom e (LLL a b c) = Just (LLLL a b c e)
pushRoom e _           = Nothing

popRoom (FFFF a b c d) = Just (d, FFF a b c)
popRoom (FFF  a b c  ) = Just (c, FF  a b  )
popRoom (FF   a b    ) = Just (b, F   a    )
popRoom (F    a      ) = Just (a, Z        )
popRoom _              = Nothing

putHallway ri e hi hw | any isJust c = Nothing
                      | otherwise    = Just $ hw A.// [(hi, Just e)]
  where b x y = map revHallwayPos [min x y .. max x y]
        c = [hw A.! i | Just i <- b (roomPos ri) (hallwayPos hi)]

takeHallway ri hi hw | any isJust c        = Nothing
                     | Just e <- hw A.! hi = Just (e, hw A.// [(hi, Nothing)])
                     | otherwise           = Nothing
  where b x y = map revHallwayPos [min x y .. max x y]
        c = [hw A.! i | Just i <- b (roomPos ri) (hallwayPos hi), i /= hi]

roomPos 0 = 2
roomPos 1 = 4
roomPos 2 = 6
roomPos 3 = 8

revRoomPos 2 = 0
revRoomPos 4 = 1
revRoomPos 6 = 2
revRoomPos 8 = 3

hallwayPos 0 =  0
hallwayPos 1 =  1
hallwayPos 2 =  3
hallwayPos 3 =  5
hallwayPos 4 =  7
hallwayPos 5 =  9
hallwayPos 6 = 10

revHallwayPos  0 = Just 0
revHallwayPos  1 = Just 1
revHallwayPos  3 = Just 2
revHallwayPos  5 = Just 3
revHallwayPos  7 = Just 4
revHallwayPos  9 = Just 5
revHallwayPos 10 = Just 6
revHallwayPos  _ = Nothing

distance ri hi = abs $ roomPos ri - hallwayPos hi

moveToHallway :: RoomId -> HallwayPos -> Burrow -> Maybe (Burrow, Cost)
moveToHallway ri hi (Burrow hw rms) = do
  (e,rm) <- popRoom $ rms A.! ri
  hw     <- putHallway ri e hi hw
  let c = (distance ri hi + roomSteps rm) * cost e
  pure (Burrow hw (rms A.// [(ri, rm)]), c)
  where roomSteps (Z        ) = 4
        roomSteps (F   _    ) = 3
        roomSteps (L   _    ) = 3
        roomSteps (FF  _ _  ) = 2
        roomSteps (LL  _ _  ) = 2
        roomSteps (FFF _ _ _) = 1
        roomSteps (LLL _ _ _) = 1

moveToRoom :: HallwayPos -> RoomId -> Burrow -> Maybe (Burrow, Cost)
moveToRoom hi ri (Burrow hw rms) = do
  (e,hw) <- takeHallway ri hi hw
  rm     <- pushRoom e $ rms A.! ri
  let c = (distance ri hi + roomSteps rm) * cost e
  pure (Burrow hw (rms A.// [(ri, rm)]), c)
  where roomSteps (L    _      ) = 4
        roomSteps (LL   _ _    ) = 3
        roomSteps (LLL  _ _ _  ) = 2
        roomSteps (LLLL _ _ _ _) = 1

solved (Burrow _ rm) = f A 0 && f B 1 && f C 2 && f D 3
  where f a r = rm A.! r == LLLL a a a a

unsolvable (Burrow _ rm) = f A 0 || f B 1 || f C 2 || f D 3
  where f a r = g a $ rm A.! r
        g e (LLLL a b c d) = a /= e || b /= e || c /= e || d /= e
        g e (LLL  a b c  ) = a /= e || b /= e || c /= e
        g e (LL   a b    ) = a /= e || b /= e
        g e (L    a      ) = a /= e
        g e _              = False

type PH = H.MinPrioHeap Cost Burrow
type SB = S.Set Burrow

solve :: Burrow -> Cost
solve b = loop S.empty $ H.singleton (0, b)
  where loop :: SB -> PH -> Cost
        loop s m | solved b  = c 
                 | otherwise = loop s' m''
          where ((c, b), m') = fromJust $ H.view m
                lh  = [moveToHallway r h b | r <- [0..3], h <- [0..6]]
                lr  = [moveToRoom    h r b | r <- [0..3], h <- [0..6]]
                l   = h lr ++ h lh
                h = filter (\(b,c) -> S.notMember b s)
                  . filter (not . unsolvable . fst)
                  . catMaybes
                m'' = foldr H.insert m' $ map (\(b,c') -> (c + c', b)) l
                s' = S.insert b s

main = print $ solve input
