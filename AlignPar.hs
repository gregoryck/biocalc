
module AlignPar where
import Data.ByteString.Char8 as BS hiding (map)

import Control.Monad.Par.Scheds.Trace as P
import Data.Array
--import Data.IntMap hiding (map)
import Data.Traversable

tobs :: String -> BS.ByteString
tobs = BS.pack

space :: BS.ByteString
space = tobs " "

(@!) :: BS.ByteString -> Int -> Char
(@!) = BS.index

----format :: [(a, a)] -> [[a]]
--format l = [map fst l, map snd l]


--traverseWithKey :: Applicative t => (Key -> a -> t b) -> IntMap a -> t (IntMap b)
--fork :: Par () -> Par ()
--spawn :: NFData a => Par a -> Par (IVar a)
--new :: Par (IVar a)
--get :: IVar a -> Par a
--put :: NFData a => IVar a -> a -> Par ()
--runPar :: Par a -> a
--return :: a -> Par a    -- (for example)

type Seq = ByteString
data Aln = Aln ByteString ByteString deriving Show

align :: Seq -> Seq -> Aln
align s1 s2 = Aln s1_gaps s2_gaps
    where
        s1_gaps = addGaps grid s1
        s2_gaps = addGaps grid s2
        grid = makeGrid s1 s2

data Grid = Grid (Array Int (Array Int Int)) deriving Show

addGaps :: Grid -> Seq -> Seq
addGaps = undefined

makeGrid :: Seq -> Seq -> Grid
makeGrid s1 s2 = Grid $ listArray (0, BS.length s1) $ map arrayForIdx [0 .. BS.length s1]
    where
        arrayForIdx :: Int -> Array Int Int
        arrayForIdx idx = listArray (0, BS.length s2) $ map bestScore [0 .. BS.length s2]

        bestScore :: Int -> Int
        bestScore = undefined

difference :: Seq -> Int -> Seq -> Int -> Int
difference s1 n1 s2 n2 | BS.index s1 n1 == BS.index s2 n2 = 2 --matches
                       | otherwise                  = -1 --doesn't match


--align :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
--align da db = map BS.pack $ format $ reverse $ traceback lena lenb
--    where
--        lena = BS.length da
--        lenb = BS.length db
--        a = BS.append space da
--        b = BS.append space db

--        memscore :: IVar (IntMap Int)
--        memscore = runPar $ do
--                        m <- buildTraverse
--                        traverse get m

--        buildTraverse :: Par (IVar (IntMap Int))
--        buildTraverse = undefined


--        infix 5 @@
--        (@@) i j = (memscore ! i) ! j

--        score :: Int -> Int -> Int
--        score 0 _ = 0
--        score _ 0 = 0
--        score x y = maximum [(x-1 @@ y - 1)  + difference x y,
--                             x-1  @@ y,
--                             x    @@ y-1]



--        traceback :: Int -> Int -> [(Char, Char)]
--        traceback 0 0 = []
--        traceback x y     | x == 0               = (' '    , b @! y):traceback 0     (y-1)
--                          | y == 0               = (a @! x ,  ' '  ):traceback (x-1) 0
--                          | x @@ y == x @@ y-1   = (' ', b @! y)    :traceback x     (y-1)
--                          | x @@ y == x-1 @@ y   = (a @! x, ' ')    :traceback (x-1) y
--                          | otherwise            = (a @! x, b @! y) :traceback (x-1) (y-1)
