
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
        grid = myGrid s1 s2

data Dir = Up | Across | Diag deriving Show

data Cell = Cell Int Dir deriving Show

instance Eq Cell where
    Cell n _ == Cell n' _ = n == n'

instance Ord Cell where
    Cell n _ <= Cell n' _ = n <= n'

cellScore :: Cell -> Int
cellScore (Cell n _) = n

data Grid = Grid (Array Int (Array Int Cell)) deriving Show

addGaps :: Grid -> Seq -> Seq
addGaps = undefined

lookUp :: Grid -> Int -> Int -> Cell
lookUp (Grid a) n1 n2 =  a ! n1 ! n2


myGrid :: Seq -> Seq -> Grid
myGrid s1 s2 = Grid $ listArray (0, (BS.length s1) - 1) $ map arrayForIdx [0..]
    where
        arrayForIdx :: Int -> Array Int Cell
        arrayForIdx 0   = listArray (0, (BS.length s2) - 1) $ [Cell (scoreAt s1 0 s2 idx2) Up -* gapPenalty | idx2 <- [0..]]
        arrayForIdx idx = listArray (0, (BS.length s2) - 1) $ map bestScore [0..]
            where
                bestScore :: Int -> Cell
                bestScore 0 = Cell (scoreAt s1 idx s2 0) Across -* gapPenalty
                bestScore idx2 | (up idx2) >= (across idx2) && (up idx2) >= (diag idx2)     = up idx2
                               | (across idx2) >= (up idx2) && (across idx2) >= (diag idx2) = across idx2
                               | otherwise                                                  = diag idx2

                diag idx2   = lookUp (myGrid s1 s2) (idx-1) (idx2-1)               +* (scoreAt s1 idx s2 idx2)
                across idx2 = (lookUp (myGrid s1 s2) (idx-1) idx2)   -* gapPenalty +* (scoreAt s1 idx s2 idx2)
                up idx2     = (lookUp (myGrid s1 s2) idx (idx2-1))   -* gapPenalty +* (scoreAt s1 idx s2 idx2)




--SimilarityScoreAt :: Seq -> Int -> Seq -> Int -> Int
--difference s1 n1 s2 n2 | BS.index s1 n1 == BS.index s2 n2 = 2 --matches
                       -- | otherwise                  = -1 --doesn't match


scoreAt :: Seq -> Int -> Seq -> Int -> Int
scoreAt s1 i1 s2 i2 = similarityScore (BS.index s1 i1) (BS.index s2 i2)

similarityScore :: Char -> Char -> Int
similarityScore c1 c2 | c1 == c2  = 2
                      | otherwise = -1

(-*) :: Cell -> Int -> Cell
(-*) (Cell n dir) n' = Cell (n - n') dir
(+*) :: Cell -> Int -> Cell
(+*) (Cell n dir) n' = Cell (n + n') dir

gapPenalty = 3

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
