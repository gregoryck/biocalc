
module AlignPar where
import Data.ByteString.Char8 as BS hiding (map, concat, zip)

--import Control.Monad.Par.Scheds.Trace as P
import Data.Array
--import Debug.Trace (trace)

tobs :: String -> BS.ByteString
tobs = BS.pack

space :: BS.ByteString
space = tobs " "

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

data SeqPosition = Top | Side

align :: Seq -> Seq -> Aln
align s1 s2 = Aln s1_gaps s2_gaps
    where
        s1_gaps = addGaps Side grid s1
        s2_gaps = addGaps Top grid s2
        grid = myGrid s1 s2

addGaps :: SeqPosition -> Grid -> Seq -> Seq
addGaps pos g s = BS.pack $ concat $ [letterWithGaps pos x | x <- zip (path g) (BS.unpack s)]

letterWithGaps :: SeqPosition -> (Cell, Char) -> [Char]
letterWithGaps Top (Across _, c) = [' ', c]
letterWithGaps Top (Up _, c) = [c]
letterWithGaps Side (Across _, c) = [c]
letterWithGaps Side (Up _, c) = [' ', c]
letterWithGaps _ (Diag _, c) = [c]
letterWithGaps _ (Start _, c) = [c]

data Cell = Up Int | Across Int | Diag Int | Start Int deriving (Show, Eq, Ord)

data Grid = Grid (Array Int (Array Int Cell)) deriving Show

aArray :: Array Int Cell
aArray = listArray (0,4) [Start 1, Up 1, Up 1, Up 1, Up 1]
badGrid :: Grid
badGrid = Grid $ (listArray (0,2) $ repeat aArray)

goodGrid :: Grid
goodGrid = Grid $ listArray (0,3) $ [listArray (0,3) [Start 0, Up 1, Up 2, Up 3],
                                     listArray (0,3) [Across 1, Across 2, Up 3, Up 4],
                                     listArray (0,3) [Across 2, Diag 2, Diag 3, Diag 4],
                                     listArray (0,3) [Across 3, Diag 2, Diag 3, Diag 4]
                                    ]

gridBounds :: Grid -> (Int, Int)
gridBounds (Grid array0) = (snd $ bounds $ array0,
                              snd $ bounds $ (array0 ! 0))


path :: Grid -> [Cell]
path g = path' g x y []
    where
        (x, y) = gridBounds g

-- this will runtime exception on a malformed Grid
path' :: Grid -> Int -> Int -> [Cell] -> [Cell]
path' g' x y accumList = case cell of
                        Up _     -> path' g' x (y-1) (cell:accumList)
                        Across _ -> path' g' (x-1) y (cell:accumList)
                        Diag _   -> path' g' (x-1) (y-1) (cell:accumList)
                        Start _  -> cell:accumList
                     where
                        cell = lookUp g' x y

lookUp :: Grid -> Int -> Int -> Cell
lookUp (Grid a) n1 n2 =  a ! n1 ! n2

scoreOf :: Cell -> Int
scoreOf (Diag n) = n
scoreOf (Across n) = n
scoreOf (Up n) = n
scoreOf (Start n) = n

diagTo :: Cell -> Cell
diagTo cell = Diag (scoreOf cell)

acrossTo :: Cell -> Cell
acrossTo cell = Across (scoreOf cell)

upTo :: Cell -> Cell
upTo cell = Up (scoreOf cell)

myGrid :: Seq -> Seq -> Grid
myGrid s1 s2 = Grid $ listArray (0, (BS.length s1) - 1) $ map arrayForIdx [0..]
    where
        arrayForIdx :: Int -> Array Int Cell
        arrayForIdx 0   = listArray (0, (BS.length s2) - 1) $ Start (scoreAt s1 0 s2 0) : [Up (scoreAt s1 0 s2 idx2) -* gapPenalty | idx2 <- [1..]]
        arrayForIdx idx = listArray (0, (BS.length s2) - 1) $ map bestScore [0..]
            where
                bestScore :: Int -> Cell
                bestScore 0 = Across (scoreAt s1 idx s2 0) -* gapPenalty
                bestScore idx2 | (up idx2) >= (across idx2) && (up idx2) >= (diag idx2)     = up idx2
                               | (across idx2) >= (up idx2) && (across idx2) >= (diag idx2) = across idx2
                               | otherwise                                                  = diag idx2

                diag idx2   = diagTo (lookUp (myGrid s1 s2) (idx-1) (idx2-1)) +* (scoreAt s1 idx s2 idx2)
                across idx2 = acrossTo (lookUp (myGrid s1 s2) (idx-1) idx2)   +* (scoreAt s1 idx s2 idx2)
                up idx2     = upTo (lookUp (myGrid s1 s2) idx (idx2-1))       +* (scoreAt s1 idx s2 idx2)

scoreAt :: Seq -> Int -> Seq -> Int -> Int
scoreAt s1 i1 s2 i2 = similarityScore (BS.index s1 i1) (BS.index s2 i2)

similarityScore :: Char -> Char -> Int
similarityScore c1 c2 | c1 == c2  = 2
                      | otherwise = -1

(-*) :: Cell -> Int -> Cell
(-*) (Across n) n' = Across (n - n')
(-*) (Up n) n' = Up (n - n')
(-*) (Diag n) n' = Diag (n - n')
(-*) (Start n) n' = Start (n - n')

(+*) :: Cell -> Int -> Cell
(+*) (Diag n) n' = Diag (n + n')
(+*) (Across n) n' = Across (n + n')
(+*) (Up n) n' = Up (n + n')
(+*) (Start n) n' = Start (n + n')

gapPenalty :: Int
gapPenalty = 3

