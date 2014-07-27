
module AlignPar where
import Data.ByteString.Char8 as BS hiding (map, concat, zip, maximum)

--import Control.Monad.Par.Scheds.Trace as P
import Data.Array
import Debug.Trace (trace)
import Text.Printf

tobs :: String -> BS.ByteString
tobs = BS.pack

toseq :: String -> Seq       
toseq = tobs

space :: BS.ByteString
space = tobs " "

type Seq = ByteString
data Aln = Aln ByteString ByteString deriving Show

data SeqPosition = Top | Side

align :: Seq -> Seq -> Aln
align s1 s2 = Aln s1_gaps s2_gaps
    where
        s1_gaps = addGaps Side g s1
        s2_gaps = addGaps Top g s2
        g = grid s1 s2

addGaps :: SeqPosition -> Grid -> Seq -> Seq
addGaps pos g s = BS.pack $ concat $ [letterWithGaps pos x | x <- zip (path g) (BS.unpack s)]

letterWithGaps :: SeqPosition -> (Cell, Char) -> [Char]
letterWithGaps Top (Across _, c) = [' ', c]
letterWithGaps Top (Up _, c) = [c]
letterWithGaps Side (Across _, c) = [c]
letterWithGaps Side (Up _, c) = [' ', c]
letterWithGaps _ (Diag _, c) = [c]
letterWithGaps _ (Start _, c) = [c]

data Cell = Up Int | Across Int | Diag Int | Start Int deriving (Show, Eq)

instance Ord Cell where
    compare c1 c2 = compare (scoreOf c1) (scoreOf c2)

data Grid = Grid (Array (Int, Int) Cell) deriving Show

arrOfGrid (Grid arr) = arr

theArray :: Array (Int, Int) Cell
theArray = listArray ((0,0), (3,3)) [Start 0, Up 1, Up 2, Up 3, Across 1, Across 2, Up 3, Up 4, Across 2, Diag 2, Diag 3, Diag 4, Across 3, Diag 2, Diag 3, Diag 4]
goodGrid :: Grid
goodGrid = Grid theArray


gridBounds :: Grid -> (Int, Int)
--gridBounds (Grid array0) = maximum $ indices array0
gridBounds (Grid arr) = snd $ bounds arr

xMax :: Grid -> Int
xMax = fst . gridBounds

yMax :: Grid -> Int
yMax = snd . gridBounds

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
lookUp (Grid arr) x y = arr ! (x,y)

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

coordRange :: Int -> Int -> [(Int, Int)]
coordRange x y = [(x', y') | x' <- [0..x], y' <- [0..y]]

grid :: Seq -> Seq -> Grid
grid s1 s2 = newGrid 
               where
                 newGrid = Grid newArray
                 newArray = listArray ((0,0), (maxX, maxY)) $ map cellAtIdx (coordRange maxX maxY)
                 maxX = (BS.length s1) - 1
                 maxY = (BS.length s2) - 1
                 cellAtIdx (x,y) = bestScore newGrid s1 s2 x y
                 -- cellAtIdx (x,y) = Start 1
                        

bestScore :: Grid -> Seq -> Seq -> Int -> Int -> Cell
bestScore _g s1 s2 0 0 = Start (scoreAt s1 0 s2 0)
bestScore _g s1 s2 0 idx = Up (scoreAt s1 0 s2 idx) -* gapPenalty
bestScore _g s1 s2 idx 0 = Across (scoreAt s1 idx s2 0) -* gapPenalty
bestScore g s1 s2 idx1 idx2 | (up idx2) >= (across idx2) && 
                              (up idx2) >= (diag idx2)     = up idx2
                            | (across idx2) >= (up idx2) && 
                              (across idx2) >= (diag idx2) = across idx2
                            | otherwise                    = diag idx2
            where
                diag idx2'   = diagTo (lookUp g (idx1-1) (idx2'-1)) +* (scoreAt s1 idx1 s2 idx2')
                across idx2' = acrossTo (lookUp g (idx1-1) idx2')   +* (scoreAt s1 idx1 s2 idx2') -* gapPenalty
                up idx2'     = upTo (lookUp g idx1 (idx2'-1))       +* (scoreAt s1 idx1 s2 idx2') -* gapPenalty


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
