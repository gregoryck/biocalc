{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}


import Test.QuickCheck
import AlignPar as AP
-- import AlnHtml
--import Align as A
import Data.Array
-- import Control.Monad
--import Data.Char

import Data.ByteString.Char8 as BS hiding (map, concat, zip, null)
-- import Debug.Trace

arbitraryNonEmpty :: Gen String
arbitraryNonEmpty = arbitrary `suchThat` (not . null)



--TODO instance arbitrary Seq just AGCT ???
instance Arbitrary BS.ByteString where
    arbitrary :: Gen Seq
    arbitrary = fmap AP.tobs arbitraryNonEmpty

instance Arbitrary Grid where
    arbitrary = do
        seq1 <- arbitrary
        seq2 <- arbitrary
        --return $ grid (trace "Seq1: " seq1) (trace "Seq2: " seq2)
        return $ grid seq1 seq2


equalsItself :: Seq -> Bool
equalsItself s =  s == s

equalThemselves :: [Seq] -> Bool
equalThemselves xs = and $ map equalsItself xs

emptyOrFirstIsStart :: Grid -> Bool
emptyOrFirstIsStart g = or [isEmpty g, isStart $ lookUp g 0 0]

isEmpty :: Grid -> Bool
isEmpty (Grid _ _ array0) = or [(bounds array0) == (0,-1),
                            (bounds (array0 ! 0)) == (0,-1)]

isStart :: Cell -> Bool
isStart (Start _) = True
isStart _ = False

startIsStart :: Seq -> Seq -> Bool
startIsStart s1 s2 = emptyOrFirstIsStart $ grid s1 s2

intslessThan10 :: Gen Int
intslessThan10 = suchThat arbitrary (<10)

lessThan10 :: Int -> Bool
lessThan10 = (<10)

triviallyTrue :: Property -- Property is Gen Prop
                          -- Prop is not used anywhere else
                          -- but is an instance of Testable

triviallyTrue = forAll intslessThan10 lessThan10
--              forAll (arbitrary `suchThat` (<10)) lessThan10

pointsToBest :: Grid -> Int -> Int -> Bool
pointsToBest g x y = case lookUp g x y of
                        Diag _   -> and [diagScore > upScore,
                                         diagScore > acrossScore]
                        Across _ -> and [acrossScore > upScore,
                                         acrossScore > diagScore]
                        Up _     -> and [upScore > diagScore,
                                         upScore > acrossScore]
                        Start _  -> and [x == 0, y == 0]
                     where
                        diagScore = scoreOf $ lookUp g (x - 1) (y - 1)
                        upScore = scoreOf $ lookUp g x (y - 1)
                        acrossScore = scoreOf $ lookUp g (x - 1) y

inRangeOfGrid :: Grid -> ((Int, Int) -> Int) -> Int -> Bool
inRangeOfGrid g sel x = and [x >= (sel $ gridBounds g),
                             x <= (sel $ gridBounds g)]

inXRangeOfGrid :: Grid -> Int -> Bool
inXRangeOfGrid g = inRangeOfGrid g fst

inYRangeOfGrid :: Grid -> Int -> Bool
inYRangeOfGrid g = inRangeOfGrid g snd


-- this is an ugly, heavyweight thing
-- including a grid, arbitrary coordinates, and
-- the arbitrary sequences that produce the grid.
-- Having the sequences handy is nice for visualizing test failures.
data GridAndCoords = GridAndCoords Grid Int Int Seq Seq
    deriving Show
instance Arbitrary GridAndCoords where
    arbitrary = do
                -- g <- arbitrary
                s1 <- arbitrary
                s2 <- arbitrary
                g <- return $ grid s1 s2
                x <- choose (0, xMax g - 1)
                y <- choose (0, yMax g - 1)
                return $ GridAndCoords g x y s1 s2
    
    -- arbitrary = arbitrary >>= \s1 ->
    --             arbitrary >>= \s2 ->
    --             liftM2 grid s1 s2 >>= \g ->
    --             liftM3 GridAndCoords 


trivial2 :: GridAndCoords -> Bool
trivial2 (GridAndCoords _g _x _y _s1 _s2) = True

genWorks :: GridAndCoords -> Bool
genWorks (GridAndCoords g x y _s1 _s2) = and [inXRangeOfGrid g x,
                                            inYRangeOfGrid g y]

genWorksDetail :: Seq -> Seq -> Bool
genWorksDetail s1 s2 = and [inXRangeOfGrid g x,
                      inYRangeOfGrid g y]
                 where
                   g = grid s1 s2
                   x = 0 -- hmm
                   y = 0


                                 


main :: IO ()
main = do
    quickCheck equalsItself
    quickCheck equalThemselves
    quickCheck startIsStart
    quickCheck triviallyTrue
    quickCheck pointsToBest
    quickCheck trivial2
    quickCheck genWorks
