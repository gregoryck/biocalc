{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}


import Test.QuickCheck
import AlignPar as AP
-- import AlnHtml
--import Align as A
-- import Data.Array
import Control.Monad
--import Data.Char

import qualified Data.ByteString.Char8 as BS 
-- import Debug.Trace

arbitraryNonEmpty :: Gen String
arbitraryNonEmpty = arbitrary `suchThat` (not . null)

arbitraryNuc :: Gen Char
arbitraryNuc = 
  oneof $ map return ['A', 'G', 'C', 'T']

instance Arbitrary Seq where
    arbitrary = do
        length_ <- choose (1,100)
        str <- (liftM (take length_) $ infiniteListOf arbitraryNuc) :: Gen String
        return $ AP.tobs str
        -- return $ AP.tobs (trace ("length_ is " ++ (show length_) ++ "str " ++ str) str) 
               
arbitrarySeq :: Gen Seq
arbitrarySeq = do
        length_ <- choose (1,10)
        str <- (liftM (take length_) $ listOf arbitraryNuc) :: Gen String
        return $ AP.tobs str
        -- return $ AP.tobs (trace ("length_ is" ++ (show length_)) str) 

instance Arbitrary Grid where
    arbitrary = do
        seq1 <- arbitrarySeq
        seq2 <- arbitrarySeq
        --return $ grid (trace "Seq1: " seq1) (trace "Seq2: " seq2)
        return $ grid seq1 seq2

instance Arbitrary Cell where
    arbitrary = do
        n <- elements [1,2,3] :: Gen Integer
        let cell = case n of 1 -> Diag 
                             2 -> Across
                             3 -> Up
                             _ -> error "1-3 only plz"
        liftM cell arbitrary

equalsItself :: Seq -> Bool
equalsItself s =  s == s

noEmptySeqs :: Seq -> Bool
noEmptySeqs s = s /= BS.empty

equalThemselves :: [Seq] -> Bool
equalThemselves xs = and $ map equalsItself xs

emptyOrFirstIsStart :: Grid -> Bool
emptyOrFirstIsStart g = or [isEmpty g, isStart $ lookUp g 0 0]

isEmpty :: Grid -> Bool
isEmpty = undefined
-- isEmpty (Grid array0) = or [(bounds array0) == (0,-1),
--                             (bounds (array0 ! 0)) == (0,-1)]

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

pointsToBest :: GridAndCoords -> Bool
pointsToBest (GridAndCoords g x y _s1 _s2) = case lookUp g x y of
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
inRangeOfGrid g sel x = and [x >= 0,
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

derivingOrd :: Cell -> Cell -> Bool
derivingOrd c1 c2 = (c1 > c2) == (scoreOf c1 > scoreOf c2)
                                 


main :: IO ()
main = do
    print "I do not want to gen empty Seqs"
    quickCheck noEmptySeqs
    print "simple stuff"
    quickCheck equalsItself
    quickCheck equalThemselves
    quickCheck startIsStart
    quickCheck triviallyTrue
    print "arbitrary :: Gen GridAndCoords is sane" 
    quickCheck genWorks
    print "points to Best"
    quickCheck pointsToBest
    print "instance Ord Cell is correct"
    quickCheck derivingOrd
