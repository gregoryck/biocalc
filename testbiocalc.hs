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

data GridAndCoords = GridAndCoords Grid Int Int
    deriving Show
instance Arbitrary GridAndCoords where
    arbitrary = do
                g <- arbitrary
                x <- choose (0, xMax g - 1)
                y <- choose (0, yMax g - 1)
                return $ GridAndCoords g x y

trivial2 :: GridAndCoords -> Bool
trivial2 (GridAndCoords _ _x _y) = True

genWorks :: GridAndCoords -> Bool
genWorks (GridAndCoords g x y) = and [inXRangeOfGrid g x,
                                      inYRangeOfGrid g y]

main :: IO ()
main = do
    quickCheck equalsItself
    quickCheck equalThemselves
    quickCheck startIsStart
    quickCheck triviallyTrue
    --quickCheck pointsToBest
    quickCheck trivial2
    quickCheck genWorks
