{-# OPTIONS_GHC -Wall -XInstanceSigs #-}


import Test.QuickCheck
import AlignPar as AP
--import Align as A
import Data.Array
import Control.Monad
--import Data.Char

import Data.ByteString.Char8 as BS hiding (map, concat, zip)

import Debug.Trace

instance Arbitrary BS.ByteString where
    arbitrary :: Gen Seq
    arbitrary = fmap AP.tobs arbitrary

instance Arbitrary Grid where
    arbitrary = liftM2 grid arbitrary arbitrary

equalsItself :: Seq -> Bool
equalsItself = (\s -> s == s)

equalThemselves :: [Seq] -> Bool
equalThemselves xs = and $ map equalsItself xs

emptyOrFirstIsStart :: Grid -> Bool
emptyOrFirstIsStart g = or [isEmpty g, isStart $ lookUp g 0 0]

isEmpty :: Grid -> Bool
isEmpty (Grid array0) = or [(bounds array0) == (0,-1),
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

triviallyTrue :: Property
triviallyTrue = forAll intslessThan10 lessThan10

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

--gridPointsToBest :: Seq -> Seq -> Int -> Int -> Bool
--gridPointsToBest s1 s2 =



main :: IO ()
main = do
    quickCheck equalsItself
    quickCheck equalThemselves
    quickCheck startIsStart
    quickCheck triviallyTrue
    quickCheck pointsToBest
