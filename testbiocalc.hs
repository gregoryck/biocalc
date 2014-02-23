{-# OPTIONS_GHC -Wall -XInstanceSigs #-}


import Test.QuickCheck
import AlignPar as AP
import Align as A
import Data.Array
--import Data.Char

import Data.ByteString.Char8 as BS hiding (map, concat, zip)


instance Arbitrary BS.ByteString where
    arbitrary :: Gen Seq
    arbitrary = fmap AP.tobs arbitrary


equalsItself :: Seq -> Bool
equalsItself = (\s -> s == s)


equalThemselves :: [Seq] -> Bool
equalThemselves xs = and $ map equalsItself xs


emptyOrFirstIsStart g = or [isEmpty g, isStart $ lookUp g 0 0]

isEmpty (Grid array0) = or [(bounds array0) == (0,-1),
                            (bounds (array0 ! 0)) == (0,-1)]

isStart (Start _) = True
isStart _ = False

startIsStart :: Seq -> Seq -> Bool
startIsStart s1 s2 = emptyOrFirstIsStart $ grid s1 s2


main :: IO ()
main = do
    quickCheck equalsItself
    quickCheck equalThemselves
    quickCheck startIsStart
