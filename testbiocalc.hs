{-# OPTIONS_GHC -Wall -XInstanceSigs #-}


import Test.QuickCheck
import AlignPar as AP
import Align as A
--import Data.Char

import Data.ByteString.Char8 as BS hiding (map, concat, zip)


instance Arbitrary BS.ByteString where
    arbitrary :: Gen Seq
    arbitrary = fmap AP.tobs arbitrary


equalsItself :: Seq -> Bool
equalsItself = (\s -> s == s)


equalThemselves :: [Seq] -> Bool
equalThemselves xs = and $ map equalsItself xs

main :: IO ()
main = do
    quickCheck equalsItself
    quickCheck equalThemselves
