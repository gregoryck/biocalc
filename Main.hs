{-# OPTIONS_GHC -Wall #-}

module Main where

import Web.Scotty
import BioCalc

main :: IO ()
main = scotty 4040 $ do
    concatenateRoute
    needleRoute
    needleParRoute
    gridRoute
    sliceRoute
    orfsRoute
