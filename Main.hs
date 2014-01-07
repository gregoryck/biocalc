{-# OPTIONS_GHC -Wall #-}

module Main where

import Web.Scotty
import BioCalc

main = scotty 3000 $ do
    concatenateRoute
    needleRoute
