{-# LANGUAGE OverloadedStrings #-}

module BioCalc where

import Web.Scotty

import Data.Monoid (mconcat)

import qualified Data.Text.Lazy as T
--import Control.Monad

import Align

--xxx = 1 + "2"

concatenateRoute :: ScottyM ()
concatenateRoute = get "/concatenate/:string1/:string2/" concatAction
needleRoute :: ScottyM ()
needleRoute = get "/needle/:string1/:string2/" needleAction


concatAction :: ActionM ()
concatAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ mconcat [string1, string2]

needleAction :: ActionM ()
needleAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ T.pack $ show $ Align.align  string1  string2

--needleActionBS :: ActionM ()
--needleActionBS = do
--    string1 <- param "string1"
--    string2 <- param "string2"
--    text $ T.pack $ show $ AlignBS.align  string1  string2



