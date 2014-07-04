{-# LANGUAGE OverloadedStrings #-}

module BioCalc where

import Web.Scotty

import Data.Monoid (mconcat)

import qualified Data.Text.Lazy as T
--import Control.Monad

import AlignPar as AP
import Align as A
import AlnHtml as AH

--xxx = 1 + "2"

concatenateRoute :: ScottyM ()
concatenateRoute = get "/concatenate/:string1/:string2/" concatAction
needleRoute :: ScottyM ()
needleRoute = get "/needle/:string1/:string2/" needleAction
needleParRoute :: ScottyM ()
needleParRoute = get "/needlepar/:string1/:string2/" needleActionPar
gridRoute :: ScottyM ()
gridRoute = get "/grid/:string1/:string2/" gridAction

concatAction :: ActionM ()
concatAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ mconcat [string1, string2]

needleAction :: ActionM ()
needleAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ T.pack $ show $ A.align  string1  string2

needleActionPar :: ActionM ()
needleActionPar = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ T.pack $ show $ AP.align  string1  string2

gridAction :: ActionM ()
gridAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    html $ AH.prettyGrid $ AP.grid string1  string2
    setHeader "Content-Type" "text/html; charset=UTF-8"

--needleActionBS :: ActionM ()
--needleActionBS :: ActionM ()
--needleActionBS = do
--    string1 <- param "string1"
--    string2 <- param "string2"
--    text $ T.pack $ show $ AlignBS.align  string1  string2



