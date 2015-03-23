{-# LANGUAGE OverloadedStrings #-}

module BioCalc where

import Web.Scotty

import Data.Monoid (mconcat)

import qualified Data.Text.Lazy as T
import Control.Monad

import AlignPar as AP
import Align as A
import AlnHtml as AH
import Translate as Tr

-- xxxx = 1 + "2"

concatenateRoute :: ScottyM ()
concatenateRoute = get "/concatenate/:string1/:string2/" concatAction
needleRoute :: ScottyM ()
needleRoute = get "/needle/:string1/:string2/" needleAction
needleParRoute :: ScottyM ()
needleParRoute = get "/needlepar/:string1/:string2/" needleActionPar
gridRoute :: ScottyM ()
gridRoute = get "/grid/:string1/:string2/" gridAction
gridParRoute :: ScottyM ()
gridParRoute = get "/gridpar/:string1/:string2/" gridParAction
sliceRoute :: ScottyM ()
sliceRoute = get "/slice/:string1/:begin/:end/" sliceAction               
             
orfsRoute :: ScottyM ()
orfsRoute = get "/orfs/:string1/" orfsAction

          

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

gridParAction :: ActionM ()
gridParAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    html $ AH.prettyGrid $ AP.grid string1  string2
    setHeader "Content-Type" "text/html; charset=UTF-8"
              
orfsAction :: ActionM ()
orfsAction = do
    string1 <- param "string1"
    text $ T.pack $ show $ Tr.orfs string1

              
-- 1-based index, inclusive end
sliceAction :: ActionM ()
sliceAction = do
    string <- param "string1"
    begin <- liftM read $ param "begin"
    end <- liftM read $ param "end"
    let result = T.take (end - begin + 1) $ T.drop (begin - 1) string
    text $ result
 
    

--needleActionBS :: ActionM ()
--needleActionBS :: ActionM ()
--needleActionBS = do
--    string1 <- param "string1"
--    string2 <- param "string2"
--    text $ T.pack $ show $ AlignBS.align  string1  string2



