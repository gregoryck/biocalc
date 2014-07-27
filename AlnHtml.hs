{-# LANGUAGE OverloadedStrings #-}

module AlnHtml where

import qualified AlignPar as AP
-- import qualified Align as A

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_)
import Data.Array
import qualified Data.Text.Lazy as LazyT



huh :: String
huh = "test"


prettyGrid :: AP.Grid -> LazyT.Text 
prettyGrid = renderHtml . gridToHtml

gridToHtml :: AP.Grid -> Html
gridToHtml g = H.table $ forM_ (rows g) (H.tr . rowToHtml)

rowToHtml :: [AP.Cell] -> Html
rowToHtml a = forM_  a (H.td . cellToHtml)

cellToHtml :: AP.Cell -> Html
cellToHtml (AP.Across n) = toHtml $ ("↑" ++ (show n))
cellToHtml (AP.Up n) = toHtml $ ("←" ++ (show n))
cellToHtml (AP.Diag n) = toHtml $ ("↖" ++ (show n))
cellToHtml (AP.Start n) = toHtml $ ("↪" ++ (show n))

cells :: Array Int AP.Cell -> [AP.Cell]
cells = elems


splitBy :: Int -> [a] -> [[a]]
splitBy n xs = [front] ++ (splitBy n back) 
                 where (front, back) = splitAt n xs

rows :: AP.Grid -> [[AP.Cell]]
rows (AP.Grid arr) = splitEvery rowLength $ elems arr 
             where
               rowLength = (+1) $ snd $ snd $ bounds arr
               
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery howMany lst = [take howMany lst] ++ 
                         (splitEvery howMany $ drop howMany lst) 
