{-# LANGUAGE OverloadedStrings #-}

module AlnHtml where

import AlignPar
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (toHtml, Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_)
import Data.Array
import qualified Data.Text.Lazy as LazyT



huh :: String
huh = "test"


prettyGrid :: Grid -> LazyT.Text 
prettyGrid = renderHtml . gridToHtml

gridToHtml :: Grid -> Html
gridToHtml g = H.table $ forM_ (rows g) (H.tr . rowToHtml)

rowToHtml :: Array Int Cell -> Html
rowToHtml a = forM_ (cells a) (H.td . cellToHtml)

cellToHtml :: Cell -> Html
cellToHtml (Across n) = toHtml $ ("↑" ++ (show n))
cellToHtml (Up n) = toHtml $ ("←" ++ (show n))
cellToHtml (Diag n) = toHtml $ ("↖" ++ (show n))
cellToHtml (Start n) = toHtml $ ("↪" ++ (show n))

cells :: Array Int Cell -> [Cell]
cells = elems

rows :: Grid -> [Array Int Cell]
rows (Grid _x _y array0) = elems array0
