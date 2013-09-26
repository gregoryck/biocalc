{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)

main = scotty 3000 $ do
    concatenateAction



concatenateAction :: ScottyM ()
concatenateAction = get "/concatenate/:string1/:string2/" concatThem



concatThem :: ActionM ()
concatThem = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ mconcat [string1, string2]
