{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Data.Monoid (mconcat)

import Control.Monad
import Data.Array
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Char8 as BS






main = scotty 3000 $ do
    concatenateRoute
    needleRoute



concatenateRoute :: ScottyM ()
concatenateRoute = get "/concatenate/:string1/:string2/" concatAction
needleRoute = get "/needle/:string1/:string2/" needleAction


concatAction :: ActionM ()
concatAction = do
    string1 <- param "string1"
    string2 <- param "string2"
    text $ mconcat [string1, string2]

needleAction :: ActionM ()
needleAction = do
    string1 <- liftM BS.unpack $ param "string1"
    string2 <- liftM BS.unpack $ param "string2"
    text $ T.pack $ show $ align  string1  string2


align :: String -> String -> [String]
align da db = format $ reverse $ traceback lena lenb
    where
        lena = length da
        lenb = length db
        a = ' ' : da
        b = ' ' : db
        memscore = listArray ((0,0), (lena, lenb))
                [score x y | x <- [0..lena], y <- [0..lenb]]
        infix 5 @@
        (@@) i j = memscore !(i,j)
        score 0 _ = 0
        score _ 0 = 0
        score x y = maximum [(x-1 @@ y - 1)  + difference x y,
                      x-1  @@ y,
                      x    @@ y-1]
                        where difference x y | a !! x == b !! y = 1 --matches
                                                    | otherwise        = 0 --doesn't match
        traceback :: Int -> Int -> [(Char, Char)]
        traceback 0 0 = []
        traceback x y | x == 0               = (' '    , b !! y):traceback 0     (y-1)
                          | y == 0               = (a !! x ,  ' '  ):traceback (x-1) 0
                          | x @@ y == x @@ y-1   = (' ', b !! y)    :traceback x     (y-1)
                          | x @@ y == x-1 @@ y   = (a !! x, ' ')    :traceback (x-1) y
                          | otherwise            = (a !! x, b !! y) :traceback (x-1) (y-1)
        format l = [map fst l, map snd l]
