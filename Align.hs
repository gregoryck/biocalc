
module Align where
import qualified Data.ByteString.Char8 as BS

import Data.Array

tobs :: String -> BS.ByteString
tobs = BS.pack

space :: BS.ByteString
space = tobs " "

(@!) :: BS.ByteString -> Int -> Char
(@!) = BS.index

format :: [(a, a)] -> [[a]]
format l = [map fst l, map snd l]

align :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
align da db = map BS.pack $ format $ reverse $ traceback lena lenb
    where
        lena = BS.length da
        lenb = BS.length db
        a = BS.append space da
        b = BS.append space db

        memscore :: Array (Int, Int) Int
        memscore = listArray ((0,0), (lena, lenb))
                             [score x y | x <- [0..lena], y <- [0..lenb]]
        infix 5 @@
        (@@) i j = memscore !(i,j)

        score :: Int -> Int -> Int
        score 0 _ = 0
        score _ 0 = 0
        score x y = maximum [(x-1 @@ y - 1)  + difference x y,
                             x-1  @@ y,
                             x    @@ y-1]
                        where difference x' y' | BS.index a x' == BS.index b y' = 1 --matches
                                             | otherwise                    = 0 --doesn't match

        traceback :: Int -> Int -> [(Char, Char)]
        traceback 0 0 = []
        traceback x y     | x == 0               = (' '    , b @! y):traceback 0     (y-1)
                          | y == 0               = (a @! x ,  ' '  ):traceback (x-1) 0
                          | x @@ y == x @@ y-1   = (' ', b @! y)    :traceback x     (y-1)
                          | x @@ y == x-1 @@ y   = (a @! x, ' ')    :traceback (x-1) y
                          | otherwise            = (a @! x, b @! y) :traceback (x-1) (y-1)
