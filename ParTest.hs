import Control.Monad.Par
import Data.IntMap as IM

import Data.Traversable
import Data.Array hiding ((!))


pr :: Key -> Int -> IO ()
pr a = print


theMap = fromList [(1, 100), (2, 200), (3, 300)]

printAtKey :: Key -> Int -> IO ()
printAtKey k times = print $ take times $ repeat $ theMap ! k

printAtKey' :: Key -> Int -> IO String
printAtKey' k times = case IM.lookup k theMap of Nothing  -> do
                                                    print "key not found"
                                                    return "key not found"
                                                 Just val -> do
                                                    print $ take times $ repeat val
                                                    return "printed"

-- traverseWithKey ::

doIt = traverseWithKey printAtKey theMap
doIt' = traverseWithKey printAtKey' theMap


-- what of list monad
listFromKey k times = take k $ repeat times



doList = traverseWithKey listFromKey theMap



xList = do
    y <- [1..6]
    z <- [1..9]
    return (y,z)


--rp = get $ runPar $ spawn $ return 1


x :: Par (IVar Int)
x = spawn $ return 1


x' = do
    xx <- spawn $ return (1 :: Int)
    y <- get xx
    return y
    --return $ get xx