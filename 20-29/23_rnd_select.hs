import System.IO
import System.Random

-- the helper function is necessary to perform tail recursion of this monadic function
rnd_select_r :: [a] -> Int -> [a] -> IO [a]
rnd_select_r _ 0 acc = return (reverse acc)
rnd_select_r [] _ acc = return (reverse acc)
rnd_select_r x n acc = do
    index <- randomRIO (1, length x)
    let
        item = last $ take index x
        newList = (init $ take index x) ++ (drop index x)
    rnd_select_r newList (n - 1) (item:acc)

rnd_select :: [a] -> Int -> IO [a]
rnd_select x n = rnd_select_r x n []
