import System.IO
import System.Random

-- function from problem#23
rnd_select_r :: [a] -> Int -> [a] -> IO [a]
rnd_select_r _ 0 acc = return (reverse acc)
rnd_select_r [] _ acc = return (reverse acc)
rnd_select_r x n acc = do
    index <- randomRIO (1, length x)
    let
        item = last $ take index x
        newList = (init $ take index x) ++ (drop index x)
    rnd_select_r newList (n - 1) (item:acc)

diff_select :: Int -> Int -> IO [Int]
diff_select n limit = rnd_select_r [1 .. limit] n []
