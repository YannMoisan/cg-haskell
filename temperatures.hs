import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    input_line <- getLine
    let input = words input_line

    let temps = forM [0..(n-1)] $ (\i -> do
        let t = read (input!!(i)) :: Int -- a temperature expressed as an integer ranging from -273 to 5526
        return (t)
        ) :: IO [Int]

    -- hPutStrLn stderr "Debug messages..."

    -- Write answer to stdout
    temps >>= putStrLn.show.closestToZero
    return ()

closestToZero :: [Int] -> Int
closestToZero [] = 0
closestToZero xs = foldr1 (\a b -> if (abs a == abs b) then max a b else if (abs a < abs b) then a else b) xs