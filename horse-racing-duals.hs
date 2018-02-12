import System.IO
import Control.Monad
import Data.List

-- https://www.codingame.com/ide/puzzle/horse-racing-duals
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int

    let strengths = replicateM n $ do
        input_line <- getLine
        let pi = read input_line :: Int
        return (pi)

    strengths >>= print.diffBetweenClosest
    return ()

diffBetweenClosest :: [Int] -> Int
diffBetweenClosest xs = foldr (\(a, b) acc -> min (b - a) acc) 10000 (windowed xs)
    where
      windowed xs = zip (sort xs) (tail (sort xs))
