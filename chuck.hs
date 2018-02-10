import System.IO
import Control.Monad
import Data.List

-- https://www.codingame.com/ide/puzzle/chuck-norris
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    message <- getLine
    let binaries = concat $ fmap (toBinary.fromEnum) message
    putStrLn . mkString $ concatMap encode (group binaries)
    return ()
    
toBinary :: Int -> [Int]
toBinary = go 7 [] where
    go 0 acc x = acc
    go n acc x = go (n-1) (bit:acc) x' where
         (x', bit) = x `divMod` 2
         
encode :: [Int] -> [String]
encode xs = [(if (head xs == 1) then "0" else "00"), (replicate (length xs) '0')]

mkString :: [String] -> String
mkString = tail . concatMap (\s -> " " ++ s)
