import System.IO
import Control.Monad
import Data.Char
import Data.List

-- https://www.codingame.com/ide/puzzle/ascii-art
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    t <- getLine

    alpha <- replicateM h $ do
        row <- getLine
        return (row)

    let letters = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ?" (parse l h alpha)
    let messageLetters = traverse (\c -> lookup c letters) (map dd t)
    let lines = foldr1 merge (getOrElse messageLetters [])

    -- hPutStrLn stderr "Debug messages..."

    traverse putStrLn lines
    return ()

type Letter = [String]

grouped :: Int -> [a] -> [[a]]
grouped i xs = reverse $ go i xs [] where
    go i [] acc = acc
    go i xs acc = go i b (a : acc) where (a, b) = splitAt i xs

merge :: Letter -> Letter -> Letter
merge a b = map (\(c, d) -> c ++ d) $ zip a b

parse :: Int -> Int -> Letter -> [Letter]
parse w h l = transpose $ map (grouped w) l

getOrElse :: Maybe a -> a -> a
getOrElse Nothing def = def
getOrElse (Just a) def = a

dd :: Char -> Char
dd c = if (elem (toUpper c) "ABCDEFGHIJKLMNOPQRSTUVWXYZ?") then (toUpper c) else '?'