import System.IO
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as M

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int -- Number of elements which make up the association table.
    input_line <- getLine
    let q = read input_line :: Int -- Number Q of file names to be analyzed.

    let types = replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let ext = input!!0 -- file extension
        let ext2 = map toLower ext -- file extension
        let mt = input!!1 -- MIME type.
        return ((ext2, mt))

    let names = replicateM q $ do
        fname <- getLine
        -- One file name per line.
        return (fname)

    t <- types
    let m = M.fromList t

    let tt = fmap (\xs -> map (\x -> lookupDefaultMaybe (ext x) "UNKNOWN" m) xs) names
    tt >>= putStrLn.unlines

    return ()

ext :: String -> Maybe String
ext s = case (elemIndices '.' s) of [] -> Nothing
                                    xs -> Just $ drop ((last xs) + 1) s

lookupDefault :: String -> String -> M.Map String String -> String
lookupDefault k def m = case (M.lookup k m) of Nothing -> def
                                               Just s -> s

lookupDefaultMaybe :: Maybe String -> String -> M.Map String String -> String
lookupDefaultMaybe Nothing def m = "UNKNOWN"
lookupDefaultMaybe (Just k) def m = lookupDefault (map toLower k) def m
