import System.Environment
import Data.List.Split
import Data.String.Utils
import Data.List

readData :: FilePath -> IO (String)
readData file = do
    content <- readFile file
    return content

outputData :: String -> IO ()
outputData contents = writeFile "relations.log" contents

spl :: String -> [String]
spl x = splitOn "," x

cleanData :: String -> String
cleanData content = replace "\r" "" content

processData :: String -> String
processData content = unlines([ show (prepLine line) | line <- (zip[0..] (lines(content)) )])

prepLine :: (Int,String) -> [(Int,String)]
prepLine line = processLine (snd line) (fst line)

processLine :: String -> Int -> [(Int,String)]
processLine line r_ord = [ (getTuple col r_ord) | col <- (spl line)]

getTuple :: String -> Int -> (Int,String) 
getTuple col r_ord = (r_ord,col)

main :: IO ()
main = do
    file:_ <- getArgs
    content <- readData file
    outputData $ processData (cleanData content)
