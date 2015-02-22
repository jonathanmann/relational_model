import System.Environment
import Data.List.Split
import Data.String.Utils

readData :: FilePath -> IO (String)
readData file = do
    content <- readFile file
    return content

processData :: String -> String
processData content = cleanData (unlines([ processLine line 1 | line <- lines(content)]))

cleanData :: String -> String
cleanData content = replace "\r" "" content

processLine :: String -> Int -> String
processLine line r_ord =  unwords [ (getTuple col r_ord 1) | col <- (splitOn "," line)]

getTuple :: String -> Int -> Int -> String
getTuple col r_ord c_ord  = "(" ++ (show r_ord) ++ "," ++ (show c_ord) ++ "," ++ (col) ++ ")"

outputData :: String -> IO ()
outputData contents = writeFile "relations.log" contents

main :: IO ()
main = do
    file:_ <- getArgs
    content <- readData file
    outputData $ processData content
    --putStrLn "check relations.log"
