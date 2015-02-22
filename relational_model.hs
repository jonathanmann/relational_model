import System.Environment
import Data.List.Split
import Data.String.Utils

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
processData content = cleanData (unlines([ prepLine line (head (lines(content))) | line <- (zip[0..] (lines(content)) )]))

prepLine :: (Int,String) -> String -> String
prepLine line headers = processLine (snd line) (fst line) (spl headers)

processLine :: String -> Int -> [String] -> String
processLine line r_ord headers =  unwords [ (getTuple (snd col) r_ord (fst col)) | col <- (zip headers (spl line))]

getTuple :: String -> Int -> String -> String
getTuple col r_ord c_name  = "(" ++ (c_name) ++ "," ++ (show r_ord) ++ "," ++ (col) ++ ")"

main :: IO ()
main = do
    file:_ <- getArgs
    content <- readData file
    outputData $ processData content
    --putStrLn "check relations.log"
