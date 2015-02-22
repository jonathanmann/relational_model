--cabal install split
--cabal install uuid

--used
import System.Environment
import Data.List.Split
import Data.String.Utils

--not used
import Data.List
import Data.Maybe
import Data.UUID as U
import qualified Data.UUID.V4 as U4
import Control.Monad

readData :: FilePath -> IO (String)
readData file = do
    content <- readFile file
    return content

processData :: String -> String
processData content = cleanData (unlines([ processLine(line) | line <- lines(content)]))

cleanData :: String -> String
cleanData content = replace "\r" "" content

--U.toString U4.nextRandom
processLine :: String -> String
processLine line =  unwords [ (getTuple col 3 1) | col <- (splitOn "," line)]
--processLine line =  unwords [[ "(" ++ ("1") ++ "," ++ (col) ++ ")" | col <- (splitOn "," line)]!!1] 

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
