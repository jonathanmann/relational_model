import System.Environment
import Data.List.Split
import Data.String.Utils
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashSet

readData :: FilePath -> IO (String)
readData file = do
    content <- readFile file
    return content

outputData :: String -> IO ()
outputData contents = writeFile "relations.log" contents

spl :: String -> [String]
spl x = splitOn "," x

getSet :: String -> String
getSet raw_data = show [Data.HashSet.fromList (data_list) | data_list <- (processData raw_data)]

processData :: String -> [[(Int,String)]]
processData content =  filterData (transpose [(prepLine line) | line <- (zip[0..] (lines(cleanData content)) )])

filterData :: [[(Int,String)]] -> [[(Int,String)]]
filterData tuple_lists = [ Data.List.filter (\(x,y) ->  y /= "") tuple_list  | tuple_list <- tuple_lists] 

cleanData :: String -> String
cleanData content = replace "\r" "" content

prepLine :: (Int,String) -> [(Int,String)]
prepLine line = processLine (snd line) (fst line)

processLine :: String -> Int -> [(Int,String)]
processLine line r_ord = [ (getTuple col r_ord) | col <- (spl line)]

getTuple :: String -> Int -> (Int,String) 
getTuple col r_ord = (r_ord,col)


main :: IO ()
main = do
    file:_ <- getArgs
    raw_data <- readData file
    outputData $ (getSet raw_data) 
