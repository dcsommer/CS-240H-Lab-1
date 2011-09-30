import Data.Char
import Data.List
import Data.Map (Map, assocs, insertWith, empty, (!))
-- A Histogram data type keeps track of the number of occurances
-- for the most frequent word, the length of the longest word,
-- and a Map from words to frequencies
data Histogram = Histogram Integer Integer (Map String Integer)

histWidth = 60
wordMaxLen = 30
tickSpace = 8
tableWidth = wordMaxLen + 1 + histWidth
ioUnit = return ()

orderTuple :: (String, Integer) -> (String, Integer) -> Ordering
orderTuple (_, first) (_, second) = if first < second
                                    then GT else LT

putChars :: Integral a => Char -> a -> IO ()
putChars ch times | times > 0 = putChar ch >>
                                putChars ch (times - 1)
                  | otherwise = ioUnit

putCharsEnd :: Integral a => Char -> a -> Char -> IO ()
putCharsEnd ch times end = putChars ch times >> putChar end

printTick :: RealFrac a => a -> IO a -> String -> IO a
printTick stepSize x num = do
  remainder <- x
  let real = remainder + stepSize - fromIntegral (length num) in
    let toPrint = floor real in
      if toPrint >= 3
      then do
        putChars ' ' toPrint 
        putStr num
        return (real - fromIntegral toPrint)
      else
        return (real + fromIntegral (length num))

printLegend maxCount row1Width =
    let stepSize = fromIntegral histWidth / fromIntegral maxCount
    in do
      putCharsEnd ' ' (row1Width-1) '0'
      foldl (printTick stepSize) (return 0) (map show [1..maxCount])
      putStr "\n"

printHistogramEntries :: Histogram -> IO ()
printHistogramEntries (Histogram maxCount longest entries) = 
  let row1Width = 1 + min wordMaxLen longest in do
      putCharsEnd '=' tableWidth '\n'
      printLegend maxCount row1Width
      foldl (\x (key, count) ->
        let word = take (fromIntegral row1Width - 1) key in
          x >> putStr word >>
          putChars ' ' (row1Width - fromIntegral (length word)) >>
          putCharsEnd '#' (div (count * histWidth) maxCount) '\n')
        ioUnit (sortBy orderTuple (assocs entries))
      putCharsEnd '=' tableWidth '\n'

createCounts :: Integer -> Integer -> 
                Map String Integer-> [String] -> Histogram
createCounts maxCount longest entries []
    = Histogram maxCount longest entries
createCounts maxCount longest entries (x:xs) =
    let added = insertWith (+) x 1 entries
    in maxCount `seq` longest `seq` createCounts
           (max maxCount (added ! x))
           (max longest (fromIntegral (length x))) added xs

generateHistogram :: [String] -> Histogram
generateHistogram = createCounts 1 1 empty

main = getContents >>=
       printHistogramEntries . generateHistogram . words . map toLower
