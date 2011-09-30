import Data.Char
import Data.Map (Map, assocs, insertWith, empty, (!))
data Histogram = Histogram Integer (Map String Integer)

histWidth = 60
wordMaxLen = 12
tickSpace = 8
tableWidth = wordMaxLen + 1 + histWidth
ioUnit = return ()

putChars :: Integral a => Char -> a -> IO ()
putChars ch times | times > 0 = putChar ch >>
                                putChars ch (times - 1)
                  | otherwise = ioUnit

putCharsEnd :: Integral a => Char -> a -> Char -> IO ()
putCharsEnd ch times end = (putChars ch times) >> putChar end

printTick :: RealFrac a => a -> IO a -> String -> IO a
printTick stepSize x num = do
  remainder <- x
  let real = remainder + stepSize - fromIntegral (length num) in
    let toPrint = floor real in
      if (toPrint >= 3)
      then do
        putChars ' ' toPrint 
        putStr num
        return (real - (fromIntegral toPrint))
      else do
        return (real + (fromIntegral (length num)))

printLegend maxCount =
    let stepSize = (fromIntegral histWidth) / (fromIntegral maxCount)
    in do
      putCharsEnd ' ' wordMaxLen '0'
      foldl (printTick stepSize) (return 0) (map show [1..maxCount])
      putStr "\n"

printHistogramEntries :: Histogram -> IO ()
printHistogramEntries (Histogram maxCount entries) = do
  putCharsEnd '=' tableWidth '\n'
  foldl (\x (key, count) ->
        x >> putStr (take (fromIntegral wordMaxLen) key) >>
        putCharsEnd ' ' (wordMaxLen - fromIntegral(length key)) '|' >>
        putCharsEnd '*' (div (count * histWidth) maxCount) '\n')
    ioUnit (assocs entries)
  printLegend maxCount
  putCharsEnd '=' tableWidth '\n'

createCounts :: Integer -> Map String Integer -> [String] -> Histogram
createCounts maxCount entries []     = Histogram maxCount entries
createCounts maxCount entries (x:xs) =
    let added = insertWith (+) x 1 entries
    in maxCount `seq` createCounts (max maxCount (added ! x)) added xs

generateHistogram :: [String] -> Histogram
generateHistogram input = createCounts 1 empty input

main = getContents >>=
       printHistogramEntries . generateHistogram . words . map toLower
