module Archiver where

import Data.Word (Word8)
import Data.List (foldl, unfoldr)
import qualified Data.ByteString as BS
import Data.Bits (testBit)
import System.IO
import Control.Monad (replicateM)


writeFreqTableToFile :: FilePath -> [(Char, Int)] -> IO ()
writeFreqTableToFile filePath freqTable = do
  writeFile filePath (show $ length freqTable)
  let formattedTable = map formatFreqLine freqTable
      content = unlines formattedTable

  appendFile filePath "\n"
  appendFile filePath content

formatFreqLine :: (Char, Int) -> String
formatFreqLine (char, count)
  | char == '\n' = "\\\\n " ++ show count
  | otherwise    = char : ' ' : show count

writeBinaryToFile :: FilePath -> String -> IO ()
writeBinaryToFile filePath binaryString = do
  let bytes = map (toInt . reverse) (chunksOf 8 binaryString)
      byteString = BS.pack bytes
  BS.appendFile filePath byteString

toInt :: String -> Word8
toInt = foldl (\acc bit -> acc * 2 + fromIntegral (read [bit])) 0

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

readBinaryFile :: FilePath -> IO String
readBinaryFile filePath = do
  byteString <- BS.readFile filePath
  return (bytesToBinaryString (BS.unpack byteString))

bytesToBinaryString :: [Word8] -> String
bytesToBinaryString = concatMap byteToBinary
  where
    byteToBinary byte = reverse (map bitToChar [7, 6 .. 0])
      where
        bitToChar bit = if testBit byte bit then '1' else '0'

byteToBinary :: Word8 -> String
byteToBinary byte = reverse (map bitToChar [7, 6 .. 0])
  where
    bitToChar bit = if testBit byte bit then '1' else '0'

parseLine :: String -> (Char, Int)
parseLine line =
  let (charPart, countStr) = span (/= ' ') line
      char = case charPart of
        "\\\\n" -> '\n'
        "" -> ' '
        _ -> head charPart
  in (char, read countStr)

readLinesAndBinary filePath = withFile filePath ReadMode $ \handle -> do
  nStr <- hGetLine handle
  let n = read nStr :: Int

  linesRead <- replicateM n (hGetLine handle)
  let parsedLines = map parseLine linesRead

  endPosition <- hTell handle

  return (parsedLines, endPosition)

readBinaryFileFromPosition :: FilePath -> Integer -> IO String
readBinaryFileFromPosition filePath position = do
  handle <- openFile filePath ReadMode
  hSeek handle AbsoluteSeek position
  binaryData <- BS.hGetContents handle
  hClose handle
  return (bytesToBinaryString (BS.unpack binaryData))