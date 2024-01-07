import Huffman
import Archiver
import System.Environment

main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath, outputFilePath, isEncode] -> do
      if isEncode == "0"
      then do
        contents <- readFile filePath

        let pq = createPriorityQueue contents
            huffmanTree = buildHuffmanTree pq
            codeTable = buildCodeTable huffmanTree
            encodedStr = encodeString codeTable contents

        writeFreqTableToFile outputFilePath pq
        writeBinaryToFile outputFilePath encodedStr
      else do
        (parsedLines, binaryPos) <- readLinesAndBinary filePath
        binaryData <- readBinaryFileFromPosition filePath binaryPos

        let huffmanTree = buildHuffmanTree parsedLines
            codeTable = buildCodeTable huffmanTree
            decoded = decodeString codeTable binaryData

        writeFile outputFilePath decoded

    _ -> putStrLn "Usage: program filePath outputFilePath [0 or 1, 0 - encode, 1 - decode]"