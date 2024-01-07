module Huffman where

import Data.Map (Map, fromListWith)
import qualified Data.Map as Map
import Data.List (sortOn, isPrefixOf)
import Data.Maybe (fromJust)

data HuffmanNode = Leaf Char Int | InternalNode Int HuffmanNode HuffmanNode deriving (Show)

createPriorityQueue :: String -> [(Char, Int)]
createPriorityQueue str = sortOn snd (Map.assocs freqMap)
  where
    freqMap = getCharacterFrequencies str

getCharacterFrequencies :: String -> Map Char Int
getCharacterFrequencies str = Map.fromListWith (+) (map (\c -> (c, 1)) str)

buildHuffmanTree :: [(Char, Int)] -> HuffmanNode
buildHuffmanTree pq = buildTree $ map (uncurry Leaf) pq
  where
    buildTree [node] = node
    buildTree nodes = buildTree $ combineNodes nodes

    combineNodes (x:y:xs) = sortOn getFrequency (InternalNode (getFrequency x + getFrequency y) x y : xs)

    getFrequency (Leaf _ freq) = freq
    getFrequency (InternalNode freq _ _) = freq


buildCodeTable :: HuffmanNode -> Map Char String
buildCodeTable tree = Map.fromList $ buildCodeTable' tree ""
  where
    buildCodeTable' (Leaf c _) code = [(c, code)]
    buildCodeTable' (InternalNode _ left right) code =
      buildCodeTable' left (code ++ "0") ++ buildCodeTable' right (code ++ "1")

encodeString :: Map Char String -> String -> String
encodeString codeTable = concatMap (\c -> fromJust (Map.lookup c codeTable))

decodeString :: Map Char String -> String -> String
decodeString codeTable str = decodeString' "" str
  where
    decodeString' decoded "" = decoded
    decodeString' decoded remainingStr =
      case findMatchingCode codeTable remainingStr of
        Just (char, code) -> decodeString' (decoded ++ [char]) (drop (length code) remainingStr)
        Nothing -> decoded

    findMatchingCode :: Map Char String -> String -> Maybe (Char, String)
    findMatchingCode codeTable str = headMay $ filter (\(_, code) -> code `isPrefixOf` str) (Map.assocs codeTable)

    headMay :: [a] -> Maybe a
    headMay [] = Nothing
    headMay (x:_) = Just x
    