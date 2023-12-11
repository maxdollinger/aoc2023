import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.Regex.TDFA

main :: IO ()
main = do
  input <- getContents
  let (seeds, mappings) = parseInput input
  print (minimum (map (findLocation mappings) seeds))

type Mapping = (Int, Int, Int)

findLocation :: [[Mapping]] -> Int -> Int
findLocation xs loc = foldl (flip mapNumber) loc xs

isInRange :: Int -> Mapping -> Bool
isInRange n (s, e, _) = s <= n && e >= n

calcDest :: Mapping -> Int -> Int
calcDest (srcS, _, dstStart) n = dstStart + (n - srcS)

mapNumber :: [Mapping] -> Int -> Int
mapNumber m n
  | isJust mapping = calcDest (fromJust mapping) n
  | otherwise = n
  where
    mapping = find (isInRange n) m

parseInput :: String -> ([Int], [[Mapping]])
parseInput str =
  let splits = splitOn "map" str
   in (parseSeeds (head splits), parseMappings (tail splits))

parseSeeds :: String -> [Int]
parseSeeds = createRange . parseInts

createRange :: [Int] -> [Int]
createRange (s : r : xs) = [s .. (s + r - 1)] ++ createRange xs
createRange _ = []

parseMappings :: [String] -> [[Mapping]]
parseMappings = map (mapMaybe (createMapping . parseInts) . lines)

createMapping :: [Int] -> Maybe Mapping
createMapping [d, s, r] = Just (s, s + r - 1, d)
createMapping _ = Nothing

parseInts :: String -> [Int]
parseInts str = map read (getAllTextMatches (str =~ "[0-9]+") :: [String])
