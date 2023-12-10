import Data.Char (isDigit)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.Regex.TDFA

main :: IO ()
main = do
  input <- getContents
  let (seeds, mappings) = parseInput input
  let locs = map (findLocation mappings) seeds
  print (minimum locs)

type Range = (Int, Int)

type Mapping = (Range, Int)

findLocation :: [[Mapping]] -> Int -> Int
findLocation ms loc = foldl (flip mapNumber) loc ms

isInRange :: Int -> Mapping -> Bool
isInRange n ((s, e), _) = s <= n && e >= n

calcMappedNumber :: Mapping -> Int -> Int
calcMappedNumber (_, offset) n = n + offset

mapNumber :: [Mapping] -> Int -> Int
mapNumber m n
  | isJust mapping = calcMappedNumber (fromJust mapping) n
  | otherwise = n
  where
    mapping = find (isInRange n) m

parseInput :: String -> ([Int], [[Mapping]])
parseInput str =
  let splits = splitOn "map" str
   in (parseInts (head splits), parseMaps (tail splits))

parseMaps :: [String] -> [[Mapping]]
parseMaps = map (mapMaybe (createMapping . parseInts) . filter isMappingLine . lines)

createMapping :: [Int] -> Maybe Mapping
createMapping [d, s, r] = Just ((s, s + r), d - s)
createMapping _ = Nothing

isMappingLine :: String -> Bool
isMappingLine [] = False
isMappingLine (x : _) = isDigit x

parseInts :: String -> [Int]
parseInts str = map (\s -> read s :: Int) (getAllTextMatches (str =~ "[0-9]+") :: [String])
