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
findLocation ms loc = foldl (flip mapNumber) loc ms

isInRange :: Int -> Mapping -> Bool
isInRange n (s, e, _) = s <= n && e >= n

calcDst :: Mapping -> Int -> Int
calcDst (s, _, ds) n = ds + (n - s)

mapNumber :: [Mapping] -> Int -> Int
mapNumber m n
  | isJust mapping = calcDst (fromJust mapping) n
  | otherwise = n
  where
    mapping = find (isInRange n) m

parseInput :: String -> ([Int], [[Mapping]])
parseInput str =
  let splits = splitOn "map" str
   in (parseInts (head splits), parseMaps (tail splits))

parseMaps :: [String] -> [[Mapping]]
parseMaps = map (mapMaybe (createMapping . parseInts) . lines)

createMapping :: [Int] -> Maybe Mapping
createMapping [d, s, r] = Just (s, s + r - 1, d)
createMapping _ = Nothing

parseInts :: String -> [Int]
parseInts str = map read (getAllTextMatches (str =~ "[0-9]+") :: [String])
