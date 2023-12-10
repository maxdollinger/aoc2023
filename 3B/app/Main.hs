import Text.Regex.TDFA

main :: IO ()
main = do
  content <- getContents
  let symbols = extractGearSymbolIdxs content
  let numbers = extractNumberInfo content
  print (sum (getGears symbols numbers))

getGears :: [(Int, Int, Int)] -> [(Int, Int, Int, Int)] -> [Int]
getGears symbols numbers = map (multiGear . (`getNearbyNumbers` numbers)) symbols

multiGear :: [Int] -> Int
multiGear [] = 0
multiGear [_] = 0
multiGear (x : xs) = foldr (*) x xs

getNearbyNumbers :: (Int, Int, Int) -> [(Int, Int, Int, Int)] -> [Int]
getNearbyNumbers symb numbers = map (\(_, _, _, v) -> v) (filter (isNumberInRange symb) numbers)

isNumberInRange :: (Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
isNumberInRange (s, e, l) (ns, ne, nl, _) = ns - 1 <= s && ne + 1 >= e && (l == nl + 1 || l == nl || l == nl - 1)

extractGearSymbolIdxs :: String -> [(Int, Int, Int)]
extractGearSymbolIdxs str = addLineNumber (map findSymbolIdxs (lines str)) 0

findSymbolIdxs :: String -> [(Int, Int)]
findSymbolIdxs str = map calcEndIndx $ getAllMatches (str =~ "[*]") :: [(Int, Int)]

extractNumberInfo :: String -> [(Int, Int, Int, Int)]
extractNumberInfo str = zipWith (curry (\((s, e, i), v) -> (s, e, i, v))) (extractNumberIdxs str) (extractNumberValues str)

extractNumberValues :: String -> [Int]
extractNumberValues str = map (\s -> read s :: Int) (getAllTextMatches (str =~ "[0-9]+") :: [String])

extractNumberIdxs :: String -> [(Int, Int, Int)]
extractNumberIdxs str = addLineNumber (map findNumberIdxs (lines str)) 0

findNumberIdxs :: String -> [(Int, Int)]
findNumberIdxs str = map calcEndIndx $ getAllMatches (str =~ "[0-9]+") :: [(Int, Int)]

addLineNumber :: [[(Int, Int)]] -> Int -> [(Int, Int, Int)]
addLineNumber [] _ = []
addLineNumber (l : ls) ln = map (\(s, e) -> (s, e, ln)) l ++ addLineNumber ls (ln + 1)

calcEndIndx :: (Int, Int) -> (Int, Int)
calcEndIndx (s, e) = (s, s + e)
