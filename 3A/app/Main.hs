import Text.Regex.TDFA

main :: IO ()
main = do
  content <- getContents
  let nIdxs = addLineNumber (map getNumberIdxs (lines content)) 0
  let symbols = addLineNumber (map getSymbols (lines content)) 0
  let takeNumbers = map (`isNearSymbol` symbols) nIdxs
  print (sumNumbers (getNumbers (lines content)) takeNumbers)

sumNumbers :: [Int] -> [Bool] -> Int
sumNumbers ns bs = sum $ map snd (filter fst (zip bs ns))

getNumbers :: [String] -> [Int]
getNumbers = concatMap (\str -> map (\s -> read s :: Int) (getAllTextMatches (str =~ "[0-9]+") :: [String]))

addLineNumber :: [[(Int, Int)]] -> Int -> [(Int, Int, Int)]
addLineNumber [] _ = []
addLineNumber (l : ls) ln = map (\(s, e) -> (s, e, ln)) l ++ addLineNumber ls (ln + 1)

isNearSymbol :: (Int, Int, Int) -> [(Int, Int, Int)] -> Bool
isNearSymbol num = any (isSymbolInRange num)

isSymbolInRange :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isSymbolInRange (ns, ne, nl) (sys, sye, sl) = ns - 1 <= sys && ne + 1 >= sye && (sl == nl + 1 || sl == nl || sl == nl - 1)

getNumberIdxs :: String -> [(Int, Int)]
getNumberIdxs str = map calcEndIndx $ getAllMatches (str =~ "[[:digit:]]+") :: [(Int, Int)]

getSymbols :: String -> [(Int, Int)]
getSymbols str = map calcEndIndx $ getAllMatches (str =~ "[^0-9.]") :: [(Int, Int)]

calcEndIndx :: (Int, Int) -> (Int, Int)
calcEndIndx (s, e) = (s, s + e)
