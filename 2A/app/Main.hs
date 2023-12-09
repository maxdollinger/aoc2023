import Text.Regex.TDFA

main :: IO ()
main = do
  input <- getContents
  let games = map validateGame (lines input)
  print (sumValidGameIdxs games 1 0)

sumValidGameIdxs :: [Bool] -> Integer -> Integer -> Integer
sumValidGameIdxs [] _ c = c
sumValidGameIdxs (True : xs) i c = sumValidGameIdxs xs (i + 1) (c + i)
sumValidGameIdxs (False : xs) i c = sumValidGameIdxs xs (i + 1) c

validateGame :: String -> Bool
validateGame str = isValidGame $ getGameValues str

isValidGame :: [Integer] -> Bool
isValidGame [r, g, b] = 12 >= r && 13 >= g && 14 >= b
isValidGame _ = False

getGameValues :: String -> [Integer]
getGameValues str = [getColorValue "red" str, getColorValue "green" str, getColorValue "blue" str]

getColorValue :: String -> String -> Integer
getColorValue color str = extractMax $ map getNumber $ extractColorValues color str

extractMax :: [Integer] -> Integer
extractMax [] = 0
extractMax (x : xs) = foldr max x xs

getNumber :: String -> Integer
getNumber str = read (str =~ "[0-9]+" :: String) :: Integer

extractColorValues :: String -> String -> [String]
extractColorValues color str = getAllTextMatches (str =~ ("[[:digit:]]+ " ++ color)) :: [String]
