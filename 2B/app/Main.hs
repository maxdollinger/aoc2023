import Text.Regex.TDFA

main :: IO ()
main = do
  input <- getContents
  print (sum $ map calcGamePower $ lines input)

calcGamePower :: String -> Integer
calcGamePower str = multiplyValues $ getGameValues str

multiplyValues :: [Integer] -> Integer
multiplyValues [] = 0
multiplyValues (x : xs) = foldr (*) x xs

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
