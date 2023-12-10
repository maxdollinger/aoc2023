import Data.List
import Text.Regex.TDFA

main :: IO ()
main = do
  content <- getContents
  let cards = map (getCardWinns . parseCard) (lines content)
  print (getNumberOfWonCards cards + length cards)

getNumberOfWonCards :: [(Int, [Int])] -> Int
getNumberOfWonCards cards = length (concatMap (replaceWins cards) cards)

replaceWins :: [(Int, [Int])] -> (Int, [Int]) -> [(Int, [Int])]
replaceWins cards (_, ws) =
  let wins = map (findCardByNumber cards) ws
   in if null wins
        then wins
        else wins ++ concatMap (replaceWins cards) wins

findCardByNumber :: [(Int, [Int])] -> Int -> (Int, [Int])
findCardByNumber [] _ = (0, [])
findCardByNumber ((n, w) : xs) c
  | n == c = (n, w)
  | otherwise = findCardByNumber xs c

getCardWinns :: (Int, [Int], [Int]) -> (Int, [Int])
getCardWinns (c, w, n) = (c, winnsToCardNumbers c (w `intersect` n))

winnsToCardNumbers :: Int -> [Int] -> [Int]
winnsToCardNumbers _ [] = []
winnsToCardNumbers n (_ : xs) = n + 1 : winnsToCardNumbers (n + 1) xs

parseCard :: String -> (Int, [Int], [Int])
parseCard str = (extractCardNumber str, extractWinningNumbers str, extractNumbers str)

parseInts :: String -> [Int]
parseInts str = map (\s -> read s :: Int) (getAllTextMatches (str =~ "[0-9]+") :: [String])

extractCardNumber :: String -> Int
extractCardNumber str = read ((str =~ "[0-9]+") :: String) :: Int

extractNumbers :: String -> [Int]
extractNumbers = parseInts . dropWhile (/= '|')

extractWinningNumbers :: String -> [Int]
extractWinningNumbers = parseInts . takeWhile (/= '|') . dropWhile (/= ':')
