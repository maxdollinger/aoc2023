import Data.Bifunctor (Bifunctor (bimap))
import Data.List
import Text.Regex.TDFA

main :: IO ()
main = do
  content <- getContents
  let cards = lines content
  print (sum (map getCardValue cards))

getCardValue :: String -> Int
getCardValue = calcPoints . filterWinns . parseCard

calcPoints :: [Int] -> Int
calcPoints [] = 0
calcPoints xs = 2 ^ (length xs - 1)

filterWinns :: ([Int], [Int]) -> [Int]
filterWinns (w, n) = w `intersect` n

parseCard :: String -> ([Int], [Int])
parseCard = bimap parseInts parseInts . splitParts . removeCardString

parseInts :: String -> [Int]
parseInts str = map (\s -> read s :: Int) (getAllTextMatches (str =~ "[0-9]+") :: [String])

splitParts :: String -> (String, String)
splitParts = span (/= '|')

removeCardString :: String -> String
removeCardString = dropWhile (/= ':')
