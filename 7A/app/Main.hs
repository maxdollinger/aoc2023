import Data.Char (digitToInt, isDigit)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Ord

main :: IO ()
main = do
  input <- getContents
  let games = parseInput input
  print ((calcBidSum . orderGames) games)

type Game = ([Int], Int, Int)

calcBidSum :: [Game] -> Int
calcBidSum = sum . zipWith (curry (\(idx, (_, b, _)) -> idx * b)) [1 ..]

orderGames :: [Game] -> [Game]
orderGames = sortBy compareGames . map addHandValue

compareGames :: Game -> Game -> Ordering
compareGames (as, _, a) (bs, _, b)
  | a < b = LT
  | a > b = GT
  | a == b = compareGames (tail as, 0, head as) (tail bs, 0, head bs)
  | otherwise = EQ

elcnt :: [Int] -> [Int]
elcnt xs = sortBy (comparing Data.Ord.Down) $ map (\x -> length (filter (== x) xs)) xs

addHandValue :: Game -> Game
addHandValue (hand, bid, _) = (hand, bid, rank (elcnt hand))
  where
    rank (5 : _) = 7
    rank (4 : _) = 6
    rank [3, 3, 3, 2, 2] = 5
    rank (3 : _) = 4
    rank [2, 2, 2, 2, _] = 3
    rank (2 : _) = 2
    rank _ = 1

parseInput :: String -> [Game]
parseInput = map parseGame . lines

parseGame :: String -> Game
parseGame str = (parseHand (takeWhile (/= ' ') str), read (dropWhile (/= ' ') str), 1)

parseHand :: String -> [Int]
parseHand (c : xs) = toInt c : parseHand xs
  where
    toInt char
      | isDigit char = digitToInt char
      | char == 'A' = 14
      | char == 'K' = 13
      | char == 'Q' = 12
      | char == 'J' = 11
      | char == 'T' = 10
      | otherwise = 0
parseHand [] = []
