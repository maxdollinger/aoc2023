import Text.Regex.TDFA

main :: IO ()
main = do
  input <- getContents
  let races = parseInput input
  let racesWithDsts = map (filterRecords . mapRaceWithPoss) races
  print (countRecordTries racesWithDsts)

type Race = (Int, Int)

countRecordTries :: [(Int, Int, [Int])] -> Int
countRecordTries = product . map (\(_, _, ds) -> length ds)

filterRecords :: (Int, Int, [Int]) -> (Int, Int, [Int])
filterRecords (t, s, ds) = (t, s, filter (> s) ds)

mapRaceWithPoss :: Race -> (Int, Int, [Int])
mapRaceWithPoss (t, s) = (t, s, calcPossibleDst t 1)

calcPossibleDst :: Int -> Int -> [Int]
calcPossibleDst t b
  | t == b = [0]
  | otherwise = (b * (t - b)) : calcPossibleDst t (b + 1)

parseInput :: String -> [Race]
parseInput = makeRace . take 2 . map parseInts . lines
  where
    makeRace [t, s] = zip t s
    makeRace _ = []

parseInts :: String -> [Int]
parseInts str = map read (getAllTextMatches (str =~ "[0-9]+") :: [String])
