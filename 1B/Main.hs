import Control.Monad
import Data.Char (isDigit)
import System.IO

main = do
  content <- getContents
  print (sum (getDigits (lines content)))

getDigits = map ((\s -> read s :: Int) . headAndLast . filter isDigit . replaceNumberWords)

headAndLast xs = [head xs, last xs]

replaceNumberWords =
  replace "one" "o1e"
    . replace "two" "t2o"
    . replace "three" "t3e"
    . replace "four" "4"
    . replace "five" "5e"
    . replace "six" "6"
    . replace "seven" "7n"
    . replace "eight" "e8t"
    . replace "nine" "n9e"

replace substr replacement str =
  case begins str substr of
    Just remains -> replacement ++ replace substr replacement remains
    Nothing -> case str of
      [] -> []
      x : xs -> x : replace substr replacement xs

begins :: (Eq a) => [a] -> [a] -> Maybe [a]
begins str [] = Just str
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _ _ = Nothing
