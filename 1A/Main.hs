import Control.Monad
import Data.Char (isDigit)
import System.IO

main = do
  content <- getContents
  print (sum (getDigits (lines content)))

getDigits = map ((\s -> read s :: Int) . headAndLast . filter isDigit)

headAndLast xs = [head xs, last xs]
