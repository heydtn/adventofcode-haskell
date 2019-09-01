-- REMINDER: Never touch MD5 in haskell again

import Crypto.Hash.MD5 (hash)
import Numeric (showHex)
import Data.Char (ord)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  file <- readFile "problem4.input" >>= return . filter (/= '\n')
  print . head . dropWhile ((/= "00000") . pullPrefix . BS.unpack . hash . BS.pack . (file++) . show) $ [0..]
  where pullPrefix = take 5 . concatMap (padLeft . flip showHex "" . ord)
        padLeft x  =
          if length x == 1 then
            '0' : x
          else
            x
