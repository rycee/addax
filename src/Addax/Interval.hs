-- Copyright (C) 2017 Robert Helgesson <robert@rycee.net>
--
-- Addax is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Addax is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Addax.  If not, see <http://www.gnu.org/licenses/>.

module Addax.Interval
  ( Interval(..)
  , readInterval
  , readIntervalText
  ) where

import           Control.Monad (when)
import           Data.Char (digitToInt)
import           Data.Foldable (foldl')
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (DiffTime)
import           Text.Parsec
import           Text.Parsec.String (Parser)

-- | A time interval with one second precision.
newtype Interval = Interval DiffTime
  deriving (Eq, Ord)

instance Show Interval where
  show (Interval 0) = "0s"
  show (Interval i) = go (round i)
    where
      tweak n m ch =
        let (q, r) = n `quotRem` m
        in  show q ++ [ch] ++ go r

      go :: Integer -> String
      go n | n >= 60*60*24*7 = tweak n (60*60*24*7) 'w'
           | n >= 60*60*24   = tweak n (60*60*24)   'd'
           | n >= 60*60      = tweak n (60*60)      'h'
           | n >= 60         = tweak n (60)         'm'
           | n >= 1          = show n ++ "s"
           | otherwise       = ""

instance Read Interval where
  readsPrec _ = either (const []) (\ a -> [(a,"")]) . readInterval

decimal :: Parser Int
decimal = foldl' (\ a b -> a + digitToInt b) 0 <$> many1 digit

-- | Parses one interval string component, e.g., 5d.
parseOne :: Char -> Int -> Parser Int
parseOne f m = option 0 . try $ (m *) <$> (decimal <* char f)

-- | Parses an interval string, e.g., 5d2h.
parseInterval :: Parser DiffTime
parseInterval =
  do
    w <- parseOne 'w' (60*60*24*7)
    d <- parseOne 'd' (60*60*24  )
    h <- parseOne 'h' (60*60     )
    m <- parseOne 'm' (60        )
    s <- parseOne 's' (1         )
    let r = w + d + h + m + s
    when (r == 0) $ parserFail "non-zero interval expected"
    eof
    return (fromIntegral r)

-- | Reads an interval string.
readInterval :: String -> Either String Interval
readInterval =
    either (Left . show) (Right . Interval)
    . runParser parseInterval () ""

readIntervalText :: Text -> Either String Interval
readIntervalText = readInterval . T.unpack
