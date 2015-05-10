import Data.Time.LocalTime
import Numeric (readDec)
import System.Environment (getArgs)
import Data.List

data Timestamp = In TimeOfDay | Out TimeOfDay deriving (Show, Eq)
type Day = [Timestamp]

instance Ord Timestamp where
  (In time1) `compare` (In time2) = time1 `compare` time2
  (In time3) `compare` (Out time4) = time3 `compare` time4
  (Out time5) `compare` (In time6) = time5 `compare` time6
  (Out time7) `compare` (Out time8) = time7 `compare` time8

-- parse times like "13:45"
parseStringTime :: String -> TimeOfDay
parseStringTime t = TimeOfDay h m 0
  where [(h,rest)]  = readDec t
        [(m,rest1)] = readDec (tail rest)

day :: Day
day = [In (parseStringTime "13:45"), Out (parseStringTime "13:50"), In (parseStringTime "15:00"), Out (parseStringTime "16:00")]

main :: IO ()
main = do
  args <- getArgs
  return ()
