import Data.Time.LocalTime
import Data.Time.Clock
import Numeric (readDec)
import System.Environment (getArgs)
import Data.List

data Timestamp = Stamp { timeIn :: TimeOfDay , timeOut :: TimeOfDay } deriving (Eq)
type Day = [Timestamp]

instance Ord Timestamp where
  (Stamp timeIn1 _) `compare` (Stamp timeIn2 _) = timeIn1 `compare` timeIn2

-- parse times like "13:45"
stringToTime :: String -> TimeOfDay
stringToTime t = TimeOfDay h m 0
  where [(h,rest)]  = readDec t
        [(m,rest1)] = readDec (tail rest)

stamp1 :: Timestamp
stamp1 = Stamp { timeIn  = stringToTime "13:30"
               , timeOut = stringToTime "13:50" }

stamp2 :: Timestamp
stamp2 = Stamp { timeIn  = stringToTime "14:00"
               , timeOut = stringToTime "15:00" }

day :: Day
day = [stamp1, stamp2]

instance Show Timestamp where
  show (Stamp timeIn timeOut) = (show timeIn) ++ " -> " ++ (show timeOut)
