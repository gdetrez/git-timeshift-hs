module Lib where

import Control.Applicative
import Data.Char hiding (Space)
-- import Data.Hourglass
import Text.Regex.Applicative
-- import Debug.Trace
import Data.Time

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Seconds = Integer

data TimeShift
  = Offset { offsetSeconds :: Seconds }
  | FortyTwo -- 🐰🥚
  -- | TimeRange { timeRangeStart :: TimeOfDay, timeRangeEnd :: TimeOfDay }
  deriving (Show, Eq)

parseTimeShift :: String -> Either String TimeShift
parseTimeShift s =
    case match timeshift s of
      Nothing -> Left "Invalid timeshift, try `+1h`, `-45m` or `+6s`"
      Just x -> Right x
  where
    timeshift :: RE Char TimeShift
    timeshift = Offset <$> offset <|> "#42" *> pure FortyTwo
        -- <|> TimeRange <$> time <* string ".." <*> time
  --   time :: RE Char TimeOfDay
  --   time = fromJust <$> (makeTimeOfDayValid <$> num <*> (sym ':' *> num) <*> pure 0)
    offset :: RE Char Seconds
    offset = (id <$ sym '+' <|> negate <$ sym '-') <*> duration
    duration :: RE Char Seconds
    duration = hours <|> minutes <|> seconds
    hours :: RE Char Seconds
    hours = (* 3600) <$> num <* sym 'h'
    minutes :: RE Char Seconds
    minutes = (* 60) <$> num <* sym 'm'
    seconds :: RE Char Seconds
    seconds = num <* sym 's'
    num :: (Read a, Num a) => RE Char a
    num = fromInteger . read <$> many (psym isDigit)

applyTimeShift :: TimeShift -> ZonedTime -> ZonedTime
applyTimeShift (Offset seconds) t@(ZonedTime _ tz) = utcToZonedTime tz $ addUTCTime (fromInteger seconds) $ zonedTimeToUTC t
applyTimeShift FortyTwo (ZonedTime (LocalTime day tod) tz) = ZonedTime (LocalTime day tod') tz
  where tod' = TimeOfDay (todHour tod) (todMin tod) 42

setSeconds :: Seconds -> ZonedTime -> ZonedTime
setSeconds s (ZonedTime (LocalTime day tod) tz) = ZonedTime (LocalTime day tod') tz
  where tod' = TimeOfDay (todHour tod) (todMin tod) (fromInteger s)

