-- © 2002 Peter Thiemann
module MissingH.Wash.Utility.ISO8601 where

import Char
import Monad
import Time

import System.IO.Unsafe

import MissingH.Wash.Utility.IntToString
import MissingH.Wash.Utility.SimpleParser

secondsToString seconds =
  intToString 20 seconds

isoDateToString isoDate = 
  let seconds = unsafePerformIO $ isoDateToSeconds isoDate 
  in secondsToString seconds

isoDateAndTimeToString isoDateAndTime =
  let seconds = unsafePerformIO $ isoDateAndTimeToSeconds isoDateAndTime 
  in secondsToString seconds

applyToCalT :: (CalendarTime -> a) -> IO a
applyToCalT g =
  do clkT <- getClockTime
     calT <- toCalendarTime clkT
     return $ g calT

isoDateAndTimeToSeconds :: ISODateAndTime -> IO Integer
isoDateAndTimeToSeconds isoDateAndTime =
  applyToCalT $ toSeconds isoDateAndTime

isoTimeToSeconds :: ISOTime -> IO Integer
isoTimeToSeconds isoTime =
  applyToCalT $ toSeconds isoTime
  
isoDateToSeconds :: ISODate -> IO Integer
isoDateToSeconds isoDate =
  applyToCalT $ toSeconds isoDate

class ToSeconds iso where
  -- |returns number of seconds since reference point
  toSeconds :: iso -> CalendarTime -> Integer
  toRawSeconds :: iso -> CalendarTime -> Integer
  --
  toRawSeconds = toSeconds

instance ToSeconds ISODateAndTime where
  toSeconds isoDateAndTime@(ISODateAndTime isoDate isoTime) calT =
    let rawseconds = toRawSeconds isoDateAndTime calT in
    case addLeapSeconds leapSeconds rawseconds of
      NotLeapSecond seconds -> seconds
      LeapSecond seconds -> seconds + leapSecondCorrection isoTime

  toRawSeconds (ISODateAndTime isoDate isoTime) calT =
    toRawSeconds isoDate calT + toRawSeconds isoTime calT

-- |problem: 19720630T235960 and 19720701T000000 are both mapped to the same
-- number, 78796800, and then addLeapSeconds adds one yielding 78796801. While
-- this is correct for 19720701T000000, 19720630T235960 must be
-- 78796800. Implemented solution: if the current second specification is 0 and
-- the time to convert is the leap second, then add 1.
leapSecondCorrection (ISOTime isoHourSpec isoMinuteSpec isoSecondSpec isoTimeZoneSpec) =
  case isoSecondSpec of
    Second ss -> if ss == 0 then 1 else 0
    NoSecond  -> 1

instance ToSeconds ISODate where
  toSeconds isoDate calT =
    case addLeapSeconds leapSeconds (toRawSeconds isoDate calT) of
      NotLeapSecond seconds -> seconds
      LeapSecond seconds -> seconds + 1			    -- we always mean 00:00:00
	
  toRawSeconds (ISODate isoYearSpec isoDayOfYearSpec) calT =
    let year = isoYearSpecToYear isoYearSpec calT 
    in
    secondsPerDay * fromIntegral (yearsToDays year) +
    isoDaysOfYearToSeconds year isoDayOfYearSpec calT

isoDaysOfYearToSeconds year NoDayOfYear calT =
  0
isoDaysOfYearToSeconds year (MonthDay isoMonthSpec isoDayOfMonthSpec) calT =
  let month = isoMonthSpecToMonth isoMonthSpec calT
      dayOfMonth = isoDayOfMonthSpecToDayOfMonth isoDayOfMonthSpec calT
  in
  fromIntegral(dayOfMonth - 1 + daysUptoMonth year month) * secondsPerDay
isoDaysOfYearToSeconds year (DayOfYear ddd) calT =
  fromIntegral ddd * secondsPerDay
isoDaysOfYearToSeconds year (WeekAndDay (Week ww) NoDayOfWeek) calT =
  fromIntegral (7 * (ww - 1)) * secondsPerDay
isoDaysOfYearToSeconds year (WeekAndDay (Week ww) (DayOfWeek d)) calT =
  let weekdayOfJan1 = yearsToWeekDay year in
  fromIntegral (7 * (ww - 1) + d - weekdayOfJan1) * secondsPerDay
isoDaysOfYearToSeconds year (WeekAndDay ImplicitWeek (DayOfWeek d)) calT =
  let weekdayOfJan1 = yearsToWeekDay year 
      ww = (ctYDay calT + weekdayOfJan1 + 5) `div` 7 
  in
  fromIntegral (7 * (ww - 1) + d - weekdayOfJan1) * secondsPerDay
isoDaysOfYearToSeconds year (WeekAndDay _ _) calT =
  error "Sorry, this combination of week and day does not make sense!"

isoMonthSpecToMonth ImplicitMonth calT =
  fromEnum (ctMonth calT) + 1
isoMonthSpecToMonth (Month mm) calT =
  mm

isoDayOfMonthSpecToDayOfMonth NoDayOfMonth calT =
  1
isoDayOfMonthSpecToDayOfMonth (DayOfMonth dd) calT =
  dd

daysUptoMonth year month = 
  let daysPerMonth = [31, 28 + leapDays year, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
  sum (take (month-1) daysPerMonth)

isoYearSpecToYear ImplicitYear calT = 
  (ctYear calT)
isoYearSpecToYear (ImplicitCentury yy) calT =
  (100 * (ctYear calT `div` 100) + yy)
isoYearSpecToYear (Century cc) calT =
  (100 * cc)
isoYearSpecToYear (ImplicitDecade y) calT =
  (10 * (ctYear calT `div` 10) + y)
isoYearSpecToYear (Year ccyy) calT =
  ccyy

leapDays year =
  if leapYear year then 1 else 0

leapYear year =
  year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)

yearsToDays ccyy =
  let nrOfYears = ccyy - 1970
      leapYears = [ year | year <- [1970 .. ccyy-1] , leapYear year ]
      nrOfLeapDays = length leapYears
  in 
  365 * nrOfYears + nrOfLeapDays

-- |compute weekday of Jan 1
yearsToWeekDay ccyy =
  let nrOfDays = yearsToDays ccyy
      jan_1_1970 = 4 -- Thursday
  in  1 + (nrOfDays + 6) `mod` 7

-- |in seconds from epoch; needs to be updated when time leaps again
leapSeconds :: [Integer]
leapSeconds = 
  [ -- Leap	1972	Jun	30	23:59:60	+	S
    00000000000078796800,
    -- Leap	1972	Dec	31	23:59:60	+	S
    00000000000094694400 + 1,
    -- Leap	1973	Dec	31	23:59:60	+	S
    00000000000126230400 + 2,
    -- Leap	1974	Dec	31	23:59:60	+	S
    00000000000157766400 + 3,
    -- Leap	1975	Dec	31	23:59:60	+	S
    00000000000189302400 + 4,
    -- Leap	1976	Dec	31	23:59:60	+	S
    00000000000220924800 + 5,
    -- Leap	1977	Dec	31	23:59:60	+	S
    00000000000252460800 + 6,
    -- Leap	1978	Dec	31	23:59:60	+	S
    00000000000283996800 + 7,
    -- Leap	1979	Dec	31	23:59:60	+	S
    00000000000315532800 + 8,
    -- Leap	1981	Jun	30	23:59:60	+	S
    00000000000362793600 + 9,
    -- Leap	1982	Jun	30	23:59:60	+	S
    00000000000394329600 + 10,
    -- Leap	1983	Jun	30	23:59:60	+	S
    00000000000425865600 + 11,
    -- Leap	1985	Jun	30	23:59:60	+	S
    00000000000489024000 + 12,
    -- Leap	1987	Dec	31	23:59:60	+	S
    00000000000567993600 + 13,
    -- Leap	1989	Dec	31	23:59:60	+	S
    00000000000631152000 + 14,
    -- Leap	1990	Dec	31	23:59:60	+	S
    00000000000662688000 + 15,
    -- Leap	1992	Jun	30	23:59:60	+	S
    00000000000709948800 + 16,
    -- Leap	1993	Jun	30	23:59:60	+	S
    00000000000741484800 + 17,
    -- Leap	1994	Jun	30	23:59:60	+	S
    00000000000773020800 + 18,
    -- Leap	1995	Dec	31	23:59:60	+	S
    00000000000820454400 + 19,
    -- Leap	1997	Jun	30	23:59:60	+	S
    00000000000867715200 + 20,
    -- Leap	1998	Dec	31	23:59:60	+	S
    00000000000915148800 + 21
  ]

data LeapSeconds = LeapSecond Integer | NotLeapSecond Integer
  deriving Show

addLeapSeconds [] seconds = NotLeapSecond seconds
addLeapSeconds (ls: rest) seconds =
  if ls > seconds then NotLeapSecond seconds else
  if ls == seconds then LeapSecond seconds else
  addLeapSeconds rest (seconds+1)
  
secondsPerMinute = 60
secondsPerHour = 60 * secondsPerMinute
secondsPerDay = 24 * secondsPerHour
secondsPerYear = 365 * secondsPerDay

instance ToSeconds ISOTime where
  -- seconds to 0:00 UTC
  -- may become negative to indicate previous day!
  toSeconds (ISOTime isoHourSpec isoMinuteSpec isoSecondSpec isoTimeZoneSpec) calT =
    toSeconds isoHourSpec calT +
    toSeconds isoMinuteSpec calT +
    toSeconds isoSecondSpec calT +
    toSeconds isoTimeZoneSpec calT
    
instance ToSeconds ISOHourSpec where
  toSeconds ImplicitHour calT = fromIntegral (3600 * ctHour calT - ctTZ calT)
  toSeconds (Hour hh) calT    = fromIntegral (3600 * hh - ctTZ calT)

instance ToSeconds ISOMinuteSpec where
  toSeconds ImplicitMinute calT = fromIntegral (60 * ctMin calT)
  toSeconds (Minute mm) calT = fromIntegral (60 * mm)
  toSeconds NoMinute calT = 0

instance ToSeconds ISOSecondSpec where
  toSeconds (Second ss) calT = fromIntegral ss
  toSeconds NoSecond calT = 0

instance ToSeconds ISOTimeZoneSpec where
  toSeconds LocalTime calT = 0
  toSeconds UTCTime calT = fromIntegral (ctTZ calT)
  toSeconds (PlusTime (Hour hh) isoMinuteSpec) calT = 
    fromIntegral (ctTZ calT - (3600 * hh + 60 * minutes isoMinuteSpec))
  toSeconds (MinusTime (Hour hh) isoMinuteSpec) calT =
    fromIntegral (ctTZ calT + (3600 * hh + 60 * minutes isoMinuteSpec))

minutes ImplicitMinute = 0
minutes (Minute mm) = mm
minutes NoMinute = 0

isoDateToClockTime :: ISODate -> ClockTime
isoDateToClockTime isoDate =
  let seconds = unsafePerformIO $ isoDateToSeconds isoDate 
  in secondsToClockTime seconds

isoDateAndTimeToClockTime :: ISODateAndTime -> ClockTime
isoDateAndTimeToClockTime isoDateAndTime =
  let seconds = unsafePerformIO $ isoDateAndTimeToSeconds isoDateAndTime 
  in secondsToClockTime seconds

secondsToClockTime seconds =
  let tdiff = TimeDiff { tdYear =0,
			 tdMonth =0,
			 tdDay =0,
			 tdHour =0,
			 tdMin =0,
			 tdSec = fromIntegral seconds,
			 tdPicosec =0
		       }
  in addToClockTime tdiff epochClkT
      
epochClkT = toClockTime epoch
epoch = CalendarTime {  ctYear   = 1970,
			ctMonth  = January,
			ctDay    = 1,
			ctHour   = 0,
			ctMin    = 0,
			ctSec    = 0,
			ctPicosec= 0,
			ctWDay   = Thursday,		    -- ignored
			ctYDay   = 0,			    -- ignored
			ctTZName = "UTC",		    -- ignored
			ctTZ     = 0,
			ctIsDST  = False		    -- ignored
		     }

    
-- |data type for representing ISO time
data ISODateAndTime =
  ISODateAndTime ISODate ISOTime
  deriving Show

data ISODate =
  ISODate ISOYearSpec ISODayOfYearSpec
  deriving Show

data ISOYearSpec
	= ImplicitYear | ImplicitCentury Int | Century Int | ImplicitDecade Int | Year Int
  deriving Show
data ISODayOfYearSpec
	= NoDayOfYear
	| MonthDay ISOMonthSpec ISODayOfMonthSpec
	| DayOfYear Int
	| WeekAndDay ISOWeekSpec ISODayOfWeekSpec
  deriving Show
data ISOMonthSpec
	= ImplicitMonth | Month Int
  deriving Show
data ISODayOfMonthSpec
	= NoDayOfMonth | DayOfMonth Int
  deriving Show
data ISOWeekSpec
	= ImplicitWeek | AnyWeek | Week Int
  deriving Show
data ISODayOfWeekSpec
	= NoDayOfWeek | DayOfWeek Int
  deriving Show
data ISOTime
	= ISOTime ISOHourSpec ISOMinuteSpec ISOSecondSpec ISOTimeZoneSpec
  deriving Show
data ISOHourSpec
	= ImplicitHour | Hour Int
  deriving Show
data ISOMinuteSpec
	= ImplicitMinute | Minute Int | NoMinute
  deriving Show
data ISOSecondSpec
	= Second Int | NoSecond
  deriving Show
data ISOTimeZoneSpec
	= LocalTime | UTCTime | PlusTime ISOHourSpec ISOMinuteSpec | MinusTime ISOHourSpec ISOMinuteSpec
  deriving Show

updateTZ (ISOTime isoHourSpec isoMinuteSpec isoSecondSpec _) isoTimeZoneSpec =
	ISOTime isoHourSpec isoMinuteSpec isoSecondSpec isoTimeZoneSpec

digitval = digitToInt

skipHyphen	= char '-' >> return ()
skipColon	= char ':' >> return ()
skipSolidus	= char '/' >> return ()
skipMinus	= char '-' >> return ()
skipPlus	= char '+' >> return ()
skipP		= oneOf "pP" >> return ()
skipT		= oneOf "tT" >> return ()
skipW		= oneOf "wW" >> return ()
skipZ		= oneOf "zZ" >> return ()

parseDateFromString :: String -> Maybe ISODate
parseDateFromString = parseFromString parseDate
parseTimeFromString :: String -> Maybe ISOTime
parseTimeFromString = parseFromString parseTime
parseDateAndTimeFromString :: String -> Maybe ISODateAndTime
parseDateAndTimeFromString = parseFromString parseDateAndTime

-- |external entry point
parseDate = 
  parseBasicOrExtended parseDateInternal

parseTime =
  parseBasicOrExtended parseTimeInternal

parseDateAndTime =
  parseBasicOrExtended parseTimeAndDateInternal

parseBasicOrExtended parser = 
  parser True <|> parser False

parseTimeAndDateInternal extended =
  do isodate <- parseDateInternal extended
     isotime <- option (ISOTime (Hour 0) NoMinute NoSecond UTCTime) 
                       (skipT >> parseTimeInternal extended)
     return $ ISODateAndTime isodate isotime

-- I was pretty much fed up with the irregular format of ISO 8601. After a few
-- tries, I decided that the simplest approach was to just list all the
-- alternatives from the standard.

-- |argument determines whether extended format is parsed
parseDateInternal False =
  -- 5.2.1.1, complete representation, basic format: CCYYMMDD
  (try $ do ccyy <- parseFourDigits
	    mm <- parseTwoDigits
	    dd <- parseTwoDigits
	    return $ ISODate (Year ccyy) $ MonthDay (Month mm) (DayOfMonth dd))
  <|>
  -- !!! CHECK THIS !!!
  -- 5.2.1.2.a, a specific month, basic format: CCYY-MM
  (try $ do ccyy <- parseFourDigits
	    skipHyphen
	    mm <- parseTwoDigits
	    return $ ISODate (Year ccyy) $ MonthDay (Month mm) NoDayOfMonth)
  <|>
  -- 5.2.1.2.b, a specific year, basic format: CCYY
  (try $ do ccyy <- parseFourDigits
	    return $ ISODate (Year ccyy) NoDayOfYear)
  <|>
  -- 5.2.1.2.c, a specific century, basic format: CC
  (try $ do cc <- parseTwoDigits
	    return $ ISODate (Century cc) NoDayOfYear)
  <|>
  -- 5.2.1.3.a, truncated representation, specific date in current century, basic format: YYMMDD
  (try $ do yy <- parseTwoDigits
	    mm <- parseTwoDigits
	    dd <- parseTwoDigits
	    return $ ISODate (ImplicitCentury yy) $ MonthDay (Month mm) (DayOfMonth dd))
  <|>
  -- 5.2.1.3.b, truncated representation, specific year and month in current century, basic format: -YYMM
  (try $ do skipHyphen
	    yy <- parseTwoDigits
	    mm <- parseTwoDigits
	    return $ ISODate (ImplicitCentury yy) $ MonthDay (Month mm) NoDayOfMonth)
  <|>
  -- 5.2.1.3.c, truncated representation, specific year in current century, basic format: -YY
  (try $ do skipHyphen
	    yy <- parseTwoDigits
	    return $ ISODate (ImplicitCentury yy) NoDayOfYear)
  <|>
  -- 5.2.1.3.d, truncated representation, specific day of a month, basic format: --MMDD
  (try $ do skipHyphen
	    skipHyphen
	    mm <- parseTwoDigits
	    dd <- parseTwoDigits
	    return $ ISODate ImplicitYear $ MonthDay (Month mm) (DayOfMonth dd))
  <|>
  -- 5.2.1.3.e, truncated representation, specific month, basic format: --MM
  (try $ do skipHyphen
	    skipHyphen
	    mm <- parseTwoDigits
	    return $ ISODate ImplicitYear $ MonthDay (Month mm) NoDayOfMonth)
  <|>
  -- 5.2.1.3.f, truncated representation, specific day, basic format: ---DD
  (try $ do skipHyphen
	    skipHyphen
	    skipHyphen
	    dd <- parseTwoDigits
	    return $ ISODate ImplicitYear $ MonthDay ImplicitMonth (DayOfMonth dd))
  <|>
  -- 5.2.2 Ordinal date
  -- 5.2.2.1, complete representation, basic format: CCYYDDD
  (try $ do ccyy <- parseFourDigits
	    ddd <- parseOrdinalDay
	    return $ ISODate (Year ccyy) $ DayOfYear ddd)
  <|>
  -- 5.2.2.2.a, truncated representation, specific year and day in current century, basic format: YYDDD
  (try $ do yy <- parseTwoDigits
	    ddd <- parseOrdinalDay
	    return $ ISODate (ImplicitCentury yy) $ DayOfYear ddd)
  <|>
  -- 5.2.2.2.b, truncated representation, specific day only, basic format: -DDD
  (try $ do skipHyphen
	    ddd <- parseOrdinalDay
	    return $ ISODate ImplicitYear $ DayOfYear ddd)
  <|>
  -- 5.2.3 date by calendar week and day number
  -- 5.2.3.1, complete representation, basic format: CCYYWwwD
  (try $ do ccyy <- parseFourDigits
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    d <- parseWeekDay
	    return $ ISODate (Year ccyy) $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- 5.2.3.2, reduced prec representation, basic format: CCYYWww
  (try $ do ccyy <- parseFourDigits
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    return $ ISODate (Year ccyy) $ WeekAndDay (Week ww) NoDayOfWeek)
  <|>
  -- 5.2.3.3.a, truncated representation, current century, basic format: YYWwwD
  (try $ do yy <- parseTwoDigits
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    d <- parseWeekDay
	    return $ ISODate (ImplicitCentury yy) $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- 5.2.3.3.b, truncated representation, current century, year and week only, basic format: YYWww
  (try $ do yy <- parseTwoDigits
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    return $ ISODate (ImplicitCentury yy) $ WeekAndDay (Week ww) NoDayOfWeek)
  <|>
  -- 5.2.3.3.c, truncated representation, current decade, week, and day, basic format: -YWwwD
  (try $ do skipHyphen
	    y <- parseOneDigit
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    d <- parseWeekDay
	    return $ ISODate (ImplicitDecade y) $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- 5.2.3.3.d, truncated representation, current year, week, and day, basic format: -WwwD
  (try $ do skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    d <- parseWeekDay
	    return $ ISODate ImplicitYear $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- 5.2.3.3.e, truncated representation, current year, week only, basic format: -Www
  (try $ do skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    return $ ISODate ImplicitYear $ WeekAndDay (Week ww) NoDayOfWeek)
  <|>
  -- 5.2.3.3.f, truncated representation, day only of current week, basic format: -W-D
  (try $ do skipHyphen
	    skipW
	    skipHyphen
	    d <- parseWeekDay
	    return $ ISODate ImplicitYear $ WeekAndDay ImplicitWeek (DayOfWeek d))
  <|>
  -- 5.2.3.3.g, truncated representation, day only of any week, basic format: ---D
  (try $ do skipHyphen
	    skipHyphen
	    skipHyphen
	    d <- parseWeekDay
	    return $ ISODate ImplicitYear $ WeekAndDay AnyWeek (DayOfWeek d))


-- ----------------------------------------------------------------------
-- extended formats  
parseDateInternal True =
  -- 5.2.1.1, complete representation, extended format CCYY-MM-DD
  (try $ do ccyy <- parseFourDigits
	    skipHyphen
	    mm <- parseTwoDigits
	    skipHyphen
	    dd <- parseTwoDigits
	    return $ ISODate (Year ccyy) $ MonthDay (Month mm) (DayOfMonth dd))
  <|>
  -- 5.2.1.3.a, truncated representation, extended format: YY-MM-DD
  (try $ do yy <- parseTwoDigits
	    skipHyphen
	    mm <- parseTwoDigits
	    skipHyphen
	    dd <- parseTwoDigits
	    return $ ISODate (ImplicitCentury yy) $ MonthDay (Month mm) (DayOfMonth dd))
  <|>
  -- 5.2.1.3.b, truncated representation, specific year and month in current century, extended format: -YY-MM
  (try $ do skipHyphen
	    yy <- parseTwoDigits
	    skipHyphen
	    mm <- parseTwoDigits
	    return $ ISODate (ImplicitCentury yy) $ MonthDay (Month mm) NoDayOfMonth)
  <|>
  -- 5.2.1.3.d, truncated representation, specific day of a month, extended format: --MM-DD
  (try $ do skipHyphen
	    skipHyphen
	    mm <- parseTwoDigits
	    skipHyphen
	    dd <- parseTwoDigits
	    return $ ISODate ImplicitYear $ MonthDay (Month mm) (DayOfMonth dd))
  <|>
  -- 5.2.2 Ordinal date
  -- 5.2.2.1, complete representation, extended format: CCYY-DDD
  (try $ do ccyy <- parseFourDigits
	    skipHyphen
	    ddd <- parseOrdinalDay
	    return $ ISODate (Year ccyy) $ DayOfYear ddd)
  <|>
  -- 5.2.2.2.a, truncated representation, specific year and day in current century, extended format: YY-DDD
  (try $ do yy <- parseTwoDigits
	    skipHyphen
	    ddd <- parseOrdinalDay
	    return $ ISODate (ImplicitCentury yy) $ DayOfYear ddd)
  <|>
  -- 5.2.3 date by calendar week and day number
  -- 5.2.3.1, complete representation, extended format: CCYY-Www-D
  (try $ do ccyy <- parseFourDigits
	    skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    skipHyphen
	    d <- parseWeekDay
	    return $ ISODate (Year ccyy) $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- 5.2.3.2, reduced prec representation, extended format: CCYY-Www
  (try $ do ccyy <- parseFourDigits
	    skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    return $ ISODate (Year ccyy) $ WeekAndDay (Week ww) NoDayOfWeek)
  <|>
  -- 5.2.3.3.a, truncated representation, current century, extended format: YY-Www-D
  (try $ do yy <- parseTwoDigits
	    skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    skipHyphen
	    d <- parseWeekDay
	    return $ ISODate (ImplicitCentury yy) $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- 5.2.3.3.b, truncated representation, current century, year and week only, extended format: YY-Www
  (try $ do yy <- parseTwoDigits
	    skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    return $ ISODate (ImplicitCentury yy) $ WeekAndDay (Week ww) NoDayOfWeek)
  <|>
  -- 5.2.3.3.c, truncated representation, current decade, week, and day, extended format: -Y-Www-D
  (try $ do skipHyphen
	    y <- parseOneDigit
	    skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    skipHyphen
	    d <- parseWeekDay
	    return $ ISODate (ImplicitDecade y) $ WeekAndDay (Week ww) (DayOfWeek d))
  <|>
  -- !!! CHECK THIS
  -- 5.2.3.3.d, truncated representation, current year, week, and day, extended format: -Www-D
  (try $ do skipHyphen
	    skipW
	    ww <- parseTwoDigits
	    checkWeeks ww
	    skipHyphen
	    d <- parseWeekDay
	    return $ ISODate ImplicitYear $ WeekAndDay (Week ww) (DayOfWeek d))

-- |time parsers
parseTimeInternal extended =
  do localtime <- parseLocalTimeInternal extended
     tzsuffix  <- option LocalTime $ parseTZsuffix extended
     return $ updateTZ localtime tzsuffix

parseTZsuffix extended =
  (do skipZ 
      return UTCTime)
  <|>
  (do skipPlus
      (hours, minutes) <- parseHoursMinutes extended
      return $ PlusTime hours minutes)
  <|>
  (do skipMinus
      (hours, minutes) <- parseHoursMinutes extended
      return $ MinusTime hours minutes)

parseHoursMinutes False =
  do hh <- parseTwoDigits
     mm <- option NoMinute $ (liftM Minute) parseTwoDigits
     return (Hour hh, mm)

parseHoursMinutes True =
  do hh <- parseTwoDigits
     mm <- option NoMinute $ (liftM Minute) (skipColon >> parseTwoDigits)
     return (Hour hh, mm)

parseLocalTimeInternal False =
  -- 5.3.1.1, local time, basic format: hhmmss
  (try $ do hh <- parseTwoDigits
	    mm <- parseTwoDigits
	    ss <- parseTwoDigits
	    checkHours hh
	    checkMinutes mm
	    checkSeconds ss
	    return $ ISOTime (Hour hh) (Minute mm) (Second ss) LocalTime)
  <|>
  -- 5.3.1.2, local time, reduced precision, basic format: hhmm ; hh
  (try $ do hh <- parseTwoDigits
	    mm <- parseTwoDigits
	    checkHours hh
	    checkMinutes mm
	    return $ ISOTime (Hour hh) (Minute mm) NoSecond LocalTime)
  <|>
  (try $ do hh <- parseTwoDigits
	    checkHours hh
	    return $ ISOTime (Hour hh) NoMinute NoSecond LocalTime)
  <|>
  -- 5.3.1.4.a, local time, truncated, basic format: -mmss
  (try $ do skipHyphen
	    mm <- parseTwoDigits
	    ss <- parseTwoDigits
	    checkMinutes mm
	    checkSeconds ss
	    return $ ISOTime ImplicitHour (Minute mm) (Second ss) LocalTime)
  <|>
  -- 5.3.1.4.b, local time, truncated, basic format: -mm
  (try $ do skipHyphen
	    mm <- parseTwoDigits
	    checkMinutes mm
	    return $ ISOTime ImplicitHour (Minute mm) NoSecond LocalTime)
  <|>
  -- 5.3.1.4.c, local time, truncated, basic format: --ss
  (try $ do skipHyphen
	    skipHyphen
	    ss <- parseTwoDigits
	    checkSeconds ss
	    return $ ISOTime ImplicitHour ImplicitMinute (Second ss) LocalTime)
  

parseLocalTimeInternal True =
  -- 5.3.1.1, local time, extended format: hh:mm:ss
  (try $ do hh <- parseTwoDigits
	    skipColon
	    mm <- parseTwoDigits
	    skipColon
	    ss <- parseTwoDigits
	    checkHours hh
	    checkMinutes mm
	    checkSeconds ss
	    return $ ISOTime (Hour hh) (Minute mm) (Second ss) LocalTime)
  <|>
  -- 5.3.1.2, local time, reduced precision, extended format: hh:mm
  (try $ do hh <- parseTwoDigits
	    skipColon
	    mm <- parseTwoDigits
	    checkHours hh
	    checkMinutes mm
	    return $ ISOTime (Hour hh) (Minute mm) NoSecond LocalTime)
  <|>
  -- 5.3.1.4.a, local time, truncated, extended format: -mm:ss
  (try $ do skipHyphen
	    mm <- parseTwoDigits
	    skipColon
	    ss <- parseTwoDigits
	    checkMinutes mm
	    checkSeconds ss
	    return $ ISOTime ImplicitHour (Minute mm) (Second ss) LocalTime)

-- make ISOTime, ISODate, ISODateAndTime instances of Read
instance Read ISOTime where
  readsPrec i = parserToRead parseTime

instance Read ISODate where
  readsPrec i = parserToRead parseDate

instance Read ISODateAndTime where
  readsPrec i = parserToRead parseDateAndTime
-- auxiliary parsers

checkSeconds ss = if ss > 60 then fail "more than 60 seconds" else return ()
checkMinutes mm = if mm > 59 then fail "more than 59 minutes" else return ()
checkHours   hh = if hh > 24 then fail "more than 24 hours" else return ()
checkDays   ddd = if ddd < 1 || ddd > 366 then fail "illegal ordinal day" else return ()
checkWeeks   ww = if ww < 1 || ww > 53 then fail "illegal week nr" else return ()

parseWeekDay = do d0 <- oneOf "1234567"
		  return (digitval d0)

parseOneDigit = do d0 <- digit
		   return (digitval d0)
parseTwoDigits = do d1 <- digit
		    vv <- parseOneDigit
		    return (10 * digitval d1 + vv)
parseThreeDigits = do d2 <- digit
		      vv <- parseTwoDigits
		      let vvv = 100 * digitval d2 + vv
		      return vvv
parseOrdinalDay = do vvv <- parseThreeDigits
		     checkDays vvv
		     return vvv
parseFourDigits = do d3 <- digit
		     vvv <- parseThreeDigits
		     return (1000 * digitval d3 + vvv)

