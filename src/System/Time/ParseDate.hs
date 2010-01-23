{- |
   Module      : System.Time.ParseDate
   Copyright   : (c) by Björn Bringert
   License     : GPL2

   Maintainer  : Björn Bringert
   Stability   : provisional
   Portability : portable

Utility for parsing dates.
-}
module System.Time.ParseDate (parseCalendarTime) where

import Control.Monad (liftM)
import Data.Char (isSpace)
import System.Locale
import System.Time
import Text.ParserCombinators.Parsec

{- | Parse a date string as formatted by 'formatCalendarTime'.

   The resulting 'CalendarTime' will only have those fields set that
   are represented by a format specifier in the format string, and those
   fields will be set to the values given in the date string.
   If the same field is specified multiple times, the rightmost
   occurence takes precedence.

   The resulting date is not neccessarily a valid date. For example,
   if there is no day of the week specifier in the format string,
   the value of 'ctWDay' will most likely be invalid.

   Format specifiers are % followed by some character. All other
   characters are treated literally. Whitespace in the format string
   matches zero or more arbitrary whitespace characters.

   Format specifiers marked with * are matched, but do not set any
   field in the output.

   Some of the format specifiers are marked as space-padded or
   zero-padded. Regardless of this, space-padded, zero-padded
   or unpadded inputs are accepted. Note that strings using
   unpadded fields without separating the fields may cause
   strange parsing.

   Supported format specfiers:

    [%%]   a % character.

    [%a]   locale's abbreviated weekday name (Sun ... Sat)

    [%A]   locale's full weekday name (Sunday .. Saturday)

    [%b]   locale's abbreviated month name (Jan..Dec)

    [%B]   locale's full month name (January..December)

    [%c]   locale's date and time format (Thu Mar 25 17:47:03 CET 2004)

    [%C]   century [00-99]

    [%d]   day of month, zero padded (01..31)

    [%D]   date (%m\/%d\/%y)

    [%e]   day of month, space padded ( 1..31)

    [%h]   same as %b

    [%H]   hour, 24-hour clock, zero padded (00..23)

    [%I]   hour, 12-hour clock, zero padded (01..12)

    [%j]   day of the year, zero padded (001..366)

    [%k]   hour, 24-hour clock, space padded ( 0..23)

    [%l]   hour, 12-hour clock, space padded ( 1..12)

    [%m]   month, zero padded (01..12)

    [%M]   minute, zero padded (00..59)

    [%n]   a newline character

    [%p]   locale's AM or PM indicator

    [%r]   locale's 12-hour time format (hh:mm:ss AM\/PM)

    [%R]   hours and minutes, 24-hour clock (hh:mm)

    [%s]   * seconds since '00:00:00 1970-01-01 UTC'

    [%S]   seconds, zero padded (00..59)

    [%t]   a horizontal tab character

    [%T]   time, 24-hour clock (hh:mm:ss)

    [%u]   numeric day of the week (1=Monday, 7=Sunday)

    [%U]   * week number, weeks starting on Sunday, zero padded (01-53)

    [%V]   * week number (as per ISO-8601),
             week 1 is the first week with a Thursday,
             zero padded, (01-53)

    [%w]   numeric day of the week, (0=Sunday, 6=Monday)

    [%W]   * week number, weeks starting on Monday, zero padded (01-53)

    [%x]   locale's preferred way of printing dates (%m\/%d\/%y)

    [%X]   locale's preferred way of printing time. (%H:%M:%S)

    [%y]   year, within century, zero padded (00..99)

    [%Y]   year, including century. Not padded
           (this is probably a bug, but formatCalendarTime does
           it this way). (0-9999)

    [%Z]   time zone abbreviation (e.g. CET) or RFC-822 style numeric
           timezone (-0500) -}
parseCalendarTime ::
    TimeLocale            -- ^ Time locale
    -> String             -- ^ Date format
    -> String             -- ^ String to parse
    -> Maybe CalendarTime -- ^ 'Nothing' if parsing failed.
parseCalendarTime l fmt s =
    case runParser parser epoch "<date string>" s of
         Left _ -> Nothing
         Right p -> Just p
    where parser = pCalendarTime l fmt >> getState

-- FIXME: verify input
-- FIXME: years outside 1000-9999 probably don't work
-- FIXME: what about extra whitespace in input?
-- FIXME: set ctYDay
-- FIXME: set ctIsDST
-- FIXME: missing formats from GNU date(1):
-- %F     same as %Y-%m-%d
-- %g     the 2-digit year corresponding to the %V week number
-- %G     the 4-digit year corresponding to the %V week number
-- %N     nanoseconds (000000000..999999999)
-- %P     locale's lower case am or pm indicator (blank in many locales)
-- %z     RFC-822 style numeric timezone (-0500) (a nonstandard extension)
pCalendarTime :: TimeLocale -> String -> GenParser Char CalendarTime ()
pCalendarTime l fmt = doFmt fmt
    where
    -- not padded
    -- FIXME: implement
    doFmt ('%':'-':cs) = doFmt ('%':cs)
    -- space padded
    -- FIXME: implement
    doFmt ('%':'_':cs) = doFmt ('%':cs)
    doFmt ('%':c:cs) = decode c >> doFmt cs
    doFmt (c:cs) = char c >> doFmt cs
    doFmt "" = return ()

    decode '%' = char '%' >> return ()
    decode 'a' = (parseEnum $ map snd $ wDays l) >>= setWDay
    decode 'A' = (parseEnum $ map fst $ wDays l) >>= setWDay
    decode 'b' = (parseEnum $ map snd $ months l) >>= setMonth
    decode 'B' = (parseEnum $ map fst $ months l) >>= setMonth
    decode 'c' = doFmt (dateTimeFmt l)
    decode 'C' = read2 >>= \c -> updateYear (\y -> c * 100 + y `rem` 100)
    decode 'd' = read2 >>= \day -> if day > 31 || day < 1
                                       then fail $ "Invalid day " ++ (show day)
                                       else setDay day
    decode 'D' = doFmt "%m/%d/%y"
    decode 'e' = read2 >>= setDay
    decode 'h' = decode 'b'
    decode 'H' = read2 >>= setHour
    decode 'I' = read2 >>= setHour12
    decode 'j' = read3 >>= setYDay
    decode 'k' = read2 >>= setHour
    decode 'l' = read2 >>= setHour12
    decode 'm' = read2 >>= \mon -> if (mon-1) > fromEnum (maxBound :: Month)
                                      then fail $ "Invalid month " ++ (show mon)
                                      else setMonth (toEnum (mon-1))
    decode 'M' = read2 >>= setMin
    -- FIXME: strptime(3) accepts "arbitrary whitespace" for %n
    decode 'n' = char '\n' >> return ()
    decode 'p' = do
                 x <- (string am >> return 0) <|> (string pm >> return 12)
                 updateHour (\h -> x + h `rem` 12)
        where (am,pm) = amPm l
    decode 'r' = doFmt (time12Fmt l)
    decode 'R' = doFmt "%H:%M"
    -- FIXME: implement %s.
    -- FIXME: implement %s in formatCalendarTime
    decode 's' = int >> return ()
    decode 'S' = read2 >>= setSec
    -- FIXME: strptime(3) accepts "arbitrary whitespace" for %t
    decode 't' = char '\t' >> return ()
    decode 'T' = doFmt "%H:%M:%S"
    decode 'u' = readN 1 >>= setWDay . toEnum . (\w -> if w == 7 then 0 else w)
    -- FIXME: implement %U.
    decode 'U' = read2 >> return ()
    -- FIXME: implement %V.
    decode 'V' = read2 >> return ()
    decode 'w' = readN 1 >>= setWDay . toEnum
    -- FIXME: implement %W.
    decode 'W' = read2 >> return ()
    decode 'x' = doFmt (dateFmt l)
    decode 'X' = doFmt (timeFmt l)
    -- FIXME: should probably be zero padded,
    --        need to change formatCalendarTime too
    decode 'Y' = int >>= setYear
    -- FIXME: maybe 04 should be 2004, not 1904?
    decode 'y' = read2 >>= \c -> updateYear (\y -> (y `quot` 100) * 100 + c)
    -- FIXME: are timezone names always [A-Z]+ ?
    -- FIXME: set ctTZ when parsing timezone name and
    --        ctTZName when parsing offset
    decode 'Z' = tzname <|> tzoffset
        where tzname = many1 (oneOf ['A'..'Z']) >>= setTZName
              tzoffset = do
                         s <- sign
                         h <- read2
                         m <- read2
                         setTZ (s * (h * 3600 + m * 60))
    -- following the example of strptime(3),
    -- whitespace matches zero or more whitespace
    -- characters in the input string
    decode c | isSpace c = spaces >> return ()
    decode c = char c >> return ()

epoch :: CalendarTime
epoch = CalendarTime {
                      ctYear = 1970,
                      ctMonth = January,
                      ctDay = 1,
                      ctHour = 0,
                      ctMin = 0,
                      ctSec = 0,
                      ctPicosec = 0,
                      ctWDay = Thursday,
                      ctYDay = 1,
                      ctTZName = "UTC",
                      ctTZ = 0,
                      ctIsDST = False
                     }

parseEnum :: Enum a => [String] -> CharParser st a
parseEnum ss = choice (zipWith tryString ss (enumFrom (toEnum 0)))
    where tryString s x = try (string s) >> return x

setYear,setDay,setHour,setHour12,setMin,setSec,setYDay,setTZ :: Int -> GenParser tok CalendarTime ()
setYear x   = updateState (\t -> t{ ctYear   = x })
setMonth :: Month -> GenParser tok CalendarTime ()
setMonth x  = updateState (\t -> t{ ctMonth  = x })
setDay x    = updateState (\t -> t{ ctDay    = x })
setHour x   = updateState (\t -> t{ ctHour   = x })
setMin x    = updateState (\t -> t{ ctMin    = x })
setSec x    = updateState (\t -> t{ ctSec    = x })
setWDay :: Day -> GenParser tok CalendarTime ()
setWDay x   = updateState (\t -> t{ ctWDay   = x })
setYDay x   = updateState (\t -> t{ ctYDay   = x })
setTZName :: String -> GenParser tok CalendarTime ()
setTZName x = updateState (\t -> t{ ctTZName = x })
setTZ x     = updateState (\t -> t{ ctTZ     = x })

updateYear :: (Int -> Int) -> GenParser tok CalendarTime ()
updateYear f = updateState (\t -> t{ ctYear = f (ctYear t) })
updateHour :: (Int -> Int) -> GenParser tok CalendarTime ()
updateHour f = updateState (\t -> t{ ctHour = f (ctHour t) })

setHour12 x = updateHour (\h -> (h `quot` 12) * 12 + from12 x)
    where from12 h = if h == 12 then 0 else h

read2, read3 :: GenParser Char st Int
read2 = readN 2
read3 = readN 3

-- | Read up to a given number of digits, optionally left-padded
--   with whitespace and interpret them as an 'Int'.
readN :: Int -> GenParser Char st Int
readN n =
    liftM read (spaces >> choice [try (count m digit) | m <- [n,n-1..1]])

int :: GenParser Char st Int
int = liftM read (many1 digit)

sign :: GenParser Char st Int
sign = (char '+' >> return 1) <|> (char '-' >> return (-1))
