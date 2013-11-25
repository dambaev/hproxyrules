{-# LANGUAGE BangPatterns #-}
module Data.HProxy.Rules
    where

import Data.Maybe
import Data.List
import Data.Bits
import System.IO
import Control.Monad
import System.Directory
import Text.ParserCombinators.Parsec
import Network.AD.SID

{- Containes either error with line number or list with line and rule
 - in it
 -}
data Rules = Rules FileName (Either (Int,ParseError) [(Int, Rule)])
    deriving (Show)

type FileName = String

type NetT = String
type IP = String
type MaskT = Int

data RulePermission = RuleAllow -- permit connection
                    | RuleDeny -- deny connection
                    | RuleAllowNotify -- permit, but notify
                    | RuleDenyNotify -- deny and notify
    deriving (Eq,Show)

data Rule = Rule
    { rulePermission:: RulePermission -- must be some permission
    , ruleSIDs:: (Maybe SIDs) -- rule may contain SIDs filter
    , ruleDestinations:: (Maybe Destinations) -- ... destinations filter
    , ruleDates::(Maybe Dates) -- dates filter
    , ruleTimes:: (Maybe Times) -- time filter
    }
          | Comment -- or may be comment
    deriving (Eq,Show)




{-
 - destination may be: Host (including host name), network (ip/mask),
 - addressport (host:port)
 -}
data Destination = DestinationHost IPAddress
                 | DestinationNet IPAddress MaskT
                 | DestinationAddrPort IPAddress PortT
    deriving (Eq,Show)
type Destinations = [Destination]

{-
 - checks either addr2 containes addr1
 -}
isAddrPortExistsInDests:: Destination-> Destination-> Bool
isAddrPortExistsInDests addr1@(DestinationAddrPort ip1 port1) 
    addr2@(DestinationAddrPort ip2 port2) = addr1 == addr2
isAddrPortExistsInDests addr1@(DestinationAddrPort ip1 port1) 
    addr2@(DestinationHost ip2) = ip1 == ip2
isAddrPortExistsInDests addr1@(DestinationAddrPort ip1 port1) 
    addr2@(DestinationNet ip2 mask2) = isNetContainesIP addr2 ip1
isAddrPortExistsInDests _ _ = False

{- calculates containment of addr in net by formula 
 - (net .&. mask) == ( addr .&. mask)
 - addr must be IPv4
 -}
isNetContainesIP:: Destination-> IPAddress-> Bool
isNetContainesIP (DestinationNet netip netmask) ip1 
    | netmask == 32 = netip == ip1
    | netmask == 0 = True
    | otherwise = 
        let enetrawip = ip4ToInt netip
            eiprawip = ip4ToInt ip1
            mask:: Int
            mask = genMask netmask
        in case enetrawip of
                Left _ -> False
                Right netrawip ->
                    case eiprawip of
                        Left _ -> False
                        Right rawip ->
                            (netrawip .&. mask) == ( rawip .&. mask)

{-
 - generates network mask by specified bits count
 -}
genMask bits = genMask' bits 0
    where
        genMask' 0 tmp = tmp
        genMask' bits tmp = genMask' (bits - 1) (setBit tmp (32 - bits))

{- 
 - convert string IPv4 to 32-bit Int
 -}
ip4ToInt:: IPAddress -> Either ParseError Int
ip4ToInt (IPAddress ip) = parse parseIntFromIP4 "unknown" ip
    where
        parseIntFromIP4:: GenParser Char st Int
        parseIntFromIP4 = do
            _b1 <- many1 digit
            char '.'
            _b2 <- many1 digit
            char '.'
            _b3 <- many1 digit
            char '.'
            _b4 <- many1 digit
            let b1 = read _b1
                b2 = read _b2
                b3 = read _b3
                b4 = read _b4
                ret = (b1 `shiftL` 24 ) .|. ( b2 `shiftL` 16) .|. 
                    ( b3 `shiftL` 8) .|. b4
            return $! ret
    
{-
 - represents IP address. May be ipv4 or host name
 -}
data IPAddress = IPAddress IPT
    deriving (Eq,Show)
type IPT = String

{- 
 - IP port [0-65535]
 -}
data Port = Port PortT
    deriving (Eq,Show)
type PortT = Int


{-
 - represent dates: day of week, range days of week, date range, one day
 -}
data Date = DateDayOfWeek Int
          | DateDaysOfWeek Int Int
          | DateRange DateYYYYMMDD DateYYYYMMDD
          | DateDay DateYYYYMMDD
    deriving (Eq,Show)
type Dates = [Date]

{-
 - checks if date1 is beetwin date2 range or the same as date2 in case 
 - date2 is one day
 -}
isDateBeetwinDates:: DateYYYYMMDD-> Date -> Bool
isDateBeetwinDates date1 (DateRange date2 date3) = 
    date1 >= date2 && date1 <= date3
isDateBeetwinDates date1 (DateDay date2) = 
    date1 == date2
isDateBeetwinDates _ _ = False

{- 
 - checks if day1 of week is beetwin range of day of week day2 or is it 
 - the same as day2 in case it just one day of week
 -}
isDateOfWeekBeetwinDaysOfWeek day1 (DateDaysOfWeek day2 day3) = 
    day1 >= day2 && day1 <= day3
isDateOfWeekBeetwinDaysOfWeek day1 (DateDayOfWeek day2) = 
    day1 == day2
isDateOfWeekBeetwinDaysOfWeek _ _ = False


data Time = TimeRange TimeHHMM TimeHHMM
    deriving (Eq,Show)
type Times = [Time]  

isTimeBeetwinTimes:: TimeHHMM-> Time-> Bool
isTimeBeetwinTimes time1 (TimeRange time2 time3) = 
    time1 >= time2 && time1 <= time3



{-
 - represents time (HH:MM)
 -}
data TimeHHMM = TimeHHMM Int Int
    deriving (Eq,Show)

instance Ord TimeHHMM where
    (TimeHHMM h1 m1) > (TimeHHMM h2 m2) 
        | h1 < h2 = False
        | h1 > h2 = True
        | m1 < m2 = False
        | m1 > m2 = True
        | otherwise = False
    time1 < time2 | time1 /= time2 = not $! time1 > time2
                  | otherwise = False
    time1 >= time2 = time1 > time2 || time1 == time2
    time1 <= time2 = time1 < time2 || time1 == time2
    
    
{-
 - represents date (YYYY.MM.DD)
 -}
data DateYYYYMMDD = DateYYYYMMDD Int Int Int
    deriving (Eq, Show)

instance Ord DateYYYYMMDD where
    (DateYYYYMMDD y1 m1 d1) > (DateYYYYMMDD y2 m2 d2) 
        | y1 < y2 = False
        | y1 > y2 = True
        | m1 < m2 = False
        | m1 > m2 = True
        | d1 < d2 = False
        | d1 > d1 = True
        | otherwise = False
    date1 < date2 | date1 == date2 = False
                  | otherwise = not $! date1 > date2
    date1 >= date2 = date1 > date2 || date1 == date2
    date1 <= date2 = date1 < date2 || date1 == date2




parseRuleLine:: String -> Either ParseError Rule
parseRuleLine str = parse parseRuleOrComment "(unknown)" str

parseRuleOrComment:: GenParser Char st Rule
parseRuleOrComment = do
    try parseComment <|> try parseRule <|> do
        spaces
        eof
        return Comment

parseComment:: GenParser Char st Rule
parseComment = do
    spaces 
    char '#'
    many anyChar
    return Comment

parsePermission:: GenParser Char st RulePermission
parsePermission = do
    let allow_ = do
            spaces
            string "allow"
            return RuleAllow
        deny_ = do
            spaces
            string "deny"
            return RuleDeny
        allowNotify_ = do
            spaces
            string "allowNotify"
            return RuleAllowNotify
        denyNotify_ = do
            spaces
            string "denyNotify"
            return RuleDenyNotify        
    allowNotify_ <|> allow_  <|>  denyNotify_ <|> deny_

parseSIDs:: GenParser Char st SIDs
parseSIDs = do
    spaces
    string "sids"
    spaces
    char '['
    sids <- many1 $! try parseSID
    spaces
    char ']'
    return sids

parseSID:: GenParser Char st SID
parseSID = do
    let sidUser = do
            spaces
            string "user"
            spaces
            ret <- many1 alphaNum
            return $! SIDUser ret
        sidGroup = do
            spaces
            string "group"
            spaces
            many1 alphaNum >>= return . SIDGroup
    optional $! try (spaces >> char ',')
    try sidUser <|> try sidGroup

parseDestinations:: GenParser Char st Destinations
parseDestinations = do
    spaces
    string "dests"
    spaces
    char '['
    dests <- many1 $! try parseDestination
    spaces
    char ']'
    return dests

parseDestination:: GenParser Char st  Destination
parseDestination = do
    optional $! try (spaces >> char ',')
    try parseHost <|> try parseNet <|> try parseAddrPort

parseHost:: GenParser Char st Destination
parseHost = do
    spaces
    string "host"
    spaces
    ip <- parseIPAddress
    return $! DestinationHost ip

parseNet:: GenParser Char st Destination
parseNet = do
    spaces
    string "net"
    ip <- parseIPAddress
    char '/'
    mask <- count 2 digit
    return $! DestinationNet ip (read mask)

parseIPAddress :: GenParser Char st IPAddress
parseIPAddress = do
    spaces
    ret <- many $! alphaNum <|> char '.' 
    return $! IPAddress ret
    

parseAddrPort:: GenParser Char st Destination
parseAddrPort = do
    spaces
    string "addr"
    ip <- parseIPAddress
    char ':'
    port <- many1 digit
    let iport = read port
    if iport < 0 || iport > 65535 
        then unexpected "port must be >0 and <65536"
        else return $! DestinationAddrPort ip (read port)


parseDates:: GenParser Char st Dates
parseDates = do
    spaces 
    string "dates"
    spaces
    char '['
    dates <- many1 $! do
         optional $! try (spaces >> char ',')
         parseDate
    spaces
    char ']'
    return dates

parseDate:: GenParser Char st Date
parseDate = do
    try parseDateRange <|> try parseWeekDay <|> try parseWeekDays <|> try parseDay
    
parseDay:: GenParser Char st Date
parseDay = do
    spaces
    string "day"
    date <- parseYYYYMMDD
    return $! DateDay date
    
parseWeekDay:: GenParser Char st Date
parseWeekDay = do
    spaces
    string "weekDay"
    spaces
    day <- count 1 digit
    return $! DateDayOfWeek (read day)


parseWeekDays:: GenParser Char st Date
parseWeekDays = do
    spaces
    string "weekDays"
    spaces
    from <- count 1 digit
    spaces
    char '-'
    spaces
    to <- count 1 digit
    return $! DateDaysOfWeek (read from) (read to)

parseYYYYMMDD:: GenParser Char st DateYYYYMMDD
parseYYYYMMDD = do
    spaces
    y <- count 4 digit
    char '.'
    m <- count 2 digit
    char '.'
    d <- count 2 digit
    return $! DateYYYYMMDD (read y) (read m) (read d)

parseDateRange:: GenParser Char st Date
parseDateRange = do
    spaces
    string "range"
    spaces
    from <- parseYYYYMMDD
    spaces
    char '-'
    spaces
    to <- parseYYYYMMDD
    return $! DateRange from to

parseTimes:: GenParser Char st Times
parseTimes = do
    spaces 
    string "times"
    spaces
    char '['
    dates <- many1 $! do
         optional $! try (spaces >> char ',')
         try parseTime
    spaces
    char ']'
    return dates

parseTime:: GenParser Char st Time
parseTime = do
    from <- parseTimeHHMM
    spaces
    char '-'
    spaces
    to <- parseTimeHHMM
    return $! TimeRange from to

parseTimeHHMM:: GenParser Char st TimeHHMM
parseTimeHHMM = do
    spaces
    h <- count 2 digit
    char ':'
    m <- count 2 digit
    return $! TimeHHMM (read h) (read m)

parseRule:: GenParser Char st Rule
parseRule = do
    permission <- parsePermission
    sids <- optionMaybe $! try parseSIDs
    dests <- optionMaybe $! try parseDestinations
    dates <- optionMaybe $! try parseDates
    times <- optionMaybe $! try parseTimes
    return $! Rule permission sids dests dates times
    

parseRuleFile:: FileName-> IO Rules
parseRuleFile fname = do
    contents <- readFile fname
    case length contents of
        _ -> return $! Rules fname $!
            let fileLines = lines contents
                foo:: Either (Int, ParseError) [(Int, Rule)]-> String-> Either (Int,ParseError) [(Int, Rule)]
                foo err@(Left some) _ = err
                foo (Right []) str = 
                    case parseRuleLine str of 
                        Left e -> Left (1, e)
                        Right rule -> Right [(1, rule)]
                foo  (Right rules@((line, _):_)) str = 
                    case parseRuleLine str of 
                        Left e -> Left (line + 1, e)
                        Right rule -> Right ((line + 1, rule ):rules)
                filterComment:: (Int,Rule) -> Bool
                filterComment (_, Comment)  = False
                filterComment _ = True
            in case foldl foo (Right []) fileLines of
                Left e -> Left e
                Right some ->  Right $! reverse $! filter filterComment some

-- allow sids [user SID1] dests [ net 192.168.11.30/30 ] dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]
-- deny sids [user SID1] dests [ net 192.168.11.30/30 ] time [ datetime 2013.11.10 00:00]


{-- 
 - parse files in given directory. Files processed only with suffix
 - ".rule"
 -}
parseRuleDir:: String-> IO [Rules]
parseRuleDir dirname = do
    !contents <- getDirectoryContents dirname
    !sorted <- filterM filterfoo $! reverse $! sort $! contents
    foldM doParseFiles [] sorted 
        where
        filterfoo x | ".rule" `isSuffixOf` x = return True
                    | otherwise = doesFileExist x
                    
        doParseFiles:: [Rules] -> String-> IO [Rules]
        doParseFiles rules fname = do
            newrules <- parseRuleFile $! dirname ++ "/" ++ fname
            return $! newrules:rules 
    
