{-# LANGUAGE BangPatterns #-}
module Data.HProxy.Rules
    ( parseDir
    , parseFile
    , Rules
    ) where

import Data.Maybe
import Data.List
import System.IO
import Control.Monad
import System.Directory
import Text.ParserCombinators.Parsec


data Rules = Rules FileName (Either (Int,ParseError) [(Int, Rule)])
    deriving (Show)

type FileName = String
type SIDT = String

type NetT = String
type IP = String
type MaskT = Int

data RulePermission = RuleAllow
                    | RuleDeny
    deriving (Eq,Show)

data Rule = Rule RulePermission (Maybe SIDs) (Maybe Destinations) (Maybe Dates) (Maybe Times)
          | Comment
    deriving (Eq,Show)



data SID = SIDUser SIDT
         | SIDGroup SIDT
    deriving (Eq,Show)
type SIDs = [SID]


data Destination = DestinationHost IPAddress
                 | DestinationNet IPAddress MaskT
                 | DestinationAddrPort IPAddress PortT
    deriving (Eq,Show)
type Destinations = [Destination]
           
   

data Host = Host IP
    deriving (Eq,Show)
data Mask = Mask MaskT
    deriving (Eq,Show)

data IPAddress = IPAddress IPT
    deriving (Eq,Show)
type IPT = String

data Port = Port PortT
    deriving (Eq,Show)
type PortT = Int

data Date = DateDayOfWeek Int
          | DateDaysOfWeek Int Int
          | DateRange DateYYYYMMDD DateYYYYMMDD
          | DateDay DateYYYYMMDD
    deriving (Eq,Show)
type Dates = [Date]

data Time = TimeRange TimeHHMM TimeHHMM
    deriving (Eq,Show)
type Times = [Time]  

data TimeHHMM = TimeHHMM Int Int
    deriving (Eq,Show)


data DateYYYYMMDD = DateYYYYMMDD Int Int Int
    deriving (Eq, Show)





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
    allow_ <|> deny_

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
    return $! DestinationAddrPort ip (read port)


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
    

parseFile:: FileName-> IO Rules
parseFile fname = do
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
            in case foldl foo (Right []) fileLines of
                Left e -> Left e
                Right some ->  Right $! reverse some

-- allow sids [user SID1] dests [ net 192.168.11.30/30 ] dates [range 2013.11.01 - 2014.01.01, weekDay 5, weekDays 0-4, day 2013.11.10 ] times [ 08:30 - 21:00 ]
-- deny sids [user SID1] dests [ net 192.168.11.30/30 ] time [ datetime 2013.11.10 00:00]


{-- 
 - parse files in given directory. Files processed only with suffix
 - ".rule"
 -}
parseDir:: String-> IO [Rules]
parseDir dirname = do
    !contents <- getDirectoryContents dirname
    !sorted <- filterM filterfoo $! reverse $! sort $! contents
    foldM doParseFiles [] sorted 
        where
        filterfoo x | ".rule" `isInfixOf` x = return True
                    | otherwise = doesFileExist x
                    
        doParseFiles:: [Rules] -> String-> IO [Rules]
        doParseFiles rules fname = do
            newrules <- parseFile $! dirname ++ "/" ++ fname
            return $! newrules:rules 
    
    

main = do
    ret <- parseDir "rules"
    print ret
    return ()

