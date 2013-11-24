{-# LANGUAGE BangPatterns #-}
module Data.HProxy.Session where


import Data.HProxy.Rules
import Network.AD.SID

data ProxySession = ProxySession 
    { sessionUserSID:: SID
    , sessionGroupsSIDs:: SIDs
    , sessionDate:: DateYYYYMMDD
    , sessionWeekDay:: Int
    , sessionTime:: TimeHHMM
    , sessionDestination:: Destination
    }
    deriving (Eq, Show)

data MatchHelper = MatchHelper
    { matchedByUserSID:: Maybe Bool
    , matchedByGroupSID:: Maybe Bool
    , matchedByDate:: Maybe Bool
    , matchedByWeekDay:: Maybe Bool
    , matchedByTime:: Maybe Bool
    , matchedByDestination:: Maybe Bool
    }
    deriving (Eq, Show)

emptyHelper = MatchHelper
    { matchedByUserSID = Nothing
    , matchedByGroupSID = Nothing
    , matchedByDate = Nothing
    , matchedByWeekDay = Nothing
    , matchedByTime = Nothing
    , matchedByDestination = Nothing
    }

isMatched:: MatchHelper -> Bool
-- SIDs
isMatched (MatchHelper{ matchedByUserSID = Just False
                      , matchedByGroupSID = Just False}) = False
-- dests
isMatched (MatchHelper{ matchedByDestination = Just False}) = False
-- dates
isMatched (MatchHelper{ matchedByDate = Just False
                      , matchedByWeekDay = Just False }) = False
-- times
isMatched (MatchHelper{ matchedByTime = Just False}) = False
isMatched _ = True

matchSessionRules::ProxySession -> [Rules]-> Maybe (String, Int, Rule)
matchSessionRules session !rulesFiles = 
    case matchSessionRulesHelper session rulesFiles of
        [] -> Nothing
        some:_ -> Just some
    



matchSessionRulesHelper:: ProxySession -> [Rules]-> [(String, Int, Rule)]-- [(String, [(Int, MatchHelper)])]
matchSessionRulesHelper session !rulesFiles = 
    let filterFoo:: Rules -> Bool
        filterFoo (Rules !fname (Left _)) = False
        filterFoo (Rules !fname (Right !rules)) = any (\(!line, !rule)-> 
                isMatched $! matchSessionRule session rule) rules
        mapFoo:: Rules -> (String, Int, Rule)
        mapFoo (Rules !fname (Right !rules)) = 
            let (!line, !rule) = head $! filter (\(_, x) -> isMatched $! matchSessionRule session x) rules
            in (fname, line, rule)
    in map mapFoo $! filter filterFoo rulesFiles



matchSessionRule:: ProxySession-> Rule-> MatchHelper
matchSessionRule session rule = matchSessionRule' emptyHelper session rule 

matchSessionRule':: MatchHelper-> ProxySession-> Rule-> MatchHelper
-- user SIDs
matchSessionRule' 
    !helper@(MatchHelper{ matchedByUserSID = Nothing})
    !session@(ProxySession{ sessionUserSID = userSID})
    !rule@(Rule{ ruleSIDs = Nothing} ) = 
        let !newhelper = helper{matchedByUserSID = Just True}
        in matchSessionRule' newhelper session rule
matchSessionRule' 
    !helper@(MatchHelper{ matchedByUserSID = Nothing})
    !session@(ProxySession{ sessionUserSID = userSID})
    !rule@(Rule{ ruleSIDs = Just !rulesids} ) = 
        let !newhelper = helper{matchedByUserSID = Just exist}
            !exist = userSID `elem` rulesids
        in matchSessionRule' newhelper session rule

-- groups' SIDs
matchSessionRule' 
    !helper@(MatchHelper{ matchedByGroupSID = Nothing})
    !session@(ProxySession{ sessionGroupsSIDs = groupSIDs})
    !rule@(Rule{ ruleSIDs = Just rulesids} ) = 
        let !newhelper = helper{matchedByGroupSID = Just exist}
            !exist = case [ x | x <- rulesids, y <- groupSIDs, x == y] of
                [] -> False
                _ -> True
        in matchSessionRule' newhelper session rule
matchSessionRule' 
    !helper@(MatchHelper{ matchedByGroupSID = Nothing})
    !session@(ProxySession{ sessionGroupsSIDs = groupSIDs})
    !rule@(Rule{ ruleSIDs = Nothing} ) = 
        let !newhelper = helper{matchedByGroupSID = Just True}
        in matchSessionRule' newhelper session rule

-- date
matchSessionRule' 
    !helper@(MatchHelper{ matchedByDate = Nothing})
    !session@(ProxySession{ sessionDate = sesDate})
    !rule@(Rule{ ruleDates = Nothing} ) = 
        let !newhelper = helper{matchedByDate = Just True}
        in matchSessionRule' newhelper session rule
matchSessionRule' 
    !helper@(MatchHelper{ matchedByDate = Nothing})
    !session@(ProxySession{ sessionDate = !sesDate})
    !rule@(Rule{ ruleDates = Just !_ruleDates} ) = 
        let !newhelper = helper{matchedByDate = Just beetwin}
            !beetwin = any (\date-> sesDate `isDateBeetwinDates` date) _ruleDates
        in matchSessionRule' newhelper session rule

-- dayOfWeek
matchSessionRule' 
    !helper@(MatchHelper{ matchedByWeekDay = Nothing})
    !session@(ProxySession{ sessionWeekDay = sesWeekDay})
    !rule@(Rule{ ruleDates = Nothing} ) = 
        let !newhelper = helper{matchedByWeekDay = Just True}
        in matchSessionRule' newhelper session rule
matchSessionRule' 
    !helper@(MatchHelper{ matchedByWeekDay = Nothing})
    !session@(ProxySession{ sessionWeekDay = sesWeekDay})
    !rule@(Rule{ ruleDates = Just _ruleDates} ) = 
        let !newhelper = helper{matchedByWeekDay = Just beetwin}
            !beetwin = any (\date -> sesWeekDay `isDateOfWeekBeetwinDaysOfWeek` date) _ruleDates
        in matchSessionRule' newhelper session rule

-- time
matchSessionRule' 
    !helper@(MatchHelper{ matchedByTime = Nothing})
    !session@(ProxySession{ sessionTime = sesTime})
    !rule@(Rule{ ruleTimes = Nothing} ) = 
        let !newhelper = helper{matchedByTime = Just True}
        in matchSessionRule' newhelper session rule
matchSessionRule' 
    !helper@(MatchHelper{ matchedByTime = Nothing})
    !session@(ProxySession{ sessionTime = sesTime})
    !rule@(Rule{ ruleTimes = Just _ruleTimes} ) = 
        let !newhelper = helper{matchedByTime = Just beetwin}
            !beetwin = any (\time-> sesTime `isTimeBeetwinTimes` time) _ruleTimes
        in matchSessionRule' newhelper session rule

-- destination
matchSessionRule' 
    !helper@(MatchHelper{ matchedByDestination = Nothing})
    !session@(ProxySession{ sessionDestination = sesDest})
    !rule@(Rule{ ruleDestinations = Nothing} ) = 
        let !newhelper = helper{matchedByDestination = Just True}
        in matchSessionRule' newhelper session rule
matchSessionRule' 
    !helper@(MatchHelper{ matchedByDestination = Nothing})
    !session@(ProxySession{ sessionDestination = sesDest})
    !rule@(Rule{ ruleDestinations = Just _ruleDests} ) = 
        let !newhelper = helper{matchedByDestination = Just beetwin}
            !beetwin = any (\dest -> sesDest `isAddrPortExistsInDests` dest) _ruleDests
        in matchSessionRule' newhelper session rule

-- after checks
matchSessionRule'  !helper !session !rule = helper
