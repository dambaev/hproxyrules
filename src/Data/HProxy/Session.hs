{-# LANGUAGE BangPatterns #-}
module Data.HProxy.Session where


import Data.HProxy.Rules

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

matchSessionRules:: ProxySession -> [Rules]-> [(String, [(Int, MatchHelper)])]
matchSessionRules session !rulesFiles = 
    let foo:: Rules -> ( String, [(Int, MatchHelper)])
        foo (Rules !fname (Left _)) = (fname, [])
        foo (Rules !fname (Right !rules)) = 
            ( fname
            , map (\(!line, !rule)-> 
                (line, matchSessionRule session rule)) rules
            )
    in map foo rulesFiles



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
    !rule@(Rule{ ruleSIDs = Just rulesids} ) = 
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
    !session@(ProxySession{ sessionDate = sesDate})
    !rule@(Rule{ ruleDates = Just _ruleDates} ) = 
        let !newhelper = helper{matchedByDate = Just beetwin}
            !beetwin = case [ x 
                            | x <- _ruleDates
                            , sesDate `isDateBeetwinDates` x
                            ] of
                [] -> False
                _ -> True
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
            !beetwin = case [ x 
                            | x <- _ruleDates
                            , sesWeekDay `isDateOfWeekBeetwinDaysOfWeek` x
                            ] of
                [] -> False
                _ -> True
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
            !beetwin = case [ x 
                            | x <- _ruleTimes
                            , sesTime `isTimeBeetwinTimes` x
                            ] of
                [] -> False
                _ -> True
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
            !beetwin = case [ x 
                            | x <- _ruleDests
                            , sesDest `isAddrPortExistsInDests` x
                            ] of
                [] -> False
                _ -> True
        in matchSessionRule' newhelper session rule

-- after checks
matchSessionRule'  !helper !session !rule = helper
