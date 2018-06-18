#!/usr/bin/runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad             (forM_)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Char                 (isDigit)
import Data.Function             (on)
import Data.List                 (isPrefixOf, findIndices, sort, nub, sortBy)
import Data.Time.Calendar
import Text.Printf               (printf)

-- This is a chunk of miles earned on a flight
data Flight = Flight { getDate    :: Day, -- ^ flight date
                       getBase    :: Integer,  -- ^ base miles
                       getBonus   :: Integer,  -- ^ bonus miles
                       getRemark  :: String,   -- ^ remark
                       expDate    :: Day, -- ^ expiry date
                       getID      :: Integer   -- ^ unique ID is necessary
                     }
--
-- Redeeming the used miles is also recorded.
-- Redeeming will use the oldest but not expired miles
-- We use only the base miles and leave the bonus miles intact.
--
            | Redeem { getDate  :: Day, -- ^ redeem date
                       getBase  :: Integer,  -- ^ base miles
                       getID    :: Integer   -- ^ unique ID (really necessary?)
                     }

printFlight :: Flight -> IO ()
printFlight f = case f of
    Flight date base bonus remark expiry i ->
                let date'   = showGregorian date
                    expiry' = showGregorian expiry
                 in printf "[%4d] %s              %8d (%6d) %s (expiry %s)\n"
                        i date' base bonus remark expiry'
    Redeem date base _                     ->
                let date' = showGregorian date
                 in printf "REDEEM %s              %8d\n" date' (-base)
printExpire :: Flight -> IO ()
printExpire f = case f of
    Flight date base bonus _ expiry i -> do
                let date' = showGregorian date
                    expiry' = showGregorian expiry
                if base == 0 && bonus == 0
                  then return ()
                  else printf "EXPIRE %s              %8d (%6d) flight [%d] (%s)\n"
                          expiry' (-base) (-bonus) i date'
    _ -> return ()

addID :: [Flight] -> [Flight]
addID fs' = addID' fs' [] 0
  where
    addID' :: [Flight] -> [Flight] -> Integer -> [Flight]
    addID' []     out _ = out
    addID' (f:fs) out c = case f of
                Flight d ba bo r e _ -> addID' fs ((Flight d ba bo r e (c+1)):out) (c+1)
                Redeem d ba _        -> addID' fs ((Redeem d ba (c+1)):out)        (c+1)

parser :: String -> Flight
parser l = let ww     = words (dropWhile (not . isDigit) l)
               date   = head ww
               ss     = findIndices (== '/') date
               f      = read :: String -> Integer
               f'     = read :: String -> Int
               base   = f $ ww !! 1
               bonus  = f $ ww !! 2
               remark = unwords $ drop 3 ww
            in if length ss /= 2  -- minimum format check
                 then error $ "inconsistent date field: " ++ date
                 else let (d0, r0) = span (/= '/') date
                          r1       = tail r0
                          (m0, r2) = span (/= '/') r1
                          y0       = tail r2
                          now      = fromGregorian (f y0) (f' m0) (f' d0)
                       in if base < 0 -- redeeming
                            then Redeem now (-base) 0
                            else Flight now base bonus remark (expireDate now) 0

-- find last day of month by stepping back one day
-- from the 1st day of the next month
-- "マイルはご搭乗日の 36 か月後の月末まで有効"
expireDate :: Day -> Day
expireDate from = let (y, m, d)  = toGregorian from
                      d'         = gregorianMonthLength y m
                      endOfmonth = fromGregorian y m d'
                   in addGregorianMonthsClip 36 endOfmonth
--
-- walk through the flights with keeping track of the state of flights and miles
--
type MileStatus = ([Flight], Integer, Integer) -- ^ (flights, baseMile, bonusMile)

isAlive :: Day -> Flight -> Bool
isAlive _     (Redeem _ _ _)                          = False
isAlive today (Flight date base _ _ expiry _) = date < today && today < expiry
                                                        && base > 0
countMiles :: Day -> StateT MileStatus IO ()
countMiles today = do
    (flights, base, bonus) <- get

    let earnToday = filter (\f -> case f of
                                    Flight d _ _ _ _ _ -> d == today
                                    Redeem _ _ _       -> False) flights
        expireToday = filter (\f -> case f of
                                    Flight _ _ _ _ e _ -> e == today
                                    Redeem _ _ _       -> False) flights
        redeemToday = filter (\f -> case f of
                                      Flight _ _ _ _ _ _ -> False
                                      Redeem d _ _       -> d == today) flights
 
    -- print out today's events
    mapM_ (lift . printFlight) (earnToday ++ redeemToday)
    mapM_ (lift . printExpire) expireToday

    -- update mileage
    let earned  = foldr (\f (a,o) -> (getBase f + a, getBonus f + o)) (0,0) earnToday
        expired = foldr (\f (a,o) -> (getBase f + a, getBonus f + o)) (0,0) expireToday
        base'   = base + fst earned - fst expired
        bonus'  = bonus + snd earned - snd expired

    put (flights, base', bonus')

    -- redeem if necessary
    forM_ redeemToday (\(Redeem _ toRedeem _) -> do
        (flights0, base0, bonus0) <- get

        let redeemable   = sortBy (compare `on` getDate) $ filter (isAlive today) flights0
            unredeemable = filter (not . isAlive today) flights0
            redeemed     = redeem redeemable [] toRedeem
            flights1     = redeemed ++ unredeemable
            base1        = base0 - toRedeem

            redeem :: [Flight] -- ^ redeemable
                   -> [Flight] -- ^ already redeemed so far
                   -> Integer  -- ^ miles to redeem
                   -> [Flight] -- ^ already redeemed
            redeem [] done _ = done -- Run out of earned mileage if residue > 0.
                                    -- In actuality, the deficit is paid from bonus or
                                    -- other non-work-related mileage.
            redeem (f:fs) done residue
                | getBase f < residue
                        = redeem fs ((f {getBase = 0}):done) (residue - getBase f)
                | otherwise -- getBase f >= residue
                        = (f {getBase = getBase f - residue}):(fs ++ done)
        put (flights1, base1, bonus0)
        )

    (_, base'', bonus'') <- get
    lift $ printf "               %8d (%6d)\n" base'' bonus''


main :: IO ()
main = do
    c <- readFile "WorkMileageCounter.hs"
    let flights' = map parser . filter (isPrefixOf "---") . lines $ c
        flights = addID flights'
        dates   = sort $ nub $ concat $ map (\f -> case f of
                                             Flight d _ _ _ e _ -> [d, e]
                                             Redeem d _ _       -> [d]) flights

    _ <- runStateT (mapM_ countMiles dates) (flights, 0, 0)
    return ()

-- date         mile    bonus   
---16/05/2009   5882    588     Narita - Papeete
---26/06/2009    262    0       Hobart - Melbourne
---26/06/2009    319    0       Melbourne - Sydney
---26/06/2009   2432    0       Sydney - Narita
---06/07/2009    510    51      Haneda - Chitose
---08/07/2009    510    51      Chitose - Haneda
---19/06/2010   3404    736     Narita - Sydney
---25/06/2010   3404    736     Sydney - Narita
---06/08/2010   -10000  0       Coupon issued
---20/08/2010    425    57      Haneda - Fukuoka
---22/08/2010    567    57      Fukuoka - Haneda
