#!/usr/bin/runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad             (forM_)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as B
import Data.Char                 (isDigit)
import Data.Function             (on)
import Data.List                 (isPrefixOf, findIndices, sort, nub, sortBy)
import Data.UnixTime
import Text.Printf               (printf)

-- This is a chunk of miles earned on a flight
data Flight = Flight { getDate    :: UnixTime, -- ^ flight date
                       getBase    :: Integer,  -- ^ base miles
                       getBonus   :: Integer,  -- ^ bonus miles
                       getRemark  :: String,   -- ^ remark
                       expDate    :: UnixTime, -- ^ expiry date
                       getID      :: Integer   -- ^ unique ID is necessary
                     }
--
-- Redeeming the used miles is also recorded.
-- Redeeming will use the oldest but not expired miles
-- We use only the base miles and leave the bonus miles intact.
--
            | Redeem { getDate  :: UnixTime, -- ^ redeem date
                       getBase  :: Integer,  -- ^ base miles
                       getID    :: Integer   -- ^ unique ID (really necessary?)
                     }

printFlight :: Flight -> IO ()
printFlight f = case f of
    Flight date base bonus remark expiry i -> do
                date' <- formatUnixTime simpleDateFormat date
                expiry' <- formatUnixTime simpleDateFormat expiry
                printf "[%4d] %s              %8d (%6d) %s (expiry %s)\n"
                        i (B.unpack date') base bonus remark (B.unpack expiry')
    Redeem date base _                     -> do
                date' <- formatUnixTime simpleDateFormat date
                printf "REDEEM %s              %8d\n" (B.unpack date') (-base)
printExpire :: Flight -> IO ()
printExpire f = case f of
    Flight date base bonus _ expiry i -> do
                date' <- formatUnixTime simpleDateFormat date
                expiry' <- formatUnixTime simpleDateFormat expiry
                if base == 0 && bonus == 0
                  then return ()
                  else printf "EXPIRE %s              %8d (%6d) flight [%d] (%s)\n"
                          (B.unpack expiry') (-base) (-bonus) i (B.unpack date')
    _ -> return ()

addID :: [Flight] -> [Flight]
addID fs' = addID' fs' [] 0
  where
    addID' :: [Flight] -> [Flight] -> Integer -> [Flight]
    addID' []     out _ = out
    addID' (f:fs) out c = case f of
                Flight d ba bo r e _ -> addID' fs ((Flight d ba bo r e (c+1)):out) (c+1)
                Redeem d ba _        -> addID' fs ((Redeem d ba (c+1)):out)        (c+1)

oneDay :: UnixDiffTime
oneDay = secondsToUnixDiffTime (24 * 60 * 60::Integer)

simpleDateFormat :: Format
simpleDateFormat = "%d/%m/%Y"

parser :: String -> IO Flight
parser l = let ww     = words (dropWhile (not . isDigit) l)
               date   = head ww
               ss     = findIndices (== '/') date
               now    = parseUnixTime simpleDateFormat (B.pack date)
               f      = read :: String -> Integer
               base   = f $ ww !! 1
               bonus  = f $ ww !! 2
               remark = unwords $ drop 3 ww
            in if length ss /= 2  -- minimum format check
                 then error $ "inconsistent date field: " ++ date
                 else if base < 0 -- redeeming
                        then return $ Redeem now (-base) 0
                        else expireDate now >>= \e ->
                             return $ Flight now base bonus remark e 0

-- find last day of month by stepping back one day
-- from the 1st day of the next month
expireDate :: UnixTime -> IO UnixTime
expireDate from = do from' <- formatUnixTime simpleDateFormat from
                     let ww       = B.splitWith (== '/') from'
                         f        = read . B.unpack :: B.ByteString -> Int
                         (m, y)   = (f $ ww !! 1, f $ ww !! 2)
                         (y', m') = if m + 1 >= 13 then (y + 4, 1)
                                                   else (y + 3, m + 1)
                         exp' = parseUnixTime simpleDateFormat $ "01/" `B.append`
                                    (B.pack . show $ m') `B.append` "/" `B.append`
                                    (B.pack . show $ y')
                     return $ addUnixDiffTime exp' ((-1) * oneDay)

--
-- walk through the flights with keeping track of the state of flights and miles
--
type MileStatus = ([Flight], Integer, Integer) -- ^ (flights, baseMile, bonusMile)

isAlive :: UnixTime -> Flight -> Bool
isAlive _     (Redeem _ _ _)                          = False
isAlive today (Flight date base _ _ expiry _) = date < today && today < expiry
                                                        && base > 0
countMiles :: UnixTime -> StateT MileStatus IO ()
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
    flights' <- mapM parser . filter (isPrefixOf "---") . lines $ c

    let flights = addID flights'
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
