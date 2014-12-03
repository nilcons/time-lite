module Main (main) where

import Control.Applicative
import Control.Lens
import Criterion.Main
import Data.Time.Lite
import Data.Time.Lite.Nano
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Thyme as Th
import qualified Data.Thyme.Time as Th

import System.Hourglass

benchmarks :: [Benchmark]
benchmarks = [
  -- Data.Time
  bench "getCurrentTime" $ nfIO $ getCurrentTime,
  bench "getPOSIXTime" $ nfIO $ getPOSIXTime,
  -- Hourglass
  bench "timeCurrentP" $ whnfIO timeCurrentP,
  -- Data.Thyme
  bench "Thyme.getCurrentTime" $ whnfIO $ Th.getCurrentTime,
  bench "Thyme.getCurrentTime compat" $ nfIO $ getCurrentTimeThyme,
  -- Ours
  bench "getCurrentTime'" $ nfIO $ getCurrentTime',
  bench "getCurrentUTime" $ whnfIO $ getCurrentUTime,
  bench "getClockUTime REALTIME" $ whnfIO $ getClockUTime CLOCK_REALTIME,
  bench "getClockUTime REALTIME_COARSE" $ whnfIO $ getClockUTime CLOCK_REALTIME_COARSE
  ]

getCurrentTime' :: IO UTCTime
getCurrentTime' = do
  UTime ns <- getCurrentUTime
  let (d, dt) = ns `iQuotRem` 86400
  return $ UTCTime (ModifiedJulianDay $ 40587 + d) (dt ^. nanoDiffTime)

getCurrentTimeThyme :: IO UTCTime
getCurrentTimeThyme = Th.fromThyme <$> Th.getCurrentTime

main :: IO ()
main = do
  defaultMain benchmarks
