module Main (main) where

import Control.Lens
import Criterion.Main
import Data.Time.Lite
import Data.Time.Lite.Nano
import Data.Time
import Data.Time.Clock.POSIX

benchmarks :: [Benchmark]
benchmarks = [
  bench "getCurrentTime" $ nfIO $ getCurrentTime,
  bench "getCurrentTime'" $ nfIO $ getCurrentTime',
  bench "getPOSIXTime" $ nfIO $ getPOSIXTime,
  bench "getCurrentUTime" $ whnfIO $ getCurrentUTime,
  bench "getClockUTime REALTIME" $ whnfIO $ getClockUTime CLOCK_REALTIME,
  bench "getClockUTime REALTIME_COARSE" $ whnfIO $ getClockUTime CLOCK_REALTIME_COARSE
  ]

getCurrentTime' :: IO UTCTime
getCurrentTime' = do
  UTime ns <- getCurrentUTime
  let (d, dt) = ns `iQuotRem` 86400
  return $ UTCTime (ModifiedJulianDay $ 40587 + d) (dt ^. nanoDiffTime)

main :: IO ()
main = do
  defaultMain benchmarks
