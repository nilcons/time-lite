module Main (main) where

import Criterion.Main
import Data.Time.Lite
import Data.Time
import Data.Time.Clock.POSIX

benchmarks :: [Benchmark]
benchmarks = [
  bench "getCurrentTime" $ nfIO $ getCurrentTime,
  bench "getPOSIXTime" $ nfIO $ getPOSIXTime,
  bench "getCurrentUTime" $ whnfIO $ getCurrentUTime,
  bench "getClockUTime REALTIME" $ whnfIO $ getClockUTime CLOCK_REALTIME,
  bench "getClockUTime REALTIME_COARSE" $ whnfIO $ getClockUTime CLOCK_REALTIME_COARSE
  ]

main :: IO ()
main = do
  defaultMain benchmarks
