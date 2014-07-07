{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Time.Lite (
  UTime(..),
  LTime(..),
  -- * Current time
  getCurrentUTime,
  ClockId(..),
  getClockUTime,
  ) where

import Control.Lens
import Data.AffineSpace
import Data.Function
import Data.Time.Lite.Nano
import Foreign.Safe
import Foreign.C

type DTime = Nano


-- TODO(klao): Show

-- | The equivalent of 'UTCTime'.
newtype UTime = UTime Nano deriving (Eq, Ord, Show)

makeWrapped ''UTime

instance AffineSpace UTime where
  type Diff UTime = DTime
  (.-.) = (-) `on` op UTime
  (UTime t) .+^ d = UTime $ t + d

--------------------------------------------------------------------------------
-- gettimeofday

data C'timeval = C'timeval {
  _c'timeval'tv_sec :: {-# UNPACK #-} !CLong,
  _c'timeval'tv_usec :: {-# UNPACK #-} !CLong
  } deriving (Eq,Show)

instance Storable C'timeval where
  sizeOf _ = 2 * sizeOf (undefined :: CLong)
  alignment _ = alignment (undefined :: CLong)
  peek (castPtr -> p) = do
    sec <- peekElemOff p 0
    usec <- peekElemOff p 1
    return $ C'timeval sec usec
  poke (castPtr -> p) (C'timeval sec usec) = do
    pokeElemOff p 0 sec
    pokeByteOff p 1 usec

foreign import ccall unsafe "sys/time.h gettimeofday" c'gettimeofday
  :: Ptr C'timeval -> Ptr () -> IO CInt

getTimeOfDay :: IO C'timeval
getTimeOfDay = alloca $ \ptimeval -> do
  r <- c'gettimeofday ptimeval nullPtr
  if r == 0
    then peek ptimeval
    else fail "c'gettimeofday failed"

--------------------
-- clock_gettime

data C'timespec = C'timespec {
  _c'timespec'tv_sec :: {-# UNPACK #-} !CLong,
  _c'timespec'tv_nsec :: {-# UNPACK #-} !CLong
  } deriving (Eq,Show)

instance Storable C'timespec where
  sizeOf _ = 2 * sizeOf (undefined :: CLong)
  alignment _ = alignment (undefined :: CLong)
  peek (castPtr -> p) = do
    sec <- peekElemOff p 0
    nsec <- peekElemOff p 1
    return $ C'timespec sec nsec
  poke (castPtr -> p) (C'timespec sec nsec) = do
    pokeElemOff p 0 sec
    pokeByteOff p 1 nsec

foreign import ccall unsafe "time.h clock_gettime" c'clock_gettime
  :: CInt -> Ptr C'timespec -> IO CInt

data ClockId
  = CLOCK_REALTIME
  | CLOCK_MONOTONIC
  | CLOCK_PROCESS_CPUTIME_ID
  | CLOCK_THREAD_CPUTIME_ID
  | CLOCK_MONOTONIC_RAW
  | CLOCK_REALTIME_COARSE
  | CLOCK_MONOTONIC_COARSE
  | CLOCK_BOOTTIME
  deriving (Eq, Show, Enum)

getClockId :: ClockId -> CInt
getClockId = fromIntegral . fromEnum

clockGetTime :: CInt -> IO C'timespec
clockGetTime clockId = alloca $ \ptimespec -> do
  r <- c'clock_gettime clockId ptimespec
  if r == 0
    then peek ptimespec
    else fail "c'clock_gettime failed"

--------------------
-- getCurrentTime-like:

getCurrentUTime :: IO UTime
getCurrentUTime = do
  C'timeval sec usec0 <- getTimeOfDay
  -- A whacky way of handling leap seconds
  let usec = max 0 $ min 1999999 usec0
      nsec = if usec < 1000000
             then usec * 1000
             else 999999000 + ((usec - 1000000) `quot` 1000)
  return $ UTime $ Nano $
    fromIntegral sec * 1000000000 + fromIntegral nsec

getClockUTime :: ClockId -> IO DTime
getClockUTime clockId = do
  C'timespec sec nsec0 <- clockGetTime (getClockId clockId)
  -- A whacky way of handling leap seconds
  let nsec = max 0 $ min 999999999 nsec0
  return $ Nano $
    fromIntegral sec * 1000000000 + fromIntegral nsec

--------------------------------------------------------------------------------

-- | The equivalent of 'LocalTime'.
newtype LTime = LTime Nano deriving (Eq, Ord)
