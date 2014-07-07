-- We don't want to simply use Nano from Data.Fixed, because that's
-- backed by an Integer internally, not Int64.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
-- For unboxed vector:
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Time.Lite.Nano
       ( Nano(..)
       , iMul
       , iDiv
       , iMod
       , fNano
       , nanoDouble
       , nanoDiffTime
       , nanoNominalDiffTime
       , nanoPico
       ) where

import Control.DeepSeq
import Control.Lens
import Control.Monad (liftM)
import Data.AdditiveGroup
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Data
import qualified Data.Fixed as F
import Data.Int
import Data.Ratio
import Data.Time.Clock
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Unboxed.Base
import Data.VectorSpace
import Unsafe.Coerce

newtype Nano = Nano { unNano :: Int64 }
             deriving (Eq, Ord, Data, Typeable, Enum, Bounded, NFData)

resolution :: Int64
{-# INLINE resolution #-}
resolution = 1000000000

resolutionHalf :: Int64
{-# INLINE resolutionHalf #-}
resolutionHalf = 500000000

fromFNano :: F.Nano -> Nano
{-# INLINE fromFNano #-}
fromFNano = Nano . fromIntegral . (unsafeCoerce :: F.Nano -> Integer)

toFNano :: Nano -> F.Nano
{-# INLINE toFNano #-}
toFNano = (unsafeCoerce :: Integer -> F.Nano) . toInteger . unNano

-- type-safe, but slower versions:
-- fromFNano :: F.Nano -> Nano
-- {-# INLINE fromFNano #-}
-- fromFNano x = Nano (truncate $ toRational x * toRational resolution)

-- toFNano :: Nano -> F.Nano
-- {-# INLINE toFNano #-}
-- toFNano (Nano x) = fromRational $ toRational x / toRational resolution

fNano :: Iso' Nano F.Nano
{-# INLINE fNano #-}
fNano = iso toFNano fromFNano

nanoDouble :: Iso' Nano Double
{-# INLINE nanoDouble #-}
nanoDouble = iso ((/ fromIntegral resolution) . fromIntegral . unNano)
                 (Nano . round . (* fromIntegral resolution))

nanoNominalDiffTime :: Iso' Nano NominalDiffTime
nanoNominalDiffTime = iso
                      ((unsafeCoerce :: Integer -> NominalDiffTime) . toInteger . (* 1000) . unNano)
                      (Nano . fromIntegral . (`div` 1000) . (unsafeCoerce :: NominalDiffTime -> Integer))

nanoDiffTime :: Iso' Nano DiffTime
nanoDiffTime = iso
               ((unsafeCoerce :: Integer -> DiffTime) . toInteger . (* 1000) . unNano)
               (Nano . fromIntegral . (`div` 1000) . (unsafeCoerce :: DiffTime -> Integer))

nanoPico :: Iso' Nano F.Pico
nanoPico = iso
               ((unsafeCoerce :: Integer -> F.Pico) . toInteger . (* 1000) . unNano)
               (Nano . fromIntegral . (`div` 1000) . (unsafeCoerce :: F.Pico -> Integer))

instance Show Nano where
  {-# INLINE showsPrec #-}
  showsPrec _ (Nano n) = sign . shows i . ('.':) . zeros . shows df
    where
      sign = if n < 0 then ('-':) else id
      (i,df) = abs n `quotRem` resolution
      zeros = (replicate (9-digits) '0' ++)
      digits = df `seq` go 1 10
      go !d !t | t > df = d
               | otherwise = go (d+1) (t*10)

instance Read Nano where
  {-# INLINE readsPrec #-}
  readsPrec p s = readsPrec p s & traverse . _1 %~ fromFNano

instance Num Nano where
  {-# INLINE (+) #-}
  Nano a + Nano b = Nano $ a + b
  {-# INLINE (-) #-}
  Nano a - Nano b = Nano $ a - b
  {-# INLINE (*) #-}
  a * b = a & fNano *~ (b ^. fNano)
  {-# INLINE negate #-}
  negate (Nano a) = Nano $ negate a
  {-# INLINE abs #-}
  abs (Nano a) = Nano $ abs a
  {-# INLINE signum #-}
  signum (Nano a) = Nano $ signum a * resolution
  {-# INLINE fromInteger #-}
  fromInteger i = Nano $ fromInteger i * resolution

instance Real Nano where
  {-# INLINE toRational #-}
  toRational (Nano a) = toInteger a % toInteger resolution

instance Fractional Nano where
  {-# INLINE (/) #-}
  a / b = a & fNano %~ (/ b ^. fNano)
  {-# INLINE recip #-}
  -- recip (Nano x) = Nano $ (resolution * resolution) `div` x
  recip = fNano %~ recip
  {-# INLINE fromRational #-}
  fromRational r = Nano $ round $ r * fromIntegral resolution

instance RealFrac Nano where
  {-# INLINE properFraction #-}
  properFraction (Nano x) = (fromIntegral whole, Nano frac)
    where
      (whole, frac) = x `quotRem` resolution
  {-# INLINE truncate #-}
  truncate (Nano x) = fromIntegral $ x `quot` resolution
  {-# INLINE floor #-}
  floor (Nano x) = fromIntegral $ x `div` resolution
  {-# INLINE round #-}
  round (Nano x) = fromIntegral $ (x + resolutionHalf) `div` resolution
  {-# INLINE ceiling #-}
  ceiling (Nano x) = fromIntegral $ (x + (resolution-1)) `div` resolution

instance AdditiveGroup Nano where
  zeroV = Nano 0
  (^+^) = (+)
  negateV = negate

-- This is not a proper vector space, but a nice notation to multiply
-- a Nano by an Int64.
instance VectorSpace Nano where
  type Scalar Nano = Int64
  a *^ (Nano b) = Nano (a * b)

-- Put and get is using big-endian encoding.  We want little-endian
-- for performance and compatibility on mainstream platforms.
instance Binary Nano where
  {-# INLINE put #-}
  put (Nano x) = putWord64le $ fromIntegral x
  {-# INLINE get #-}
  get = fmap (Nano . fromIntegral) getWord64le

-- ------------------------------
-- Useful, vector-like operations
-- ------------------------------

iMul :: Integral i => i -> Nano -> Nano
{-# INLINE iMul #-}
iMul a (Nano b) = Nano (fromIntegral a * b)

iDiv :: Integral i => Nano -> Nano -> i
{-# INLINE iDiv #-}
iDiv (Nano a) (Nano b) = fromIntegral $ a `div` b

iMod :: Nano -> Nano -> Nano
{-# INLINE iMod #-}
iMod (Nano a) (Nano b) = Nano $ a `mod` b

infixl 7 `iMul`
infixl 7 `iDiv`
infixl 7 `iMod`

-- --------------
-- Unboxed Vector
-- --------------

-- TODO(klao): use dervingUnbox from vector-th-unbox instead!

newtype instance MVector s Nano = MV_Nano (MVector s Int64)
newtype instance Vector    Nano = V_Nano  (Vector    Int64)

instance Unbox Nano

instance GM.MVector MVector Nano where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Nano v) = GM.basicLength v
  basicUnsafeSlice i n (MV_Nano v) = MV_Nano $ GM.basicUnsafeSlice i n v
  basicOverlaps (MV_Nano v1) (MV_Nano v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Nano `liftM` GM.basicUnsafeNew n
  basicUnsafeReplicate n x = MV_Nano `liftM` GM.basicUnsafeReplicate n (unNano x)
  basicUnsafeRead (MV_Nano v) i = Nano `liftM` GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_Nano v) i x = GM.basicUnsafeWrite v i (unNano x)
  basicClear (MV_Nano v) = GM.basicClear v
  basicSet (MV_Nano v) x = GM.basicSet v (unNano x)
  basicUnsafeCopy (MV_Nano v1) (MV_Nano v2) = GM.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Nano v1) (MV_Nano v2) = GM.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Nano v) n = MV_Nano `liftM` GM.basicUnsafeGrow v n

instance G.Vector Vector Nano where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Nano v) = V_Nano `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Nano v) = MV_Nano `liftM` G.basicUnsafeThaw v
  basicLength (V_Nano v) = G.basicLength v
  basicUnsafeSlice i n (V_Nano v) = V_Nano $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Nano v) i = Nano `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Nano mv) (V_Nano v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
