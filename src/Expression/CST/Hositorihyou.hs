{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Expression.CST.Hositorihyou
  ( Hositorihyou ()
  ) where

import Numeric.Natural
import Data.List
import Text.ParserCombinators.ReadP

import Expression.CST.Hosi

-- | 星取表
newtype Hositorihyou = Hositorihyou String

-- | 空の星取表
ε :: Hositorihyou
ε = Hositorihyou ""

-- | インデックス (1-origin)
(!) :: Hositorihyou -> Natural -> Hosi
(!) (Hositorihyou hs) = hosi . genericIndex hs . pred

-- | 星取表の長さ
len :: Hositorihyou -> Natural
len (Hositorihyou hs) = length hs

-- | 星取表の相等性
instance Eq Hositorihyou where
  Hositorihyou hs == Hositorihyou hs'
    = hs == hs'

-- | Show
instance Show Hositorihyou where
  showsPrec _ = \ case
    Hositorihyou hs -> (hs ++)

-- | Read
instance Read Hositorihyou where
  readsPrec _ = readP_to_S rHositorihyou

rHositorihyou :: ReadP Hositorihyou
rHositorihyou = Hositorihyou <$> munch (`elem` "●○")

-- | 星取表の連接
(·) :: Hositorihyou -> Hositorihyou -> Hositorihyou
Hositorihyou hs · Hositorihyou hs' = Hositorihyou (hs ++ hs')
