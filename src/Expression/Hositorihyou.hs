{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Expression.Hositorihyou where

import Data.List (foldl', unfoldr, genericLength)
import Numeric.Natural
import Text.ParserCombinators.ReadP

import Expression.Encodable

-- | 
-- 星
--
data Hosi
  = Kuro
  | Siro
  deriving (Eq)

-- |
-- 星取表
--
type Hositorihyou = [Hosi]

-- |
-- 空列
--
ε :: Hositorihyou
ε = []

-- |
-- 長さ
--
len :: Hositorihyou -> Natural
len = \ case
  []     -> 0
  _ : xs -> 1 + len xs

-- |
-- 同じ(identical)星取表
--
(≡) :: Hositorihyou -> Hositorihyou -> Bool
[] ≡ []      = True
[] ≡ (_ : _) = False
(_ : _)  ≡ []       = False 
(x : xs) ≡ (y : ys) = x == y && xs ≡ ys    

-- |
-- 連接
-- 
(・) :: Hositorihyou -> Hositorihyou -> Hositorihyou
(・) = (++)

-- |
-- εは連接演算の単位元
--
-- ε ・ α ≡ α ・ ε ≡ α

-- |
-- 連接演算の結合性 (定理 2.1)
--
-- (α ・ β)  ・ γ ≡ α ・ (β ・ γ)
--
-- 証明は α 上の帰納法による
-- α ≡ ε の場合
--    (α ・ β) ・ γ
--    ≡
--    (ε ・ β) ・ γ
--    ≡ { ε は単位元 }
--    β ・ γ
--    ≡ { ε は単位元 }
--    ε ・ (β ・ γ)
--    ≡
--    α ・ (β ・ γ)
--
-- α ≡ x : xs の場合
--    (α ・ β) ・ γ
--    ≡
--    （x : xs ・β) ・ γ
--    ≡ { (・) の定義 }
--    (x : (xs ・ β)) ・ γ
--    ≡ { (・) の定義 }
--    x : ((xs ・ β) ・ γ)
--    ≡ { 帰納法の仮定 }
--    x : (xs ・ (β ・ γ))
--    ≡ { (・) の定義 }
--    x:xs ・ (β ・ γ)
--    ≡
--    α ・ (β ・ γ)

-- |
-- 左簡約法則 (定理 2.2)
--
-- α ・ β ≡ α ・ γ ならば β ≡ γ
--
-- 証明は α 上の帰納法による
--
-- α ≡ ε の場合
--    α ・ β ≡ α ・ γ
--    ⇒
--    ε ・ β ≡ ε ・ γ
--    ⇒ { εは単位元 }
--    β ≡ γ
--
-- α ≡ x : xs の場合
--    α ・ β ≡ α ・ γ
--    ⇒
--    (x : xs) ・ β ≡ (x : xs) ・ γ
--    ⇒ { (・)の定義 }
--    x : (xs ・ β) ≡ x : (xs ・ γ)
--    ⇒ { 星取表上の(≡)の定義 }
--    xs ・ β ≡ xs ・γ
--    ⇒ { 帰納法の仮定 }
--    β ≡ γ

-- 補助

instance Show Hosi where
  showsPrec _ = \ case
    Kuro -> ('●' :)
    Siro -> ('○' :)

instance Read Hosi where
  readsPrec _ = readP_to_S rHosi

rHosi :: ReadP Hosi
rHosi = rKuro +++ rSiro

rKuro :: ReadP Hosi
rKuro = Kuro <$ char '●'

rSiro :: ReadP Hosi
rSiro = Siro <$ char '○'

instance Encodable Hosi Char where
  encoder = Encoder enc dec
    where
      enc = \ case
        Kuro -> '●'
        Siro -> '○'
      dec = \ case
        '●' -> Kuro
        '○' -> Siro

instance Encodable Hosi Natural where
  encoder = Encoder enc dec
    where
      enc = \ case
        Kuro -> 0
        Siro -> 1
      dec = \ case
        0 -> Kuro
        1 -> Siro
      
instance {-# Overlapping #-} Show Hositorihyou where
  showsPrec _ = \ case
    []   -> ('ε' :)
    s:[] -> shows s
    s:ss -> shows s . shows ss
          
instance {-# Overlapping #-} Read Hositorihyou where
  readsPrec _ = readP_to_S rHositorihyou
          
rHositorihyou :: ReadP Hositorihyou
rHositorihyou = rε +++ rHositori

rε :: ReadP Hositorihyou
rε = [] <$ char 'ε'

rHositori :: ReadP Hositorihyou
rHositori = map (decode . enenc) <$> munch1 (flip elem "●○") 

instance Encodable Hositorihyou [Natural] where
  encoder :: Encoder Hositorihyou [Natural]
  encoder = Encoder enc dec
    where
      enc :: Hositorihyou -> [Natural]
      enc = \ case
        []   -> []
        h:hs -> unenc (encode h) : unenc (encode hs)
      dec :: [Natural] -> Hositorihyou
      dec = \ case
        []   -> []
        n:ns -> decode (enenc n) : decode (enenc ns)

instance Encodable Hositorihyou Natural where
  encoder :: Encoder Hositorihyou Natural
  encoder = Encoder enc dec
    where
      enc :: Hositorihyou -> Natural
      enc = foldl' ((+) . (2 *)) 0 . unenc . (encode :: Hositorihyou -> Encoded Hositorihyou [Natural])
      dec :: Natural -> Hositorihyou
      dec = reverse . unfoldr psi
        where
          psi :: Natural -> Maybe (Hosi, Natural)
          psi = \ case
            0 -> Nothing
            n -> case divMod n 2 of
              (q, r) -> Just (decode (enenc r), q)
          
          