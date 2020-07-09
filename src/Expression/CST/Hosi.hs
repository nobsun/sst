{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Expression.CST.Hosi
  ( Hosi ()
  , fromChar
  , toChar
  ) where

-- | 星
newtype Hosi = Hosi Char
  deriving Eq

-- | 構成子
_Kuro :: Hosi
_Kuro = Hosi '●'
_Siro :: Hosi
_Siro = Hosi '○'

fromChar :: Char -> Hosi
fromChar = \ case
  '●' -> _Kuro
  '○' -> _Siro

toChar :: Hosi -> Char
toChar = \ case
  Hosi h -> h

-- | Show
instance Show Hosi where
  showsPrec _ = \ case
    Hosi h -> (h :)


-- | Read
instance Read Hosi where
  readsPrec _ = \ case
    c:cs | c `elem` "●○" -> [(Hosi c, cs)]
    _                    -> []

