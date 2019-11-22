{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Expression.Expr where

import Data.Maybe (listToMaybe, fromJust)

import Expression.Hositorihyou
import Expression.Encodable

data Expr 
  = Leaf
  | Expr :^: Expr

instance Encodable Expr Hositorihyou where
  encoder = Encoder enc dec
    where
      enc = \ case
        Leaf -> [Kuro]
        e1 :^: e2 -> Siro : (enc e1 ++ enc e2)
      dec = \ case
        hs -> fromJust (expr hs)

expr :: Hositorihyou -> Maybe Expr
expr = \ case
  hs -> listToMaybe [ e | (e, []) <- expr' hs]

expr' :: Hositorihyou -> [(Expr, Hositorihyou)]
expr' = \ case
  Kuro : rs -> [(Leaf, rs)]
  Siro : rs -> [(e1 :^: e2, rs2) | (e1, rs1) <- expr' rs, (e2, rs2) <- expr' rs1]
  []        -> []