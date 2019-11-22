{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE InstanceSigs #-}

module Expression.Encodable
  ( Encodable (..)
  , Encoder (..)
  , Encoded ()
  , encode
  , decode
  ) where

class Encodable a b where
  encoder :: Encoder a b
  enenc :: b -> Encoded a b
  enenc = Encoded
  unenc :: Encoded a b -> b
  unenc = \ case
    Encoded y -> y

data Encoder a b where
  Encoder :: (a -> b) -> (b -> a) -> Encoder a b
  EncChain :: Encoder a b -> Encoder b c -> Encoder a c

enc :: Encoder a b -> (a -> b)
enc = \ case
  Encoder f _ -> f
  EncChain e1 e2 -> enc e2 . enc e1

dec :: Encoder a b -> (b -> a)
dec = \ case
  Encoder _ g -> g
  EncChain e1 e2 -> dec e1 . dec e2

data Encoded a b where
  Encoded :: b -> Encoded a b

encode :: Encodable a b => a -> Encoded a b
encode = enenc . enc encoder 

decode :: Encodable a b => Encoded a b -> a
decode = dec encoder . unenc
