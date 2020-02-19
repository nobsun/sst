---
marp: true

---

# シングルトン

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Utility.Singleton
  where

import Data.Kind (Type)
import Data.Typeable
import Data.Void
import Data.Maybe (mapMaybe)
import Unsafe.Coerce (unsafeCoerce)
import Numeric.Natural
```

---

```haskell
data family Sing (a :: k)

data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: SomeSing k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
  type Demote k = r | r -> k
  toSing :: Demote k -> SomeSing k
  fromSing :: Sing (a :: k) -> Demote k

class SingI (a :: k) where
  sing :: Sing a
```
---

Bool

```haskell
data instance Sing (a :: Bool) where
  SFalse :: Sing 'False
  STrue  :: Sing 'True

instance SingKind Bool where
  type Demote Bool = Bool
  toSing True  = SomeSing STrue
  toSing False = SomeSing SFalse
  fromSing STrue  = True
  fromSing SFalse = False


instance SingI 'False where
  sing = SFalse
```

---

Maybe

```haskell
data instance Sing (a :: Maybe k) where
  SNothing :: Sing 'Nothing
  SJust :: Sing (a :: k) -> Sing ('Just a)

instance SingI a => SingI ('Just a) where
  sing = SJust sing

instance SingI 'Nothing where
  sing = SNothing

instance (k ~ Demote k, SingKind k) => SingKind (Maybe k) where
  type Demote (Maybe k) = Maybe k
  toSing Nothing  = SomeSing SNothing
  toSing (Just a) = withSomeSing (toSing a) $ SomeSing . SJust
  fromSing SNothing  = Nothing 
  fromSing (SJust a) = Just $ fromSing a  
```

---

List

```haskell
data instance Sing (a :: [k]) where
  SNil :: Sing '[]
  SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)

instance SingI '[]  where
  sing = SNil

instance (SingI h, SingI t) => SingI (h ': t) where
  sing = SCons sing sing

instance (k ~ Demote k, SingKind k) => SingKind [k] where
  type Demote [k] = [k]
  toSing [] = SomeSing SNil
  toSing (h : t)
    = withSomeSing (toSing h) $ \sh ->
      withSomeSing (toSing t) $ \st ->
      SomeSing $ SCons sh st
  fromSing SNil = []
  fromSing (SCons sh st) = fromSing sh : fromSing st
```

---

Nat

```haskell
data Nat
  = Z
  | S Nat
  deriving Eq

data instance Sing (n :: Nat) where
  SZ :: Sing 'Z
  SS :: Sing (n :: Nat) -> Sing ('S n)

instance SingI 'Z where
  sing = SZ

instance SingI n => SingI ('S n) where
  sing = SS sing

instance SingKind Nat where
  type Demote Nat = Nat
  toSing Z     = SomeSing SZ
  toSing (S n) = withSomeSing (toSing n) (SomeSing . SS)
  fromSing SZ = Z
  fromSing (SS sn) = S (fromSing sn) 
```
---

```haskell
natVal :: SomeSing Nat -> Natural
natVal = \ case
  SomeSing SZ -> 0
  SomeSing (SS sn) -> withSomeSing (SomeSing sn) (succ . natVal . SomeSing)
```

---

```haskell
type (≡) = (:~:)

data Decision a
  = Proved a
  | Disproved (a -> Void)

class SDecide k where
  (%~) :: Sing (a :: k) -> Sing (b :: k) -> Decision (a ≡ b)

instance (Eq (Demote k), SingKind k) => SDecide k where
  a %~ b = if fromSing a == fromSing b
    then Proved (unsafeCoerce Refl)
    else Disproved (const undefined)
```
---

```haskell
instance (SDecide a) => SDecide (Maybe a) where
  SNothing %~ SNothing = Proved Refl
  SJust a  %~ SJust b  = case a %~ b of
    Proved Refl -> Proved Refl
    _           -> Disproved (const undefined)
  _        %~ _        = Disproved (const undefined) 
  
```
---

$\Sigma$ 型（依存対）

```haskell
data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma :: (forall (a :: k). Sing a -> f a -> r) -> Sigma f -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma fa = Sigma sing fa

fromSigma :: forall k (a :: k) (f :: k -> Type). (SingI a, SDecide k)
          => Sigma f -> Maybe (f a)
fromSigma (Sigma s fa) 
  = case s %~ (sing :: Sing a) of
      Proved Refl -> Just fa
      Disproved _ -> Nothing

catSigmas :: forall k (a :: k) f. (SingI a, SDecide k)
          => [Sigma f] -> [f a]
catSigmas = mapMaybe fromSigma
```
