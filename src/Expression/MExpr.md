---
marp: true

---

# M式 (meta expression)

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Expression.MExpr
  (
  ) where

import Numeric.Natural
import Text.ParserCombinators.ReadP
import qualified Data.Functor.Foldable as F
import Data.Functor.Foldable hiding (ListF (..))
import Expression.Hosi
import Expression.Expr
import Expression.MSiki

```
---

## M式の抽象構文

```haskell
data MExpr
  = Var MExpr
  | Unit
  | Quote MExpr
  | Pair MExpr MExpr
  deriving (Eq)
```
---

```haskell
instance Show MExpr where
  showsPrec _ = \ case
    Var t    -> ('x' :)
    Unit     -> ("()" ++)
    Quote t  -> ("#(" ++) . shows t . (')' :)
    Pair s t -> ('(' :) . shows s . ('.' :) . shows t . (')' :) 
```

---

```haskell
instance Read MExpr where
  readsPrec _ = readP_to_S rMExpr

rMExpr :: ReadP MExpr
rMExpr = rVar +++ rUnit +++ rQuote +++ rPair

rVar :: ReadP MExpr
rVar =  Var <$> rMExpr

rUnit :: ReadP MExpr
rUnit = Unit <$ char '(' <* char ')'

rQuote :: ReadP MExpr
rQuote = Quote <$> (string "#(" *> rMExpr <* char ')')

rPair :: ReadP MExpr
rPair = Pair <$> (char '(' *> rMExpr <* char '.') <*> (rMExpr <* char ')')
```

---

### M式の再帰図式

```haskell
data MExprF a
  = VarF a
  | UnitF
  | QuoteF a
  | PairF a a

instance Functor MExprF where
  fmap f = \ case
    VarF x    -> VarF (f x)
    UnitF     -> UnitF
    QuoteF x  -> QuoteF (f x)
    PairF x y -> PairF (f x) (f y)

type instance Base MExpr = MExprF
```

---

```haskell
instance Recursive MExpr where
  project = \ case 
    Var x    -> VarF x
    Unit     -> UnitF
    Quote t  -> QuoteF t
    Pair s t -> PairF s t

instance Corecursive MExpr where
  embed = \ case
    VarF x    -> Var x
    UnitF     -> Unit
    QuoteF t  -> Quote t
    PairF s t -> Pair s t    
```

---

### M式の具象構文とM式の抽象構文との相互変換

```haskell
fromMExpr :: MExpr -> MSiki (Siki Hositorihyou)
fromMExpr = cata phi
  where
    phi = \ case
      VarF x    -> _M1 x
      UnitF     -> _M2
      QuoteF t  -> _M3 t
      PairF s t -> _M4 s t

fromMExpr' :: MExpr -> Expr
fromMExpr' = cata phi
  where
    phi = \ case
      VarF x    -> Leaf :^: x
      UnitF     -> Leaf :^: Leaf
      QuoteF t  -> t :^: Leaf
      PairF s t -> s :^: t 
```

---

```haskell
toMExpr :: MSiki (Siki Hositorihyou) -> MExpr
toMExpr = \ case
  ms -> toMExpr' (toExpr (unmsiki ms))

toMExpr' :: Expr -> MExpr
toMExpr' = ana psi
  where
    psi = \ case
      s :^: t -> case s of
        Leaf    -> case t of
          Leaf    -> UnitF
          _       -> VarF t
        _       -> case t of
          Leaf    -> QuoteF s
          _       -> PairF s t
```

---

### M式の大きさ

```haskell
size :: MExpr -> Natural
size = cata phi
  where
    phi = \ case
      VarF _    -> 0
      UnitF     -> 0
      QuoteF t  -> succ t
      PairF s t -> succ (s + t)
```

---

### リストの具象構文

```haskell
newtype MEList a = MEList a

unmelist :: MEList a -> a
unmelist (MEList ls) = ls

_L1 :: MEList (MSiki (Siki Hositorihyou))
_L1 = MEList _M2

_L2 :: MSiki (Siki Hositorihyou)
    -> MEList (MSiki (Siki Hositorihyou)) -> MEList (MSiki (Siki Hositorihyou))
_L2 h t = MEList (_M4 h (unmelist t)) 
```

---

### リストの抽象構文

```haskell
data List a 
  = Nil
  | Cons a (List a)
  deriving (Eq)

type instance Base (List a) = F.ListF a
```

---

### リストの再帰図式

```haskell
instance Recursive (List MExpr) where
  project = \ case
    Nil      -> F.Nil
    Cons s l -> F.Cons s l 

instance Corecursive (List MExpr) where
  embed = \ case
    F.Nil      -> Nil
    F.Cons s l -> Cons s l  
```

---

リストかどうかの判定

```haskell
isList :: MExpr -> Bool
isList = cata phi
  where
    phi = \ case
      UnitF     -> True
      PairF _ t -> t
      _         -> False
```

---

```haskell
fromList :: List MExpr -> MEList (MSiki (Siki Hositorihyou))
fromList = cata phi
  where
    phi = \ case
      F.Nil      -> _L1
      F.Cons s t -> _L2 (fromMExpr s) t

fromList' :: List MExpr -> MExpr
fromList' = \ case
  Nil      -> Unit
  Cons h t -> Pair h (fromList' t)
```
---

```haskell
toList :: MEList (MSiki (Siki Hositorihyou)) -> List MExpr
toList ls = toList' (toMExpr (unmelist ls))

toList' :: MExpr -> List MExpr
toList' = ana psi
  where
    psi = \ case
      Unit     -> F.Nil
      Pair h t -> F.Cons h t 
```

---

要素かどうか（要素であるという関係があるかどうか）の判定

```haskell
(∈) :: MExpr -> List MExpr -> Bool
_ ∈ Nil      = False
s ∈ Cons h t = s == h || s ∈ t
```

---