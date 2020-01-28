---
marp: true

---

# M式 (meta expression)

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Expression.MExpr
  (
  ) where

import Numeric.Natural
import Text.ParserCombinators.ReadP
import Data.Functor.Foldable
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
