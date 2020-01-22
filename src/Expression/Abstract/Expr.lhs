---
marp: true

---

```haskell
{-# LANGUAGE LambdaCase #-}
module Expression.Abstract.Expr
  (
  ) where

import Data.Functor.Foldable
import Expression.Concrete.Hosi
import Expression.Concrete.Siki
import Expression.Abstract.Tree
```

---

式の具象構文は``Siki Hositorihyou``という特別な星取表，すなわち一次元の記号列で表現されています．
式を数学的対象とするとき，その構造は全二分木と見なせます．すなわち，式の抽象構文は``Tree``と同型（Isomorphic）です．そこで改めて，式の抽象構文を``Tree``とみなします．

```haskel
type Expr = Tree
```

---

式の具象構文と式の抽象構文の相互変換

```haskell
fromExpr :: Expr -> Siki Hositorihyou
fromExpr = cata phi
  where
    phi = \ case
      LeafF    -> _E1
      s :^^: t -> _E2 s t

toExpr :: Siki Hositorihyou -> Expr
toExpr = ana psi
  where
    psi e = case unsiki e of
      Kuro:[] -> LeafF
      Siro:α  -> β :^^: γ
        where
          (β,γ) = head [(l, r)
                       | (Just l, Just r) <- map (siki *** siki) (splits α)]      
```
