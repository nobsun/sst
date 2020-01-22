---
marp: true

---

# 星取表

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NPlusKPatterns #-}
module Expression.Concrete.Hosi where

import Numeric.Natural
import Prelude hiding (length, take, drop)
```

---

2つの記号，黒星 $●$ と白星 $○$（総称して星という）を定義します．

```haskell
data Hosi
  = Kuro
  | Siro
  deriving (Eq)
```

---

利便のため，星（``Hosi``）を``Show``および``Read``型クラスのインスタンスとしておきます．

```haskell
instance Show Hosi where
  showsPrec _ = \ case
    Kuro -> ('●' :)
    Siro -> ('○' :)

instance Read Hosi where
  readsPrec _ = \ case
    '●':rs -> [(Kuro, rs)]
    '○':rs -> [(Siro, rs)]
```
---

次に星取表（星を有限個左から右へ並べたもの）を定義します．
星取表は，``[Hosi]``すなわち``Hosi``を要素とするリストとしましょう．

```haskell
type Hositorihyou = [Hosi]
```

---

これも``Show``および``Read``型クラスのインスタンスにしておきましょう．

```haskell
instance {-# Overlapping #-} Show Hositorihyou where
  showsPrec _ = \ case
    []       -> ('ε' :)
    hs@(_:_) -> foldr ((.) . shows) id hs

instance {-# Overlapping #-} Read Hositorihyou where
  readsPrec _ = \ case
    ""      -> [([], "")]
    "ε"    -> [([], "")]
    '●':rs -> [(Kuro : hs, rs') | (hs, rs') <- readsPrec 0 rs]
    '○':rs -> [(Siro : hs, rs') | (hs, rs') <- readsPrec 0 rs]
    _       -> []
```
---

### 星取表の相等性

星取表は星のリストで，星``Hosi``は``Eq``クラスのインスタンスなので，星取表``[Hosi]``も``Eq``クラスのインスタンスになります．
したがって，2つの星取表の相等性は，``(==)``で検査できます．

```
(==)   :: Eq a => [a] -> [a] -> Bool
[]     == []     = True
(x:xs) == (y:ys) = x == y && xs == ys
_      == _      = False 
```

$\alpha \equiv \beta$ $\Leftrightarrow$  Haskell上で``α == β`` $\equiv$ ``True`` 

---

### 星取表の長さ

星取表の長さは，リストの長さで表せますが，ここでは独自に定義しておきます．

```haskell
length :: Hositorihyou -> Natural
length = \ case
  []   -> 0
  h:hs -> 1 + length hs

ε :: [Hosi]
ε = []
```

---

### 星取表の連接

星取表の連接演算は，リストの連接で表せますが，ここでは独自に定義しておきます．

```haskell
(・) :: Hositorihyou -> Hositorihyou -> Hositorihyou
[]     ・ ys = ys
(x:xs) ・ ys = x : (xs ・ ys)

infixr 5 ・
```

---

連接が満すべき条件は以下のとおりです．

```
γ = α ・ β であるとき
take (length α) γ ≡ α
drop (length α) γ ≡ β
```

ただし，``take``と``drop``の定義は以下のものとする．

```haskell
take :: Natural -> Hositorihyou -> Hositorihyou
take 0     _      = []
take _     []     = []
take (n+1) (x:xs) = x : take n xs

drop :: Natural -> Hositorihyou -> Hositorihyou
drop 0     xs     = xs
drop _     []     = []
drop (n+1) (x:xs) = drop n xs
```


---

前述の性質を証明するには，α上の帰納法を使う．

```
α = [] の場合
  take (length []) ([] ・ β)  
= { lengthの定義 }
  take 0 ([] ・ β)
= { takeの定義 }
  []

  drop (length []) ([] ・ β)
= { lengthの定義 }
  drop 0 ([] ・ β)
= { dropの定義 }
  [] ・ β
= { ・の定義 }
  β
```

---

```
α = x : xs の場合
  take (length (x : xs)) ((x:xs) ・ β)
= { lengthの定義 }
  take (1 + length xs) ((x:xs) ・ β)
= { ・の定義 }
  take (1 + length xs) (x : (xs ・ β))
= { takeの定義 }
  x : take (length xs) (xs ・ β)
= { 帰納法の仮定 }
  x : xs

  drop (length (x : xs)) ((x:xs) ・ β)
= { lengthの定義 }
  drop (1 + length xs) ((x:xs) ・ β)
= { ・の定義 }
  drop (1 + length xs) (x : (xs ・ β))
= { dropの定義 }
  drop (length xs) (xs ・ β)
= { 帰納法の仮定 }
  β
```

--- 

星取表上の連接演算・は以下の結合法則を満たします．

### 連接は結合的演算子

> $\alpha$，$\beta$，$\gamma$ を星取表とするとき
> $$ (\alpha ・ \beta) ・ \gamma \equiv \alpha ・ (\beta ・ \gamma) $$
> がなりたつ.

---

証明は $\alpha$ 上の帰納法を使います．

```
α = [] の場合
  ([] ・ β) ・ γ
= { ・の定義 }
  β ・ γ
= { ・の定義 }
  [] ・ (β ・ γ)

α = x : xs の場合
  ((x : xs) ・ β) ・ γ
= { ・の定義 }
  (x : (xs ・ β)) ・ γ
= { ・の定義 }
  x : ((xs ・ β) ・ γ)
= { 帰納法の仮定 }
  x : (xs ・ (β ・ γ))
= { ・の定義 }
  (x : xs) ・ (β ・ γ)
```

---

### 左簡約法則(left cancelation law)

> $\alpha$，$\beta$，$\gamma$ を星取表とするとき，$\alpha ・ \beta \equiv \alpha ・ \gamma$ ならば $\beta \equiv \gamma$ である．

---

証明は $\alpha$ 上の帰納法を使います．

```
α = [] の場合
   [] ・ β ≡ [] ・ γ
⇒ { ・の定義 }
   β ≡ γ

α = x : xs の場合
   (x : xs) ・ β ≡ (x : xs) ・ γ
⇒ { ・の定義 }
   x : (xs ・ β) ≡ x : (xs ・ γ)
⇒ { (y : ys) ≡ (z : zs) ⇒ y ≡ z ∧ ys ≡ zs }
   (xs ・ β) ≡ (xs ・ γ)
⇒ { 帰納法の仮定 }
   β ≡ γ
```
