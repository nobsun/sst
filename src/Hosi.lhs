---
marp: true

---

# 星取表

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NPlusKPatterns #-}
module Hosi where

import Numeric.Natural
import Prelude hiding (length, take, drop)
```

---

> 本章で扱う記号は，本質的には，○と●だけである．
> これらの記号をそれぞれ，白星，黒星とよぶことにする．
> 白星，黒星を総称して星という．
> これらふたつの記号が本章での対象記号である．

具象構文の表現は，記号列です．
2つの記号のみからなる記号列を考えています．

---

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

次に星取表を定義します．

> 星を有限個左から右へならべたものを**星取表**とよぶ．

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

星取表は星のリストとしたので，星取表も``Eq``クラスのインスタンスになります．
したがって，2つの星取表の相等性は，``(==)``で検査できます．

```
(==)   :: Hositorihyou -> Hositorihyou -> Bool
[]     == []     = True
(x:xs) == (y:ys) = x == y && xs == ys
_      == _      = False 
```

---

> $\alpha$を長さ$n$の星取表とするとき，各 $i\;(1\le i \le n)$ に対して，$\alpha$ の中で左から $i$ 番目に書かれている記号を $\alpha_{i}$ で表し，$\alpha$ の**第 $i$ 要素**($i$-th element)とよぶ．
> (中略)
> 同じ長さ$n$を持ち，各$i(1 \le i \le n)$について第$i$要素がすべて同じ記号であるふたつの星取表は**同じ**(identical)星取表であるという．
> 星取表$\alpha$と$\beta$が同じ星取表でることを$\alpha \equiv \beta$で表す．$\alpha$と$\beta$が同じでないときは，$\alpha$と$\beta$は**異なる**(different)といって，$\alpha \not\equiv \beta$であらわす


$\alpha \equiv \beta$ $\Leftrightarrow$  Haskell上で``α == β``が``True`` がいえます．

---

### 星取表の長さ

> 星取表$\alpha$に現れる白星，黒星の総数を$\alpha$の長さ（length）といい，$|\alpha|$で表す．
たとえば，○●●は長さ3の星取表である．また長さ0の星取表を空列（empty sequence）とよび $\epsilon$ で表す．

星取表の長さは，リストの長さで表せます．

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


> $\alpha$ と $\beta$ を星取表とするとき，$|\gamma| = |\alpha| + |\beta|$ で
> $$ \gamma_i \equiv \left\{ \begin{array}{ll} \alpha_i & (i \le |\alpha|のとき) \\ \beta_i & (i > |\alpha| のとき) \end{array}\right. $$
> となるような $\gamma$ が一意に定まる．この $\gamma$ を $\alpha$ と $\beta$ の**連接**(juxtaposition)といい $\alpha \cdot \beta$ で表す．

連接はHaskellでは以下のように定義します．

```haskell
(・) :: Hositorihyou -> Hositorihyou -> Hositorihyou
[]     ・ ys = ys
(x:xs) ・ ys = x : (xs ・ ys)

infixr 5 ・
```

---

連接が見たすべき前述の条件は以下の条件と同等です．

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

> 連接の定義から，任意の星取表 $\alpha$ について，$\epsilon ・ \alpha \equiv \alpha ・ \epsilon$ となることがわかる．すなわち，空列は連接に関する**単位元**(identity element)になる．連接はまた次の**左簡約法則**(left cancelation law)を満たす．

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
