---
marp: true

---

# M式

```haskell
module Expression.MSiki
  ( MSiki ()
  , unmsiki
  , msiki
  , _M1
  , _M2
  , _M3
  , _M4
  ) where

import Expression.Hosi
import Expression.Siki
```
---

以下の4つの規則で構成される特別な式のことを**M式**(meta-expression)といいます．

(M1) $\alpha$ および $\beta$ を式とするとき $(●\;.\;(α\;.\;β))$ はM式である
(M2) $(●\;.\;●)$ はM式である
(M3) $s$ がM式のとき $(s\;.\;●)$ はM式である
(M4) $s$ および $t$ がM式のとき $(s . t)$ はM式である

規則M1で構成されるM式を**変数**とよびます．規則M2で構成されるM式は``()``で表します．さらに，規則M3で構成されるM式は $\#(s)$ と表します．

---

```haskell
newtype MSiki a = MSiki a

unmsiki :: MSiki (Siki Hositorihyou) -> Siki Hositorihyou
unmsiki (MSiki s) = s
```

**M式の生成規則**

```
_M1' :: Siki Hositorihyou 
    -> Siki Hositorihyou -> MSiki (Siki Hositorihyou)
_M1' α β = MSiki (_E2 _E1 (_E2 α β))

_M2 :: MSiki (Siki Hositorihyou)
_M2 = MSiki (_E2 _E1 _E1)

_M3 :: MSiki (Siki Hositorihyou) -> MSiki (Siki Hositorihyou)
_M3 s = MSiki (_E2 (unmsiki s) _E1)

_M4 :: MSiki (Siki Hositorihyou) 
    -> MSiki (Siki Hositorihyou) -> MSiki (Siki Hositorihyou)
_M4 s t = MSiki (_E2 (unmsiki s) (unmsiki t))
```

---

**M式の生成規則（改訂版）**

```haskell
_M1 :: MSiki (Siki Hositorihyou) -> MSiki (Siki Hositorihyou)
_M1 t = MSiki (_E2 _E1 (unmsiki t))

_M2 :: MSiki (Siki Hositorihyou)
_M2 = MSiki (_E2 _E1 _E1)

_M3 :: MSiki (Siki Hositorihyou) -> MSiki (Siki Hositorihyou)
_M3 s = MSiki (_E2 (unmsiki s) _E1)

_M4 :: MSiki (Siki Hositorihyou) 
    -> MSiki (Siki Hositorihyou) -> MSiki (Siki Hositorihyou)
_M4 s t = MSiki (_E2 (unmsiki s) (unmsiki t))
```

---

M式は式のうち生成規則(E1)でのみ構成された $●$ を除いたものですから．

```haskell
msiki :: Siki Hositorihyou -> Maybe (MSiki (Siki Hositorihyou))
msiki s = case unsiki s of
  [Kuro] -> Nothing
  _      -> Just (MSiki s)
```
