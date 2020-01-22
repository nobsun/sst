---
marp: true

---

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Expression.Abstract.Tree
  ( Tree (..)
  
  ) where

import Data.Functor.Foldable
import Expression.Concrete

```
---

# 木


```haskell
data Tree
  = Leaf           -- ^ 式の生成規則 E1，● に対応 
  | Tree :^: Tree  -- ^ 式の生成規則 E2，○・α・β に対応(αおよびβは式)
  deriving (Eq)
```

---

``Tree``の文字列表現は以下のようにE2に対応するものは点対表現にしておきます．

```haskell
instance Show Tree where
  showsPrec _ = \ case
    Leaf    -> ('●' :)
    s :^: t -> ('(' :) . shows s . ('.' :) . shows t . (')' :)

instance Read Tree where
  readsPrec _ = readP_to_readS pTree

rTree :: ReadP Tree
rTree = pLeaf +++ pFork

rLeaf :: ReadP Tree
rLeaf = Leaf <$ char '●'

rFork :: ReadP Tree
rFork = (:^:) <$> (char '(' *> rTree <* char '.') <*> (rTree <* char ')') 
```

---

``Tree``の再帰図式（recursion scheme）を定義しておきます．

```haskell
data TreeF a
  = LeafF
  | a :^^: a
  deriving (Eq, Show)

instance Functor TreeF where
  fmap f = \ case
    LeafF -> LeafF
    x :^^: y -> f x :^^: f y

type instance Base Tree = TreeF

instance Recursive Tree where
  project = \ case
    Leaf    -> LeafF
    s :^: t -> s :^^: t

instance Corecursive Tree where
  embed = \ case
    LeafF    -> Leaf
    s :^^: t -> s :^: t  
```

---

```haskell
fromTreeForm :: Tree -> Siki Hositorihyou
fromTreeForm = cata phi
  where
    phi = \ case
      LeafF -> _E1
      α :^^: β -> _E2 α β

toTreeForm :: Siki Hositorihyou -> Tree
toTreeForm = ana psi
  where
    psi e = case unsiki e of
      Kuro:[] -> LeafF
      Siro:α -> β :^^: γ
        where
          (β,γ) = head [ (unsiki l, unsiki r) 
                       | (Just l, Just r) <- map (siki *** siki) (splits α)]

```