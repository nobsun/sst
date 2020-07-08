-- ---
-- marp: true
-- 
-- ---
-- 
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Expression.Tree
  ( Tree (..)
  , TreeF (..)
  ) where

import Text.ParserCombinators.ReadP
import Data.Functor.Foldable

-- ---
-- 
-- # 木
-- 
-- 
data Tree
  = Leaf           -- ^ 式の生成規則 E1，● に対応 
  | Tree :^: Tree  -- ^ 式の生成規則 E2，○・α・β に対応(αおよびβは式)
  deriving (Eq)
-- 
-- ---
-- 
-- ``Tree``の文字列表現は以下のようにE2に対応するものは点対表現にしておきます．
-- 
instance Show Tree where
  showsPrec _ = \ case
    Leaf    -> ('●' :)
    s :^: t -> ('(' :) . shows s . ('.' :) . shows t . (')' :)

instance Read Tree where
  readsPrec _ = readP_to_S rTree

rTree :: ReadP Tree
rTree = rLeaf +++ rFork

rLeaf :: ReadP Tree
rLeaf = Leaf <$ char '●'

rFork :: ReadP Tree
rFork = (:^:) <$> (char '(' *> rTree <* char '.') <*> (rTree <* char ')') 
-- 
-- ---
-- 
-- ``Tree``の再帰図式（recursion scheme）を定義しておきます．
-- 
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
