-- ---
-- marp: true
-- 
-- ---
-- 
-- # 式
-- 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NPlusKPatterns #-}
module Expression.Siki
  ( Siki ()
  , unsiki
  , _E1
  , _E2
  , siki
  , splits
  ) where

import Control.Arrow ((***))
import Data.Bool (bool)
import Numeric.Natural
import Expression.Hosi

-- 
-- ---
-- 
-- > **式**(expression)とよばれる特別の星取表を定義する．式は以下のように定義される
-- 
-- ### 式の定義
-- 
-- > (E1) $●$は式である．
-- > (E2) $\alpha$ と $\beta$ が式ならば $○$・$\alpha$・$\beta$ も式である．
-- 
-- ---
-- 
newtype Siki a = Siki a

unsiki :: Siki Hositorihyou -> Hositorihyou
unsiki (Siki hs) = hs

_E1 :: Siki Hositorihyou
_E1 = Siki [Kuro]

_E2 :: Siki Hositorihyou -> Siki Hositorihyou -> Siki Hositorihyou
_E2 (Siki α) (Siki β) = Siki ([Siro] ・ α ・ β)
-- 
-- ---
-- 
-- **式の構成に関する帰納法による証明**
-- 
-- > (I1) $P(●)$ を証明する
-- > (I2) $\alpha$，$\beta$ を式とするとき，$P(\alpha)$ と $P(\beta)$ を仮定して，$P(○\cdot\alpha\cdot\beta)$ を証明する
-- 
-- ---
-- 
-- > 式であるような星取表 $\alpha$ に含まれる $○$ の数を $n$ とすると $●$ の数は $n+1$ である．
-- 
-- 証明は式の構成に関する帰納法によります．
-- 
-- ---
-- 
-- **α が規則（E1）によって構成された星取表の場合**
-- 
-- $\alpha = ●$ のとき，
-- $\alpha$ に含まれる $○$ の数は $0$，$●$ の数は $1 = 0 + 1$．
-- 
-- ---
-- 
-- **α が規則 (E2) によって構成された星取表の場合**
-- 
-- $\alpha = ○\cdot\beta\cdot\gamma$（$\beta$，$\gamma$ が共に式）であるとき，帰納法の仮定より，
-- 
-- 1. $\beta$ に含まれる $○$ の数を $i$ とすると $\beta$ に含まれる $●$ の数は $i+1$
-- 1. $\gamma$ に含まれる $○$ の数を $j$ とすると $\gamma$ に含まれる $●$ の数は $j+1$
-- 
-- したがって，
-- $\alpha$ に含まれる $○$ の数は $1+i+j$，$●$ の数は $(i+1)+(j+1) = (1+i+j)+1$
-- 
-- ---
-- 
-- > $\alpha$ と $\beta$ を式とするとき，$\alpha\cdot\alpha^\prime \equiv \beta\cdot\beta^\prime$ となる星取表 $\alpha^\prime$，$\beta^\prime$ が存在するとき，$\alpha$ と $\beta$ は**両立する**（compatible）という．
-- 
-- 式 $\alpha$ と $\beta$ が同じ式であるなら，$\alpha^\prime$ および $\beta^\prime$ を $[]$ とおくことにより，$\alpha$ と $\beta$ は両立する式であることが判ります．
-- 
-- ---
-- 
-- **式** $\alpha$ **と式** $\beta$ **が両立するなら** $\alpha \equiv \beta$
-- 
-- 証明は $\alpha$ 上の帰納法によります．
-- 
-- $\alpha = ●$ の場合
-- $\alpha_1 = \beta_1 = ●$ であるが，先頭が $●$ であるような式は $●$ のみであるから，$\beta = ●$
-- 
-- ---
-- 
-- $\alpha = ○\cdot\gamma\cdot\delta$  （$\gamma$ および $\delta$ は式）の場合
-- $\alpha_1 = \beta_1 = ○$ であるから，$\beta = ○\cdot\gamma^\prime\cdot\delta^\prime$ （$\gamma^\prime$ および $\delta^\prime$ は式）と書ける．
-- 
-- $\alpha$ と $\beta$ は両立だから，ある星取表 $\eta$ と $\eta^\prime$が存在して，
-- $$○\cdot\gamma\cdot\delta\cdot\eta = ○\cdot\gamma^\prime\cdot\delta^\prime\cdot\eta^\prime$$
-- 
-- である．左簡約の法則より，
-- $$\gamma\cdot\delta\cdot\eta = \gamma^\prime\cdot\delta^\prime\cdot\eta^\prime$$
-- 
-- すなわち，$\gamma$ および $\gamma^\prime$ は両立する式である．帰納法の仮定により，$\gamma = \gamma^\prime$．再び，左簡約の法則より，
-- $$\delta\cdot\eta = \delta^\prime\cdot\eta^\prime$$
-- 
-- すなわち，$\delta$ および $\delta^\prime$ は両立する式．帰納法の仮定により，$\delta = \delta^\prime$．よって，
-- $$\alpha = ○\cdot\gamma\cdot\delta = ○\cdot\gamma^\prime\cdot\delta^\prime = \beta$$
-- 
-- ---
-- 
-- > 次に任意の星取表 $\alpha$ に対して，$\alpha$ が式のときは $○$ を対応させ，そうでないときは $●$ を対応させる関数 $f$ を以下のように具体的な手順として，星取表の長さに関して帰納的に定める．
-- 
-- ここでは，任意の星取表``α``に対して，``α``が式のときは，``Just (Siki α)``を対応させ，そうでないときには``Nothing``を対応をさせる関数を``siki``を考えます．
-- ``siki``は $\alpha \equiv ○\cdot\beta\cdot\gamma$ となる式 $\beta$ と式 $\gamma$ を決定できます．
-- 
-- ---
-- 
siki :: Hositorihyou -> Maybe (Siki Hositorihyou)
siki = \ case
  []      -> Nothing
  Kuro:hs -> bool Nothing (Just _E1) (null hs)
  Siro:hs -> case [ (s, t) | (Just s, Just t) <- map (siki *** siki) (splits hs)] of
    [(s, t)] -> Just (_E2 s t)
    []       -> Nothing

splits :: [a] -> [([a], [a])]
splits = \ case 
  []         -> [([], [])]
  xxs@(x:xs) -> ([], xxs) : [ (x:ys, zs) | (ys, zs) <- splits xs ]
-- 
-- ---
-- 
-- > $\alpha$ と $\beta$ が式のとき $○\cdot\alpha\cdot\beta$ の形の式を``(α . β)``で表し，$\alpha$ と $\beta$ の**点対**（dotted pair）とよぶ．このとき，$\alpha$，$\beta$ をそれぞれ点対``(α . β)``の**左要素**（left element），**右要素**（right element）という．
