---
title: "レイトレーシング(2): `Algebra`モジュールをいじる"
emoji: "💡"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "RayTracing", "PhotonMapping"]
published: true
---
(この記事はQiitaからの転載です)

レイトレーシング：[目次](https://zenn.dev/eijian/articles/raytracing-index-20220814)

今回はテストについて書くつもりだったが、テストをいろいろ「テスト」していてなかなか確認事が多そうなので、後回しにする。そこで、前回作った`Algebra`モジュールをちょっといじろうと思う。

#### 位置ベクトルと方向ベクトル

三次元ベクトル`Vector3`を定義したが、実用的にはもう少し分類しておきたい。具体的には、位置ベクトル(positional vector)と方向ベクトル(directional vector)に分けて扱えるようにしたい。そこで`Vector3`に別名をつけよう。

```haskell
type Position3 = Vector3
type Direction3 = Vector3
```

これだけだと名前だけの話だ。今回のプログラムでは方向ベクトルは必ず正規化されているもの、という制約をつけよう（ただ、邪魔くさそうなので厳密にはやらないが）。そのため、生成時に必ず正規化されるように`initDir`を用意する。形だけだが、位置ベクトルにも`initPos`を用意しよう。また、後々のため、ゼロベクトル、単位ベクトルも合わせて定義しておこう。

```haskell
initDir :: Double -> Double -> Double -> Maybe Direction3
initDir x y z = normalize $ Vector3 x y z

initPos :: Double -> Double -> Double -> Position3
initPos x y z = Vector3 x y z

o3  = initPos 0 0 0            -- ゼロベクトル
ex3 = fromJust $ initDir 1 0 0 -- 単位ベクトル(x)
ey3 = fromJust $ initDir 0 1 0 -- 単位ベクトル(x)
ez3 = fromJust $ initDir 0 0 1 -- 単位ベクトル(x)
```

`initDir`の結果に`Maybe`を使っているのは、引数が全部ゼロの場合、すなわちゼロベクトルがあり得るから。ゼロベクトルは方向ベクトルにできないので「値なし」ということで`Nothing`を返す。

#### 関数名を演算子にしたい

ベクトルの加減算のため、`madd`や`msub`などを定義した。使い方は次のようになるだろう。例として反射ベクトルの計算式 [tex: \boldsymbol{r} = \boldsymbol{d}-2(\boldsymbol{n} cdot \boldsymbol{d}) \boldsymbol{n}]を示す。

```haskell
r = msub d (mscale (2 * (dot n d)) n)          -- 前置
  もしくは
r = d `msub` ((2 * (n `dot` d)) `mscale` n)    -- 中置
```

なんと見難くて醜いことか。今回のネタではベクトル演算を多用するため、このままでは見た目もタイプ量もデバッグにもよろしくない。なんとかできないものか。

幸いHaskellでは演算子も関数として定義できるらしい。代わりに+や-を定義してみよう。

```haskell
class (Show a, Eq a) => BasicMatrix a where                                     
  (+) :: a -> a -> a                                                            
  (-) :: a -> a -> a                                                            

  (中略)

instance BasicMatrix Vector3 where                                              
  (Vector3 ax ay az) + (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)                   
  (Vector3 ax ay az) - (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)
```

でもこれだと山ほどコンパイルエラーが出る。

```
$ ghc -o Algebra Algebra.hs
[1 of 1] Compiling Ray.Algebra      ( Algebra.hs, Algebra.o )

Algebra.hs:50:57:
    Ambiguous occurrence ‘+’
    It could refer to either ‘Ray.Algebra.+’,
                             defined at Algebra.hs:13:3
                          or ‘Prelude.+’,
                             imported from ‘Prelude’ at Algebra.hs:5:8-18
                             (and originally defined in ‘GHC.Num’)
  (以下同様)
```

加算の定義中で使われている'+'と、定義した'+'がぶつかっているようだ。修飾子をつけないとダメらしい。定義中のものに`Prelude.`を追加してみる。

```haskell
instance BasicMatrix Vector3 where                                              
  (Vector3 ax ay az) + (Vector3 bx by bz) = Vector3 (ax Prelude.+ bx) (ay Prelude.+ by) (az Prelude.+ bz)           
  (Vector3 ax ay az) - (Vector3 bx by bz) = Vector3 (ax Prelude.- bx) (ay Prelude.- by) (az Prelude.- bz)
```           
これでエラーが出なくなった。`Vector3`の定義はあまり綺麗ではないが、他のところでスッキリ書けるならまあいいだろう。と思って、以下のようなサンプルを書いてみた。

```haskell
module Main where                                                               
                                                                                
import Ray.Algebra                                                              
                                                                                
main :: IO ()                                                                   
main = do                                                                       
  let a = Vector3 1 2 3                                                         
  let b = Vector3 3 4 5                                                         
  let c = a + b                                                                 
  putStrLn $ show c                                                             
```

そしたら、エラーが出た。

```
$ ghc -o ray Main.hs
[2 of 2] Compiling Main             ( Main.hs, Main.o )

Main.hs:13:13:
    Ambiguous occurrence ‘+’
    It could refer to either ‘Prelude.+’,
                             imported from ‘Prelude’ at Main.hs:5:8-11
                             (and originally defined in ‘GHC.Num’)
                          or ‘Ray.Algebra.+’,
                             imported from ‘Ray.Algebra’ at Main.hs:7:1-18
```

だめだ、`Algebra`モジュール外でもエラーになる！引数が`Vector3`型なのだからどちらを使うかは自明と思うが? Haskellは型推論が優れていると自慢しているのに、なぜこのぐらいの判断ができない?こんな修飾子を毎回書くのなら、せっかく式を簡略化しようとしたのに本末転倒だ。ちょこちょこ調べたところnobsunさんの[コメント](http://www.sampou.org/cgi-bin/w3ml.cgi/haskell-jp/msg/325)を発見した。結局のところ、'+'とか'-'とかが`Num`クラスで定義されているせいだと。しかしベクトル型を`Num`クラスのインスタンスにするのは無理がある。乗算とか。だいたい、なぜ '+' を数値型前提で定義するのだろう。文字列でもなんでも数値以外にも使いようがいっぱいあるのに。数学者が寄って仕様を作ったのかと思ってた。。。

と愚痴っても仕方ないので調べたところ、[NumericPrelude](https://hackage.haskell.org/package/numeric-prelude-0.4.2)というのがあるそうな。これを組み入れてみよう。cabalでインストールする。

```
$ cabal install numeric-prelude
```

これを使うために、`Algebra`のソースを少々（いやかなり）いじる。`Additive`は加減算、`Module`はスカラー倍を定義しているクラス。

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
  :
module Ray.Algebra where
  :
import NumericPrelude
import qualified Algebra.Additive as Additive
import qualified Algebra.Module as Module
  :
class (Show a, Eq a, Additive.C a, Module.C Double a) => BasicMatrix a where                       
  :
instance Additive.C Vector3 where
  zero = Vector3 0 0 0
  (Vector3 ax ay az) + (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)
  (Vector3 ax ay az) - (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)
```

これに倣い、他の関数名も変更してみた。

| 変更前      | 変更後      |
|:-----------|:-----------|
| madd       | +          |
| msub       | -          |
| mscale     | *>         |
| mdiv       | />         |
| nearlyEqual| .=.        |
| dot        | <.>        |
| cross      | <*>        |

`main`の方も少し追加が必要だ。

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import NumericPrelude
import Ray.Algebra

main :: IO ()
main = do
  let a = Vector3 1 2 3
  let b = Vector3 1 (-1) 1
  putStrLn $ show (a + b)
  putStrLn $ show $ norm d
  putStrLn $ show (a <.> b)
  putStrLn $ show ((1.2::Double) *> a)
  putStrLn $ show (b /> 3)
  let w = fromJust $ normalize b
  let r = w - (2 * (w <.> ey3)) *> ey3
  putStrLn $ show r
```

(詳しいことは[ソース](https://github.com/eijian/raytracer/blob/master/src/Ray/Algebra.hs)を)

これだと`Vector3`を使う方のソースは簡潔だし、正しく計算も出来ている！

```
$ ghc -o ray Main.hs
[2 of 2] Compiling Main             ( Main.hs, Main.o )
Linking ray ...
$ ./ray
[2.0,1.0,4.0]
  :
[0.5773502691896258,0.5773502691896258,0.5773502691896258]
```

記述方法を比較してみる。

```haskell
r = msub d (mscale (2 * (dot n d)) n)          -- 前置
r = d `msub` ((2 * (n `dot` d)) `mscale` n)    -- 中置
r = d - (2 * (d <.> n)) *> n                   -- 改善後
```

ああ、見やすくなった。（と自分では思う）

#### 公開するもの、しないもの

中には他のモジュールには見せたくない関数なども含まれている（としよう）。JavaでいうPrivateメソッドとかのようなものだ。このように外部に晒したくないものがある場合は、見せてよいものだけを列挙したらいいらしい。モジュールの先頭で列挙するだけなので簡単だった。特に`Algebra`では位置ベクトルと方向ベクトルを定義したので、`Vector3`で直接生成したり要素を取り出したりできないようにしておく。また後で必要に迫られたら考えたらいい。

```haskell
module Ray.Algebra
  ( nearly0
  , o3
  , ex3
    :
  , (+)
  , (-)
    :
  ,
  , initPos
  , initDir
  ) where
```

これで、ここに書いてある以外の定義、関数などは他から使えなくなる。関数や定数(`o3`とか)も並べるだけでよい。

今回はここまで。


