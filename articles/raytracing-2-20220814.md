---
title: "レイトレーシング(2): ユニットテストなど"
emoji: "💡"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "RayTracing", "PhotonMapping"]
published: true
---
#### ユニットテスト


前回定義したベクトルのモジュールをテストしたい。xUnitみたいなのはないかと思ったら、やはりHaskell用にはHUnitがあるらしい。しかし、JUnit等と同様にテストコードを別ファイルで定義したり一つ一つ値を試すテストを書いてしないといけないようだ。これまでxUnit系を少しかじったことがあるが、結構邪魔くさい。

一方、Haskell勉強用にと買った[]には、QuickCheckなるものがあると書いてある。これは値をいちいち記述してその結果と比べて、みたいな書き方ではなく、その関数がどうふるまうべきかを書いていくようだ。先のベクトルモジュールで言えば、「`normalize`(正規化)したベクトルの大きさは1.0になる」ということ。

でもQuickCheckでも、テスト用のコードを別に書かないといけないなぁと思っていたら、[あどけない話: Haskellの単体テスト最前線](http://d.hatena.ne.jp/kazu-yamamoto/20121205/1354692144)にいいことが書いてあるではないか。doctestにQuickCheckを組み合わせるということだ。これならソースファイルにテストコードを記述でき、複数のテストの同時実行もファイルを一つだけ用意するだけみたい。(難しいことはよくわからないので、とりあえずHSpecは無視)

さっそく、`test/doctests.hs`を用意する。

```haskell
module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Ray/Algebra.hs"]
```

`Algebra`の方もテストを追加。

```haskell
instance BasicMatrix Vector3 where
-- |
-- vector addition
--
-- >>> madd (Vector3 1 2 3) (Vector3 4 5 6)
-- [5.0,7.0,9.0]
  madd (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)
```

なるほど、これまで別のファイルに書いていたのを直接その関数のところに書けるのはとてもよい。これまでは、しばしばテストがソースの仕様変更に追いついていなかったので・・・。なお、`>>>`で始まる行がテストする具体的なコード、次の行がその結果だが、結果は文字列表記の比較のようだ。だから、`Vector3`には`show`関数と`(==)`が必要。(前回、「こっそり`Show`クラスと`Eq`クラスの子にしておいた」のはこれも理由)

```haskell
instance Show Vector3 where
  show (Vector3 ax ay az) = "[" ++ (show ax) ++ "," ++ (show ay) ++ "," ++ (show az) ++ "]"

instance Eq Vector3 where
  (==) (Vector3 ax ay az) (Vector3 bx by bz) = ax == bx && ay == by && az == bz
```

そうそう、`raytracer.cabal`にも追記しておこう。

```
Test-Suite doctest
  Type:                exitcode-stdio-1.0
  Default-Language:    Haskell2010
  hs-source-dirs:      test
  ghc-options:         -threaded -Wall
  main-is:             doctests.hs
  build-depends:       base
                       , doctest >= 0.9.3
```

`main-is`でテストを取りまとめているdoctests.hsを指定、`build-depends`には`doctest`のモジュールを指定する。実行するとこんな感じ。

```
$ cabal test
Preprocessing test suite 'doctest' for raytracer-0.1.0.0...
Running 1 test suites...
Test suite doctest: RUNNING...
Test suite doctest: PASS
Test suite logged to: dist/test/raytracer-0.1.0.0-doctest.log
  (後略)
```



#### ベクトルモジュールをいじる

#### (1) 位置ベクトルと方向ベクトル

三次元ベクトル`Vector3`を定義したが、実用的にはもう少し分類しておきたい。つまり、位置ベクトル(positional vector)と方向ベクトル(directional vector)に分けて管理できるようにしたい。そこで、`Vector3`に別名をつけよう。

```haskell
type Position3 = Vector3
type Direction3 = Vector3
```

これだけだと名前だけの話だ。今回のプログラムでは方向ベクトルは必ず正規化されているもの、という制約をつけよう（ただ、邪魔くさそうなので厳密にはやらないけど）。そのために、生成時に必ず正規化されるように`initDir`を用意する。あとあとのため、ゼロベクトル、単位ベクトルも合わせて定義しておこう。形だけだが、位置ベクトルにも`initPos`を用意しよう。

```haskell
o3 = Vector3 0 0 0    -- ゼロベクトル
ex3 = fromJust $ initDir 1 0 0 -- 単位ベクトル(x)
ey3 = fromJust $ initDir 0 1 0 -- 単位ベクトル(x)
ez3 = fromJust $ initDir 0 0 1 -- 単位ベクトル(x)

initDir :: Double -> Double -> Double -> Maybe Direction3
initDir x y z
  | v == o3   = Nothing
  | otherwise = Just (normalize v)
  where
    v = Vector3 x y z

initPos :: Double -> Double -> Double -> Position3
initPos x y z = Vector3 x y z

```

`initDir`の結果に`Maybe`を使っているのは、引数が全部ゼロの場合があり得るから。これでは方向ベクトルにできない。


#### (2) 公開するもの、しないもの

中には他のモジュールには見せたくない関数なども含まれている（としよう）。Javaでいうprivateメソッドとかのようなものだな。このように外部に晒したくないものがある場合は、見せてよいものだけを列挙したらいいらしい。モジュールの先頭で列挙するだけなので簡単だった。

```haskell
module Ray.Algebra
  ( madd
  , msub
    :
  , nearly0
  , o3
  , ex3
    :
  ,
  , initPos
  , initDir
    :
  ) where
```

これで、ここに書いてある以外の定義、関数などは他から使えなくなる。関数や定数(o3とか)も並べるだけでよい。
