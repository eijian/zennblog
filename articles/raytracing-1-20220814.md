---
title: "レイトレーシング(1): バージョン1の定義、ベクトル演算"
emoji: "💡"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["Haskell", "RayTracing", "PhotonMapping"]
published: true
---
(この記事はQiitaからの転載です)

レイトレーシング：[目次](https://zenn.dev/eijian/articles/raytraicing-index-20220814)

Haskellは数学と関連があるというような話をちょくちょく見ることがある。圏論がどうとか数学的な概念が…といったところは筆者にはわからないが、ソース（見た目）はかなり数学っぽいと思う。実際これが一番Haskellにはまっている理由かもしれない。この簡潔さは素晴らしい。数学とくればレイトレーシング？ということで作ってみる。

手元にこんな本がある。

[フォトンマッピング-実写に迫るコンピュータグラフィックス](https://www.ohmsha.co.jp/book/9784274079504/) (オーム社)

古典的なレイトレーシングソフトは作ったことがあるので、今回はフォトンマッピングに手を出してみよう。そういうことだから、完成できる保証はない。また、途中でときどき別のネタに脱線すると思う。なお、ここでは理論の詳細には触れない。レイトレーシングのアルゴリズムや実装については上記の本や[ここ](http://kagamin.net/hole/edupt/index.htm)を参照するとよいかもしれない。

#### "バージョン1"の定義

フォトンマッピング法に詳しいわけではないので、大ウソの連発かもしれないことはあらかじめ言い訳しておく。

さてこの手法は第一フェーズでフォトンマップを作成し、第二フェーズでレイトレーシングする、二段階アルゴリズムである。ただし第二フェーズでは普通のレイトレだと光源が見えるかどうか調べるが、この手法は光が到達する量をフォトンマップから「推定」する。これがみそ。

最初から超リアルな画像を生成できるものは無理なので、簡単なものを作って少しずつ肉付けしていけばいいだろう。バージョン1ではだいぶ単純化した仕様にして、まずは動くものを作る。以下が最初の仕様だ。

* 光源は点光源だけ
* 物体表面は拡散反射のみ（鏡面反射・屈折は無視）
* フォトンの追跡は反射を無視（=相互拡散反射による効果はお預け）
* 物体は球と無限平面のみ
* 材質（というか色）は単色

この仕様で画像が生成できるのか今の時点ではよくわからないが、とりあえずは進めてみよう。

#### ベクトル演算

レイトレーシングの処理は、ほとんどが3次元ベクトルか光量(輝度）の演算で占められている。まずはベクトル演算のモジュールを作ろう。代数に関するモジュールなので名前をAlgebraにしよう。開発用ディレクトリの基本的な構造は以前に書いた通り([GitHub](https://github.com/eijian/raytracer))。今回は`src`ディレクトリの下に`Ray`というディレクトリを作ってその中にソースファイルを作ることにする。トップディレクトリから見ると`src/Ray/Algebra.hs`だ。

そうそう、三次元座標系は筆者の好みで「左手系」かつy軸が上(x軸は右、z軸は奥)を正方向とする。

主要なベクトル演算は型クラスで定義しておくと2次元ベクトルや行列など似たような型を定義するのにも使えそうである。どちらかというとベクトルは行列の特殊なものと考えれば、型クラスは

```
 BasicMatrix --> Matrix, Vector
```

という親子関係にしたほうがよさそう。`BasicMatrix`に行列やベクトルに共通な基本的な演算(関数)を定義し、特有の演算はそれぞれMatrixと`Vector`クラスに定義するようにしよう。まず`BasicMatrix`で基本的な演算を定義する。加減算、スカラー倍、スカラー除算、ノルムにしよう。他に必要なものがあれば出てきてから追加する。なお、こっそり`Show`クラスと`Eq`クラスの子にしておく。(実はこの歳になって初めてノルムにも色々な種類があると知った。ただここでは一般的(?)な、ベクトルで長さを意味するノルムとしよう。）

```haskell
class (Show a, Eq a) => BasicMatrix a where
  madd :: a -> a -> a                 -- 加算
  msub :: a -> a -> a                 -- 減算
  mscale :: Double -> a -> a          -- スカラー倍
  mdiv   :: a -> Double -> Maybe a    -- スカラー除算
  mdiv a s                                                                      
    | s == 0    = Nothing                                                       
    | otherwise = Just ((1 / s) `mscale` a)
  norm :: a -> Double                 -- ノルム
  nearlyEqual :: a -> a -> Bool       -- ≒

class (BasicMatrix a) => Vector a where
  dot :: a -> a -> Double             -- 内積
  normalize :: a -> Maybe a           -- 正規化
  normalize a = a `mdiv` (norm a)
  square :: a -> Double               -- 二乗
  square a = a `dot` a
```

`mdiv`は逆数を掛けるのと等しいことをクラス定義で記述しておく。除算なので`s`が0の場合はエラーだ。ここでは解をMaybe型にし、エラーなら`Nothing`を返すようにしている。`Vector`クラスの`normalize`(正規化)と`square`(二乗)についても同じくクラス定義で実装してしまおう。`normalEqual`はベクトル同士の比較用関数である。ご存知の通りコンピュータで実数を扱うと誤差が生じるので、理論上同一になる筈の結果がそうならないことがある。「誤差の範囲なら同じとみなす」ような比較用だ。使うかどうかわからないが。またこれに付随して(?)、`nearly0::Double`も定義しておく。

のちのちMaybe型に関連する関数を使うためには`Data.Maybe`モジュールをimportしないといけない。今のうちに入れておく。

```haskell
import Data.Maybe
```

なお、`Matrix`(行列)は将来的には使うが、とりあえず今は無視する。

次に三次元ベクトル型を定義しよう。`Vector3`だ。

```haskell
data Vector3 = Vector3 Double Double Double

(中略)

instance Matrix Vector3 where
  madd (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax + bx) (ay + by) (az + bz)
  msub (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax - bx) (ay - by) (az - bz)
  
(中略)

cross :: Vector3 -> Vector3 -> Vector3                                          
cross (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ay * bz - by * az) (az * bx - bz * ax) (ax * by - ay * bx)       


(以下続く…)
```

外積だけは三次元ベクトル特有の演算なので（本当かどうか知らない）、クラス定義には含められず独立した関数`cross`として定義した。動作確認のため、対話環境(ghci)で試す。以下はsrcディレクトリ内で実行した場合である。

```
$ ghci
Prelude> :l Ray.Algebra
[1 of 1] Compiling Ray.Algebra      ( Ray/Algebra.hs, interpreted )
Ok, modules loaded: Ray.Algebra.
*Ray.Algebra> let a = Vector3 1 2 3
*Ray.Algebra> a
[1.0,2.0,3.0]
*Ray.Algebra> let b = Vector3 4 5 6
*Ray.Algebra> putStrLn $ show $ madd a b
[5.0,7.0,9.0]
*Ray.Algebra> putStrLn $ show $ msub a b
[-3.0,-3.0,-3.0]
*Ray.Algebra> 
*Ray.Algebra> putStrLn $ show $ mscale 5 a
[5.0,10.0,15.0]
*Ray.Algebra> putStrLn $ show $ mdiv a 5
Just [0.2,0.4,0.6000000000000001]
*Ray.Algebra> putStrLn $ show $ norm a 
3.7416573867739413
*Ray.Algebra> putStrLn $ show $ dot a b
32.0
*Ray.Algebra> let c = normalize a
*Ray.Algebra> c
Just [0.2672612419124244,0.5345224838248488,0.8017837257372732]
*Ray.Algebra> let d = fromJust c
*Ray.Algebra> d
[0.2672612419124244,0.5345224838248488,0.8017837257372732]
*Ray.Algebra> putStrLn $ show $ norm d
1.0
*Ray.Algebra> let x = Vector3 1 0 0
*Ray.Algebra> let y = Vector3 0 1 0
*Ray.Algebra> let z = cross x y
*Ray.Algebra> z
[0.0,0.0,1.0]
```

それなりにうまくいっているようだ。が、いくつか適当な値で試しても、正直な所ちゃんとテストできているかどうかわからない。ということで次回はユニットテストを考える。

