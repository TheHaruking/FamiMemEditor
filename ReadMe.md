## ファミメムエディタ
2019/12/01(日)  

---

![](_ReadMe/1_demo.gif)

### ■ 概要
ファミコン用メモリエディタです。  
入力した 16 進数を機械語としてそのまま実行する機能も備わっています。  
FamiMemEditor.nes : 【[ダウンロード](https://github.com/TheHaruking/FamiMemEditor/releases/download/v2.0/FamiMemEditor.nes)】

### ■ 実行方法

**パソコンで実行** : ファミコンエミュレーター ([Mesen](https://www.mesen.ca/ja/) 等) を用いて FamiMemEditor.nes を開く  
**レトロフリークで実行** : MicroSD に FamiMemEditor.nes を入れてメニューから起動

> ※レトロフリークで実行時は、[出力設定] で [オーバースキャン] : ON に設定下さい。でないと、左端が途切れます。

### ■ 操作方法

※ [ ] は、"押しながら" を意味します。

|||
|-|-|
| [A] + 十時キー | カーソル移動 |
| [十時キー] + A or B | 16進数値入力 (0-F) |
| [START] + ↑ or ↓ | 範囲選択 & コピー |
| [START] + → | ペースト |
| SELECT | 機械語として実行 (表示ﾍﾟｰｼﾞ先頭ｱﾄﾞﾚｽから) |
| [B] + ← | ページ移動モードへ |
|【ページ移動モード中】↑ or ↓ | メモリアドレス 0x40 分移動  |
|【ページ移動モード中】← or → | RAM (0x0000), SRAM(0x0600), ROM(0x8000) 間を移動 (0x0400 単位) |
| [B] + → | ニーモニック入力モードへ |

---

### ■ サンプル1 : はじめての機械語

```
0000:A9 88 8D 20 
0004:00 60
```

十字キーを押しながら ABボタン で数値が入力できます。  
Aボタンを押しながら 十字キー でカーソル移動ができます。  
(ややこしいですが、慣れてみて下さい。)  
これを入力後、SELECT で実行すると、 0020 番地に 88 が書き込まれることを確認できます。  
この機械語は、「88 を読み込み(A9)、0020 番地に書き込み(8D)、戻る(60)」という命令です。  

88 の部分を自由に変えてみて下さい。  
0020 番地に書き込まれる値が変化します。  

後述の "ステップ実行機能" と "ニーモニック入力機能" を使用することで、1命令ずつ動作を確認したり、別の命令を試したりすることもできます。

---

### ■ 機能1. ステップ実行 & レジスタ表示機能

![](_ReadMe/2_step.gif)

ファミメムエディタは 00 (BRK)命令に、ブレーク(一時停止)機能を実装してあります。  
なので、例えば各行の先頭に 00 命令を書いておくことで、1命令ごとの実行ができます。  
その際、レジスタ情報表示部が更新されるので、1命令ごとのレジスタの値の変化を確認することが出来ます。  

---

### ■ 機能2. ニーモニック入力機能

![](_ReadMe/3_mnemonic.gif)

機械語だけでなく、ニーモニックでも入力出来るようになっています。  
B を押しながら → を押下すると、ウインドウが開き、選択できるようになります。

表示は大きく3つのカテゴリで分類しており、

**読み書き** | **計算** | **移動**

で分けています。

---
### ■ サンプル2 : BPS社サウンドロゴ似の音

少し長いですが、実行すると音が鳴ります。

```
0000:A2 00 A0 00
0004:A5 24 91 20 
0008:B5 28 91 22 
000C:E8 C8 EA EA 
0010:E0 08 D0 F4 
0014:60 00 00 00 
0018:00 00 00 00 
001C:00 00 00 00
0020:15 40 00 40 
0024:03 00 00 00 
0028:B8 AF FF 03 
002C:B8 AF FC 03 
0030:00 00 00 00 
0034:00 00 00 00 
0038:00 00 00 00 
003C:00 00 00 00
```

上記の機械語をアセンブリで表すと、以下となります。

```LLVM
; x, y ﾚｼﾞｽﾀ に 0 代入。
ldx #$00
ldy #$00
; $24 から a ﾚｼﾞｽﾀ に読み込み、
; a ﾚｼﾞｽﾀ の値を $20 が指すｱﾄﾞﾚｽ($4015) + y 番地 に書き込む。
lda $24
sta ($20), y
; ここから繰り返し処理
-
    ; $28 + x 番地の値を a に読み込み、
    ; $22 が指すｱﾄﾞﾚｽ($4000) + y 番地 に書き込む。
    lda $28, x
    sta ($22), y
    ; x, y を 1足す。(nop は何もしない命令)
    ; "x != 8" だった場合、"-" まで戻る
    inx
    iny
    nop
    nop
    cpx #$08
bne -
; 実行元に戻る
rts

; パディング
.db    0, 0, 0
.db 0, 0, 0, 0
.db 0, 0, 0, 0

;---------------------------------------------
DATA_dst_Address:
    ; [$4015]ﾁｬﾝﾈﾙ有効ﾚｼﾞｽﾀ, [$4000]矩形波1 制御ﾚｼﾞｽﾀ
    .dw $4015, $4000
DATA_src_4015:
    ; [$4015] 矩形波1, 2有効 (0, 0, 0 は パディング)
    .db $03, 0, 0, 0
DATA_src_4000:
    ; [$4000-$4003] 矩形波1 [ 音色･音量 | ｽｲｰﾌﾟ | 周波数下位 | 周波数上位 (ｷｰｵﾝ/ｵﾌ) ]
    ; [$4004-$4007] 矩形波2 [ 音色･音量 | ｽｲｰﾌﾟ | 周波数下位 | 周波数上位 (ｷｰｵﾝ/ｵﾌ) ]
    .db $B8, $AF, $FF, $03 
    .db $B8, $AF, $FC, $03
```

---
参考 : [https://dic.nicovideo.jp/a/fc音源](https://dic.nicovideo.jp/a/fc音源)  

上記リンク先に書かれておりました
>10 POKE &H4015,3  
>20 POKE &H4000,&HB8,&HAF,255,3,&HB8,&HAF,252,3  

を、機械語で書き直したものです。

---

### ■ あとがき : このソフトで出来ること

ひらがなを読み書きし、足し算を覚え、体を動かす。  

人は早い段階でこれらに取り組みますが、コンピューターも似たようなもので、  
**読み書き** | **計算** | **移動**  
が動作の基本です。  
  
そんなコンピューターを制御する最も原始的な方法が「機械語を入力し実行する」ことです。  
ただし、現在の高機能化したコンピューターではその「機械語を入力し実行する」ことの敷居はずいぶんと高くなってしまいました。  
  
そこで、ファミコンと、このファミメムエディタです。  
  
ファミコンの CPU は Ricoh 2A03 という CPU です。  
これは、MOS 6502 という CPU が元となっています。(以後、ファミコンの CPU を 6502 と表記します。)  
6502 は非常にシンプルな CPU で、はじめて学ぶ CPU として相応しいです。  
機械語は 151 個。ニーモニックだと **56** 個です。  
8 bit CPU の機械語はいわば、コンピューター界の **ひらがな** と言えます。  

Linux を作った リーナス・トーバルズ 氏は、11 才のときに VIC-20 という 6502 で動くコンピューターに触れ、コンピューターを好きになったそうです。  
そして、凄腕プログラマであった 岩田聡 氏。PET 2001 という、同じく 6502 で動くコンピューターに入れ込んだ経緯があります。  
これは私の勝手な想像ですが、不思議と 6502 には人間にコンピューターを好きにさせる力があるように思えてなりません。  

さて、あなたもこのファミメムエディタで、  
コンピューターとのコミュニケーションを **"直接"** 味わってみませんか？

---

### ■ 更新履歴
- 2018/12/31(月) : 製作開始
- 2019/08/07(水) : 作り直し
- 2019/09/08(日) : 一通り完成。[はるきのへや.みんな](https://はるきのへや.みんな/1.ｹﾞ-ﾑ･技術/製作物/ﾌｧﾐﾒﾑｴﾃﾞｨﾀ/) で公開
- 2019/09/10(火) : github にアップロード (ReadMe.txt を修正した上で ReadMe.md に変更)
- 2019/09/16(月) : [追加] コピー&ペースト機能
- 2019/09/16(月) : v1.0 リリース
- 2019/09/18(水) : [追加] レジスタ表示機能
- 2019/10/06(日) : [追加] ステップ実行時、ページが切り替わるように
- 2019/10/22(日) : [追加] ディスアセンブル表示機能
- 2019/10/28(月) : [追加] ニーモニック入力機能
- 2019/12/01(日) : v2.0 リリース