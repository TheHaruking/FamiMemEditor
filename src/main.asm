;///////////////////////////////////////////
;■ 変数
; $0000
	_y			= $D0
	_y_View		= $D1
	_x			= $D2
	_x_View 	= $D3

	_base		= $D4
	_base1		= $D5
	_curaddr	= $D6
	_curaddr1	= $D7

	_BaseOfsIdx	= $D8	;// 0:0000, 1:6000, 2:8000
	_padMode 	= $D9
	_is_Hex_Changing = $DA
	_is_need_AllDraw = $DB

	_count		= $DC
	_count1		= $DD
	_pad1dir8	= $DE
	_reserve	= $DF

	_select_y		= $E0
	_selectN		= $E1
	_selectedAddr	= $E2
	_selectedAddr1	= $E3
	_is_pasted		= $E4
	_is_selecting	= $E5
	_select_y_View	= $E6
	_select_reserve = $E7

	_reg_a	= $E8
	_reg_x	= $E9
	_reg_y	= $EA
	_reg_s	= $EB
	_reg_pc = $EC
	_reg_pc1 = $ED
	_reg_p	= $EE

	; ボタン
	_pad1	= $F0
	_pad11	= $F1
	_pad12	= $F2
	_pad13	= $F3
	_pad2	= $F4
	_pad21	= $F5
	_pad22	= $F6
	_pad23	= $F7

	; ルーチンで使用
	_N_		= $F8 ; 8 bit の値を渡すのに使用
	_N1_	= $F9
	_MM_	= $FA ; 汎用
	_MM1_	= $FB
	_ADDR_	= $FC ; 16 bit のアドレスを渡すのに使用
	_ADDR1_ = $FD
	_SRCA_  = $FE ; 16 bit のアドレスを渡すのに使用
	_SRCA1_ = $FF
; $0100 : スタック
	STACK		EQU $0100
; $0200 : スプライトバッファ
	MEM_SP		EQU $0200
; $0300 : BGバッファ (8行分)
	MEM_BG0		EQU $0300
	MEM_BG1		EQU $0320
	MEM_BG2		EQU $0340
	MEM_BG3		EQU $0360
	MEM_BG4		EQU $0380
	MEM_BG5		EQU $03A0
	MEM_BG6		EQU $03C0
	MEM_BG7		EQU $03E0
; $0400 : BGバッファ (8行分)
	MEM_BG8		EQU $0400
	MEM_BG9		EQU $0420
	MEM_BG10	EQU $0440
	MEM_BG11	EQU $0460
	MEM_BG12	EQU $0480
	MEM_BG13	EQU $04A0
	MEM_BG14	EQU $04C0
	MEM_BG15	EQU $04E0

; $0500
	_copy_buf	= $500 ; $500-$53F
; $0600
; $0700

;■ 定数
	; ボタン (一般のファミコンゲームとは逆になっているが、こちらが好み。)
	pad_A		EQU #$01
	pad_B		EQU #$02
	pad_select	EQU #$04
	pad_start	EQU #$08
	pad_Up		EQU #$10
	pad_Down	EQU #$20
	pad_Left	EQU #$40
	pad_Right	EQU #$80

	Y_OFS		EQU $20 ; 上4行空けてカーソルを表示する際のオフセット値
	X_OFS		EQU $30 ; "0000:" の分を空けてカーソルを表示する際のオフセット値
	;
	; メモリアドレス
	SRAM	EQU $6000
	;
	; PPU アドレス
	NAMETABLE00 EQU $2000
	NAMETABLE01 EQU $2020
	NAMETABLE02 EQU $2040
	NAMETABLE03 EQU $2060
	NAMETABLE04 EQU $2080
	NAMETABLE05 EQU $20A0
	NAMETABLE06 EQU $20C0
	NAMETABLE07 EQU $20E0
	NAMETABLE08 EQU $2100
	NAMETABLE09 EQU $2120
	NAMETABLE10 EQU $2140
	NAMETABLE11 EQU $2160
	NAMETABLE12 EQU $2180
	NAMETABLE13 EQU $21A0
	NAMETABLE14 EQU $21C0
	NAMETABLE15 EQU $21E0
	NAMETABLE16 EQU $2200
	NAMETABLE17 EQU $2220
	NAMETABLE18 EQU $2240
	NAMETABLE19 EQU $2260
	NAMETABLE20 EQU $2280
	NAMETABLE21 EQU $22A0
	NAMETABLE22 EQU $22C0
	NAMETABLE23 EQU $22E0
	NAMETABLE24 EQU $2300
	NAMETABLE25 EQU $2320
	NAMETABLE26 EQU $2340
	NAMETABLE27 EQU $2360
	NAMETABLE28 EQU $2380
	NAMETABLE29 EQU $23A0
;///////////////////////////////////////////
; macro
macro SET_ADDR ADDR
	lda #<(ADDR)
	sta _ADDR_
	lda #>(ADDR)
	sta _ADDR_+1
endm
macro SET_ADDR_PTR ADDR
	lda ADDR
	sta _ADDR_
	lda ADDR+1
	sta _ADDR_+1
endm
macro SET_SRCA SRCA
	lda #<(SRCA)
	sta _SRCA_
	lda #>(SRCA)
	sta _SRCA_+1
endm
macro SET_SRCA_PTR SRCA
	lda SRCA
	sta _SRCA_
	lda SRCA+1
	sta _SRCA_+1
endm
macro SET_N N
	lda N
	sta _N_
endm
macro SET_MMPTR MM
	lda MM
	sta _MM_
	lda MM+1
	sta _MM_+1
endm
macro SET_ARGS ADDR,SRCA,N
	SET_ADDR ADDR
	SET_SRCA SRCA
	SET_N N
endm
macro PUSH_REG X,Y,ADDR,SRCA,N
	if X=1
		txa
		pha
	endif
	if Y=1
		tya
		pha
	endif
	if ADDR=1
		lda _ADDR_
		pha
		lda _ADDR_+1
		pha
	endif
	if SRCA=1
		lda _SRCA_
		pha
		lda _SRCA_+1
		pha
	endif
	if N=1
		lda _N_
		pha
	endif
endm

macro POP_REG X,Y,ADDR,SRCA,N
	if N=1
		pla
		sta _N_
	endif
	if SRCA=1
		pla
		sta _SRCA_+1
		pla
		sta _SRCA_
	endif
	if ADDR=1
		pla
		sta _ADDR_+1
		pla
		sta _ADDR_
	endif
	if Y=1
		pla
		tay
	endif
	if X=1
		pla
		tax
	endif
endm

macro LSR_n N
	rept N
		lsr
	endr
endm

macro ASL_n N
	rept N
		asl
	endr
endm

macro WAIT_VBLANK
	-	bit $2002
	bpl -
endm

macro SET_SPBUF N, y, chr, attr, x
	lda y		; Y座標
	sta (MEM_SP+(N*4)+0)
	lda chr		; キャラ番号
	sta (MEM_SP+(N*4)+1)
	lda attr	; 反転・優先順位
	sta (MEM_SP+(N*4)+2)
	lda x		; X座標
	sta (MEM_SP+(N*4)+3)
endm
;///////////////////////////////////////////
;// 関数
;
; 電源投入後最初に実行
RESET: 
	;
	; IRQ禁止セット・BDCクリア
	sei
	cld
	;
	; スタック初期化
	ldx #$FF
	txs
	;
	; PPU初期化
	lda #%00000000
    sta $2000  ; NMI を無効化
    sta $2001  ; 描画を無効化
    sta $4010  ; DMC IRQ有効ビット をクリア
	;
	; APU ﾌﾚｰﾑｶｳﾝﾀｰIRQ禁止ビット セット
	lda #%01000000
	sta $4017
	;	
	; メモリ初期化
	lda #0
	tax
	-	sta $000, x
		sta $100, x
		sta $200, x
		sta $300, x
		sta $400, x
		sta $500, x
		sta $600, x
		sta $700, x
		inx
	bne -
	;
	; VBlank 待機
	WAIT_VBLANK
	WAIT_VBLANK
	;
	; パレット読込
	lda #>$3F00
	sta $2006
	lda #<$3F00
	sta $2006
	ldx #0
	-	lda PALLET, x
		sta #$2007
		inx
		cpx #32
	bne -
	;
	; スクロール設定
	lda #0 ; X座標
	sta #$2005
	lda #0 ; Y座標
	sta #$2005
	;
	; 0番スプライト カーソル
	lda #$09 ; キャラ番号
	sta MEM_SP + 0*4 + 1
	jsr MoveCursor	
	;
	; PPU 有効化・NMI はそのまま
	lda #%00011110 ; 赤 緑 青 SP BG cS cB 色
	sta $2001
	;lda #%10000000 ; NMI ? SPsize BGbase SPbase BG縦横 MainScreen(2bit)
	;sta $2000
	jsr DrawInitialize
	jsr DrawHex16Lines
;
; メインループ
MainLoop:
	jsr GetJoyPad
	jsr Get8Direction
	;
	; 処理
	jsr SelectRoutine
	jsr CalcCursor
	;
	; カウンタ
	inc _count
	bne +
		inc _count+1
	+
	;
	; ■ 描画
	; 選択カーソル
	lda _is_selecting
	beq +
		jsr DrawSelectingCursor
		jmp ++
	+
	lda _pad1+2
	and #pad_start
	beq +
		jsr CleanSelectingCursor
	+
	;
	; メモリビュワー
		jsr DrawHex16Lines
		SET_N #0
		jsr Draw1Sprite
	++
	jmp MainLoop

;
; ボタンの組み合わせによって、実行するルーチンを分岐させる
SelectRoutine:
	;
	; 離された【次フレーム】に padMode をリセット
	lda _pad1
	ora _pad1+2
	bne +
		ldx #0
		stx _padMode
	+
	; _padMode に値をセット
	; 十字キー : 5, A B select start : 1 2 3 4
	lda _padMode
	bne ++
		lda _pad1
		and #$F0
		beq +
			lda #5
			sta _padMode
			jmp ++
		+
		lda _pad1
		and #$0F
		tax
		lda DATA_SelectRoutine, x
		sta _padMode
	++
	;
	; padMode に応じて分岐
	lda _padMode
	asl ; dw 配列を読むので 2 倍しておく
	tax
	lda DATA_SelectRoutine_func, x
	sta _ADDR_
	lda DATA_SelectRoutine_func+1, x
	sta _ADDR_+1
	jmp (_ADDR_)
	rts

; A:1, B:2, start:3, select:4, 十字キー:5
DATA_SelectRoutine
	.db 0, 1, 2, 0	; - A B -
	.db 3, 0, 0, 0	; e - - -     
	.db 4, 0, 0, 0	; S - - -   
	.db 0, 0, 0, 0	; - - - -  
	
DATA_SelectRoutine_func:
	.dw nonefunc, MoveCursor, ChangeBaseAddr, Exec, OtherFunc
	.dw EditHex ; 5:十時キー
;
;///////////////////////////////////////////

; コントローラーの情報を取得
;	pad1+0 : 現在の状態
;	pad1+1 : 押した瞬間
;	pad1+2 : 離した瞬間
;	pad1+3 : 1フレーム前 
GetJoyPad:
	;
	; 1フレーム前のボタン情報を保存
	lda _pad1
	sta _pad1+3
	lda _pad2
	sta _pad2+3
	;
	; ボタン取得 [A B select start ↑ ↓ ← →]
	lda #1
	sta $4016
	lda #0 
	sta $4016
	ldx #8
	-	lda $4016 ; 1P
		ror
		ror _pad1
		lda $4017 ; 2P
		ror
		ror _pad2
		dex
	bne -
	;
	; ボタン計算 (押した瞬間・離した瞬間 の算出)
	ldx #1
	ldy #0
	-	lda _pad1, y
		eor _pad1+3, y
		pha
		and _pad1, y
		sta _pad1+1, y
		pla
		and _pad1+3, y
		sta _pad1+2, y
		ldy #_pad2-#_pad1 ; インデックス計算
		dex
	bpl -
	rts

Get8Direction:
	lda _pad1
	LSR_n 4
	tax
	lda DATA_Get8Direction, x
	sta _pad1dir8
	rts

DATA_Get8Direction:
	.db $FF, $00, $04, $FF
	.db $06, $07, $05, $FF
	.db $02, $01, $03, $FF
	.db $FF, $FF, $FF, $FF

EditHex:
	;
	; 0 - 15 を取得
	lda _pad1+1
	ASL_n 3
	ora _pad1dir8
	and #$1F
	tax
	lda DATA_EditHex, x
	;
	; 十時キーを押していない, A B 押していない場合 return
	bpl +
		rts
	+
	;
	; 初回時 上位バイト4bit化
	ldx _is_Hex_Changing
	bne + ; 初回
		ASL_n 4
	+
	pha
	;
	; カーソル選択中アドレスに書き込む
	lda _curaddr
	sta _ADDR_
	lda _curaddr+1
	sta _ADDR_+1

	ldy #0
	lda (_ADDR_), y
	ldx _is_Hex_Changing
	bne + ; 初回
		and #$0F
		jmp ++
	+ ; 2回目
		and #$F0
	++
	tsx
	ora $100+1,x
	sta (_ADDR_), y
	pla

	lda _is_Hex_Changing
	beq + ; 2回目
		inc _x
	+
	eor #1
	sta _is_Hex_Changing
	rts

DATA_EditHex:
	.db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; 十時キーのみ (Return)
	.db $00,$02,$04,$06,$08,$0A,$0C,$0E ; A + 十時キー
	.db $01,$03,$05,$07,$09,$0B,$0D,$0F ; B + 十時キー
	.db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; AB + 十時キー, 十時キー押していない (Return)

MoveCursor:		;_x, _y の変更
	lda _pad1+1
	and #pad_Down
	beq +
		inc _y
	+
	lda _pad1+1
	and #pad_Up
	beq +
		dec _y
	+
	lda _pad1+1
	and #pad_Right
	beq +
		inc _x
	+
	lda _pad1+1
	and #pad_Left
	beq +
		lda _is_Hex_Changing
		bne ++
			dec _x
		++
	+
	;
	; _is_Hex_Changing を初期化
	lda _pad1+1
	and #$F0
	beq +
		lda #0
		sta _is_Hex_Changing
	+
	rts

CalcCursor:
	;
	; x 正規化
	lda _x
	cmp #255
	bne +
		dec _y
	+
	cmp #4
	bne +
		inc _y
	+
	and #3
	sta _x
	;
	; y 正規化
	lda _y
	and #15
	sta _y
	;
	; y, x カーソル座標 算出
	lda _y
	ASL_n 3
	clc
	adc #Y_OFS 	; 表示調整オフセットを足す
	sta _y_View
	dec _y_View	; スプライトは下に 1pxcel ずれる。さらに調整

	lda _x
	asl
	clc
	adc _x
	adc _is_Hex_Changing
	ASL_n 3
	clc
	adc #X_OFS
	sta _x_View
	;
	; カーソル処理
	lda _y_View ; Y座標
	sta MEM_SP + 0*4 + 0
	lda _x_View ; X座標
	sta MEM_SP + 0*4 + 3
	;
	; カレントアドレス計算
	lda _base
	sta _ADDR_
	lda _base+1
	sta _ADDR_+1
	lda _y		; _y * 4
	ASL_n 2
	clc			; + _x
	adc _x
	sta _N_
	jsr add_16_ADDR
	lda _ADDR_
	sta _curaddr
	lda _ADDR_+1
	sta _curaddr+1
	rts

ChangeBaseAddr:	; base アドレスの変更
	lda _pad1+1
	and #pad_Right
	beq ++
		lda _base
		clc
		adc #$40
		sta _base
		bcc +
			inc _base+1
		+
	++
	lda _pad1+1
	and #pad_Left
	beq ++
		lda _base
		sec
		sbc #$40
		sta _base
		bcs +
			dec _base+1
		+
	++

	lda _pad1+1
	and #pad_Down | #pad_Up
	beq ++
		lda _pad1+1
		and #pad_Down
		beq +
			inc _BaseOfsIdx
			lda _BaseOfsIdx
			cmp #3
			bmi +++ ; _BaseOfsIdx < 3 のときスキップ
				lda #0
				sta _BaseOfsIdx
			+++
		+
		lda _pad1+1
		and #pad_Up
		beq +
			dec _BaseOfsIdx
			bit _BaseOfsIdx
			bpl +++
				lda #2
				sta _BaseOfsIdx
			+++
		+

		; Base を既定アドレスに変更
		lda _BaseOfsIdx
		tax
		lda #$00
		sta _base
		lda DATA_ChangeBaseAddr, x
		sta _base+1
	++
	rts

DATA_ChangeBaseAddr:
	.db >$0000, >$6000, >$8000 ; 下バイトは全部 $00 なので、上だけで良い.

Exec: ; 実行
;
; 押した瞬間でなければ Return
	lda _pad1+1
	and #pad_select
	bne +
		rts
	+
;
; A,X,Y のクリア
	lda #0
	tax
	tay
	WAIT_VBLANK
	jmp (_base)

;
; コピペ, 設定画面など
OtherFunc:
	;
	; 初回押下時, _select_y を設定
	lda _pad1+1
	and #pad_start
	beq +
		lda #0
		sta _is_pasted
		lda #1
		sta _is_selecting
		jsr StartSelectRange
		rts
	+
	;
	; 離し時、コピーを実行 (ペースト実行後を除く)
	lda _pad1+2
	and #pad_start
	beq +
		lda _is_pasted
		bne ++
			jsr CopySelectRange
		++
		lda #0
		sta _is_selecting
		rts
	+
	;
	; 十時入力に応じた処理を実行
	lda _pad1+1
	and #$F0
	bne +
		rts
	+
	lda _pad1dir8
	bpl +
		rts
	+
	asl
	tax
	lda DATA_OtherFuncTable, x
	sta _ADDR_
	lda DATA_OtherFuncTable+1, x
	sta _ADDR_+1
	jmp (_ADDR_)

;
; 範囲選択開始
StartSelectRange:
	lda _y
	sta _select_y
	jsr calc_select_y_view
	rts
;
; 選択範囲コピー
CopySelectRange:
	jsr calc_SelectRenge
	SET_ADDR _copy_buf+1
	SET_SRCA_PTR _selectedAddr
	SET_N _selectN
	jsr memcpy

	lda _selectN
	sta _copy_buf
	rts
;
; 範囲選択 (↑)
SelectRangeU:
	lda _select_y
	cmp #$01
	bmi +
		dec _select_y
	+
	and #$0F
	jsr calc_select_y_view
	rts
;
; 範囲選択 (↓)
SelectRangeD:
	lda _select_y
	cmp #$0F
	bpl +
		inc _select_y
	+
	and #$0F
	jsr calc_select_y_view
	rts

;
; ペースト
SelectPaste:
	SET_ADDR_PTR _curaddr
	SET_SRCA _copy_buf+1
	SET_N _copy_buf
	jsr memcpy
	inc _is_pasted
	lda #0
	sta _is_selecting
	rts

;
; select_y カーソル座標算出
calc_select_y_view:
	;
	; y, x カーソル座標 算出
	lda _select_y
	ASL_n 3
	clc
	adc #Y_OFS 	; 表示調整オフセットを足す
	sta _select_y_View
	dec _select_y_View	; スプライトは下に 1pxcel ずれる。さらに調整
	rts

;
; 選択範囲算出
calc_SelectRenge:
	;
	; コピー範囲算出
	lda _select_y
	sec
	sbc _y
	;
	; 逆方向選択時
	bpl +
		;
		; 符号反転(xor #$FF + 1)
		eor #$FF
		clc
		adc #1
		;
		; 範囲バイト数算出
		ASL_n 2
		clc
		adc #4
		sta _selectN
		;
		; dst を _selectedAddr へ変更
		lda _select_y
		ASL_n 2
		clc
		adc _base
		sta _selectedAddr
		jmp ++
	;
	; 通常選択時
	+
		;
		; 範囲バイト数算出
		ASL_n 2
		clc
		adc #4
		sta _selectN
		;
		; dst を _selectedAddr へ変更
		lda _y
		ASL_n 2
		clc
		adc _base
		sta _selectedAddr
	++
	lda _base+1
	sta _selectedAddr+1
	rts

DATA_OtherFuncTable:
	.dw SelectRangeU, nonefunc, SelectPaste, nonefunc, SelectRangeD, nonefunc, nonefunc, nonefunc

nonefunc:
	rts
;
; 描画関係
DrawInitialize:
	;
	; スプライト初期化 (これがないと EverDrive 起動時、ゴミが表示される)
	WAIT_VBLANK
	jsr DrawAllSprites
	;
	; 事前描画 (タイトル)
	SET_ARGS MEM_BG0, DATA_TITLE+1, DATA_TITLE
	jsr memcpy32

	SET_ARGS MEM_BG1, DATA_Register+1, DATA_Register
	jsr memcpy32
	
	SET_ARGS MEM_BG7, DATA_HELP+1, DATA_HELP
	jsr memcpy32
	;
	; 描画
	SET_ARGS $2040, MEM_BG0, DATA_TITLE
	WAIT_VBLANK
	jsr DrawXLines
	jsr DrawScrollZero

	SET_ARGS $2280, MEM_BG1, DATA_Register
	WAIT_VBLANK
	jsr DrawXLines
	jsr DrawScrollZero

	SET_ARGS $2340, MEM_BG7, DATA_HELP
	WAIT_VBLANK
	jsr DrawXLines
	jsr DrawScrollZero
	;
	; メモリの後始末
	SET_ARGS MEM_BG0, #0, #0 ; SRCA はアドレスではなく、値として使用. memset の N は 0 指定で 256 扱い.
	jsr memset
	SET_ARGS MEM_BG8, #0, #0
	jsr memset

	rts

DATA_TITLE:
	.db 1 ; memcpy32 に渡す値
	.!hira "010101020203030303　　ふぁみめむ80えでぃた　　030303030202010101"

DATA_HELP
	.db 4
	;.!hira "Ａ＋じゅうじ：かーそるいどう　　Ｂ＋じゅうじ：あどれすへんこう　"
	.db $14,$2b,$1a,$1b,$19,$18,$3a,$8b,$2d,$9d,$cb,$84,$a9,$86,$00,$00,$17,$2b,$1a,$1b,$3a,$af,$d3,$84,$97,$a6,$84,$26,$93,$b4,$2d,$00
	.db $15,$2b,$19,$18,$3a,$ba,$2d,$98,$84,$a9,$86,$00,$00,$00,$00,$00,$17,$2b,$18,$3a,$ba,$2d,$99,$a8,$00,$00,$00,$00,$00,$00,$00,$00
	.db $7f,$2b,$14,$15,$3a,$ab,$c5,$86,$ca,$c7,$8f,$00,$00,$00,$00,$00,$16,$3a,$98,$a3,$93,$86,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

DATA_Register:
	.db 6
	.db $dc,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$dd,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $da,$41,$3a,$2d,$2d,$00,$e0,$3a,$2d,$2d,$2d,$2d,$00,$00,$00,$00,$00,$00,$00,$da,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $da,$58,$3a,$2d,$2d,$00,$00,$00,$4e,$56,$2d,$42,$2d,$49,$5a,$43,$00,$00,$00,$da,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $da,$59,$3a,$2d,$2d,$00,$50,$3a,$10,$10,$10,$10,$10,$10,$10,$10,$2d,$2d,$00,$da,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $da,$53,$3a,$2d,$2d,$3a,$5b,$2d,$2d,$00,$2d,$2d,$20,$2d,$2d,$20,$2d,$2d,$5d,$da,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $de,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$d9,$df,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

DrawHex16Lines:
	;
	; BG バッファを初期化
	SET_ARGS MEM_BG0, #0, #0 ; 256バイト扱い
	jsr memset

	; #$2080, _base
	SET_ADDR $2080
	SET_SRCA_PTR _base
	jsr DrawHex8Lines

	; 次のアドレス
	inc _ADDR_+1 ; #$2180
	SET_N #$20
	jsr add_16_SRCA

	; #$2180, _base+$20
	jsr DrawHex8Lines
	rts

; _ADDR_ : Nametable
; _SRCA_ : _base
; _MM_ : 破壊
DrawHex8Lines:
	PUSH_REG 0,0,1,1,0

	; 退避
	SET_MMPTR _ADDR_
	
	; 準備
	SET_ADDR MEM_BG0
;	SET_SRCA_PTR _base ; 引数で入力済み
	jsr BufDraw_addr_hex_32x8

	; 描画
	SET_ADDR_PTR _MM_ ; ★ #$2080 [2回目:#$2180]

	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero

	POP_REG 0,0,1,1,0
	rts

DrawSelectingCursor:
	SET_SPBUF 1, _y_View, #$18, #0, #5*8
	SET_SPBUF 2, _y_View, #$19, #0, #17*8
	SET_SPBUF 3, _select_y_View, #$18, #0, #5*8
	SET_SPBUF 4, _select_y_View, #$19, #0, #17*8

	WAIT_VBLANK
	jsr DrawAllSprites 
	rts

CleanSelectingCursor:
	SET_SPBUF 1, #0, #0, #0, #0
	SET_SPBUF 2, #0, #0, #0, #0
	SET_SPBUF 3, #0, #0, #0, #0
	SET_SPBUF 4, #0, #0, #0, #0
	WAIT_VBLANK
	jsr DrawAllSprites 
	rts


;///////////////////////////////////////////
; 汎用関数
; _ADDR_ : 対象アドレス
; _SRCR_ : 値 (アドレスではない)
; _N_ : サイズ (0-255)
memset:
	PUSH_REG 0,1,0,0,0

	lda _SRCA_
	ldy _N_
	-	sta (_ADDR_), y
		dey
	bne -

	POP_REG 0,1,0,0,0
	rts

; _ADDR_ : 対象アドレス
; _SRCA_ : ソースアドレス
; _N_ : サイズ
memcpy:
	PUSH_REG 1,1,0,0,0

	ldy #0
	ldx _N_
	-	lda (_SRCA_), y
		sta (_ADDR_), y
		iny
		dex
	bne -

	POP_REG 1,1,0,0,0
	rts

; _ADDR_ : 対象アドレス
; _SRCA_ : ソースアドレス
; _N_ : サイズ (32バイト単位) (0 の場合 256 扱い)
memcpy32:
	PUSH_REG 1,1,1,1,1
	
	ldx _N_
	-
		ldy #0
	rept 32
		lda (_SRCA_), y
		sta (_ADDR_), y
		iny
	endr
		lda #32
		sta _N_
		jsr add_16_ADDR
		jsr add_16_SRCA

		dex
		beq +
	jmp -
	+

	POP_REG 1,1,1,1,1
	rts

; _ADDR_ : 対象
; _N_ : 足す数
add_16_ADDR:
	lda _ADDR_
	clc
	adc _N_
	sta _ADDR_
	bcc +
		inc _ADDR_+1
	+
	rts

; _ADDR_ : 
; _N_ : 足す数
add_16_SRCA:
	lda _SRCA_
	clc
	adc _N_
	sta _SRCA_
	bcc +
		inc _SRCA_+1
	+
	rts

DrawScrollZero:
	lda #0 ; X座標
	sta #$2005
	lda #0 ; Y座標
	sta #$2005
	rts

; _ADDR_ : 対象バッファ
; _SRCA_ : ソースメモリ
BufDraw_addr_hex_32x8:
	PUSH_REG 1,1,1,1,1

	ldx #8
	ldy #0
	-
		SET_N #1 ; 1つ空ける
		jsr add_16_ADDR

		jsr BufDraw_addr_2bytes ;(+5)
		jsr BufDraw_hex_4bytes ;(+12)

		SET_N #(32-(1+5+12))
		jsr add_16_ADDR

		SET_N #4
		jsr add_16_SRCA

		dex
	bne -

	POP_REG 1,1,1,1,1
	rts

; _ADDR_ : 書き込みアドレス (!! 返却 : +5 !!)
; _SRCA_ : 直接読みこみ文字列化する
BufDraw_addr_2bytes:
	PUSH_REG 0,1,0,0,1
	;
	; 0000 (アドレス部分)
	SET_N _SRCA_+1
	jsr bin2hex
	SET_N #2
	jsr add_16_ADDR
	SET_N _SRCA_
	jsr bin2hex
	SET_N #2
	jsr add_16_ADDR

	lda #$3A ; ':'
	ldy #0
	sta (_ADDR_), y

	SET_N #1
	jsr add_16_ADDR

	POP_REG 0,1,0,0,1
	rts

; 4バイト分実行 (00 00 00 00 )
; _ADDR_ : 書き込み先アドレス (!! 返却 : +12 !!)
; _SRCA_ : 読みこみアドレス
BufDraw_hex_4bytes:
	PUSH_REG 1,1,0,0,1

	ldx #4
	ldy #0
	-
		lda (_SRCA_), y
		sta _N_
		jsr bin2hex

		SET_N #3
		jsr add_16_ADDR

		iny
		dex
	bne -

	POP_REG 1,1,0,0,1
	rts

; 説明 : _N_ の値を ASCII 2バイトに変換し、指定アドレスに書き込む。
; _ADDR_ : 指定アドレス
; _N_  : 値 
bin2hex:
	PUSH_REG 0,1,0,0,0

	lda _N_
	and #$0F
	tay
	lda DATA_BIN2HEX, y
	ldy #1
	sta (_ADDR_), y

	lda _N_
	LSR_n 4
	tay
	lda DATA_BIN2HEX, y
	ldy #0
	sta (_ADDR_),y

	POP_REG 0,1,0,0,0
	rts

DATA_BIN2HEX:	.db "0123456789ABCDEF"

;///////////////////////////////////////////
; _ADDR_ : NAMETABLE
Draw8Lines:
if 0	
	; バランス ver
	PUSH_REG 1,1,0,0,0

	lda _ADDR_+1
	sta #$2006
	lda _ADDR_
	sta #$2006

	ldx #15
	-
		ldy Draw8Lines_table, x ; MEM_BG の インデックス
		i=0
	    REPT 16
			lda MEM_BG0+i, y
			sta #$2007
			i=i+1
	    ENDR
		dex
	bpl -

	POP_REG 1,1,0,0,0
	rts

	Draw8Lines_table:
		.db 240,224,208,192,176,160,144,128,112,96,80,64,48,32,16,0
else
	; 最大速度ver
	lda _ADDR_+1
	sta #$2006
	lda _ADDR_
	sta #$2006

	i=0
	REPT 256
		lda MEM_BG0+i
		sta #$2007
		i=i+1
	ENDR
	rts
endif

; _ADDR_ : NameTable
; _SRCA_ : MEM_BG
; _N_    : 行数 (最大:約6行)
DrawXLines:
	PUSH_REG 1,1,0,0,0

	lda _ADDR_+1
	sta #$2006
	lda _ADDR_
	sta #$2006
	ldx _N_
	ldy #0 ; MEM_BG の インデックス
	-
		dex
		bpl +
			jmp ++
		+
	    REPT 32
			lda (_SRCA_), y
			sta #$2007
			iny
	    ENDR
	jmp -
	++

	POP_REG 1,1,0,0,0
	rts

; _N_ : spr id
Draw1Sprite:
	PUSH_REG 0,1,0,0,0

	lda _N_
	sta $2003
	ASL_n 2
	tay ; インデックスを計算

	lda MEM_SP, y ; Y座標
	sta $2004
	lda MEM_SP+1, y ; キャラ番号
	sta $2004
	lda MEM_SP+2, y ; 反転・優先順位
	sta $2004
	lda MEM_SP+3, y ; X座標
	sta $2004

	POP_REG 0,1,0,0,0
	rts

DrawAllSprites:
	lda #>MEM_SP
	sta $4014
	rts

;///////////////////////////////////////////
; https://www.wizforest.com/tech/Z80vs6502/;p1#LoopAdd
One2Handlet:
	lda #0
	sta $00
	sta $01
	sta $02

	lda #100	; 2
	sta $02		; 3
	lda #0		; 2
	tax			; 2
	clc			; 2
	-
		adc $02	; 3
		bcc +	; 3 / 2
			inx		; 2
			clc		; 2
		+
		dec $02	; 5
	bne -		; 3 / 2
	sta $00		; 3
	stx $01		; 3
	rts

;///////////////////////////////////////////
NMI:
	pha
	PUSH_REG 1,1,0,0,0
	;
	; 処理
	nop

	POP_REG 1,1,0,0,0
	pla
	rti

;///////////////////////////////////////////
BREAK:
	sta _reg_a
	stx _reg_x
	sty _reg_y
	tsx
	stx _reg_s
	lda STACK+1, x
	sta _reg_p
	;
	; ■ rti 戻り先補正
	; BREAK 命令の戻りアドレスは +1 される.
	; 補正のため, スタック2番目のPC下位バイトに対し dec する.
	; 参考図 : [ P, <PC0>, PC1 ]
	tsx
	dec STACK+2, x
	lda #$FF
	cmp STACK+2, x
	bne +
		dec STACK+3, x
	+

	; 
	lda STACK+2, x
	sta _reg_pc
	lda STACK+3, x
	sta _reg_pc+1
	dec _reg_pc

	;
	; レジスタ情報表示
	jsr DrawRegisters

	;
	; ボタン入力待機
	;-
	;	jsr GetJoyPad
	;	WAIT_VBLANK
	;	lda _pad1+1
	;	and #pad_start
	;beq -

	ldy _reg_y
	ldx _reg_x
	lda _reg_a
	rti

;///////////////////////////////////////////
DrawRegisters:
	;
	; 枠組み描画
	SET_ARGS MEM_BG0, DATA_Register+1, DATA_Register
	jsr memcpy32
	;
	; A レジスタ
	SET_ADDR MEM_BG1+3
	SET_N _reg_a
	jsr bin2hex
	;
	; X レジスタ
	SET_ADDR MEM_BG2+3
	SET_N _reg_x
	jsr bin2hex
	;
	; Y レジスタ
	SET_ADDR MEM_BG3+3
	SET_N _reg_y
	jsr bin2hex
	;
	; S レジスタ [上位4 バイト]
	SET_ADDR MEM_BG4+3
	SET_N _reg_s
	jsr bin2hex

	ldx _reg_s
	i=0
	rept 4
		SET_ADDR MEM_BG4 + 7+(i*3)
		lda STACK+i, x
		sta _N_
		jsr bin2hex
		i=i+1
	endr
	;
	; PC レジスタ
	SET_ADDR MEM_BG1+8
	SET_N _reg_pc1
	jsr bin2hex
	SET_ADDR MEM_BG1+10
	SET_N _reg_pc
	jsr bin2hex
	;
	; P レジスタ XX
	SET_ADDR MEM_BG3+16
	SET_N _reg_p
	jsr bin2hex
	;
	; P レジスタ ○○○○○○○○
	lda #$01
	sta _N_ ; AND ビット
	ldx #7 ; カウンタ
	-
		lda _N_
		bit _reg_p
		beq +
			lda DATA_StatusBitColor, x
			sta MEM_BG3+8, x
		+
		asl _N_
		dex
	bpl -
	;
	; 描画
	SET_ARGS NAMETABLE20, MEM_BG0, DATA_Register
	WAIT_VBLANK
	WAIT_VBLANK
	jsr DrawXLines
	jsr DrawScrollZero
	rts

DATA_StatusBitColor:
	.db $13,$13,$12,$12,$12,$12,$13,$13

;０１２３４５６７８９０１２３４５６７８
;｜Ａ：ＸＸ　PC：ＸＸＸＸ　　　　　　　
;｜Ｘ：ＸＸ　　　ＮＶ－Ｂ－ＩＺＣ　　　
;｜Ｙ：ＸＸ　Ｐ：●●●●●●●●ＸＸ　
;｜Ｓ：ＸＸ　［ＸＸ　ＸＸ　ＸＸ　ＸＸ］