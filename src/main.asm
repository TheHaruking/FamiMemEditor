;*****************************************************************************;
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
	_reg_pc_last_40 = $E7

	_reg_a	= $E8
	_reg_x	= $E9
	_reg_y	= $EA
	_reg_s	= $EB
	_reg_pc = $EC
	_reg_pc1 = $ED
	_reg_p	= $EE
	_reg_pc_last = $EF

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
	_MM_	= $FA ; 汎用 (Switch ルーチンのジャンプアドレス)
	_MM1_	= $FB
	_ADDR_	= $FC ; 16 bit のアドレスを渡すのに使用
	_ADDR1_ = $FD
	_SRCA_  = $FE ; 16 bit のアドレスを渡すのに使用
	_SRCA1_ = $FF
; $0100 : スタック
	STACK		EQU $0100
; $0200 : スプライトバッファ
	MEM_SP0		EQU $0200
	MEM_SP1		EQU $0204
	MEM_SP2		EQU $0208
	MEM_SP3		EQU $020C
	MEM_SP4		EQU $0210
	MEM_SP5		EQU $0214
	MEM_SP6		EQU $0218
	MEM_SP7		EQU $021C
	MEM_SP8		EQU $0220
	MEM_SP9		EQU $0224
	MEM_SP10	EQU $0228
	MEM_SP11	EQU $022C
	MEM_SP12	EQU $0230
	MEM_SP13	EQU $0234
	MEM_SP14	EQU $0238
	MEM_SP15	EQU $023C
	MEM_SP16	EQU $0240
	MEM_SP17	EQU $0244
	MEM_SP18	EQU $0248
	MEM_SP19	EQU $024C
	MEM_SP20	EQU $0250
	MEM_SP21	EQU $0254
	MEM_SP22	EQU $0258
	MEM_SP23	EQU $025C
	MEM_SP24	EQU $0260
	MEM_SP25	EQU $0264
	MEM_SP26	EQU $0268
	MEM_SP27	EQU $026C
	MEM_SP28	EQU $0270
	MEM_SP29	EQU $0274
	MEM_SP30	EQU $0278
	MEM_SP31	EQU $027C
	MEM_SP32	EQU $0280
	MEM_SP33	EQU $0284
	MEM_SP34	EQU $0288
	MEM_SP35	EQU $028C
	MEM_SP36	EQU $0290
	MEM_SP37	EQU $0294
	MEM_SP38	EQU $0298
	MEM_SP39	EQU $029C
	MEM_SP40	EQU $02A0
	MEM_SP41	EQU $02A4
	MEM_SP42	EQU $02A8
	MEM_SP43	EQU $02AC
	MEM_SP44	EQU $02B0
	MEM_SP45	EQU $02B4
	MEM_SP46	EQU $02B8
	MEM_SP47	EQU $02BC
	MEM_SP48	EQU $02C0
	MEM_SP49	EQU $02C4
	MEM_SP50	EQU $02C8
	MEM_SP51	EQU $02CC
	MEM_SP52	EQU $02D0
	MEM_SP53	EQU $02D4
	MEM_SP54	EQU $02D8
	MEM_SP55	EQU $02DC
	MEM_SP56	EQU $02E0
	MEM_SP57	EQU $02E4
	MEM_SP58	EQU $02E8
	MEM_SP59	EQU $02EC
	MEM_SP60	EQU $02F0
	MEM_SP61	EQU $02F4
	MEM_SP62	EQU $02F8
	MEM_SP63	EQU $02FC

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

; $0500 :
; $0600
; $0700
	_copy_buf	= $700 ; $700-$73F

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
; 座標計算
	Y_OFS		EQU $20 ; 上4行空けてカーソルを表示する際のオフセット値
	X_OFS		EQU $2F ; "0000:" の分 - 文字との右間隔 を空けてカーソルを表示する際のオフセット値

;■ メモリアドレス
; $6000 : SRAM
	SRAM	EQU $6000
; $2000 : NAMETABLE
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

;*****************************************************************************;
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
		lda _MM_
		pha
		lda _MM_+1
		pha
	endif
endm

macro POP_REG X,Y,ADDR,SRCA,N
	if N=1
		pla
		sta _MM_+1
		pla
		sta _MM_
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
;
; 事前に tsx すること
macro LOAD_STACK N, MMMM
	lda STACK+N, x
	sta MMMM
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
	sta (MEM_SP0+(N*4)+0)
	lda chr		; キャラ番号
	sta (MEM_SP0+(N*4)+1)
	lda attr	; 反転・優先順位
	sta (MEM_SP0+(N*4)+2)
	lda x		; X座標
	sta (MEM_SP0+(N*4)+3)
endm
;*****************************************************************************;
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
	; 00, 01 の BRK で停止するようにしておく
	lda #$FC
	sta _reg_pc
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
	sta MEM_SP0 + 1
	jsr MoveCursor	
	;
	; PPU 有効化・NMI はそのまま
	lda #%00011110 ; 赤 緑 青 SP BG cS cB 色
	sta $2001
	;lda #%10000000 ; NMI ? SPsize BGbase SPbase BG縦横 MainScreen(2bit)
	;sta $2000
	jsr DrawInitialize
	SET_SRCA_PTR _base
	jsr Draw_MemoryViewer
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
	; カーソル
	lda #$09 ; キャラ番号
	sta MEM_SP0 + 1
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
		SET_SRCA_PTR _base
		jsr Draw_MemoryViewer
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
	ASL_n 1 ; dw 配列を読むので 2 倍しておく
	tax
	lda DATA_SelectRoutine_func, x
	sta _MM_
	lda DATA_SelectRoutine_func+1, x
	sta _MM_+1
	jmp (_MM_)
DATA_SelectRoutine_func:
	.dw nonefunc, MoveCursor, ChangeBaseAddr, Exec, OtherFunc
	.dw EditHex ; 5:十時キー

; A:1, B:2, start:3, select:4, 十字キー:5
DATA_SelectRoutine
	.db 0, 1, 2, 0	; - A B - 
	.db 3, 0, 0, 0	; e - - - 
	.db 4, 0, 0, 0	; S - - - 
	.db 0, 0, 0, 0	; - - - - 
;
;*****************************************************************************;

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
	ASL_n 1
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
	sta MEM_SP0 + 0
	lda _x_View ; X座標
	sta MEM_SP0 + 3
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
; A,X,Y をクリアしてから実行
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
	ASL_n 1
	tax
	lda DATA_OtherFuncTable, x
	sta _MM_
	lda DATA_OtherFuncTable+1, x
	sta _MM_+1
	jmp (_MM_)
DATA_OtherFuncTable:
	;
	; ↑ ↗ → ↘ ↓ ↙ ← ↖
	.dw SelectRangeU, nonefunc, SelectPaste, nonefunc, SelectRangeD, nonefunc, nonefunc, nonefunc
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
	;
	; ペースト分カーソルを移動
	; (CulcCorsor 前なので _curaddr でなく _y に対して処理)
	lda _copy_buf
	LSR_n 2
	clc
	adc _y
	sta _y
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
	jsr memcpy16

	SET_ARGS MEM_BG1, DATA_Register+1, DATA_Register
	jsr memcpy16
	
	SET_ARGS MEM_BG7, DATA_HELP+1, DATA_HELP
	jsr memcpy16
	;
	; 描画
	SET_ARGS $2040, MEM_BG0, DATA_TITLE
	WAIT_VBLANK
	jsr Draw16XTiles
	jsr DrawScrollZero

	SET_ARGS $2280, MEM_BG1, DATA_Register
	WAIT_VBLANK
	jsr Draw16XTiles
	jsr DrawScrollZero

	SET_ARGS $2340, MEM_BG7, DATA_HELP
	WAIT_VBLANK
	jsr Draw16XTiles
	jsr DrawScrollZero
	;
	; メモリの後始末
	SET_ARGS MEM_BG0, #0, #0 ; SRCA はアドレスではなく、値として使用. memset の N は 0 指定で 256 扱い.
	jsr memset
	SET_ARGS MEM_BG8, #0, #0
	jsr memset

	rts

DATA_TITLE:
	.db (1) *2 ; memcpy16 に渡す値 (行数)*2
	.!hira "010101010202020303　　ふぁみめむ80えでぃた　　030302020201010101"

DATA_Register: ; 横幅 19
	.db (6) *2 ; memcpy16 に渡す値 (行数)*2
	.db $1c,$cc,$98,$99,$9f,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$41,$3a,$2d,$2d,$00,$ff,$3a,$2d,$2d,$2d,$2d,$00,$00,$00,$00,$00,$00,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$58,$3a,$2d,$2d,$00,$00,$00,$4e,$56,$52,$42,$44,$49,$5a,$43,$00,$00,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$59,$3a,$2d,$2d,$00,$50,$3a,$10,$10,$10,$10,$10,$10,$10,$10,$2d,$2d,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$53,$3a,$2d,$2d,$5b,$2d,$2d,$00,$2d,$2d,$20,$2d,$2d,$20,$2d,$2d,$5d,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1e,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

DATA_HELP
	.db (4) *2 ; memcpy16 に渡す値 (行数)*2
	;.!hira "Ａ＋じゅうじ：かーそるいどう　　Ｂ＋じゅうじ：あどれすへんこう　"
	.db $d4,$2b,$d8,$d9,$da,$db,$3a,$8b,$2d,$9d,$cb,$84,$a9,$86,$00,$00,$d7,$2b,$da,$db,$3a,$af,$d3,$84,$97,$a6,$84,$26,$93,$b4,$2d,$00
	.db $d5,$2b,$d8,$d9,$00,$00,$3a,$ba,$2d,$98,$84,$a9,$86,$00,$00,$00,$d7,$2b,$d9,$00,$3a,$ba,$2d,$99,$a8,$00,$00,$00,$00,$00,$00,$00
	.db $7f,$2b,$d4,$d5,$00,$00,$3a,$ab,$c5,$86,$ca,$c7,$8f,$00,$00,$00,$d6,$00,$00,$00,$3a,$98,$a3,$93,$86,$00,$00,$00,$00,$00,$00,$00
	.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

; _SRCA_ : 表示させたいメモリ領域の先頭アドレス
; 描画バッファを実際に描画する
; 備考 : 30fps
Draw_MemoryViewer:
	PUSH_REG 0,0,1,1,0
	;
	;   (MEM_BG0 はここでバッファクリア)
	SET_ADDR MEM_BG0
	SET_SRCA #0
	SET_N #16
	jsr memset16
	;
	; ■ 1-1.BUF描画1 (memory 0x0000)
	SET_ADDR MEM_BG0
;	SET_SRCA_PTR _base
	tsx
	LOAD_STACK 1, _SRCA_+1
	LOAD_STACK 2, _SRCA_
	jsr BufDraw_addr_hex_32x8
	;
	;   1-2.BUF描画1 (disasm)
	SET_ADDR MEM_BG0+19
	lda _curaddr
	;and #$FC
	sta _SRCA_
	jsr BufDraw_Asm_32x16
	;
	;   1-3.実描画1 (0-7)
	SET_ADDR NAMETABLE04
	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero
	;
	; ■ 2-1.BUF描画2 (mem 0x0020)
	SET_ADDR MEM_BG8
	tsx
	LOAD_STACK 1, _SRCA_+1
	LOAD_STACK 2, _SRCA_
	SET_N #$20
	jsr add_16_SRCA
	jsr BufDraw_addr_hex_32x8	
	;
	;   (Draw8Lines は MEM_BG0 からの転送しか対応していない。)
	SET_ARGS MEM_BG0, MEM_BG8, #16
	jsr memcpy16
	;
	;   (MEM_BG8 はここでバッファクリア)
	SET_ADDR MEM_BG8
	SET_SRCA #0
	SET_N #16
	jsr memset16
	;
	;   2-2.実描画2 (8-15)
	SET_ADDR NAMETABLE12
	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero
	SET_N #0
	jsr Draw1Sprite

	POP_REG 0,0,1,1,0
	rts

DrawSelectingCursor:
	SET_SPBUF 1, _y_View, #$D9, #0, #5*8 ; →
	SET_SPBUF 2, _y_View, #$D8, #0, #17*8 ; ←
	SET_SPBUF 3, _select_y_View, #$D9, #0, #5*8 ; →
	SET_SPBUF 4, _select_y_View, #$D8, #0, #17*8 ; ←

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


;*****************************************************************************;
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
; _SRCR_ : 値 (アドレスではない)
; _N_ : サイズ (16バイト単位) (max : 8)
memset16:
	PUSH_REG 1,1,0,0,0
	
	ldx _N_
	lda _SRCA_
	ldy #0
	-
		rept 16
			sta (_ADDR_), y
			iny
		endr
		dex
	bne -

	POP_REG 1,1,0,0,0
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
; _N_ : サイズ (16バイト単位) (max : 16)
memcpy16:
	PUSH_REG 1,1,0,0,0
	
	ldx _N_
	ldy #0
	-
		rept 16
			lda (_SRCA_), y
			sta (_ADDR_), y
			iny
		endr

		dex
	bne -

	POP_REG 1,1,0,0,0
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
;******************************************************************************
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

; _ADDR_ : 対象バッファ
; _SRCA_ : disasm 対象先頭メモリアドレス
BufDraw_Asm_32x16
	PUSH_REG 1,1,1,1,0

	ldx #16
	ldy #0
	-
		lda (_SRCA_),y
		sta _N_
		jsr bin2asm

		SET_N #5
		jsr add_16_ADDR

		jsr bin2adressing

		SET_N _N_+1
		jsr add_16_SRCA
 		SET_N #27
		jsr add_16_ADDR
		dex
	bne -

	POP_REG 1,1,1,1,0
	rts

; 説明 : _N_ の値を ASM文字 4バイトに変換し、指定アドレスに書き込む。
; _ADDR_ : 指定アドレス
; _N_  : 値 
bin2asm:
	PUSH_REG 1,1,0,0,0

	lda _N_
	tay
	lda DATA_BIN2ASMID, y
	ASL_n 2
	tax
	ldy #0
	rept 4
		lda DATA_ASMSTR, x
		sta (_ADDR_), y
		iny
		inx
	endr

	POP_REG 1,1,0,0,0
	rts

; 説明 : _SRCA_ の指す値を元に、その ASM に対応する Adressing 文字列 6バイトに変換し、指定アドレスに書き込む。
; _ADDR_ : 文字列を書き込むアドレス
; _SRCA_ : ソースアドレス
; 返り値(_N_+1) : 読み取りバイト数
bin2adressing:
	PUSH_REG 1,1,1,0,1
	;
	; ASM を読む
	ldy #0
	lda (_SRCA_), y
	;
	; アドレッシングに応じてジャンプ
	tax
	lda DATA_DISASMADRESSING, x
	;
	; 先に読み取りバイト数を返り値にセットしておく
	tax
	pha
	lda DATA_bin2adressing_bytes, x
	sta _N_+1
	;
	; アドレッシングに応じてジャンプ (続き)
	pla
	ASL_n 1
	tax
	lda DATA_bin2adressing ,x
	sta _MM_
	lda DATA_bin2adressing+1, x
	sta _MM_+1
	jmp (_MM_)
DATA_bin2adressing:
	.dw bin2adressing_n, bin2adressing_imm, bin2adressing_zp, bin2adressing_addr
	.dw bin2adressing_zpX, bin2adressing_zpY, bin2adressing_addrX, bin2adressing_addrY
	.dw bin2adressing_indX, bin2adressing_indY, bin2adressing_ind, bin2adressing_unknown
DATA_bin2adressing_bytes:
	.db 1, 2, 2, 3
	.db 2, 2, 3, 3
	.db 2, 2, 3, 1
; 00:      04:00,x    08:(00,x)
; 01:#00   05:00,y    09:(00),y
; 02:00    06:0000,x  10:(0000)
; 03:0000  07:0000,y
bin2adressing_n:
	jmp bin2adressing_END
bin2adressing_imm:
	ldy #0
	lda #'#'
	sta (_ADDR_), y

	ldy #1
	lda #'$'
	sta (_ADDR_), y

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	jmp bin2adressing_END
bin2adressing_zp:
	ldy #0
	lda #'$'
	sta (_ADDR_), y
	
	SET_N #1
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	jmp bin2adressing_END
bin2adressing_addr:
	ldy #0
	lda #'$'
	sta (_ADDR_), y

	SET_N #1
	jsr add_16_ADDR

	ldy #2
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex
	jmp bin2adressing_END
bin2adressing_zpX:
	ldy #0
	lda #'$'
	sta (_ADDR_), y

	ldy #3
	lda #$2C ; ,
	sta (_ADDR_), y

	ldy #4
	lda #'X'
	sta (_ADDR_), y

	SET_N #1
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex
	jmp bin2adressing_END
bin2adressing_zpY:
	ldy #0
	lda #'$'
	sta (_ADDR_), y

	ldy #3
	lda #$2C ; ,
	sta (_ADDR_), y

	ldy #4
	lda #'Y'
	sta (_ADDR_), y

	SET_N #1
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex
	jmp bin2adressing_END
bin2adressing_addrX:
	ldy #0
	lda #'$'
	sta (_ADDR_), y

	ldy #5
	lda #$2C ; ,
	sta (_ADDR_), y

	ldy #6
	lda #'X'
	sta (_ADDR_), y

	SET_N #1
	jsr add_16_ADDR

	ldy #2
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex
	jmp bin2adressing_END
bin2adressing_addrY:
	ldy #0
	lda #'$'
	sta (_ADDR_), y

	ldy #5
	lda #$2C ; ,
	sta (_ADDR_), y

	ldy #6
	lda #'Y'
	sta (_ADDR_), y

	SET_N #1
	jsr add_16_ADDR

	ldy #2
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex
	jmp bin2adressing_END
bin2adressing_indX:
	ldy #0
	lda #$28 ; (
	sta (_ADDR_), y

	ldy #1
	lda #'$'
	sta (_ADDR_), y

	ldy #4
	lda #$2C ; ,
	sta (_ADDR_), y

	ldy #5
	lda #'X'
	sta (_ADDR_), y

	ldy #6
	lda #$29 ; )
	sta (_ADDR_), y

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	jmp bin2adressing_END
bin2adressing_indY:
	ldy #0
	lda #$28 ; (
	sta (_ADDR_), y
	
	ldy #1
	lda #'$'
	sta (_ADDR_), y

	ldy #4
	lda #$29 ; )
	sta (_ADDR_), y

	ldy #5
	lda #$2C ; ,
	sta (_ADDR_), y

	ldy #6
	lda #'Y'
	sta (_ADDR_), y

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex
	jmp bin2adressing_END
bin2adressing_ind:
	ldy #0
	lda #$28 ; (
	sta (_ADDR_), y

	ldy #1
	lda #'$'
	sta (_ADDR_), y

	ldy #6
	lda #$29 ; )
	sta (_ADDR_), y

	SET_N #2
	jsr add_16_ADDR

	ldy #2
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	SET_N #2
	jsr add_16_ADDR

	ldy #1
	lda (_SRCA_), y
	sta _N_
	jsr bin2hex

	jmp bin2adressing_END
bin2adressing_unknown:
	jmp bin2adressing_n
bin2adressing_END:
	POP_REG 1,1,1,0,1
	rts

DATA_BIN2ASMID:
	;	 0   1   2   3    4   5   6   7    8   9   A   B    C   D   E   F
	.db $26,$11,$38,$38, $38,$11,$15,$38, $0D,$11,$15,$38, $38,$11,$15,$38
	.db $2A,$11,$38,$38, $38,$11,$15,$38, $34,$11,$38,$38, $38,$11,$15,$38
	.db $24,$10,$38,$38, $22,$10,$17,$38, $0F,$10,$17,$38, $22,$10,$17,$38
	.db $2B,$10,$38,$38, $38,$10,$17,$38, $35,$10,$38,$38, $38,$10,$17,$38

	.db $27,$12,$38,$38, $38,$12,$16,$38, $0C,$12,$16,$38, $23,$12,$16,$38
	.db $2E,$12,$38,$38, $38,$12,$16,$38, $30,$12,$38,$38, $38,$12,$16,$38
	.db $25,$13,$38,$38, $38,$13,$18,$38, $0E,$13,$18,$38, $23,$13,$18,$38
	.db $2F,$13,$38,$38, $38,$13,$18,$38, $31,$13,$38,$38, $38,$13,$18,$38

	.db $38,$03,$38,$38, $05,$03,$04,$38, $1E,$38,$06,$38, $05,$03,$04,$38
	.db $2C,$03,$38,$38, $05,$03,$04,$38, $07,$03,$08,$38, $38,$03,$38,$38
	.db $02,$00,$01,$38, $02,$00,$01,$38, $0A,$00,$09,$38, $02,$00,$01,$38
	.db $2D,$00,$38,$38, $02,$00,$01,$38, $36,$00,$0B,$38, $02,$00,$01,$38

	.db $21,$1F,$38,$38, $21,$1F,$1C,$38, $1B,$1F,$1D,$38, $21,$1F,$1C,$38
	.db $28,$1F,$38,$38, $38,$1F,$1C,$38, $32,$1F,$38,$38, $38,$1F,$1C,$38
	.db $20,$14,$38,$38, $20,$14,$1A,$38, $1A,$14,$37,$38, $20,$14,$1A,$38
	.db $29,$14,$38,$38, $38,$14,$1A,$38, $33,$14,$38,$38, $38,$14,$1A,$38

DATA_ASMSTR:
	;     0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F 
	; 00:LDA,LDX,LDY,STA,STX,STY,TXA,TYA,TXS,TAX,TAY,TSX,PHA,PHP,PLA,PLP
	; 10:AND,ORA,EOR,ADC,SBC,ASL,LSR,ROL,ROR,INC,INX,INY,DEC,DEX,DEY,CMP
	; 20:CPX,CPY,BIT,JMP,JSR,RTS,BRK,RTI,BNE,BEQ,BPL,BMI,BCC,BCS,BVC,BVS
	; 30:CLI,SEI,CLD,SED,CLC,SEC,CLV,NOP,???
	.db $e0,$4c,$44,$41,$e0,$4c,$44,$58,$e0,$4c,$44,$59,$e1,$53,$54,$41,$e1,$53,$54,$58,$e1,$53,$54,$59,$e2,$54,$58,$41,$e2,$54,$59,$41
	.db $e2,$54,$58,$53,$e2,$54,$41,$58,$e2,$54,$41,$59,$e2,$54,$53,$58,$e3,$50,$48,$41,$e3,$50,$48,$50,$e4,$50,$4c,$41,$e4,$50,$4c,$50
	.db $26,$41,$4e,$44,$7c,$4f,$52,$41,$5e,$45,$4f,$52,$2b,$41,$44,$43,$2d,$53,$42,$43,$e5,$41,$53,$4c,$e6,$4c,$53,$52,$e7,$52,$4f,$4c
	.db $e8,$52,$4f,$52,$e9,$49,$4e,$43,$e9,$49,$4e,$58,$e9,$49,$4e,$59,$ea,$44,$45,$43,$ea,$44,$45,$58,$ea,$44,$45,$59,$ec,$43,$4d,$50
	.db $ec,$43,$50,$58,$ec,$43,$50,$59,$eb,$42,$49,$54,$ed,$4a,$4d,$50,$ee,$4a,$53,$52,$ef,$52,$54,$53,$11,$42,$52,$4b,$1b,$52,$54,$49
	.db $f0,$42,$4e,$45,$3d,$42,$45,$51,$f1,$42,$50,$4c,$3c,$42,$4d,$49,$f3,$42,$43,$43,$f2,$42,$43,$53,$f5,$42,$56,$43,$f4,$42,$56,$53
	.db $f7,$43,$4c,$49,$f6,$53,$45,$49,$f9,$43,$4c,$44,$f8,$53,$45,$44,$fb,$43,$4c,$43,$fa,$53,$45,$43,$fc,$43,$4c,$56,$00,$4e,$4f,$50
	.db $3f,$3f,$3f,$3f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

DATA_DISASMADRESSING:
	; 00:      04:00,x    08:(00,x)
	; 01:#00   05:00,y    09:(00),y
	; 02:00    06:0000,x  10:(0000)
	; 03:0000  07:0000,y  11:[unknown]
	.db $00,$08,$0B,$0B, $0B,$02,$02,$0B, $00,$01,$00,$0B, $0B,$03,$03,$0B
	.db $01,$09,$0B,$0B, $0B,$04,$04,$0B, $00,$07,$0B,$0B, $0B,$06,$06,$0B
	.db $03,$08,$0B,$0B, $02,$02,$02,$0B, $00,$01,$00,$0B, $03,$03,$03,$0B
	.db $01,$09,$0B,$0B, $0B,$04,$04,$0B, $00,$07,$0B,$0B, $0B,$06,$06,$0B

	.db $00,$08,$0B,$0B, $0B,$02,$02,$0B, $00,$01,$00,$0B, $03,$03,$03,$0B
	.db $01,$09,$0B,$0B, $0B,$04,$04,$0B, $00,$07,$0B,$0B, $0B,$06,$06,$0B
	.db $00,$08,$0B,$0B, $0B,$02,$02,$0B, $00,$01,$00,$0B, $0A,$03,$03,$0B
	.db $01,$09,$0B,$0B, $0B,$04,$04,$0B, $00,$07,$0B,$0B, $0B,$06,$06,$0B

	.db $0B,$08,$0B,$0B, $02,$02,$02,$0B, $00,$0B,$00,$0B, $03,$03,$03,$0B
	.db $01,$09,$0B,$0B, $04,$04,$05,$0B, $00,$07,$00,$0B, $0B,$06,$0B,$0B
	.db $01,$08,$01,$0B, $02,$02,$02,$0B, $00,$01,$00,$0B, $03,$03,$03,$0B
	.db $01,$09,$0B,$0B, $04,$04,$05,$0B, $00,$07,$00,$0B, $06,$06,$07,$0B

	.db $01,$08,$0B,$0B, $02,$02,$02,$0B, $00,$01,$00,$0B, $03,$03,$03,$0B
	.db $01,$09,$0B,$0B, $0B,$04,$04,$0B, $00,$07,$0B,$0B, $0B,$06,$06,$0B
	.db $01,$08,$0B,$0B, $02,$02,$02,$0B, $00,$01,$00,$0B, $03,$03,$03,$0B
	.db $01,$09,$0B,$0B, $0B,$04,$04,$0B, $00,$07,$0B,$0B, $0B,$06,$06,$0B

; Thanks! http://sasq.comyr.com/Stuff/Elektronika/6502_Opcodes_Table.png

DATA_ASM2ADRESSING:
	; 0:      
	; 1:#00   
	; 2:00; 0000
	; 3:00,x  
	; 4:00,y
	; 5:0000,x  
	; 6:0000,y
	; 7:(00,x); (00),y
	; ※ JMP:%10000000 と JSR:%00000000 はプログラム中で特別に扱う
	.db %11101110, %01010110, %00101110, %11101100 ; LDA,LDX,LDY,STA
	.db %00010100, %00001100, %00000001, %00000001 ; STX,STY,TXA,TYA
	.db %00000001, %00000001, %00000001, %00000001 ; TXS,TAX,TAY,TSX
	.db %00000001, %00000001, %00000001, %00000001 ; PHA,PHP,PLA,PLP

	.db %11101110, %11101110, %11101110, %11101110 ; AND,ORA,EOR,ADC
	.db %11101110, %00011101, %00011101, %00011101 ; SBC,ASL,LSR,ROL
	.db %00011101, %00011100, %00000001, %00000001 ; ROR,INC,INX,INY
	.db %00011100, %00000001, %00000001, %11101110 ; DEC,DEX,DEY,CMP

	.db %00000110, %00000110, %00000100, %10000000 ; CPX,CPY,BIT,JMP
	.db %00000000, %00000001, %00000000, %00000001 ; JSR,RTS,BRK,RTI
	.db %00000001, %00000001, %00000001, %00000001 ; BNE,BEQ,BPL,BMI
	.db %00000001, %00000001, %00000001, %00000001 ; BCC,BCS,BVC,BVS

	.db %00000001, %00000001, %00000001, %00000001 ; CLI,SEI,CLD,SED
	.db %00000001, %00000001, %00000001, %00000001 ; CLC,SEC,CLV,NOP
	.db %00000001                                  ; ???

;*****************************************************************************;
; _ADDR_ : NAMETABLE
; 備考 : MEM_BG0 から 256 バイト書き込む
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
		ldy DATA_Draw8Lines_table, x ; MEM_BG の インデックス
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

	DATA_Draw8Lines_table:
		.db 240,224,208,192,176,160,144,128,112,96,80,64,48,32,16,0
else
	; 最大速度ver
	lda _ADDR_+1
	sta #$2006
	lda _ADDR_
	sta #$2006

	i=0
	REPT 256
		lda MEM_BG0+i	; 4 clock
		sta #$2007		; 4 clock
		i=i+1
	ENDR
	rts
endif

; _ADDR_ : NameTable
; _SRCA_ : MEM_BG
; _N_    : 16ブロック単位 (最大:約6行)
Draw16XTiles:
	PUSH_REG 1,1,0,0,0

	lda _ADDR_+1
	sta #$2006
	lda _ADDR_
	sta #$2006
	ldx _N_
	ldy #0 ; MEM_BG の インデックス
	-
	    REPT 16
			lda (_SRCA_), y
			sta #$2007
			iny
	    ENDR
		dex
	bne -

	POP_REG 1,1,0,0,0
	rts

; _N_ : spr id
Draw1Sprite:
	PUSH_REG 0,1,0,0,0

	lda _N_
	sta $2003
	ASL_n 2
	tay ; インデックスを計算

	lda MEM_SP0, y ; Y座標
	sta $2004
	lda MEM_SP0+1, y ; キャラ番号
	sta $2004
	lda MEM_SP0+2, y ; 反転・優先順位
	sta $2004
	lda MEM_SP0+3, y ; X座標
	sta $2004

	POP_REG 0,1,0,0,0
	rts

DrawAllSprites:
	lda #>MEM_SP0
	sta $4014
	rts

;*****************************************************************************;
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

;*****************************************************************************;
NMI:
	pha
	PUSH_REG 1,1,0,0,0
	;
	; 処理
	nop

	POP_REG 1,1,0,0,0
	pla
	rti

;*****************************************************************************;
BREAK:
	;
	; 保存
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
	; 前回の PC を保存してから スタックから PC を読み込み
	lda _reg_pc
	sta _reg_pc_last
	and #%11000000
	sta _reg_pc_last_40
	LOAD_STACK 2, _reg_pc
	LOAD_STACK 3, _reg_pc+1
	dec _reg_pc
	;
	; 連続した BRK (PC が前回の+1) だった場合、Return (上位PCは見ない)
	lda _reg_pc
	sec
	sbc _reg_pc_last
	cmp #1
	bne +
		jmp +end
	+
	;
	; レジスタ情報表示
	jsr DrawRegisters
	;
	; ボタン入力待機
	-
		jsr GetJoyPad
		WAIT_VBLANK
		;
		; A : ステップ実行(そのままループを抜ける)
		lda _pad1+1
		and #pad_A
		beq +
			jmp ++
		+
		;
		; SELECT : デバッグ中止(STACK を SELECT 押下時点の値まで戻して rts)
		lda _pad1+1
		and #pad_select
		beq +
			jmp EndDebug
		+
	jmp -
	++

+end
	ldy _reg_y
	ldx _reg_x
	lda _reg_a
	rti

;*****************************************************************************;
DrawRegisters:
	;
	; 枠組み描画
	SET_ARGS MEM_BG0, DATA_Register+1, DATA_Register
	jsr memcpy16
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
		SET_ADDR MEM_BG4 + 6+(i*3)
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
	; PC レジスタ (カーソル)
	lda _reg_pc
	and #3
	tax
	lda DATA_XCursor, x
	sta MEM_SP0+3

	lda _reg_pc
	LSR_n 2
	and #15
	tax
	lda DATA_YCursor, x
	sta MEM_SP0+0

	lda #$D9 ; →
	sta MEM_SP0+1
	;
	; 描画
	SET_ARGS NAMETABLE20, MEM_BG0, DATA_Register
	WAIT_VBLANK
	WAIT_VBLANK
	jsr Draw16XTiles
	jsr DrawScrollZero
	WAIT_VBLANK
	SET_N #$0
	jsr Draw1Sprite
	;
	; メモリページ描画 (前回からページが変わっている場合)
	lda _reg_pc
	and #%11000000 ; $40 単位でページ変化を確認
	cmp _reg_pc_last_40
	beq +
		sta _SRCA_
		lda _reg_pc1
		sta _SRCA_+1
		jsr Draw_MemoryViewer
	+
	rts

DATA_StatusBitColor:
	.db $13,$13,$12,$12,$12,$12,$13,$13
;０１２３４５６７８９０１２３４５６７８
;｜Ａ：ＸＸ　PC：ＸＸＸＸ　　　　　　　
;｜Ｘ：ＸＸ　　　ＮＶ－Ｂ－ＩＺＣ　　　
;｜Ｙ：ＸＸ　Ｐ：●●●●●●●●ＸＸ　
;｜Ｓ：ＸＸ　［ＸＸ　ＸＸ　ＸＸ　ＸＸ］

DATA_XCursor: ; PC レジスタ矢印のオフセット(X)
	.db 40, 64, 88, 112
DATA_YCursor: ; PC レジスタ矢印のオフセット(Y)
	.db 31, 39, 47, 55, 63, 71, 79, 87, 95, 103, 111, 119, 127, 135, 143, 151

EndDebug:
	;
	; SelectRoutine まで戻る
	ldx #$FD
	txs
	rts