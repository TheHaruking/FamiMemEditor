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
DrawXLines:
	PUSH_REG 1,1,0,0,0

	lda _ADDR_+1
	sta #$2006
	lda _ADDR_
	sta #$2006
	ldx _N_
	ldy #0 ; MEM_BG の インデックス
	-
	    REPT 32
			lda (_SRCA_), y
			sta #$2007
			iny
	    ENDR
		dex
		beq +
	jmp -
	+

	POP_REG 1,1,0,0,0
	rts

; _N_ : spr id
Draw1Sprite:
	PUSH_REG 0,1,0,0,0

	lda _N_
	sta $2003
	ASL_n 2		; インデックスを計算
	tay
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

DrawScrollZero:
	lda #0 ; X座標
	sta #$2005
	lda #0 ; Y座標
	sta #$2005
	rts

;
; _ADDR_ : MEM_BG の始点
; _SRCA_ : コピー元アドレス
; _N_ : 縦幅
; _MM_ ; 横幅
BufCopyBlock:
	PUSH_REG 1,1,1,1,0

	ldx _N_
	dex
	-
		ldy _MM_
		dey
		--
			lda (_SRCA_), y
			sta (_ADDR_), y
			dey
		bpl --

		ADD_16 _ADDR_, #32
		ADD_16 _SRCA_, _MM_
	dex
	bpl -

	POP_REG 1,1,1,1,0
	rts