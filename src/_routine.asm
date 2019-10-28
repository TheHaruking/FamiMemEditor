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

; _ADDR_ : 変更対象のアドレス
; _SRCA_ : 最小
; _N_ ; 最大
InRange:
	PUSH_REG 0,1,0,0,0

	ldy #0
	lda (_ADDR_), y
	cmp _N_
	bmi +
		lda _SRCA_
	+
	cmp _SRCA_
	bpl +
		lda _N_
		sec
		sbc #1
	+
	sta (_ADDR_), y
	POP_REG 0,1,0,0,0
	rts

Mul3_tbl:
	.db 0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99, 102, 105, 108, 111, 114, 117, 120, 123, 126, 129, 132, 135, 138, 141, 144, 147, 150, 153, 156, 159, 162, 165, 168, 171, 174, 177, 180, 183, 186, 189, 192, 195, 198, 201, 204, 207, 210, 213, 216, 219, 222, 225, 228, 231, 234, 237, 240, 243, 246, 249, 252, 255
;Mul4_tbl: asl asl : 2byte 4clock, lda Mul4_tbl,x 3byte 4clock
Mul5_tbl:
	.db 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200, 205, 210, 215, 220, 225, 230, 235, 240, 245, 250, 255
Mul6_tbl:
	.db 0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120, 126, 132, 138, 144, 150, 156, 162, 168, 174, 180, 186, 192, 198, 204, 210, 216, 222, 228, 234, 240, 246, 252
Mul7_tbl:
	.db 0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77, 84, 91, 98, 105, 112, 119, 126, 133, 140, 147, 154, 161, 168, 175, 182, 189, 196, 203, 210, 217, 224, 231, 238, 245, 252
Mul8_tbl: ; asl asl asl : 3byte 6clock, lda Mul8_tbl,x 3byte 4clock
	.db 0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 168, 176, 184, 192, 200, 208, 216, 224, 232, 240, 248
Mul9_tbl:
	.db 0, 9, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99, 108, 117, 126, 135, 144, 153, 162, 171, 180, 189, 198, 207, 216, 225, 234, 243, 252
Mul10_tbl:
	.db 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250
Mul11_tbl:
	.db 0, 11, 22, 33, 44, 55, 66, 77, 88, 99, 110, 121, 132, 143, 154, 165, 176, 187, 198, 209, 220, 231, 242, 253
Mul12_tbl:
	.db 0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252
Mul13_tbl:
	.db 0, 13, 26, 39, 52, 65, 78, 91, 104, 117, 130, 143, 156, 169, 182, 195, 208, 221, 234, 247
Mul14_tbl:
	.db 0, 14, 28, 42, 56, 70, 84, 98, 112, 126, 140, 154, 168, 182, 196, 210, 224, 238, 252
Mul15_tbl:
	.db 0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195, 210, 225, 240, 255
Mul16_tbl:
	.db 0, 16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240