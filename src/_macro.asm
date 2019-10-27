;******************************************************************************
; ROMヘッダマクロ
macro NES_header PRG, CHR, V, SRAM, MAP
	.db "N", "E", "S", $1A, PRG, CHR, (V&1) | ((SRAM&1)<<1) | ((MAP & $0F)<<4), (MAP & $F0)
	.db $00, $00, $00, $00, $00, $00, $00, $00
endm
;参考 : http://www.geocities.co.jp/SiliconValley/2530/nes/iNESHeader.txt

;******************************************************************************
; ボタン
	_pad1	= $F0
	_pad11	= $F1
	_pad12	= $F2
	_pad13	= $F3
	_pad2	= $F4
	_pad21	= $F5
	_pad22	= $F6
	_pad23	= $F7
	; ボタン (一般のファミコンゲームとは逆になっているが、こちらが好み。)
		pad_A		EQU #$01
		pad_B		EQU #$02
		pad_select	EQU #$04
		pad_start	EQU #$08
		pad_Up		EQU #$10
		pad_Down	EQU #$20
		pad_Left	EQU #$40
		pad_Right	EQU #$80

	; 共通で使用
	_N_		= $F8 ; 8 bit の値を渡すのに使用
	_N1_	= $F9
	_MM_	= $FA ; 汎用 (Switch ルーチンのジャンプアドレス)
	_MM1_	= $FB
	_ADDR_	= $FC ; 16 bit のアドレスを渡すのに使用
	_ADDR1_ = $FD
	_SRCA_  = $FE ; 16 bit のアドレスを渡すのに使用
	_SRCA1_ = $FF

;■ メモリアドレス
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
macro SET_M MM
	lda MM
	sta _MM_
endm
macro SET_MM MM
	lda #<(MM)
	sta _MM_
	lda #>(MM)
	sta _MM_+1
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

macro ADD_16 ADDR, n
	lda ADDR
	clc
	adc n
	sta ADDR
	bcc +
		inc ADDR+1
	+
endm

macro SUB_16 ADDR, n
	lda ADDR
	sec
	sbc n
	sta ADDR
	bcs +
		dec ADDR+1
	+
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