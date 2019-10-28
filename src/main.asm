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

	_base_preset_n	= $D8	;// 0:0000, 1:6000, 2:8000
	_padMode 	= $D9
	_is_Hex_Changing = $DA
	_is_need_AllDraw = $DB

	_count		= $DC
	_count1		= $DD
	_pad1dir8	= $DE
	_reg_s_exec	= $DF

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

	; Asm 関連
	_is_AsmEdit = $C0
	_asm_id 	= $C1
	_asm_x 		= $C2	; 6
	_asm_y 		= $C3	; 13
	_asm_adr_y 	= $C4
	_asm_adr_idx = $C5
	
	; $F0-$FF : 汎用

; $0200-$4FF
;	SPBUF[$100], BGBUF[$200]
; $0500
	_copy_buf	= $500 ; $500-$53F
; $0600
;	(未使用)
; $0700
;	(未使用)

;■ 定数
; 座標計算
	Y_OFS		EQU $20 ; 上4行空けてカーソルを表示する際のオフセット値
	X_OFS		EQU $2F ; "0000:" の分 - 文字との右間隔 を空けてカーソルを表示する際のオフセット値
; ASMID
	;     0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F 
	; 00:LDA,LDX,LDY,STA,STX,STY,TXA,TYA,TXS,TAX,TAY,TSX,PHA,PHP,PLA,PLP
	; 10:AND,ORA,EOR,ADC,SBC,ASL,LSR,ROL,ROR,INC,INX,INY,DEC,DEX,DEY,CMP
	; 20:CPX,CPY,BIT,JMP,JSR,RTS,BRK,RTI,BNE,BEQ,BPL,BMI,BCC,BCS,BVC,BVS
	; 30:CLI,SEI,CLD,SED,CLC,SEC,CLV,NOP,???
	ID_LDA	EQU 0
	ID_LDX	EQU 1
	ID_LDY	EQU 2
	ID_STA	EQU 3
	ID_STX	EQU 4
	ID_STY	EQU 5
	ID_TXA	EQU 6
	ID_TYA	EQU 7
	ID_TXS	EQU 8
	ID_TAX	EQU 9
	ID_TAY	EQU 10
	ID_TSX	EQU 11
	ID_PHA	EQU 12
	ID_PHP	EQU 13
	ID_PLA	EQU 14
	ID_PLP	EQU 15
	ID_AND	EQU 16
	ID_ORA	EQU 17
	ID_EOR	EQU 18
	ID_ADC	EQU 19
	ID_SBC	EQU 20
	ID_ASL	EQU 21
	ID_LSR	EQU 22
	ID_ROL	EQU 23
	ID_ROR	EQU 24
	ID_INC	EQU 25
	ID_INX	EQU 26
	ID_INY	EQU 27
	ID_DEC	EQU 28
	ID_DEX	EQU 29
	ID_DEY	EQU 30
	ID_CMP	EQU 31
	ID_CPX	EQU 32
	ID_CPY	EQU 33
	ID_BIT	EQU 34
	ID_JMP	EQU 35
	ID_JSR	EQU 36
	ID_RTS	EQU 37
	ID_BRK	EQU 38
	ID_RTI	EQU 39
	ID_BEQ 	EQU 40
	ID_BNE	EQU 41
	ID_BMI	EQU 42
	ID_BPL	EQU 43
	ID_BCS	EQU 44
	ID_BCC	EQU 45
	ID_BVS	EQU 46
	ID_BVC	EQU 47
	ID_SEI	EQU 48
	ID_CLI	EQU 49
	ID_SED	EQU 50
	ID_CLD	EQU 51
	ID_SEC	EQU 52
	ID_CLC	EQU 53
	ID_CLV	EQU 54
	ID_NOP	EQU 55
	ID____	EQU 56

;*****************************************************************************
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
	;lda #%10000000 ; NMI - SPsize BGbase SPbase BG縦横 MainScreen(2bit)
	;sta $2000
	jsr DrawInitialize
	SET_SRCA_PTR _base
	jsr Draw_MemoryViewer
	;
	; メインループ
	-
		;
		; 入力
		jsr GetJoyPad
		jsr Get8Direction
		;
		; 処理
		jsr SelectRoutine
		jsr CalcCursor
		jsr Count
		;
		; 描画
		jsr Draw
	jmp -
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
	.dw nonefunc
	.dw MoveCursor		; A
	.dw ChangeBaseAddr	; B
	.dw Exec			; select
	.dw OtherFunc		; start
	.dw EditHex			; 十時キー

; A:1, B:2, start:3, select:4, 十字キー:5
DATA_SelectRoutine
	.db 0, 1, 2, 0	; - A B - 
	.db 3, 0, 0, 0	; e - - - 
	.db 4, 0, 0, 0	; S - - - 
	.db 0, 0, 0, 0	; - - - - 
;
;*****************************************************************************
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

ChangeBaseAddr:
	;
	; RESERVE!
	lda _pad1+1
	and #pad_Down
	beq +
		; 
	+
	;
	; RESERVE!
	lda _pad1+1
	and #pad_Up
	beq +
		;
	+
	;
	; base アドレスの変更 (プリセット)
	lda _pad1+1
	and #pad_Left
	beq +
		jsr ChangeBaseAddrMode
	+
	;
	; ASM 入力へ移行
	lda _pad1+1
	and #pad_Right
	beq +
		lda #1
		sta _is_AsmEdit
		jsr AsmEdit
	+
	rts

ChangeBaseAddrMode:
	;
	; 一時的にスプライトを変更
	lda #$D9 ; →
	sta MEM_SP0+1
	lda #0
	sta MEM_SP0+3
	lda #30
	sta MEM_SP0+0
	-
		jsr GetJoyPad
		lda _pad1+1
		and #pad_B
		beq +
			jmp +end
		+

		lda _pad1+1
		and #pad_Right
		beq +
			inc _base_preset_n
		+
		lda _pad1+1
		and #pad_Left
		beq +
			dec _base_preset_n
		+
		;
		; base アドレスの変更
		lda _pad1+1
		and #pad_Down
		beq ++
			ADD_16 _base, #$40
		++
		lda _pad1+1
		and #pad_Up
		beq ++
			SUB_16 _base, #$40		
		++

		SET_ARGS _base_preset_n, #0, #11
		jsr InRange

		lda _pad1+1
		and #pad_Right | #pad_Left
		beq +
			lda _base_preset_n
			tax
			lda #$00
			sta _base
			lda DATA_ChangeBaseAddr_Preset, x
			sta _base+1
		+
		;
		; 描画
		SET_SRCA_PTR _base
		jsr Draw_MemoryViewer
	jmp -
	+end
	lda #$09 ; →
	sta MEM_SP0+1
	rts

DATA_ChangeBaseAddr_Preset: ; 下バイトは全部 $00 なので、上だけで良い.
	.db >$0000, >$0400
	.db >$6000, >$6400, >$6800, >$6C00
	.db >$7000, >$7400, >$7800, >$7C00
	.db >$8000

Exec: ; 実行
	;
	; 押した瞬間でなければ Return
	lda _pad1+1
	and #pad_select
	bne +
		rts
	+
	;
	; EndDebug で中断できるようスタックを退避
	tsx
	stx _reg_s_exec
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
;******************************************************************************
AsmEdit:
	SET_ARGS MEM_BG0, DATA_ASMEDIT_INIT, #16
	jsr memcpy16
	SET_ARGS MEM_BG8, DATA_ASMEDIT_INIT+256, #14
	jsr memcpy16

	SET_ADDR NAMETABLE04
	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero
	;
	; 一時的にスプライトを変更
	lda #$D9 ; →
	sta MEM_SP0+1

	jsr AsmEditEngine_CalcCursor

	SET_ARGS MEM_BG0, MEM_BG8, #16
	jsr memcpy16
	SET_ADDR NAMETABLE12
	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero

	;
	; AsmEdit ループ
	-
		jsr GetJoyPad
		jsr Get8Direction

		jsr AsmEditEngine

		jsr AsmEditDraw

		lda _is_AsmEdit
		cmp #2
		bne +
			jmp AsmEdit
		+
		lda _is_AsmEdit
	bne -
	;
	; スプライトを戻す
	lda #$09 ; →
	sta MEM_SP0+1
	rts
DATA_ASMEDIT_INIT: ; 15
	.db $1c,$19,$19,$19,$19,$19,$19,$19,$19,$19,$15,$19,$19,$19,$19,$19,$19,$19,$19,$19,$15,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1d,$00
	.db $1a,$e0,$4c,$44,$41,$00,$e1,$53,$54,$41,$1a,$26,$41,$4e,$44,$00,$7c,$4f,$52,$41,$1a,$ed,$4a,$4d,$50,$00,$00,$00,$00,$00,$1a,$00
	.db $1a,$e0,$4c,$44,$58,$00,$e1,$53,$54,$58,$1a,$5e,$45,$4f,$52,$00,$00,$00,$00,$00,$1a,$ee,$4a,$53,$52,$00,$ef,$52,$54,$53,$1a,$00
	.db $1a,$e0,$4c,$44,$59,$00,$e1,$53,$54,$59,$1a,$2b,$41,$44,$43,$00,$2d,$53,$42,$43,$1a,$fe,$42,$52,$4b,$00,$fd,$52,$54,$49,$1a,$00
	.db $1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$00
	.db $1a,$e2,$54,$58,$41,$00,$e2,$54,$41,$58,$1a,$e5,$41,$53,$4c,$00,$e6,$4c,$53,$52,$1a,$3d,$42,$45,$51,$00,$f0,$42,$4e,$45,$1a,$00
	.db $1a,$e2,$54,$59,$41,$00,$e2,$54,$41,$59,$1a,$e7,$52,$4f,$4c,$00,$e8,$52,$4f,$52,$1a,$3c,$42,$4d,$49,$00,$f1,$42,$50,$4c,$1a,$00
	.db $1a,$e2,$54,$58,$53,$00,$e2,$54,$53,$58,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$f2,$42,$43,$53,$00,$f3,$42,$43,$43,$1a,$00
	.db $1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$e9,$49,$4e,$43,$00,$ea,$44,$45,$43,$1a,$f4,$42,$56,$53,$00,$f5,$42,$56,$43,$1a,$00
	.db $1a,$e3,$50,$48,$41,$00,$e4,$50,$4c,$41,$1a,$e9,$49,$4e,$58,$00,$ea,$44,$45,$58,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$00
	.db $1a,$e3,$50,$48,$50,$00,$e4,$50,$4c,$50,$1a,$e9,$49,$4e,$59,$00,$ea,$44,$45,$59,$1a,$f6,$53,$45,$49,$00,$f7,$43,$4c,$49,$1a,$00
	.db $16,$19,$19,$19,$19,$19,$19,$19,$19,$19,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$f8,$53,$45,$44,$00,$f9,$43,$4c,$44,$1a,$00
	.db $1a,$00,$4e,$4f,$50,$00,$00,$00,$00,$00,$1a,$ec,$43,$4d,$50,$00,$ec,$43,$50,$58,$1a,$fa,$53,$45,$43,$00,$fb,$43,$4c,$43,$1a,$00
	.db $1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1a,$eb,$42,$49,$54,$00,$ec,$43,$50,$59,$1a,$00,$00,$00,$00,$00,$fc,$43,$4c,$56,$1a,$00
	.db $1e,$19,$19,$19,$19,$19,$19,$19,$19,$19,$14,$19,$19,$19,$19,$19,$19,$19,$19,$19,$14,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1f,$00

AsmEditEngine:
	;
	; キャンセル
	lda _pad1+1
	and #pad_B
	beq +
		lda #0
		sta _is_AsmEdit
	+

	-retry
		lda _pad1+1
		and #pad_Down
		beq +
			inc _asm_y
		+
		lda _pad1+1
		and #pad_Up
		beq +
			dec _asm_y
		+
		lda _pad1+1
		and #pad_Right
		beq +
			inc _asm_x
		+
		lda _pad1+1
		and #pad_Left
		beq +
			dec _asm_x
		+
		;
		; 正規化
		SET_ARGS _asm_y, #0, #13
		jsr InRange
		SET_ARGS _asm_x, #0, #6
		jsr InRange

		;
		; _asm_x * 13
		lda _asm_x
		tax
		lda Mul13_tbl, x
		clc
		adc _asm_y
		tax
		lda DATA_ASMEDIT_LIST, x
		sta _asm_id
		;
		; カーソル位置が不明箇所だった場合、もう一回移動
		cmp #ID____
	beq -retry

	jsr AsmEditEngine_CalcCursor

	;
	; 決定
	lda _pad1+1
	and #pad_A
	beq +
		lda _asm_id
		;
		; ???? の場合 スキップ (万が一用)
		cmp #ID____
		beq +

		tax
		lda DATA_ASMISONE, x
		cmp #1
		bne ++
			lda DATA_ASMID2CODE_and_TBL, x
			ldy #0
			sta (_curaddr), y
			lda #0
			sta _is_AsmEdit
			jmp +++
		++
			jsr AsmEditAdressing
			lda _is_AsmEdit
			cmp #0
			bne ++++
				lda #0
				sta _is_AsmEdit
				jmp +++++
			++++
				lda #2
				sta _is_AsmEdit
			+++++
		+++
	+
	rts

AsmEditEngine_CalcCursor:
	;
	; PC レジスタ (カーソル)
	lda _asm_x
	tax
	lda DATA_AsmEditEngine_CalcCursor_x, x
	sta MEM_SP0+3

	lda _asm_y
	tax
	lda DATA_AsmEditEngine_CalcCursor_y, x
	sta MEM_SP0+0
	rts
DATA_AsmEditEngine_CalcCursor_x:
	.db 0, 40, 80, 120, 160, 200
DATA_AsmEditEngine_CalcCursor_y:
	.db 39, 47, 55, 63, 71, 79, 87, 95, 103, 111, 119, 127, 135

AsmEditDraw:
	WAIT_VBLANK
	SET_N #0
	jsr Draw1Sprite
	rts

DATA_ASMEDIT_LIST: ; 13 * 6
	.db ID_LDA, ID_LDX, ID_LDY, ID____, ID_TXA, ID_TYA, ID_TXS, ID____, ID_PHA, ID_PHP, ID____, ID_NOP, ID____
	.db ID_STA, ID_STX, ID_STY, ID____, ID_TAX, ID_TAY, ID_TSX, ID____, ID_PLA, ID_PLP, ID____, ID____, ID____
	.db ID_AND, ID_EOR, ID_ADC, ID____, ID_ASL, ID_ROL, ID____, ID_INC, ID_INX, ID_INY, ID____, ID_CMP, ID_BIT
	.db ID_ORA, ID____, ID_SBC, ID____, ID_LSR, ID_ROR, ID____, ID_DEC, ID_DEX, ID_DEY, ID____, ID_CPX, ID_CPY
	.db ID_JMP, ID_JSR, ID_BRK, ID____, ID_BEQ, ID_BMI, ID_BCS, ID_BVS, ID____, ID_SEI, ID_SED, ID_SEC, ID____
	.db ID____, ID_RTS, ID_RTI, ID____, ID_BNE, ID_BPL, ID_BCC, ID_BVC, ID____, ID_CLI, ID_CLD, ID_CLC, ID_CLV

DATA_ASMISONE:
	.db 0, 0, 0, 0 ; LDA,LDX,LDY,STA
	.db 0, 0, 1, 1 ; STX,STY,TXA,TYA
	.db 1, 1, 1, 1 ; TXS,TAX,TAY,TSX
	.db 1, 1, 1, 1 ; PHA,PHP,PLA,PLP

	.db 0, 0, 0, 0 ; AND,ORA,EOR,ADC
	.db 0, 0, 0, 0 ; SBC,ASL,LSR,ROL
	.db 0, 0, 1, 1 ; ROR,INC,INX,INY
	.db 0, 1, 1, 0 ; DEC,DEX,DEY,CMP

	.db 0, 0, 0, 0 ; CPX,CPY,BIT,JMP
	.db 1, 1, 1, 1 ; JSR,RTS,BRK,RTI
	.db 1, 1, 1, 1 ; BEQ,BNE,BMI,BPL
	.db 1, 1, 1, 1 ; BCS,BCC,BVS,BVC

	.db 1, 1, 1, 1 ; SEI,CLI,SED,CLD
	.db 1, 1, 1, 1 ; SEC,CLC,CLV,NOP
	.db 0          ; ???

DATA_ASMID2CODE_and_TBL:
	.db   0,   1,   2,   3
	.db   4,   5, $8A, $98
	.db $9A, $AA, $A8, $BA
	.db $48, $68, $08, $28

	.db   6,   7,   8,   9
	.db  10,  11,  12,  13
	.db  14,  15, $E8, $C8
	.db  16, $CA, $88,  17

	.db  18,  19,  20,  21
	.db $20, $60, $00, $40
	.db $F0, $D0, $30, $10
	.db $B0, $90, $70, $50

	.db $78, $58, $F8, $D8
	.db $38, $18, $B8, $EA
	.db $00

DATA_ASMADRESSINGID2CODE: ; 11x22行
	;         #    0    $   0,x  0,y  $,x  $,y  (0x) (0)y  ($)
	.db   0, $A9, $A5, $AD, $B5,   0, $BD, $B9, $A1, $B1,   0 ; LDA
	.db   0, $A2, $A6, $AE,   0, $B6,   0, $BE,   0,   0,   0 ; LDX
	.db   0, $A0, $A4, $AC, $B4,   0, $BC,   0,   0,   0,   0 ; LDY
	.db   0,   0, $85, $8D, $95,   0, $9D, $99, $81, $91,   0 ; STA
	.db   0,   0, $86, $8E,   0, $96,   0,   0,   0,   0,   0 ; STX
	.db   0,   0, $84, $8C, $94,   0,   0,   0,   0,   0,   0 ; STY
	.db   0, $29, $25, $2D, $35,   0, $3D, $39, $21, $31,   0 ; AND
	.db   0, $09, $05, $0D, $15,   0, $1D, $19, $01, $11,   0 ; ORA
	.db   0, $49, $45, $4D, $55,   0, $5D, $59, $41, $51,   0 ; EOR
	.db   0, $69, $65, $6D, $75,   0, $7D, $79, $61, $71,   0 ; ADC
	.db   0, $E9, $E5, $ED, $F5,   0, $FD, $F9, $E1, $F1,   0 ; SBC
	.db $0A,   0, $06, $0E, $16,   0, $1E,   0,   0,   0,   0 ; ASL
	.db $4A,   0, $46, $4E, $56,   0, $5E,   0,   0,   0,   0 ; LSR
	.db $2A,   0, $26, $2E, $36,   0, $3E,   0,   0,   0,   0 ; ROL
	.db $6A,   0, $66, $6E, $76,   0, $7E,   0,   0,   0,   0 ; ROR
	.db   0,   0, $E6, $EE, $F6,   0, $FE,   0,   0,   0,   0 ; INC
	.db   0,   0, $C6, $CE, $D6,   0, $DE,   0,   0,   0,   0 ; DEC
	.db   0, $C9, $C5, $CD, $D5,   0, $DD, $D9, $C1, $D1,   0 ; CMP
	.db   0, $E0, $E4, $EC,   0,   0,   0,   0,   0,   0,   0 ; CPX
	.db   0, $C0, $C4, $CC,   0,   0,   0,   0,   0,   0,   0 ; CPY
	.db   0,   0, $24, $2C,   0,   0,   0,   0,   0,   0,   0 ; BIT
	.db   0,   0,   0, $4C,   0,   0,   0,   0,   0,   0, $6C ; JMP
;
; _is_AsmEdit : 完了時 : 0, キャンセル時 : 1
AsmEditAdressing:
	lda #2
	sta _is_AsmEdit

	;
	; 配列のアドレスを保存
	lda _asm_id
	tax
	lda DATA_ASMID2CODE_and_TBL, x
	tax
	lda Mul11_tbl, x
	sta _asm_adr_idx
	-retry
		;
		; 正規化
		SET_ARGS _asm_adr_y, #0, #11
		jsr InRange

		;
		; 不明 ADR だった場合、移動
		lda _asm_adr_idx
		clc
		adc _asm_adr_y
		tax
		lda DATA_ASMADRESSINGID2CODE, x
		bne +

		inc _asm_adr_y
	jmp -retry
	+
	;
	; 画面初期化
	SET_ARGS MEM_BG0, DATA_ASMEDIT_INIT, #16
	jsr memcpy16

	SET_ARGS MEM_BG1+8, DATA_ASMEDIT_ADRESSING_INIT, #13
	SET_M #15
	jsr BufCopyBlock

	;
	; 対応する Adressing のみ ニーモニックを表示
	SET_SRCA DATA_ASMSTR
	lda _asm_id
	ASL_n 2
	sta _N_
	jsr add_16_SRCA
	SET_ADDR MEM_BG2+10
	SET_N #4

	ldy #0
	ldx _asm_adr_idx
	-
		lda DATA_ASMADRESSINGID2CODE, x
		beq +
			SET_N #4
			jsr memcpy
			jmp ++
		+
			PUSH_REG 0,0,0,1,0
			SET_SRCA DATA_ASMADRESSINGBLANK
			SET_N #12
			jsr memcpy			
			POP_REG 0,0,0,1,0
		++
		SET_N #32
		jsr add_16_ADDR
		inx
		iny
		cpy #11
	bmi -

	SET_ADDR NAMETABLE04
	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero

	SET_ARGS MEM_BG0, MEM_BG8, #16
	jsr memcpy16

	SET_ADDR NAMETABLE12
	WAIT_VBLANK
	jsr Draw8Lines
	jsr DrawScrollZero
	-
		jsr GetJoyPad
		jsr Get8Direction

		jsr AsmEditAdressingEngine
		jsr AsmEditDraw

		lda _is_AsmEdit
		cmp #2
	beq -
	rts

DATA_ASMEDIT_ADRESSING_INIT: ; 15x13
	.db $1c,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1d
	.db $1a,$00,$00,$00,$00,$00,$00,$2D,$00,$00,$00,$00,$00,$00,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$23,$24,$30,$30,$00,$00,$00,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$24,$30,$30,$00,$00,$00,$00,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$24,$30,$30,$30,$30,$00,$00,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$24,$30,$30,$2c,$58,$00,$00,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$24,$30,$30,$2c,$59,$00,$00,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$24,$30,$30,$30,$30,$2c,$58,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$24,$30,$30,$30,$30,$2c,$59,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$28,$24,$30,$30,$2c,$58,$29,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$28,$24,$30,$30,$29,$2c,$59,$1a
	.db $1a,$00,$00,$00,$00,$00,$00,$28,$24,$30,$30,$30,$30,$29,$1a
	.db $1e,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1f
DATA_ASMADRESSINGBLANK:
	.db "----        " ; 12
DATA_ASMADRESSING_Cursor_y: ; perl -e 'for(0..11){ print $_*8+47, ", "}'
	.db 47, 55, 63, 71, 79, 87, 95, 103, 111, 119, 127, 135

AsmEditAdressingEngine:
	lda _pad1+1
	and #pad_B
	beq +
		lda #1 ; キャンセル時は 1
		sta _is_AsmEdit
	+
	
	-retry
		lda _pad1+1
		and #pad_Down
		beq +
			inc _asm_adr_y
		+
		lda _pad1+1
		and #pad_Up
		beq +
			dec _asm_adr_y
		+
		;
		; 正規化
		SET_ARGS _asm_adr_y, #0, #11
		jsr InRange
		;
		; 不明 ADR だった場合、再度移動
		lda _asm_adr_idx
		clc
		adc _asm_adr_y
		tax
		lda DATA_ASMADRESSINGID2CODE, x
	beq -retry
	;
	; A
	lda _pad1+1
	and #pad_A
	beq +
		lda _asm_adr_idx
		clc
		adc _asm_adr_y
		tax
		lda DATA_ASMADRESSINGID2CODE, x
		beq ++
			ldy #0
			sta (_curaddr), y
			lda #0
			sta _is_AsmEdit
		++
	+
	;
	; カーソル
	lda #72
	sta MEM_SP0+3

	lda _asm_adr_y
	tax
	lda DATA_ASMADRESSING_Cursor_y, x
	sta MEM_SP0+0
	rts

;******************************************************************************
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

Count
	;
	; カウンタ
	inc _count
	bne +
		inc _count+1
	+
	rts
;******************************************************************************
; 描画関係
Draw:
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
	rts

DrawInitialize:
	;
	; スプライト初期化 (これがないと EverDrive 起動時、ゴミが表示される)
	WAIT_VBLANK
	jsr DrawAllSprites
	;
	; 事前描画 (タイトル)
	SET_ARGS MEM_BG0, DATA_TITLE+1, DATA_TITLE
	asl _N_
	jsr memcpy16

	SET_ARGS MEM_BG1, DATA_Register+1, DATA_Register
	asl _N_
	jsr memcpy16
	
	SET_ARGS MEM_BG7, DATA_HELP+1, DATA_HELP
	asl _N_
	jsr memcpy16
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
	SET_ARGS MEM_BG0, #0, #16 ; SRCA はアドレスではなく、値として使用. memset の N は 0 指定で 256 扱い.
	jsr memset16
	SET_ARGS MEM_BG8, #0, #16
	jsr memset16
	rts

DATA_TITLE:
	.db 1	; 行数 (DrawXLines で使用. memcpy16 に渡す際は *2 すること！)
	.!hira "010101010202020303　　ふぁみめむ80えでぃた　　030302020201010101"

DATA_Register: ; 横幅 19
	.db 6	; 行数 (DrawXLines で使用. memcpy16 に渡す際は *2 すること！)
	.db $1c,$cc,$98,$99,$9f,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$41,$3a,$2d,$2d,$00,$ff,$3a,$2d,$2d,$2d,$2d,$00,$00,$00,$00,$00,$00,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$58,$3a,$2d,$2d,$00,$00,$00,$4e,$56,$72,$62,$64,$69,$5a,$43,$00,$00,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$59,$3a,$2d,$2d,$00,$50,$3a,$10,$10,$10,$10,$10,$10,$10,$10,$2d,$2d,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1a,$53,$3a,$2d,$2d,$5b,$2d,$2d,$00,$2d,$2d,$20,$2d,$2d,$20,$2d,$2d,$5d,$1a,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.db $1e,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$19,$1f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

DATA_HELP
	.db 4	; 行数 (DrawXLines で使用. memcpy16 に渡す際は *2 すること！)
	;.!hira "Ａ＋じゅうじ：かーそるいどう　　Ｂ＋じゅうじ：あどれすへんこう　"
	.db $d4,$2b,$7f,$00,$3a,$8b,$2d,$9d,$cb,$84,$a9,$86,$00,$00,$00,$00,$d7,$2b,$da,$db,$3a,$af,$d3,$84,$97,$a6,$84,$26,$93,$b4,$2d,$00
	.db $d5,$2b,$d8,$00,$3a,$ba,$2d,$98,$84,$a9,$86,$c2,$2d,$a9,$00,$00,$d7,$2b,$d9,$00,$3a,$ba,$2d,$99,$a8,$00,$00,$00,$00,$00,$00,$00
	.db $7f,$2b,$d4,$d5,$3a,$ab,$c5,$86,$ca,$c7,$8f,$00,$00,$00,$00,$00,$d6,$00,$00,$00,$3a,$98,$a3,$93,$86,$00,$00,$00,$00,$00,$00,$00
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
	;SET_SRCA_PTR _base
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
	.db $2B,$11,$38,$38, $38,$11,$15,$38, $35,$11,$38,$38, $38,$11,$15,$38
	.db $24,$10,$38,$38, $22,$10,$17,$38, $0F,$10,$17,$38, $22,$10,$17,$38
	.db $2A,$10,$38,$38, $38,$10,$17,$38, $34,$10,$38,$38, $38,$10,$17,$38

	.db $27,$12,$38,$38, $38,$12,$16,$38, $0C,$12,$16,$38, $23,$12,$16,$38
	.db $2F,$12,$38,$38, $38,$12,$16,$38, $31,$12,$38,$38, $38,$12,$16,$38
	.db $25,$13,$38,$38, $38,$13,$18,$38, $0E,$13,$18,$38, $23,$13,$18,$38
	.db $2E,$13,$38,$38, $38,$13,$18,$38, $30,$13,$38,$38, $38,$13,$18,$38

	.db $38,$03,$38,$38, $05,$03,$04,$38, $1E,$38,$06,$38, $05,$03,$04,$38
	.db $2D,$03,$38,$38, $05,$03,$04,$38, $07,$03,$08,$38, $38,$03,$38,$38
	.db $02,$00,$01,$38, $02,$00,$01,$38, $0A,$00,$09,$38, $02,$00,$01,$38
	.db $2C,$00,$38,$38, $02,$00,$01,$38, $36,$00,$0B,$38, $02,$00,$01,$38

	.db $21,$1F,$38,$38, $21,$1F,$1C,$38, $1B,$1F,$1D,$38, $21,$1F,$1C,$38
	.db $29,$1F,$38,$38, $38,$1F,$1C,$38, $33,$1F,$38,$38, $38,$1F,$1C,$38
	.db $20,$14,$38,$38, $20,$14,$19,$38, $1A,$14,$37,$38, $20,$14,$19,$38
	.db $28,$14,$38,$38, $38,$14,$19,$38, $32,$14,$38,$38, $38,$14,$19,$38

DATA_ASMSTR:
	;     0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F 
	; 00:LDA,LDX,LDY,STA,STX,STY,TXA,TYA,TXS,TAX,TAY,TSX,PHA,PHP,PLA,PLP
	; 10:AND,ORA,EOR,ADC,SBC,ASL,LSR,ROL,ROR,INC,INX,INY,DEC,DEX,DEY,CMP
	; 20:CPX,CPY,BIT,JMP,JSR,RTS,BRK,RTI,BEQ,BNE,BMI,BPL,BCS,BCC,BVS,BVC
	; 30:SEI,CLI,SED,CLD,SEC,CLC,CLV,NOP,???
	.db $e0,$4c,$44,$41,$e0,$4c,$44,$58,$e0,$4c,$44,$59,$e1,$53,$54,$41,$e1,$53,$54,$58,$e1,$53,$54,$59,$e2,$54,$58,$41,$e2,$54,$59,$41
	.db $e2,$54,$58,$53,$e2,$54,$41,$58,$e2,$54,$41,$59,$e2,$54,$53,$58,$e3,$50,$48,$41,$e3,$50,$48,$50,$e4,$50,$4c,$41,$e4,$50,$4c,$50
	.db $26,$41,$4e,$44,$7c,$4f,$52,$41,$5e,$45,$4f,$52,$2b,$41,$44,$43,$2d,$53,$42,$43,$e5,$41,$53,$4c,$e6,$4c,$53,$52,$e7,$52,$4f,$4c
	.db $e8,$52,$4f,$52,$e9,$49,$4e,$43,$e9,$49,$4e,$58,$e9,$49,$4e,$59,$ea,$44,$45,$43,$ea,$44,$45,$58,$ea,$44,$45,$59,$ec,$43,$4d,$50
	.db $ec,$43,$50,$58,$ec,$43,$50,$59,$eb,$42,$49,$54,$ed,$4a,$4d,$50,$ee,$4a,$53,$52,$ef,$52,$54,$53,$fe,$42,$52,$4b,$fd,$52,$54,$49
	.db $3d,$42,$45,$51,$f0,$42,$4e,$45,$3c,$42,$4d,$49,$f1,$42,$50,$4c,$f2,$42,$43,$53,$f3,$42,$43,$43,$f4,$42,$56,$53,$f5,$42,$56,$43
	.db $f6,$53,$45,$49,$f7,$43,$4c,$49,$f8,$53,$45,$44,$f9,$43,$4c,$44,$fa,$53,$45,$43,$fb,$43,$4c,$43,$fc,$43,$4c,$56,$00,$4e,$4f,$50
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

EndDebug:
	;
	; SelectRoutine まで戻る
	ldx _reg_s_exec
	txs
	rts

DrawRegisters:
	;
	; 枠組み描画
	SET_ARGS MEM_BG0, DATA_Register+1, DATA_Register
	asl _N_
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
	;
	; 一時的にスプライトを変更
	lda #$D9 ; →
	sta MEM_SP0+1
	;
	; 描画
	SET_ARGS NAMETABLE20, MEM_BG0, DATA_Register
	WAIT_VBLANK
	WAIT_VBLANK
	jsr DrawXLines
	jsr DrawScrollZero
	WAIT_VBLANK
	SET_N #$0
	jsr Draw1Sprite

	;
	; メモリページ描画
	; (disasm のため _curaddr を操作)
	lda _curaddr
	pha
	lda _curaddr+1
	pha
	lda _reg_pc
	sta _curaddr
	and #%11000000
	sta _SRCA_
	lda _reg_pc1
	sta _curaddr+1
	sta _SRCA_+1
	jsr Draw_MemoryViewer
	;
	; 元に戻していく
	pla
	sta _curaddr+1
	pla
	sta _curaddr

	lda #$09 	; '|' に戻す
	sta MEM_SP0+1

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
