.include src/_macro.asm
;// iNES ﾍｯﾀﾞ 定義
;//	[PRGﾊﾞﾝｸ 1, CHRﾊﾞﾝｸ 1, 水平ﾐﾗｰ, SRAM有り, MAPPER:0]
NES_header 1, 1, 1, 1, 0

;// $8000-BFFF : PRG-ROM (LOW) 領域
;// $C000-FFFF : PRG-ROM (HIGH) 領域
.org $C000
	.include src/main.1.asm

.org $FFD0	; パレットはここに配置しておく
PALLET:
	.incbin chr/CHR.dat

.org $FFFA	; 割り込み設定
	.dw NMI
	.dw RESET
	.dw BREAK

;// $10000~ : CHR-ROM 領域
.org $10000
CHR:
	.incbin chr/CHR.chr
