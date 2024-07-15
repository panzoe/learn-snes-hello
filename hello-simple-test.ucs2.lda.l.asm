; Minimal example of using ca65 to build SNES ROM.

.p816   ; 65816 processor
.i16    ; X/Y are 16 bits
.a8     ; A is 8 bits

.include "snes.inc"
.include "charmap.inc"

.segment "HEADER"    ; +$7FE0 in file
.byte "CA65 EXAMPLE" ; ROM name

.segment "ROMINFO"   ; +$7FD5 in file
.byte $30            ; LoROM, fast-capable
.byte 0              ; no battery RAM
.byte $07            ; 128K ROM
.byte 0,0,0,0
.word $AAAA,$5555    ; dummy checksum and complement

.segment "CODE"
   jmp start

VRAM_CHARSET   = $0000 ; must be at $1000 boundary
VRAM_BG1       = $1000 ; must be at $0400 boundary
VRAM_BG2       = $1400 ; must be at $0400 boundary
VRAM_BG3       = $1800 ; must be at $0400 boundary
VRAM_BG4       = $1C00 ; must be at $0400 boundary
START_X        = 9
START_Y        = 14
START_TM_ADDR  = VRAM_BG1 + 32*START_Y + START_X

hello_str: .asciiz "Hello, World!"

start:
   clc             ; native mode
   xce
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit

   ; Clear registers
   ldx #$33

   jsr ClearVRAM

@loop:
   stz INIDISP,x
   stz NMITIMEN,x
   dex
   bpl @loop

   lda #128
   sta INIDISP ; undo the accidental stz to 2100h due to BPL actually being a branch on nonnegative

   ; Set palette to black background and 3 shades of red
   stz CGADD ; start with color 0 (background)
   stz CGDATA ; None more black
   stz CGDATA
   lda #$10 ; Color 1: dark red
   sta CGDATA
   stz CGDATA
   lda #$1F ; Color 2: neutral red
   sta CGDATA
   stz CGDATA
   lda #$1F  ; Color 3: light red
   sta CGDATA
   lda #$42
   sta CGDATA

   ; Setup Graphics Mode 0, 8x8 tiles all layers
   stz BGMODE
   lda #>VRAM_BG1
   sta BG1SC ; BG1 at VRAM_BG1, only single 32x32 map (4-way mirror)
   lda #((>VRAM_CHARSET >> 4) | (>VRAM_CHARSET & $F0))
   sta BG12NBA ; BG 1 and 2 both use char tiles

   ; Load character set into VRAM
   lda #$80
   sta VMAIN   ; VRAM stride of 1 word
   ldx #VRAM_CHARSET
   stx VMADDL
   ldx #0


; 这里快速测试一下新加的 UCS-2 字符集字库的正确性
;
; 这里用“我”这个汉字为起始地址加载到 VRAM 中
; lda.l 指令可以使用 长地址(24bit)读取数据
; cc65 不支持 lda.l，可以用 lds f:地址标签,X
;
; UCS-2 Char Locating
; 		"我" UCS-2 			: $6211 -> %25105
;  		UCS-2 Index			：25105
;  		Tile Size			: 8x8 1bpp = 8byte
;  		lowROM bank Size	: 32k
;  		Char per bank4		: 32*1024/8 = 4096char/bank
;  		Bank Index			: 25105/4096 = 6.12915039 = 7
;  		Bank7 Address		: $078000
;  		Tile Offset			: 25105%4096 = 529 -> 529 * 8x8 1bpp = 4232 byte
;  		Final point			: $078000 + %4232 = $078000 + $1088 = $079088
.DEFINE CHAR_WO $079088

@charset_loop:
   lda f:CHAR_WO,x
   stz VMDATAL ; color index low bit = 0
   sta VMDATAH ; color index high bit set -> neutral red (2)
   inx
   cpx #(128*8)
   bne @charset_loop

   ; Place string tiles in background
   ldx #START_TM_ADDR
   stx VMADDL
   ldx #0
@string_loop:
   lda hello_str,x
   beq @enable_display
   sta VMDATAL
   lda #$20 ; priority 1
   sta VMDATAH
   inx
   bra @string_loop

@enable_display:
   ; Show BG1
   lda #$01
   sta TM
   ; Maximum screen brightness
   lda #$0F
   sta INIDISP

   ; enable NMI for Vertical Blank
   lda #$80
   sta NMITIMEN

game_loop:
   wai ; Pause until next interrupt complete (i.e. V-blank processing is done)
   ; Do something
   jmp game_loop


nmi:
   rep #$10        ; X/Y 16-bit
   sep #$20        ; A 8-bit
   phd
   pha
   phx
   phy
   ; Do stuff that needs to be done during V-Blank
   lda RDNMI ; reset NMI flag
   ply
   plx
   pla
   pld
return_int:
   rti

;----------------------------------------------------------------------------
; ClearVRAM -- Sets every byte of VRAM to zero
; from bazz's VRAM tutorial
; In: None
; Out: None
; Modifies: flags
;----------------------------------------------------------------------------
ClearVRAM:
   pha
   phx
   php

   REP #$30		; mem/A = 8 bit, X/Y = 16 bit
   SEP #$20

   LDA #$80
   STA $2115         ;Set VRAM port to word access
   LDX #$1809
   STX $4300         ;Set DMA mode to fixed source, WORD to $2118/9
   LDX #$0000
   STX $2116         ;Set VRAM port address to $0000
   STX $0000         ;Set $00:0000 to $0000 (assumes scratchpad ram)
   STX $4302         ;Set source address to $xx:0000
   LDA #$00
   STA $4304         ;Set source bank to $00
   LDX #$FFFF
   STX $4305         ;Set transfer size to 64k-1 bytes
   LDA #$01
   STA $420B         ;Initiate transfer

   STZ $2119         ;clear the last byte of the VRAM

   plp
   plx
   pla
   RTS

.include "charset.asm"
.include "ucs2char.asm"

.segment "VECTORS"
.word 0, 0        ;Native mode vectors
.word return_int  ;COP
.word return_int  ;BRK
.word return_int  ;ABORT
.word nmi         ;NMI
.word start       ;RST
.word return_int  ;IRQ

.word 0, 0        ;Emulation mode vectors
.word return_int  ;COP
.word 0
.word return_int  ;ABORT
.word nmi         ;NMI
.word start       ;RST
.word return_int  ;IRQ