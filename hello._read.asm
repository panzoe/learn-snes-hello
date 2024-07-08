; 使用 ca65 构建 SFC 程序ROM 的最小示例

.p816   ; 指示编译器是 65816(即 SFC 硬件使用的) 处理器
.i16    ; X/Y 寄存器设置为 16 位
.a8     ; A 寄存器设置为 8 位

.include "snes.inc"        ; 引入地址常量表
.include "charmap.inc"     ; 引入ASCII-128字符映射表

.segment "HEADER"    ; 设置头信息段
.byte "CA65 EXAMPLE" ; ROM 名称

.segment "ROMINFO"   ; 设置ROM信息段
.byte $30            ; ROM映射模式：LoROM, 支持快速访问
.byte 0              ; 不使用电池供电的RAM（即无游戏存档功能）
.byte $07            ; ROM 大小：128K
.byte 0,0,0,0        ; 保留字段
.word $AAAA,$5555    ; 虚拟的检验和其补码，在实际发布前，需要使用工具计算更新

.segment "CODE"      ; 从这里开始基本上全是代码段内容
   jmp start         ; 跳转到 start 标签处，即程序入口，用于兼容某些开发工具或模拟器，同时也符合 6502 时代早期的汇编风格

; 定义一些常量
VRAM_CHARSET   = $0000 ; 地址必须 $1000 (4K)边界对齐
VRAM_BG1       = $1000 ; 地址必须 $0400 (1K)边界对齐
VRAM_BG2       = $1400 ; 地址必须 $0400 (1K)边界对齐
VRAM_BG3       = $1800 ; 地址必须 $0400 (1K)边界对齐
VRAM_BG4       = $1C00 ; 地址必须 $0400 (1K)边界对齐
START_X        = 9
START_Y        = 14
START_TM_ADDR  = VRAM_BG1 + 32*START_Y + START_X

hello_str: .asciiz "Hello, World!"  ; 将字符串转为ASCII码序列，并添加NULL结尾(\0) 存储于此

; 程序入口地址标签,全局性标签
start:
   ; CLC(Clear Carry Flag) 清除进位标志位，即进入本地(native)模式
   ; 65816处理器在启动时默认为模拟(Emulation)模式，需要通过此指令切换到本地模式
   ; 模拟模式主要用于兼容旧 6502 处理器行为
   ; clc -> xce 从模拟模式切换到本地模式， sec -> xce 从本地模式切换到模拟模式
   clc
   xce
   ; rep (Reset Processor Status Bits) 重置处理器状态位
   ; X/Y 寄存器设置为 16 位
   rep #$10
   ; sep (Set Processor Status Bits) 设置处理器状态位
   ; A 寄存器设置为 8 位
   sep #$20

   ; 清空寄存器
   ; ldx (Load X Register) 从内存加载数据到 X 寄存器
   ; # 井号为立即数标识符，表示后面的数值为立即数（也就是数值字面量）
   ldx #$33

   ; 跳转到 ClearVRAM 函数，函数执行后，会接着执行下面的代码
   jsr ClearVRAM

; @ 为标签前缀，用于标识局部标签
; 局部标签是可以重复定义的，因为它只在定义的附近有效
; 局部标签只用于小范围内的代码块，比如循环体等
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
@charset_loop:
   lda NESfont,x
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
; ClearVRAM -- 将 VRAM 清空（即：每个字节都设置为零）
; 代码来源： bazz 的 VRAM 教程
; 输入: 无
; 输出: 无
; 修改: 标志位
;----------------------------------------------------------------------------
ClearVRAM:
   ; 保存寄存器状态，只要保存这个函数里需要用到的寄存器就可以了
   ; 保存 A/X/P 寄存器，压入堆栈，以便后续恢复
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

.include "charset.asm"     ; 引入ASCII-128字符集
.include "ucs2char.asm"    ; 引入UCS-2字符集

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