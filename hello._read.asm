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

   ; REP,SEP 都是针对 P 寄存器的指令
   ; REP 会重置值为 1 的位对应的状态
   ; SEP 会设置值为 1 的位对应的状态
   ; P 寄存器的位定义如下：
   ;|    负数   |    溢出   |    内存/累加器标志位 |    索引寄存器宽度标志位 |    十进制模式标志位 |    中断屏蔽标志位   |   零标志位 | 进位标志位 |
   ;|     N    |     V    |           M        |           X          |         D        |          I        |     Z    |      C     |
   ;| Negative | Overflow | Memory/Accumulator | Index register size  |   Decimal mode   | Interrupt disable |    Zero  |    Carry   |
   ; 不是所有位组合码都有实际用途或频繁设置，比如 C,Z,V,N 通常是由算术和逻辑操作自动设置的
   ; REP(Reset Processor Status Bits) 重置处理器状态位，针对的是 P 寄存器
   ; $30 -> 0B00110000， 也就是重置 X/Y 寄存器为 16 位
   REP #$30		; mem/A = 8 bit, X/Y = 16 bit
   ; SEP(SET Processor Status Bits) 设置处理器状态位
   ; $20 -> 0B00100000， 也就是设置 A 寄存器为 8 位
   SEP #$20

   ; LDA(LoaD Accumulator) 从内存加载数据到 A 寄存器
   LDA #$80
   ; STA(STore Accumulator) 将 A 寄存器的值存储到指定内存地址
   ; $2100 - $21FF 主要用于 PPU(Picture Processing Unit，即图像处理单元) 寄存器
   ; $2115(VMAIN) 用于设置 VRAM 地址增量模式（也就是访问字节模式）
   ; 这里设置为字模式，即每次访问 VRAM 时，地址自动增加 2
   STA $2115

   ; SFC 使用 Little-Endian (小端)字节序，即低字节在前，高字节在后
   ; $1809 -> 0B00011000_000_01_0_0_1
   ; 0B0001_1000 -> $4300 高字节放于低地址
   ; 0B0000_1001 -> $4301 低字节放于高地址
   ; $4300 用于设置传输目标的偏移地址，DMA 的目标地址仅限于 $21xx 系列寄存器，所以这里8位地址是偏移量
   ; $4301 是 8位状态寄存器，用于设置 DMA 传输模式
   ;
   ; ### DMA 传输模式 ###
   ; 1-3bit: 未使用 | 4-5bit: 传输模式 | 6bit: HDMA间接寻址模式 | 7bit: 传输方向 | 8bit: 启用双寄存器I/O
   ;
   ; ### HDMA间接寻址模式 ###
   ; 允许在每个扫描线上自动更新寄存器，创建复杂图形效果，如每行变化的背景色或者模式7的透视效果
   ; 直接寻址模式HDMA表包含要传输的数据，间接寻址模式则包含指向实际数据的指针
   ; 直接模式适用于数据量较小或者变化不频繁的情况，间接模式适用于需要频繁更改大量数据的情况
   ; 间接模式允许更灵活地管理和更新 HDMA 数据，可以节省 WRAM，因为可以重用数据块
   ; HDMA 是一个复杂的功能，需要仔细规则时序
   ; 仅 HDMA 模式下有效，普通操作将忽略
   ;
   ; ### 传输模式 ### 
   ; A) 0B00(0): 1字节传输，同一地址
   ; 每次传输只写入/读取一个字节，目标是 $2100-$21FF 范围内的单个地址(由$4300指定)
   ; B) 0B01(1): 2字节传输，同一地址
   ; 每次传输写入/读取两个字节，同一地址(由$4300指定)，通常用于某些支持16位操作的寄存器
   ; C) 0B10(2): 2次1字节传输，连续地址
   ; 设$4300指定地址为 n，则 1st: $21n, 2nd: $21(n+1)，第二轮传输时 3rd: $21(n+2), 4th: $21(n+3) ... 以此类推
   ; D) 0B11(3): 2次2字节传输，交替地址
   ; 设$4300指定地址为 n，则 1st: $21n, 2nd: $21(n-1)，第二轮传输时 3rd: $21(n), 4th: $21(n-1) ... 以此类推
   ; 由于 VRAM 的 IO 仅限于 $2118,$2119，所以一般会设置为 $2119 实现交替
   ;
   ; ### 传输方向 ###
   ; 0: 从CPU到PPU,即写入 1: 从PPU到CPU，即读取
   ;
   ; ### 启用双寄存器I/O ###
   ; 0: 禁用 1: 启用对 VRAM 数据端口 ($2118,$2119) 的特殊处理
   ; --------------------------------------------------------
   ; VMRAM 是通过两个 8位 寄存器访问的，两者共同构成了一个 16位的 VRAM 数据端口
   ; $2119 负责高位数据， $2118 负责低位数据 (与SFC CPU的地址风格相反)
   ; 一般与 传输模式 0B11(交替地址模式) 一起使用(交替地址使用:n , n-1 的顺序读取和写入)
   LDX #$1809
   ; $4300 是 DMA 控制器0的参数寄存器地址，用于设置 DMA 传输模式
   ; 这是一个16位寄存器，低8位($4300)用于设置 DMA 模式，高8位($4301)用于设置传输模式等
   ; $2118,$2119 是 VRAM 数据I/O端口
   ; $2122 是 CGRAM 数据I/O端口
   ; $2124 是 OAM 数据I/O端口
   ; DMA 主要用途：快速向 VRAM 写入数据、更新调色板，修改精灵属性
   STX $4300

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

   ; 按顺序将函数开头压入堆栈的寄存器状态恢复
   plp
   plx
   pla
   ; rts (Return from Subroutine) 从子程序返回，JSR 指令会跳转前的地址压入了堆栈，此指令会将其弹出并跳转到该地址
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