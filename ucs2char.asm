; ------------------------------------------------------------------------
; UCS-2 Character data
; ------------------------------------------------------------------------
; 512KB, 8x8 1bpp, 2^16 characters, BANK1 to BANK16

.segment "BANK1"
UCS2_FONT:
    .incbin "ucs2char.bin", $00000, $8000  ; 0-32KB

.segment "BANK2"
    .incbin "ucs2char.bin", $08000, $8000  ; 32-64KB

.segment "BANK3"
    .incbin "ucs2char.bin", $10000, $8000  ; 64-96KB

.segment "BANK4"
    .incbin "ucs2char.bin", $18000, $8000  ; 96-128KB

.segment "BANK5"
    .incbin "ucs2char.bin", $20000, $8000  ; 128-160KB

.segment "BANK6"
    .incbin "ucs2char.bin", $28000, $8000  ; 160-192KB

.segment "BANK7"
    .incbin "ucs2char.bin", $30000, $8000  ; 192-224KB

.segment "BANK8"
    .incbin "ucs2char.bin", $38000, $8000  ; 224-256KB

.segment "BANK9"
    .incbin "ucs2char.bin", $40000, $8000  ; 256-288KB

.segment "BANK10"
    .incbin "ucs2char.bin", $48000, $8000  ; 288-320KB

.segment "BANK11"
    .incbin "ucs2char.bin", $50000, $8000  ; 320-352KB

.segment "BANK12"
    .incbin "ucs2char.bin", $58000, $8000  ; 352-384KB

.segment "BANK13"
    .incbin "ucs2char.bin", $60000, $8000  ; 384-416KB

.segment "BANK14"
    .incbin "ucs2char.bin", $68000, $8000  ; 416-448KB

.segment "BANK15"
    .incbin "ucs2char.bin", $70000, $8000  ; 448-480KB

.segment "BANK16"
    .incbin "ucs2char.bin", $78000, $8000  ; 480-512KB