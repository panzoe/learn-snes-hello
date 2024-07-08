.ifndef CHARMAP_INC
CHARMAP_INC = 1

; ASCII-ASCII character map to prevent translation
; 字符转换表，根据 AI 的回答，该文件的定义用于编译器在处理 字符串 字面量时的转换，目前该文件定义的转换即不转换，目的可能是覆盖一些编译器的配置值
; 目测编译器可能是在提取到 "" 字符串字面量的时候转换成 byte[] 再根据这张表进行值转换，也就是说 .charmap 指令可能只能处理 0-255 的值范围转换
; cc65 似乎是不支持 utf-8 源码及字节串的处理，如果想处理宽字节应该使用 WLA-DX 
; WLA-DX 支持的功能更多
; .charmap "你" $81                          ; 支持宽字符的重映射
; .stringmap script "你好世界"                ; 支持字符串的重映射 
; .stringmaptable script "my_char_map.tbl"  ; 支持从外部文件加载映射表
; .marcro CUSTOM_STRING str                 ; 支持自定义宏
;   .local len, i
;   len = .strlen(str)
;   i = 0
;   .repeat len, i
;       .db .charmap(str[i])
;       i = i + 1
;   .rend
;   .endmacro
; CUSTOM_STRING "你好世界"
.charmap $40, $40 ; @
.charmap $41, $41 ; A
.charmap $42, $42
.charmap $43, $43
.charmap $44, $44
.charmap $45, $45
.charmap $46, $46
.charmap $47, $47
.charmap $48, $48
.charmap $49, $49
.charmap $4a, $4a
.charmap $4b, $4b
.charmap $4c, $4c
.charmap $4d, $4d
.charmap $4e, $4e
.charmap $4f, $4f
.charmap $50, $50
.charmap $51, $51
.charmap $52, $52
.charmap $53, $53
.charmap $54, $54
.charmap $55, $55
.charmap $56, $56
.charmap $57, $57
.charmap $58, $58
.charmap $59, $59
.charmap $5a, $5a
.charmap $5b, $5b
.charmap $5c, $5c
.charmap $5d, $5d
.charmap $5e, $5e
.charmap $5f, $5f
.charmap $61, $61
.charmap $62, $62
.charmap $63, $63
.charmap $64, $64
.charmap $65, $65
.charmap $66, $66
.charmap $67, $67
.charmap $68, $68
.charmap $69, $69
.charmap $6a, $6a
.charmap $6b, $6b
.charmap $6c, $6c
.charmap $6d, $6d
.charmap $6e, $6e
.charmap $6f, $6f
.charmap $70, $70
.charmap $71, $71
.charmap $72, $72
.charmap $73, $73
.charmap $74, $74
.charmap $75, $75
.charmap $76, $76
.charmap $77, $77
.charmap $78, $78
.charmap $79, $79
.charmap $7a, $7a
.charmap $7b, $7b
.charmap $7c, $7c
.charmap $7d, $7d
.charmap $7e, $7e
.charmap $7f, $7f

.endif
