; Forth OS (RCOS)

    .macro .wp num
        db ${num}, ^${num}
    .endm
    
    .macro .lit num
        .wp LIT
        .wp ${num}
    .endm

    .macro dcode [name],namelen,flags=0,[label]=${name}
        name_${label}:
            db wptr, ^wptr
            .set wptr, name_${label}
            db ${flags}+${namelen}, '${name}'
        ${label}:
    .endm

    .macro dword [name],namelen,flags=0,[label]=${name}
        dcode ${name},${namelen},${flags},${label}
        ent DOCOL
    .endm

    .macro dvar [name],namelen,flags=0,[label]=${name},value=0
            dcode ${name},${namelen},${flags},${label}
            lda var_${label}
            pha
            nxt
        var_${label}:
            db ${value}, ^${value}
    .endm

    .macro dconst [name],namelen,flags=0,[label]=${name},value
        dcode ${name},${namelen},${flags},${label}
        lda #${value}
        pha
        nxt
    .endm

section dict

    .set F_IMMED, $80
    .set F_HIDDEN, $40
    .set F_COMPILEONLY, $20
    .set F_LENMASK, $1F

    .set wptr, $00
    
    ; FORTH Constants
    
    .set _F_RPORIG,$02FF
    .set _F_SPORIG,$01FF
    .set _F_BL,$20
    .set _F_BACKSPACE,$08
    .set _F_RETURN,$0D
    .set _F_TRUE,$FFFF
    .set _F_FALSE,$0000

    dcode DOCOL,5,,
    nxt

    dcode EXIT,4,,
        rli
    nxt
    
    dcode LIT,3,,
        nxa
        pha
    nxt

    dconst RP0,3,,RPORIG,_F_RPORIG

    dcode RP@,3,,RPGET
        trx
        phx
    nxt

    dcode RP!,3,,RPSTORE
        plx
        txr
    nxt

    dcode 0RP,3,,RPRST
        ldx #_F_RPORIG
        txr
    nxt

    dconst SP0,3,,SPORIG,_F_SPORIG

    dcode SP@,3,,SPGET
        tsx
        phx
    nxt

    dcode SP!,3,,SPSET
        plx
        txs
    nxt

    dcode 0SP,3,,SPRST
        ldx #_F_SPORIG
        txs
    nxt
    
    dcode DROP,4,,
        pla
    nxt
    
    dcode SWAP,4,,
        plx
        ply
        phx
        phy
    nxt
    
    dcode DUP,3,,
        lda $01, s
        pha
    nxt
    
    dcode OVER,4,,
        lda $03, s
        pha
    nxt
    
    dword NIP,3,,
        .wp SWAP
        .wp DROP
    .wp EXIT
    
    dword TUCK,4,,
        .wp SWAP
        .wp OVER
    .wp EXIT
    
    dcode ROT,3,,
        pla
        plx
        ply
        phx
        pha
        phy
    nxt
    
    dcode -ROT,4,,NROT
        pla
        plx
        ply
        pha
        phy
        phx
    nxt
    
    dcode 2DROP,5,,TWODROP
        pla
        pla
    nxt
    
    dcode 2DUP,4,,TWODUP
        lda $01, s
        tax
        lda $03, s
        pha
        phx
    nxt
    
    dcode 2SWAP,5,,TWOSWAP
        plx
        ply
        rhx
        rhy
        plx
        ply
        rla
        pha
        rla
        pha
        phy
        phx
    nxt
    
    dcode ?DUP,4,,QDUP
        pla
        pha
        beq QDUP_zero
        pha
QDUP_zero:
    nxt
    
    dcode 1+,2,,INCR
        pla
        inc a
        pha
    nxt
    
    dcode 1-,2,,DECR
        pla
        dec a
        pha
    nxt
    
    dcode 2+,2,,INCRTWO
        pla
        inc a
        inc a
        pha
    nxt
    
    dcode 2-,2,,DECRTWO
        pla
        dec a
        dec a
        pha
    nxt
    
    dcode +,1,,ADD
        clc
        pla
        clc
        adc $01, s
        ply
        pha
    nxt
    
    dcode -,1,,SUB
        lda $03, s
        sec
        sbc $01, s
        ply
        ply
        pha
    nxt
    
    dcode *,1,,MUL
        pla
        tsx
        mul $0000, x
        ply
        pha
    nxt
    
    dword 2*,2,,MULTWO
        .lit 1
        .wp LSHIFT
    .wp EXIT
    
    dcode /MOD,4,,DIVMOD
        pla
        tsx
        div $0000, x
        ply
        pha
        phd
    nxt
    
    dword /,1,,DIV
        .wp DIVMOD
        .wp DROP
    .wp EXIT
    
    dword MOD,3,,
        .wp DIVMOD
        .wp NIP
    .wp EXIT
    
    dword 2/,2,,TWODIV
        .lit 1
        .wp RSHIFT
    .wp EXIT
    
;    dcode =,1,,EQU
;        pla
;        sec
;        sbc $02, s
;        beq EQU_yes
;        lda #_F_FALSE
;        bra EQU_end
;EQU_yes:
;        lda #_F_TRUE
;EQU_end:
;        ply
;        pha
;    nxt
    
    dword =,1,,EQU
        .wp SUB
        .wp ZBRANCH
        .wp $0004
        .wp FALSE
        .wp EXIT
        .wp TRUE
    .wp EXIT
    
    dword <>,2,,NEQU
        .wp EQU
        .wp INVERT
    .wp EXIT
    
    dcode <,1,,LT
        pla
        sec
        sbc $01, s
        bcs LT_no
        lda #_F_TRUE
        bra LT_end
LT_no:
        lda #_F_FALSE
LT_end:
        ply
        pha
    nxt
    
    dword >,1,,GT
        .wp SWAP
        .wp LT
    nxt
    
    dword <=,2,,LE
        .wp GT
        .wp INVERT
    .wp EXIT
    
    dword >=,2,,GE
        .wp LT
        .wp INVERT
    .wp EXIT
    
    dcode 0=,2,,ZEQU
        pla
        beq ZEQU_yes
        lda #_F_FALSE
        bra ZEQU_end
ZEQU_yes:
        lda #_F_TRUE
ZEQU_end:
        pha
    nxt
    
    dcode AND,3,,
        pla
        and $01, s
        ply
        pha
    nxt
    
    dcode OR,2,,
        pla
        ora $01, s
        ply
        pha
    nxt
    
    dcode XOR,3,,
        pla
        eor $01, s
        ply
        pha
    nxt
    
    dword INVERT,6,,
        pla
        eor #$ffff
        pha
    .wp EXIT
    
    dcode LROTATE,7,,
        clc
        ply ; get amount to rot left
        pla ; get number
LROT_loop:
        rol a
        dey
        bne LROT_loop
        bcc LROT_end
        inc a
LROT_end:
        pha
    nxt
    
    dcode RROTATE,7,,
        clc
        ply ; get amount to rot right
        pla ; get number
RROT_loop:
        ror a
        dey
        bne RROT_loop
        bcc RROT_end
        clc
        adc #$8000
RROT_end:
        pha
    nxt
    
    dcode LSHIFT,6,,
        ply ; get amount to rot left
        pla ; get number
LSHIFT_loop:
        clc
        rol a
        dey
        bne LSHIFT_loop
        pha
    nxt
    
    dcode RSHIFT,6,,
        ply ; get amount to rot left
        pla ; get number
RSHIFT_loop:
        clc
        ror a
        dey
        bne RSHIFT_loop
        pha
    nxt
    
    dconst CELL,4,,,$02
    
    dcode !,1,,POKE
        lda $03, s
        ldy #$0000
        sta ($01, s), y
        ply
        ply
    nxt
    
    dcode @,1,,PEEK
        ldy #$0000
        lda ($01, s), y
        ply
        pha
    nxt
    
    dword C!,2,,POKEBYTE
        .wp DUP
        .wp PEEK
        .wp SPLIT
        .wp DROP
        .wp ROT
        .wp JOIN
        .wp POKE
    .wp EXIT
    
    dword C@,2,,PEEKBYTE
        .wp PEEK
        .lit $FF
        .wp AND
    .wp EXIT
    
    dvar STATE,5,,,
    dvar HERE,4,,,here_pos
    dvar LATEST,6,,,name_WORDS
    dvar BASE,4,,,10
    
    dcode >R,2,,TOR
        pla
        rha
    nxt
    
    dcode R>,2,,FROMR
        rla
        pha
    nxt
    
    dcode R@,2,,RFETCH
        lda $01, r
        pha
    nxt
    
    dcode RSP@,4,,RSPGET
        trx
        phx
    nxt
    
    dcode RSP!,4,,RSPSET
        plx
        txr
    nxt
    
    dcode RDROP,5,,RDROP
        rla
    nxt
    
    dword COLD,4,,
        ; print welcome messages, etc.
        .wp CR
        .lit COLD_wline
        .lit 15
        .wp TYPE
        .wp CR
        .wp QUIT
        
COLD_wline: db 'RCOS v0.1 alpha'

    dword QUIT,4,,
        .wp RPRST
        .wp INTERPRET
        .wp BRANCH ; this jumps back to INTERPRET
        .wp $fffa ; ($fffa = -6)

    dconst TRUE,4,,,_F_TRUE
    dconst FALSE,4,,,_F_FALSE

    dcode BRANCH,6,,
        clc
        nxa
        tix
        stx $02
        adc $02
        clc
        tax
        txi
    nxt

    dcode 0BRANCH,7,,ZBRANCH
        clc
        nxa
        ply
        bne ZBRANCH_false
        tix
        stx $02
        adc $02
        clc
        tax
        txi
ZBRANCH_false:
    nxt
    
    dcode TICK,4,,
        wai
    nxt
    
    dcode I,1,,
        lda $03, r
        pha
    nxt
    
    dcode J,1,,
        lda $07, r
        pha
    nxt
    
    dcode K,1,,
        lda $0b, r
        pha
    nxt

    dword INTERPRET,9,,
        .wp CR        
        .lit $3E ; show '> ' prompt
        .wp EMIT
        .lit $20
        .wp EMIT
        .lit $80
        .lit $80
        
        .wp READLINE ; read into address $80, max length $80
        .wp TYPE
    .wp EXIT
    
    dword SPLIT,5,,
        .wp DUP
        .lit 8
        .wp RSHIFT
        .wp SWAP
        .lit $00FF
        .wp AND
    .wp EXIT
    
    dword JOIN,4,,
        .lit $00FF
        .wp AND
        .wp SWAP
        .lit 8
        .wp LSHIFT
        .wp OR
    .wp EXIT
    
    dcode HALT,4,,
        stp

    dvar TERMADDR,8,,,1
    dvar DISKADDR,8,,,2
    dvar IOXADDR,7,,,3

    dcode BA!,3,,BUS_SETADDR
        pla
        mmu $00
    nxt
    
    dcode BA@,3,,BUS_GETADDR
        mmu $80
        pha
    nxt

    dcode BW!,3,,BUS_SETWIN
        pla
        mmu $01
    nxt

    dcode BW@,3,,BUS_GETWIN
        mmu $81
        pha
    nxt

    dconst BL,2,,,_F_BL
    dconst RETURN,6,,,_F_RETURN
    dconst BACK,4,,,_F_BACKSPACE
    
    dword SPACE,5,,
        .wp BL
        .wp EMIT
    .wp EXIT
    
    dcode BINDTERM,8,,
        jsr bind_term
    nxt
    
    bind_term:
        lda var_TERMADDR
        mmu $00
    rts
    
    dcode EMIT,4,,
        jsr bind_term
        mmu $81
        tay
        sep #$20
        lda $0002, y
        sta $0000, y
        lda $0001, y
        tax
        clc
        adc #$10
        sta $00
        stz $01
        pla ; get char
        sta ($0000), y
        pla ; throw high bytes away
        txa
        inc a
        cmp #$50
        beq CR
        sta $0001, y
        rep #$20
    nxt

    dcode CR,2,,
        jsr bind_term
        mmu $81
        tax
        sep #$20
        stz $0001, x
        lda $0002, x
        inc a
        cmp #$32
        sta $0002, x
        beq SCROLL
        rep #$20
    nxt
        
    dcode SCROLL,6,,
        jsr bind_term
        mmu $81
        tax
        sep #$20
        stz $0008, x
        stz $000A, x
        stz $000B, x
        lda #$01
        sta $0009, x
        lda #$50
        sta $000C, x
        lda #$31
        sta $000D, x
        lda #$03
        sta $0007, x
        wai
        lda #$20
        sta $0008, x
        lda #$31
        sta $000B, x
        lda #$01
        sta $000D, x
        sta $0007, x
        wai
        lda $0002, x
        beq SCROLL_nocursor
        dec $0002, x
SCROLL_nocursor:
        rep #$20
    nxt
    
    dword TYPE,4,,
        .lit 0
        .wp TOR
        .wp TOR
        .wp DUP
        .wp I
        .wp ADD
        .wp PEEK
        .wp EMIT
        .wp FROMR
        .wp FROMR
        .wp INCR
        .wp TWODUP
        .wp EQU
        .wp ZBRANCH
        .wp $ffe4
        .wp TWODROP
        .wp DROP
    .wp EXIT
    
    dword XY@,3,,GETXY
        .wp BINDTERM
        .wp BUS_GETWIN
        .wp INCR
        .wp PEEK
        .wp SPLIT
        .wp SWAP
    .wp EXIT
    
    dword XY!,3,,SETXY
        .wp SWAP
        .wp JOIN
        .wp BINDTERM
        .wp BUS_GETWIN
        .wp INCR
        .wp POKE
    .wp EXIT
    
    dcode KEY,3,,
        mmu $81
        tax
        lda #$0000
        sep #$20
        bra KEY_check
KEY_waitloop:
        wai
KEY_check:
        lda $0004, x
        cmp $0005, x
        beq KEY_waitloop
        lda $0006, x
        inc $0004, x
        rep #$20
        pha
    nxt
    
    dword DECSR,5,F_HIDDEN,
        .wp GETXY
        .wp SWAP
        .wp DECR
        .wp SWAP
        .wp TWODUP
        .wp SETXY
        .wp SPACE
        .wp SETXY
    .wp EXIT
    
    dword READ-LINE,9,,READLINE ; ( c-addr maxlength -- read )
        .wp TOR
        .wp TOR
        ; version 1391293912931912893893898912, fuck
    .wp EXIT
    
    dword WORD,4,,
    .wp EXIT
    
    dcode U.,1,,PRINT_UNUM
        ; print number and space
    nxt

    dword WORDS,5,,

    .wp EXIT
here_pos:
    
section .text

    ;lda $00
    ;sta var_DISKADDR
    ;lda $01
    ;sta var_TERMADDR
start:
    clc
    rep #$30
    lda #$0300
    mmu $01
    mmu $02
    lda #$0400
    mmu $03
    mmu $04
    lda #start
    mmu $05
    mmu $06
    ent COLD
