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
            dword ${name},${namelen},${flags},${label}
            .lit var_${label}
            .wp EXIT
        var_${label}:
            db ${value}, ^${value}
    .endm

    .macro dconst [name],namelen,flags=0,[label]=${name},value
        dword ${name},${namelen},${flags},${label}
        .lit ${value}
        .wp EXIT
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
    
    dcode BRKPT,5,F_HIDDEN,
        lda $01, s
        tax
        lda $03, s
        tay
        mmu $ff
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
    
    dword 2DUP,4,,TWODUP
        .wp OVER
        .wp OVER
    .wp EXIT
    
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
    
    dcode U*,1,,UMUL
        pla
        tsx
        mul $0001, x
        ply
        pha
    nxt
    
    dword 2*,2,,TWOMUL
        .lit 1
        .wp LSHIFT
    .wp EXIT
    
    dword U/MOD,5,,UDIVMOD
        .wp SWAP
        .wp _DM
    .wp EXIT
    
    dcode (U/MOD),7,,_DM
        pla
        tsx
        div $0001, x
        ply
        pha
        phd
    nxt
    
    dword U/,2,,UDIV
        .wp UDIVMOD
        .wp DROP
    .wp EXIT
    
    dword MOD,3,,
        .wp UDIVMOD
        .wp NIP
    .wp EXIT
    
    dword 2/,2,,TWODIV
        .lit 1
        .wp RSHIFT
    .wp EXIT
    
    dword =,1,,EQU
        .wp SUB
        .wp ZBRANCH
        .wp EQU_true
        .wp FALSE
        .wp EXIT
EQU_true:
        .wp TRUE
    .wp EXIT
    
    dword <>,2,,NEQU
        .wp EQU
        .wp INVERT
    .wp EXIT
    
    dword <,1,,LT
        .wp SUB
        .wp ZLT
    .wp EXIT
    
    
    dword >,1,,GT
        .wp SWAP
        .wp LT
    .wp EXIT
    
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
    
    dcode 0<,2,,ZLT
        pla
        bmi ZLT_yes
        lda #_F_FALSE
        bra ZLT_end
ZLT_yes:
        lda #_F_TRUE
ZLT_end:
        pha
    nxt
    
    dword 0>,2,,ZGT
        .lit 0
        .wp GT
    .wp EXIT
    
    dword 0<=,2,,ZLE
        .lit 0
        .wp LE
    .wp EXIT
    
    dword 0>=,2,,ZGE
        .lit 0
        .wp GE
    .wp EXIT
    
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
    
    dcode INVERT,6,,
        pla
        eor #$ffff
        pha
    nxt
    
    dword NEGATE,6,,
        .wp INVERT
        .wp INCR
    .wp EXIT
    
    dword ABS,3,,
        .wp DUP
        .wp ZGE
        .wp ZBRANCH
        .wp ABS_noaction
        .wp NEGATE
ABS_noaction:
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
    
    dword ?,1,,PRTADDR
        .wp PEEK
        .wp PRINT_UNUM
    .wp EXIT
    
    dword C!,2,,POKEBYTE
        .wp DUP
        .wp PEEK
        .wp SPLIT
        .wp DROP
        .wp ROT
        .wp JOIN
        .wp SWAP
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
    
    dword MEMTEST,7,F_HIDDEN,
        .wp DUP ; address address
        .wp PEEK ; address old-value
        .wp SWAP ; old-value address
        .wp TRUE ; old-value address $ffff
        .wp OVER ; old-value address $ffff address
        .wp POKE ; old-value address
        .wp DUP ; old-value address address
        .wp PEEK ; old-value address result
        .wp SWAP ; old-value result address
        .wp ROT ; result address old-value
        .wp POKE ; result
    .wp EXIT
    
    dword FREE,4,,
        .lit $2000
        
        .lit $2001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $4000
        
        .lit $4001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $6000
        
        .lit $6001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $8000
        
        .lit $8001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $a000
        
        .lit $a001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $c000
        
        .lit $c001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $e000
        
        .lit $e001
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $0000
        
FREE_end:
        .wp HERE
        .wp SUB
    .wp EXIT
    
    dword COLD,4,,
        ; print welcome messages, etc.
        
        .wp GETXY
        .wp DROP
        .wp ZBRANCH
        .wp COLD_a
        .wp CR
COLD_a:
        .wp CR
        .lit COLD_linea
        .lit 15
        .wp TYPE
        .wp CR
        .wp FREE
        .wp PRINT_UNUM
        .lit COLD_lineb
        .lit 11
        .wp TYPE
        .wp CR
        
        .wp QUIT
        
COLD_linea: db 'RCOS v0.1 alpha'
COLD_lineb: db 'bytes free.'

    dword QUIT,4,,
        .wp RPRST
QUIT_loop:
        .wp CR        
        .lit $3E ; show '> ' prompt
        .wp EMIT
        .lit $20
        .wp EMIT
        
        .lit $80
        .lit $80
        .wp READLINE ; read into address $80, max length $80
        
        .lit $80
        .wp SWAP
        .wp INTERPRET
        
        .lit QUIT_ok
        .lit 3
        .wp TYPE
        
        .wp BRANCH ; loop infinitely
        .wp QUIT_loop ; ($fffa = -6)
        
QUIT_ok: db ' ok'

    dconst TRUE,4,,,_F_TRUE
    dconst FALSE,4,,,_F_FALSE

    dcode BRANCH,6,,
        nxa
        tax
        txi
    nxt

    dcode 0BRANCH,7,,ZBRANCH
        nxa
        ply
        bne ZBRANCH_false
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
    
    dword @1+!,4,,PEEKINCR
        .wp DUP
        .wp PEEK
        .wp DUP
        .wp INCR
        .wp SWAP
        .wp NROT
        .wp SWAP
        .wp POKE
    .wp EXIT
    
    dword (?SKIP),7,F_HIDDEN,SHOULDSKIP
        .wp DUP
        .wp ZBRANCH
        .wp SKIP_yes
        
        .wp OVER
        .lit $22
        .wp PEEK
        .wp LT
        .wp ZBRANCH
        .wp SKIP_yes
        .wp BRANCH
        .wp SKIP_no
SKIP_yes:
        .wp FALSE
        .wp EXIT
SKIP_no:
        .wp TRUE
    .wp EXIT
    
    dword INTERPRET,9,, ; ( address length -- )
        .wp OVER
        .lit $20
        .wp POKE ; source address is at $20
        
        .wp ADD
        .lit $22
        .wp POKE ; end address is at $22
        
INTERPRET_loop:
        
        .wp WORD
        .wp SHOULDSKIP
        .wp ZBRANCH
        .wp INTERPRET_end
        
        .wp NUMBER
        .wp PRINT_UNUM
        .wp PRINT_UNUM
        ;.wp TYPE
        
        .wp BRANCH
        .wp INTERPRET_loop
        
INTERPRET_end:
        .wp TWODROP
    .wp EXIT
    
    dword (GCL),5,F_HIDDEN,GCL
        .lit $20
        .wp PEEKINCR
    .wp EXIT
    
    dword WORD,4,, ; ( -- word-address word-length )
WORD_loop_a:
        .wp GCL
        .wp DUP
        .wp PEEKBYTE
        .wp BL
        .wp EQU
        .wp ZBRANCH
        .wp WORD_noblank
        .wp DROP
        .wp BRANCH
        .wp WORD_loop_a
WORD_noblank:
        
        .lit 0
        
WORD_loop_b:
        
        .wp TWODUP
        .wp ADD
        .lit $22
        .wp PEEK
        .wp GE
        .wp ZBRANCH
        .wp WORD_continue
        .wp EXIT
WORD_continue:
        
        .wp INCR
        
        .wp GCL
        .wp PEEKBYTE
        .wp BL
        .wp EQU
        .wp ZBRANCH
        .wp WORD_noblank_b
        .wp EXIT
WORD_noblank_b:
        
        .wp BRANCH
        .wp WORD_loop_b
        
    .wp EXIT
    
    dword UNLOOP,6,,
        .wp RDROP
        .wp RDROP
    .wp EXIT
    
    dword (GDL),5,F_HIDDEN,GDL
        .lit $24
        .wp PEEKINCR
    .wp EXIT
    
    dword KEY>DIGIT,9,,KEYTODIGIT
        .lit $FF
        .wp AND
        .lit $30
        .wp SUB
        .wp DUP
        .lit $10
        .wp GT
        .wp ZBRANCH
        .wp KD_nooffset
        .lit 7 
        .wp SUB
KD_nooffset:
    .wp EXIT
    
    dword NUMBER,6,, ; ( word-address word-length -- result upcc )
        .lit 0
        .lit $26
        .wp POKE ; set result to 0
        
        .wp OVER
        .lit $24
        .wp POKE
        
        .wp DUP ; w-a w-l w-l
        .wp ZBRANCH
        .wp NUMBER_zerolength
        
        .wp BASE
        .wp PEEK ; w-a w-l base
        
        .wp ROT ; w-l base w-a
        .wp DUP ; w-l base w-a w-a
        .wp PEEKBYTE ; w-l base w-a first-char
        .lit $2d
        .wp EQU
        
        .wp DUP ; w-l base w-a cond cond
        .wp ZBRANCH
        .wp NUMBER_notneg
        .wp GDL
        .wp DROP ; skip char from parsing
NUMBER_notneg:
        
        .wp TOR ; save whether the number should be negative for the end
                ; w-l base w-a
                
        .wp ROT ; base w-a w-l
        
        .lit 0 ; base w-a w-l 0
        
NUMBER_loop:
        .wp TOR ; base w-a w-l
        .wp TOR ; base w-a
        
        .wp SWAP ; w-a base
        .lit $26
        .wp PEEK ; w-a base value
        .wp OVER ; w-a base value base
        .wp UMUL ; w-a base newv
        .wp OVER ; w-a base newv base
        
        .wp GDL
        .wp PEEK ; w-a base newv base char
        .wp KEYTODIGIT ; w-a base newv base digit
        .wp DUP ; w-a base newv base digit digit
        .wp ZGE
        .wp ZBRANCH
        .wp NUMBER_invalidchar
        .wp DUP ; w-a base newv base digit digit
        .wp NROT ; w-a base newv digit base digit
        .wp GT
        .wp ZBRANCH
        .wp NUMBER_invalidchar
        
        .wp ADD ; w-a base newv+digit
        .lit $26
        .wp POKE ; w-a base
        .wp SWAP ; base w-a
        
        .wp FROMR ; base w-a w-l
        .wp FROMR ; base w-a w-l i
        .wp INCR ; base w-a w-l i+1
        .wp TWODUP ; base w-a w-l i+1 w-l i+1
        .wp EQU ; base w-a w-l i+1 cond
        .wp ZBRANCH ; base w-a w-l i+1
        .wp NUMBER_loop
        .wp TWODROP ; base w-a
        .wp TWODROP ;
        
        .lit $26
        .wp PEEK ; value
        .wp FROMR ; value cond
        .wp ZBRANCH ; value
        .wp NUMBER_notneg_b
        .wp NEGATE ; -value
NUMBER_notneg_b:
        .lit 0 ; value 0
        
    .wp EXIT

NUMBER_zerolength:
        .wp TWODROP
        .lit 0
        .lit 0
    .wp EXIT
    
NUMBER_invalidchar:
        .wp TWODROP ; w-a base
        .wp TWODROP ; 
        .wp FROMR ; w-l
        .wp FROMR ; w-l i
        .wp RDROP
        .wp SUB ; r-c
        .lit $26
        .wp PEEK ; r-c result
        .wp SWAP ; result r-c
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
TYPE_loop:
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
        .wp TYPE_loop
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

    dword READ-LINE,9,,READLINE ; ( c-addr maxlength -- read )
        .wp TOR
        .wp TOR
        
        .lit 0
        
RL_loop:
        .wp KEY
        
        .wp DUP
        .wp RETURN
        .wp EQU
        .wp ZBRANCH
        .wp RL_a
        .wp DROP
        .wp RDROP
        .wp RDROP
        .wp SPACE
        .wp EXIT
RL_a:
        .wp DUP
        .wp BACK
        .wp EQU
        .wp ZBRANCH
        .wp RL_b
        .wp DROP
        .wp DUP
        .wp ZBRANCH
        .wp RL_loop
        .wp GETXY
        .wp DROP
        .wp ZBRANCH
        .wp RL_loop
        .wp GETXY
        .wp SWAP
        .wp DECR
        .wp SWAP
        .wp TWODUP
        .wp SETXY
        .wp SPACE
        .wp SETXY
        .wp DECR
        .wp BRANCH
        .wp RL_loop
RL_b:
        .wp OVER
        .wp FROMR
        .wp FROMR
        .wp DUP
        .wp NROT
        .wp TOR
        .wp TOR
        .wp EQU
        .wp ZBRANCH
        .wp RL_c
        .wp DROP
        .wp BRANCH
        .wp RL_loop
RL_c:
        
        .wp DUP
        .wp EMIT
        .wp OVER
        .wp RFETCH
        .wp ADD
        .wp POKEBYTE
        .wp INCR
        .wp BRANCH
        .wp RL_loop
    ;.wp EXIT
    
    dword U.,2,,PRINT_UNUM
        .wp DUP
        .wp TOR
        .wp BASE
        .wp PEEK
        .wp TOR
        
        .lit 0
        
PN_loop:
        ; int counter
        .wp SWAP ; counter int
        .wp RFETCH ; counter int base
        .wp UDIVMOD ; counter result mod
        .lit $30 ; counter result mod offset
        .wp ADD ; counter result char
        .wp ROT ; result char counter
        .wp DUP ; result char counter counter
        .lit $7f ; result char counter counter $7f
        .wp SWAP ; result char counter $7f counter
        .wp SUB ; result char counter address
        .wp ROT ; result counter address char
        .wp SWAP ; result counter char address
        .wp POKEBYTE ; result counter
        .wp INCR ; result counter+1
        .wp OVER ; result counter+1 result
        .wp ZBRANCH
        .wp PN_end
        .wp BRANCH
        .wp PN_loop        
PN_end:
        .wp SWAP ; counter result
        .wp DROP ; counter
        .wp RDROP
        .wp RDROP
        .lit $7f
        .wp OVER ; counter $7f counter
        .wp SUB ; counter address
        .wp INCR
        .wp SWAP
        
        .wp TYPE
        .wp SPACE
    .wp EXIT

    dword ls,2,,
        .lit text_linux
        .lit 31
        .wp TYPE
    .wp EXIT
    
    dword dir,3,,
        .lit text_linux
        .lit 25
        .wp TYPE
        .lit text_windows
        .lit 8
        .wp TYPE
    .wp EXIT
    
text_linux: db 'No dice buddy', $2C, ' this ain't Linux!'
text_windows: db 'Windows!'
    
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
