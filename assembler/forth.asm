; Forth OS (RCOS)

; Known bugs:
; - LOAD is buggy when in compile mode
;
;
;
;

    .macro .wp num
        db ${num}, ^${num}
    .endm
    
    .macro .lit num
        .wp LIT
        .wp ${num}
    .endm
    
    .macro .compb num
        .lit ${num}
        .wp CCOMMA
    .endm
    
    .macro .comp num
        .lit ${num}
        .wp COMMA
    .endm
    
    .macro .clt num
        .comp LIT
        .comp ${num}
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
            ent DOVAR
        var_${label}:
            db ${value}, ^${value}
    .endm

    .macro dconst [name],namelen,flags=0,[label]=${name},value
            dcode ${name},${namelen},${flags},${label}
            ent DOCON
        const_${label}:
            db ${value}, ^${value}
    .endm

;   .macro callf word
;           lda #${word}
;           jsr callf_do
;   .endm

section 0_strings

    ALLOT_str: db 'Out of memory'
    INTERPRET_texta: db 'Unknown Token\: '
    INTERPRET_textb: db 'Stack Empty'
    INTERPRET_textc: db 'Stack Overflow'
    INTERPRET_textd: db 'Interpreting compile-only word\: '
    QUIT_ok: db ' ok'
    QUIT_prompta: db '> '
    QUIT_promptb: db 'compile\: '
    COLD_linea: db 'RCOS v0.9 alpha'
    COLD_lineb: db 'bytes free.'
    not_implemented: db 'Not implemented'
    
section 1_dict

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
    
    dcode DOVAR,5,,
        tix
        phx
        rli
    nxt
    
    dcode DOCON,5,,
        nxa
        pha
        rli
    nxt

    dcode EXIT,4,,
        rli
    nxt
    
    dcode LIT,3,,
        nxa
        pha
    nxt

    dconst 0,1,,ZERO,0
    dconst 1,1,,ONE,1
    
    dcode BRKPT,5,F_HIDDEN,
        lda $01, s
        tax
        lda $03, s
        tay
        mmu $ff
    nxt

    dconst RP0,3,,RPORIG,_F_RPORIG

    dcode RP@,3,,RPGET ; ( -- RP )
        trx
        phx
    nxt

    dcode RP!,3,,RPSTORE ; ( RP -- )
        plx
        txr
    nxt

    dcode 0RP,3,,RPRST ; ( -- )
        ldx #_F_RPORIG
        txr
    nxt

    dconst SP0,3,,SPORIG,_F_SPORIG

    dcode SP@,3,,SPGET ; ( -- SP )
        tsx
        phx
    nxt

    dcode SP!,3,,SPSET ; ( SP -- )
        plx
        txs
    nxt

    dcode 0SP,3,,SPRST ; ( -- )
        ldx #_F_SPORIG
        txs
    nxt
    
    dword DEPTH,5,, ; ( -- stack-depth )
        .wp SPORIG
        .wp SPGET
        .wp SUB
        .lit 2
        .wp SUB
        .wp CELL
        .wp DIV
    .wp EXIT
    
    dcode DROP,4,, ; ( a -- )
        pla
    nxt
    
    dcode SWAP,4,, ; ( a b -- b a )
        plx
        ply
        phx
        phy
    nxt
    
    dcode DUP,3,, ; ( a -- a a )
        lda $01, s
        pha
    nxt
    
    dcode OVER,4,, ; ( a b -- a b a )
        lda $03, s
        pha
    nxt
    
    dword NIP,3,, ; ( a b -- b )
        .wp SWAP
        .wp DROP
    .wp EXIT
    
    dword TUCK,4,, ; ( a b -- b a b )
        .wp SWAP
        .wp OVER
    .wp EXIT
    
    dcode ROT,3,, ; ( a b c -- b c a )
        pla
        plx
        ply
        phx
        pha
        phy
    nxt
    
    dcode -ROT,4,,NROT ; ( a b c -- c a b )
        pla
        plx
        ply
        pha
        phy
        phx
    nxt
    
    dcode 2DROP,5,,TWODROP ; ( a b -- )
        pla
        pla
    nxt
    
    dword 2DUP,4,,TWODUP ; ( a b -- a b a b )
        .wp OVER
        .wp OVER
    .wp EXIT
    
    dcode 2SWAP,5,,TWOSWAP ; ( a b c d -- c d a b )
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
    
    dcode ?DUP,4,,QDUP ; ( a -- a a )
        pla
        pha
        beq QDUP_zero
        pha
    QDUP_zero:
    nxt
    
    dcode 1+,2,,INCR ; ( a -- a+1 )
        pla
        inc a
        pha
    nxt
    
    dcode 1-,2,,DECR ; ( a -- a-1 )
        pla
        dec a
        pha
    nxt
    
    dcode 2+,2,,INCRTWO ; ( a -- a+2 )
        pla
        inc a
        inc a
        pha
    nxt
    
    dcode 2-,2,,DECRTWO ; ( a -- a-2 )
        pla
        dec a
        dec a
        pha
    nxt
    
    dcode +,1,,ADD ; ( a b -- a+b )
        clc
        pla
        clc
        adc $01, s
        ply
        pha
    nxt
    
    dcode -,1,,SUB ; ( a b -- a-b )
        lda $03, s
        sec
        sbc $01, s
        ply
        ply
        pha
    nxt
    
    dcode U*,2,,UMUL ; ( a b -- a*b )
        pla
        tsx
        sec
        mul $0001, x
        ply
        pha
    nxt
    
    dcode *,1,,MUL ; ( a b -- a*b )
        pla
        tsx
        clc
        mul $0001, x
        ply
        pha
    nxt
    
    dcode 2*,2,,TWOMUL ; ( a -- 2*a )
        pla
        clc
        rol a
        pha
    nxt
    
    dword U/MOD,5,,UDIVMOD ; ( a b -- a/b a%b )
        .wp SWAP
        .wp _DM
    .wp EXIT
    
    dword /MOD,4,,DIVMOD ; ( a b -- a/b a%b )
        .wp SWAP
        .wp _DMS
    .wp EXIT
    
    dcode (U/MOD),7,F_HIDDEN,_DM
        pla
        tsx
        sec
        div $0001, x
        ply
        pha
        phd
    nxt
    
    dcode (/MOD),6,F_HIDDEN,_DMS
        pla
        tsx
        clc
        div $0001, x
        ply
        pha
        phd
    nxt
    
    dword U/,2,,UDIV ; ( a b -- a/b )
        .wp UDIVMOD
        .wp DROP
    .wp EXIT
    
    dword /,1,,DIV ; ( a b -- a/b )
        .wp DIVMOD
        .wp DROP
    .wp EXIT
    
    dword MOD,3,, ; ( a b -- a%b )
        .wp UDIVMOD
        .wp NIP
    .wp EXIT
    
    dword 2/,2,,TWODIV ; ( a -- b/2 )
        .wp ONE
        .wp RSHIFT
    .wp EXIT
    
    dword =,1,,EQU ; ( a b -- a==b )
        .wp SUB
        .wp ZBRANCH
        .wp EQU_true
        .wp FALSE
        .wp EXIT
    EQU_true:
        .wp TRUE
    .wp EXIT
    
    dword <>,2,,NEQU ; ( a b -- a!=b )
        .wp EQU
        .wp INVERT
    .wp EXIT
    
    dword <,1,,LT ; ( a b -- a<b )
        .wp SUB
        .wp ZLT
    .wp EXIT
    
    
    dword >,1,,GT ; ( a b -- a>b )
        .wp SWAP
        .wp LT
    .wp EXIT
    
    dword <=,2,,LE ; ( a b -- a<=b )
        .wp GT
        .wp INVERT
    .wp EXIT
    
    dword >=,2,,GE ; ( a b -- a>=b )
        .wp LT
        .wp INVERT
    .wp EXIT
    
    dcode 0=,2,,ZEQU ; ( a -- a==0 )
        pla
        beq ZEQU_yes
        lda #_F_FALSE
        bra ZEQU_end
    ZEQU_yes:
        lda #_F_TRUE
    ZEQU_end:
        pha
    nxt
    
        dcode 0<>,3,,ZNEQU ; ( a -- a!=0 )
        pla
        bne ZNEQU_yes
        lda #_F_FALSE
        bra ZNEQU_end
    ZNEQU_yes:
        lda #_F_TRUE
    ZNEQU_end:
        pha
    nxt
    
    dcode 0<,2,,ZLT ; ( a -- a<0 )
        pla
        bmi ZLT_yes
        lda #_F_FALSE
        bra ZLT_end
    ZLT_yes:
        lda #_F_TRUE
    ZLT_end:
        pha
    nxt
    
    dword 0>,2,,ZGT ; ( a -- a>0 )
        .wp ZERO
        .wp GT
    .wp EXIT
    
    dword 0<=,2,,ZLE ; ( a -- a<=0 )
        .wp ZERO
        .wp LE
    .wp EXIT
    
    dword 0>=,2,,ZGE ; ( a -- a>=0 )
        .wp ZERO
        .wp GE
    .wp EXIT

    dword MIN,3,, ; ( a b -- {a|b} )
        .wp TWODUP ; a b a b
        .wp LT ; a b ?a<b
        .wp ZBRANCH
        .wp MIN_b
        .wp DROP
    .wp EXIT
    MIN_b:
        .wp NIP
    .wp EXIT

    dword MAX,3,, ; ( a b -- {a|b} )
        .wp TWODUP ; a b a b
        .wp LT ; a b ?a<b
        .wp ZBRANCH
        .wp MAX_b
        .wp NIP
    .wp EXIT
    MAX_b:
        .wp DROP
    .wp EXIT

    dcode AND,3,, ; ( a b -- a&b )
        pla
        and $01, s
        ply
        pha
    nxt
    
    dcode OR,2,, ; ( a b -- a|b )
        pla
        ora $01, s
        ply
        pha
    nxt
    
    dcode XOR,3,, ; ( a b -- a^b )
        pla
        eor $01, s
        ply
        pha
    nxt
    
    dcode INVERT,6,, ; ( a -- ~a )
        pla
        eor #$ffff
        pha
    nxt
    
    dword NEGATE,6,, ; ( a -- -a )
        .wp INVERT
        .wp INCR
    .wp EXIT
    
    dword ABS,3,, ; ( a -- |a| )
        .wp DUP
        .wp ZGE
        .wp ZBRANCH
        .wp ABS_noaction
        .wp NEGATE
    ABS_noaction:
    .wp EXIT
    
    dcode LSHIFT,6,, ; ( a b -- a<<b )
        ply ; get amount to rot left
        pla ; get number
    LSHIFT_loop:
        clc
        rol a
        dey
        bne LSHIFT_loop
        pha
    nxt
    
    dcode RSHIFT,6,, ; ( a b -- a>>b )
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
    
    dcode CELLS,5,,
        pla
        clc
        rol a
        pha
    nxt
    
    dcode !,1,,POKE ; ( value addr -- )
        lda $03, s
        ldy #$0000
        sta ($01, s), y
        ply
        ply
    nxt
    
    dcode @,1,,PEEK ; ( addr -- value )
        ldy #$0000
        lda ($01, s), y
        ply
        pha
    nxt
    
    dword ?,1,,PRTADDR ; ( addr -- )
        .wp PEEK
        .wp PRINT_NUM
    .wp EXIT
    
    dcode C!,2,,POKEBYTE ; ( value addr -- )
        lda $03, s
        ldy #$0000
        sep #$20
        sta ($01, s), y
        rep #$20
        ply
        ply
    nxt
    
    dword C@,2,,PEEKBYTE ; ( addr -- value )
        .wp PEEK
        .lit $FF
        .wp AND
    .wp EXIT
    
    dword C?,2,,PRTBADDR ; ( addr -- )
        .wp PEEKBYTE
        .wp PRINT_NUM
    .wp EXIT
    
    dvar STATE,5,,,
    dvar DP,2,,,here_pos
    
    dcode HERE,4,, ; ( -- dp@ )
        lda var_DP
        pha
    nxt
    
    dvar LATEST,6,,,name_marker
    dvar BASE,4,,,10
    
    dcode DECIMAL,7,,
        lda #$000a
        sta var_BASE
    nxt
    
    dcode HEX,3,,
        lda #$0010
        sta var_BASE
    nxt
    
    dcode BIN,3,,
        lda #$0002
        sta var_BASE
    nxt
    
    dcode >R,2,,TOR ; ( a -- ) R: ( -- a )
        pla
        rha
    nxt
    
    dcode R>,2,,FROMR ; ( -- a ) R: ( a -- )
        rla
        pha
    nxt
    
    dcode 2>R,3,,TWOTOR ; ( a b -- ) R: ( -- b a )
        pla
        rha
        pla
        rha
    nxt
    
    dcode 2R>,3,,TWOFROMR ; ( -- a b ) R: ( b a -- )
        rla
        pha
        rla
        pha
    nxt
    
    dcode R@,2,,RFETCH ; ( -- a ) R: ( a -- a )
        lda $01, r
        pha
    nxt
    
    dcode RDROP,5,,RDROP ; R: ( a -- )
        rla
    nxt
    
    dword (MEMTEST),9,F_HIDDEN,IMEMTEST
        .lit $FF
        .wp SWAP
        .wp TWODUP ; $FF addr $FF addr
        .wp POKEBYTE ; $FF addr
        .wp PEEKBYTE ; $FF value
        .wp EQU ; cond
    .wp EXIT

    dword MEMTEST,7,, ; ( addr -- writeable )
        .wp DUP
        .wp DUP ; addr addr addr
        .wp PEEKBYTE ; addr addr value
        .wp TOR ; addr addr
        .wp IMEMTEST ; addr cond
        .wp SWAP ; cond addr
        .wp FROMR ; cond addr value
        .wp SWAP ; cond value addr
        .wp POKEBYTE
    .wp EXIT

    dword FREE,4,, ; ( -- free-mem )
        .lit $1FFF
        
        .lit $2000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $3FFF
        
        .lit $4000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $5FFF
        
        .lit $6000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $7FFF
        
        .lit $8000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $9FFF
        
        .lit $a000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $BFFF
        
        .lit $c000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $DFFF
        
        .lit $e000
        .wp MEMTEST
        .wp ZBRANCH
        .wp FREE_end
        .wp DROP
        .lit $FFFF
        
    FREE_end:
        .wp HERE
        .wp SUB
    .wp EXIT
    
    dword COLD,4,,
        .wp DECIMAL
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
        
        .wp ABORT

    dword ABORT,5,,
        .lit 2
        .wp CURSOR
        .wp SPRST
        .wp QUIT
        
    dword ABORT",6,F_IMMED+F_COMPILEONLY,
        .wp PRTSTR
        .comp ABORT
    .wp EXIT

    dword QUIT,4,,
        .wp RPRST
        .wp ZERO
        .wp STATE
        .wp POKE
    QUIT_loop:
        .wp CR        
        .wp STATE
        .wp PEEK
        .wp ZBRANCH
        .wp QUIT_pis
        .wp BRANCH
        .wp QUIT_pcs
    QUIT_cont:
        
        .lit $80
        .wp DUP
        .wp DUP
        .wp ACCEPT ; read into address $80, max length $80
        .wp INTERPRET
        
        .lit QUIT_ok
        .lit 3
        .wp TYPE
        
        .wp BRANCH ; loop infinitely
        .wp QUIT_loop ; ($fffa = -6)
        
    QUIT_pis:
        .lit QUIT_prompta
        .lit 2
        .wp TYPE
        .wp BRANCH
        .wp QUIT_cont
    QUIT_pcs:
        .lit QUIT_promptb
        .lit 9
        .wp TYPE
        .wp BRANCH
        .wp QUIT_cont

    dconst TRUE,4,,,_F_TRUE
    dconst FALSE,5,,,_F_FALSE

    dcode BRANCH,6,F_COMPILEONLY,
        nxa
        tax
        txi
    nxt

    dcode 0BRANCH,7,F_COMPILEONLY,ZBRANCH
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
    
    dcode TICKS,5,,
        pla
    TICKS_loop:
        beq TICKS_end
        wai
        dec a
        bra TICKS_loop
    TICKS_end:
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
    
    dword +!,2,,ADDSTORE ; ( value addr -- )
        .wp DUP
        .wp PEEK
        .wp ROT
        .wp ADD
        .wp POKE
    .wp EXIT
    
    dword @1+!,4,,PEEKINCR ; ( addr -- value )
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
    
    dword ?COMPILE-ONLY,13,,ISCOMPONLY ; ( address -- cond )
        .wp INCRTWO
        .wp PEEK
        .lit F_COMPILEONLY
        .wp AND
        .wp ZNEQU
    .wp EXIT
    
    dword ?IMMEDIATE,10,,ISIMMEDIATE ; ( address -- cond )
        .wp INCRTWO
        .wp PEEK
        .lit F_IMMED
        .wp AND
        .wp ZNEQU
    .wp EXIT
    
    dword INTERPRET,9,, ; ( address length -- )
        .lit $20
        .wp PEEK
        .wp TOR
        .lit $22
        .wp PEEK
        .wp TOR
    
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
        
        .wp TWODUP ; w-a w-l w-a w-l
        
        .wp FIND ; w-a w-l addr
        .wp DUP ; w-a w-l addr addr
        .wp ZBRANCH ; w-a w-l addr
        .wp INTERPRET_wordnotfound
        
        .wp DUP ; w-a w-l addr addr
        .wp ISCOMPONLY ; w-a w-l addr cond
        .wp STATE
        .wp PEEK ; w-a w-l addr cond state
        .wp ZEQU
        .wp AND ; w-a w-l addr cond
        .wp INVERT
        .wp ZBRANCH ; w-a w-l addr
        .wp INTERPRET_throwic
        
        .wp NIP ; w-a addr
        .wp NIP ; addr
        
        .wp DUP
        .wp ISIMMEDIATE
        .wp STATE
        .wp PEEK
        .wp ZEQU
        .wp OR
        .wp ZBRANCH
        .wp INTERPRET_cword
        
        .wp TCFA ; addr
        .wp EXECUTE
        
    INTERPRET_cword_return:
        .wp BRANCH
        .wp INTERPRET_loop
        
    INTERPRET_wordnotfound:
        
        .wp DROP ; w-a w-l
        .wp TWODUP ; w-a w-l w-a w-l
        .wp NUMBER ; w-a w-l result upcc
        .wp ZEQU ; w-a w-l result cond
        .wp ZBRANCH ; w-a w-l result
        .wp INTERPRET_notanumber
        
        .wp NIP ; w-a result
        .wp NIP ; result
        
        .wp STATE
        .wp PEEK
        .wp ZBRANCH
        .wp INTERPRET_compilenum_ret
        .wp BRANCH
        .wp INTERPRET_compilenum
    INTERPRET_compilenum_ret:
        
        .wp BRANCH
        .wp INTERPRET_loop

    INTERPRET_notanumber:
        .wp DROP
        .lit INTERPRET_texta
        .lit 15
        .wp TYPE
        .wp TYPE
        .wp ABORT
        
    INTERPRET_end:
        .wp TWODROP
        
        .wp DEPTH
        .wp ZERO
        .wp LT
        .wp ZBRANCH
        .wp INTERPRET_checka
        .lit INTERPRET_textb
        .lit 11
        .wp TYPE
        .wp ABORT
        
    INTERPRET_checka:

        .wp DEPTH
        .lit 127
        .wp GT
        .wp ZBRANCH
        .wp INTERPRET_checkb
        .lit INTERPRET_textc
        .lit 14
        .wp TYPE
        .wp ABORT
        
    INTERPRET_checkb: ; normal exit
        .wp FROMR
        .lit $22
        .wp POKE
        .wp FROMR
        .lit $20
        .wp POKE
    .wp EXIT

    INTERPRET_cword:
        .wp TCFA
        .wp COMMA
        .wp BRANCH
        .wp INTERPRET_cword_return

    INTERPRET_compilenum:
        .comp LIT
        .wp COMMA
        .wp BRANCH
        .wp INTERPRET_compilenum_ret
        
    INTERPRET_throwic:
        .wp DROP
        .lit INTERPRET_textd
        .lit 32
        .wp TYPE
        .wp TYPE
        .wp ABORT
    
    dword (GCL),5,F_HIDDEN,GCL
        .lit $20
        .wp PEEKINCR
    .wp EXIT
    
    dword WORD,4,, ; ( -- word-address word-length )
        .wp BL
        .wp PARSE
    .wp EXIT
    
    dword PARSE,5,, ; ( delimiter-char -- word-address word-length )
        .lit $28
        .wp POKEBYTE
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
        
        .wp ZERO
        
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
        .lit $28
        .wp PEEKBYTE
        .wp EQU
        .wp ZBRANCH
        .wp WORD_noblank_b
        .wp EXIT
    WORD_noblank_b:
        
        .wp BRANCH
        .wp WORD_loop_b
        
    .wp EXIT
    
    dcode UNLOOP,6,, ; R: ( a b -- )
        rla
        rla
    nxt
    
    dword (GDL),5,F_HIDDEN,GDL
        .lit $24
        .wp PEEKINCR
    .wp EXIT
    
    dword KEY>DIGIT,9,,KEYTODIGIT ; ( char(a) -- a )
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
    
    dword DIGIT>KEY,9,,DIGITTOKEY ; ( a -- char(a) )
        .wp DUP
        .lit 9
        .wp GT
        .wp ZBRANCH
        .wp DK_nooffset
        .lit 7
        .wp ADD
    DK_nooffset:
        .lit $30
        .wp ADD
    .wp EXIT
    
    dword NUMBER,6,, ; ( word-address word-length -- result upcc )
        .wp ZERO
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
        .wp TOR
        .wp TOR
        .wp TOR
        .wp DECR
        .wp FROMR
        .wp FROMR
        .wp FROMR
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
        .wp ZBRANCH ; w-a base newv base digit
        .wp NUMBER_invalidchara
        .wp DUP ; w-a base newv base digit digit
        .wp NROT ; w-a base newv digit base digit
        .wp GT
        .wp ZBRANCH ; w-a base newv digit
        .wp NUMBER_invalidcharb
        
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
        .wp ZERO
        .wp ZERO
    .wp EXIT
    
    NUMBER_invalidchara:
        .wp NIP
    NUMBER_invalidcharb:
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
    
    dword >CFA,4,,TCFA
        .lit 2
        .wp ADD
        .wp DUP
        .wp PEEK
        .lit F_LENMASK
        .wp AND
        .wp ADD
        .wp INCR
    .wp EXIT
    
    dword >DFA,4,,TDFA
        .wp TCFA
        .lit 3
        .wp ADD
    .wp EXIT
    
    dword FIND,4,, ; ( word-address word-length -- address )
        .wp LATEST
        .wp PEEK ; w-a w-l addr
        
    FIND_loop:

        .wp DUP
        .wp ZNEQU
        .wp ZBRANCH
        .wp FIND_notfound
        
        .wp DUP ; w-a w-l addr addr
        .wp INCRTWO
        .wp PEEKBYTE ; w-a w-l addr flags
        .lit F_LENMASK
        .lit F_HIDDEN
        .wp OR ; w-a w-l addr flags mask
        .wp AND ; w-a w-l addr length
        .wp ROT ; w-a addr length w-l
        .wp DUP ; w-a addr length w-l w-l
        .wp ROT ; w-a addr w-l w-l length
        .wp EQU
        .wp ZBRANCH ; w-a addr w-l
        .wp FIND_notequal
        
        .wp TOR ; w-a addr
        .wp TWODUP ; w-a addr w-a addr
        .lit 3
        .wp ADD
        .wp STRCMP ; w-a addr len
        .wp FROMR ; w-a addr len w-l
        .wp DUP ; w-a addr len w-l w-l
        .wp ROT ; w-a addr w-l w-l len
        .wp LE
        .wp ZBRANCH ; w-a addr w-l
        .wp FIND_notequal
        
        ; Found
        
        .wp DROP ; w-a addr
        .wp NIP ; addr
    .wp EXIT
        
    FIND_notequal:
        .wp SWAP
        .wp PEEK
        .wp BRANCH
        .wp FIND_loop
    .wp EXIT
    
    FIND_notfound:
        .wp DROP ; w-a w-l
        .wp TWODROP ;
        .wp ZERO
    .wp EXIT
    
    dword ('),3,,_HTICK
        .wp WORD
        .wp TWODUP
        .wp FIND
        .wp DUP
        .wp ZBRANCH
        .wp TICK_notfound
        .wp NIP
        .wp NIP
    .wp EXIT
    
    dword ',1,,_TICK
        .wp _HTICK
        .wp TCFA
    .wp EXIT
    
    TICK_notfound:
        .wp DROP
        .lit INTERPRET_texta
        .lit 15
        .wp TYPE
        .wp TYPE
        .wp ABORT
    .wp EXIT
    
    dword STRCMP,6,, ; ( addr1 addr2 -- matching-length )
        .wp TWODUP
        .wp NEQU
        .wp ZBRANCH
        .wp STRCMP_equaddr
        
        .wp ZERO
        .wp TOR
        
    STRCMP_loop:
        
        .wp TWODUP ; addr1 addr2 addr1 addr2
        .wp RFETCH ; addr1 addr2 addr1 addr2 index
        .wp ADD ; addr1 addr2 addr1 loc2
        .wp PEEKBYTE ; addr1 addr2 addr1 value2
        .wp SWAP ; addr1 addr2 value2 addr1
        .wp RFETCH ; addr1 addr2 value2 addr1 index
        .wp ADD ; addr1 addr2 value2 loc1
        .wp PEEKBYTE ; addr1 addr2 value2 value1
        .wp EQU
        .wp ZBRANCH ; addr1 addr2
        .wp STRCMP_noequ
        .wp FROMR
        .wp INCR
        .wp TOR
        .wp BRANCH
        .wp STRCMP_loop
    
    STRCMP_noequ:
        .wp TWODROP
        .wp FROMR
    .wp EXIT
    
    STRCMP_equaddr:
        .wp TWODROP
        .lit $ffff
    .wp EXIT
    
    dcode SPLIT,5,, ; ( $1234 -- $34 $12 )
        pla
        sep #$30
        ldx #$00
        phx
        pha
        xba
        phx
        pha
        rep #$30
    nxt
    
    dcode JOIN,4,, ; ( $34 $12 -- $1234 )
        sep #$30
        plx
        ply
        ply
        pla
        phx
        phy
        rep #$30
    nxt
    
    dcode EXECUTE,7,,
        rts
    
    dword ALLOT,5,, ; ( length -- addr )
        .wp DUP
        .wp FREE
        .wp SWAP
        .wp SUB
        .wp ZGE
        .wp ZBRANCH
        .wp ALLOT_outofmemory
        
        .wp HERE
        .wp TUCK
        .wp ADD
        .wp DP
        .wp POKE
    .wp EXIT
    
    ALLOT_outofmemory:
        .wp DROP
        .lit ALLOT_str
        .lit 13
        .wp TYPE
    .wp ABORT ; reset execution

    dcode MEMCPY,6,, ; ( source target length -- )
        ply
        pla
        plx
        jsr memcpy
    nxt

    memcpy: ; (A: target X: source Y: length)
        stx memcpy_src
        sta memcpy_dest
        sep #$20
    memcpy_loop:
        cpy #$0000
        beq memcpy_end
        dey
        lda (memcpy_src), y
        sta (memcpy_dest), y
        bra memcpy_loop
    memcpy_end:
        rep #$20
    rts

    memcpy_src: db 0,0
    memcpy_dest: db 0,0

    dcode FILL,4,, ; ( byte target length -- )
            ply
            pla
            plx
            jsr fill
        nxt

        fill: ; (A: target X: fill Y: length)
            stx memcpy_src
            sta memcpy_dest
            sep #$20
        fill_loop:
            cpy #$0000
            beq fill_end
            dey
            lda memcpy_src
            sta (memcpy_dest), y
            bra fill_loop
        fill_end:
            rep #$20
        rts
    
    dword ERASE,5,, ; ( target length -- )
        .wp ZERO
        .wp NROT
        .wp FILL
    .wp EXIT
    
    dword HIDE,4,,
        .wp LATEST
        .wp PEEK
        .wp INCRTWO
        .wp DUP
        .wp PEEKBYTE
        .lit F_HIDDEN
        .wp OR
        .wp SWAP
        .wp POKEBYTE
    .wp EXIT
    
    dword REVEAL,6,,
        .wp LATEST
        .wp PEEK
        .wp INCRTWO
        .wp DUP
        .wp PEEKBYTE
        .lit F_HIDDEN
        .wp INVERT
        .wp AND
        .wp SWAP
        .wp POKEBYTE
    .wp EXIT
    
    dword IMMEDIATE,9,F_IMMED,
        .wp LATEST
        .wp PEEK
        .wp INCRTWO
        .wp DUP
        .wp PEEKBYTE
        .lit F_IMMED
        .wp OR
        .wp SWAP
        .wp POKEBYTE
    .wp EXIT
    
    dword COMPILE-ONLY,12,F_IMMED,COMPILEONLY
        .wp LATEST
        .wp PEEK
        .wp INCRTWO
        .wp DUP
        .wp PEEKBYTE
        .lit F_COMPILEONLY
        .wp OR
        .wp SWAP
        .wp POKEBYTE
    .wp EXIT
    
    dword HEADER,6,,
        .wp WORD ; w-a w-l
        .wp IHEADER
    .wp EXIT
    
    dword (HEADER),8,,IHEADER ; ( word-addr word-length -- )
        .lit F_LENMASK
        .wp AND
        .wp HERE ; w-a w-l here
        .wp LATEST
        .wp PEEK ; w-a w-l here latest
        .wp COMMA ; w-a w-l here
        .wp LATEST 
        .wp POKE ; w-a w-l
        .wp DUP ; w-a w-l w-l
        .lit F_HIDDEN
        .wp OR
        .wp CCOMMA ; w-a w-l
        .wp DUP ; w-a w-l w-l
        .wp ALLOT ; w-a w-l here
        .wp SWAP ; w-a here w-l
        .wp MEMCPY
    .wp EXIT
    
    dword \,,1,,COMMA ; ( a -- )
        .wp HERE
        .wp POKE
        .wp CELL
        .wp ALLOT
        .wp DROP
    .wp EXIT
    
    dword C\,,2,,CCOMMA ; ( a -- )
        .wp HERE
        .wp POKEBYTE
        .wp ONE
        .wp ALLOT
        .wp DROP
    .wp EXIT
    
    dword CREATE,6,,
        .wp HEADER
        .wp REVEAL
        .lit $22
        .wp CCOMMA
        .comp DOVAR
    .wp EXIT
    
    dword VARIABLE,8,,
        .wp CREATE
        .comp 0
    .wp EXIT
    
    dword CONSTANT,8,, ; ( a -- )
        .wp HEADER
        .wp REVEAL
        .compb $22
        .comp DOCON
        .wp COMMA
    .wp EXIT
    
    dword ],1,F_IMMED,RBRAC
        .wp ONE
        .wp STATE
        .wp POKE
    .wp EXIT
    
    dword [,1,F_IMMED,LBRAC
        .wp ZERO
        .wp STATE
        .wp POKE
    .wp EXIT

    dword \;,1,F_IMMED+F_COMPILEONLY,SEMICOLON
        .comp EXIT
        .wp REVEAL
        .wp LBRAC
    .wp EXIT

    dword \:,1,,COLON
        .wp HEADER
        .lit $22
        .wp CCOMMA
        .comp DOCOL
        .wp RBRAC
    .wp EXIT

    dword },1,F_IMMED+F_COMPILEONLY,END_QCOMP
        .comp EXIT
        .wp LBRAC
        .wp LATEST
        .wp PEEK
        .wp TCFA
        .wp EXECUTE
        .wp LATEST
        .wp PEEK
        .wp IFORGET
    .wp EXIT
    
    dword {,1,,BEGIN_QCOMP
        .wp ZERO
        .wp DUP
        .wp IHEADER
        .lit $22
        .wp CCOMMA
        .comp DOCOL
        .wp RBRAC
    .wp EXIT
    
    dword LITERAL,7,F_IMMED+F_COMPILEONLY,
        .comp LIT
        .wp COMMA
    .wp EXIT
    
    dword CHAR,4,,
        .wp WORD
        .wp DROP
        .wp PEEKBYTE
    .wp EXIT
    
    dword [COMPILE],9,F_IMMED+F_COMPILEONLY,COMPILEB
        .wp _HTICK
        .wp TCFA
        .wp COMMA
    .wp EXIT
    
    dword RECURSE,7,F_IMMED+F_COMPILEONLY,
        .wp LATEST
        .wp PEEK
        .wp TCFA
        .wp COMMA
    .wp EXIT
    
    dword IF,2,F_IMMED+F_COMPILEONLY,
        .comp ZBRANCH
        .wp HERE
        .comp 0
    .wp EXIT
    
    dword THEN,4,F_IMMED+F_COMPILEONLY,
        .wp HERE
        .wp SWAP
        .wp POKE
    .wp EXIT
    
    dword ELSE,4,F_IMMED+F_COMPILEONLY,
        .comp BRANCH
        .wp HERE
        .comp 0
        .wp SWAP
        .wp THEN
    .wp EXIT
    
    dword BEGIN,5,F_IMMED+F_COMPILEONLY,
        .wp HERE
    .wp EXIT
    
    dword UNTIL,5,F_IMMED+F_COMPILEONLY,
        .comp ZBRANCH
        .wp COMMA
    .wp EXIT
    
    dword AGAIN,5,F_IMMED+F_COMPILEONLY,
        .comp BRANCH
        .wp COMMA
    .wp EXIT
    
    dword UNLESS,6,F_IMMED+F_COMPILEONLY,
        .comp ZEQU
        .wp IF
    .wp EXIT
    
    dword WHILE,5,F_IMMED+F_COMPILEONLY,
        .comp ZBRANCH
        .wp HERE
        .comp 0
    .wp EXIT
    
    dword REPEAT,6,F_IMMED+F_COMPILEONLY,
        .comp BRANCH
        .wp SWAP
        .wp COMMA
        .wp HERE
        .wp SWAP
        .wp POKE
    .wp EXIT
    
    dword DO,2,F_IMMED+F_COMPILEONLY,
        .wp ZERO
        .wp HERE
        .comp TWOTOR
    .wp EXIT
    
    dword ?DO,3,F_IMMED+F_COMPILEONLY,QDO
        .comp TWODUP
        .comp NEQU
        .comp ZBRANCH
        .wp HERE
        .comp 0
        .wp HERE
        .comp TWOTOR
    .wp EXIT
    
    dword +LOOP,5,F_IMMED+F_COMPILEONLY,PLUSLOOP
        .comp TWOFROMR
        .comp ROT
        .comp ADD
        .comp TWODUP
        .comp EQU
        .comp ZBRANCH
        .wp COMMA
        .comp TWODROP
        .wp DUP
        .wp ZBRANCH
        .wp LOOP_noqdo
        .wp HERE
        .wp SWAP
        .wp POKE
    .wp EXIT
    LOOP_noqdo:
        .wp DROP
    .wp EXIT
    
    dword LOOP,4,F_IMMED+F_COMPILEONLY,
        .clt 1
        .wp PLUSLOOP
    .wp EXIT
    
    dword TIMES,5,,
        .wp _TICK
        .wp SWAP
    TIMES_loop:
        .wp DUP
        .wp ZBRANCH
        .wp TIMES_end
        .wp OVER
        .wp EXECUTE
        .wp DECR
        .wp BRANCH
        .wp TIMES_loop
    TIMES_end:
        .wp TWODROP
    .wp EXIT
    
    dword \\,1,F_IMMED,COMMENT
        .lit $FF
        .wp PARSE
        .wp TWODROP
    .wp EXIT
    
    dword (,1,F_IMMED,PAREN
        .lit $29
        .wp PARSE
        .wp TWODROP
    .wp EXIT
    
    dcode ("),3,F_COMPILEONLY,LITSTRING
        nxa
        tix
        phx
        pha
        phx
        clc
        adc $01, s
        plx
        tax
        txi
    nxt
    
    dword ",1,F_IMMED,PUTSTR
        .wp STATE
        .wp PEEK
        .wp ZBRANCH
        .wp PUTSTR_immed
        .comp LITSTRING
        .lit $22
        .wp PARSE ; addr len
        .wp DUP
        .wp COMMA
        .wp DUP ; addr len len
        .wp ALLOT ; addr len here
        .wp SWAP ; addr here len
        .wp MEMCPY
    .wp EXIT
    PUTSTR_immed:
        .wp HERE ; here
        .lit $22
        .wp PARSE ; here addr len
        .wp TUCK ; here len addr len
        .wp HERE ; here len addr len here
        .wp SWAP ; here len addr here len
        .wp MEMCPY ; here len
    .wp EXIT
    
    dword .",2,F_IMMED+F_COMPILEONLY,PRTSTR
        .wp PUTSTR
        .comp TYPE
    .wp EXIT
    
    dword .(,2,F_IMMED,PRGSTR
        .lit $29
        .wp PARSE
        .wp TYPE
    .wp EXIT
    
    dcode BYE,3,,
        stp
    nxt

    dvar TERMADDR,8,,,
    dvar DISKADDR,8,,,
    dvar IOXADDR,7,,,3

    dcode BA!,3,,BUS_SETADDR ; ( addr -- )
        pla
        mmu $00
    nxt
    
    dcode BA@,3,,BUS_GETADDR ; ( -- addr )
        mmu $80
        pha
    nxt

    dcode BW!,3,,BUS_SETWIN ; ( win -- )
        pla
        mmu $01
    nxt

    dcode BW@,3,,BUS_GETWIN ; ( win -- )
        mmu $81
        pha
    nxt
    
    dword BUS!,4,,BUS_POKE ; ( value addr -- )
        .wp BUS_GETWIN
        .wp ADD
        .wp POKE
    .wp EXIT
    
    dword BUS@,4,,BUS_PEEK ; ( addr -- value )
        .wp BUS_GETWIN
        .wp ADD
        .wp PEEK
    .wp EXIT
    
    dword BUSC!,5,,BUS_POKEBYTE ; ( value addr -- )
        .wp BUS_GETWIN
        .wp ADD
        .wp POKEBYTE
    .wp EXIT
    
    dword BUSC@,5,,BUS_PEEKBYTE ; ( addr -- value )
        .wp BUS_GETWIN
        .wp ADD
        .wp PEEKBYTE
    .wp EXIT

    dconst BL,2,,,_F_BL
    
    dword SPACE,5,,
        .wp BL
        .wp EMIT
    .wp EXIT

    dword SPACES,6,, ; ( n -- )
        .wp TOR
    SPACES_loop:
        .wp RFETCH
        .wp ZBRANCH
        .wp SPACES_end

        .wp SPACE

        .wp FROMR
        .wp DECR
        .wp TOR
        .wp BRANCH
        .wp SPACES_loop
    SPACES_end:
        .wp RDROP
    .wp EXIT
    
    dcode BINDTERM,8,F_HIDDEN,
        jsr bind_term
    nxt
    
    bind_term:
        lda var_TERMADDR
        mmu $00
    rts
    
    dcode EMIT,4,, ; ( char -- )
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
        sta $02
        stz $03
        pla ; get char
        sta ($0002), y
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
        lda $0002, x
        beq SCROLL_nocursor
        dec $0002, x
    SCROLL_nocursor:
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
        rep #$20
    nxt
    
    dword PAGE,4,,
        .wp BINDTERM
        
        .lit $20
        .lit $08
        .wp BUS_POKEBYTE
        .lit $00
        .lit $0A
        .wp BUS_POKEBYTE
        .lit $00
        .lit $0B
        .wp BUS_POKEBYTE
        .lit 80
        .lit $0C
        .wp BUS_POKEBYTE
        .lit 50
        .lit $0D
        .wp BUS_POKEBYTE
        .wp ONE
        .lit $07
        .wp BUS_POKEBYTE
        
        .wp ZERO
        .lit $01
        .wp BUS_POKE
        
        .wp TICK
    .wp EXIT
    
    dword TYPE,4,, ; ( address length -- )
        .wp DUP
        .wp ZBRANCH
        .wp TYPE_zerolength
        
        .wp ZERO
    TYPE_loop:
        .wp TWOTOR
        .wp DUP
        .wp I
        .wp ADD
        .wp PEEK
        .wp EMIT
        .wp TWOFROMR
        .wp INCR
        .wp TWODUP
        .wp EQU
        .wp ZBRANCH
        .wp TYPE_loop
        .wp DROP
    TYPE_zerolength:
        .wp TWODROP
    .wp EXIT
    
    dword XY@,3,,GETXY ; ( -- x y )
        .wp BINDTERM
        .wp ONE
        .wp BUS_PEEK
        .wp SPLIT
    .wp EXIT
    
    dword XY!,3,,SETXY ; ( x y -- )
        .wp JOIN
        .wp BINDTERM
        .wp ONE
        .wp BUS_POKE
    .wp EXIT
    
    dword CURSOR,6,, ; ( cursortype -- )
        .wp BINDTERM
        .lit 3
        .wp BUS_POKEBYTE
    .wp EXIT

    dcode KEY?,4,,KEYQ ; ( -- key/0 )
        mmu $81
        tax
        lda #$0000
        sep #$20
        lda $0004, x
        cmp $0005, x
        beq KEYQ_nokey
        lda $0006, x
        inc $0004, x
        rep #$20
        pha
    nxt

    KEYQ_nokey:
        rep #$20
        lda #$0000
        pha
    nxt

    dcode KEY,3,, ; ( -- key )
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

    dword ACCEPT,6,,ACCEPT ; ( c-addr maxlength -- read )
        .wp TOR
        .wp TOR
        
        .wp ZERO
        
    RL_loop:
        .wp KEY
        
        .wp DUP
        .lit _F_RETURN
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
        .lit _F_BACKSPACE
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

    dword N>S,3,,NUM_TOSTRING ; ( value -- length address )
        .wp DUP
        .wp TOR
        .wp BASE
        .wp PEEK
        .wp TOR

        .wp ZERO

    PN_loop:
        ; int counter
        .wp SWAP ; counter int
        .wp RFETCH ; counter int base
        .wp UDIVMOD ; counter result mod
        .wp DIGITTOKEY ; counter result char
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
    .wp EXIT

    dword U.R,3,,PRINT_UNUM_ALIGN ; ( value align -- )
        .wp SWAP ; align value
        .wp NUM_TOSTRING ; align address length
        .wp ROT ; address length align
        .wp OVER ; address length align length
        .wp SUB ; address length space-count
        .wp ZERO
        .wp MAX
        .wp SPACES ; address length
        .wp TYPE
    .wp EXIT
    
    dword U.,2,,PRINT_UNUM ; ( value -- )
        .wp ZERO
        .wp PRINT_UNUM_ALIGN
        .wp SPACE
    .wp EXIT

    dword .R,2,,PRINT_NUM_ALIGN ; ( value align -- )
        .wp SWAP ; align value
        .wp DUP ; align value value
        .wp ZLT ; align value cond
        .wp ZBRANCH ; align value
        .wp PN_notneg
        .lit $2d
        .wp EMIT
        .wp NEGATE
        .wp SWAP ; value align
        .wp DECR
        .wp SWAP ; align value
    PN_notneg:
        .wp SWAP
        .wp PRINT_UNUM_ALIGN
    .wp EXIT
    
    dword .,1,,PRINT_NUM ; ( value -- )
        .wp ZERO
        .wp PRINT_NUM_ALIGN
        .wp SPACE
    .wp EXIT
    
    dword .S,2,,PRINT_STACK
        .lit $3c
        .wp EMIT
        .wp DEPTH
        .wp ZERO
        .wp PRINT_UNUM_ALIGN
        .lit $3e
        .wp EMIT
        .wp SPACE
        
        .wp DEPTH
        .wp ZBRANCH
        .wp PS_nostack
        
        .wp SPORIG
        .wp DECR
        
        .wp DEPTH
        .wp DECR
        
    PS_loop:
        .wp TOR
        
        .wp DUP
        .wp PRTADDR
        .wp DECRTWO
        
        .wp FROMR
        .wp DECR
        .wp DUP
        .wp ZBRANCH
        .wp PS_end
        .wp BRANCH
        .wp PS_loop
    PS_end:
        .wp TWODROP
    PS_nostack:
    .wp EXIT
    
    dword (FORGET),8,,IFORGET ; ( word -- )
        .wp DUP
        .wp PEEK
        .wp LATEST
        .wp POKE
        .wp DP
        .wp POKE
    .wp EXIT
    
    dword FORGET,6,,
        .wp _HTICK
        .wp IFORGET
    .wp EXIT
    
    dword ['],3,F_IMMED+F_COMPILEONLY,CLIT
        .comp LIT
    .wp EXIT
    
    dword (DOES>),7,,NDOES
        ; replace ent DOVAR with jmp
        .wp LATEST
        .wp PEEK
        .wp TCFA
        .wp DUP
        .lit $20 ; jsr abs
        .wp SWAP
        .wp POKEBYTE
        .wp INCR
        .wp RFETCH
        .wp INCRTWO ; skip 'EXIT'
        .wp SWAP
        .wp POKE
    .wp EXIT

    dword DOES>,5,F_IMMED+F_COMPILEONLY,DOES
        .comp NDOES
        .comp EXIT
        .compb $22 ; ent WORD
        .comp DOCOL
    .wp EXIT
    
    dword POSTPONE,8,F_IMMED+F_COMPILEONLY,
        .wp _HTICK
        .wp DUP
        .wp ISIMMEDIATE
        .wp ZBRANCH
        .wp POSTPONE_notimmed
        .wp TCFA
        .wp COMMA
    .wp EXIT
    POSTPONE_notimmed:
        ; TODO, not implemented
        .lit not_implemented
        .lit 15
        .wp TYPE
        .wp ABORT
    .wp EXIT

    dword WORDS,5,,
        .wp LATEST
        .wp PEEK ; addr
    WORDS_loop:

        .wp DUP ; addr addr
        .wp ZNEQU ; addr cond
        .wp ZBRANCH ; addr
        .wp WORDS_end

        .wp DUP ; addr addr
        .wp INCRTWO ; addr flagaddr
        .wp PEEKBYTE ; addr flags
        .wp DUP ; addr flags flags
        .lit F_HIDDEN
        .wp AND ; addr flags hidden
        .wp ZEQU
        .wp ZBRANCH ; addr flags
        .wp WORDS_hidden
        .lit F_LENMASK
        .wp AND ; addr length
        .wp OVER ; addr length addr
        .lit 3
        .wp ADD ; addr length straddr
        .wp SWAP ; addr straddr length
        .wp TYPE
        .wp SPACE
    WORDS_hidden_return:
        .wp PEEK ; newaddr
        .wp BRANCH
        .wp WORDS_loop
    WORDS_end:
        .wp DROP
    .wp EXIT

    WORDS_hidden:
        .wp DROP ; addr
        .wp BRANCH
        .wp WORDS_hidden_return

    ; Redstone Port controls
    
    dword BINDIOX,7,F_HIDDEN,
        .wp IOXADDR
        .wp PEEK
        .wp BUS_SETADDR
    .wp EXIT

    dword IOX!,4,,IOXPOKE
        .wp BINDIOX
        .lit 2
        .wp BUS_POKE
    .wp EXIT
    
    dword IOXO@,5,F_HIDDEN,IOXOPEEK
        .wp BINDIOX
        .lit 2
        .wp BUS_PEEK
    .wp EXIT
    
    dword IOX@,4,,IOXPEEK
        .wp BINDIOX
        .wp ZERO
        .wp BUS_PEEK
    .wp EXIT
    
    dword IOXSET,6,,
        .wp IOXOPEEK
        .wp OR
        .wp IOXPOKE
    .wp EXIT
    
    dword IOXRST,6,,
        .wp NEGATE
        .wp IOXOPEEK
        .wp AND
        .wp IOXPOKE
    .wp EXIT
    
    ; Disk drive controls
    
    ; Disk commands:
    ; 1: Read disk name
    ; 2: Write disk name
    ; 3: Read disk serial
    ; 4: Read disk sector
    ; 5: Write disk sector
    ; 6: Clear sector buffer

    bind_disk:
        lda var_DISKADDR
        mmu $00
    rts

    dcode BINDDISK,8,F_HIDDEN,
        jsr bind_disk
    nxt
    
    BLKBUF_INT: db 0,0
    dword BLKBUF,6,,
        .lit BLKBUF_INT
        .wp PEEK
        .wp ZBRANCH
        .wp BLKBUF_allot
        .lit BLKBUF_INT
        .wp PEEK
    .wp EXIT
    BLKBUF_allot:
        .lit 1024
        .wp ALLOT
        .wp DUP
        .lit BLKBUF_INT
        .wp POKE
    .wp EXIT
    
    dvar BLKNO,5,,,
    dvar BLKINT,6,F_HIDDEN,,_F_TRUE
    
    dword DISKCMD,7,,
        .wp BINDDISK
        .lit $82
        .wp BUS_POKEBYTE
        .wp TICK
    .wp EXIT
    
    dword CLDRBUF,7,F_HIDDEN,
        .lit 6
        .wp DISKCMD
    .wp EXIT
    
    dword DISKNAME",9,,SET_DISKNAME
        .wp CLDRBUF
        .lit $22
        .wp PARSE ; addr len
        .wp TWODUP
        .wp BUS_GETWIN
        .wp SWAP
        .wp MEMCPY
        .lit 2
        .wp DISKCMD
    .wp EXIT
    
    dword BLOCK,5,,
        .wp BLKNO
        .wp POKE
        .wp REVERT
    .wp EXIT

    dword REVERT,6,, ; get 8 sectors (128 * 8 = 1024) into the buffer
            .wp BINDDISK
            .wp BLKNO
            .wp PEEK
            .lit 8
            .wp MUL ; origin

            .lit 0
    REVERT_loop:
            .wp TOR
            .wp RFETCH ; origin i
            .wp OVER ; origin i origin
            .wp ADD ; origin current-sector
            .lit $80
            .wp BUS_POKE ; origin
            .lit 4
            .wp DISKCMD

            .wp BUS_GETWIN ; origin c-origin
            .wp RFETCH
            .lit 128
            .wp UMUL
            .wp BLKBUF
            .wp ADD
            .lit 128 ; origin c-origin c-target c-length
            .wp MEMCPY ; origin

            .wp FROMR ; origin i
            .wp INCR ; origin i+1
            .wp DUP ; origin i i
            .lit 8
            .wp LE ; origin i cond
            .wp ZBRANCH ; origin i
            .wp REVERT_end
            .wp BRANCH
            .wp REVERT_loop
    REVERT_end:
            .wp TWODROP
    .wp EXIT

    dword FLUSH,5,,
        .wp BINDDISK
        .wp BLKNO
        .wp PEEK
        .lit 8
        .wp MUL ; origin

        .wp ZERO
    FLUSH_loop:
        .wp TOR
        .wp RFETCH ; origin i
        .wp OVER ; origin i origin
        .wp ADD ; origin current-sector
        .lit $80
        .wp BUS_POKE ; origin

        .wp BUS_GETWIN ; origin c-origin
        .wp RFETCH
        .lit 128
        .wp UMUL
        .wp BLKBUF
        .wp ADD
        .wp SWAP
        .lit 128 ; origin c-origin c-target c-length
        .wp MEMCPY ; origin

        .lit 5
        .wp DISKCMD

        .wp FROMR ; origin i
        .wp INCR ; origin i+1
        .wp DUP ; origin i i
        .lit 8
        .wp LT ; origin i cond
        .wp ZBRANCH ; origin i
        .wp FLUSH_end
        .wp BRANCH
        .wp FLUSH_loop
    FLUSH_end:
        .wp TWODROP
    .wp EXIT

    dword LIST,4,,
        .wp CR
        .lit 0
    LIST_loop:
        .wp TOR
        .wp RFETCH
        .lit 2
        .wp PRINT_UNUM_ALIGN
        .wp SPACE

        .wp BLKBUF
        .wp RFETCH
        .lit 64
        .wp MUL
        .wp ADD
        .lit 64
        .wp TYPE

        .wp CR

        .wp FROMR
        .wp INCR
        .wp DUP
        .lit 16
        .wp EQU
        .wp ZBRANCH
        .wp LIST_loop
        .wp DROP
    .wp EXIT

    dword WIPE,4,,
        .wp BL
        .wp BLKBUF
        .lit 1024
        .wp FILL
    .wp EXIT

    dconst BLKSIZE,7,F_HIDDEN,,128
    dword BLKCEIL,7,F_HIDDEN,
        .wp DECR
        .wp BLKSIZE
        .wp UDIV
        .wp INCR
        .wp BLKSIZE
        .wp UMUL
    .wp EXIT

    dword SAVE,4,,
        .wp BINDDISK

        .wp ZERO
    SAVE_loop:
        .wp TOR

        .wp RFETCH ; i
        .lit $80
        .wp BUS_POKE
        .lit $0500
        .wp RFETCH
        .lit $80
        .wp UMUL
        .wp ADD
        .wp BUS_GETWIN
        .lit $80
        .wp MEMCPY
        .lit 5
        .wp DISKCMD

        .wp FROMR ; i
        .wp INCR ; i+1
        .wp DUP ; i i
        .wp HERE
        .lit $0500
        .wp SUB
        .lit $80
        .wp UDIV ; i i max
        .wp LE ; i cond
        .wp ZBRANCH ; i
        .wp SAVE_end
        .wp BRANCH
        .wp SAVE_loop
    SAVE_end:
        .wp DROP
    .wp EXIT

    dword PP,2,,
        .wp BL ; ln bl
        .wp BLKBUF ; ln bl blkbuf
        .wp ROT ; bl blkbuf ln
        .lit 64
        .wp UMUL ; bl blkbuf offset
        .wp ADD ; bl pos
        .wp TUCK ; pos bl pos
        .lit 64
        .wp FILL ; pos
        .lit $FF
        .wp PARSE ; pos addr len
        .wp ROT ; addr len pos
        .wp SWAP ; addr pos len
        .lit 64
        .wp MIN
        .wp MEMCPY
    .wp EXIT

    dword LOAD,4,,
        .wp BLOCK
        .lit 0
    LOAD_loop:
        .wp TOR

        .wp RFETCH
        .lit 64
        .wp UMUL
        .wp BLKBUF
        .wp ADD
        .lit 64
        .wp INTERPRET

        .wp FROMR
        .wp INCR
        .wp DUP
        .lit 16
        .wp LT
        .wp ZBRANCH
        .wp LOAD_end
        .wp BRANCH
        .wp LOAD_loop
    LOAD_end:
        .wp DROP
    .wp EXIT

    dcode m,0,F_HIDDEN,marker
    here_pos:
    
section .text

    sep #$20
    lda $00
    sta var_DISKADDR
    lda $01
    sta var_TERMADDR
    rep #$20
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

    ; currently unused, here for when it's required

;   callf_do: ; subroutine to call forth words from native code, use the macro 'callf'
;       ply
;       rhy
;       sta callf_ptr
;       rhi
;       ldx callf_a
;       txi
;
;       ent
;   callf_ptr:
;       db $00, $00
;
;   callf_a:
;       .wp callf_b
;   callf_b:
;       rli
;       rli
;       rly
;       phy
;   rts
