    dvar BASE,4,,,10

    dcode DECIMAL,7,,
        lda #$000a
        sta var_BASE
    nxt

    dcode HEX,3,,
        lda #$0010
        sta var_BASE
    nxt

    .ifncflag min
        dcode BIN,3,,
            lda #$0002
            sta var_BASE
        nxt
    .endif

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

    .ifncflag min
        dword .(,2,F_IMMED,PRGSTR
            .lit $29
            .wp PARSE
            .wp TYPE
        .wp EXIT
    .endif

    dcode N>D,3,,NUM_TODOUBLE ; ( value -- d )
        pla
        sea
        pha
        phd
    nxt

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

        .ifncflag min
            .lit $3c
            .wp EMIT
            .wp DEPTH
            .wp ZERO
            .wp PRINT_UNUM_ALIGN
            .lit $3e
            .wp EMIT
            .wp SPACE
        .endif

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