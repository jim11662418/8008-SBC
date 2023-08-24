        PAGE 0                   ; suppress page headings in AS listing file 
;============================================================================= 
; Space Capture is a game of skill and chance. The object of the game is to capture 
; an imaginary space ship by destroying all the possible sectors that it might attempt 
; to travel in. The game as presented herein utilizes a game board consisting of a grid 
; containing 64 squares or sectors. The sectors are identified by X and Y coordinates. 
; 
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023 
;
; edited to use 'new' Intel 8008 mnemonics by Jim Loos 08/23/2023 
; 
; jump to 0200H to start 
; 
; serial I/O at 2400 bps N-8-1 
;============================================================================= 
 
            cpu 8008new          ; use 'new' Intel 8008 mnemonics
            radix 8              ; all numbers are octal 
 
        ORG (0*400)+000 
        db  215,212,212,323,320,301,303,305 
        db  323,310,311,320,240,303,301,320 
        db  324,325,322,305,256,240,331,317 
        db  325,240,310,301,326,305,240,261 
        db  265,240,320,310,301,323,317,322 
        db  215,212,323,310,317,324,323,240 
        db  327,311,324,310,240,327,310,311 
        db  303,310,240,324,317,240,304,305 
        db  323,324,322,317,331,240,315,331 
        db  240,324,322,301,326,305,314,215 
        db  212,323,305,303,324,317,322,323 
        db  256,240,311,306,240,301,314,314 
        db  240,315,331,240,301,304,312,301 
        db  303,305,316,324,240,323,305,303 
        db  324,317,322,323,215,212,301,322 
        db  305,240,304,305,323,324,322,317 
        db  331,305,304,240,311,240,301,315 
        db  240,303,301,320,324,325,322,305 
        db  304,256,240,311,306,240,331,317 
        db  325,215,212,310,311,324,240,315 
        db  305,240,317,322,240,322,325,316 
        db  240,317,325,324,240,317,306,240 
        db  320,310,301,323,317,322,240,305 
        db  316,305,322,307,331,254,215,212 
        db  324,310,305,316,240,331,317,325 
        db  240,314,317,323,305,241,215,212 
        db  212,000,000,000,000,215,212,212 
        db  327,301,316,324,240,324,317,240 
        db  320,314,301,331,277,240,240,000 
        db  215,212,212,320,317,317,322,240 
        db  323,320,317,322,324,241,000,215 
        db  212,212,315,331,240,314,301,323 
        db  324,240,320,317,323,311,324,311 
        db  317,316,240,327,301,323,272,240 
        db  240,330,240,275,240,000,254,240 
        db  240,331,240,275,240,000,215,212 
        db  212,331,317,325,240,301,322,305 
        db  240,306,311,322,311,316,307,240 
        db  324,317,272,240,240,330,240,275 
        db  240,000,215,212,212,331,317,325 
        db  240,310,311,324,240,315,305,241 
        db  241,240,240,331,317,325,240,314 
        db  317,323,305,241,000,215,212,212 
        db  331,317,325,240,301,322,305,240 
        db  317,325,324,240,317,306,240,320 
        db  310,301,323,317,322,240,305,316 
        db  305,322,307,331,254,240,240,331 
        db  317,325,240,314,317,323,305,241 
        db  000,215,212,212,243,241,260,243 
        db  207,207,207,240,240,304,301,322 
        db  316,241,240,240,331,317,325,240 
        db  310,301,326,305,240,315,305,240 
        db  240,303,240,301,240,320,240,324 
        db  240,325,240,322,240,305,240,304 
        db  240,241,241,000 
 
        ORG (1*400)+350 
MSG:    mov a,m 
        ana a 
        rz 
        call PRINT 
        inr l 
        jnz MSG 
        inr h 
        jmp MSG 
 
        ORG (1*400)+370 
        db  000 
        db  000 
        db  000 
        db  000 
        db  000 
        db  000 
        db  000 
        db  000 
 
        ORG (2*400)+000 
START:  mvi h,000 
        mvi l,000 
        call MSG 
OVER:   mvi h,000 
        mvi l,325 
        call MSG 
INAGN:  call CKINP 
        ana a 
;       cp INPUTN 
        cp CINP2 
        inr l 
        cpi 316 
        jnz NOTNO 
        mvi h,000 
        mvi l,350 
        call MSG 
        HLT 
 
NOTNO:  cpi 331 
        jnz INAGN 
        mov a,l 
        ani 007 
        adi 001 
        mvi h,001 
        mvi l,372 
        mov m,a 
        inr l 
        mov m,a 
        mvi l,377 
        ani 007 
        rlc 
        ori 260 
        mov m,a 
        mvi h,001 
        mvi l,376 
        mvi m,020 
        mvi h,003 
        mvi l,300 
        mvi a,377 
FILOOP: mov m,a 
        inr l 
        jnz FILOOP 
PLAYIN: mvi h,000 
        mvi l,367 
        call MSG 
        mvi h,001 
        mvi l,372 
        mov a,m 
        ori 260 
        call PRINT 
        mvi h,001 
        mvi l,026 
        call MSG 
        mvi h,001 
        mvi l,373 
        mov a,m 
        ori 260 
        call PRINT 
        call TRYMOV 
        mvi h,001 
        mvi l,376 
        mov b,m 
        dcr b 
        mov m,b 
        jnz CONTIN 
PHASOR: mvi h,001 
        mvi l,125 
        call MSG 
        jmp OVER 
CONTIN: mvi h,001 
        mvi l,036 
        call MSG 
INX:    call INPUTN 
        cpi 261 
        jm INX 
        cpi 271 
        jp INX 
        mvi h,001 
        mvi l,370 
        ani 017 
        mov m,a 
        mvi h,001 
        mvi l,026 
        call MSG 
INY:    call CKINP 
        ana a 
;       cp INPUTN 
        cp CINP2 
        inr l 
        cpi 261 
        jm INY 
        cpi 271 
        jp INY 
        mov b,l 
        mvi h,001 
        mvi l,371 
        ani 017 
        mov m,a 
        mov a,b 
        ani 007 
        rlc 
        ori 260 
        mvi h,001 
        mvi l,377 
        mov m,a 
HITEST: mvi h,001 
        mvi l,370 
        mov a,m 
        inr l 
        inr l 
        cmp m 
        jnz ZERSEC 
        dcr l 
        mov a,m 
        inr l 
        inr l 
        cmp m 
        jnz ZERSEC 
BOMB:   mvi h,001 
        mvi l,072 
        call MSG 
        jmp OVER 
 
ZERSEC: mvi l,370 
        mov a,m 
        sui 001 
        rlc 
        rlc 
        rlc 
        mov d,a 
        inr l 
        mov a,m 
        sui 001 
        add d 
        ori 300 
        mov l,a 
        mvi h,003 
        mvi m,000 
        jmp PLAYIN 
 
TRYMOV: mvi h,001 
        mvi l,377 
        mov a,m 
        mvi c,010 
TRYSEC: mov l,a 
        mov b,a 
        mvi h,003 
        mov a,m 
        mvi h,001 
        mvi l,372 
        add m 
        cpi 001 
        jm NOGDX 
        cpi 011 
        jp NOGDX 
        mvi h,001 
        mvi l,374 
        mov m,a 
        inr b 
        mov l,b 
        mvi h,003 
        mov a,m 
        mvi h,001 
        mvi l,373 
        add m 
        cpi 001 
        jm NOGDY 
        cpi 011 
        jp NOGDY 
        mvi h,001 
        mvi l,375 
        mov m,a 
CHECK:  dcr l 
        mov a,m 
        sui 001 
        rlc 
        rlc 
        rlc 
        mov d,a 
        inr l 
        mov a,m 
        sui 001 
        add d 
        ori 300 
        mov l,a 
        mvi h,003 
        mov a,m 
        ana a 
        jnz SAVPOS 
        jmp NOGDY 
 
NOGDX:  inr b 
NOGDY:  inr b 
        mov a,b 
        ani 017 
        ori 260 
        dcr c 
        jnz TRYSEC 
        mvi h,001 
        mvi l,201 
        call MSG 
        jmp OVER 
 
SAVPOS: mvi h,001 
        mvi l,374 
        mov d,m 
        inr l 
        mov e,m 
        mvi l,372 
        mov m,d 
        inr l 
        mov m,e 
        ret 
 
        ORG (3*400)+260 
        DB 377 
        DB 001 
        DB 000 
        DB 001 
        DB 001 
        DB 001 
        DB 377 
        DB 000 
        DB 001 
        DB 000 
        DB 377 
        DB 377 
        DB 000 
        DB 377 
        DB 001 
        DB 377 
 
            ORG (4*400)+000 
 
            cpu 8008new          ; use "new" 8008 mnemonics 
 
;----------------------------------------------------------------------------------------- 
; The CKINP subroutine is a user provided routine that simply performs a check 
; to see if the input device has a character waiting to be inputted. If so, 
; the subroutine must return with the MSB of the accumulator set to '0.' 
; If a character is not ready, the subroutine should return with the most 
; significant bit of the accumulator set to a logic '1' state. 
;----------------------------------------------------------------------------------------- 
CKINP:      in 0                 ; get input from serial port 
            rrc                  ; transfer the low-order bit to the high-order bit position 
            ret 
 
 
            ORG (4*400)+020 
;----------------------------------------------------------------------------------------- 
; wait for a character from the serial port. 
; echo the character. return the character in A. 
;----------------------------------------------------------------------------------------- 
INPUTN:     in 0                 ; get input from serial port 
            rar                  ; rotate the received serial bit right into carry 
            jc INPUTN            ; jump if start bit detected 
 
                                 ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit 
CINP2:      mvi e,0              ; initialize E 
            mvi e,0              ; timing adjustment 
            xra a                ; clear the accumulator 
            out 08h              ; send the start bit 
 
            call getbitecho      ; bit 0 
            call getbitecho      ; bit 1 
            call getbitecho      ; bit 2 
            call getbitecho      ; bit 3 
            call getbitecho      ; bit 4 
            call getbitecho      ; bit 5 
            call getbitecho      ; bit 6 
            call getbitecho      ; bit 7 
 
                                 ; wait 104 cycles, then send the stop bit 
            mov a,e              ; save the character in E 
            mvi e,0fch 
            call delay 
            mov e,a              ; save the character in E 
            mvi a,1 
            out 08h              ; send the stop bit 
                                 ; wait 104 cycles. 
            mov a,e              ; restore the character from E to A 
            ori 80h              ; set the most significant bit 
            mvi e,0fch 
            mvi e,0fch           ; timing adjustment 
            call delay 
            ret 
 
getbitecho: mov a,e              ; save the bits in A 
            mvi e,0ffh 
            mvi e,0ffh           ; timing adjustment 
            call delay 
            mov e,a              ; save the bits in E 
            in 0                 ; get input from the serial port 
            out 08h              ; echo the received bit 
            rar                  ; rotate the received bit right into carry 
            mov a,e              ; restore the previously received bits from E to A 
            rar                  ; rotate the newly received bit in carry right into the MSB of A 
            mov e,a              ; save the received bits in E 
            ret 
 
;------------------------------------------------------------------------ 
; delay in microseconds = (((255-value in E)*16)+19) * 4 microseconds 
;------------------------------------------------------------------------ 
delay:      inr e 
            jnz delay 
            ret 
 
 
            ORG (4*400)+200 
;------------------------------------------------------------------------ 
; sends the character in A out from the serial port at 2400 bps. 
;------------------------------------------------------------------------ 
PRINT:      ani 7fh              ; clear the most signficant bit 
            mov e,a              ; save the character in E 
            xra a                ; clear A for the start bit 
 
            out 08h              ; send the start bit 
            mov a,e              ; restore the character to A 
            mov a,e              ; timing adjustment 
            mvi e,0fch 
            call delay 
 
            call putbit          ; bit 0 
            call putbit          ; bit 1 
            call putbit          ; bit 2 
            call putbit          ; bit 3 
            call putbit          ; bit 4 
            call putbit          ; bit 5 
            call putbit          ; bit 6 
            call putbit          ; bit 7 
 
                                 ; send the stop bit 
            mov e,a              ; save the character in E 
            mvi a,1              ; stop bit 
            out 08h              ; send the stop bit 
            mov a,e              ; restore the character from E 
            mvi e,0fch 
            call delay 
            ret 
 
                                 ; 93 cycles 
putbit:     out 08h              ; output the least significant bit 
            mvi e,0fdh 
            call delay 
            ana a                ; timing adjustment 
            rrc                  ; shift A right 
            ret 
 
            cpu 8008new          ; use "old" 8008 mnemonics 
 
            end 0200H 
