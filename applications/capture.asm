        PAGE 0                  ; suppress page headings in AS listing file
;=============================================================================        
; Space Capture is a game of skill and chance. The object of the game is to capture
; an imaginary space ship by destroying all the possible sectors that it might attempt
; to travel in. The game as presented herein utilizes a game board consisting of a grid
; containing 64 squares or sectors. The sectors are identified by X and Y coordinates. 
;
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023
;
; jump to 0200H to start        
;=============================================================================                
        
        cpu 8008                ; use "old" 8008 mnemonics
        radix 8                 ; use octal for numbers

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
MSG:    LAM
        NDA
        RTZ
        CAL PRINT
        INL
        JFZ MSG
        INH
        JMP MSG

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
START:  LHI 000
        LLI 000
        CAL MSG
OVER:   LHI 000
        LLI 325
        CAL MSG
INAGN:  CAL CKINP
        NDA
;       CFS INPUTN
        CFS CINP2
        INL
        CPI 316
        JFZ NOTNO
        LHI 000
        LLI 350
        CAL MSG
        HLT

NOTNO:  CPI 331
        JFZ INAGN
        LAL
        NDI 007
        ADI 001
        LHI 001
        LLI 372
        LMA
        INL
        LMA
        LLI 377
        NDI 007
        RLC
        ORI 260
        LMA
        LHI 001
        LLI 376
        LMI 020
        LHI 003
        LLI 300
        LAI 377
FILOOP: LMA
        INL
        JFZ FILOOP
PLAYIN: LHI 000
        LLI 367
        CAL MSG
        LHI 001
        LLI 372
        LAM
        ORI 260
        CAL PRINT
        LHI 001
        LLI 026
        CAL MSG
        LHI 001
        LLI 373
        LAM
        ORI 260
        CAL PRINT
        CAL TRYMOV
        LHI 001
        LLI 376
        LBM
        DCB
        LMB
        JFZ CONTIN
PHASOR: LHI 001
        LLI 125
        CAL MSG
        JMP OVER
CONTIN: LHI 001
        LLI 036
        CAL MSG
INX:    CAL INPUTN
        CPI 261
        JTS INX
        CPI 271
        JFS INX
        LHI 001
        LLI 370
        NDI 017
        LMA
        LHI 001
        LLI 026
        CAL MSG
INY:    CAL CKINP
        NDA
;       CFS INPUTN
        CFS CINP2
        INL
        CPI 261
        JTS INY
        CPI 271
        JFS INY
        LBL
        LHI 001
        LLI 371
        NDI 017
        LMA
        LAB
        NDI 007
        RLC
        ORI 260
        LHI 001
        LLI 377
        LMA
HITEST: LHI 001
        LLI 370
        LAM
        INL
        INL
        CPM
        JFZ ZERSEC
        DCL
        LAM
        INL
        INL
        CPM
        JFZ ZERSEC
BOMB:   LHI 001
        LLI 072
        CAL MSG
        JMP OVER

ZERSEC: LLI 370
        LAM
        SUI 001
        RLC
        RLC
        RLC
        LDA
        INL
        LAM
        SUI 001
        ADD
        ORI 300
        LLA
        LHI 003
        LMI 000
        JMP PLAYIN

TRYMOV: LHI 001
        LLI 377
        LAM
        LCI 010
TRYSEC: LLA
        LBA
        LHI 003
        LAM
        LHI 001
        LLI 372
        ADM
        CPI 001
        JTS NOGDX
        CPI 011
        JFS NOGDX
        LHI 001
        LLI 374
        LMA
        INB
        LLB
        LHI 003
        LAM
        LHI 001
        LLI 373
        ADM
        CPI 001
        JTS NOGDY
        CPI 011
        JFS NOGDY
        LHI 001
        LLI 375
        LMA
CHECK:  DCL
        LAM
        SUI 001
        RLC
        RLC
        RLC
        LDA
        INL
        LAM
        SUI 001
        ADD
        ORI 300
        LLA
        LHI 003
        LAM
        NDA
        JFZ SAVPOS
        JMP NOGDY

NOGDX:  INB
NOGDY:  INB
        LAB
        NDI 017
        ORI 260
        DCC
        JFZ TRYSEC
        LHI 001
        LLI 201
        CAL MSG
        JMP OVER

SAVPOS: LHI 001
        LLI 374
        LDM
        INL
        LEM
        LLI 372
        LMD
        INL
        LME
        RET

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

            cpu 8008new             ; use "new" 8008 mnemonics

;-----------------------------------------------------------------------------------------
; The CKINP subroutine is a user provided routine that simply performs a check
; to see if the input device has a character waiting to be inputted. If so,
; the subroutine must return with the MSB of the accumulator set to '0.'
; If a character is not ready, the subroutine should return with the most
; significant bit of the accumulator set to a logic '1' state.
;-----------------------------------------------------------------------------------------
CKINP:      in 0                    ; get input from serial port
            rrc                     ; transfer the low-order bit to the high-order bit position
            ret


            ORG (4*400)+020
;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. 
; echo the character. return the character in A.
;-----------------------------------------------------------------------------------------
INPUTN:     in 0                    ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc INPUTN               ; jump if start bit detected

            ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit
CINP2:      mvi e,0                 ; initialize E
            mvi e,0                 ; timing adjustment
            xra a                   ; clear the accumulator
            out 08h                 ; send the start bit

            call getbitecho         ; bit 0
            call getbitecho         ; bit 1
            call getbitecho         ; bit 2
            call getbitecho         ; bit 3
            call getbitecho         ; bit 4
            call getbitecho         ; bit 5
            call getbitecho         ; bit 6
            call getbitecho         ; bit 7

            ; wait 104 cycles, then send the stop bit
            mov a,e                 ; save the character in E
            mvi e,0fch
            call delay
            mov e,a                 ; save the character in E
            mvi a,1
            out 08h                 ; send the stop bit
            ; wait 104 cycles.
            mov a,e                 ; restore the character from E to A
            ori 80h                 ; set the most significant bit
            mvi e,0fch
            mvi e,0fch              ; timing adjustment
            call delay
            ret

getbitecho: mov a,e                 ; save the bits in A
            mvi e,0ffh
            mvi e,0ffh               ; timing adjustment
            call delay
            mov e,a                 ; save the bits in E
            in 0                    ; get input from the serial port
            out 08h                 ; echo the received bit
            rar                     ; rotate the received bit right into carry
            mov a,e                 ; restore the previously received bits from E to A
            rar                     ; rotate the newly received bit in carry right into the MSB of A
            mov e,a                 ; save the received bits in E
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
PRINT:      ani 7fh                 ; clear the most signficant bit
            mov e,a                 ; save the character in E
            xra a                   ; clear A for the start bit

            out 08h                 ; send the start bit
            mov a,e                 ; restore the character to A
            mov a,e                 ; timing adjustment
            mvi e,0fch
            call delay

            call putbit             ; bit 0
            call putbit             ; bit 1
            call putbit             ; bit 2
            call putbit             ; bit 3
            call putbit             ; bit 4
            call putbit             ; bit 5
            call putbit             ; bit 6
            call putbit             ; bit 7

            ; send the stop bit
            mov e,a                 ; save the character in E
            mvi a,1                 ; stop bit
            out 08h                 ; send the stop bit
            mov a,e                 ; restore the character from E
            mvi e,0fch
            call delay
            ret

            ; 93 cycles
putbit:     out 08h                 ; output the least significant bit
            mvi e,0fdh
            call delay
            ana a                   ; timing adjustment
            rrc                     ; shift A right
            ret

            cpu 8008                ; use "old" 8008 mnemonics
            
            end 0200H
