        PAGE 0              ; suppress page headings in AS listing file
;=============================================================================        
; HANGMAN is a word game with which most readers are probably well acquainted.
; The object of the game is to determine what word a player is thinking of by 
; guessing the letters that make up the word. When characters contained in the
; word are correctly identified, the positions of the letters that have been 
; ascertained are disclosed. The goal of the game is to ascertain all the letters
; making up the concealed word with the least amount of incorrect guesses.
; In the computerized version of the game to be presented here, the computer will
; select a word from a list of words (which may be created by the reader if desired).
; The computer will then allow a player to enter guesses as to the letters contained
; in the word selected. Each time the player correctly identifies a letter contained
; in the word, the characters that have been ascertained will be displayed in their 
; proper location within the word. Each time a guess is incorrect, the computer will
; add a letter towards the spelling of Hangman! A game is finished when a player
; correctly identifies the word selected by the computer. Or, when eight incorrect letter
; guesses result in the complete spelling of Hangman! 
;
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023
;
; jump to 0200H to start        
;=============================================================================                

        CPU 8008
        RADIX 8

        ORG (2*400)+000        
START:  LHI 003
        LLI 350
        LMI 001
        LLI 356
        LMI 000
        INL
        LMI 005
NEWONE: LHI 004
        LLI 000
        CAL MSG
        CAL INPUT
        CPI 'N'
        JTZ NOMORE
        CPI 'Y'
        JFZ NEWONE
        CAL MOVTAB
        JMP GUESS
NOMORE: LHI 004
        LLI 025
        CAL MSG
        HLT

GUESS:  LHI 004
        LLI 037
        CAL MSG
        CAL INPUT
        LCA

SCAN:   LBI 000
        LHI 003
        LLI 360

CKMTCH: CPM
        JFZ NOMTCH
        INB
        LAI 010
        ADL
        LLA
        LMC
        LAL
        SUI 010
        LLA

NOMTCH: INL
        LAM
        NDA
        JTZ EOWORD
        LAI 007
        NDL
        JTZ EOWORD
        LAC
        JMP CKMTCH

EOWORD: INB
        DCB
        JTZ HANGIT
        LHI 004
        LLI 074
        CAL MSG
        LHI 003
        LLI 353
        LMI 000
        LLI 370

NOTEND: LAM
        CPI 255
        JFZ AHEAD2
        LEL
        LLI 353
        LBM
        INB
        LMB
        LLE

AHEAD2: CAL PRINT
        INL
        LAL
        SUI 010
        LLA
        LAM
        NDA
        JTZ ENDAGN
        LAL
        NDI 007
        JTZ ENDAGN
        LAI 010
        ADL
        LLA
        JMP NOTEND

ENDAGN: LLI 353
        LBM
        INB
        DCB
        JFZ GUESS
        LHI 004
        LLI 120
        CAL MSG
        JMP NEWONE

HANGIT: LHI 004
        LLI 062
        CAL MSG
        LHI 003
        LLI 350
        LBM
        INB
        LMB
        INL
        LMB
        LAI 010
        SUB
        INL
        LMA 
        LEI 340

HANGMR: LLE
        LAM
        CAL PRINT
        INL
        LEL
        LLI 351
        LBM
        DCB
        LMB
        JFZ HANGMR
        LLI 352
        LCM
        INC
        DCC
        JTZ NEWONE
        LAI 255

MRDASH: CAL PRINT
        LCM
        DCC
        LMC
        JFZ MRDASH
        JMP GUESS

MOVTAB: LHI 003
        LLI 350
        LBM
        LLI 370
        LCI 010
        LAI 255

DASHFL: LMA
        INL
        DCC
        JFZ DASHFL

NXWORD: LLI 360
        LCI 010
        XRA

ZEROFL: LMA
        INL
        DCC
        JFZ ZEROFL
        LLI 356
        LAM
        INL
        LHM
        LLA
        XRA
        CPM
        JFZ AHEAD3
        LHI 005
        LLI 000

AHEAD3: LDI 003
        LEI 360

BUFFMR: LAM
        NDA
        JTZ NEXT
        CAL SWITCH
        LMA
        INL
        CAL SWITCH
        INL
        JFZ NOHIGH
        INH

NOHIGH: JMP BUFFMR

NEXT:   INL
        JFZ NOTHI
        INH

NOTHI:  CAL SWITCH
        LLI 356
        LME
        INL 
        LMD
        DCB
        JFZ NXWORD
        LLI 350
        LMI 000
        RET

SWITCH: LCH
        LHD
        LDC
        LCL
        LLE
        LEC
        RET

MSG:        LAM
        NDA
        RTZ
        CAL PRINT
        INL
        JFZ MSG
        INH
        JMP MSG

        ORG (3*400)+340        
        DB      "HANGMAN!"
        DB      000,000,000,000
        
        ORG (3*400)+356        
        DB      000,000
        DB      000,000,000,000,000,000,000,000
        DB      255,255,255,255,255,255,255,255
        DB      215,212,212
        DB      "WANT A NEW WORD? "
        DB      000,215,212
        DB      "GOODBYE"
        DB      000,215,212
        DB   "GUESS A LETTER: "
        DB   000,215,212
        DB      "NOPE!  "
        DB   000,215,212
        DB      "GOOD. YOU HAVE:  "
        DB   000,215,212
        DB   "CONGRATULATIONS!"
        DB   000
        
        cpu 8008new
;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. 
; echos the character. return the character in A.
;-----------------------------------------------------------------------------------------
INPUT:  in 0                    ; get input from serial port
        rar                     ; rotate the received serial bit right into carry
        jc INPUT                ; jump if start bit not detected

        ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit
        mvi b,0                 ; initialize B
        mvi b,0                 ; tweak timing
        xra a                   ; clear the accumulator
        out 08h                 ; send the start bit
            
        call getbit             ; bit 0
        call getbit             ; bit 1
        call getbit             ; bit 2
        call getbit             ; bit 3
        call getbit             ; bit 4
        call getbit             ; bit 5
        call getbit             ; bit 6
        call getbit             ; bit 7
            
        ; wait 104 cycles, then send the stop bit
        mov a,b                 ; save the character (now in B) in A
        mvi b,0fch
        call delay
        mov b,a                 ; retrieve the character from A to B
        mvi a,1
        out 08h                 ; send the stop bit
        ; wait 104 cycles.
        mov a,b                 ; restore the character to A from B
        ;ori 80h                ; set the most significant bit            
        mvi b,0fch
        mvi b,0fch
        call delay
        ret
            
;------------------------------------------------------------------------        
; sends the character in A out from the serial port at 2400 bps.
;------------------------------------------------------------------------
PRINT:  ani 7fh                 ; clear the most signficant bit
        mov b,a                 ; save the character in B
        xra a                   ; clear A for the start bit

        out 08h                 ; send the start bit
        mov a,b                 ; restore the character to A 
        mov a,b                 ; timing adjustment
        mvi b,0fch
        call delay
            
        call outbit             ; bit 0
        call outbit             ; bit 1
        call outbit             ; bit 2
        call outbit             ; bit 3
        call outbit             ; bit 4
        call outbit             ; bit 5
        call outbit             ; bit 6
        call outbit             ; bit 7            

        ;send the stop bit
        mov b,a                 ; save the character in B
        mvi a,1                 ; stop bit
        out 08h                 ; send the stop bit 
        mov a,b                 ; restore the character from B to A
        mvi b,0fch
        call delay
        ret                     ; return to caller

outbit: out 08h                 ; output the least significant bit
        mvi b,0fdh
        call delay
        ana a                   ; timing adjustment
        rrc                     ; shift A right
        ret
            
delay:  inr b
        jnz delay
        ret                         
            
getbit: mov a,b                 ; save B in A
        mvi b,0ffh
        mvi b,0ffh
        call delay
        mov b,a                 ; restore B from A
        in 0                    ; get input from the serial port
        out 08h                 ; echo the received bit
        rar                     ; rotate the received bit right into carry
        mov a,b                 ; restore the previously received bits
        rar                     ; rotate the newly received bit in carry right into the MSB of A
        mov b,a                 ; save the received bits in B
        ret
        cpu 8008  
        
        ORG (5*400)+000
        
        DB      "HELLO",0
        DB      "ARROW",0
        DB      "COMPUTER",0
        DB      "PREMIUM",0
        DB      "NOTICE",0
        DB      "FUN",0
        DB      "HEAVY",0
        DB      "RUBBER",0
        DB      "RUSTLE",0
        DB      "THWART",0
        DB      "OYSTER",0
        DB      "OXIDIZE",0
        DB      "OSSIFY",0
        DB      "OPINION",0
        DB      "OOZY",0
        DB      "ONEROUS,0"
        DB      "NOMAD",0
        DB      "NOCTURNE,0"
        DB      "NOMINATE",0
        DB      "NUMSKULL",0
        DB      "DAFFODIL",0
        DB      "SIDEREAL",0
        DB      "CRICKET",0
        DB      "COURIER",0
        DB      "COSMOS",0
        DB      "CHEMIST",0
        DB      "CHEMICAL",0
        DB      "CHICORY",0
        DB      "CHLORINE",0
        DB      "CITIZEN",0
        DB      "CITRUS",0
        DB      "CLOSET",0
        DB      "COGENT",0
        DB      "BIRD",0
        DB      "BEETLE",0
        DB      "BELIEVE",0
        DB      "BATHTUB",0
        DB      "BASKET",0
        DB      "BANQUET",0
        DB      "BABBITT",0
        DB      "BACKBONE",0
        DB      "AUDIBLE",0
        DB      "ASPIRIN",0
        DB      "ASTEROID",0
        DB      "APPROVAL",0
        DB      "APOGEE",0
        DB      "ANNUITY",0
        DB      "ANODIZE",0
        DB      "ALUMINUM",0
        DB      "AIR",0
        DB      "AISLE",0
        DB      "ADJOIN",0
        DB      "ABYSS",0
        DB      "ABOLISH",0
        DB      "QUEUE",0
        DB      "QUIVER",0
        DB      "QUALM",0
        DB      "QUITE",0
        DB      "QUIXOTIC",0
        DB      "QUOIN",0
        DB      "QUOIT",0
        DB      "QUOTIENT,0"
        DB      "RADIO",0
        DB      "RAISIN",0
        DB      "RAPT",0
        DB      "RATIO",0
        DB      "RAUCOUS",0
        DB      "RAYON",0
        DB      "RAZOR",0
        DB      "REALM",0
        DB      "REEK",0
        DB      "REGISTER",0
        DB      "RIVET",0
        DB      "SCHOONER",0
        DB      "SAUNA",0
        DB      "SATIN",0
        DB      "SCEPTER",0
        DB      "SCIENCE",0
        DB      "SCRIBBLE",0
        DB      "BEHIND",0
        DB      "DIGNIFY",0
        DB      "ELLIPTIC",0
        DB      "ELOQUENT",0
        DB      "ELUSIVE",0
        DB      "FEATHER",0
        DB      "GALLOWS",0
        DB      "GARDEN",0
        DB      "GAZELLE",0
        DB      "MACABRE",0
        DB      "VALIANT",0
        DB      "VENISON",0
        DB      "VIVID",0
        DB      "WEIGHT",0
        DB      "WEIRD",0
        DB      "WISE",0
        DB      "ZERO",0
        DB      "ZOOLOGY",0
        DB      "ZENITH",0
        DB      "YAWN",0
        DB      "YOLK",0
        DB      "YELLOW",0
        DB      "YULE",0
        DB      "TRICKLE",0
        DB      "END",0
        DB      000
        
        end 0200H