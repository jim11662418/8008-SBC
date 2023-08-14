        PAGE 0              ; suppress page headings in AS listing file
;=============================================================================        
; Hexpawn consists of a 3 x 3 playing board and six pawns, three pawns for each player. 
; The pawns move in a manner similar to their moves in chess. A pawn can move one square
; forward, provided the square it is moving to is vacant, or one square diagonally to 
; capture an opponent's pawn. A diagonal move cannot be made if an opponent's pawn is not
; captured by the move. The object of the game is to move a pawn to the opponent's side
; of the board while blocking the opponent from doing so, or capture all of the opponent's
; pawns. A game is a draw when no one can make a legal move. 
;
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023
;
; jump to 0100H to start        
;=============================================================================                
        
        CPU 8008
        RADIX 8
        
CR      equ 0DH
LF      equ 0AH

        ORG 0100H
START:  LLI 076             ;set pointer to intro message
        LHI 005
        CAL MSG             ;print introduction
        LHI 004             ;set pointer to move index page
        LDI 000             ;initialize active move list pointer
        LEI 200             ;initialize restore move list pointer
RSTR:   LLE                 ;set restore list pointer
        LAM                 ;fetch restore list entry
        LLD                 ;set pointer to active list
        LMA                 ;store entry in active list
        IND                 ;increment active list pointer
        INE                 ;increment restore list pointer
        JFZ RSTR            ;done? no, continue transfer
AGAIN:  LLI 000             ;set pointer to current board
        LHI 003
        LMI 340             ;set board to starting setup
        INL
        LMI 007
PBD:    LLI 302             ;set pointer to board printout
        LBI 240             ;set space character to clear board
        LMB                 ;store space in '1' position
        LLI 304
        LMB                 ;store space in '2' position
        LLI 306
        LMB                 ;store space in '3' position
        LLI 311
        LMB                 ;store space in '4' position
        LLI 313
        LMB                 ;store space in '5' position
        LLI 315
        LMB                 ;store space in '6' position
        LLI 320
        LMB                 ;store space in '7' position
        LLI 322
        LMB                 ;store space in '8' position
        LLI 324
        LMB                 ;store space in '9' position
        LLI 000             ;set pointer to current 'X' board
        LAM                 ;fetch 'X' board
        INL                 ;advance to 'O' board
        LBM                 ;fetch current 'O' board
        LLI 302             ;set pointer to '1' position
        CAL STX             ;if 'X' here, store character
        LLI 304             ;set pointer to '2' position    
        CAL STX             ;if 'X' here, store character
        LLI 306             ;set pointer to '3' position
        CAL STX             ;if 'X' here, store character
        LLI 311             ;set pointer to '4' position
        CAL STX             ;if 'X' here, store character
        LCA                 ;save 'X' board
        LAB                 ;fetch 'O' board
        RLC                 ;position to '4'
        RLC
        CAL STO             ;if 'O' here, store character
        LBA                 ;save 'O' board
        LLI 313             ;set pointer to '5' position    
        LAC                 ;fetch 'X' board
        CAL STX             ;if 'X' here, store character
        LCA                 ;save 'X' board
        LAB                 ;fetch 'O' board
        CAL STO             ;if 'O' here, store character
        LBA                 ;save 'O' board
        LLI 315             ;set pointer to '6' position
        LAC                 ;fetch 'X' board
        CAL STX             ;if 'X' here, store character
        LAB                 ;fetch 'O' board
        CAL STO             ;if 'O' here, store character
        LLI 320             ;set pointer to '7' position
        CAL STO             ;if 'O' here, store character   
        LLI 322             ;set pointer to '8' position
        CAL STO             ;if 'O' here, store character
        LLI 324             ;set pointer to '9' position
        CAL STO             ;if 'O' here, store character
        LLI 300             ;set pointer to board printout
        CAL MSG             ;print current board
        LLI 002             ;set pointer to input storage
        CAL INPUT           ;input FROM move
        LMA                 ;save input
        CAL FNUM            ;number valid?
        JTS ERROR           ;no, error
        LAM                 ;fetch number
        NDI 017             ;delete ASCII code
        LMA                 ;save FROM location
        LBA                 ;save bit count for RTAL
        DCL                 ;set pointer to 'O' board
        LAM                 ;fetch 'O' board
        CAL RTAL            ;is pawn in FROM position?
        JFC ERROR           ;no, illegal move
        LLI 333             ;set pointer to TO message
        CAL MSG             ;print TO message
        LLI 003             ;set pointer to input storage
        CAL INPUT           ;input TO move
        LMA                 ;save TO input
        CAL FNUM            ;valid input?
        JTS ERROR           ;no, error
        LAM                 ;fetch number
        NDI 017             ;delete ASCII code
        LMA                 ;save TO location
        DCL                 ;set FROM pointer
        LAM                 ;fetch FROM
        SUI 003             ;is move forward?
        INL                 ;check against TO
        CPM
        JTZ BLK             ;yes, check if legal
        ADI 001             ;no, move right '1' square
        CPM                 ;is TO here
        JTZ CKCAP           ;yes, check for capture
        SUI 002             ;no, move left '1' square
        CPI 003             ;is move from '7' to '3'?
        JTZ ERROR           ;yes, illegal
        CPM                 ;is TO here
        JFZ ERROR           ;no, illegal move
CKCAP:  CPI 007             ;is move to '7'?
        JTZ ERROR           ;yes, error
        LBM                 ;fetch TO move
        LAI 200             ;set up calculate capture
        CAL RTLP            ;bit by rotating right
        LEA                 ;save capture bit
        LLI 000             ;set 'X' board pointer
        NDM                 ;capture?
        JTZ ERROR           ;no, illegal move
HMV:    LLI 002             ;set pointer to FM
        LAM                 ;fetch FROM position
        CAL RTAR            ;set up FROM bit
        LDA                 ;save FROM bit
        INL                 ;set pointer to TO
        LAM                 ;fetch TO location
        CPI 004             ;human wins?
        JTC HWIN            ;yes, zero last move
        CAL RTAR            ;set up TO bit
        LCA                 ;save TO bit
        LLI 001             ;set pointer to current 'O' board
        LAM                 ;fetch current board
        XRD                 ;clear old set
        ORC                 ;set new position
        LCA                 ;save new 'O' board
        LMA                 ;save current board
        DCL
        LAE                 ;fetch capture bit
        NDA                 ;capture?
        JTZ NOCP            ;no, skip
        XRM                 ;yes, delete piece
        LMA                 ;save current 'X' board
NOCP:   LDM                 ;save new 'X' board
        LLI 010             ;set pointer to model table
SMDL:   LAD                 ;fetch 'X' board
        CPM                 ;does 'X' moard match model
        JTZ OHLF            ;yes, try 'O' half
        INL                 ;advance table pointer
SMD1:   INL
        LAL                 ;check for end of table
        CPI 112             ;end of table
        JFZ SMDL            ;no, continue search
        LLI 340             ;no match, illegal move made
        LHI 004             ;print "NO GOOD!"
CMSG:   CAL MSG             ;print message
        JMP AGAIN           ;start new game
        
OHLF:   INL                 ;advance pointer to 'O' board
        LAC                 ;fetch current 'O' board
        CPM                 ;O boards match?
        JFZ SMD1            ;no, continue search

        DCL                 ;move pointer to 'X' board
        LAL                 ;set up to calculate pointer
        RRC                 ;divide by 2
        ADI 106             ;add to start of model index table
        LLA                 ;set pointer to model index table
        LLM                 ;fetch pointer to move index table
        LHI 004             ;set pointer to move index table
MFD1:   LAM                 ;fetch move number
        NDA                 ;move number here
        JTS ONO             ;no move available, human wins
        JFZ MOVE            ;move found, make it
        INL                 ;move zeroed, try next location
        JMP MFD1

MOVE:   LEL                 ;save move location
        LLI 004             ;set pointer to last move
        LHI 003
        LME                 ;save location as last move
        RLC                 ;set up pointer to move
        RLC                 ; storage table
        ADI 174
        LLA                 ;set pointer
        LDM                 ;fetch FROM bit
        INL                 ;advance pointer
        LCM                 ;fetch TO bit
        INL                 ;advance pointer
        LEM                 ;fetch capture bit
        INL                 ;advance pointer
        LAM                 ;fetch contest bit
        NDA                 ;is gave over?
        JTS WIN             ;yes, computer wins
        JFZ DRAW            ;yes, draw
        LLI 000             ;set pointer to 'X' board
        LAM                 ;fetch current 'X' board
        XRD                 ;clear old position
        ORC                 ;set new position
        LMA                 ;save new 'X' board
        INL                 ;advance pointer to 'O' board
        LAE                 ;fetch capture bit
        NDA                 ;capture?
        JTZ PBD             ;no, print new board
        XRM                 ;yes, delete piece
        LMA                 ;save new 'O' board
        JFZ PBD             ;non zero, continue game
        
        LLI 004             ;set pointer to last move
        LLM                 ;fetch last move location
        LHI 004             ;set pointer to active move list
        LMI 000             ;cancel last move
        LHI 005             ;set pointer to message "I WIN
        LLI 330             ;YOU NAVE NO PAWNS!"
        CAL CMSG            ;print message and start again

STX:    RLC                 ;bit set?
        RFC                 ;no, return
        LMI 330             ;yes, put 'X' in board
        RET
        
STO:    RLC                 ;bit set?
        RFC                 ;no, return
        LMI 317             ;yes, put 'O' in board
        RET
        
MSG:    LAM                 ;fetch character to print
        NDA                 ;end of message?
        RTZ                 ;yes, return
        CAL PRINT           ;no, print character
        INL                 ;increment low address msg pointer
        JFZ MSG             ;if not zero, continue output
        INH                 ;else, increment page address pointer
        JMP MSG
        
ERROR:  LLI 364             ;set pointer to error message
        LHI 005
        CAL MSG             ;print error message
        LHI 003
        JMP PBD             ;print current board
        
RTAL:   DCB                 ;decrement bit count
        RTZ                 ;if zero, return
        RLC                 ;else, rotate left
        JMP RTAL
        
RTAR:   LBA                 ;set bit count
        LAI 001             ;set bit to rotate
RTLP:   DCB                 ;decrement bit count
        RTZ                 ;if zero, return
        RRC                 ;else rotate right
        JMP RTLP
        
BLK:    LBM                 ;fetch TO move
        LLI 000             ;set pointer to 'X' board
        LAM                 ;fetch 'X' board
        CAL RTAL            ;check for blocked move
SET:    NDA                 ;is move blocked?
        JTS ERROR           ;yes, illegal move
        LEI 000             ;set for no capture
        JMP HMV             ;return to make human move
        
WIN:    LLI 025             ;set pointer to store win move
        LHI 005
        LMA                 ;store win move in message
        LLI 011             ;print "I MOVE TO "
        JMP CMSG            ;"I WIN, YOU LOSE"
        
DRAW:   LLI 052             ;print "DRAW, NO ONE WINS"
        LHI 005
        JMP CMSG            ;print draw message
        
FNUM:   LAM                 ;fetch ASCII number   
        CPI 261             ;is number valid?
        RTS                 ;no, return with S flag set
        SUI 272             ;if number is valid, return
        ADI 200             ;with S flag set
        RET
        
ONO:    LLI 374             ;print "I CONCEDE!"
        LHI 004
        CAL MSG             ;then zero last move
        
HWIN:   LLI 004             ;set pointer to last move
        LHI 003
        LLM                 ;fetch last move address
        LHI 004
        LMI 000             ;zero last move
        LLI 315             ;set pointer to lose message
        LHI 005
        CAL MSG             ;print lose message
        JMP AGAIN           ;start new game

        ORG 0300H
;           TEMPORARY DATA
        DB 000,000,000,000,000
        
        ORG 0308H
;           MODEL TABLE
        DB 340,043,340,016,340,025,260,021
        DB 150,041,240,062,300,051,150,014
        DB 160,034,260,012,304,061,140,054
        DB 140,021,140,024,240,041,070,010
        DB 200,070,120,030,104,060,230,010
        DB 240,014,210,042,054,040,060,020
        DB 110,040,110,010,220,020,044,020
        DB 200,060,240,032,100,020,250,052
        DB 040,070
        
        ORG 034AH
;           MOVE-TO-MOVE INDEX TABLE
        DB         000,004,010,013,017,023
        DB 027,033,036,041,043,046,051,054
        DB 057,061,063,065,070,073,075,077
        DB 101,103,107,112,115,120,123,125
        DB 131,133,135
        
        ORG 0380H        
;           MOVE TABLE
        DB 200,020,000,000,200,010,020,000
        DB 100,020,040,000,100,010,000,000
        DB 100,004,010,000,040,010,020,000
        DB 040,004,000,000,020,000,000,267
        DB 020,000,000,270,010,000,000,267
        DB 010,000,000,270,010,000,000,271
        DB 004,000,000,270,004,000,000,271
        DB 000,000,000,100
        
        ORG 03C0H        
;           BOARD OUTPUT MESSAGE
        DB CR,LF,"X|X|X"
        DB CR,LF," | | "
        DB CR,LF,"O|O|O"
        DB CR,LF,"FM ",0
        DB " TO ",0

        ORG 0400H
;           MOVE INDEX TABLE
        DB 007,004,003,200,001,004,005,200
        DB 001,002,200,007,006,010,200,007
        DB 003,013,200,007,002,006,200,003
        DB 004,017,200,005,012,200,005,006
        DB 200,010,200,002,003,200,005,017
        DB 200,006,017,200,006,007,200,017
        DB 200,010,200,002,200,005,010,200
        DB 003,016,200,013,200,017,200,017
        DB 200,016,200,007,006,010,200,003
        DB 013,200,005,013,200,002,010,200
        DB 006,016,200,002,200,001,006,017
        DB 200,017,200,017,200,006,200
        
        ORG 0480H
;       RESTORE LIST
        DB 007,004,003,200,001,004,005,200
        DB 001,002,200,007,006,010,200,007
        DB 003,013,200,007,002,006,200,003
        DB 004,017,200,005,012,200,005,006
        DB 200,010,200,002,003,200,005,017
        DB 200,006,017,200,006,007,200,017
        DB 200,010,200,002,200,005,010,200
        DB 003,016,200,013,200,017,200,017
        DB 200,016,200,007,006,010,200,003
        DB 013,200,005,013,200,002,010,200
        DB 006,016,200,002,200,001,006,017
        DB 200,017,200,017,200,006,200
        
        ORG 04E0H       
;       MESSAGE STORAGE
        DB CR,LF,"NO GOOD! MUST START AGAIN",0
        DB CR,LF,"I CONCEDE!",0
        DB CR,LF,"I MOVE TO  ,"
        DB CR,LF,"I WIN! YOU LOSE!",0
        DB CR,LF,"DRAW,NO ONE WINS.",0
        
MSG1:   DB CR,LF,"HERE\'S THE BOARD"
        DB CR,LF,"1|2|3"
        DB CR,LF,"4|5|6"
        DB CR,LF,"7|8|9"
        DB CR,LF,"I\'M X, YOU\'RE O"
        DB CR,LF,"MOVE ONE SQUARE FORWARD IF VACANT"
        DB CR,LF,"OR ONE SQUARE DIAGONALLY TO CAPTURE"
        DB CR,LF,"YOU START.",CR,LF,0
        DB CR,LF,"YOU WIN.",0
        DB CR,LF,"I WIN! YOU HAVE NO PAWNS.",0
        DB CR,LF,"NO! NO!",0
   
        ORG 0600H   
        
        cpu 8008new
;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. 
; echo each bit as it's received. returns the character in A.
;-----------------------------------------------------------------------------------------
INPUT:  in 0                    ; get input from serial port
        rar                     ; rotate the received serial bit right into carry
        jc INPUT                ; jump if start bit not detected

        ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit
        mvi b,0                 ; initialize B
        mvi b,0                 ; tweak timing
        xra a                   ; clear the accumulator
        out 08h                 ; send the start bit
            
        call getbit             ; get bit 0 and echo it
        call getbit             ; get bit 1 and echo it
        call getbit             ; get bit 2 and echo it
        call getbit             ; get bit 3 and echo it
        call getbit             ; get bit 4 and echo it
        call getbit             ; get bit 5 and echo it
        call getbit             ; get bit 6 and echo it
        call getbit             ; get bit 7 and echo it
            
        ; wait 104 cycles, then send the stop bit
        mov a,b                 ; save the character (now in B) in A
        mvi b,0fch
        call delay
        mov b,a                 ; retrieve the character from A to B
        mvi a,1
        out 08h                 ; send the stop bit
        ; wait 104 cycles.
        mov a,b                 ; restore the character to A from B
        ori 80h                 ; set the most significant bit            
        mvi b,0fch
        mvi b,0fch
        call delay
        ret                     ; return to caller with the character in A
            
        ORG 0640H 
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

        cpu 8008                ; return to using "old" mneumonics
        
        END 0100H
        