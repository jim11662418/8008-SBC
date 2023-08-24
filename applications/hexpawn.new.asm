        PAGE 0                   ; suppress page headings in AS listing file 
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
; edited to use 'new' Intel 8008 mnemonics by Jim Loos 08/23/2023 
;
; serial I/O at 2400 bps N-8-1 
; 
; jump to 0100H to start 
;============================================================================= 
 
        cpu 8008new              ; use 'new' Intel 8008 mnemonics
        RADIX 8                  ; all numbers are octal
 
CR      equ 0DH 
LF      equ 0AH 
 
        ORG 0100H 
START:  mvi l,076                ;set pointer to intro message 
        mvi h,005 
        call MSG                 ;print introduction 
        mvi h,004                ;set pointer to move index page 
        mvi d,000                ;initialize active move list pointer 
        mvi e,200                ;initialize restore move list pointer 
RSTR:   mov l,e                  ;set restore list pointer 
        mov a,m                  ;fetch restore list entry 
        mov l,d                  ;set pointer to active list 
        mov m,a                  ;store entry in active list 
        inr d                    ;increment active list pointer 
        inr e                    ;increment restore list pointer 
        jnz RSTR                 ;done? no, continue transfer 
AGAIN:  mvi l,000                ;set pointer to current board 
        mvi h,003 
        mvi m,340                ;set board to starting setup 
        inr l 
        mvi m,007 
PBD:    mvi l,302                ;set pointer to board printout 
        mvi b,240                ;set space character to clear board 
        mov m,b                  ;store space in '1' position 
        mvi l,304 
        mov m,b                  ;store space in '2' position 
        mvi l,306 
        mov m,b                  ;store space in '3' position 
        mvi l,311 
        mov m,b                  ;store space in '4' position 
        mvi l,313 
        mov m,b                  ;store space in '5' position 
        mvi l,315 
        mov m,b                  ;store space in '6' position 
        mvi l,320 
        mov m,b                  ;store space in '7' position 
        mvi l,322 
        mov m,b                  ;store space in '8' position 
        mvi l,324 
        mov m,b                  ;store space in '9' position 
        mvi l,000                ;set pointer to current 'X' board 
        mov a,m                  ;fetch 'X' board 
        inr l                    ;advance to 'O' board 
        mov b,m                  ;fetch current 'O' board 
        mvi l,302                ;set pointer to '1' position 
        call STX                 ;if 'X' here, store character 
        mvi l,304                ;set pointer to '2' position 
        call STX                 ;if 'X' here, store character 
        mvi l,306                ;set pointer to '3' position 
        call STX                 ;if 'X' here, store character 
        mvi l,311                ;set pointer to '4' position 
        call STX                 ;if 'X' here, store character 
        mov c,a                  ;save 'X' board 
        mov a,b                  ;fetch 'O' board 
        rlc                      ;position to '4' 
        rlc 
        call STO                 ;if 'O' here, store character 
        mov b,a                  ;save 'O' board 
        mvi l,313                ;set pointer to '5' position 
        mov a,c                  ;fetch 'X' board 
        call STX                 ;if 'X' here, store character 
        mov c,a                  ;save 'X' board 
        mov a,b                  ;fetch 'O' board 
        call STO                 ;if 'O' here, store character 
        mov b,a                  ;save 'O' board 
        mvi l,315                ;set pointer to '6' position 
        mov a,c                  ;fetch 'X' board 
        call STX                 ;if 'X' here, store character 
        mov a,b                  ;fetch 'O' board 
        call STO                 ;if 'O' here, store character 
        mvi l,320                ;set pointer to '7' position 
        call STO                 ;if 'O' here, store character 
        mvi l,322                ;set pointer to '8' position 
        call STO                 ;if 'O' here, store character 
        mvi l,324                ;set pointer to '9' position 
        call STO                 ;if 'O' here, store character 
        mvi l,300                ;set pointer to board printout 
        call MSG                 ;print current board 
        mvi l,002                ;set pointer to input storage 
        call INPUT               ;input FROM move 
        mov m,a                  ;save input 
        call FNUM                ;number valid? 
        jm ERROR                 ;no, error 
        mov a,m                  ;fetch number 
        ani 017                  ;delete ASCII code 
        mov m,a                  ;save FROM location 
        mov b,a                  ;save bit count for RTAL 
        dcr l                    ;set pointer to 'O' board 
        mov a,m                  ;fetch 'O' board 
        call RTAL                ;is pawn in FROM position? 
        jnc ERROR                ;no, illegal move 
        mvi l,333                ;set pointer to TO message 
        call MSG                 ;print TO message 
        mvi l,003                ;set pointer to input storage 
        call INPUT               ;input TO move 
        mov m,a                  ;save TO input 
        call FNUM                ;valid input? 
        jm ERROR                 ;no, error 
        mov a,m                  ;fetch number 
        ani 017                  ;delete ASCII code 
        mov m,a                  ;save TO location 
        dcr l                    ;set FROM pointer 
        mov a,m                  ;fetch FROM 
        sui 003                  ;is move forward? 
        inr l                    ;check against TO 
        cmp m 
        jz BLK                   ;yes, check if legal 
        adi 001                  ;no, move right '1' square 
        cmp m                    ;is TO here 
        jz CKCAP                 ;yes, check for capture 
        sui 002                  ;no, move left '1' square 
        cpi 003                  ;is move from '7' to '3'? 
        jz ERROR                 ;yes, illegal 
        cmp m                    ;is TO here 
        jnz ERROR                ;no, illegal move 
CKCAP:  cpi 007                  ;is move to '7'? 
        jz ERROR                 ;yes, error 
        mov b,m                  ;fetch TO move 
        mvi a,200                ;set up calculate capture 
        call RTLP                ;bit by rotating right 
        mov e,a                  ;save capture bit 
        mvi l,000                ;set 'X' board pointer 
        ana m                    ;capture? 
        jz ERROR                 ;no, illegal move 
HMV:    mvi l,002                ;set pointer to FM 
        mov a,m                  ;fetch FROM position 
        call RTAR                ;set up FROM bit 
        mov d,a                  ;save FROM bit 
        inr l                    ;set pointer to TO 
        mov a,m                  ;fetch TO location 
        cpi 004                  ;human wins? 
        jc HWIN                  ;yes, zero last move 
        call RTAR                ;set up TO bit 
        mov c,a                  ;save TO bit 
        mvi l,001                ;set pointer to current 'O' board 
        mov a,m                  ;fetch current board 
        xra d                    ;clear old set 
        ora c                    ;set new position 
        mov c,a                  ;save new 'O' board 
        mov m,a                  ;save current board 
        dcr l 
        mov a,e                  ;fetch capture bit 
        ana a                    ;capture? 
        jz NOCP                  ;no, skip 
        xra m                    ;yes, delete piece 
        mov m,a                  ;save current 'X' board 
NOCP:   mov d,m                  ;save new 'X' board 
        mvi l,010                ;set pointer to model table 
SMDL:   mov a,d                  ;fetch 'X' board 
        cmp m                    ;does 'X' moard match model 
        jz OHLF                  ;yes, try 'O' half 
        inr l                    ;advance table pointer 
SMD1:   inr l 
        mov a,l                  ;check for end of table 
        cpi 112                  ;end of table 
        jnz SMDL                 ;no, continue search 
        mvi l,340                ;no match, illegal move made 
        mvi h,004                ;print "NO GOOD!" 
CMSG:   call MSG                 ;print message 
        jmp AGAIN                ;start new game 
 
OHLF:   inr l                    ;advance pointer to 'O' board 
        mov a,c                  ;fetch current 'O' board 
        cmp m                    ;O boards match? 
        jnz SMD1                 ;no, continue search 
 
        dcr l                    ;move pointer to 'X' board 
        mov a,l                  ;set up to calculate pointer 
        rrc                      ;divide by 2 
        adi 106                  ;add to start of model index table 
        mov l,a                  ;set pointer to model index table 
        mov l,m                  ;fetch pointer to move index table 
        mvi h,004                ;set pointer to move index table 
MFD1:   mov a,m                  ;fetch move number 
        ana a                    ;move number here 
        jm ONO                   ;no move available, human wins 
        jnz MOVE                 ;move found, make it 
        inr l                    ;move zeroed, try next location 
        jmp MFD1 
 
MOVE:   mov e,l                  ;save move location 
        mvi l,004                ;set pointer to last move 
        mvi h,003 
        mov m,e                  ;save location as last move 
        rlc                      ;set up pointer to move 
        rlc                      ; storage table 
        adi 174 
        mov l,a                  ;set pointer 
        mov d,m                  ;fetch FROM bit 
        inr l                    ;advance pointer 
        mov c,m                  ;fetch TO bit 
        inr l                    ;advance pointer 
        mov e,m                  ;fetch capture bit 
        inr l                    ;advance pointer 
        mov a,m                  ;fetch contest bit 
        ana a                    ;is gave over? 
        jm WIN                   ;yes, computer wins 
        jnz DRAW                 ;yes, draw 
        mvi l,000                ;set pointer to 'X' board 
        mov a,m                  ;fetch current 'X' board 
        xra d                    ;clear old position 
        ora c                    ;set new position 
        mov m,a                  ;save new 'X' board 
        inr l                    ;advance pointer to 'O' board 
        mov a,e                  ;fetch capture bit 
        ana a                    ;capture? 
        jz PBD                   ;no, print new board 
        xra m                    ;yes, delete piece 
        mov m,a                  ;save new 'O' board 
        jnz PBD                  ;non zero, continue game 
 
        mvi l,004                ;set pointer to last move 
        mov l,m                  ;fetch last move location 
        mvi h,004                ;set pointer to active move list 
        mvi m,000                ;cancel last move 
        mvi h,005                ;set pointer to message "I WIN 
        mvi l,330                ;YOU NAVE NO PAWNS!" 
        call CMSG                ;print message and start again 
 
STX:    rlc                      ;bit set? 
        rnc                      ;no, return 
        mvi m,330                ;yes, put 'X' in board 
        ret 
 
STO:    rlc                      ;bit set? 
        rnc                      ;no, return 
        mvi m,317                ;yes, put 'O' in board 
        ret 
 
MSG:    mov a,m                  ;fetch character to print 
        ana a                    ;end of message? 
        rz                       ;yes, return 
        call PRINT               ;no, print character 
        inr l                    ;increment low address msg pointer 
        jnz MSG                  ;if not zero, continue output 
        inr h                    ;else, increment page address pointer 
        jmp MSG 
 
ERROR:  mvi l,364                ;set pointer to error message 
        mvi h,005 
        call MSG                 ;print error message 
        mvi h,003 
        jmp PBD                  ;print current board 
 
RTAL:   dcr b                    ;decrement bit count 
        rz                       ;if zero, return 
        rlc                      ;else, rotate left 
        jmp RTAL 
 
RTAR:   mov b,a                  ;set bit count 
        mvi a,001                ;set bit to rotate 
RTLP:   dcr b                    ;decrement bit count 
        rz                       ;if zero, return 
        rrc                      ;else rotate right 
        jmp RTLP 
 
BLK:    mov b,m                  ;fetch TO move 
        mvi l,000                ;set pointer to 'X' board 
        mov a,m                  ;fetch 'X' board 
        call RTAL                ;check for blocked move 
SET:    ana a                    ;is move blocked? 
        jm ERROR                 ;yes, illegal move 
        mvi e,000                ;set for no capture 
        jmp HMV                  ;return to make human move 
 
WIN:    mvi l,025                ;set pointer to store win move 
        mvi h,005 
        mov m,a                  ;store win move in message 
        mvi l,011                ;print "I MOVE TO " 
        jmp CMSG                 ;"I WIN, YOU LOSE" 
 
DRAW:   mvi l,052                ;print "DRAW, NO ONE WINS" 
        mvi h,005 
        jmp CMSG                 ;print draw message 
 
FNUM:   mov a,m                  ;fetch ASCII number 
        cpi 261                  ;is number valid? 
        rm                       ;no, return with S flag set 
        sui 272                  ;if number is valid, return 
        adi 200                  ;with S flag set 
        ret 
 
ONO:    mvi l,374                ;print "I CONCEDE!" 
        mvi h,004 
        call MSG                 ;then zero last move 
 
HWIN:   mvi l,004                ;set pointer to last move 
        mvi h,003 
        mov l,m                  ;fetch last move address 
        mvi h,004 
        mvi m,000                ;zero last move 
        mvi l,315                ;set pointer to lose message 
        mvi h,005 
        call MSG                 ;print lose message 
        jmp AGAIN                ;start new game 
 
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
INPUT:  in 0                     ; get input from serial port 
        rar                      ; rotate the received serial bit right into carry 
        jc INPUT                 ; jump if start bit not detected 
 
        ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit 
        mvi b,0                  ; initialize B 
        mvi b,0                  ; tweak timing 
        xra a                    ; clear the accumulator 
        out 08h                  ; send the start bit 
 
        call getbit              ; get bit 0 and echo it 
        call getbit              ; get bit 1 and echo it 
        call getbit              ; get bit 2 and echo it 
        call getbit              ; get bit 3 and echo it 
        call getbit              ; get bit 4 and echo it 
        call getbit              ; get bit 5 and echo it 
        call getbit              ; get bit 6 and echo it 
        call getbit              ; get bit 7 and echo it 
 
        ; wait 104 cycles, then send the stop bit 
        mov a,b                  ; save the character (now in B) in A 
        mvi b,0fch 
        call delay 
        mov b,a                  ; retrieve the character from A to B 
        mvi a,1 
        out 08h                  ; send the stop bit 
        ; wait 104 cycles. 
        mov a,b                  ; restore the character to A from B 
        ori 80h                  ; set the most significant bit 
        mvi b,0fch 
        mvi b,0fch 
        call delay 
        ret                      ; return to caller with the character in A 
 
        ORG 0640H 
;------------------------------------------------------------------------ 
; sends the character in A out from the serial port at 2400 bps. 
;------------------------------------------------------------------------ 
PRINT:  ani 7fh                  ; clear the most signficant bit 
        mov b,a                  ; save the character in B 
        xra a                    ; clear A for the start bit 
 
        out 08h                  ; send the start bit 
        mov a,b                  ; restore the character to A 
        mov a,b                  ; timing adjustment 
        mvi b,0fch 
        call delay 
 
        call outbit              ; bit 0 
        call outbit              ; bit 1 
        call outbit              ; bit 2 
        call outbit              ; bit 3 
        call outbit              ; bit 4 
        call outbit              ; bit 5 
        call outbit              ; bit 6 
        call outbit              ; bit 7 
 
        ;send the stop bit 
        mov b,a                  ; save the character in B 
        mvi a,1                  ; stop bit 
        out 08h                  ; send the stop bit 
        mov a,b                  ; restore the character from B to A 
        mvi b,0fch 
        call delay 
        ret                      ; return to caller 
 
outbit: out 08h                  ; output the least significant bit 
        mvi b,0fdh 
        call delay 
        ana a                    ; timing adjustment 
        rrc                      ; shift A right 
        ret 
 
delay:  inr b 
        jnz delay 
        ret 
 
getbit: mov a,b                  ; save B in A 
        mvi b,0ffh 
        mvi b,0ffh 
        call delay 
        mov b,a                  ; restore B from A 
        in 0                     ; get input from the serial port 
        out 08h                  ; echo the received bit 
        rar                      ; rotate the received bit right into carry 
        mov a,b                  ; restore the previously received bits 
        rar                      ; rotate the newly received bit in carry right into the MSB of A 
        mov b,a                  ; save the received bits in B 
        ret 
 
        END 0100H 
 
