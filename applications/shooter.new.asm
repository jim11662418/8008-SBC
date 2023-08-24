            PAGE 0               ; suppress page headings in AS listing file 
;=================================================================================== 
; "Shooting Stars" published in Byte Magazine May, 1976. 
; 
; Slight modifications made to the published input/output routines to accommodate my 
; single board computer - jsl 
; 
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023 
; 
; serial I/O at 2400 bps N-8-1   
; 
; Jump to 0000H to start. 
;=================================================================================== 
 
            cpu 8008new          ; use "old" 8008 mnemonics 
            radix 8              ; use octal for numbers 
 
CR          EQU 015 
LF          EQU 012 
EM          EQU 031 
DEL         EQU 177 
 
            ORG (0*400)+000 
            jmp SHOOTSTR 
 
            ORG (0*400)+060      ; rst 6 
            jmp OUTCHAR 
 
            ORG (0*400)+070      ; rst 7 
            jmp INCHAR 
 
            ORG (000*400)+100 
SHOOTSTR:   mvi a,CR             ;DISPLAY A LINEFEED TO 
            rst 6                ;   INITIALZE DISPLAY; 
 
            mvi l,HMESS&377      ;SET ADDRESS POINTERS 
            mvi h,HMESS/400      ;   TO HEADING MESSAGE; 
            call OUTPUT          ;PRINT MESSAGE & RETURN; 
            call INPUT           ;CALL INPUT LOOPER; 
            cpi 'N'              ;IS FIRST LETTER 'N'? 
            jz ASTART            ;IF SO THEN PLUNGE INTO GAME; 
            mvi l,PAGE1&377      ;IF NOT THEN POINT TO FIRST 
            mvi h,PAGE1/400      ;   PAGE OF RULES TEXT; 
            call OUTPUT          ;AND GO OUTPUT RULES MESSAGE; 
            rst 7                ;WAIT FOR GOAHEAD; 
 
            mvi l,PAGE2&377      ;POINT TO SECOND PAGE OF 
            mvi h,PAGE2/400      ;   RULES TEXT; 
            call OUTPUT          ;DISPLAY SECOND PAGE OF RULES; 
            rst 7                ;WAIT FOR GOAHEAD; 
            mvi l,PAGE3&377      ;POINT TO THIRD PAGE OF RULES; 
            mvi h,PAGE3/400      ;   RULES TEXT; 
            call OUTPUT          ;DISPLAY THIRD PAGE OF RULES; 
            rst 7                ;WAIT FOR GOAHEAD 
ASTART:     mvi a,CR             ;SET UP LINEFEED 
            rst 6                ;DISPLAY ONE LINEFEED, 
            rst 6                ;   THEN A SECOND LINEFEED 
            rst 6                ;   THEN A THIRD 
            mvi b,0              ;INITIALIZE THE UNIVERSE 
            mvi c,1              ;   TO STARTING PATTERN; 
            mov d,b              ;THEN CLEAR SHOT COUNTER; 
CNTSHOT:    inr d                ;COUNT A SHOT (ANTICIPATORY), 
SETCNT:     mvi e,012            ;LOOP COUNT 10 INTERATIONS 
DISLOOP:    dcr e                ;IS THE LOOP DONE? 
            jz WINTEST           ;IF SO THEN GO TO WIN TESTING. 
            mov a,e              ;IF NOT THEN CONTINUE DISPLAY. 
            cpi 6                ;IF IS FOURTH CYCLE? 
            jz LINFEED           ;IF SO THEN NEW LINE NEEDED. 
            cpi 3                ;IS IT SEVENTH CYCLE? 
            jz LINFEED           ;IF SO THEN NEW LINE NEEDED 
            cpi 5                ;IS IT STAR NUMBER 5 
            jz FIVTST            ;IF SO THEN GO TEST STAR 5 
NEDOT:      xra a                ;CLEAR THE CARRY (AND A TOO) 
            mov a,b              ;MOVE UNIVERSE TO A 
            rrc                  ;ROTATE NEXT PLACE INTO CARRY, 
            mov b,a              ;SAVE IT IN 8 FOR A WHILE 
PSEUDOT:    jnc LOADOT           ;IF DOT THEN GO OUTPUT DOT; 
            mvi a,'*'            ;OTHERWISE LOAD A STAR; 
            rst 6                ;THEN PRINT THE STAR 
            jmp SPCNOW           ;BRANCH AROUND THE DOT LOGIC; 
 
LOADOT:     mvi a,'.'            ;LOAD A DOT 
            rst 6                ;THEN PRINT THE DOT 
SPCNOW:     mvi a,' '            ;LOAD A SPACE; 
            rst 6                ;PRINT ONE SPACE; 
            rst 6                ;   THEN PRINT A SECOND 
            jmp DISLOOP          ;WALTZ AROUND LOOP ONCE MORE; 
 
LINFEED:    mvi a,CR             ;LOAD A LINE FEED. 
            rst 6                ;DISPLAY A LINE FEED 
            rst 6                ;   THEN A SECOND ONE; 
            jmp NEDOT            ;BACK TO PRINT NEXT DOT OR STAR. 
 
FIVTST:     xra a                ;NO OPERATION INTENDED - LEFTOVER. 
            mov a,c              ;GET POSITION 5 STATUS; 
            rrc                  ;PUT STATUS INTO CARRY; 
            jmp PSEUDOT          ;REJOIN MIANLINE AFTER RCC; 
 
GOTSTAR:    mvi a,CR             ;LOAD A LINEFEED; 
            rst 6                ;HAVE FINISHED UNIVERSE PRINT, 
            rst 6                ;   SO PRINT SEVERAL 
            rst 6                ;   LINE FEEDS 
            rst 6                ;   TO SEPARATE 
            rst 6                ;   SUCESSIVE ROUNDS 
            mvi l,MESS7&377      ;POINT TO THE 'YOUR SHOT' 
            mvi h,MESS7/400      ;   MESSAGE; 
            call OUTPUT          ;THEN GO PRINT IT 
            rst 7                ;CALL INPUT FOR CHARACTER 
            rst 6                ;IMMEDIATELY ECHO THE INPUT; 
            mov e,a              ;SAVE INPUT TEMPORARILY IN E; 
            mvi a,CR             ;LOAD A LINE FEED 
            rst 6                ;PRINT THREE LINE FEEDS TO 
            rst 6                ;   SPACE out THE RESPONSE 
            rst 6                ;   A BIT MORE 
            mov a,e              ;RECOVER INPUT FOR TESTING 
            mvi e,011            ;LOOP COUNT FOR TABLE SEARCH; 
            mvi l,MASKTAB&377    ;SET UP POINTER TO THE 
            mvi h,MASKTAB/400    ;   THE MASK TABLE; 
NEXTGRUP:   cmp m                ;IS INPUT EQUAL TABLE CHARACTER? 
            jz FOUND             ;IF SO THEN GO ALTER STRUCTER OF 
            dcr e                ;   THE UNIVERSE OTHERWISE JUST 
            jz INVAL             ;   CHECK END OF LOOP; 
            inr l                ;INCREMENT THE L 
            inr l                ;   REGISTER POINTER 
            inr l                ;   FOUR TIMES TO GET 
            inr l                ;   TO NEXT TABLE ENTRY; 
            jmp NEXTGRUP         ;THEN GO TEST NEXT ENTRY; 
 
FOUND:      inr l                ;POINT TO POSITION MASK 
            mov a,m              ;   AND LOAD MASK INTO A; 
            cpi 0                ;IS IT ZERO? 
            jnz UNIV2A           ;IF NOT THEN FRINGE POSITION; 
            mov a,c              ;OTHERWISE THE CENTER POSITION; 
            cpi 1                ;IS A STAR IN CENTER? 
            jnz BADFELO          ;IF NOT THEN HAVE WRONG MOVE; 
            jmp NEXBYT           ;IFSO THEN GO PROCESS STAR 
 
UNIV2A:     mov a,b              ;REST OF UNIVERS TO A; 
            ana m                ;AND WITH MASK TO ISOLATE STAR; 
            jz BADFELO           ;IF NOT STAR THEN WRONG MOVE; 
NEXBYT:     inr l                ;POINT TO THE GALAXY MASK; 
            mov a,b              ;FETCH UNIVERSE AGAIN 
            xra m                ;AND COMPLEME NT THE UNIVERSE 
            mov b,a              ;   ON A FINE PERFORMANCE 
            inr l                ;POINT TO CERNTER MASK 
            mov a,c              ;FETCH CENTER OF UNIVERSE 
            xra m                ;COMPLEMENT CENTER IF REQUIRED 
            mov c,a              ;SAVE CENTER OF UNIVERSE 
            jmp CNTSHOT          ;GO DISPLAY A NEW UNIVERSE 
 
INVAL:      cpi DEL              ;WAS INVALID SHOT A 'DELETE'? 
            jnz NOTVAL           ;IF NOT THEN RECYCLE BAD STAR; 
            mvi l,MESS6&377      ;OTHER POINT TO GIVING UP 
            mvi l,MESS6/400      ;   MESSAGE; 
            jmp PRNTIT           ;DISPLAY THEN TEST FOR RESTART 
 
NOTVAL:     mvi l,MESS2&377      ;POINT TO INVALID STAR 
            mvi h,MESS2/400      ;   NUMBER MESSAGE 
OUTMES:     call OUTPUT          ;OUTPUT A MESSAGE THEN 
            jmp SETCNT           ;   GO DISPLAY THE UNIVERSE AGAIN; 
 
WINTEST:    mov a,b              ;MOVE UNIVERSE TO A 
            cpi 11111111B        ;ARE ALL FRINGE STARS PRESENT? 
            jnz LOSSTST          ;IF NOT SEE IF PLAYER HAS LOST; 
            mov a,c              ;FETCH CENTER OF UNIVERSE; 
            cpi 0                ;IS CENTER OF UNIVERSE EMPTY 
            jnz GOTSTAR          ;IS FULL THEN NOT WIN; 
            mvi l,MESS4&377      ;NO STAR! GOT A WIN FOLKS 
            mvi h,MESS4/400      ;   SO POINT TO WIN MESSAGE; 
            call OUTPUT          ;THEN DISPLAY WIN MESSAGE; 
            mvi e,'0'            ;BEGIN BINARY TO DECIMAL CONVERSION 
            mov b,e              ;   BY SETTING ALL THREE WORKING 
            mov c,e              ;   REGISTER TO (ASCII) ZERO; 
            dcr d                ;GET RID OF LAST SHOT; 
            mov a,d              ;MOVE SHOT COUNT TO A FOR TEST; 
            cpi 0                ;TEST FOR ZERO (NOT NEEDED IN 
            jz LSTSIG            ;   SHOOTING STARS BUT GENERALLY 
                                 ;   USEFUL WITH CONVERSIONS); 
            mvi a,'9'+1          ;NEED COMPARE TO ASCII '9'+1; 
MOREDEC:    inr e                ;COUNT UP ONE IN 1.S DIGIT; 
            cmp e                ;IS IT EQUAL TO OVERFLOW CODE? 
            jnz TALLYHO          ;IF NOT THEN TALLY AND CONTINUE 
            mvi e,'0'            ;ELSE RESET 1'S DIGIT TO ZERO 
            inr c                ;   AND CARRY INTO NEXT DIGIT; 
            cmp c                ;IS IT EQUAL TO OVERFLOW CODE TOO? 
            jnz TALLYHO          ;IF NOT THEN TALLY AND CONTINUE; 
            mvi c,'0'            ;ELSE RESET MIDDLE DIGIT TO ZERO 
            inr b                ;   AND CARRY INTO M.S. DIGIT; 
TALLYHO:    dcr d                ;DECREMENT SCORE COUNTER FOR TALLY 
            jnz MOREDEC          ;IF NOT ZERO THEN KEEP OPTIONS 
            mov a,b              ;FETCH LEADING DIGIT TO A: 
            cpi '0'              ;IS IT (ASCII) ZERO? 
            jnz THREED           ;IF NOT GO DISPLAY THREE DIGITS 
            mov a,c              ;FETCH MIDDLE DIGIT TO A; 
            cpi '0'              ;IS IT (ASCII) ZERO TOO? 
            jnz MIDPRINT         ;IF NOT GO DISPLAY TWO DIGITS 
            jmp LSTSIG           ;IF SO DISPLAY ONLY ONE; 
 
THREED:     rst 6                ;DISPLAY THREE DIGITS, LEFT FIRST; 
            mov a,c              ;FETCH MIDDLE DIGIT TO A; 
MIDPRINT:   rst 6                ;DISPLAY TOW DIGITS, LEFT FIRST; 
LSTSIG:     mov a,e              ;FETCH 1'S DIGIT; 
            rst 6                ;DISPLAY REMAINING DIGIT; 
            mvi l,MESS5&377      ;POINT TO FIRST PART OF YOU WIN; 
RECYC:      mvi h,MESS5/400      ;SECOND PART OF MESS5/MESS6 POINTER; 
PRNTIT:     call OUTPUT          ;DISPLAY THE MESSAGE; 
            call INPUT           ;FETCH A CHARACTER FOR CONTINUE 
            cpi 'Y'              ;   QUERY, IS IT 'YES"? 
            jz ASTART            ;IF SO THEN CONTINUE GAME; 
            HLT                  ;OTHERWISE HALT 
             
LOSSTST:    cpi 0                ;IS FRINGE UNIVERSE ALL BLACK HOLES? 
            jnz GOTSTAR          ;IFNOT THEN CONTINUE GAME; 
            mov a,c              ;IF SO THEN TEST CENTER POSITION; 
            cpi 0                ;IS CENTER ALSO BLACK HOLE? 
            jnz GOTSTAR          ;IF NOT THEN CONTINUE GAME; 
            mvi l,MESS3&377      ;ELSE POINT TO LOSS MESSAGE; 
            jmp RECYC            ;AND GO PRINT LOSS; 
 
MASKTAB:    DB   061,001,013,001 ;36 BYTES OF MASK TABLE 
            DB   062,002,007,000 
            DB   063,004,026,001 
            DB   064,010,051,000 
            DB   065,000,132,001 
            DB   066,020,224,000 
            DB   067,040,150,001 
            DB   070,100,340,000 
            DB   071,200,320,001 
 
OUTPUT:     mov a,m              ;FETCH NEXT MESSAGE BYTE. 
            cpi EM               ;IS IT A DELIMITER? 
            rz                   ;RETURN WHEN DELIMITER FOUND. 
            rst 6                ;OTHERWISE DISPLAY BYTE; 
            inr l                ;POINT TO NEXT BYTE; 
            jnz OUTPUT           ;IS IT PAGE BOUNDARY? 
            inr h                ;IF SO INCREMENT PAGE; 
            jmp OUTPUT           ;AND THEN RECYCLE. 
 
INPUT:      rst 7                ;GET NEXT CHARACTER 
            mov e,a              ;SAVE IT IN E. 
            rst 6                ;ECHO ON DISPLAY 
GETNEXT:    rst 7                ;GET NEXT CHARACTER; 
            rst 6                ;ECH ON DISPLY; 
            cpi CR               ;WAS IT A LINE FEED? 
            jnz GETNEXT          ;IF NOT CONTINUE SCAN. 
            mov a,e              ;IF SO, RESTORE FIRST INPUT; 
            ret                  ;AND THEN RETURN TO CALLER; 
 
BADFELO:    mvi l,MESS1&377      ;POINT TO THE ERROR MESSAGE 
            mvi h,MESS1/400      ;   ADMONISHING BAD "STAR'; 
            jmp OUTMES           ;AND GO DISPLAY ERROR; 
 
MESS1:      DB      "HEY! YOU CAN ONLY SHOOT STARS,",LF 
            DB      "NOT BLACK HOLES.",LF 
            DB      "TRY AGAIN!",LF,EM 
MESS2:      DB      "THAT WASNT A VALID STAR NUMBER!",LF 
            DB      "TRY AGAIN!",LF,EM 
MESS3:      DB      "YOU LOST THE GAME:",LF 
            DB      "WANT TO SHOOT SOME MORE STARS!  ",EM 
MESS4:      DB      "YOU WIN! GOOD SHOOTING:",LF 
            DB      "YOU FIRED  ",EM 
MESS5:      DB      " SHOTS.",LF 
            DB      "BEST POSSIBLE SCORE IS 11 SHOTS",LF 
            DB      "WANT TO SHOOT AGAIN DEADEYE?  ",EM 
MESS6:      DB      "YOU GIVE UP TOO EASILY!",LF 
            DB      "WANT TO SHOOT SOME MORE STARS? ",EM 
MESS7:      DB      "YOUR SHOT? ",EM 
HMESS:      DB      "S H O   SSS   TTT   AAA   RRR   SSS",LF 
            DB      "        S     T     A A   R R   S",LF 
            DB      "O * T   SSS   T     AAA   RRR   SSS",LF 
            DB      "          S   T     A A   RR      S",LF 
            DB      "I N G   SSS   T     A A   R R   SSS",LF 
            DB      "**********S H O O T I N G   S T A R S******",LF 
            DB      "           A BRAIN TEASER GAME",LF 
            DB      "WANT THE RULES? ",EM 
PAGE1:      DB      "THERE ARE STARS:      *",LF 
            DB      "AND BLACK HOLES:      .",LF,LF 
            DB      "IN THE UNIVERSE         + + +",LF 
            DB      "                        + + +",LF 
            DB      "                        + + +",LF 
            DB      "YOU SHOOT A STAR",LF 
            DB      "(NOT A BLACK HOLE)",LF 
            DB      "BY TYPING ITS NUMBER    1 2 3",LF 
            DB      "                        4 5 6" ,LF 
            DB      "                        7 8 9",LF 
            DB      "THAT CHANGES THE STAR TO A BLACK HOLE!",LF 
            DB      "(TO SEE MORE RULES, TYPE ANY KEY.) ",EM 
PAGE2:      DB      "EACH STAR IS IN A GALAXY. WHEN YOU",LF 
            DB      "SHOOT A STAR, EVERYTHING IN ITS GALAXY",LF 
            DB      "CHANGES. ALL STARS BECOME BLOCK HOLES",LF 
            DB      "AND ALL BLACK HOLES BECOME STARS.",LF,LF 
            DB      "GALAXIES", 012 
            DB      "1*.   *2*   .3*   *..  .*.",LF 
            DB      "**.   ...   .**   4..  *5*",LF 
            DB      "...   ...   ...   *..  .*.",LF,LF 
            DB      "..*   ...   ...   ...",LF 
            DB      "..6   **.   ...   .**",LF 
            DB      "..*   7**   *8*   .*9",LF 
            DB      "(TYPE ANY KEY FOR LAST PAGE OF RULES.) ",EM 
PAGE3:      DB      "THE GAME STARTS",LF 
            DB      "WITH THE UNIVERSE  ...",LF 
            DB      "LIKE THIS          .*.",LF 
            DB      "                   ...",LF,LF 
            DB      "YOU WIN WHEN YOU   ***",LF 
            DB      "CHANGE IT TO THIS  *.*",LF 
            DB      "                   ***",LF,LF 
            DB      "YOU LOSE IF YOU    ...",LF 
            DB      "GET THIS           ...",LF 
            DB      "                   ...",LF,LF 
            DB      "READY TO PLAY. TYPE ANY KEY TO START",LF 
            DB      "THE GAME. GOOD LUCK!",EM 
 
            cpu 8008new          ; use "new" 8008 mnemonics 
;----------------------------------------------------------------------------------------- 
; wait for a character from the serial port. do not echo. return the character in A. 
; uses A and D. 
;----------------------------------------------------------------------------------------- 
INCHAR:     in 0                 ; get input from serial port 
            rar                  ; rotate the received serial bit right into carry 
            jc INCHAR            ; jump if start bit not detected 
 
                                 ; start bit detected. wait 52 cycles 
            mvi d,0              ; initialize D 
            mvi d,0              ; timing adjustment 
            mvi d,0              ; timing adjustment 
            xra a                ; clear the accumulator 
 
            call getbit          ; bit 0 
            call getbit          ; bit 1 
            call getbit          ; bit 2 
            call getbit          ; bit 3 
            call getbit          ; bit 4 
            call getbit          ; bit 5 
            call getbit          ; bit 6 
            call getbit          ; bit 7 
 
                                 ; wait 104 cycles for the stop bit 
            mov a,d              ; move the character from D to A 
            mvi d,0fch 
            call delay 
            mov d,a              ; move the character from A to D 
            mvi a,1 
            mvi a,1              ; timing adjustment 
                                 ; wait 104 cycles. 
            mov a,d              ; restore the character from D to A 
            mvi d,0fch 
            mvi d,0fch           ; timing adjustment 
            call delay 
            ret 
 
getbit:     mov a,d              ; save the bits in A 
            mvi d,0ffh 
            mvi d,0ffh           ; timing adjustment 
            call delay 
            mov d,a              ; save the bits in D 
            in 0                 ; get input from the serial port 
            in 0                 ; timing adjustment 
            rar                  ; rotate the received bit right into carry 
            mov a,d              ; restore the previously received bits from D to A 
            rar                  ; rotate the newly received bit in carry right into the MSB of A 
            mov d,a              ; save the received bits in D 
            ret 
 
;------------------------------------------------------------------------ 
; delay in microseconds = (((255-value in D)*16)+19) * 4 microseconds 
;------------------------------------------------------------------------ 
delay:      inr d 
            jnz delay 
            ret 
 
 
;------------------------------------------------------------------------ 
; sends the character in A out from the serial port at 2400 bps. 
; uses A and D. 
;------------------------------------------------------------------------ 
OUTCHAR:    ani 7fh              ; mask off MSB 
            mov d,a              ; save the character in D 
            xra a                ; clear A for the start bit 
 
            out 08h              ; send the start bit 
            mov a,d              ; restore the character to A 
            mov a,d              ; timing adjustment 
            mvi d,0fch 
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
            mov d,a              ; save the character in D 
            mvi a,1              ; stop bit 
            out 08h              ; send the stop bit 
            mov a,d              ; restore the character from D 
            mvi d,0fch 
            call delay 
            ret 
 
putbit:     out 08h              ; output the least significant bit 
            mvi d,0fdh 
            call delay 
            ana a                ; timing adjustment 
            rrc                  ; shift A right 
            ret 
 
            cpu 8008new          ; return to using "old" 8008 mnemonics 
             
            end 0000H