            PAGE 0                  ; suppress page headings in AS listing file
;===================================================================================
; "Shooting Stars" published in Byte Magazine May, 1976.
;
; Slight modifications made to the published input/output routines to accommodate my
; single board computer - jsl
;
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023
;
; Jump to 0000H to start.
;===================================================================================

            cpu 8008                ; use "old" 8008 mnemonics
            radix 8                 ; use octal for numbers

CR          EQU 015
LF          EQU 012
EM          EQU 031
DEL         EQU 177

            ORG (0*400)+000
            JMP SHOOTSTR

            ORG (0*400)+060 ; RST 6
            JMP OUTCHAR

            ORG (0*400)+070 ; RST 7
            JMP INCHAR

            ORG (000*400)+100
SHOOTSTR:   LAI CR          ;DISPLAY A LINEFEED TO
            RST 6           ;   INITIALZE DISPLAY;

            LLI HMESS&377   ;SET ADDRESS POINTERS
            LHI HMESS/400   ;   TO HEADING MESSAGE;
            CAL OUTPUT      ;PRINT MESSAGE & RETURN;
            CAL INPUT       ;CALL INPUT LOOPER;
            CPI 'N'         ;IS FIRST LETTER 'N'?
            JTZ ASTART      ;IF SO THEN PLUNGE INTO GAME;
            LLI PAGE1&377   ;IF NOT THEN POINT TO FIRST
            LHI PAGE1/400   ;   PAGE OF RULES TEXT;
            CAL OUTPUT      ;AND GO OUTPUT RULES MESSAGE;
            RST 7           ;WAIT FOR GOAHEAD;

            LLI PAGE2&377   ;POINT TO SECOND PAGE OF
            LHI PAGE2/400   ;   RULES TEXT;
            CAL OUTPUT      ;DISPLAY SECOND PAGE OF RULES;
            RST 7           ;WAIT FOR GOAHEAD;
            LLI PAGE3&377   ;POINT TO THIRD PAGE OF RULES;
            LHI PAGE3/400   ;   RULES TEXT;
            CAL OUTPUT      ;DISPLAY THIRD PAGE OF RULES;
            RST 7           ;WAIT FOR GOAHEAD
ASTART:     LAI CR          ;SET UP LINEFEED
            RST 6           ;DISPLAY ONE LINEFEED,
            RST 6           ;   THEN A SECOND LINEFEED
            RST 6           ;   THEN A THIRD
            LBI 0           ;INITIALIZE THE UNIVERSE
            LCI 1           ;   TO STARTING PATTERN;
            LDB             ;THEN CLEAR SHOT COUNTER;
CNTSHOT:    IND             ;COUNT A SHOT (ANTICIPATORY),
SETCNT:     LEI 012         ;LOOP COUNT 10 INTERATIONS
DISLOOP:    DCE             ;IS THE LOOP DONE?
            JTZ WINTEST     ;IF SO THEN GO TO WIN TESTING.
            LAE             ;IF NOT THEN CONTINUE DISPLAY.
            CPI 6           ;IF IS FOURTH CYCLE?
            JTZ LINFEED     ;IF SO THEN NEW LINE NEEDED.
            CPI 3           ;IS IT SEVENTH CYCLE?
            JTZ LINFEED     ;IF SO THEN NEW LINE NEEDED
            CPI 5           ;IS IT STAR NUMBER 5
            JTZ FIVTST      ;IF SO THEN GO TEST STAR 5
NEDOT:      XRA             ;CLEAR THE CARRY (AND A TOO)
            LAB             ;MOVE UNIVERSE TO A
            RRC             ;ROTATE NEXT PLACE INTO CARRY,
            LBA             ;SAVE IT IN 8 FOR A WHILE
PSEUDOT:    JFC LOADOT      ;IF DOT THEN GO OUTPUT DOT;
            LAI '*'         ;OTHERWISE LOAD A STAR;
            RST 6           ;THEN PRINT THE STAR
            JMP SPCNOW      ;BRANCH AROUND THE DOT LOGIC;

LOADOT:     LAI '.'         ;LOAD A DOT
            RST 6           ;THEN PRINT THE DOT
SPCNOW:     LAI ' '         ;LOAD A SPACE;
            RST 6           ;PRINT ONE SPACE;
            RST 6           ;   THEN PRINT A SECOND
            JMP DISLOOP     ;WALTZ AROUND LOOP ONCE MORE;

LINFEED:    LAI CR          ;LOAD A LINE FEED.
            RST 6           ;DISPLAY A LINE FEED
            RST 6           ;   THEN A SECOND ONE;
            JMP NEDOT       ;BACK TO PRINT NEXT DOT OR STAR.

FIVTST:     XRA             ;NO OPERATION INTENDED - LEFTOVER.
            LAC             ;GET POSITION 5 STATUS;
            RRC             ;PUT STATUS INTO CARRY;
            JMP PSEUDOT     ;REJOIN MIANLINE AFTER RCC;

GOTSTAR:    LAI CR          ;LOAD A LINEFEED;
            RST 6           ;HAVE FINISHED UNIVERSE PRINT,
            RST 6           ;   SO PRINT SEVERAL
            RST 6           ;   LINE FEEDS
            RST 6           ;   TO SEPARATE
            RST 6           ;   SUCESSIVE ROUNDS
            LLI MESS7&377   ;POINT TO THE 'YOUR SHOT'
            LHI MESS7/400   ;   MESSAGE;
            CAL OUTPUT      ;THEN GO PRINT IT
            RST 7           ;CALL INPUT FOR CHARACTER
            RST 6           ;IMMEDIATELY ECHO THE INPUT;
            LEA             ;SAVE INPUT TEMPORARILY IN E;
            LAI CR          ;LOAD A LINE FEED
            RST 6           ;PRINT THREE LINE FEEDS TO
            RST 6           ;   SPACE OUT THE RESPONSE
            RST 6           ;   A BIT MORE
            LAE             ;RECOVER INPUT FOR TESTING
            LEI 011         ;LOOP COUNT FOR TABLE SEARCH;
            LLI MASKTAB&377 ;SET UP POINTER TO THE
            LHI MASKTAB/400 ;   THE MASK TABLE;
NEXTGRUP:   CPM             ;IS INPUT EQUAL TABLE CHARACTER?
            JTZ FOUND       ;IF SO THEN GO ALTER STRUCTER OF
            DCE             ;   THE UNIVERSE OTHERWISE JUST
            JTZ INVAL       ;   CHECK END OF LOOP;
            INL             ;INCREMENT THE L
            INL             ;   REGISTER POINTER
            INL             ;   FOUR TIMES TO GET
            INL             ;   TO NEXT TABLE ENTRY;
            JMP NEXTGRUP    ;THEN GO TEST NEXT ENTRY;

FOUND:      INL             ;POINT TO POSITION MASK
            LAM             ;   AND LOAD MASK INTO A;
            CPI 0           ;IS IT ZERO?
            JFZ UNIV2A      ;IF NOT THEN FRINGE POSITION;
            LAC             ;OTHERWISE THE CENTER POSITION;
            CPI 1           ;IS A STAR IN CENTER?
            JFZ BADFELO     ;IF NOT THEN HAVE WRONG MOVE;
            JMP NEXBYT      ;IFSO THEN GO PROCESS STAR

UNIV2A:     LAB             ;REST OF UNIVERS TO A;
            NDM             ;AND WITH MASK TO ISOLATE STAR;
            JTZ BADFELO     ;IF NOT STAR THEN WRONG MOVE;
NEXBYT:     INL             ;POINT TO THE GALAXY MASK;
            LAB             ;FETCH UNIVERSE AGAIN
            XRM             ;AND COMPLEME NT THE UNIVERSE
            LBA             ;   ON A FINE PERFORMANCE
            INL             ;POINT TO CERNTER MASK
            LAC             ;FETCH CENTER OF UNIVERSE
            XRM             ;COMPLEMENT CENTER IF REQUIRED
            LCA             ;SAVE CENTER OF UNIVERSE
            JMP CNTSHOT     ;GO DISPLAY A NEW UNIVERSE

INVAL:      CPI DEL         ;WAS INVALID SHOT A 'DELETE'?
            JFZ NOTVAL      ;IF NOT THEN RECYCLE BAD STAR;
            LLI MESS6&377   ;OTHER POINT TO GIVING UP
            LLI MESS6/400   ;   MESSAGE;
            JMP PRNTIT      ;DISPLAY THEN TEST FOR RESTART

NOTVAL:     LLI MESS2&377   ;POINT TO INVALID STAR
            LHI MESS2/400   ;   NUMBER MESSAGE
OUTMES:     CAL OUTPUT      ;OUTPUT A MESSAGE THEN
            JMP SETCNT      ;   GO DISPLAY THE UNIVERSE AGAIN;

WINTEST:    LAB             ;MOVE UNIVERSE TO A
            CPI 11111111B   ;ARE ALL FRINGE STARS PRESENT?
            JFZ LOSSTST     ;IF NOT SEE IF PLAYER HAS LOST;
            LAC             ;FETCH CENTER OF UNIVERSE;
            CPI 0           ;IS CENTER OF UNIVERSE EMPTY
            JFZ GOTSTAR     ;IS FULL THEN NOT WIN;
            LLI MESS4&377   ;NO STAR! GOT A WIN FOLKS
            LHI MESS4/400   ;   SO POINT TO WIN MESSAGE;
            CAL OUTPUT      ;THEN DISPLAY WIN MESSAGE;
            LEI '0'         ;BEGIN BINARY TO DECIMAL CONVERSION
            LBE             ;   BY SETTING ALL THREE WORKING
            LCE             ;   REGISTER TO (ASCII) ZERO;
            DCD             ;GET RID OF LAST SHOT;
            LAD             ;MOVE SHOT COUNT TO A FOR TEST;
            CPI 0           ;TEST FOR ZERO (NOT NEEDED IN
            JTZ LSTSIG      ;   SHOOTING STARS BUT GENERALLY
                            ;   USEFUL WITH CONVERSIONS);
            LAI '9'+1       ;NEED COMPARE TO ASCII '9'+1;
MOREDEC:    INE             ;COUNT UP ONE IN 1.S DIGIT;
            CPE             ;IS IT EQUAL TO OVERFLOW CODE?
            JFZ TALLYHO     ;IF NOT THEN TALLY AND CONTINUE
            LEI '0'         ;ELSE RESET 1'S DIGIT TO ZERO
            INC             ;   AND CARRY INTO NEXT DIGIT;
            CPC             ;IS IT EQUAL TO OVERFLOW CODE TOO?
            JFZ TALLYHO     ;IF NOT THEN TALLY AND CONTINUE;
            LCI '0'         ;ELSE RESET MIDDLE DIGIT TO ZERO
            INB             ;   AND CARRY INTO M.S. DIGIT;
TALLYHO:    DCD             ;DECREMENT SCORE COUNTER FOR TALLY
            JFZ MOREDEC     ;IF NOT ZERO THEN KEEP OPTIONS
            LAB             ;FETCH LEADING DIGIT TO A:
            CPI '0'         ;IS IT (ASCII) ZERO?
            JFZ THREED      ;IF NOT GO DISPLAY THREE DIGITS
            LAC             ;FETCH MIDDLE DIGIT TO A;
            CPI '0'         ;IS IT (ASCII) ZERO TOO?
            JFZ MIDPRINT    ;IF NOT GO DISPLAY TWO DIGITS
            JMP LSTSIG      ;IF SO DISPLAY ONLY ONE;

THREED:     RST 6           ;DISPLAY THREE DIGITS, LEFT FIRST;
            LAC             ;FETCH MIDDLE DIGIT TO A;
MIDPRINT:   RST 6           ;DISPLAY TOW DIGITS, LEFT FIRST;
LSTSIG:     LAE             ;FETCH 1'S DIGIT;
            RST 6           ;DISPLAY REMAINING DIGIT;
            LLI MESS5&377   ;POINT TO FIRST PART OF YOU WIN;
RECYC:      LHI MESS5/400   ;SECOND PART OF MESS5/MESS6 POINTER;
PRNTIT:     CAL OUTPUT      ;DISPLAY THE MESSAGE;
            CAL INPUT       ;FETCH A CHARACTER FOR CONTINUE
            CPI 'Y'         ;   QUERY, IS IT 'YES"?
            JTZ ASTART      ;IF SO THEN CONTINUE GAME;
            HLT             ;OTHERWISE HALT
            
LOSSTST:    CPI 0           ;IS FRINGE UNIVERSE ALL BLACK HOLES?
            JFZ GOTSTAR     ;IFNOT THEN CONTINUE GAME;
            LAC             ;IF SO THEN TEST CENTER POSITION;
            CPI 0           ;IS CENTER ALSO BLACK HOLE?
            JFZ GOTSTAR     ;IF NOT THEN CONTINUE GAME;
            LLI MESS3&377   ;ELSE POINT TO LOSS MESSAGE;
            JMP RECYC       ;AND GO PRINT LOSS;

MASKTAB:    DB   061,001,013,001 ;36 BYTES OF MASK TABLE
            DB   062,002,007,000
            DB   063,004,026,001
            DB   064,010,051,000
            DB   065,000,132,001
            DB   066,020,224,000
            DB   067,040,150,001
            DB   070,100,340,000
            DB   071,200,320,001

OUTPUT:     LAM             ;FETCH NEXT MESSAGE BYTE.
            CPI EM          ;IS IT A DELIMITER?
            RTZ             ;RETURN WHEN DELIMITER FOUND.
            RST 6           ;OTHERWISE DISPLAY BYTE;
            INL             ;POINT TO NEXT BYTE;
            JFZ OUTPUT      ;IS IT PAGE BOUNDARY?
            INH             ;IF SO INCREMENT PAGE;
            JMP OUTPUT      ;AND THEN RECYCLE.

INPUT:      RST 7           ;GET NEXT CHARACTER
            LEA             ;SAVE IT IN E.
            RST 6           ;ECHO ON DISPLAY
GETNEXT:    RST 7           ;GET NEXT CHARACTER;
            RST 6           ;ECH ON DISPLY;
            CPI CR          ;WAS IT A LINE FEED?
            JFZ GETNEXT     ;IF NOT CONTINUE SCAN.
            LAE             ;IF SO, RESTORE FIRST INPUT;
            RET             ;AND THEN RETURN TO CALLER;

BADFELO:    LLI MESS1&377   ;POINT TO THE ERROR MESSAGE
            LHI MESS1/400   ;   ADMONISHING BAD "STAR';
            JMP OUTMES      ;AND GO DISPLAY ERROR;

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

            cpu 8008new             ; use "new" 8008 mnemonics
;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. do not echo. return the character in A.
; uses A and D.
;-----------------------------------------------------------------------------------------
INCHAR:     in 0                    ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc INCHAR               ; jump if start bit not detected

            ; start bit detected. wait 52 cycles
            mvi d,0                 ; initialize D
            mvi d,0                 ; timing adjustment
            mvi d,0                 ; timing adjustment
            xra a                   ; clear the accumulator

            call getbit             ; bit 0
            call getbit             ; bit 1
            call getbit             ; bit 2
            call getbit             ; bit 3
            call getbit             ; bit 4
            call getbit             ; bit 5
            call getbit             ; bit 6
            call getbit             ; bit 7

            ; wait 104 cycles for the stop bit
            mov a,d                 ; move the character from D to A
            mvi d,0fch
            call delay
            mov d,a                 ; move the character from A to D
            mvi a,1
            mvi a,1                 ; timing adjustment
            ; wait 104 cycles.
            mov a,d                 ; restore the character from D to A
            mvi d,0fch
            mvi d,0fch              ; timing adjustment
            call delay
            ret

getbit:     mov a,d                 ; save the bits in A
            mvi d,0ffh
            mvi d,0ffh              ; timing adjustment
            call delay
            mov d,a                 ; save the bits in D
            in 0                    ; get input from the serial port
            in 0                    ; timing adjustment
            rar                     ; rotate the received bit right into carry
            mov a,d                 ; restore the previously received bits from D to A
            rar                     ; rotate the newly received bit in carry right into the MSB of A
            mov d,a                 ; save the received bits in D
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
OUTCHAR:    ani 7fh                 ; mask off MSB
            mov d,a                 ; save the character in D
            xra a                   ; clear A for the start bit

            out 08h                 ; send the start bit
            mov a,d                 ; restore the character to A
            mov a,d                 ; timing adjustment
            mvi d,0fch
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
            mov d,a                 ; save the character in D
            mvi a,1                 ; stop bit
            out 08h                 ; send the stop bit
            mov a,d                 ; restore the character from D
            mvi d,0fch
            call delay
            ret

putbit:     out 08h                 ; output the least significant bit
            mvi d,0fdh
            call delay
            ana a                   ; timing adjustment
            rrc                     ; shift A right
            ret

            cpu 8008                ; return to using "old" 8008 mnemonics
            
            end 0000H