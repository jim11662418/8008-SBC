        PAGE 0                   ; suppress page headings in AS listing file 
;============================================================================= 
; This is port of the classic "Star Trek" computer game to the 8008. 
; The object of the Galaxy game is to destroy all the alien ships in 
; the galaxy. The exact number of alien ships which must be destroyed 
; is defined in the initial message along with the number of stardates 
; one has to complete the mission, and the number of space stations 
; available in the galaxy for refueling. 
; 
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023 
;
; edited to use 'new' Intel 8008 mnemonics by Jim Loos 08/23/2023 
; 
; jump to 0A00H to start 
; 
; serial I/O at 2400 bps N-8-1 
;============================================================================= 
 
            cpu 8008new          ; use 'new' Intel 8008 mnemonics
            radix 8              ; all numbers are octal 
 
CR          EQU   0DH 
LF          EQU   0AH 
 
                                 ;ORG (0*400)+000 
            org 0000H 
            DB    002            ;Course 1.0 
            DB    000 
            DB    002            ;Course 1.5 
            DB    377 
            DB    002            ;Course 2.0 
            DB    376 
            DB    001            ;Course 2.5 
            DB    376 
            DB    000            ;Course 3.0 
            DB    376 
            DB    377            ;Course 3.5 
            DB    376 
            DB    376            ;Course 4.0 
            DB    376 
            DB    376            ;Course 4.5 
            DB    377 
            DB    376            ;Course 5.0 
            DB    000 
            DB    376            ;Course 5.5 
            DB    001 
            DB    376            ;Course 6.0 
            DB    002 
            DB    377            ;Course 6.5 
            DB    002 
            DB    000            ;Course 7.0 
            DB    002 
            DB    001            ;Course 7.5 
            DB    002 
            DB    002            ;Course 8.0 
            DB    002 
            DB    002            ;Course 8.5 
            DB    001 
 
                                 ;ORG (0*400)+050 
            org 0028H 
            DB    000            ;Register storage 
            DB    000            ;Register storage 
            DB    000            ;Register storage 
            DB    000            ;Termporary storage 
 
                                 ;ORG (0*400)+060 
            org 0030H 
            DB    000            ;Crossing flag 
            DB    000            ;Crossing indicator 
            DB    000            ;Temporary storage 
            DB    000            ;Termporary storage 
 
                                 ;ORG (0*400)+100 
            org 0040H 
            DB    000            ;100 Random number storage 
            DB    000            ;101 Random number storage 
            DB    000            ;102 Current quadrant contents 
            DB    000            ;103 Sector location of space ship 
            DB    000            ;104 Sector location of star 
            DB    000            ;105 Sector location of star 
            DB    000            ;106 Sector location of star 
            DB    000            ;107 Sector location of star 
            DB    000            ;110 Sector location of star 
            DB    000            ;111 Sector location of star 
            DB    000            ;112 Sector location of star 
            DB    000            ;113 Sector location of space station 
            DB    000            ;114 Sector location of alien ship no.1 
            DB    000            ;115 Sector location of alien ship no.2 
            DB    000            ;116 Sector location of alien ship. no.3 
            DB    000            ;117 Double precision value of main energy least significant half 
            DB    000            ;120 Double precision value of main energy most significant half 
            DB    000            ;121 Double precision value of shield energy least significant half 
            DB    000            ;122 Double precision value of shield energy most significant half 
            DB    000            ;123 Double precision value of alien ship no. 1 energy least significant half 
            DB    000            ;124 Double precision value of alien ship no. 1 energy most significant half 
            DB    000            ;125 Double precision value of alien ship no. 2 energy least significant half 
            DB    000            ;126 Double precision value of alien ship no. 2 energy most significant half 
            DB    000            ;127 Double precision value of alien ship no. 3 energy least significant half 
            DB    000            ;130 Double precision value of alien ship no. 3 energy most significant half 
            DB    000            ;131 Current quadrant location of space ship 
            DB    000            ;132 Number of torpedoes remaining 
            DB    000            ;133 Number of space stations 
            DB    000            ;134 Number of alien ships 
            DB    000            ;135 Number of stardates remaining 
            DB    000            ;136 Temporary storage 
            DB    000            ;137 Temporary storage 
            DB    000            ;140 Digit storage 
            DB    000            ;141 Digit storage 
            DB    000            ;142 Digit storage 
            DB    000            ;143 Digit storage 
            DB    000            ;144 Digit storage 
 
; delay subroutine used by serial input/output functions 
            cpu 8008new 
delay:      inr b 
            jnz delay 
            ret 
            cpu 8008new 
 
                                 ;ORG (0*400)+200 
            org 0080H 
            DB    CR,LF,"1     1     1     1     1     1     1     1     1",0 
 
;  addresses 300 (C0H) through 377 (FFH) reserved for Galaxy Content table 
 
                                 ;ORG (1*400)+000 
            org 0100H 
MSG_DYW:    DB   CR,LF,"DO YOU WANT TO GO ON A SPACE VOYAGE? ",0 
MSG_YJD:    DB   CR,LF,"YOU MUST DESTROY  " 
MSG_SPS:    DB   "  ALIEN SHIPS IN  " 
MSG_DTS:    DB   "  STARDATES WITH " 
MSG_SSS:    DB   "  SPACE STATIONS",0 
MSG_123:    DB   CR,LF," -1--2--3--4--5--6--7--8-",0 
MSG_STDT:   DB   CR,LF 
MSG_STDT1:  DB   "0" 
MSG_STDT2:  DB   "                        ",0 
MSG_STDT3:  DB   " STARDATE  300" 
MSG_SDP:    DB   "0",0 
MSG_CND:    DB   " CONDITION " 
MSG_GRN:    DB   "GREEN",0 
MSG_QAD:    DB   " QUADRANT  " 
MSG_PQD:    DB   "   ",0 
MSG_SCT:    DB   " SECTOR    " 
MSG_SC1:    DB   "   ",0 
MSG_ENR:    DB   " ENERGY       " 
MSG_ENP:    DB   " ",0 
MSG_TRP:    DB   " TORPEDOES  " 
MSG_TPP:    DB   " ",0 
MSG_SHD:    DB   " SHIELDS      " 
MSG_SHP:    DB   " ",0 
MSG_CMD:    DB   CR,LF,"COMMAND?",0 
MSG_CRS:    DB   CR,LF,"COURSE (1-8.5)? ",0 
MSG_WRP:    DB   CR,LF,"WARP FACTOR (0.1-7.7)? ",0 
MSG_LRS:    DB   CR,LF,"L.R. SCAN FOR",0 
MSG_MSF:    DB   CR,LF,"MISSION FAILED, YOU HAVE RUN OUT OF STARDATES",0 
MSG_KAB:    DB   CR,LF,"KA-BOOM, YOU CRASHED INTO A STAR. YOUR SHIP IS DESTROYED",0 
MSG_YMO:    DB   CR,LF,"YOU MOVED OUT OF THE GALAXY, YOUR SHIP IS LOST..LOST",0 
MSG_LOE:    DB   CR,LF,"LOSS OF ENERGY    " 
MSG_LOP:    DB   " ",0 
MSG_DSE:    DB   CR,LF,"DANGER-SHIELD ENERGY 000",0 
MSG_SET:    DB   CR,LF,"SHIELD ENERGY TRANSFER = ",0 
MSG_NEE:    DB   CR,LF,"NOT ENOUGH ENERGY",0 
MSG_TTY:    DB   CR,LF,"TORPEDO TRAJECTORY(1-8.5) : ",0 
MSG_ASD:    DB   CR,LF,"ALIEN SHIP DESTROYED",0 
MSG_YMA:    DB   CR,LF,"YOU MISSED! ALIEN SHIP RETALIATES",0 
MSG_SSD:    DB   CR,LF,"SPACE STATION ",215 
MSG_DES:    DB   "DESTROYED",0 
MSG_CYH:    DB   CR,LF,"CONGRATULATIONS, YOU HAVE ELIMINATED ALL OF THE ALIEN SHIPS",0 
MSG_TRG:    DB   CR,LF,"TRACKING: " 
MSG_TRK:    DB   "   ",0 
MSG_GDY:    DB   CR,LF,"GALAXY DISPLAY",0 
MSG_PEF:    DB   CR,LF,"PHASOR ENERGY TO FIRE = ",0 
MSG_ASF:    DB   CR,LF,"ALIEN SHIP AT SECTOR " 
MSG_SEC:    DB   " , : ",0 
MSG_EGY:    DB   "ENERGY =    " 
MSG_DEY:    DB   " ",0 
MSG_NAS:    DB   CR,LF,"NO ALIEN SHIPS! WASTED SHOT" 
MSG_ZRO     DB   000 
MSG_NEL:    DB   CR,LF,"ABANDON SHIP! NO ENERGY LEFT",0 
MSG_NTS:    DB   CR,LF,"NO TORPEDOES",0 
MSG_111:    DB   CR,LF,"1 " 
MSG_11a:    DB   "    1 " 
MSG_11b:    DB   "    1 " 
MSG_11c:    DB   "    1",0 
MSG_LST:    DB   CR,LF,"LAST",0 
MSG_CHK:    DB   CR,LF,"CHICKEN!",0 
 
                                 ;ORG (5*400)+000 
            org 0500H 
MSG:        mov a,m              ;fetch character 
            ana a                ;end of message? 
            rz                   ;yes, return 
            call PRINT           ;no, print character 
            call INMEM           ;increment message pointer 
            jmp MSG              ;continue printout 
 
INMEM:      inr l                ;increment low address 
            rnz                  ;if non-zero, return 
            inr h                ;else, increment page address 
            ret 
 
RN:         mvi l,40H            ;set random number pointer 
            mvi h,00H 
            mov a,m              ;the random number 
            mov b,a              ;   is generated by performing 
            rlc                  ;   the series of arithmetic 
            xra b                ;   operations presented 
            rrc 
            inr l 
            mov b,m 
            inr b 
            mov m,b 
            add b 
            dcr l 
            mov m,a              ;save random number 
            ret 
 
SSPLS:      mvi e,0F7H 
            jmp PLS 
 
SSMNS:      mvi e,08H 
            jmp MNS 
 
ASPLS:      mvi e,0CFH 
PLS:        call RN 
            ori 0C0H 
            mov l,a 
            mov a,e 
            ana m 
            mov m,a 
            jmp GLXCK 
 
ASMNS:      mvi e,10H 
MNS:        call RN 
            ori 0C0H 
            mov l,a 
            mov a,e 
            ora m 
            mov m,a 
            jmp GLXCK 
 
DIGPRT:     mov a,m 
            adi 0B0H 
            call INMEM 
            call SWITCH 
            mov m,a 
            call DCMEM 
            dcr b 
            rz 
            call SWITCH 
            jmp DIGPRT 
 
DCMEM:      dcr l 
            inr l                ;low address = 0? 
            jnz LODCR            ;no, decrement low address only 
            dcr h                ;yes, decrement page address 
LODCR:      dcr l                ;descrement low address 
            ret 
 
SWITCH:     mov c,l              ;save low address no. 1 
            mov l,e              ;move low address no. 2 
            mov e,c              ;save low address no. 1 
            mov c,h              ;save page address no. 1 
            mov h,d              ;move page address no. 2 
            mov d,c              ;save page address no. 1 
            ret 
 
BINDEC:     call SWITCH          ;save binary pointer 
            mvi l,60H            ;set pointer to digit storage 
            mvi h,00H 
            mov m,h              ;clear digit table 
            inr l 
            mov m,h 
            inr l 
            mov m,h 
            inr l 
            mov m,h 
            inr l 
            mov m,h 
            call SWITCH          ;set pointer to binary number 
            mov e,m              ;fetch least significant half 
            dcr b                ;single precision? 
            jz BNDC              ;yes, most significant half = 0 
            inr l                ;no, advance pointer 
            mov d,m              ;fetch most significant half 
BNDC:       mvi l,64H            ;set pointer to 5th digit 
            mvi h,00H 
            mvi c,10H            ;least significant half of 10000 
            mvi b,27H            ;most significant half of 10000 
            call BD              ;calculate 5th digit 
            dcr l                ;set pointer to 4th digit 
            mvi c,0E8H           ;least significant half of 1000 
            mvi b,03H            ;most significant half of 1000 
            call BD              ;calculate 4th digit 
            dcr l                ;set pointer to 3rd digit 
            mvi c,64H            ;least significant half of 100 
            mvi b,00H            ;most significant half of 100 
            call BD              ;calculate 3rd digit 
            dcr l                ;set pointer to 2nd digit 
            mvi c,0AH            ;least significant half of 10 
            call BD              ;calculate 2nd digit 
            dcr l                ;set pointer to 1st digit 
            mov m,e              ;store 1st digit 
            ret                  ;return to calling program 
 
BD:         mov a,m              ;fetch decimal digit 
            adi 01H              ;increment and 
            mov m,a              ;   save new digit 
            mov a,e              ;fetch least significant half 
            sub c                ;subtract least significant constant 
            mov e,a              ;save least significant half 
            mov a,d              ;fetch most significant half 
            sbb b                ;subtract most significant constant 
            mov d,a              ;save most significant half 
            jnc BD               ;if greater than 0, continue calculation 
            mov a,e              ;else, restore binary and decimal constant 
            add c                ;add least significant constant 
            mov e,a              ;save least significant half 
            mov a,d              ;fetch most significant half 
            adc b                ;add most significant constant 
            mov d,a              ;save most significant half 
            mov c,m              ;fetch digit 
            dcr c                ;decrement digit stored 
            mov m,c              ;save digit in table 
            ret                  ;return 
 
LOAD:       mvi l,4FH 
            mvi m,88H 
            inr l 
            mvi m,13H 
            inr l 
            mov m,h 
            inr l 
            mov m,h 
            mvi l,5AH 
            mvi m,0AH 
            ret 
 
ROTR4:      rrc                  ;rotate accumulator right 
ROTR3:      rrc 
            rrc 
            rrc 
            ret 
 
LOCSET:     call RN 
            ani 3FH 
            mov b,a 
            call MATCH 
            jz LOCSET 
            mov l,e 
            mov m,b 
            inr e 
            dcr c 
            jnz LOCSET 
            ret 
 
ROWSET:     mvi l,MSG_STDT2&0FFH 
            mvi h,MSG_STDT2/100H 
RCLR:       mvi m,0A0H 
            inr l 
            mvi a,0A7H 
            cmp l 
            jnz RCLR 
            mov a,c 
            adi 0B0H 
            mvi l,MSG_STDT1&0FFH 
            mov m,a 
            dcr c 
            mvi h,00H 
            mvi l,43H 
            call RWPNT 
            jnz STR 
            mvi m,274 
            inr l 
            mvi m,252 
            inr l 
            mvi m,276 
STR:        mvi l,104 
STR1:       mvi h,000 
            call RWPNT 
            jnz NXSTR 
            inr l 
            mvi m,252 
            mov l,e 
NXSTR:      inr l 
            mvi a,113 
            cmp l 
            jnz STR1 
            mvi h,000 
            call RWPNT 
            jnz AS 
            mvi m,276 
            inr l 
            mvi m,261 
            inr l 
            mvi m,274 
AS:         mvi l,114 
AS1:        mvi h,000 
            call RWPNT 
            jnz NXAS 
            mvi m,253 
            inr l 
            mvi m,253 
            inr l 
            mvi m,253 
            mov l,e 
NXAS:       inr l 
            mvi a,117 
            cmp l 
            jnz AS1 
            mvi h,MSG_STDT/400 
            mvi l,MSG_STDT&377 
            jmp CMSG 
 
RWPNT:      mov a,m 
            ana a 
            rm 
            call ROTR3 
            ani 007 
            cmp c 
            rnz 
            mov a,m 
            ani 007 
            mov b,a 
            rlc 
            add b 
            adi MSG_STDT2&377 
            mov e,l 
            mov l,a 
            mvi h,MSG_STDT2/400 
            xra a 
            ana a 
            ret 
RED:        mvi m,322 
            inr l 
            mvi m,305 
            inr l 
            mvi m,304 
            inr l 
            mvi m,000 
            jmp CND 
 
QUAD:       mvi l,131 
            mvi h,000 
            mvi e,MSG_PQD&377 
            mvi d,MSG_PQD/400 
            call TWO 
            mvi l,MSG_QAD&377 
            jmp MSG 
 
TWO:        mov a,m 
            mov b,a 
            call SWITCH 
T1:         call ROTR3 
            ani 007 
            adi 261 
            mov m,a 
            mov a,b 
            ani 007 
            adi 261 
            call INMEM 
            call INMEM 
            mov m,a 
            ret 
 
FNUM:       mov a,m              ; fetch number 
            cpi 260              ; is number valid? 
            rm                   ; no, return with sign bit set 
            sui 272              ; yes, return with sign bit reset 
            adi 200 
            ret 
 
NTN:        mvi h,023 
NT1:        mvi a,CR 
            call PRINT 
            mvi a,LF 
            call PRINT 
NT2:        mvi a,'-' 
            call PRINT 
            dcr h 
            jnz NT2 
            ret 
 
LRR:        adi 300 
            mov b,a 
            ani 007 
            jz CLC1 
            mov a,b 
            sui 001 
            mov l,a 
            mov a,m 
LR3:        mvi l,MSG_11a&377 
            call QDS1 
            mov l,b 
            mvi h,000 
            mov a,m 
            mvi l,MSG_11b&377 
            call QDS1 
            mov a,b 
            ani 007 
            cpi 007 
            jz CLC2 
            mov a,b 
            adi 001 
            mov l,a 
            mvi h,000 
            mov a,m 
LR4:        mvi l,MSG_11c&377 
            call QDS1 
LRP:        mvi l,MSG_111&377 
            mvi h,MSG_111/400 
            jmp MSG 
 
QDS1:       mvi h,MSG_111/400 
QDSET:      mov c,a 
            call ROTR4 
            ani 003 
            ori 260 
            mov m,a 
            call INMEM 
            mov a,c 
            call ROTR3 
            ani 001 
            ori 260 
            mov m,a 
            call INMEM 
            mov a,c 
            ani 007 
            ori 260 
            mov m,a 
            ret 
 
CLC1:       xra a 
            jmp LR3 
CLC2:       xra a 
            jmp LR4 
 
RWCM:       mvi l,136 
            mov a,m 
            rrc 
            ani 007 
            mov b,a 
            inr l 
            mov a,m 
            rlc 
            rlc 
            ani 070 
            add b 
            mov b,a 
            ret 
 
TIMER:      mvi l,MSG_MSF&377 
            mvi h,MSG_MSF/400 
DONE:       call MSG 
            jmp GALAXY 
 
LOST:       mvi l,MSG_YMO&377 
            mvi h,MSG_YMO/400 
            jmp DONE 
 
WPOUT:      mvi l,MSG_KAB&377 
            mvi h,MSG_KAB/400 
            jmp DONE 
 
EOUT:       mvi l,MSG_NEL&377 
            mvi h,MSG_NEL/400 
            jmp DONE 
 
NWQD:       mvi l,104            ;set pointer to star table 
            mvi e,013            ;set number of entries 
CLR:        mvi m,200            ;store terminate entry 
            inr l                ;   to clear table 
            dcr e                ;table clearer? 
            jnz CLR              ;no, clear more 
            mvi l,102            ;set pointer to quadrant contents 
            mov a,m              ;fetch quadrant contents 
            ani 007              ;fetch number of stars 
            mov c,a              ;save in C 
            mvi e,104            ;set pointer to star table 
            cnz LOCSET           ;set up star locations 
            mvi l,102            ;pointer to quadrant contents 
            mov a,m              ;fetch quadrant contents 
            call ROTR3           ;move to space station position 
            ani 001              ;isolate space station entry 
            mov c,a              ;save in C 
            mvi e,113            ;set pointer to space station table 
            cnz LOCSET           ;if space station present, set position 
            mvi l,102            ;pointer to quadrant contents 
            mov a,m              ;fetch quadrant contents 
            call ROTR4           ;move to alien ship position 
            ani 003              ;isolate alien ship entry 
            mov c,a              ;save in C 
            mvi e,114            ;set pointer to alien ship table 
            cnz LOCSET           ;if alien ship present, set position 
LDAS:       call RN              ;fetch random number for alien ship energy 
            mvi l,123            ;set pointer to alien ship no. 1 energy 
            call LAS             ;store alien ship no. 1 energy 
            mvi l,125            ;set pointer to alien ship no. 2 
            call LAS             ;store alien ship no. 2 energy 
            mvi l,127            ;set pointer to alien ship no. 3 
            jmp LAS              ;store alien ship no. 3 energy and return 
 
LAS:        mov m,a              ;store least significant half 
            ani 003              ;form most significant half 
            inr l 
            mov m,a              ;store more significant half 
            jmp RN               ;fetch next random number and return 
 
MATCH:      mvi l,104 
SCK:        mov a,m 
            ana a 
            jm NS 
            cmp b 
            rz 
            inr l 
            mvi a,113 
            cmp l 
            jnz SCK 
NS:         mvi l,113 
            mov a,m 
            cmp b 
            rz 
ACK:        inr l 
            mov a,m 
            cmp b 
            rz 
            mov a,l 
            cpi 116 
            jnz ACK 
            ana a 
            ret 
 
ELOS:       mvi l,062 
            mov m,e 
            inr l 
            mov m,d 
            dcr l 
            mvi b,002 
            call BINDEC 
            mvi d,MSG_LOP/400 
            mvi e,MSG_LOP&377 
            mvi b,004 
            call DIGPRT 
            mvi l,MSG_LOE&377 
            mvi h,MSG_LOE/400 
            call CMSG 
            mvi l,062 
            mov e,m 
            inr l 
            mov d,m 
ELS1:       call CKSD 
            jnc FMSD 
            mov e,m 
            inr l 
            mov d,m 
            call FMSD 
            call TOMN 
            mvi l,062 
            mov e,m 
            inr l 
            mov d,m 
SD0:        call CKMN 
            jc EOUT 
            call FMMN 
            mvi l,MSG_DSE&377 
            mvi h,MSG_DSE/400 
            call CMSG 
            mvi b,002 
            call DVD 
            call CKMN 
            jc EOUT 
            jmp FMMN 
 
ELOM:       call CKMN 
            jnc FMMN 
            mov c,e 
            mov b,d 
            mvi l,121 
            mov e,m 
            inr l 
            mov d,m 
            call FMSD 
            call TOMN 
            mov e,c 
            mov d,b 
            jmp SD0 
DLET:       mvi m,200 
            mov b,l 
            mvi l,131 
            mov a,m 
            adi 300 
            mov l,a 
            mov a,b 
            cpi 113 
            jnz DLAS 
            mov a,m 
            ani 067 
            mov m,a 
            mvi l,102 
            mov m,a 
            mvi l,133 
            mov b,m 
            dcr b 
            mov m,b 
            rnz 
            mvi l,MSG_DSE&377 
            mvi h,MSG_DSE/400 
CMSG:       call MSG 
            mvi h,000 
            ret 
 
DLAS:       mov a,m 
            sui 020 
            mov m,a 
            mvi l,102 
            mov m,a 
            mvi l,134 
            mov b,m 
            dcr b 
            mov m,b 
            rnz 
            mvi l,MSG_CYH&377 
            mvi h,MSG_CYH/400 
            jmp DONE 
 
DRCT:       call INPUT           ;input first course number 
            mvi l,136            ;pointer to temporary storage 
            mvi h,000 
            cpi '1'+80H          ;is input less than 1? 
            jc ZRET              ;yes, illegal input 
            cpi '9'+80H          ;is input greater than 8? 
            jnc ZRET             ;yes, illegal input 
            ani 017              ;mask off ascii code 
            rlc                  ;times 2 
            mov m,a              ;save in temporary storage 
            mvi a,'.'            ;print a decimal point 
            call PRINT 
            call INPUT 
            cpi '0'+80H 
            jz CR1 
            cpi '5'+80H 
            jnz ZRET 
CR1:        ani 001 
            add m 
            rlc 
            sui 004 
            mov m,a 
            rnz 
            adi 001 
            ret 
ZRET:       xra a 
            ret 
 
QCNT:       mvi h,000 
            mvi l,131 
            mov a,m 
            adi 300 
            mov l,a 
            mov a,m 
            mvi l,102 
            mov m,a 
            ret 
 
ACTV:       mvi l,136 
            mov l,m 
            mov c,m 
            inr l 
            mov d,m 
            mvi l,103 
            mov a,m 
            mov b,a 
            ani 007 
            mvi l,136 
            rlc 
            mov m,a 
            inr l 
            mov a,b 
            ani 070 
            rrc 
            rrc 
            mov m,a 
            ret 
 
TRK:        mvi l,060 
            mov m,h 
            mvi l,136 
            mov a,m 
            add c 
            mov m,a 
            jp NOBK 
            ani 017 
            mov m,a 
            mvi l,060 
            mov m,l 
            mvi l,131 
            mov a,m 
            ani 007 
            rz 
            mov b,m 
            dcr b 
            mov m,b 
            jmp RMV 
 
NOBK:       cpi 020 
            jc RMV 
            ani 017 
            mov m,a 
            mvi l,060 
            mov m,l 
            mvi l,131 
            mov a,m 
            ani 007 
            adi 001 
            cpi 010 
            rz 
            mov b,m 
            inr b 
            mov m,b 
RMV:        mvi l,137 
            mov a,m 
            add d 
            mov m,a 
            jp NOUP 
            ani 017 
            mov m,a 
            mvi l,060 
            mov m,l 
            mvi l,131 
            mov a,m 
            ani 070 
            rz 
            mov a,m 
            sui 010 
            mov m,a 
            jmp CKX 
 
NOUP:       cpi 020 
            jc CKX 
            ani 017 
            mov m,a 
            mvi l,060 
            mov m,l 
            mvi l,131 
            mov a,m 
            ani 070 
            adi 010 
            cpi 100 
            rz 
            mov a,m 
            adi 010 
            mov m,a 
CKX:        mvi l,050 
            mov m,e 
            inr l 
            mov m,d 
            inr l 
            mov m,c 
            rnz 
            mvi a,001 
            ana a 
            ret 
 
DVD:        ana a 
            mov a,d 
            rar 
            mov d,a 
            mov a,e 
            rar 
            mov e,a 
            dcr b 
            jnz DVD 
            ret 
 
WASTE:      call ELOM 
            mvi l,MSG_NAS&377 
            mvi h,MSG_NAS/400 
            call MSG 
            jmp CMND 
 
EIN:        mvi h,000            ;set pointer to sign indicator 
            mvi l,144 
            mov m,h              ;clear sign indicator 
            mvi l,143            ;set pointer to input table 
            call INPUT           ;input 1st character 
            cpi '-'+80H          ;negative sign? 
            jnz EN2              ;no, check digit 
            inr l                ;yes, advance pointer to sign indicator 
            mov m,l              ;set sign indicator to non-zero 
            dcr l                ;reset table pointer 
EN1:        call INPUT           ;input digit 
EN2:        mov m,a              ;save digit in table 
            call FNUM            ;valid digit? 
            rm                   ;no, return with sign flag set 
            mov a,m              ;yes, fetch digit 
            ani 017              ;mask off ASCII code 
            mov m,a              ;save binary value 
            dcr l                ;move table pointer 
            mvi a,137            ;is the table pointer 
            cmp l                ;  indicating table fill? 
            rz                   ;yes, return with sign flag reset 
            jmp EN1              ;no, input more digits 
 
DCBN:       mvi l,140            ;fetch units digit 
            mov a,m 
            dcr l                ;move pointer to temporary storage 
            mov m,h              ;set temporary storage to 
            dcr l                ;   value of units digit 
            mov m,a 
            mvi l,141            ;fetch tens digit 
            mov a,m 
            ana a                ;is tens digit = 0? 
            jz DC1               ;yes, do hundreds digit 
            mov b,a              ;save tens digit 
            mvi e,012            ;set up binary value 
            mov d,h              ;   of 10 in E and D 
            call TOBN            ;add 10 X digit 
DC1:        mvi l,142            ;fetch hundreds digit 
            mov a,m 
            ana a                ;is hundreds digit = 0? 
            jz DC2               ;yes, finish 
            mov b,a              ;save hundreds digit 
            mvi e,144            ;set up binary value 
            mov d,h              ;   of 100 in E and D 
            call TOBN            ;add 100 X digit 
DC2:        mvi l,143            ;fetch thousands digit 
            mov a,m 
            ana a                ;is thousands digit = 0; 
            jz DC3               ;yes, set binary value in E and D 
            mov b,a              ;save thousands digit 
            mvi e,350            ;set up binary value of 
            mvi d,003            ;   100 in E and D 
            call TOBN            ;add 1000 X digit 
DC3:        mvi l,136            ;set pointer to binary value 
            mov e,m              ;fetch least significant half 
            inr l 
            mov d,m              ;fetch most significant half 
            ret 
 
TOBN:       mvi l,136 
            call TO1 
            dcr b 
            rz 
            jmp TOBN 
 
TOMN:       mvi l,117 
TO1:        mov a,m 
            add e 
            mov m,a 
            inr l 
            mov a,m 
            adc d 
            mov m,a 
            ret 
TOSD:       mvi l,121 
            jmp TO1 
 
FMMN:       mvi l,117 
FM1:        mov a,m 
            sub e 
            mov m,a 
            inr l 
            mov a,m 
            sbb d 
            mov m,a 
            ret 
FMSD:       mvi l,121 
            jmp FM1 
 
CKMN:       mvi l,120 
CK1:        mov a,m 
            dcr l 
            cmp d 
            rnz 
CK2:        mov a,m 
            cmp e 
            ret 
 
CKSD:       mvi l,122 
            jmp CK1 
 
OVER:       mvi l,MSG_CHK&377 
            mvi h,MSG_CHK/400 
            call MSG 
            HLT 
 
SPRC:       mov a,m 
            ani 007 
            mov c,a 
            mov a,m 
            call ROTR3 
            ani 007 
            mov b,a 
            ret 
 
                                 ;ORG (12*400)+000 
            org 0A00H 
GALAXY:     mvi l,MSG_DYW&377 
            mvi h,MSG_DYW/400 
            call MSG 
START:      call RN 
            call INPCK 
            jp START 
            call INPUT 
            cpi 'N'+80H 
            jz OVER 
            mvi e,300 
GLXSET:     call RN 
            ani 177 
            mov l,a 
            mvi h,017 
            mov a,m 
            mov l,e 
            mvi h,000 
            mov m,a 
            inr e 
            jnz GLXSET 
GLXCK:      mov d,h 
            mov c,h 
            mvi l,300 
GLXCK1:     mov a,m 
            ani 010 
            add d 
            mov d,a 
            mov a,m 
            ani 060 
            rrc 
            rrc 
            add c 
            mov c,a 
            inr l 
            jnz GLXCK1 
            mov a,d 
            rrc 
            rrc 
            rrc 
            mov d,a 
            cpi 007 
            jnc SSPLS 
            cpi 002 
            jc SSMNS 
            mov a,c 
            rrc 
            rrc 
            mov c,a 
            cpi 040 
            jnc ASPLS 
            cpi 012 
            jc ASMNS 
            mvi l,133 
            mov m,d 
            inr l 
            mov m,c 
            mov a,c 
            adi 005 
            inr l 
            mov m,a 
            mvi b,001 
            call BINDEC 
            mvi d,MSG_DTS/400 
            mvi e,MSG_DTS&377 
            mvi b,002 
            call DIGPRT 
            mvi l,134 
            mvi h,000 
            mvi b,001 
            call BINDEC 
            mvi d,MSG_SPS/400 
            mvi e,MSG_SPS&377 
            mvi b,002 
            call DIGPRT 
            mvi l,133 
            mvi h,000 
            mov a,m 
            ori 260 
            mvi h,MSG_SSS/400 
            mvi l,MSG_SSS&377 
            mov m,a 
            mvi l,MSG_YJD&377 
            mvi h,MSG_YJD/400 
            call MSG 
            call RN 
            ani 077 
            mvi l,131 
            mov m,a 
            call QCNT 
            call LOAD 
            call NWQD 
            mvi c,001 
            mvi e,103 
            call LOCSET 
SRSCN:      mvi l,MSG_123&377 
            mvi h,MSG_123/400 
            call MSG 
            mvi c,001 
            call ROWSET 
            mvi l,135 
            mvi h,000 
            mvi a,062 
            sub m 
            inr l 
            mov m,a 
            mvi b,001 
            call BINDEC 
            mvi d,MSG_SDP/400 
            mvi e,MSG_SDP&377 
            mvi b,002 
            call DIGPRT 
            mvi l,MSG_STDT3&377 
            mvi h,MSG_STDT3/400 
            call MSG 
            mvi c,002 
            call ROWSET 
            mvi l,102 
            mov a,m 
            mvi l,MSG_GRN&377 
            mvi h,MSG_GRN/400 
            ani 060 
            jnz RED 
            mvi m,307 
            inr l 
            mvi m,322 
            inr l 
            mvi m,305 
            inr l 
            mvi m,305 
            inr l 
            mvi m,316 
CND:        mvi l,MSG_CND&377 
            call MSG 
            mvi c,003 
            call ROWSET 
            call QUAD 
            mvi c,004 
            call ROWSET 
            mvi l,103 
            mvi e,MSG_SC1&377 
            inr d 
            call TWO 
            mvi l,MSG_SCT&377 
            call MSG 
            mvi c,005 
            call ROWSET 
            mvi l,117 
            mvi b,002 
            call BINDEC 
            mvi d,MSG_ENP/400 
            mvi e,MSG_ENP&377 
            mvi b,004 
            call DIGPRT 
            mvi l,MSG_ENR&377 
            mvi h,MSG_ENR/400 
            call MSG 
            mvi c,006 
            call ROWSET 
            mvi l,132 
            mvi b,001 
            call BINDEC 
            mvi d,MSG_TPP/400 
            mvi e,MSG_TPP&377 
            mvi b,002 
            call DIGPRT 
            mvi l,MSG_TRP&377 
            mvi h,MSG_TRP/400 
            call MSG 
            mvi c,007 
            call ROWSET 
            mvi l,121 
            mvi b,002 
            call BINDEC 
            mvi d,MSG_SHP/400 
            mvi e,MSG_SHP&377 
            mvi b,004 
            call DIGPRT 
            mvi l,MSG_SHD&377 
            mvi h,MSG_SHD/400 
            call MSG 
            mvi c,010 
            call ROWSET 
            mvi l,MSG_123&377 
            mvi h,MSG_123/400 
            call MSG 
CMND:       mvi h,000 
            mvi e,012 
            mov d,h 
            call ELOM 
            mvi l,101 
            mov e,m 
            inr e 
            mov m,e 
CMD:        mvi l,MSG_CMD&377 
            mvi h,MSG_CMD/400 
            call CMSG 
            call INPUT 
            cpi '0'+80H 
            jz CRSE 
            cpi '1'+80H 
            jz SRSCN 
            cpi '2'+80H 
            jz LRSCN 
            cpi '3'+80H 
            jz GXPRT 
            cpi '4'+80H 
            jz SHEN 
            cpi '5'+80H 
            jz PHSR 
            cpi '6'+80H 
            jz TRPD 
            jmp CMD 
 
LRSCN:      mvi l,MSG_LRS&377 
            mvi h,MSG_LRS/400 
            call MSG 
            call QUAD 
            call NTN 
            mvi l,131 
            mov a,m 
            ani 070 
            jz RWC1 
            mov a,m 
            sui 010 
            call LRR 
LR1:        call NTN 
            mvi l,131 
            mov a,m 
            call LRR 
            call NTN 
            mvi l,131 
            mov a,m 
            cpi 070 
            jnc RWC2 
            adi 010 
            call LRR 
LR2:        call NTN 
            jmp CMND 
 
RWC1:       call RWC 
            jmp LR1 
 
RWC2:       call RWC 
            jmp LR2 
RWC:        mvi l,MSG_11a&377    ;311 
            xra a 
            call QDS1 
            mvi l,MSG_11b&377    ;317 
            xra a 
            call QDS1 
            mvi l,MSG_11c&377    ;325 
            xra a 
            call QDS1 
            jmp LRP 
 
CRSE:       mvi l,MSG_CRS&377    ;040 
            mvi h,MSG_CRS/400    ;002 
            call MSG 
            call DRCT 
            jz CRSE 
WRP:        mvi l,MSG_WRP&377    ;063 
            mvi h,MSG_WRP/400    ;002 
            call CMSG 
            mvi l,137 
            call INPUT 
            cpi '0'+80H 
            jc WRP 
            cpi '8'+80H 
            jnc WRP 
            ani 007 
            rlc 
            rlc 
            rlc 
            mov m,a 
            mvi a,'.' 
            call PRINT 
            call INPUT 
            cpi '0'+80H 
            jc WRP 
            cpi '8'+80H 
            jnc WRP 
            ani 007 
            add m 
            jz WRP 
            mov e,a 
            call ACTV 
            mvi l,061 
            mov m,h 
MOV:        call TRK 
            jz LOST 
            mvi l,060 
            mov a,m 
            ana a 
            jz CLSN 
            inr l 
            mov m,l 
            mvi e,031 
            mov d,h 
            call ELOM 
            call QCNT 
            call NWQD 
CLSN:       call RWCM 
            call MATCH 
            jnz MVDN 
            mov b,l 
            mov a,b 
            cpi 113 
            mvi l,061 
            mov a,m 
            jz SSOUT 
            jnc ASOUT 
            ana a 
            jz WPOUT 
MVDN:       mvi h,000 
            mvi l,050 
            mov e,m 
            inr l 
            mov d,m 
            inr l 
            mov c,m 
            dcr e 
            jnz MOV 
            mvi l,061 
            mov a,m 
            ana a 
            jz NOX 
            mvi l,135 
            mov b,m 
            dcr b 
            jz TIMER 
            mov m,b 
NOX:        call RWCM 
            mvi l,103 
            mov m,b 
            call MATCH 
            cz CHNG 
            call DKED 
            jmp SRSCN 
 
SSOUT:      ana a 
            jnz MVDN 
            mov l,b 
            call DLET 
            mvi e,130 
            mvi d,002 
SSO1:       call ELOS 
            jmp MVDN 
 
ASOUT:      ana a 
            jnz MVDN 
            mov l,b 
            call DLET 
            mvi e,334 
            mvi d,005 
            jmp SSO1 
 
CHNG:       mov e,l 
            mvi c,001 
            jmp LOCSET 
 
DKED:       mvi l,113 
            mov a,m 
            ana a 
            rm 
            mov a,b 
            ani 070 
            mov c,a 
            mov a,b 
            ani 007 
            mov b,a 
            mov a,m 
            ani 007 
            mov e,a 
            mov a,m 
            ani 070 
            cmp c 
            rnz 
            mov a,b 
            adi 001 
            cmp e 
            jz LOAD 
            sui 002 
            cmp e 
            rnz 
            jmp LOAD 
 
SHEN:       mvi l,MSG_SET&377 
            mvi h,MSG_SET/400 
            call MSG 
            call EIN 
            jm SHEN 
            call DCBN 
            mvi l,144 
            mov a,m 
            ana a 
            jz POS 
            call CKSD 
            jc NE 
            call FMSD 
            call TOMN 
            jmp CMND 
 
POS:        call CKMN 
            jc NE 
            call FMMN 
            call TOSD 
            jmp CMND 
 
NE:         mvi l,MSG_NEE&377 
            mvi h,MSG_NEE/400 
            call MSG 
            jmp CMND 
 
TRPD:       mvi l,132 
            mov a,m 
            ana a 
            jz NTPD 
            mvi e,372 
            mov d,h 
            call CKMN 
            jc NE 
            call FMMN 
            mvi l,132 
            mov a,m 
            sui 001 
            mov m,a 
TR1:        mvi l,MSG_TTY&377 
            mvi h,MSG_TTY/400 
            call MSG 
            call DRCT 
            jz TR1 
            call ACTV 
            mvi l,131 
            mov a,m 
            mvi l,053 
            mov m,a 
TR2:        call TRK 
            jz QOUT 
            mvi l,060 
            mov a,m 
            ana a 
            jnz QOUT 
            call RWCM 
            mov c,b 
            mvi l,MSG_TRK&377 
            mvi h,MSG_TRK/400 
            call T1 
            mvi l,022 
            call CMSG 
            mov b,c 
            call MATCH 
            jz HIT 
            mvi l,050 
            mov e,m 
            inr l 
            mov d,m 
            inr l 
            mov c,m 
            jmp TR2 
 
HIT:        mov a,l 
            cpi 113 
            jc QOUT 
            jz SSTA 
            call DLET 
            mvi l,MSG_ASD&377 
            mvi h,MSG_ASD/400 
            call MSG 
            jmp CMND 
 
SSTA:       call DLET 
            mvi l,MSG_SSD&377 
            mvi h,MSG_SSD/400 
            call MSG 
QOUT:       mvi l,MSG_YMA&377 
            mvi h,MSG_YMA/400 
            call CMSG 
            mvi e,310 
            mov d,h 
            call ELOS 
            mvi l,053 
            mov a,m 
            mvi l,131 
            mov m,a 
            jmp CMND 
 
NTPD:       mvi l,MSG_ZRO&377 
            mvi h,MSG_ZRO/400 
            call MSG 
            jmp CMND 
 
PHSR:       mvi l,MSG_PEF&377 
            mvi h,MSG_PEF/400 
            call MSG 
            call EIN 
            jm PHSR 
            call DCBN 
            call ELOM 
            mvi l,102 
            mov a,m 
            ani 060 
            jz WASTE 
            call ROTR4 
            sui 001 
            jz PH1 
            mov b,a 
            call DVD 
PH1:        mvi l,136 
            mov m,e 
            inr l 
            mov m,d 
            mvi l,050 
            mov m,e 
            inr l 
            mov m,d 
            inr l 
            mvi m,114 
            call ASPH 
            mvi l,052 
            mvi m,115 
            call ASPH 
            mvi l,052 
            mvi m,116 
            call ASPH 
            jmp CMND 
 
ASPH:       mov l,m 
            mov a,m 
            ana a 
            rm 
            mvi e,MSG_SEC&377 
            mvi d,MSG_SEC/400 
            call TWO 
            mvi l,116 
            call CMSG 
            mvi l,103 
            call SPRC 
            mov l,e 
            mov h,d 
            mov e,c 
            mov d,b 
            call SPRC 
            mov a,b 
            sub d 
            jp PH2 
            xri 377 
            adi 001 
PH2:        mov b,a 
            mov a,c 
            sub e 
            jp PH3 
            xri 377 
            adi 001 
PH3:        add b 
            rrc 
            rrc 
            ani 003 
            mov b,a 
            mov c,l 
            mvi l,050 
            mov e,m 
            inr l 
            mov d,m 
            dcr b 
            inr b 
            cnz DVD 
            mov a,c 
            ani 003 
            rlc 
            adi 123 
            mvi l,053 
            mov m,a 
            mov l,a 
            call FM1 
            jm DSTR 
            jnz ALOS 
            dcr l 
            mov a,m 
            inr l 
            ana a 
            jz DSTR 
ALOS:       dcr l 
            mvi b,002 
            call BINDEC 
            mvi e,MSG_DEY&377 
            mvi d,MSG_DEY/400 
            mvi b,004 
            call DIGPRT 
            mvi l,MSG_EGY&377 
            mvi h,MSG_EGY/400 
            call CMSG 
            mvi l,053 
            mov l,m 
            mov e,m 
            inr l 
            mov d,m 
            mvi b,002 
            call DVD 
            jmp ELOS 
 
DSTR:       mvi l,MSG_DES&377 
            mvi h,MSG_DES/400 
            call CMSG 
            mvi l,052 
            mov l,m 
            jmp DLET 
 
GXPRT:      mvi l,MSG_GDY&377 
            mvi h,MSG_GDY/400 
            call MSG 
            mvi h,061 
            call NT1 
            mvi l,300 
GL1:        mov d,h 
            mvi e,204 
GL2:        mov a,m 
            call SWITCH 
            call QDSET 
            mov a,l 
            adi 004 
            mov l,a 
            call SWITCH 
            inr l 
            cpi 264 
            jnz GL2 
            call SWITCH 
            mvi l,200 
            call MSG 
            mvi h,061 
            call NT1 
            mov a,e 
            cmp h 
            jz CMND 
            call SWITCH 
            jmp GL1 
 
; sets sign flag if character coming in 
INPCK:      in 0                 ; get input from serial port 
            rar                  ; rotate the received serial bit right into carry 
            jc INPCK2            ; jump if start bit not detected 
            call CINP2           ; finish character so we don't get garbage next call 
            xra a                ; clear sign, next instruction will complement it before returning 
INPCK2:     xri 200              ; complement MSB to set sign 0= character not coming in 
            ret 
 
                                 ;ORG (17*400)+000 
            org 0F00H 
            DB    000,001,004,043,012,003,007,000 
            DB    000,032,043,005,003,024,026,022 
            DB    000,000,000,000,000,005,004,027 
            DB    005,001,024,000,000,004,005,000 
            DB    007,002,021,011,000,004,000,000 
            DB    043,000,002,044,000,000,003,007 
            DB    000,025,000,005,014,000,002,006 
            DB    025,000,003,002,023,000,064,003 
            DB    007,001,000,000,000,003,025,000 
            DB    000,004,000,037,004,001,003,002 
            DB    003,024,000,000,000,026,015,000 
            DB    000,004,023,003,000,000,000,024 
            DB    013,001,025,023,000,000,004,003 
            DB    007,000,000,000,035,004,000,026 
            DB    000,023,025,000,000,004,006,002 
            DB    003,025,000,000,026,000,047,000 
 
 
            cpu 8008new          ; use "new" 8008 mnemonics 
INPORT      equ 0                ; serial input port address 
OUTPORT     equ 08h              ; serial output port address 
;----------------------------------------------------------------------------------------- 
; 2400 bps character input subroutine. 
; wait for a character from the serial port. echo the character. 
; return the character in A with the most significant bit set. 
; uses A and B. 
;----------------------------------------------------------------------------------------- 
INPUT:      in INPORT            ; get input from serial port 
            rar                  ; rotate the received serial bit right into carry 
            jc INPUT             ; jump if start bit detected 
 
                                 ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit 
CINP2:      mvi b,0              ; initialize B 
            mvi b,0              ; tweak timing 
            xra a                ; clear the accumulator 
            out OUTPORT          ; send the start bit 
 
            call getbit          ; bit 0 
            call getbit          ; bit 1 
            call getbit          ; bit 2 
            call getbit          ; bit 3 
            call getbit          ; bit 4 
            call getbit          ; bit 5 
            call getbit          ; bit 6 
            call getbit          ; bit 7 
 
                                 ; wait 104 cycles, then send the stop bit 
            mov a,b              ; save the character (now in B) in A 
            mvi b,0fch 
            call delay 
            mov b,a              ; retrieve the character from A to B 
            mvi a,1 
            out OUTPORT          ; send the stop bit 
                                 ; wait 104 cycles. 
            mov a,b              ; restore the character to A from B 
            ori 80h              ; set the most significant bit 
            mvi b,0fch 
            call delay 
            ret                  ; return to caller with the character in A 
 
                                 ; 92 cycles 
getbit:     mov a,b              ; 5 save B in A 
            mvi b,0ffh           ; 8 
            mvi b,0ffh           ; 8 
            call delay           ; 11+19 
            mov b,a              ; 5 restore B from A 
            in INPORT            ; 8 get input from the serial port 
            out OUTPORT          ; 8 echo the received bit 
            rar                  ; 5 rotate the received bit right into carry 
            mov a,b              ; 5 restore the previously received bits 
            rar                  ; 5 rotate the newly received bit in carry right into the MSB of A 
            mov b,a              ; 5 save the received bits in B 
            ret                  ; 5 
 
;------------------------------------------------------------------------ 
; 2400 bps character output subroutine. 
; uses A and B. 
;------------------------------------------------------------------------ 
PRINT:      ani 7fh              ; clear the most signficant bit 
            mov b,a              ; save the character in B 
            xra a                ; clear A for the start bit 
 
            out OUTPORT          ; send the start bit 
            mov a,b              ; restore the character to A 
                                 ;mov a,b                 ; timing adjustment 
            mvi b,0fch 
            call delay 
 
            call outbit          ; bit 0 
            call outbit          ; bit 1 
            call outbit          ; bit 2 
            call outbit          ; bit 3 
            call outbit          ; bit 4 
            call outbit          ; bit 5 
            call outbit          ; bit 6 
            call outbit          ; bit 7 
 
                                 ;send the stop bit 
            mov b,a              ; save the character in B 
            mvi a,1              ; stop bit 
            out OUTPORT          ; send the stop bit 
            mov a,b              ; restore the character from B to A 
            mvi b,0fch 
            call delay 
            ret                  ; return to caller 
 
outbit:     out OUTPORT          ; output the least significant bit 
            mvi b,0fdh 
            call delay 
            ana a                ; timing adjustment 
            rrc                  ; shift A right 
            ret 
 
            end 0A00H 
 
