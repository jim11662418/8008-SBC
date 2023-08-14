        PAGE 0                  ; suppress page headings in AS listing file
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
; jump to 0A00H to start
;=============================================================================

            cpu 8008                ; use "old" 8008 mnemonics
            radix 8                 ; use octal for numbers

CR          EQU   0DH
LF          EQU   0AH

            ;ORG (0*400)+000
            org 0000H
            DB    002           ;Course 1.0
            DB    000
            DB    002           ;Course 1.5
            DB    377
            DB    002           ;Course 2.0
            DB    376
            DB    001           ;Course 2.5
            DB    376
            DB    000           ;Course 3.0
            DB    376
            DB    377           ;Course 3.5
            DB    376
            DB    376           ;Course 4.0
            DB    376
            DB    376           ;Course 4.5
            DB    377
            DB    376           ;Course 5.0
            DB    000
            DB    376           ;Course 5.5
            DB    001
            DB    376           ;Course 6.0
            DB    002
            DB    377           ;Course 6.5
            DB    002
            DB    000           ;Course 7.0
            DB    002
            DB    001           ;Course 7.5
            DB    002
            DB    002           ;Course 8.0
            DB    002
            DB    002           ;Course 8.5
            DB    001

            ;ORG (0*400)+050
            org 0028H
            DB    000           ;Register storage
            DB    000           ;Register storage
            DB    000           ;Register storage
            DB    000           ;Termporary storage

            ;ORG (0*400)+060
            org 0030H
            DB    000           ;Crossing flag
            DB    000           ;Crossing indicator
            DB    000           ;Temporary storage
            DB    000           ;Termporary storage

            ;ORG (0*400)+100
            org 0040H
            DB    000           ;100 Random number storage
            DB    000           ;101 Random number storage
            DB    000           ;102 Current quadrant contents
            DB    000           ;103 Sector location of space ship
            DB    000           ;104 Sector location of star
            DB    000           ;105 Sector location of star
            DB    000           ;106 Sector location of star
            DB    000           ;107 Sector location of star
            DB    000           ;110 Sector location of star
            DB    000           ;111 Sector location of star
            DB    000           ;112 Sector location of star
            DB    000           ;113 Sector location of space station
            DB    000           ;114 Sector location of alien ship no.1
            DB    000           ;115 Sector location of alien ship no.2
            DB    000           ;116 Sector location of alien ship. no.3
            DB    000           ;117 Double precision value of main energy least significant half
            DB    000           ;120 Double precision value of main energy most significant half
            DB    000           ;121 Double precision value of shield energy least significant half
            DB    000           ;122 Double precision value of shield energy most significant half
            DB    000           ;123 Double precision value of alien ship no. 1 energy least significant half
            DB    000           ;124 Double precision value of alien ship no. 1 energy most significant half
            DB    000           ;125 Double precision value of alien ship no. 2 energy least significant half
            DB    000           ;126 Double precision value of alien ship no. 2 energy most significant half
            DB    000           ;127 Double precision value of alien ship no. 3 energy least significant half
            DB    000           ;130 Double precision value of alien ship no. 3 energy most significant half
            DB    000           ;131 Current quadrant location of space ship
            DB    000           ;132 Number of torpedoes remaining
            DB    000           ;133 Number of space stations
            DB    000           ;134 Number of alien ships
            DB    000           ;135 Number of stardates remaining
            DB    000           ;136 Temporary storage
            DB    000           ;137 Temporary storage
            DB    000           ;140 Digit storage
            DB    000           ;141 Digit storage
            DB    000           ;142 Digit storage
            DB    000           ;143 Digit storage
            DB    000           ;144 Digit storage
            
; delay subroutine used by serial input/output functions
            cpu 8008new
delay:      inr b
            jnz delay
            ret
            cpu 8008

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
MSG:        LAM                 ;fetch character
            NDA                 ;end of message?
            RTZ                 ;yes, return
            CAL PRINT           ;no, print character
            CAL INMEM           ;increment message pointer
            JMP MSG             ;continue printout
            
INMEM:      INL                 ;increment low address
            RFZ                 ;if non-zero, return
            INH                 ;else, increment page address
            RET

RN:         LLI 40H             ;set random number pointer
            LHI 00H
            LAM                 ;the random number 
            LBA                 ;   is generated by performing
            RLC                 ;   the series of arithmetic
            XRB                 ;   operations presented
            RRC
            INL
            LBM
            INB
            LMB
            ADB
            DCL
            LMA                 ;save random number
            RET

SSPLS:      LEI 0F7H
            JMP PLS

SSMNS:      LEI 08H
            JMP MNS

ASPLS:      LEI 0CFH
PLS:        CAL RN
            ORI 0C0H
            LLA
            LAE
            NDM
            LMA
            JMP GLXCK

ASMNS:      LEI 10H
MNS:        CAL RN
            ORI 0C0H
            LLA
            LAE
            ORM
            LMA
            JMP GLXCK

DIGPRT:     LAM
            ADI 0B0H
            CAL INMEM
            CAL SWITCH
            LMA
            CAL DCMEM
            DCB
            RTZ
            CAL SWITCH
            JMP DIGPRT
            
DCMEM:      DCL
            INL                     ;low address = 0?
            JFZ LODCR               ;no, decrement low address only
            DCH                     ;yes, decrement page address
LODCR:      DCL                     ;descrement low address
            RET

SWITCH:     LCL                     ;save low address no. 1
            LLE                     ;move low address no. 2
            LEC                     ;save low address no. 1
            LCH                     ;save page address no. 1
            LHD                     ;move page address no. 2
            LDC                     ;save page address no. 1
            RET

BINDEC:     CAL SWITCH              ;save binary pointer
            LLI 60H                 ;set pointer to digit storage
            LHI 00H
            LMH                     ;clear digit table
            INL
            LMH
            INL
            LMH
            INL
            LMH
            INL
            LMH
            CAL SWITCH              ;set pointer to binary number
            LEM                     ;fetch least significant half
            DCB                     ;single precision?
            JTZ BNDC                ;yes, most significant half = 0
            INL                     ;no, advance pointer
            LDM                     ;fetch most significant half
BNDC:       LLI 64H                 ;set pointer to 5th digit
            LHI 00H
            LCI 10H                 ;least significant half of 10000
            LBI 27H                 ;most significant half of 10000
            CAL BD                  ;calculate 5th digit
            DCL                     ;set pointer to 4th digit
            LCI 0E8H                ;least significant half of 1000
            LBI 03H                 ;most significant half of 1000
            CAL BD                  ;calculate 4th digit
            DCL                     ;set pointer to 3rd digit
            LCI 64H                 ;least significant half of 100
            LBI 00H                 ;most significant half of 100
            CAL BD                  ;calculate 3rd digit
            DCL                     ;set pointer to 2nd digit
            LCI 0AH                 ;least significant half of 10
            CAL BD                  ;calculate 2nd digit
            DCL                     ;set pointer to 1st digit
            LME                     ;store 1st digit
            RET                     ;return to calling program

BD:         LAM                     ;fetch decimal digit
            ADI 01H                 ;increment and
            LMA                     ;   save new digit
            LAE                     ;fetch least significant half
            SUC                     ;subtract least significant constant
            LEA                     ;save least significant half
            LAD                     ;fetch most significant half
            SBB                     ;subtract most significant constant
            LDA                     ;save most significant half
            JFC BD                  ;if greater than 0, continue calculation
            LAE                     ;else, restore binary and decimal constant
            ADC                     ;add least significant constant
            LEA                     ;save least significant half
            LAD                     ;fetch most significant half
            ACB                     ;add most significant constant
            LDA                     ;save most significant half
            LCM                     ;fetch digit
            DCC                     ;decrement digit stored
            LMC                     ;save digit in table
            RET                     ;return

LOAD:       LLI 4FH
            LMI 88H
            INL
            LMI 13H
            INL
            LMH
            INL
            LMH
            LLI 5AH
            LMI 0AH
            RET

ROTR4:      RRC                     ;rotate accumulator right
ROTR3:      RRC
            RRC
            RRC
            RET

LOCSET:     CAL RN
            NDI 3FH
            LBA
            CAL MATCH
            JTZ LOCSET
            LLE
            LMB
            INE
            DCC
            JFZ LOCSET
            RET

ROWSET:     LLI MSG_STDT2&0FFH
            LHI MSG_STDT2/100H
RCLR:       LMI 0A0H
            INL
            LAI 0A7H
            CPL
            JFZ RCLR
            LAC
            ADI 0B0H
            LLI MSG_STDT1&0FFH
            LMA
            DCC
            LHI 00H
            LLI 43H
            CAL RWPNT
            JFZ STR
            LMI 274
            INL
            LMI 252
            INL
            LMI 276
STR:        LLI 104
STR1:       LHI 000
            CAL RWPNT
            JFZ NXSTR
            INL
            LMI 252
            LLE
NXSTR:      INL
            LAI 113
            CPL
            JFZ STR1
            LHI 000
            CAL RWPNT
            JFZ AS
            LMI 276
            INL
            LMI 261
            INL
            LMI 274
AS:         LLI 114
AS1:        LHI 000
            CAL RWPNT
            JFZ NXAS
            LMI 253
            INL
            LMI 253
            INL
            LMI 253
            LLE
NXAS:       INL
            LAI 117
            CPL
            JFZ AS1
            LHI MSG_STDT/400
            LLI MSG_STDT&377
            JMP CMSG

RWPNT:      LAM
            NDA
            RTS
            CAL ROTR3
            NDI 007
            CPC
            RFZ
            LAM
            NDI 007
            LBA
            RLC
            ADB
            ADI MSG_STDT2&377
            LEL
            LLA
            LHI MSG_STDT2/400
            XRA
            NDA
            RET
RED:        LMI 322
            INL
            LMI 305
            INL
            LMI 304
            INL
            LMI 000
            JMP CND

QUAD:       LLI 131
            LHI 000
            LEI MSG_PQD&377
            LDI MSG_PQD/400
            CAL TWO
            LLI MSG_QAD&377
            JMP MSG

TWO:        LAM
            LBA
            CAL SWITCH
T1:         CAL ROTR3
            NDI 007
            ADI 261
            LMA
            LAB
            NDI 007
            ADI 261
            CAL INMEM
            CAL INMEM
            LMA
            RET

FNUM:       LAM                 ; fetch number
            CPI 260             ; is number valid?
            RTS                 ; no, return with sign bit set
            SUI 272             ; yes, return with sign bit reset
            ADI 200
            RET

NTN:        LHI 023
NT1:        LAI CR
            CAL PRINT
            LAI LF
            CAL PRINT
NT2:        LAI '-'
            CAL PRINT
            DCH
            JFZ NT2
            RET

LRR:        ADI 300
            LBA
            NDI 007
            JTZ CLC1
            LAB
            SUI 001
            LLA
            LAM
LR3:        LLI MSG_11a&377
            CAL QDS1
            LLB
            LHI 000
            LAM
            LLI MSG_11b&377
            CAL QDS1
            LAB
            NDI 007
            CPI 007
            JTZ CLC2
            LAB
            ADI 001
            LLA
            LHI 000
            LAM
LR4:        LLI MSG_11c&377
            CAL QDS1
LRP:        LLI MSG_111&377
            LHI MSG_111/400
            JMP MSG

QDS1:       LHI MSG_111/400
QDSET:      LCA
            CAL ROTR4
            NDI 003
            ORI 260
            LMA
            CAL INMEM
            LAC
            CAL ROTR3
            NDI 001
            ORI 260
            LMA
            CAL INMEM
            LAC
            NDI 007
            ORI 260
            LMA
            RET

CLC1:       XRA
            JMP LR3
CLC2:       XRA
            JMP LR4

RWCM:       LLI 136
            LAM
            RRC
            NDI 007
            LBA
            INL
            LAM
            RLC
            RLC
            NDI 070
            ADB
            LBA
            RET

TIMER:      LLI MSG_MSF&377
            LHI MSG_MSF/400
DONE:       CAL MSG
            JMP GALAXY

LOST:       LLI MSG_YMO&377
            LHI MSG_YMO/400
            JMP DONE

WPOUT:      LLI MSG_KAB&377
            LHI MSG_KAB/400
            JMP DONE

EOUT:       LLI MSG_NEL&377
            LHI MSG_NEL/400
            JMP DONE

NWQD:       LLI 104             ;set pointer to star table
            LEI 013             ;set number of entries
CLR:        LMI 200             ;store terminate entry
            INL                 ;   to clear table
            DCE                 ;table clearer?
            JFZ CLR             ;no, clear more
            LLI 102             ;set pointer to quadrant contents
            LAM                 ;fetch quadrant contents
            NDI 007             ;fetch number of stars
            LCA                 ;save in C
            LEI 104             ;set pointer to star table
            CFZ LOCSET          ;set up star locations
            LLI 102             ;pointer to quadrant contents
            LAM                 ;fetch quadrant contents
            CAL ROTR3           ;move to space station position
            NDI 001             ;isolate space station entry
            LCA                 ;save in C
            LEI 113             ;set pointer to space station table
            CFZ LOCSET          ;if space station present, set position
            LLI 102             ;pointer to quadrant contents
            LAM                 ;fetch quadrant contents
            CAL ROTR4           ;move to alien ship position
            NDI 003             ;isolate alien ship entry
            LCA                 ;save in C
            LEI 114             ;set pointer to alien ship table
            CFZ LOCSET          ;if alien ship present, set position
LDAS:       CAL RN              ;fetch random number for alien ship energy
            LLI 123             ;set pointer to alien ship no. 1 energy
            CAL LAS             ;store alien ship no. 1 energy
            LLI 125             ;set pointer to alien ship no. 2
            CAL LAS             ;store alien ship no. 2 energy
            LLI 127             ;set pointer to alien ship no. 3
            JMP LAS             ;store alien ship no. 3 energy and return

LAS:        LMA                 ;store least significant half
            NDI 003             ;form most significant half
            INL
            LMA                 ;store more significant half
            JMP RN              ;fetch next random number and return

MATCH:      LLI 104
SCK:        LAM
            NDA
            JTS NS
            CPB
            RTZ
            INL
            LAI 113
            CPL
            JFZ SCK
NS:         LLI 113
            LAM
            CPB
            RTZ
ACK:        INL
            LAM
            CPB
            RTZ
            LAL
            CPI 116
            JFZ ACK
            NDA
            RET

ELOS:       LLI 062
            LME
            INL
            LMD
            DCL
            LBI 002
            CAL BINDEC
            LDI MSG_LOP/400
            LEI MSG_LOP&377
            LBI 004
            CAL DIGPRT
            LLI MSG_LOE&377
            LHI MSG_LOE/400
            CAL CMSG
            LLI 062
            LEM
            INL
            LDM
ELS1:       CAL CKSD
            JFC FMSD
            LEM
            INL
            LDM
            CAL FMSD
            CAL TOMN
            LLI 062
            LEM
            INL
            LDM
SD0:        CAL CKMN
            JTC EOUT
            CAL FMMN
            LLI MSG_DSE&377
            LHI MSG_DSE/400
            CAL CMSG
            LBI 002
            CAL DVD
            CAL CKMN
            JTC EOUT
            JMP FMMN

ELOM:       CAL CKMN
            JFC FMMN
            LCE
            LBD
            LLI 121
            LEM
            INL
            LDM
            CAL FMSD
            CAL TOMN
            LEC
            LDB
            JMP SD0
DLET:       LMI 200
            LBL
            LLI 131
            LAM
            ADI 300
            LLA
            LAB
            CPI 113
            JFZ DLAS
            LAM
            NDI 067
            LMA
            LLI 102
            LMA
            LLI 133
            LBM
            DCB
            LMB
            RFZ
            LLI MSG_DSE&377
            LHI MSG_DSE/400
CMSG:       CAL MSG
            LHI 000
            RET

DLAS:       LAM
            SUI 020
            LMA
            LLI 102
            LMA
            LLI 134
            LBM
            DCB
            LMB
            RFZ
            LLI MSG_CYH&377
            LHI MSG_CYH/400
            JMP DONE

DRCT:       CAL INPUT       ;input first course number
            LLI 136         ;pointer to temporary storage
            LHI 000
            CPI '1'+80H     ;is input less than 1?
            JTC ZRET        ;yes, illegal input
            CPI '9'+80H     ;is input greater than 8?
            JFC ZRET        ;yes, illegal input
            NDI 017         ;mask off ascii code
            RLC             ;times 2
            LMA             ;save in temporary storage
            LAI '.'         ;print a decimal point
            CAL PRINT
            CAL INPUT
            CPI '0'+80H
            JTZ CR1
            CPI '5'+80H
            JFZ ZRET
CR1:        NDI 001
            ADM
            RLC
            SUI 004
            LMA
            RFZ
            ADI 001
            RET
ZRET:       XRA
            RET

QCNT:       LHI 000
            LLI 131
            LAM
            ADI 300
            LLA
            LAM
            LLI 102
            LMA
            RET

ACTV:       LLI 136
            LLM
            LCM
            INL
            LDM
            LLI 103
            LAM
            LBA
            NDI 007
            LLI 136
            RLC
            LMA
            INL
            LAB
            NDI 070
            RRC
            RRC
            LMA
            RET

TRK:        LLI 060
            LMH
            LLI 136
            LAM
            ADC
            LMA
            JFS NOBK
            NDI 017
            LMA
            LLI 060
            LML
            LLI 131
            LAM
            NDI 007
            RTZ
            LBM
            DCB
            LMB
            JMP RMV

NOBK:       CPI 020
            JTC RMV
            NDI 017
            LMA
            LLI 060
            LML
            LLI 131
            LAM
            NDI 007
            ADI 001
            CPI 010
            RTZ
            LBM
            INB
            LMB
RMV:        LLI 137
            LAM
            ADD
            LMA
            JFS NOUP
            NDI 017
            LMA
            LLI 060
            LML
            LLI 131
            LAM
            NDI 070
            RTZ
            LAM
            SUI 010
            LMA
            JMP CKX

NOUP:       CPI 020
            JTC CKX
            NDI 017
            LMA
            LLI 060
            LML
            LLI 131
            LAM
            NDI 070
            ADI 010
            CPI 100
            RTZ
            LAM
            ADI 010
            LMA
CKX:        LLI 050
            LME
            INL
            LMD
            INL
            LMC
            RFZ
            LAI 001
            NDA
            RET

DVD:        NDA
            LAD
            RAR
            LDA
            LAE
            RAR
            LEA
            DCB
            JFZ DVD
            RET

WASTE:      CAL ELOM
            LLI MSG_NAS&377
            LHI MSG_NAS/400
            CAL MSG
            JMP CMND

EIN:        LHI 000             ;set pointer to sign indicator
            LLI 144
            LMH                 ;clear sign indicator
            LLI 143             ;set pointer to input table
            CAL INPUT           ;input 1st character
            CPI '-'+80H         ;negative sign?
            JFZ EN2             ;no, check digit
            INL                 ;yes, advance pointer to sign indicator
            LML                 ;set sign indicator to non-zero
            DCL                 ;reset table pointer
EN1:        CAL INPUT           ;input digit
EN2:        LMA                 ;save digit in table
            CAL FNUM            ;valid digit?
            RTS                 ;no, return with sign flag set
            LAM                 ;yes, fetch digit
            NDI 017             ;mask off ASCII code
            LMA                 ;save binary value
            DCL                 ;move table pointer
            LAI 137             ;is the table pointer
            CPL                 ;  indicating table fill?
            RTZ                 ;yes, return with sign flag reset
            JMP EN1             ;no, input more digits

DCBN:       LLI 140             ;fetch units digit
            LAM
            DCL                 ;move pointer to temporary storage
            LMH                 ;set temporary storage to
            DCL                 ;   value of units digit
            LMA
            LLI 141             ;fetch tens digit
            LAM
            NDA                 ;is tens digit = 0?
            JTZ DC1             ;yes, do hundreds digit
            LBA                 ;save tens digit
            LEI 012             ;set up binary value
            LDH                 ;   of 10 in E and D
            CAL TOBN            ;add 10 X digit
DC1:        LLI 142             ;fetch hundreds digit
            LAM
            NDA                 ;is hundreds digit = 0?
            JTZ DC2             ;yes, finish
            LBA                 ;save hundreds digit
            LEI 144             ;set up binary value
            LDH                 ;   of 100 in E and D
            CAL TOBN            ;add 100 X digit
DC2:        LLI 143             ;fetch thousands digit
            LAM
            NDA                 ;is thousands digit = 0;
            JTZ DC3             ;yes, set binary value in E and D
            LBA                 ;save thousands digit
            LEI 350             ;set up binary value of
            LDI 003             ;   100 in E and D
            CAL TOBN            ;add 1000 X digit
DC3:        LLI 136             ;set pointer to binary value
            LEM                 ;fetch least significant half
            INL
            LDM                 ;fetch most significant half
            RET

TOBN:       LLI 136
            CAL TO1
            DCB
            RTZ
            JMP TOBN

TOMN:       LLI 117
TO1:        LAM
            ADE
            LMA
            INL
            LAM
            ACD
            LMA
            RET
TOSD:       LLI 121
            JMP TO1

FMMN:       LLI 117
FM1:        LAM
            SUE
            LMA
            INL
            LAM
            SBD
            LMA
            RET
FMSD:       LLI 121
            JMP FM1

CKMN:       LLI 120
CK1:        LAM
            DCL
            CPD
            RFZ
CK2:        LAM
            CPE
            RET

CKSD:       LLI 122
            JMP CK1

OVER:       LLI MSG_CHK&377
            LHI MSG_CHK/400
            CAL MSG
            HLT

SPRC:       LAM
            NDI 007
            LCA
            LAM
            CAL ROTR3
            NDI 007
            LBA
            RET

            ;ORG (12*400)+000
            org 0A00H
GALAXY:     LLI MSG_DYW&377
            LHI MSG_DYW/400
            CAL MSG
START:      CAL RN
            CAL INPCK
            JFS START
            CAL INPUT
            CPI 'N'+80H
            JTZ OVER
            LEI 300
GLXSET:     CAL RN
            NDI 177
            LLA
            LHI 017
            LAM
            LLE
            LHI 000
            LMA
            INE
            JFZ GLXSET
GLXCK:      LDH
            LCH
            LLI 300
GLXCK1:     LAM
            NDI 010
            ADD
            LDA
            LAM
            NDI 060
            RRC
            RRC
            ADC
            LCA
            INL
            JFZ GLXCK1
            LAD
            RRC
            RRC
            RRC
            LDA
            CPI 007
            JFC SSPLS
            CPI 002
            JTC SSMNS
            LAC
            RRC
            RRC
            LCA
            CPI 040
            JFC ASPLS
            CPI 012
            JTC ASMNS
            LLI 133
            LMD
            INL
            LMC
            LAC
            ADI 005
            INL
            LMA
            LBI 001
            CAL BINDEC
            LDI MSG_DTS/400
            LEI MSG_DTS&377
            LBI 002
            CAL DIGPRT
            LLI 134
            LHI 000
            LBI 001
            CAL BINDEC
            LDI MSG_SPS/400
            LEI MSG_SPS&377
            LBI 002
            CAL DIGPRT
            LLI 133
            LHI 000
            LAM
            ORI 260
            LHI MSG_SSS/400
            LLI MSG_SSS&377
            LMA
            LLI MSG_YJD&377
            LHI MSG_YJD/400
            CAL MSG
            CAL RN
            NDI 077
            LLI 131
            LMA
            CAL QCNT
            CAL LOAD
            CAL NWQD
            LCI 001
            LEI 103
            CAL LOCSET
SRSCN:      LLI MSG_123&377
            LHI MSG_123/400
            CAL MSG
            LCI 001
            CAL ROWSET
            LLI 135
            LHI 000
            LAI 062
            SUM
            INL
            LMA
            LBI 001
            CAL BINDEC
            LDI MSG_SDP/400
            LEI MSG_SDP&377
            LBI 002
            CAL DIGPRT
            LLI MSG_STDT3&377
            LHI MSG_STDT3/400
            CAL MSG
            LCI 002
            CAL ROWSET
            LLI 102
            LAM
            LLI MSG_GRN&377
            LHI MSG_GRN/400
            NDI 060
            JFZ RED
            LMI 307
            INL
            LMI 322
            INL
            LMI 305
            INL
            LMI 305
            INL
            LMI 316
CND:        LLI MSG_CND&377
            CAL MSG
            LCI 003
            CAL ROWSET
            CAL QUAD
            LCI 004
            CAL ROWSET
            LLI 103
            LEI MSG_SC1&377
            IND
            CAL TWO
            LLI MSG_SCT&377
            CAL MSG
            LCI 005
            CAL ROWSET
            LLI 117
            LBI 002
            CAL BINDEC
            LDI MSG_ENP/400
            LEI MSG_ENP&377
            LBI 004
            CAL DIGPRT
            LLI MSG_ENR&377
            LHI MSG_ENR/400
            CAL MSG
            LCI 006
            CAL ROWSET
            LLI 132
            LBI 001
            CAL BINDEC
            LDI MSG_TPP/400
            LEI MSG_TPP&377
            LBI 002
            CAL DIGPRT
            LLI MSG_TRP&377
            LHI MSG_TRP/400
            CAL MSG
            LCI 007
            CAL ROWSET
            LLI 121
            LBI 002
            CAL BINDEC
            LDI MSG_SHP/400
            LEI MSG_SHP&377
            LBI 004
            CAL DIGPRT
            LLI MSG_SHD&377
            LHI MSG_SHD/400
            CAL MSG
            LCI 010
            CAL ROWSET
            LLI MSG_123&377
            LHI MSG_123/400
            CAL MSG
CMND:       LHI 000
            LEI 012
            LDH
            CAL ELOM
            LLI 101
            LEM
            INE
            LME
CMD:        LLI MSG_CMD&377
            LHI MSG_CMD/400
            CAL CMSG
            CAL INPUT
            CPI '0'+80H
            JTZ CRSE
            CPI '1'+80H
            JTZ SRSCN
            CPI '2'+80H
            JTZ LRSCN
            CPI '3'+80H
            JTZ GXPRT
            CPI '4'+80H
            JTZ SHEN
            CPI '5'+80H
            JTZ PHSR
            CPI '6'+80H
            JTZ TRPD
            JMP CMD

LRSCN:      LLI MSG_LRS&377
            LHI MSG_LRS/400
            CAL MSG
            CAL QUAD
            CAL NTN
            LLI 131
            LAM
            NDI 070
            JTZ RWC1
            LAM
            SUI 010
            CAL LRR
LR1:        CAL NTN
            LLI 131
            LAM
            CAL LRR
            CAL NTN
            LLI 131
            LAM
            CPI 070
            JFC RWC2
            ADI 010
            CAL LRR
LR2:        CAL NTN
            JMP CMND

RWC1:       CAL RWC
            JMP LR1

RWC2:       CAL RWC
            JMP LR2
RWC:        LLI MSG_11a&377 ;311
            XRA
            CAL QDS1
            LLI MSG_11b&377 ;317
            XRA
            CAL QDS1
            LLI MSG_11c&377 ;325
            XRA
            CAL QDS1
            JMP LRP

CRSE:       LLI MSG_CRS&377 ;040
            LHI MSG_CRS/400 ;002
            CAL MSG
            CAL DRCT
            JTZ CRSE
WRP:        LLI MSG_WRP&377 ;063
            LHI MSG_WRP/400 ;002
            CAL CMSG
            LLI 137
            CAL INPUT
            CPI '0'+80H
            JTC WRP
            CPI '8'+80H
            JFC WRP
            NDI 007
            RLC
            RLC
            RLC
            LMA
            LAI '.'
            CAL PRINT
            CAL INPUT
            CPI '0'+80H
            JTC WRP
            CPI '8'+80H
            JFC WRP
            NDI 007
            ADM
            JTZ WRP
            LEA
            CAL ACTV
            LLI 061
            LMH
MOV:        CAL TRK
            JTZ LOST
            LLI 060
            LAM
            NDA
            JTZ CLSN
            INL
            LML
            LEI 031
            LDH
            CAL ELOM
            CAL QCNT
            CAL NWQD
CLSN:       CAL RWCM
            CAL MATCH
            JFZ MVDN
            LBL
            LAB
            CPI 113
            LLI 061
            LAM
            JTZ SSOUT
            JFC ASOUT
            NDA
            JTZ WPOUT
MVDN:       LHI 000
            LLI 050
            LEM
            INL
            LDM
            INL
            LCM
            DCE
            JFZ MOV
            LLI 061
            LAM
            NDA
            JTZ NOX
            LLI 135
            LBM
            DCB
            JTZ TIMER
            LMB
NOX:        CAL RWCM
            LLI 103
            LMB
            CAL MATCH
            CTZ CHNG
            CAL DKED
            JMP SRSCN

SSOUT:      NDA
            JFZ MVDN
            LLB
            CAL DLET
            LEI 130
            LDI 002
SSO1:       CAL ELOS
            JMP MVDN

ASOUT:      NDA
            JFZ MVDN
            LLB
            CAL DLET
            LEI 334
            LDI 005
            JMP SSO1

CHNG:       LEL
            LCI 001
            JMP LOCSET

DKED:       LLI 113
            LAM
            NDA
            RTS
            LAB
            NDI 070
            LCA
            LAB
            NDI 007
            LBA
            LAM
            NDI 007
            LEA
            LAM
            NDI 070
            CPC
            RFZ
            LAB
            ADI 001
            CPE
            JTZ LOAD
            SUI 002
            CPE
            RFZ
            JMP LOAD

SHEN:       LLI MSG_SET&377
            LHI MSG_SET/400
            CAL MSG
            CAL EIN
            JTS SHEN
            CAL DCBN
            LLI 144
            LAM
            NDA
            JTZ POS
            CAL CKSD
            JTC NE
            CAL FMSD
            CAL TOMN
            JMP CMND

POS:        CAL CKMN
            JTC NE
            CAL FMMN
            CAL TOSD
            JMP CMND

NE:         LLI MSG_NEE&377
            LHI MSG_NEE/400
            CAL MSG
            JMP CMND

TRPD:       LLI 132
            LAM
            NDA
            JTZ NTPD
            LEI 372
            LDH
            CAL CKMN
            JTC NE
            CAL FMMN
            LLI 132
            LAM
            SUI 001
            LMA
TR1:        LLI MSG_TTY&377
            LHI MSG_TTY/400
            CAL MSG
            CAL DRCT
            JTZ TR1
            CAL ACTV
            LLI 131
            LAM
            LLI 053
            LMA
TR2:        CAL TRK
            JTZ QOUT
            LLI 060
            LAM
            NDA
            JFZ QOUT
            CAL RWCM
            LCB
            LLI MSG_TRK&377
            LHI MSG_TRK/400
            CAL T1
            LLI 022
            CAL CMSG
            LBC
            CAL MATCH
            JTZ HIT
            LLI 050
            LEM
            INL
            LDM
            INL
            LCM
            JMP TR2

HIT:        LAL
            CPI 113
            JTC QOUT
            JTZ SSTA
            CAL DLET
            LLI MSG_ASD&377
            LHI MSG_ASD/400
            CAL MSG
            JMP CMND

SSTA:       CAL DLET
            LLI MSG_SSD&377
            LHI MSG_SSD/400
            CAL MSG
QOUT:       LLI MSG_YMA&377
            LHI MSG_YMA/400
            CAL CMSG
            LEI 310
            LDH
            CAL ELOS
            LLI 053
            LAM
            LLI 131
            LMA
            JMP CMND

NTPD:       LLI MSG_ZRO&377
            LHI MSG_ZRO/400
            CAL MSG
            JMP CMND

PHSR:       LLI MSG_PEF&377
            LHI MSG_PEF/400
            CAL MSG
            CAL EIN
            JTS PHSR
            CAL DCBN
            CAL ELOM
            LLI 102
            LAM
            NDI 060
            JTZ WASTE
            CAL ROTR4
            SUI 001
            JTZ PH1
            LBA
            CAL DVD
PH1:        LLI 136
            LME
            INL
            LMD
            LLI 050
            LME
            INL
            LMD
            INL
            LMI 114
            CAL ASPH
            LLI 052
            LMI 115
            CAL ASPH
            LLI 052
            LMI 116
            CAL ASPH
            JMP CMND

ASPH:       LLM
            LAM
            NDA
            RTS
            LEI MSG_SEC&377
            LDI MSG_SEC/400
            CAL TWO
            LLI 116
            CAL CMSG
            LLI 103
            CAL SPRC
            LLE
            LHD
            LEC
            LDB
            CAL SPRC
            LAB
            SUD
            JFS PH2
            XRI 377
            ADI 001
PH2:        LBA
            LAC
            SUE
            JFS PH3
            XRI 377
            ADI 001
PH3:        ADB
            RRC
            RRC
            NDI 003
            LBA
            LCL
            LLI 050
            LEM
            INL
            LDM
            DCB
            INB
            CFZ DVD
            LAC
            NDI 003
            RLC
            ADI 123
            LLI 053
            LMA
            LLA
            CAL FM1
            JTS DSTR
            JFZ ALOS
            DCL
            LAM
            INL
            NDA
            JTZ DSTR
ALOS:       DCL
            LBI 002
            CAL BINDEC
            LEI MSG_DEY&377
            LDI MSG_DEY/400
            LBI 004
            CAL DIGPRT
            LLI MSG_EGY&377
            LHI MSG_EGY/400
            CAL CMSG
            LLI 053
            LLM
            LEM
            INL
            LDM
            LBI 002
            CAL DVD
            JMP ELOS

DSTR:       LLI MSG_DES&377
            LHI MSG_DES/400
            CAL CMSG
            LLI 052
            LLM
            JMP DLET

GXPRT:      LLI MSG_GDY&377
            LHI MSG_GDY/400
            CAL MSG
            LHI 061
            CAL NT1
            LLI 300
GL1:        LDH
            LEI 204
GL2:        LAM
            CAL SWITCH
            CAL QDSET
            LAL
            ADI 004
            LLA
            CAL SWITCH
            INL
            CPI 264
            JFZ GL2
            CAL SWITCH
            LLI 200
            CAL MSG
            LHI 061
            CAL NT1
            LAE
            CPH
            JTZ CMND
            CAL SWITCH
            JMP GL1

; sets sign flag if character coming in
INPCK:      INP 0                   ; get input from serial port
            RAR                     ; rotate the received serial bit right into carry
            JTC INPCK2              ; jump if start bit not detected
            CAL CINP2               ; finish character so we don't get garbage next call
            XRA                     ; clear sign, next instruction will complement it before returning
INPCK2:     XRI 200                 ; complement MSB to set sign 0= character not coming in
            RET

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


            cpu 8008new             ; use "new" 8008 mnemonics
INPORT      equ 0                   ; serial input port address
OUTPORT     equ 08h                 ; serial output port address
;-----------------------------------------------------------------------------------------
; 2400 bps character input subroutine.
; wait for a character from the serial port. echo the character. 
; return the character in A with the most significant bit set.
; uses A and B.
;-----------------------------------------------------------------------------------------
INPUT:      in INPORT               ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc INPUT                ; jump if start bit detected

            ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit
CINP2:      mvi b,0                 ; initialize B
            mvi b,0                 ; tweak timing
            xra a                   ; clear the accumulator
            out OUTPORT             ; send the start bit

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
            out OUTPORT             ; send the stop bit
            ; wait 104 cycles.
            mov a,b                 ; restore the character to A from B
            ori 80h                 ; set the most significant bit
            mvi b,0fch
            call delay
            ret                     ; return to caller with the character in A

            ; 92 cycles
getbit:     mov a,b                 ; 5 save B in A
            mvi b,0ffh              ; 8
            mvi b,0ffh              ; 8 
            call delay              ; 11+19
            mov b,a                 ; 5 restore B from A
            in INPORT               ; 8 get input from the serial port
            out OUTPORT             ; 8 echo the received bit
            rar                     ; 5 rotate the received bit right into carry
            mov a,b                 ; 5 restore the previously received bits
            rar                     ; 5 rotate the newly received bit in carry right into the MSB of A
            mov b,a                 ; 5 save the received bits in B
            ret                     ; 5

;------------------------------------------------------------------------
; 2400 bps character output subroutine.
; uses A and B.
;------------------------------------------------------------------------
PRINT:      ani 7fh                 ; clear the most signficant bit
            mov b,a                 ; save the character in B
            xra a                   ; clear A for the start bit

            out OUTPORT             ; send the start bit
            mov a,b                 ; restore the character to A
            ;mov a,b                 ; timing adjustment
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
            out OUTPORT             ; send the stop bit
            mov a,b                 ; restore the character from B to A
            mvi b,0fch             
            call delay
            ret                     ; return to caller

outbit:     out OUTPORT             ; output the least significant bit
            mvi b,0fdh
            call delay
            ana a                   ; timing adjustment
            rrc                     ; shift A right
            ret

            end 0A00H
            
