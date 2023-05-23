           PAGE 0                  ; suppress page headings in AS listing file
;===============================================================================================
; This 4 function floating point calculator was demonstrated by Nat Wadsworth and Robert Findley
; to potential investors in order to get funding for SCELBI Computer Consulting.  In 1974, 
; calculators were expensive luxury items, so this sort of demo was enough to interest those 
; investors.  The program was published in the book "Machine Language Programming for the '8008'
; and similar microcomputers".
;
; Multiply is indicated by the symbol "X", not "*".  Input parsing is rather primitive.  Type a 
; space or = sign if the computer will not take your input.  There are some issues with rounding
; in this floating point package, which were later fixed by the time SCELBAL was released.
;
; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023
;
; jump to 0100H to start        
;===============================================================================================
           cpu 8008                ; use "old" 8008 mnemonics
           radix 8                 ; use octal for numbers
           
           ORG (1*400)+000
FPCONT:	   CAL CRLF2              ;DISPLAY A FEW CR & LF'S FOR I/0 DEVICE
           CAL DINPUT             ;LET OPERATOR ENTER A FP DECIMAL NUMBER
           CAL SPACES             ;DISPLAY A FEW SPACES AFTER NUMBER
           LLI 124                ;SET PWTR TO LSW OF FPACC
           LDH                    ;SET D = 0 POR SURE
           LEI 170                ;SET PNTR TO TEMP # STORAGE AREA
           LBI 004                ;SET PRECISION COUNTER
           CAL MOVEIT             ;MOVE FPACC TO TEMP STORAGE AREA
NVALID:    CAL INPUT              ;FETCH "OPERATOR" FROM INPUT DEVICE
           LBI 000                ;CLEAR REGISTER "B"
           CPI 253                ;TEST FOR "+" SIGN
           JTZ OPERA1             ;GO SET UP FOR "+" SIGN
           CPI 255                ;IF NOT"+", TEST FOR "-" SIGN
           JTZ OPERA2             ;GO SET UP FOR "-" SIGN
           CPI 330                ;IF NOT ABOVE, TEST FOR "X" (MULT) SIGN
           JTZ OPERA3             ;GO SET UP FOR "X" SIGN
           CPI 257                ;IF NOT ABOVE, TEST FOR "/" (DIV) SIGN
           JTZ OPERA4             ;GO SET UP FOR "/" SIGN
           CPI 377                ;IF NOT ABOVE, TEST FOR "RUBOUT"
           JFZ NVALID             ;IF NONE OF ABOVE, IGNORE INPUT
           JMP FPCONT             ;IF "RUBOUT" START NEW INPUT SEQUENCE
OPERA1:    DCB                    ;SET UP REGISTER "B" BASED ON ABOVE
           DCB                    ; "  "     "      "    "   "    "
OPERA2:    DCB                    ; "  "     "      "    "   "    "
           DCB                    ; "  "     "      "    "   "    "
OPERA3:    DCB                    ; "  "     "      "    "   "    "
           DCB                    ; "  "     "      "    "   "    "

OPERA4:    LCA                    ;SAVE "OPERATOR" CHARACTER IN REG "C"
           LAI 242                ;*** = NEXT TO LAST LOC IN "LOOKUP" TABLE
           ADB                    ;MODIFY ,"***" BY CONTENTS 0F "B"
           LLI 110                ;SET PNTR TO "LOOKUP" TABLE ADDR STORAGE
           LMA                    ;PLACE "LOOKUP" ADDR IN STOftAGE LOCATION
           LAC                    ;RESTORE "OPERATOR" CHARACTER TO ACC
           CAL ECHO               ;DISPLAY THE "OPERATOR" SIGN
           CAL SPACES             ;DISPLAY FEW SPACES AFTER "OPERATOR" SIGN
           CAL DINPUT             ;LET OPERATOR ENTER 2ND FP DECIMAL NUMBER
           CAL SPACES             ;PROVIDE FEW SPACES APTER 2ND NUMBER
           LAI 275                ;PLACE ASCII CODE FOR "=" IN ACCUMULATOR
           CAL ECHO               ;DISPLAY "." SIGN
           CAL SPACES             ;DISPLAY FEW SPACES AFTER "=" SIGN
           LLI 170                ;SET POINTER TO TEMP NUMBER STORAGE ARFA
           LDH                    ;SET D =0 FOR SURE
           LEI 134                ;SET ANOTHER POINTER TO LSW FPOP
           LBI 004                ;SET PRECISION COUNTER
           CAL MOVEIT             ;MOVE 1ST NUMBER INPUTTED TO FPOP
           LLI 110                ;SET PNTR TO "LOOKUP" TABLE ADDR STORAGE
           LLM                    ;BRING IN LOW ORDER ADDR OF "LOOKUP" TABLE
           LHI 001                ;XXX = PAGE THIS PROGRAM LOCATED ON
           LEM                    ;BRING IN AN ADDR STORED IN "LOOKUP" TABLE
           INL                    ;RESIDING ON THIS PAGE (XXX) AT LOCATIONS
           LDM                    ;"*** + B" AND "*** + B + 1" AND PLACE IT
           LLI 224                ;IN REGS "D & E" THEN CHANGE PNTR TO ADDR
           LME                    ;PART OF INSTRUCTION LABELED "RESULT" BE-
           INL                    ;LOW AND TRANSFER THE "LOOKUP" TABLE CON-
           LMD                    ;TENTS TO BEOMCE THE ADDRESS FOR THE IN-
           LHI 000                ;STRUCTION LABELED "RESULT." THEN RESTORE
           LDH                    ;REGISTERS "D" AND "H" BACK TO "0"
           JMP RESULT             ;NOW JUMP TO COMMAND LABELED "RESULT"
CRLF2:     LAI 215                ;SUBRTN TO PROVIDE CR & LF'S
           CAL ECHO               ;PLACE ASCII CODE FOR CR IN ACC & DISPLAY
           LAI 212                ;PLACE ASCII CODE FOR LINE FEED IN ACC
           CAL ECHO               ;AND DISPLAY
           LAI 215                ;DO IT AGAIN - CODE FOR CR IN ACC
           CAL ECHO               ;DISPLAY. IT
           LAI 212                ;CODE FOR LF
           CAL ECHO               ;DISPLAY IT
           RET                    ;RETURN TO CALLING ROUTINE

SPACES:    LAI 240                ;SET UP ASCII CODE FOR SPACE IN ACC
           CAL ECHO               ;DISPLAY A SPAC E
           LAI 240                ;DO IT AGAIN - CODE FOR SPACE IN ACC
           CAL ECHO               ;DISPLAY SPACE
           RET                    ;RETURN TO CALLING ROUTINE
RESULT:    CAL DUMMY              ;CAL RTN AT ADDRESS IN NEXT TWO BYTES1
           CAL FPOUT              ;DISPLAY RESULT
           JMP FPCONT             ;GO BACK'AND GET NEXT PROBLEM I

; TABLE OF OPERATIONS
;LOOKUP:
           DB   FPADD&377           ;240 LOW address for start of FPADD subroutine
           DB   FPADD/400           ;004 PAGE address for start of FPADD subroutine
           DB   FSUB&377            ;115 LOW address for start of FSUB subroutine
           DB   FSUB/400            ;005 PAGE address for start of FSUB subroutine
           DB   FPMULT&377          ;127 LOW address for start of FPMULT subroutine
           DB   FPMULT/400          ;005 PAGE address for start of FPMULT subroutine
;ENDLOOKUP:           
           DB   FPDIV&377           ;022 LOW address for start of FPDIV subroutine
           DB   FPDIV/400           ;006 PAGE address for start of FPMULT subroutine

FPOUT:     LLI 157                ;Set pointer to decimal exponent storage location
           LMI 000                ;Initialize storage location to zero
           LLI 126                ;Change pointer to FPACC (number to be outputted)
           LAM                    ;And fetch MSW of FPACC
           NDA                    ;Test the contents of MSW of FPACC
           JTS OUTNEG             ;If most significant bit of MSW is a one, have a minus nr.
           LAI 253                ;Else number is positive set ASCII code for space for a
           JMP AHEAD1             ;Positive number and go display a space
OUTNEG:    LLI 124                ;If number in FPACC is negative must negate in orde
           LBI 003                ;To display. Set pntr to LSW of FPACC & set prec. cntr.
           CAL COMPLM             ;Negate the number in the FPACC to make it positive
           LAI 255                ;But load ACC with ASCII code for minus sign
AHEAD1:    CAL ECHO               ;Call user display driver to output space or minus sign
           LAI 260                ;Load ASCII code for '0' into accumulato
           CAL ECHO               ;Call user display driver to output '0' as first characte
           LAI 256                ;Number string. Now load ASCII code for decimal point.
           CAL ECHO               ;Call user display driver to output '.'as second character.
           LLI 127                ;Set pointer to FPACC Exponent
           LAI 377                ;Load accumulator with minus one
           ADM                    ;Add value in FPACC Exponent
           LMA                    ;Restore compensated exponent value

DECEXT:    JFS DECEXD             ;If compen exp is zero or positive, multip MANT by 0.1
           LAI 004                ;If compensated exponent is negative
           ADM                    ;Add '4' (decimal) to exponent value
           JFS DECOUT             ;If exponent now zero or positive, output MANTISSA
           CAL FPX10              ;Otherwise, multiply MANTISSA by 10

DECREP:    LLI 127                ;Reset pointer to FPACC Exponent
           LAM                    ;Fetch value in exponent
           NDA                    ;Test value
           JMP DECEXT             ;Repeat process as required
DECEXD:    
           CAL FPD10              ;Multiply FPACC by 0.1
           JMP DECREP             ;Repeat process as required

                                  ;The next section outputs the mantissa
                                  ;(or fixed point number) by converting the value remaining
                                  ;in the FPACC (after the decimal exponent equivalent has
                                  ;been extracted from the original value if required by the
                                  ;previous routines) to a string of decirnal digits.
DECOUT:    LEI 164                ;Set pointer to LSW of output working registe
           LDH                    ;Set D to same page value as H
           LLI 124                ;Set pointer to LSW of FPACC
           LBI 003                ;Set precision counte
           CAL MOVEIT             ;Move value in FPACC to output working registe
           LLI 167                ;Set pointer to MSW plus one of output working registe
           LMI 000                ;Clear that location to 0
           LLI 164                ;Set pointer to LSW of output working registe
           LBI 003                ;Set precision counte
           CAL ROTATL             ;Rotate register left once to compensate for sign bit
           CAL OUTX10             ;Multiply output register by 10, overflow into N4SW+ 1
COMPEN:    LLI 127                ;Set pointer back to FPACC Exponent
           LBM                    ;Compensate for any remainder in the binary exponent
           INB                    ;By performing a rotate right on the output working
           LMB                    ;Register until the binary exponent becomes zero
           JTZ OUTDIG             ;Go output decimal digits when this loop is finished
           LLI 167                ;Binary exponent compensating loop. Setpointe'r to
           LBI 004                ;Working register MSW+L. Set precision counter.
           CAL ROTATR             ;Rotate working register to the right.
           JMP COMPEN             ;Repeat loop as required.
OUTDIG:    LLI 107                ;Set pointer to output digit counter storage location
           LMI 007                ;Initialize to value of seven
           LLI 167                ;Change pointer to output working register MSW+L
           LAM                    ;Fetch MSW+L byte containing BCD of digit to be
           NDA                    ;Displayed. Test the contents of this byte.
           JTZ ZERODG             ;If zero jump to ZERODG routine.
OUTDGS:    LLI 167                ;Reset pointer to working register MSW+L
           LAI 260
           ADM
           CAL ECHO

DECRDG:    LLI 107                ;Set pointer to FIXED/FLOAT indicator storage
           CAL CNTDWN
           JTZ EXPOUT
           CAL OUTX10
           JMP OUTDGS

ZERODG:    LLI 157                ;If first digit of floating point number is a zero, set
           CAL CNTDWN             ;Decrement the value to compensate for skipping
           LLI 166                ;Change pointer to MSW of output working registe
           LAM                    ;Fetch MSW of output working registe
           NDA                    ;Test the contents
           JFZ DECRDG             ;If non-zero, continue outputting
           DCL                    ;Else decrement pointer to next byte in working registe
           LAM                    ;Fetch its contents
           NDA                    ;Test
           JFZ DECRDG             ;If non-zero, continue outputting
           DCL                    ;Else decrement pointer to LSW of working registe
           LAM                    ;Fetch its contents
           NDA                    ;Test
           JFZ DECRDG             ;If non-zero, continue outputting
           LLI 157                ;If decimal mantissa is zero, set pointer to decirnal
           LMA                    ;Exponent storage and clear it
           JMP DECRDG             ;Finish outputting

                                  ;Following routine multiplies the binary number in the
                                  ;output working register by ten to push the most signifi-
                                  ;cant digit out to the MSW+L byte.

OUTX10:    LLI 167                ;Set pointer to work ing register M SW+ 1
           LMI 000                ;Clear it in preparation for receiving next digit pushed
           LLI 164                ;Into it. Change pointer to working register LSW.
           LDH                    ;Set up register D to same page as H.
           LEI 160                ;Set second pointer to LSW of second working registe
           LBI 004                ;Set precision counte
           CAL MOVEIT             ;Move first working register into second
           LLI 164                ;Reset pointer to LSW of first working registe
           LBI 004                ;Set precision counte
           CAL ROTATL             ;Rotate contents of first working register left (X 2)
           LLI 164                ;Reset pointer to LSW
           LBI 004                ;Reset precision counte
           CAL ROTATL             ;Rotate contents left again (X 4)
           LLI 160                ;Set pointer to LSW of original value in 2'nd registe
           LEI 164                ;Set pointer to LSW of rotated value
           LBI 004                ;Set precision counte
           CAL ADDER              ;Add rotated value to original value (X 5)
           LLI 164                ;Reset pointer to LSW of first working registe
           LBI 004                ;Set precision counte
           CAL ROTATL             ;Rotate contents left again (X 10)
           RET                    ;Exit to calling routine

EXPOUT:
           LAI 305                ;Else, load ACC with ASCII code for letter E.
           CAL ECHO               ;Display E for Exponent via user's display driver rtn
           LLI 157
           LAM                    ;Get decimal exponent value back into ACC
           NDA                    ;Test again
           JTS EXOUTN             ;If value is negative, skip ahead
           LAI 253                ;If positive, load ASCII code for + sign
           JMP AHEAD2             ;Jump to display the + sign
EXOUTN:    XRI 377                ;When decimal exponent is negative, must negate
           ADI 001                ;Value for display purposes. Perform two's complement
           LMA                    ;And restore the negated value to storage location
           LAI 255                ;Load ASCII code for minus sign
AHEAD2:    CAL ECHO               ;Display the ASCII character in ACC
           LBI 000                ;Clear register B
           LAM                    ;Fetch the decimal exponent value back into ACC
SUB12:     SUI 012                ;Subtract 10 (decimal) from value in ACC
           JTS TOMUCH             ;Break out of loop when accumulator goes negative
           LMA                    ;Else restore value to storage location
           INB                    ;Increment register B as a counte
           JMP SUB12              ;Repeat loop to form tens value of decimal exponent
TOMUCH:    LAI 260                ;Load base ASCII value for digit into the accumulato
           ADB                    ;Add to the count in B to forin tens digit of decimal
           CAL ECHO               ;Exponent. Display via user's driver subroutine
           LAM                    ;Fetch remainder of decimal exponent value
           ADI 260                ;Add in ASCII base value to form final digit
           CAL ECHO               ;Display second digit of decirnal exponent
           RET                    ;Finished outputting. Return to caller.

DINPUT:    LHI 000                ;** Set H to page of floating point working registers
           LLI 150                ;Set L to start of decirnal-to-binary working area
           XRA                    ;Clear the accumulato
           LBI 010                ;Set up a loop counte
CLRNX2:    LMA                    ;Deposit zero in working area to initialize
           INL                    ;Advance the memory pointe
           DCB                    ;Decrement the loop counte
           JFZ CLRNX2             ;Clear working area until loop counter is zero
           LLI 103                ;Set pointer to floating point temporary registers and
           LBI 004                ;Indicators working area. Set up a loop counter.
CLRNX3:    LMA                    ;Deposit zero in working area to initialize
           INL                    ;Advance the memory pointe
           DCB                    ;Decrement the loop counte
           JFZ CLRNX3             ;Clear working area until loop counter is zero
           CAL INPUT              ;Fetch a character from the ASCII chax string buffe
           CPI 253                ;(Typically the SYMBOL/TOKEN buffer). See if it is
           JTZ SECHO              ;Code for + sign. Jump ahead if code for + sign.
           CPI 255                ;See if code for minus (-) sign.
           JFZ NOTPLM             ;Jump ahead if not code for minus sign. If code fo
           LLI 103                ;Minus sign, set pointer to MINUS flag storage location.
           LMA                    ;Set the MINUS flag to indicate a minus numbe
                                  ;Following subroutine is used
                                  ;to convert decimal charac-
                                  ;ters to binary fixed point forinat
                                  ;in a triple-precision format.

SECHO:     CAL ECHO
NINPUT:    CAL INPUT              ;Fetch another character from the ASCII char string
NOTPLM:    CPI 377                ;See if character represents a period (decimal point) in
           JTZ ERASE             ;Input string. Jump ahead if yes.
           CPI 256
           JTZ PERIOD
           CPI 305                ;If not period, see if code for E as in Exponent
           JTZ FNDEXP             ;Jump ahead if yes.
           CPI 260                ;Else see if code for space.
           JTS ENDINP             ;Ignore space character, go fetch another character.
           CPI 272                ;If none of the above see if zero byte
           JFS ENDINP             ;Indicating end of input char string. If yes, jumn ahead.
           LLI 156                ;For valid digit, set pointer to MSW of temporary
           LBA                    ;Decimal to binary holding registers. Save character in C.
           LAI 370                ;Form mask for sizing in accumulator. Now see if
           NDM                    ;Holding register has enough room for the conversion of
           JFZ NINPUT             ;Another digit. Ignore the input if no more room.
           LAB
           CAL ECHO
           LLI 105                ;If have room in register then set pointer to input digit
           LCM                    ;Counter location. Fetch the present value.
           INC                    ;Increment it to account for incoming digit.
           LMC                    ;Restore updated count to storage location.
           CAL DECBIN             ;Call the DECimal to BINary conversion routine to add
           JMP NINPUT             ;In the new digit in holding registers. Continue inputting.
PERIOD:    LBA                    ;Save character code in register B
           LLI 106                ;Set pointer to PERIOD indicator storage location
           LAM                    ;Fetch value in PERIOD indicato
           NDA                    ;Exercise CPU flags
           JFZ ENDINP             ;If already have a period then display error message
           LLI 105                ;If not, change pointer to digit counter storage location
           LMA                    ;Clear the digit counter back to zero
           INL                    ;Advance pointer to PERIOD indicato
           LMB                    ;Set the PERIOD indicato
           LAB
           CAL ECHO
           JMP NINPUT             ;Continue processing the input character string

ERASE:     LAI 274
           CAL ECHO
           LAI 240
           CAL ECHO
           CAL ECHO
           JMP DINPUT

FNDEXP:    CAL ECHO
           CAL INPUT              ;Get next character in Exponent
           CPI 253                ;See if it is code for + sign
           JTZ EXECHO             ;Jump ahead if yes.
           CPI 255                ;If not + sign, see if minus sign
           JFZ NOEXPS             ;If not minus sign then jump ahead
           LLI 104                ;For minus sign, set pointer to EXP SIGN indicato
           LMA                    ;Set the EXP SIGN indicator for a minus exponent

EXECHO:    CAL ECHO
EXPINP:    CAL INPUT              ;Fetch the next character in the decimal exponent
NOEXPS:    CPI 377                ;Exercise the CPU flags
           JTZ ERASE              ;If character inputted was zero, then end of input string
           CPI 260                ;If not end of string, check to see
           JTS ENDINP             ;If character represents
           CPI 272                ;A valid decimal number (0 to 9)
           JFS ENDINP             ;Display error message if not a valid digit at this point!
           NDI 017                ;Else trim the ASCII code to BCD
           LBA                    ;And save in register B
           LLI 157                ;Set pointer to input exponent storage location
           LAI 003                ;Set accumulator equal to three
           CPM                    ;See if any previous digit in exponent greater than three
           JTS EXPINP             ;Display error message if yes
           LCM                    ;Else save any previous value in register C
           LAM                    ;And also place any previous value in accumulato
           NDA                    ;Clear the carry bit with this instruction
           RAL
           RAL                    ;Single precision multiply by ten algorithm
           ADC
           RAL                    ;Two rotate lefts equals times fou
           ADB                    ;Adding in the digit makes total times five
           LMA                    ;Rotating left again equals times ten
           LAI 260
           ADB                    ;now add in digit just inputted
           JMP EXECHO             ;Go get any additional exponent int)ut

ENDINP:    LLI 103                ;Set pointer to mantissa SIGN indicato
           LAM                    ;Fetch the SIGN indicator to the acclimulato
           NDA                    ;Exercise the CPU flags
           JTZ FININP             ;If SIGN indicator is zero, go finish up as nr is positive
           LLI 154                ;But, if indicator is non-zero, number is negative
           LBI 003                ;Set pntr to LSW of storage registers, set precision ent
           CAL COMPLM             ;Negate the triple-precision number in holding registers
FININP:    LLI 153                ;Set pointer to input storage LS~V minus one
           XRA                    ;Clear the accumulato
           LDA                    ;Clear the LSW minus one location
           LMA                    ;Set register D to floating point working page
           LEI 123                ;Set E to address of FPACC LSW minus one
           LBI 004                ;Set precision counte
           CAL MOVEIT             ;Move number from input register to FPACC
           LBI 027
           CAL FPNORM              ;Now convert the binary fixed point to floating point
           LLI 104                ;Set pointer to Exponent SIGN indicator location
           LAM                    ;Fetch the value of the EXP SIGN indicato
           NDA                    ;Exercise the CPU flags
           LLI 157                ;Reset pointer to input exponent storage location
           JTZ POSEXP             ;If EXP SIGN indicator zero, exponent is positive
           LAM                    ;Else, exponent is negative so must negate
           XRI 377                ;The value in the input exponent storage location
           ADI 001                ;By performing this two's complement
           LMA                    ;Restore the negated value to exponent storage location
POSEXP:    LLI 106                ;Set pointer to PERIOD indicator storage location
           LAM                    ;Fetch the contents of the PERIOD indicato
           NDA                    ;Exercise the CPU flags
           JTZ EXPOK              ;If PERIOD indicator clear, no decimal point involved
           LLI 105                ;If have a decimal point, set pointer to digit counte
           XRA                    ;Storage location. Clear the accumulator.
           SUM                    ;And get a negated value of the digit counter in ACC
EXPOK:     LLI 157                ;Change pointer to input exponent storage location
           ADM                    ;Add this value to negated digit counter value
           LMA                    ;Restore new value to storage location
           JTS MINEXP             ;If new value is minus, skip over next subroutine
           RTZ                    ;If new value is zero, no further processing required

EXPFIX:    CAL FPX10              ;Compensated decimal exponent is positive, multiply
           JFZ EXPFIX             ;FPACC by 10, loop until decimal exponent is zero
           RET                    ;Exit with converted value in FP ACC
                                  ;Following subroutine will multiply the floating point
                                  ;binary number stored in FPACC by ten tirnes the
                                  ;value stored in the deciinal exponent storage location.

FPX10:     LEI 134                ;Multiply FPACC by 10 subroutine, set pointer to
           LDH                    ;FPOP LSW, then set D = zero for sure
           LLI 124                ;Set pointer to FP ACC LSW
           LBI 004                ;Set precision counter
           CAL MOVEIT             ;Move FPACC to FPOP (including exponents)
           LLI 127                ;Set pointer to FP ACC exponent
           LMI 004                ;Place FP form of 10 (decimal) in FPACC
           DCL                    ;Place FP form of 10 (decimal) in FPACC
           LMI 120                ;Place FP form of 10 (decimal) in FPACC
           DCL                    ;Place FP form of 10 (decimal) in FPACC
           XRA                    ;Place FP form of 10 (decimal) in FPACC
           LMA                    ;Place FP form of 10 (decimal) in FPACC
           DCL                    ;Place FP form of 10 (decimal) in FPACC
           LMA                    ;Place FP form of 10 (decimal) in FPACC
           CAL FPMULT             ;Now multiply original binary number (in FPOP) by ten
           LLI 157                ;Set pointer to decimal exponent storage
           CAL CNTDWN             ;Decrement decimal exponent value
           RET                    ;Return to calling program

                                  ;Following subroutine will multiply the floating point
                                  ;binary number stored in PPACC by 0.1 times the value
                                  ;(negative) stored in the decimal exponent storage location

MINEXP:	  CAL FPD10               ;Compensated decimal exponent is minus, multiply
          JFZ MINEXP              ;FPACC by 0.1, loop until decimal exponent is zero
          RET                     ;Exit with converted value in FP ACC

FPD10:    LEI 134                 ;Multiply FPACC by 0.1 routine, pointer to FPOP LSW
          LDH                     ;Set D = '0 ' for sure
          LLI 124                 ;Set pointer to FP ACC
          LBI 004                 ;Set pointer to FP ACC
          CAL MOVEIT              ;Set precision counter
          LLI 127                 ;Move FPACC to FPOP (including exponent)
          LMI 375                 ;Set pointer to FPACC exponent
          DCL                     ;Place FP form of 0.1 (decimal) in FPACC
          LMI 146                 ;Place FP form of 0 .1 (decimal) in FPACC
          DCL                     ;Place FP form of 0.1 (decimal) in FPACC
          LMI 146                 ;Place FP form of 0 .1 (decimal) in FPACC
          DCL                     ;Place FP form of 0.1 (decimal) in FPACC
          LMI 147                 ;Place FP form of 0.1 (decimal) in FPACC
          CAL FPMULT              ;Place FP form of 0 .1 (decimal) in FPACC
          LLI 157                 ;Now multiply original binary number (in FPOP) by 0.1
          LBM                     ;Set pointer to decimal exponent storage
          INB                     ;Fetch value
          LMB                     ;Increment it
          RET                     ;Restore it to memory

DECBIN:  
           LLI 153                ;Set pointer to temporary storage location
           LAB                    ;Restore character inputted to accumulato
           NDI 017                ;Trim ASCII code to BCD
           LMA                    ;Store temporarily
           LEI 150                ;Set pointer to working area LSW of multi-byte registe
           LLI 154                ;Set another pointer to LSW of conversion registe
           LDH                    ;Make sure D set to page of working area
           LBI 003                ;Set precision counte
           CAL MOVEIT             ;Move original value of conversion register to working
           LLI 154                ;Register. Reset pointer to LSW of conversion register.
           LBI 003                ;Set precision counte
           CAL ROTATL             ;Rotate register left, (Multiplies value by two.)
           LLI 154                ;Reset pointer to LSW.
           LBI 003                ;Set precision counte
           CAL ROTATL             ;Multiply by two again (total now times four).
           LEI 154                ;Set pointer to LSW of conversion register.
           LLI 150                ;Set pointer to LSW of working register (original value).
           LBI 003                ;Set precision counter.
           CAL ADDER              ;Add original value to rotated value (now times five).
           LLI 154                ;Reset pointer to LSW
           LBI 003                ;Set precision counte
           CAL ROTATL             ;Multiply by two once more (total now times ten).
           LLI 152                ;Set pointer to clear working register locatiotis
           XRA                    ;Clear the accumulato
           LMA                    ;Clear MSW of working registe
           DCL                    ;Decrement pointe
           LMA                    ;Clear next byte
           LLI 153                ;Set pointer to current digit storage location
           LAM                    ;Fetch the current digit
           LLI 150                ;Change pointer to LSW of working registe
           LMA                    ;Deposit the current digit in LSW of working registe
           LEI 154                ;Set pointer to conversion register LSW
           LBI 003                ;Set precision counte
           CAL ADDER              ;Add current digit to conversion register to complete
           RET

FPNORM:    LAB                    ;Get CPU register B into ACC to check for special case
           NDA
           JTZ NOEXCO
           LLI 127                ;Set L to FPACC Exponent byte
           LMB                    ;Else set Exponent of FPACC to 23 decimal
NOEXCO:    LLI 126
           LAM                    ;Fetch MSW of FPACC into accumulato
           LLI 100                ;Change pointer to SIGN indicator storage location
           NDA
           JTS ACCMIN
           XRA
           LMA                    ;Place the MSW of FPACC there for future reference
           JMP ACZERT             ;If sign bit not set then jump ahead to do next test
ACCMIN:    LMA
           LBI 004                ;If sign bit set, number in FPACC is negative. Set up
           LLI 123                ;For two's complement operation
           CAL COMPLM             ;And negate the value in the FPACC to make it positive
ACZERT:    LLI 126                ;Reset pointer to MSW of FPACC
           LBI 004                ;Set precision counter to number of bytes in FPACC
LOOK0:     LAM                    ;Plus one. Fetch a byte of the FPACC.
           NDA                    ;Set CPU flags
           JFZ ACNONZ             ;If find anything then FPACC is not zero
           DCL                    ;Else decrement pointer to NSW of FPACC
           DCB                    ;Decrement precision counte
           JFZ LOOK0              ;Continue checking to see if FPACC contains anything
           LLI 127                ;Until precision counter is zero. If reach here then
           XRA                    ;Reset pointer to FPACC Exponent. Clear the ACC and
           LMA                    ;Clear out the FPACC Exponent. Value of FPACC is zip!
           RET                    ;Exit to calling routine
ACNONZ:    LLI 123                ;If FPACC has any value set pointer to LSW minus one
           LBI 004                ;Set precision counter to number of bytes in FPACC
           CAL ROTATL             ;Plus one for special cases. Rotate the contents of the
           LAM                    ;FPACC to the LEFT. Pointer will be set to MSW afte
           NDA                    ;Rotate ops. Fetch MSW and see if have anything in
           JTS ACCSET             ;Most significant bit position. If so, have rotated enough
           INL                    ;If not, advance pointer to FPACC Exponent. Fetch
           CAL CNTDWN
           JMP ACNONZ             ;Continue rotating ops to normalize the FPACC
ACCSET:    LLI 126                ;Set pntr to FPACC MSW. Now must provide room fo
           LBI 003                ;Sign bit in nonnalized FPACC. Set precision counter.
           CAL ROTATR             ;Rotate the FPACC once to the right now.
           LLI 100                ;Set the pointer to SIGN indicator storage location
           LAM                    ;Fetch the original sign of the FPACC
           NDA                    ;Set CPU flags
           RFS                    ;If original sign of FPACC was positive, can exit now.
           LLI 124                ; However, if original sign was negative, must now restore
           LBI 003                ;The FPACC to negative by performing two's comple-
           CAL COMPLM             ;Ment on FPACC. Return to caring rtn via COMPLM.
           RET

FPADD:     LLI 126                ;Set pointer to MSW of FP ACC
           LBI 003                ;Set loop counter
CKZACC:    LAM                    ;Fetch part of FPACC
           NDA                    ;Set flags after loading operation
           JFZ NONZAC             ;Finding anything means FPACC not zero
           DCB                    ;If that part equals zero, decrement loop counter
           JTZ MOVOP              ;If FPACC equals zero , move FPOP into FPACC
           DCL                    ;Not finished checking, decrement pointer
           JMP CKZACC             ;And test next part of FPACC
MOVOP:     CAL SWITCH             ;Save pointer to LSW of FPACC
           LHD                    ;Set H equal to zero for sure
           LLI 134                ;Set pointer to LSW of FPOP
           LBI 004                ;Set a loop counter
           CAL MOVEIT             ;Move FPOP into FPACC as answer
           RET                    ;Exit FP ADD subroutine
NONZAC:    LLI 136                ;Set pointer to MSW of FPOP
           LBI 003                ;Set loop counter
CKZOP:     LAM                    ;Set flags after load operation
           NDA                    ;If not zero then have a number
           JFZ CKEQEX             ;If zero, decrement loop counter
           DCB                    ;Exit subroutine if FPOP equals zero
           RTZ                    ;Else decreme nt pointer to next part of FPOP
           DCL                    ;And continue testing for zero FPOP
           JMP CKZOP              ;Check for equal expo nents
CKEQEX:    LLI 127                ;Get FPACC exponent
           LAM                    ;Change pointer to FPOP exponent
           LLI 137                ;Compare exponents
           CPM                    ;If same can setup for ADD operation
           JTZ SHACOP
           XRI 377                ;If not same, then two 's complement
           ADI 001                ;The value of the FP ACC exponent
           ADM                    ;And add in FPOP exponent
           JFS SKPNEG             ;If + then go directly to alignment test
           XRI 377                ;If negative perform two's complement
           ADI 001                ;On the result
SKPNEG:
           CPI 030                ;N ow see if result greater than 27 octal
           JTS LINEUP             ;If not can perform alignment
           LAM                    ;If not alignable, get FPOP exponent
           LLI 127                ;Set pointer to FPACC exponent
           SUM                    ;Subtract FPACC exponent from FPOP exponent
           RTS                    ;FPACC exponent greater so just exit routine
           LLI 124                ;FPOP was greater, set pointer to FPACC LSW
           JMP MOVOP              ;Go put FPOP into FPACC and then exit routine
LINEUP:
           LAM                    ;Align FPACC and FPOP, get FPOP exponent
           LLI 127                ;Change pointer to FPACC exponent
           SUM                    ;Subtract FPACC exponent from FPOP exponent
           JTS SHIFTO             ;FPACC greater so go to shift operand
           LCA                    ;FPOP greater so save difference
MORACC:
           LLI 127                ;Pointer to FP ACC exponent
           CAL SHLOOP             ;Call shift loop subroutine
           DCC                    ;Decrement difference counter
           JFZ MORACC             ;Continue aligning if not done
           JMP SHACOP             ;Setup for ADD operation
SHIFTO:
           LCA                    ;Shift FPOP routine, save difference count (negative)
MOROP:     LLI 137                ;Set pointer to FPOP exponent
           CAL SHLOOP             ;Call shift loop subroutine
           INC                    ;Increment difference counter
           JFZ MOROP              ;Shift again if not done
SHACOP:
           LLI 123                ;First clear out extra room, setup pointer
           LMI 000                ;to FP A CC LSW + 1 and set it to zero
           LLI 127                ;N ow prepare to shift FP ACC right once
           CAL SHLOOP             ;Set pointer and then call shift loop routine
           LLI 137                ;Shift FPOP right once, first set pointer
           CAL SHLOOP             ;Call shift loop subroutine
           LDH                    ;Setup pointers, set T) equal to zero for sure
           LEI 123                ;Pointer to LSW of FPACC
           LBI 004                ;Set precision counter
           CAL ADDER              ;Add FPACC to FPOP using quad-precision
           LBI 000                ;Set B for standard normalization procedure
           CAL FPNORM             ;Normalize the result of the addition
           RET                    ;Exit FP ADD subroutine with result in FP ACe

SHLOOP:    LBM                    ;Sh ifting loop for alignment
           INB                    ;Fetch exponent into B and increment
           LMB                    ;Return increment value to memory
           DCL                    ;Decrement the pointer
           LBI 004                ;Set a counter
           
FSHIFT:    LAM                    ;Get MSW of floating point number
           NDA                    ;Set flags after loading operation
           JTS BRING1             ;If number is minus, need to shift in a '1 '
           CAL ROTATR             ;Otherwise perform N'th-precision rotate
           RET                    ;Exit FSHIFT subroutine

BRING1:    RAL                    ;Save '1 ' in carry bit
           CAL ROTR               ;Do ROT ATE RIGHT without clearing carry bit
           RET                    ;Exit FSHIFT SUbroutine              

MOVEIT:    LAM                    ;Fetch a word from memory string' A'
           INL                    ;Advance 'A' string pointer
           CAL SWITCH             ;Switch pointer to string 'B'
           LMA                    ;Put word from string 'A' into 'B'
           INL                    ;Advance B string pointer
           CAL SWITCH             ;Switch pointer back to string 'A'
           DCB                    ;Decrement counter
           RTZ                    ;Return to calling routine when counter is zero
           JMP MOVEIT             ;Otherwise continue moving operation

FSUB:      LLI 124                ;Set pointer to LSW of FPACC
           LBI 003                ;Set precision counter
           CAL COMPLM             ;Perform two's complement on FPACC
           JMP FPADD              ;Subtraction accomplished now by adding!

                                  ;The first part of the FLOATING POINT MULTIPLI-
                                  ;CATION subroutine calls a subroutine to check the
                                  ;original signs of the numbers that are to be multi-
                                  ;plied and perform working register clearing functions.
                                  ;Next the exponents of the numbers to be multiplied
                                  ;are added together.

FPMULT:    CAL CKSIGN             ;Call routine to set up registers & ck signs of numbers
ADDEXP:    LLI 137                ;Set pointer to FPOP Exponent
           LAM                    ;Fetch FPOP Exponent into the accumulato
           LLI 127                ;Change pointer to FPACC Exponent
           ADM                    ;Add FPACC Exponent to FPOP Exponent
           ADI 001                ;Add one more to total for algorithm compensation
           LMA                    ;Store result in FPACC Exponent location
SETMCT:    LLI 102                ;Change pointer to bit counter storage location
           LMI 027                ;Initialize bit counter to 23 decimal

                                  ;Next portion of the FPMULT routine is the iinplernen-
                                  ;tation of the algorithm illustrated in the flow chart
                                  ;above. This portion multiplies the values of the two
                                  ;mantissas. The final value is rounded off to leave the
                                  ;23 most significant bits as the answer that is stored
                                  ;back in the FPACC.

MULTIP:    LLI 126                ;Set pointer to MSW of FPACC mantissa
           LBI 003                ;Set precision counte
           CAL ROTATR             ;Rotate FPACC (multiplier) RIGHT into carry bit
           CTC ADOPPP             ;If carry is a one, add multiplicand to partial-product
           LLI 146                ;Set pointer to partial-product most significant byte
           LBI 006                ;Set precision counter (p-p register is double length)
           CAL ROTATR             ;Shift partial-product RIGHT
           LLI 102                ;Set pointer to bit counter storage location
           CAL CNTDWN
           JFZ MULTIP             ;If have not multiplied for 23 (deciinal) bits, keep going
           LLI 146                ;If have done 23 (decimal) bits, set pntr to p-p MSW
           LBI 006                ;Set precision counter (for double length)
           CAL ROTATR             ;Shift partial-product once more to the RIGHT
           LLI 143                ;Set pointer to access 24'th bit in partial-product
           LAM                    ;Fetch the byte containing the 24'th bit
           RAL                    ;Position the 24'th bit to be MSB in the accumulato
           LAA
           NDA                    ;Set the CPU flags after to rotate operation and test to
           CTS MROUND             ;See if 24'th bit of p-p is a ONE. If so, must round-off
           LLI 123                ;Now set up pointers
           CAL SWITCH
           LHD
           LLI 143                ;From the partial-product location
           LBI 004                ;To the FPACC
	
EXMLDV:    CAL MOVEIT             ;Perform the transfer from p-p to FPACC
           LBI 000                ;Set up CPU register B to indicate regular normalization
           CAL FPNORM             ;Normalize the result of multiplication
           LLI 101                ;Now set the pointer to the original SIGNS indicato
           LAM                    ;Fetch the indicato
           NDA                    ;Exercise the CPU flags
           RFZ                    ;If indicator is non-zero, answer is positive can exit he
           LLI 124
           LBI 003
           CAL COMPLM
           RET

                                  ;The following portions of the FPMULT
                                  ;routine set up working locations in memory by clearing
                                  ;locations for an expanded FPOP area and the partial-produc
                                  ;area. Next, the signs of the two numbers to be multiplied
                                  ;are examined. Negative numbers are negated
                                  ;in preparation for the multiplication
                                  ;algorithm. A SIGNS indicator register is set up during
                                  ;this process to indicate whether the final result of the
                                  ;multiplication should be positive or negative. (Negative
                                  ;if original signs of the two numbers to be multiplied are
                                  ;different.)

CKSIGN:    CAL CLRWRK
           LLI 101                ;Set pointer to start of partial-product working area
           LMI 001                ;** Set H to proper page
           LLI 126
           LAM
           NDA
           JTS NEGFPA

OPSGNT:    LLI 136                ;Set pointer to MSW of FPOP
           LAM                    ;Fetch MSW of mantissa into accumulato
           NDA                    ;Test flags
           RFS                    ;Return to caller if number in FPOP is positive
           LLI 101                ;Else change pointer to M/D SIGNS indicato
           CAL CNTDWN
           LLI 134                ;Set pointer to LSW of FPOP
           LBI 003                ;Set precision counte
           CAL COMPLM             ;Two's complement value of FPOP & return to calle
           RET

NEGFPA:    LLI 101                ;Set pointer to M/D SIGNS indicato
           CAL CNTDWN
           LLI 124                ;Set pointer to LSW of FPACC
           LBI 003                ;Set precision counte
           CAL COMPLM             ;Two's complement value of FPACC
           JMP OPSGNT             ;Proceed to check sign of FPOP

CLRWRK:    LLI 140
           LBI 010
           XRA
CLRNEX:    LMA                    ;Now clear out locations for the partial-product
           DCB
           JTZ CLROPL
           INL                    ;Working registers
           JMP CLRNEX
CLROPL:    LBI 004                ;Set a loop counte
           LLI 130                ;Set up pointe
CLRNX1:    LMA                    ;Clear out some extra registers so that the
           DCB                    ;Perform clearing ops until loop counte
           RTZ
           INL
           JMP CLRNX1             ;Is zero

                                  ;The following subroutine adds the double length (six regis
                                  ;multiplicand in FPOP to the partial-product register when
                                  ;called on by the multiplication algorithm.

ADOPPP:    LEI 141                ;Pointer to LSW of partial-product
           LDH                    ;On same page as FPOP
           LLI 131                ;LSIV of FPOP which contains extended multiplicand
           LBI 006                ;Set precision counter (double length working registers)
           CAL ADDER              ;Add multiplicand to partial-product & return to calle
           RET

MROUND:    LBI 003                ;Set up precision counte
           LAI 100                ;Prepare to add one to 24'th bit of partial-product
           ADM                    ;Add one to the 24'th bit of the partial-product
CROUND:    LMA                    ;Restore the updated byte to memory
           INL                    ;Advance the memory pointer to next most significant
           LAI 000                ;Byte of partial-product, then clear ACC without
           ACM                    ;Disturbing carry bit. Now perform add with carry to
           DCB                    ;Propagate any rounding in the partial-product registers.
           JFZ CROUND             ;If cotinter is not zero continue propagating any carry
           LMA                    ;Restore final byte to memory
           RET                    ;Exit to calling routine

FPDIV:     CAL CKSIGN             ;Call routine to set up registers & ck signs of numbers
           LLI 126                ;Set pointer to MSW of FPACC (divisor)
           LAI 000                    ;Fetch MSW of FPACC to accumulato
           CPM
           JFZ SUBEXP
           DCL
           CPM
           JFZ SUBEXP
           DCL
           CPM
           JTZ DERROR             ;If MSW of FPACC is zero go display 'DZ' error message
SUBEXP:    LLI 137                ;Set pointer to FPOP (dividend) Exponent
           LAM                    ;Get FPOP Exponent into accumulato
           LLI 127                ;Change pointer to FPACC (divisor) Exponent
           SUM                    ;Subtract divisor exponent from dividend exponent
           ADI 001                ;Add one for algorithm compensation
           LMA                    ;Place result in FPACC Exponent
SETDCT:    LLI 102                ;Set pointer to bit counter storage location
           LMI 027                ;Initialize bit counter to 23 decimal

                                  ;Main division algorithm for mantissas

DIVIDE:    CAL SETSUB             ;Go subtmct divisor from dividend
           JTS NOGO               ;If result is negative then place a zero bit in quotient
           LEI 134                ;If result zero or positive then move remainder afte
           LLI 131                ;Subtraction from working area to become new dividend
           LBI 003                ;Set up moving pointers and initialize precision counte
           CAL MOVEIT             ;Perform the transfe
           LAI 001                ;Place a one into least significant bit of accumulato
           RAR                    ;And rotate it out into the carry bit
           JMP QUOROT             ;Proceed to rotate the carry bit into the current quotient

NOGO:      LAI 000                    ;When result is negative, put a zero in the carry bit, then
           RAR
QUOROT:    LLI 144                ;Set up pointer to LSW of quotient registe
           LBI 003                ;Set precision counte
           CAL ROTL               ;Rotate carry bit into quotient by using special entry to
           LLI 134                ;ROTATL subroutine. Now set up pointer to dividend
           LBI 003                ;LSW and set precision counte
           CAL ROTATL             ;Rotate the current dividend to the left
           LLI 102                ;Set pointer to bit counter storage location
           CAL CNTDWN
           JFZ DIVIDE             ;If bit counter is not zero, continue division process
           CAL SETSUB             ;After 23 (decimal) bits, do subtraction once more fo
           JFS DVEXIT             ;Possible rounding. Jump ahead if no rounding required.
           LLI 144                ;If rounding required set pointer to LSW of quotient
           LAM                    ;Fetch LSW of quotient to accumulato
           ADI 001                ;Add one to 23rd bit of quotient
           LMA                    ;Restore updated LSW of quotient
           LAI 000                ;Clear accumulator without disturbing carry bit
           INL                    ;Advance pointer to next significant byte of quotient
           ACM                    ;Propagate any carry as part of rounding process
           LMA                    ;Restore the updated byte of quotient
           LAI 000                ;Clear ACC again without disturbing carry bit
           INL                    ;Advance pointer to MSW of quotient
           ACM                    ;Propagate any carry to finish rounding process
           LMA                    ;Restore the updated byte of quotient
           JFS DVEXIT             ;If most significant bit of quotient is zero, go finish up
           LBI 003                ;If not, set precision counte
           CAL ROTATR             ;And rotate quotient to the right to clear the sign bit
           LLI 127                ;Set pointer to FPACC Exponent
           LBM                    ;Fetch FPACC exponent
           INL                    ;Increment the value to compensate for the rotate right
           LMB                    ;Restore the updated exponent value
DVEXIT:    LLI 144                ;Set up pointers
           LEI 124                ;To transfer the quotient into the FPACC
           LBI 003                ;Set precision counte
                                  ;THIS IS A CORRECTION FOUND IN THE NOTES
           JMP EXMLDV             ;And exit through FPMULT routine at EXMLDV

                                  ;Subroutine to subtract divisor from dividend. Used by
                                  ;main DIVIDE subroutine.

SETSUB:    LLI 131                ;Set pointer to LSW of working area
           CAL SWITCH
           LHD                    ;On same page as FPACC
           LLI 124                ;Set pointer to LSW of FPACC (divisor)
           LBI 003                ;Set precision counte
           CAL MOVEIT             ;Perform transfe
           LEI 131                ;Reset pointer to LSW of working area (now divisor)
           LLI 134                ;Reset pointer to LSW of FPOP (dividend)
           LBI 003                ;Set precision counte
           CAL SUBBER             ;Subtract divisor from dividend
           LAM                    ;Get MSW of the result of the subtraction operations
           NDA                    ;Exercise CPU flags
           RET                    ;Return to caller with status

DERROR:    CAL DERMSG
           JMP USERDF

ADDER:     NDA                    ;Initialize the carry bit to zero upon entry
ADDMOR:    LAM                    ;Fetch byte from register group A
           CAL SWITCH             ;Switch memory pointer to register group B
           ACM                    ;Add byte from A to byte from B with carry
           LMA                    ;Leave result in register group B
           DCB                    ;Decrement number of bytes (precision) counte
           RTZ                    ;Return to caller when all bytes in group processed
           INL                    ;Else advance pointer for register group B
           CAL SWITCH             ;Switch memory pointer back to register group A
           INL                    ;Advance the pointer for register group A
           JMP ADDMOR             ;Continue the multi-byte addition operation

                                  ;Subroutine to exchange the contents of H & L with
                                  ;D & E.

SWITCH:    LCH                    ;Transfer register H to C temporarily
           LHD                    ;Place value of D into H
           LDC                    ;Now put former H from C into D
           LCL                    ;Transfer register L to C temporarily
           LLE                    ;Place value of E into L
           LEC                    ;Now put former L from C into E
           RET                    ;Exit to calling routine



CNTDWN:   LCM                     ;Fetch counter
          DCC                     ;Decrement value
          LMC                     ;Return counter to storage
          RET                     ;Exit subroutine

                                  ;N'th precision two's complement (negate)
                                  ;subroutine. Performs a two's complement on the multi-byte
                                  ;registers tarting at the address pointed
                                  ; to by H & L (least significant byte) upon entry.

COMPLM:    LAM                    ;Fetch the least significant byte of the number to ACC
           XRI 377                ;Exclusive OR to complement the byte
           ADI 001                ;Add one to form two's complement of byte
MORCOM:    LMA                    ;Restore the negated byte to memory
           RAR                    ;Save the carry bit
           LDA                    ;In CPU register D
           DCB                    ;Decrement number of bytes (precision) counte
           RTZ                    ;Return to caller when all bytes in number processed
           INL                    ;Else advance the pointe
           LAM                    ;Fetch the next byte of the number to ACC
           XRI 377                ;Exclusive OR to complement the byte
           LEA                    ;Save complemented value in register E temporarily
           LAD                    ;Restore previous carry status to ACC
           RAL                    ;And rotate it out to the carry bit
           LAI 000                ;Clear ACC without disturbing carry status
           ACE                    ;Add in any carry to complemented value
           JMP MORCOM             ;Continue the two's complement procedure as req'd

                                  ;N'th precision subtraction subroutine.
                                  ;Number starting at location pointed to by D & E (least
                                  ;significant byte) is subtracted from number starting at
                                  ;address specified by contents of H & L.
		   
ROTATL:    NDA                    ;CLEAR CARRY FLAG AT THIS ENTRY POINT
ROTL:      LAM                    ;FETCH WORD FROM MEMORY
           RAL                    ;ROTATE LEFT (WITH CARRY)
           LMA                    ;RESTORE ROTATED WORD TO MEMORY
           DCB                    ;DECREMENT "PRECISION" COUNTER
           RTZ                    ;RETURN TO CALLING ROUTINE WHEN DONE
           INL                    ;OTHERWISE ADVANCE PNTR TO NEXT WORD
           JMP ROTL               ;AND ROTATE ACROSS THE HEM WORD STRING

ROTATR:    NDA                    ;CLEAR CARRY FLAG AT THIS ENTRY POINT
ROTR:      LAM                    ;FETCH WORD FROM MEMORY
           RAR                    ;ROTATE RIGHT (WITH CARRY)
           LMA                    ;RESTORE ROTATED WORD TO MEMORY
           DCB                    ;DECREMENT "PRECISION" COUNTER
           RTZ                    ;RETURN TO CALLING ROUTINE WHEN DONE
           DCL                    ;GOING OTHER WAY SO DECREMENT MEM PNTR
           JMP ROTR               ;AND ROTATE ACROSS THE MEM WORD STRING

SUBBER:    NDA                    ;Initialize the carry bit to zero upon entry
SUBTRA:    LAM                    ;Fetch byte from register group A
           CAL SWITCH             ;Switch memory pointer to register group B
           SBM                    ;Subtract byte from group B ftom that in group A
           LMA                    ;Leave result in register group B
           DCB                    ;Decrement number of bytes (precision) counte
           RTZ                    ;Return to caller when all bytes in group processed
           INL                    ;Else advance pointer for register group B
           CAL SWITCH             ;Switch memory pointer back to register group A
           INL                    ;Advance the pointer for register group A
           JMP SUBTRA             ;Continue the multi-byte subtraction operation

           ORG (7*400)+100
DERMSG:    HLT                    ;Attempted divide by zero error message

           ORG (7*400)+110
USERDF:	   HLT                    ;Direct program flow after above error

           ORG (7*400)+120
           cpu 8008new            ; use "new" 8008 mnemonics           
;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. 
; do not echo. return the character in A.
;-----------------------------------------------------------------------------------------
INPUT:      in 0                    ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc INPUT                ; jump if start bit not detected

            ; start bit detected. wait 52 cycles (1/2 bit time)
            mvi e,0                 ; initialize E
            mvi e,0                 ; timing adjustment
            mvi e,0                 ; timing adjustment
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
            mov a,e                 ; move the character from E to A
            mvi e,0fch
            call delay
            mov e,a                 ; move the character from A to E
            mvi a,1
            mvi a,1                 ; timing adjustment
            ; wait 104 cycles.
            mov a,e                 ; restore the character from E to A
            mvi e,0fch
            mvi e,0fch              ; timing adjustment
            call delay
            ori 80h                 ; set the MSB
            ret

getbit:     mov a,e                 ; save the bits in A
            mvi e,0ffh
            mvi e,0ffh              ; timing adjustment
            call delay
            mov e,a                 ; save the bits in E
            in 0                    ; get input from the serial port
            in 0                    ; timing adjustment
            rar                     ; rotate the received bit right into carry
            mov a,e                 ; restore the previously received bits from E to A
            rar                     ; rotate the newly received bit in carry right into the MSB of A
            mov e,a                 ; save the received bits in E
            ret        

            ORG (7*400)+300
;------------------------------------------------------------------------
; sends the character in A out from the serial port at 2400 bps.
;------------------------------------------------------------------------
ECHO:       ani 7fh                 ; clear the most signficant bit
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
            
;------------------------------------------------------------------------
; delay in microseconds = (((255-value in E)*16)+19) * 4 microseconds
;------------------------------------------------------------------------
delay:      inr e
            jnz delay
            ret
            
            cpu 8008                ; return to "old" 8008 mnemonics
            
DUMMY:      RET      

            end FPCONT