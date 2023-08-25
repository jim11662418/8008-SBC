           PAGE 0                ; suppress page headings in AS listing file 
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
; 
; serial I/O at 2400 bps N-8-1 
;=============================================================================================== 
           cpu 8008new           ; use "new" 8008 mnemonics 
           radix 8               ; use octal for numbers 
            
           ORG (1*400)+000 
FPCONT:	   call CRLF2            ;DISPLAY A FEW CR & LF'S FOR I/0 DEVICE 
           call DINPUT           ;LET OPERATOR ENTER A FP DECIMAL NUMBER 
           call SPACES           ;DISPLAY A FEW SPACES AFTER NUMBER 
           mvi l,124             ;SET PWTR TO LSW OF FPACC 
           mov d,h               ;SET D = 0 POR SURE 
           mvi e,170             ;SET PNTR TO TEMP # STORAGE AREA 
           mvi b,004             ;SET PRECISION COUNTER 
           call MOVEIT           ;MOVE FPACC TO TEMP STORAGE AREA 
NVALID:    call INPUT            ;FETCH "OPERATOR" FROM INPUT DEVICE 
           mvi b,000             ;CLEAR REGISTER "B" 
           cpi 253               ;TEST FOR "+" SIGN 
           jz OPERA1             ;GO SET UP FOR "+" SIGN 
           cpi 255               ;IF NOT"+", TEST FOR "-" SIGN 
           jz OPERA2             ;GO SET UP FOR "-" SIGN 
           cpi 330               ;IF NOT ABOVE, TEST FOR "X" (MULT) SIGN 
           jz OPERA3             ;GO SET UP FOR "X" SIGN 
           cpi 257               ;IF NOT ABOVE, TEST FOR "/" (DIV) SIGN 
           jz OPERA4             ;GO SET UP FOR "/" SIGN 
           cpi 377               ;IF NOT ABOVE, TEST FOR "RUBOUT" 
           jnz NVALID            ;IF NONE OF ABOVE, IGNORE INPUT 
           jmp FPCONT            ;IF "RUBOUT" START NEW INPUT SEQUENCE 
OPERA1:    dcr b                 ;SET UP REGISTER "B" BASED ON ABOVE 
           dcr b                 ; "  "     "      "    "   "    " 
OPERA2:    dcr b                 ; "  "     "      "    "   "    " 
           dcr b                 ; "  "     "      "    "   "    " 
OPERA3:    dcr b                 ; "  "     "      "    "   "    " 
           dcr b                 ; "  "     "      "    "   "    " 
 
OPERA4:    mov c,a               ;SAVE "OPERATOR" CHARACTER IN REG "C" 
           mvi a,242             ;*** = NEXT TO LAST LOC IN "LOOKUP" TABLE 
           add b                 ;MODIFY ,"***" BY CONTENTS 0F "B" 
           mvi l,110             ;SET PNTR TO "LOOKUP" TABLE ADDR STORAGE 
           mov m,a               ;PLACE "LOOKUP" ADDR IN STOftAGE LOCATION 
           mov a,c               ;RESTORE "OPERATOR" CHARACTER TO adc c 
           call ECHO             ;DISPLAY THE "OPERATOR" SIGN 
           call SPACES           ;DISPLAY FEW SPACES AFTER "OPERATOR" SIGN 
           call DINPUT           ;LET OPERATOR ENTER 2ND FP DECIMAL NUMBER 
           call SPACES           ;PROVIDE FEW SPACES APTER 2ND NUMBER 
           mvi a,275             ;PLACE ASCII CODE FOR "=" IN ACCUMULATOR 
           call ECHO             ;DISPLAY "." SIGN 
           call SPACES           ;DISPLAY FEW SPACES AFTER "=" SIGN 
           mvi l,170             ;SET POINTER TO TEMP NUMBER STORAGE ARFA 
           mov d,h               ;SET D =0 FOR SURE 
           mvi e,134             ;SET ANOTHER POINTER TO LSW FPOP 
           mvi b,004             ;SET PRECISION COUNTER 
           call MOVEIT           ;MOVE 1ST NUMBER INPUTTED TO FPOP 
           mvi l,110             ;SET PNTR TO "LOOKUP" TABLE ADDR STORAGE 
           mov l,m               ;BRING IN LOW ORDER ADDR OF "LOOKUP" TABLE 
           mvi h,001             ;XXX = PAGE THIS PROGRAM LOCATED ON 
           mov e,m               ;BRING IN AN ADDR STORED IN "LOOKUP" TABLE 
           inr l                 ;RESIDING ON THIS PAGE (XXX) AT LOCATIONS 
           mov d,m               ;"*** + B" AND "*** + B + 1" AND PLACE IT 
           mvi l,224             ;IN REGS "D & E" THEN CHANGE PNTR TO ADDR 
           mov m,e               ;PART OF INSTRUCTION LABELED "RESULT" BE- 
           inr l                 ;LOW AND TRANSFER THE "LOOKUP" TABLE CON- 
           mov m,d               ;TENTS TO BEOMCE THE ADDRESS FOR THE IN- 
           mvi h,000             ;STRUCTION LABELED "RESULT." THEN RESTORE 
           mov d,h               ;REGISTERS "D" AND "H" BACK TO "0" 
           jmp RESULT            ;NOW JUMP TO COMMAND LABELED "RESULT" 
CRLF2:     mvi a,215             ;SUBRTN TO PROVIDE CR & LF'S 
           call ECHO             ;PLACE ASCII CODE FOR CR IN adc c & DISPLAY 
           mvi a,212             ;PLACE ASCII CODE FOR LINE FEED IN adc c 
           call ECHO             ;AND DISPLAY 
           mvi a,215             ;DO IT AGAIN - CODE FOR CR IN adc c 
           call ECHO             ;DISPLAY. IT 
           mvi a,212             ;CODE FOR LF 
           call ECHO             ;DISPLAY IT 
           ret                   ;RETURN TO CALLING ROUTINE 
 
SPACES:    mvi a,240             ;SET UP ASCII CODE FOR SPACE IN adc c 
           call ECHO             ;DISPLAY A SPAC E 
           mvi a,240             ;DO IT AGAIN - CODE FOR SPACE IN adc c 
           call ECHO             ;DISPLAY SPACE 
           ret                   ;RETURN TO CALLING ROUTINE 
RESULT:    call DUMMY            ;CAL RTN AT ADDRESS IN NEXT TWO BYTES1 
           call FPOUT            ;DISPLAY RESULT 
           jmp FPCONT            ;GO BACK'AND GET NEXT PROBLEM I 
 
; TABLE OF OPERATIONS 
;LOOKUP: 
           DB   FPADD&377        ;240 LOW address for start of FPADD subroutine 
           DB   FPADD/400        ;004 PAGE address for start of FPADD subroutine 
           DB   FSUB&377         ;115 LOW address for start of FSUB subroutine 
           DB   FSUB/400         ;005 PAGE address for start of FSUB subroutine 
           DB   FPMULT&377       ;127 LOW address for start of FPMULT subroutine 
           DB   FPMULT/400       ;005 PAGE address for start of FPMULT subroutine 
;ENDLOOKUP:            
           DB   FPDIV&377        ;022 LOW address for start of FPDIV subroutine 
           DB   FPDIV/400        ;006 PAGE address for start of FPMULT subroutine 
 
FPOUT:     mvi l,157             ;Set pointer to decimal exponent storage location 
           mvi m,000             ;Initialize storage location to zero 
           mvi l,126             ;Change pointer to FPACC (number to be outputted) 
           mov a,m               ;And fetch MSW of FPACC 
           ana a                 ;Test the contents of MSW of FPACC 
           jm OUTNEG             ;If most significant bit of MSW is a one, have a minus nr. 
           mvi a,253             ;Else number is positive set ASCII code for space for a 
           jmp AHEAD1            ;Positive number and go display a space 
OUTNEG:    mvi l,124             ;If number in FPACC is negative must negate in orde 
           mvi b,003             ;To display. Set pntr to LSW of FPACC & set prec. cntr. 
           call COMPLM           ;Negate the number in the FPACC to make it positive 
           mvi a,255             ;But load adc c with ASCII code for minus sign 
AHEAD1:    call ECHO             ;Call user display driver to output space or minus sign 
           mvi a,260             ;Load ASCII code for '0' into accumulato 
           call ECHO             ;Call user display driver to output '0' as first characte 
           mvi a,256             ;Number string. Now load ASCII code for decimal point. 
           call ECHO             ;Call user display driver to output '.'as second character. 
           mvi l,127             ;Set pointer to FPACC Exponent 
           mvi a,377             ;Load accumulator with minus one 
           add m                 ;Add value in FPACC Exponent 
           mov m,a               ;Restore compensated exponent value 
 
DECEXT:    jp DECEXD             ;If compen exp is zero or positive, multip MANT by 0.1 
           mvi a,004             ;If compensated exponent is negative 
           add m                 ;Add '4' (decimal) to exponent value 
           jp DECOUT             ;If exponent now zero or positive, output MANTISSA 
           call FPX10            ;Otherwise, multiply MANTISSA by 10 
 
DECREP:    mvi l,127             ;Reset pointer to FPACC Exponent 
           mov a,m               ;Fetch value in exponent 
           ana a                 ;Test value 
           jmp DECEXT            ;Repeat process as required 
DECEXD:     
           call FPD10            ;Multiply FPACC by 0.1 
           jmp DECREP            ;Repeat process as required 
 
                                 ;The next section outputs the mantissa 
                                 ;(or fixed point number) by converting the value remaining 
                                 ;in the FPACC (after the decimal exponent equivalent has 
                                 ;been extracted from the original value if required by the 
                                 ;previous routines) to a string of decirnal digits. 
DECOUT:    mvi e,164             ;Set pointer to LSW of output working registe 
           mov d,h               ;Set D to same page value as H 
           mvi l,124             ;Set pointer to LSW of FPACC 
           mvi b,003             ;Set precision counte 
           call MOVEIT           ;Move value in FPACC to output working registe 
           mvi l,167             ;Set pointer to MSW plus one of output working registe 
           mvi m,000             ;Clear that location to 0 
           mvi l,164             ;Set pointer to LSW of output working registe 
           mvi b,003             ;Set precision counte 
           call ROTATL           ;Rotate register left once to compensate for sign bit 
           call OUTX10           ;Multiply output register by 10, overflow into N4SW+ 1 
COMPEN:    mvi l,127             ;Set pointer back to FPACC Exponent 
           mov b,m               ;Compensate for any remainder in the binary exponent 
           inr b                 ;By performing a rotate right on the output working 
           mov m,b               ;Register until the binary exponent becomes zero 
           jz OUTDIG             ;Go output decimal digits when this loop is finished 
           mvi l,167             ;Binary exponent compensating loop. Setpointe'r to 
           mvi b,004             ;Working register MSW+L. Set precision counter. 
           call ROTATR           ;Rotate working register to the right. 
           jmp COMPEN            ;Repeat loop as required. 
OUTDIG:    mvi l,107             ;Set pointer to output digit counter storage location 
           mvi m,007             ;Initialize to value of seven 
           mvi l,167             ;Change pointer to output working register MSW+L 
           mov a,m               ;Fetch MSW+L byte containing BCD of digit to be 
           ana a                 ;Displayed. Test the contents of this byte. 
           jz ZERODG             ;If zero jump to ZERODG routine. 
OUTDGS:    mvi l,167             ;Reset pointer to working register MSW+L 
           mvi a,260 
           add m 
           call ECHO 
 
DECRDG:    mvi l,107             ;Set pointer to FIXED/FLOAT indicator storage 
           call CNTDWN 
           jz EXPOUT 
           call OUTX10 
           jmp OUTDGS 
 
ZERODG:    mvi l,157             ;If first digit of floating point number is a zero, set 
           call CNTDWN           ;Decrement the value to compensate for skipping 
           mvi l,166             ;Change pointer to MSW of output working registe 
           mov a,m               ;Fetch MSW of output working registe 
           ana a                 ;Test the contents 
           jnz DECRDG            ;If non-zero, continue outputting 
           dcr l                 ;Else decrement pointer to next byte in working registe 
           mov a,m               ;Fetch its contents 
           ana a                 ;Test 
           jnz DECRDG            ;If non-zero, continue outputting 
           dcr l                 ;Else decrement pointer to LSW of working registe 
           mov a,m               ;Fetch its contents 
           ana a                 ;Test 
           jnz DECRDG            ;If non-zero, continue outputting 
           mvi l,157             ;If decimal mantissa is zero, set pointer to decirnal 
           mov m,a               ;Exponent storage and clear it 
           jmp DECRDG            ;Finish outputting 
 
                                 ;Following routine multiplies the binary number in the 
                                 ;output working register by ten to push the most signifi- 
                                 ;cant digit out to the MSW+L byte. 
 
OUTX10:    mvi l,167             ;Set pointer to work ing register M SW+ 1 
           mvi m,000             ;Clear it in preparation for receiving next digit pushed 
           mvi l,164             ;Into it. Change pointer to working register LSW. 
           mov d,h               ;Set up register D to same page as H. 
           mvi e,160             ;Set second pointer to LSW of second working registe 
           mvi b,004             ;Set precision counte 
           call MOVEIT           ;Move first working register into second 
           mvi l,164             ;Reset pointer to LSW of first working registe 
           mvi b,004             ;Set precision counte 
           call ROTATL           ;Rotate contents of first working register left (X 2) 
           mvi l,164             ;Reset pointer to LSW 
           mvi b,004             ;Reset precision counte 
           call ROTATL           ;Rotate contents left again (X 4) 
           mvi l,160             ;Set pointer to LSW of original value in 2'nd registe 
           mvi e,164             ;Set pointer to LSW of rotated value 
           mvi b,004             ;Set precision counte 
           call ADDER            ;Add rotated value to original value (X 5) 
           mvi l,164             ;Reset pointer to LSW of first working registe 
           mvi b,004             ;Set precision counte 
           call ROTATL           ;Rotate contents left again (X 10) 
           ret                   ;Exit to calling routine 
 
EXPOUT: 
           mvi a,305             ;Else, load adc c with ASCII code for letter E. 
           call ECHO             ;Display E for Exponent via user's display driver rtn 
           mvi l,157 
           mov a,m               ;Get decimal exponent value back into adc c 
           ana a                 ;Test again 
           jm EXOUTN             ;If value is negative, skip ahead 
           mvi a,253             ;If positive, load ASCII code for + sign 
           jmp AHEAD2            ;Jump to display the + sign 
EXOUTN:    xri 377               ;When decimal exponent is negative, must negate 
           adi 001               ;Value for display purposes. Perform two's complement 
           mov m,a               ;And restore the negated value to storage location 
           mvi a,255             ;Load ASCII code for minus sign 
AHEAD2:    call ECHO             ;Display the ASCII character in adc c 
           mvi b,000             ;Clear register B 
           mov a,m               ;Fetch the decimal exponent value back into adc c 
SUB12:     sui 012               ;Subtract 10 (decimal) from value in adc c 
           jm TOMUCH             ;Break out of loop when accumulator goes negative 
           mov m,a               ;Else restore value to storage location 
           inr b                 ;Increment register B as a counte 
           jmp SUB12             ;Repeat loop to form tens value of decimal exponent 
TOMUCH:    mvi a,260             ;Load base ASCII value for digit into the accumulato 
           add b                 ;Add to the count in B to forin tens digit of decimal 
           call ECHO             ;Exponent. Display via user's driver subroutine 
           mov a,m               ;Fetch remainder of decimal exponent value 
           adi 260               ;Add in ASCII base value to form final digit 
           call ECHO             ;Display second digit of decirnal exponent 
           ret                   ;Finished outputting. Return to caller. 
 
DINPUT:    mvi h,000             ;** Set H to page of floating point working registers 
           mvi l,150             ;Set L to start of decirnal-to-binary working area 
           xra a                 ;Clear the accumulato 
           mvi b,010             ;Set up a loop counte 
CLRNX2:    mov m,a               ;Deposit zero in working area to initialize 
           inr l                 ;Advance the memory pointe 
           dcr b                 ;Decrement the loop counte 
           jnz CLRNX2            ;Clear working area until loop counter is zero 
           mvi l,103             ;Set pointer to floating point temporary registers and 
           mvi b,004             ;Indicators working area. Set up a loop counter. 
CLRNX3:    mov m,a               ;Deposit zero in working area to initialize 
           inr l                 ;Advance the memory pointe 
           dcr b                 ;Decrement the loop counte 
           jnz CLRNX3            ;Clear working area until loop counter is zero 
           call INPUT            ;Fetch a character from the ASCII chax string buffe 
           cpi 253               ;(Typically the SYMBOL/TOKEN buffer). See if it is 
           jz SECHO              ;Code for + sign. Jump ahead if code for + sign. 
           cpi 255               ;See if code for minus (-) sign. 
           jnz NOTPLM            ;Jump ahead if not code for minus sign. If code fo 
           mvi l,103             ;Minus sign, set pointer to MINUS flag storage location. 
           mov m,a               ;Set the MINUS flag to indicate a minus numbe 
                                 ;Following subroutine is used 
                                 ;to convert decimal charac- 
                                 ;ters to binary fixed point forinat 
                                 ;in a triple-precision format. 
 
SECHO:     call ECHO 
NINPUT:    call INPUT            ;Fetch another character from the ASCII char string 
NOTPLM:    cpi 377               ;See if character represents a period (decimal point) in 
           jz ERASE              ;Input string. Jump ahead if yes. 
           cpi 256 
           jz PERIOD 
           cpi 305               ;If not period, see if code for E as in Exponent 
           jz FNDEXP             ;Jump ahead if yes. 
           cpi 260               ;Else see if code for space. 
           jm ENDINP             ;Ignore space character, go fetch another character. 
           cpi 272               ;If none of the above see if zero byte 
           jp ENDINP             ;Indicating end of input char string. If yes, jumn ahead. 
           mvi l,156             ;For valid digit, set pointer to MSW of temporary 
           mov b,a               ;Decimal to binary holding registers. Save character in C. 
           mvi a,370             ;Form mask for sizing in accumulator. Now see if 
           ana m                 ;Holding register has enough room for the conversion of 
           jnz NINPUT            ;Another digit. Ignore the input if no more room. 
           mov a,b 
           call ECHO 
           mvi l,105             ;If have room in register then set pointer to input digit 
           mov c,m               ;Counter location. Fetch the present value. 
           inr c                 ;Increment it to account for incoming digit. 
           mov m,c               ;Restore updated count to storage location. 
           call DECBIN           ;Call the DECimal to BINary conversion routine to add 
           jmp NINPUT            ;In the new digit in holding registers. Continue inputting. 
PERIOD:    mov b,a               ;Save character code in register B 
           mvi l,106             ;Set pointer to PERIOD indicator storage location 
           mov a,m               ;Fetch value in PERIOD indicato 
           ana a                 ;Exercise CPU flags 
           jnz ENDINP            ;If already have a period then display error message 
           mvi l,105             ;If not, change pointer to digit counter storage location 
           mov m,a               ;Clear the digit counter back to zero 
           inr l                 ;Advance pointer to PERIOD indicato 
           mov m,b               ;Set the PERIOD indicato 
           mov a,b 
           call ECHO 
           jmp NINPUT            ;Continue processing the input character string 
 
ERASE:     mvi a,274 
           call ECHO 
           mvi a,240 
           call ECHO 
           call ECHO 
           jmp DINPUT 
 
FNDEXP:    call ECHO 
           call INPUT            ;Get next character in Exponent 
           cpi 253               ;See if it is code for + sign 
           jz EXECHO             ;Jump ahead if yes. 
           cpi 255               ;If not + sign, see if minus sign 
           jnz NOEXPS            ;If not minus sign then jump ahead 
           mvi l,104             ;For minus sign, set pointer to EXP SIGN indicato 
           mov m,a               ;Set the EXP SIGN indicator for a minus exponent 
 
EXECHO:    call ECHO 
EXPINP:    call INPUT            ;Fetch the next character in the decimal exponent 
NOEXPS:    cpi 377               ;Exercise the CPU flags 
           jz ERASE              ;If character inputted was zero, then end of input string 
           cpi 260               ;If not end of string, check to see 
           jm ENDINP             ;If character represents 
           cpi 272               ;A valid decimal number (0 to 9) 
           jp ENDINP             ;Display error message if not a valid digit at this point! 
           ani 017               ;Else trim the ASCII code to BCD 
           mov b,a               ;And save in register B 
           mvi l,157             ;Set pointer to input exponent storage location 
           mvi a,003             ;Set accumulator equal to three 
           cmp m                 ;See if any previous digit in exponent greater than three 
           jm EXPINP             ;Display error message if yes 
           mov c,m               ;Else save any previous value in register C 
           mov a,m               ;And also place any previous value in accumulato 
           ana a                 ;Clear the carry bit with this instruction 
           ral 
           ral                   ;Single precision multiply by ten algorithm 
           add c 
           ral                   ;Two rotate lefts equals times fou 
           add b                 ;Adding in the digit makes total times five 
           mov m,a               ;Rotating left again equals times ten 
           mvi a,260 
           add b                 ;now add in digit just inputted 
           jmp EXECHO            ;Go get any additional exponent int)ut 
 
ENDINP:    mvi l,103             ;Set pointer to mantissa SIGN indicato 
           mov a,m               ;Fetch the SIGN indicator to the acclimulato 
           ana a                 ;Exercise the CPU flags 
           jz FININP             ;If SIGN indicator is zero, go finish up as nr is positive 
           mvi l,154             ;But, if indicator is non-zero, number is negative 
           mvi b,003             ;Set pntr to LSW of storage registers, set precision ent 
           call COMPLM           ;Negate the triple-precision number in holding registers 
FININP:    mvi l,153             ;Set pointer to input storage LS~V minus one 
           xra a                 ;Clear the accumulato 
           mov d,a               ;Clear the LSW minus one location 
           mov m,a               ;Set register D to floating point working page 
           mvi e,123             ;Set E to address of FPACC LSW minus one 
           mvi b,004             ;Set precision counte 
           call MOVEIT           ;Move number from input register to FPACC 
           mvi b,027 
           call FPNORM           ;Now convert the binary fixed point to floating point 
           mvi l,104             ;Set pointer to Exponent SIGN indicator location 
           mov a,m               ;Fetch the value of the EXP SIGN indicato 
           ana a                 ;Exercise the CPU flags 
           mvi l,157             ;Reset pointer to input exponent storage location 
           jz POSEXP             ;If EXP SIGN indicator zero, exponent is positive 
           mov a,m               ;Else, exponent is negative so must negate 
           xri 377               ;The value in the input exponent storage location 
           adi 001               ;By performing this two's complement 
           mov m,a               ;Restore the negated value to exponent storage location 
POSEXP:    mvi l,106             ;Set pointer to PERIOD indicator storage location 
           mov a,m               ;Fetch the contents of the PERIOD indicato 
           ana a                 ;Exercise the CPU flags 
           jz EXPOK              ;If PERIOD indicator clear, no decimal point involved 
           mvi l,105             ;If have a decimal point, set pointer to digit counte 
           xra a                 ;Storage location. Clear the accumulator. 
           sub m                 ;And get a negated value of the digit counter in adc c 
EXPOK:     mvi l,157             ;Change pointer to input exponent storage location 
           add m                 ;Add this value to negated digit counter value 
           mov m,a               ;Restore new value to storage location 
           jm MINEXP             ;If new value is minus, skip over next subroutine 
           rz                    ;If new value is zero, no further processing required 
 
EXPFIX:    call FPX10            ;Compensated decimal exponent is positive, multiply 
           jnz EXPFIX            ;FPACC by 10, loop until decimal exponent is zero 
           ret                   ;Exit with converted value in FP adc c 
                                 ;Following subroutine will multiply the floating point 
                                 ;binary number stored in FPACC by ten tirnes the 
                                 ;value stored in the deciinal exponent storage location. 
 
FPX10:     mvi e,134             ;Multiply FPACC by 10 subroutine, set pointer to 
           mov d,h               ;FPOP LSW, then set D = zero for sure 
           mvi l,124             ;Set pointer to FP adc c LSW 
           mvi b,004             ;Set precision counter 
           call MOVEIT           ;Move FPACC to FPOP (including exponents) 
           mvi l,127             ;Set pointer to FP adc c exponent 
           mvi m,004             ;Place FP form of 10 (decimal) in FPACC 
           dcr l                 ;Place FP form of 10 (decimal) in FPACC 
           mvi m,120             ;Place FP form of 10 (decimal) in FPACC 
           dcr l                 ;Place FP form of 10 (decimal) in FPACC 
           xra a                 ;Place FP form of 10 (decimal) in FPACC 
           mov m,a               ;Place FP form of 10 (decimal) in FPACC 
           dcr l                 ;Place FP form of 10 (decimal) in FPACC 
           mov m,a               ;Place FP form of 10 (decimal) in FPACC 
           call FPMULT           ;Now multiply original binary number (in FPOP) by ten 
           mvi l,157             ;Set pointer to decimal exponent storage 
           call CNTDWN           ;Decrement decimal exponent value 
           ret                   ;Return to calling program 
 
                                 ;Following subroutine will multiply the floating point 
                                 ;binary number stored in PPACC by 0.1 times the value 
                                 ;(negative) stored in the decimal exponent storage location 
 
MINEXP:	  call FPD10             ;Compensated decimal exponent is minus, multiply 
          jnz MINEXP             ;FPACC by 0.1, loop until decimal exponent is zero 
          ret                    ;Exit with converted value in FP adc c 
 
FPD10:    mvi e,134              ;Multiply FPACC by 0.1 routine, pointer to FPOP LSW 
          mov d,h                ;Set D = '0 ' for sure 
          mvi l,124              ;Set pointer to FP adc c 
          mvi b,004              ;Set pointer to FP adc c 
          call MOVEIT            ;Set precision counter 
          mvi l,127              ;Move FPACC to FPOP (including exponent) 
          mvi m,375              ;Set pointer to FPACC exponent 
          dcr l                  ;Place FP form of 0.1 (decimal) in FPACC 
          mvi m,146              ;Place FP form of 0 .1 (decimal) in FPACC 
          dcr l                  ;Place FP form of 0.1 (decimal) in FPACC 
          mvi m,146              ;Place FP form of 0 .1 (decimal) in FPACC 
          dcr l                  ;Place FP form of 0.1 (decimal) in FPACC 
          mvi m,147              ;Place FP form of 0.1 (decimal) in FPACC 
          call FPMULT            ;Place FP form of 0 .1 (decimal) in FPACC 
          mvi l,157              ;Now multiply original binary number (in FPOP) by 0.1 
          mov b,m                ;Set pointer to decimal exponent storage 
          inr b                  ;Fetch value 
          mov m,b                ;Increment it 
          ret                    ;Restore it to memory 
 
DECBIN:   
           mvi l,153             ;Set pointer to temporary storage location 
           mov a,b               ;Restore character inputted to accumulato 
           ani 017               ;Trim ASCII code to BCD 
           mov m,a               ;Store temporarily 
           mvi e,150             ;Set pointer to working area LSW of multi-byte registe 
           mvi l,154             ;Set another pointer to LSW of conversion registe 
           mov d,h               ;Make sure D set to page of working area 
           mvi b,003             ;Set precision counte 
           call MOVEIT           ;Move original value of conversion register to working 
           mvi l,154             ;Register. Reset pointer to LSW of conversion register. 
           mvi b,003             ;Set precision counte 
           call ROTATL           ;Rotate register left, (Multiplies value by two.) 
           mvi l,154             ;Reset pointer to LSW. 
           mvi b,003             ;Set precision counte 
           call ROTATL           ;Multiply by two again (total now times four). 
           mvi e,154             ;Set pointer to LSW of conversion register. 
           mvi l,150             ;Set pointer to LSW of working register (original value). 
           mvi b,003             ;Set precision counter. 
           call ADDER            ;Add original value to rotated value (now times five). 
           mvi l,154             ;Reset pointer to LSW 
           mvi b,003             ;Set precision counte 
           call ROTATL           ;Multiply by two once more (total now times ten). 
           mvi l,152             ;Set pointer to clear working register locatiotis 
           xra a                 ;Clear the accumulato 
           mov m,a               ;Clear MSW of working registe 
           dcr l                 ;Decrement pointe 
           mov m,a               ;Clear next byte 
           mvi l,153             ;Set pointer to current digit storage location 
           mov a,m               ;Fetch the current digit 
           mvi l,150             ;Change pointer to LSW of working registe 
           mov m,a               ;Deposit the current digit in LSW of working registe 
           mvi e,154             ;Set pointer to conversion register LSW 
           mvi b,003             ;Set precision counte 
           call ADDER            ;Add current digit to conversion register to complete 
           ret 
 
FPNORM:    mov a,b               ;Get CPU register B into adc c to check for special case 
           ana a 
           jz NOEXCO 
           mvi l,127             ;Set L to FPACC Exponent byte 
           mov m,b               ;Else set Exponent of FPACC to 23 decimal 
NOEXCO:    mvi l,126 
           mov a,m               ;Fetch MSW of FPACC into accumulato 
           mvi l,100             ;Change pointer to SIGN indicator storage location 
           ana a 
           jm ACCMIN 
           xra a 
           mov m,a               ;Place the MSW of FPACC there for future reference 
           jmp ACZERT            ;If sign bit not set then jump ahead to do next test 
ACCMIN:    mov m,a 
           mvi b,004             ;If sign bit set, number in FPACC is negative. Set up 
           mvi l,123             ;For two's complement operation 
           call COMPLM           ;And negate the value in the FPACC to make it positive 
ACZERT:    mvi l,126             ;Reset pointer to MSW of FPACC 
           mvi b,004             ;Set precision counter to number of bytes in FPACC 
LOOK0:     mov a,m               ;Plus one. Fetch a byte of the FPACC. 
           ana a                 ;Set CPU flags 
           jnz ACNONZ            ;If find anything then FPACC is not zero 
           dcr l                 ;Else decrement pointer to NSW of FPACC 
           dcr b                 ;Decrement precision counte 
           jnz LOOK0             ;Continue checking to see if FPACC contains anything 
           mvi l,127             ;Until precision counter is zero. If reach here then 
           xra a                 ;Reset pointer to FPACC Exponent. Clear the adc c and 
           mov m,a               ;Clear out the FPACC Exponent. Value of FPACC is zip! 
           ret                   ;Exit to calling routine 
ACNONZ:    mvi l,123             ;If FPACC has any value set pointer to LSW minus one 
           mvi b,004             ;Set precision counter to number of bytes in FPACC 
           call ROTATL           ;Plus one for special cases. Rotate the contents of the 
           mov a,m               ;FPACC to the LEFT. Pointer will be set to MSW afte 
           ana a                 ;Rotate ops. Fetch MSW and see if have anything in 
           jm ACCSET             ;Most significant bit position. If so, have rotated enough 
           inr l                 ;If not, advance pointer to FPACC Exponent. Fetch 
           call CNTDWN 
           jmp ACNONZ            ;Continue rotating ops to normalize the FPACC 
ACCSET:    mvi l,126             ;Set pntr to FPACC MSW. Now must provide room fo 
           mvi b,003             ;Sign bit in nonnalized FPACC. Set precision counter. 
           call ROTATR           ;Rotate the FPACC once to the right now. 
           mvi l,100             ;Set the pointer to SIGN indicator storage location 
           mov a,m               ;Fetch the original sign of the FPACC 
           ana a                 ;Set CPU flags 
           rp                    ;If original sign of FPACC was positive, can exit now. 
           mvi l,124             ; However, if original sign was negative, must now restore 
           mvi b,003             ;The FPACC to negative by performing two's comple- 
           call COMPLM           ;Ment on FPACC. Return to caring rtn via COMPLM. 
           ret 
 
FPADD:     mvi l,126             ;Set pointer to MSW of FP adc c 
           mvi b,003             ;Set loop counter 
CKZACC:    mov a,m               ;Fetch part of FPACC 
           ana a                 ;Set flags after loading operation 
           jnz NONZAC            ;Finding anything means FPACC not zero 
           dcr b                 ;If that part equals zero, decrement loop counter 
           jz MOVOP              ;If FPACC equals zero , move FPOP into FPACC 
           dcr l                 ;Not finished checking, decrement pointer 
           jmp CKZACC            ;And test next part of FPACC 
MOVOP:     call SWITCH           ;Save pointer to LSW of FPACC 
           mov h,d               ;Set H equal to zero for sure 
           mvi l,134             ;Set pointer to LSW of FPOP 
           mvi b,004             ;Set a loop counter 
           call MOVEIT           ;Move FPOP into FPACC as answer 
           ret                   ;Exit FP add d subroutine 
NONZAC:    mvi l,136             ;Set pointer to MSW of FPOP 
           mvi b,003             ;Set loop counter 
CKZOP:     mov a,m               ;Set flags after load operation 
           ana a                 ;If not zero then have a number 
           jnz CKEQEX            ;If zero, decrement loop counter 
           dcr b                 ;Exit subroutine if FPOP equals zero 
           rz                    ;Else decreme nt pointer to next part of FPOP 
           dcr l                 ;And continue testing for zero FPOP 
           jmp CKZOP             ;Check for equal expo nents 
CKEQEX:    mvi l,127             ;Get FPACC exponent 
           mov a,m               ;Change pointer to FPOP exponent 
           mvi l,137             ;Compare exponents 
           cmp m                 ;If same can setup for add d operation 
           jz SHACOP 
           xri 377               ;If not same, then two 's complement 
           adi 001               ;The value of the FP adc c exponent 
           add m                 ;And add in FPOP exponent 
           jp SKPNEG             ;If + then go directly to alignment test 
           xri 377               ;If negative perform two's complement 
           adi 001               ;On the result 
SKPNEG: 
           cpi 030               ;N ow see if result greater than 27 octal 
           jm LINEUP             ;If not can perform alignment 
           mov a,m               ;If not alignable, get FPOP exponent 
           mvi l,127             ;Set pointer to FPACC exponent 
           sub m                 ;Subtract FPACC exponent from FPOP exponent 
           rm                    ;FPACC exponent greater so just exit routine 
           mvi l,124             ;FPOP was greater, set pointer to FPACC LSW 
           jmp MOVOP             ;Go put FPOP into FPACC and then exit routine 
LINEUP: 
           mov a,m               ;Align FPACC and FPOP, get FPOP exponent 
           mvi l,127             ;Change pointer to FPACC exponent 
           sub m                 ;Subtract FPACC exponent from FPOP exponent 
           jm SHIFTO             ;FPACC greater so go to shift operand 
           mov c,a               ;FPOP greater so save difference 
MORACC: 
           mvi l,127             ;Pointer to FP adc c exponent 
           call SHLOOP           ;Call shift loop subroutine 
           dcr c                 ;Decrement difference counter 
           jnz MORACC            ;Continue aligning if not done 
           jmp SHACOP            ;Setup for add d operation 
SHIFTO: 
           mov c,a               ;Shift FPOP routine, save difference count (negative) 
MOROP:     mvi l,137             ;Set pointer to FPOP exponent 
           call SHLOOP           ;Call shift loop subroutine 
           inr c                 ;Increment difference counter 
           jnz MOROP             ;Shift again if not done 
SHACOP: 
           mvi l,123             ;First clear out extra room, setup pointer 
           mvi m,000             ;to FP A CC LSW + 1 and set it to zero 
           mvi l,127             ;N ow prepare to shift FP adc c right once 
           call SHLOOP           ;Set pointer and then call shift loop routine 
           mvi l,137             ;Shift FPOP right once, first set pointer 
           call SHLOOP           ;Call shift loop subroutine 
           mov d,h               ;Setup pointers, set T) equal to zero for sure 
           mvi e,123             ;Pointer to LSW of FPACC 
           mvi b,004             ;Set precision counter 
           call ADDER            ;Add FPACC to FPOP using quad-precision 
           mvi b,000             ;Set B for standard normalization procedure 
           call FPNORM           ;Normalize the result of the addition 
           ret                   ;Exit FP add d subroutine with result in FP ACe 
 
SHLOOP:    mov b,m               ;Sh ifting loop for alignment 
           inr b                 ;Fetch exponent into B and increment 
           mov m,b               ;Return increment value to memory 
           dcr l                 ;Decrement the pointer 
           mvi b,004             ;Set a counter 
            
FSHIFT:    mov a,m               ;Get MSW of floating point number 
           ana a                 ;Set flags after loading operation 
           jm BRING1             ;If number is minus, need to shift in a '1 ' 
           call ROTATR           ;Otherwise perform N'th-precision rotate 
           ret                   ;Exit FSHIFT subroutine 
 
BRING1:    ral                   ;Save '1 ' in carry bit 
           call ROTR             ;Do ROT ATE RIGHT without clearing carry bit 
           ret                   ;Exit FSHIFT SUbroutine               
 
MOVEIT:    mov a,m               ;Fetch a word from memory string' A' 
           inr l                 ;Advance 'A' string pointer 
           call SWITCH           ;Switch pointer to string 'B' 
           mov m,a               ;Put word from string 'A' into 'B' 
           inr l                 ;Advance B string pointer 
           call SWITCH           ;Switch pointer back to string 'A' 
           dcr b                 ;Decrement counter 
           rz                    ;Return to calling routine when counter is zero 
           jmp MOVEIT            ;Otherwise continue moving operation 
 
FSUB:      mvi l,124             ;Set pointer to LSW of FPACC 
           mvi b,003             ;Set precision counter 
           call COMPLM           ;Perform two's complement on FPACC 
           jmp FPADD             ;Subtraction accomplished now by adding! 
 
                                 ;The first part of the FLOATING POINT MULTIPLI- 
                                 ;CATION subroutine calls a subroutine to check the 
                                 ;original signs of the numbers that are to be multi- 
                                 ;plied and perform working register clearing functions. 
                                 ;Next the exponents of the numbers to be multiplied 
                                 ;are added together. 
 
FPMULT:    call CKSIGN           ;Call routine to set up registers & ck signs of numbers 
ADDEXP:    mvi l,137             ;Set pointer to FPOP Exponent 
           mov a,m               ;Fetch FPOP Exponent into the accumulato 
           mvi l,127             ;Change pointer to FPACC Exponent 
           add m                 ;Add FPACC Exponent to FPOP Exponent 
           adi 001               ;Add one more to total for algorithm compensation 
           mov m,a               ;Store result in FPACC Exponent location 
SETMCT:    mvi l,102             ;Change pointer to bit counter storage location 
           mvi m,027             ;Initialize bit counter to 23 decimal 
 
                                 ;Next portion of the FPMULT routine is the iinplernen- 
                                 ;tation of the algorithm illustrated in the flow chart 
                                 ;above. This portion multiplies the values of the two 
                                 ;mantissas. The final value is rounded off to leave the 
                                 ;23 most significant bits as the answer that is stored 
                                 ;back in the FPACC. 
 
MULTIP:    mvi l,126             ;Set pointer to MSW of FPACC mantissa 
           mvi b,003             ;Set precision counte 
           call ROTATR           ;Rotate FPACC (multiplier) RIGHT into carry bit 
           cc ADOPPP             ;If carry is a one, add multiplicand to partial-product 
           mvi l,146             ;Set pointer to partial-product most significant byte 
           mvi b,006             ;Set precision counter (p-p register is double length) 
           call ROTATR           ;Shift partial-product RIGHT 
           mvi l,102             ;Set pointer to bit counter storage location 
           call CNTDWN 
           jnz MULTIP            ;If have not multiplied for 23 (deciinal) bits, keep going 
           mvi l,146             ;If have done 23 (decimal) bits, set pntr to p-p MSW 
           mvi b,006             ;Set precision counter (for double length) 
           call ROTATR           ;Shift partial-product once more to the RIGHT 
           mvi l,143             ;Set pointer to access 24'th bit in partial-product 
           mov a,m               ;Fetch the byte containing the 24'th bit 
           ral                   ;Position the 24'th bit to be MSB in the accumulato 
           mov a,a 
           ana a                 ;Set the CPU flags after to rotate operation and test to 
           cm MROUND             ;See if 24'th bit of p-p is a ONE. If so, must round-off 
           mvi l,123             ;Now set up pointers 
           call SWITCH 
           mov h,d 
           mvi l,143             ;From the partial-product location 
           mvi b,004             ;To the FPACC 
	 
EXMLDV:    call MOVEIT           ;Perform the transfer from p-p to FPACC 
           mvi b,000             ;Set up CPU register B to indicate regular normalization 
           call FPNORM           ;Normalize the result of multiplication 
           mvi l,101             ;Now set the pointer to the original SIGNS indicato 
           mov a,m               ;Fetch the indicato 
           ana a                 ;Exercise the CPU flags 
           rnz                   ;If indicator is non-zero, answer is positive can exit he 
           mvi l,124 
           mvi b,003 
           call COMPLM 
           ret 
 
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
 
CKSIGN:    call CLRWRK 
           mvi l,101             ;Set pointer to start of partial-product working area 
           mvi m,001             ;** Set H to proper page 
           mvi l,126 
           mov a,m 
           ana a 
           jm NEGFPA 
 
OPSGNT:    mvi l,136             ;Set pointer to MSW of FPOP 
           mov a,m               ;Fetch MSW of mantissa into accumulato 
           ana a                 ;Test flags 
           rp                    ;Return to caller if number in FPOP is positive 
           mvi l,101             ;Else change pointer to M/D SIGNS indicato 
           call CNTDWN 
           mvi l,134             ;Set pointer to LSW of FPOP 
           mvi b,003             ;Set precision counte 
           call COMPLM           ;Two's complement value of FPOP & return to calle 
           ret 
 
NEGFPA:    mvi l,101             ;Set pointer to M/D SIGNS indicato 
           call CNTDWN 
           mvi l,124             ;Set pointer to LSW of FPACC 
           mvi b,003             ;Set precision counte 
           call COMPLM           ;Two's complement value of FPACC 
           jmp OPSGNT            ;Proceed to check sign of FPOP 
 
CLRWRK:    mvi l,140 
           mvi b,010 
           xra a 
CLRNEX:    mov m,a               ;Now clear out locations for the partial-product 
           dcr b 
           jz CLROPL 
           inr l                 ;Working registers 
           jmp CLRNEX 
CLROPL:    mvi b,004             ;Set a loop counte 
           mvi l,130             ;Set up pointe 
CLRNX1:    mov m,a               ;Clear out some extra registers so that the 
           dcr b                 ;Perform clearing ops until loop counte 
           rz 
           inr l 
           jmp CLRNX1            ;Is zero 
 
                                 ;The following subroutine adds the double length (six regis 
                                 ;multiplicand in FPOP to the partial-product register when 
                                 ;called on by the multiplication algorithm. 
 
ADOPPP:    mvi e,141             ;Pointer to LSW of partial-product 
           mov d,h               ;On same page as FPOP 
           mvi l,131             ;LSIV of FPOP which contains extended multiplicand 
           mvi b,006             ;Set precision counter (double length working registers) 
           call ADDER            ;Add multiplicand to partial-product & return to calle 
           ret 
 
MROUND:    mvi b,003             ;Set up precision counte 
           mvi a,100             ;Prepare to add one to 24'th bit of partial-product 
           add m                 ;Add one to the 24'th bit of the partial-product 
CROUND:    mov m,a               ;Restore the updated byte to memory 
           inr l                 ;Advance the memory pointer to next most significant 
           mvi a,000             ;Byte of partial-product, then clear adc c without 
           adc m                 ;Disturbing carry bit. Now perform add with carry to 
           dcr b                 ;Propagate any rounding in the partial-product registers. 
           jnz CROUND            ;If cotinter is not zero continue propagating any carry 
           mov m,a               ;Restore final byte to memory 
           ret                   ;Exit to calling routine 
 
FPDIV:     call CKSIGN           ;Call routine to set up registers & ck signs of numbers 
           mvi l,126             ;Set pointer to MSW of FPACC (divisor) 
           mvi a,000             ;Fetch MSW of FPACC to accumulato 
           cmp m 
           jnz SUBEXP 
           dcr l 
           cmp m 
           jnz SUBEXP 
           dcr l 
           cmp m 
           jz DERROR             ;If MSW of FPACC is zero go display 'DZ' error message 
SUBEXP:    mvi l,137             ;Set pointer to FPOP (dividend) Exponent 
           mov a,m               ;Get FPOP Exponent into accumulato 
           mvi l,127             ;Change pointer to FPACC (divisor) Exponent 
           sub m                 ;Subtract divisor exponent from dividend exponent 
           adi 001               ;Add one for algorithm compensation 
           mov m,a               ;Place result in FPACC Exponent 
SETDCT:    mvi l,102             ;Set pointer to bit counter storage location 
           mvi m,027             ;Initialize bit counter to 23 decimal 
 
                                 ;Main division algorithm for mantissas 
 
DIVIDE:    call SETSUB           ;Go subtmct divisor from dividend 
           jm NOGO               ;If result is negative then place a zero bit in quotient 
           mvi e,134             ;If result zero or positive then move remainder afte 
           mvi l,131             ;Subtraction from working area to become new dividend 
           mvi b,003             ;Set up moving pointers and initialize precision counte 
           call MOVEIT           ;Perform the transfe 
           mvi a,001             ;Place a one into least significant bit of accumulato 
           rar                   ;And rotate it out into the carry bit 
           jmp QUOROT            ;Proceed to rotate the carry bit into the current quotient 
 
NOGO:      mvi a,000             ;When result is negative, put a zero in the carry bit, then 
           rar 
QUOROT:    mvi l,144             ;Set up pointer to LSW of quotient registe 
           mvi b,003             ;Set precision counte 
           call ROTL             ;Rotate carry bit into quotient by using special entry to 
           mvi l,134             ;ROTATL subroutine. Now set up pointer to dividend 
           mvi b,003             ;LSW and set precision counte 
           call ROTATL           ;Rotate the current dividend to the left 
           mvi l,102             ;Set pointer to bit counter storage location 
           call CNTDWN 
           jnz DIVIDE            ;If bit counter is not zero, continue division process 
           call SETSUB           ;After 23 (decimal) bits, do subtraction once more fo 
           jp DVEXIT             ;Possible rounding. Jump ahead if no rounding required. 
           mvi l,144             ;If rounding required set pointer to LSW of quotient 
           mov a,m               ;Fetch LSW of quotient to accumulato 
           adi 001               ;Add one to 23rd bit of quotient 
           mov m,a               ;Restore updated LSW of quotient 
           mvi a,000             ;Clear accumulator without disturbing carry bit 
           inr l                 ;Advance pointer to next significant byte of quotient 
           adc m                 ;Propagate any carry as part of rounding process 
           mov m,a               ;Restore the updated byte of quotient 
           mvi a,000             ;Clear adc c again without disturbing carry bit 
           inr l                 ;Advance pointer to MSW of quotient 
           adc m                 ;Propagate any carry to finish rounding process 
           mov m,a               ;Restore the updated byte of quotient 
           jp DVEXIT             ;If most significant bit of quotient is zero, go finish up 
           mvi b,003             ;If not, set precision counte 
           call ROTATR           ;And rotate quotient to the right to clear the sign bit 
           mvi l,127             ;Set pointer to FPACC Exponent 
           mov b,m               ;Fetch FPACC exponent 
           inr l                 ;Increment the value to compensate for the rotate right 
           mov m,b               ;Restore the updated exponent value 
DVEXIT:    mvi l,144             ;Set up pointers 
           mvi e,124             ;To transfer the quotient into the FPACC 
           mvi b,003             ;Set precision counte 
                                 ;THIS IS A CORRECTION FOUND IN THE NOTES 
           jmp EXMLDV            ;And exit through FPMULT routine at EXMLDV 
 
                                 ;Subroutine to subtract divisor from dividend. Used by 
                                 ;main DIVIDE subroutine. 
 
SETSUB:    mvi l,131             ;Set pointer to LSW of working area 
           call SWITCH 
           mov h,d               ;On same page as FPACC 
           mvi l,124             ;Set pointer to LSW of FPACC (divisor) 
           mvi b,003             ;Set precision counte 
           call MOVEIT           ;Perform transfe 
           mvi e,131             ;Reset pointer to LSW of working area (now divisor) 
           mvi l,134             ;Reset pointer to LSW of FPOP (dividend) 
           mvi b,003             ;Set precision counte 
           call SUBBER           ;Subtract divisor from dividend 
           mov a,m               ;Get MSW of the result of the subtraction operations 
           ana a                 ;Exercise CPU flags 
           ret                   ;Return to caller with status 
 
DERROR:    call DERMSG 
           jmp USERDF 
 
ADDER:     ana a                 ;Initialize the carry bit to zero upon entry 
ADDMOR:    mov a,m               ;Fetch byte from register group A 
           call SWITCH           ;Switch memory pointer to register group B 
           adc m                 ;Add byte from A to byte from B with carry 
           mov m,a               ;Leave result in register group B 
           dcr b                 ;Decrement number of bytes (precision) counte 
           rz                    ;Return to caller when all bytes in group processed 
           inr l                 ;Else advance pointer for register group B 
           call SWITCH           ;Switch memory pointer back to register group A 
           inr l                 ;Advance the pointer for register group A 
           jmp ADDMOR            ;Continue the multi-byte addition operation 
 
                                 ;Subroutine to exchange the contents of H & L with 
                                 ;D & E. 
 
SWITCH:    mov c,h               ;Transfer register H to C temporarily 
           mov h,d               ;Place value of D into H 
           mov d,c               ;Now put former H from C into D 
           mov c,l               ;Transfer register L to C temporarily 
           mov l,e               ;Place value of E into L 
           mov e,c               ;Now put former L from C into E 
           ret                   ;Exit to calling routine 
 
 
 
CNTDWN:   mov c,m                ;Fetch counter 
          dcr c                  ;Decrement value 
          mov m,c                ;Return counter to storage 
          ret                    ;Exit subroutine 
 
                                 ;N'th precision two's complement (negate) 
                                 ;subroutine. Performs a two's complement on the multi-byte 
                                 ;registers tarting at the address pointed 
                                 ; to by H & L (least significant byte) upon entry. 
 
COMPLM:    mov a,m               ;Fetch the least significant byte of the number to adc c 
           xri 377               ;Exclusive OR to complement the byte 
           adi 001               ;Add one to form two's complement of byte 
MORCOM:    mov m,a               ;Restore the negated byte to memory 
           rar                   ;Save the carry bit 
           mov d,a               ;In CPU register D 
           dcr b                 ;Decrement number of bytes (precision) counte 
           rz                    ;Return to caller when all bytes in number processed 
           inr l                 ;Else advance the pointe 
           mov a,m               ;Fetch the next byte of the number to adc c 
           xri 377               ;Exclusive OR to complement the byte 
           mov e,a               ;Save complemented value in register E temporarily 
           mov a,d               ;Restore previous carry status to adc c 
           ral                   ;And rotate it out to the carry bit 
           mvi a,000             ;Clear adc c without disturbing carry status 
           adc e                 ;Add in any carry to complemented value 
           jmp MORCOM            ;Continue the two's complement procedure as req'd 
 
                                 ;N'th precision subtraction subroutine. 
                                 ;Number starting at location pointed to by D & E (least 
                                 ;significant byte) is subtracted from number starting at 
                                 ;address specified by contents of H & L. 
		    
ROTATL:    ana a                 ;CLEAR CARRY FLAG AT THIS ENTRY POINT 
ROTL:      mov a,m               ;FETCH WORD FROM MEMORY 
           ral                   ;ROTATE LEFT (WITH CARRY) 
           mov m,a               ;RESTORE ROTATED WORD TO MEMORY 
           dcr b                 ;DECREMENT "PRECISION" COUNTER 
           rz                    ;RETURN TO CALLING ROUTINE WHEN DONE 
           inr l                 ;OTHERWISE ADVANCE PNTR TO NEXT WORD 
           jmp ROTL              ;AND ROTATE ACROSS THE HEM WORD STRING 
 
ROTATR:    ana a                 ;CLEAR CARRY FLAG AT THIS ENTRY POINT 
ROTR:      mov a,m               ;FETCH WORD FROM MEMORY 
           rar                   ;ROTATE RIGHT (WITH CARRY) 
           mov m,a               ;RESTORE ROTATED WORD TO MEMORY 
           dcr b                 ;DECREMENT "PRECISION" COUNTER 
           rz                    ;RETURN TO CALLING ROUTINE WHEN DONE 
           dcr l                 ;GOING OTHER WAY SO DECREMENT MEM PNTR 
           jmp ROTR              ;AND ROTATE ACROSS THE MEM WORD STRING 
 
SUBBER:    ana a                 ;Initialize the carry bit to zero upon entry 
SUBTRA:    mov a,m               ;Fetch byte from register group A 
           call SWITCH           ;Switch memory pointer to register group B 
           sbb m                 ;Subtract byte from group B ftom that in group A 
           mov m,a               ;Leave result in register group B 
           dcr b                 ;Decrement number of bytes (precision) counte 
           rz                    ;Return to caller when all bytes in group processed 
           inr l                 ;Else advance pointer for register group B 
           call SWITCH           ;Switch memory pointer back to register group A 
           inr l                 ;Advance the pointer for register group A 
           jmp SUBTRA            ;Continue the multi-byte subtraction operation 
 
           ORG (7*400)+100 
DERMSG:    HLT                   ;Attempted divide by zero error message 
 
           ORG (7*400)+110 
USERDF:	   HLT                   ;Direct program flow after above error 
 
           ORG (7*400)+120 
           cpu 8008new           ; use "new" 8008 mnemonics            
;----------------------------------------------------------------------------------------- 
; wait for a character from the serial port.  
; do not echo. return the character in A. 
;----------------------------------------------------------------------------------------- 
INPUT:      in 0                 ; get input from serial port 
            rar                  ; rotate the received serial bit right into carry 
            jc INPUT             ; jump if start bit not detected 
 
                                 ; start bit detected. wait 52 cycles (1/2 bit time) 
            mvi e,0              ; initialize E 
            mvi e,0              ; timing adjustment 
            mvi e,0              ; timing adjustment 
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
            mov a,e              ; move the character from E to A 
            mvi e,0fch 
            call delay 
            mov e,a              ; move the character from A to E 
            mvi a,1 
            mvi a,1              ; timing adjustment 
                                 ; wait 104 cycles. 
            mov a,e              ; restore the character from E to A 
            mvi e,0fch 
            mvi e,0fch           ; timing adjustment 
            call delay 
            ori 80h              ; set the MSB 
            ret 
 
getbit:     mov a,e              ; save the bits in A 
            mvi e,0ffh 
            mvi e,0ffh           ; timing adjustment 
            call delay 
            mov e,a              ; save the bits in E 
            in 0                 ; get input from the serial port 
            in 0                 ; timing adjustment 
            rar                  ; rotate the received bit right into carry 
            mov a,e              ; restore the previously received bits from E to A 
            rar                  ; rotate the newly received bit in carry right into the MSB of A 
            mov e,a              ; save the received bits in E 
            ret         
 
            ORG (7*400)+300 
;------------------------------------------------------------------------ 
; sends the character in A out from the serial port at 2400 bps. 
;------------------------------------------------------------------------ 
ECHO:       ani 7fh              ; clear the most signficant bit 
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
             
;------------------------------------------------------------------------ 
; delay in microseconds = (((255-value in E)*16)+19) * 4 microseconds 
;------------------------------------------------------------------------ 
delay:      inr e 
            jnz delay 
            ret 
             
            cpu 8008new          ; return to "old" 8008 mnemonics 
             
DUMMY:      ret       
 
            end FPCONT
