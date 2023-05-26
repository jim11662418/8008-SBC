            PAGE 0             ; suppress page headings in ASW listing file
;;; This is a 1K Floating-Point Concentric Circle Grapher for the 8008
;;; written in March 2013 by Mark G. Arnold. 
;;; It is intended to be an example for other 1K graphing routines
;;; (e.g., Mandelbrot>>08H)&0FFH, but this one seems about the easiest
;;; "interesting" picture to generate with FP arithmetic.
 
;;; The Floating point routines were taken from
;;; SCELBAL by Mark G. Arnold (MGA) and Nat Wadsworth  
;;;
;;;  Copyright 1975 Scelbi Computer Consulting, Inc.
;;;  All rights reserved
;;;
;;; MGA gives permission to use SCELBAL for 
;;; educational, historical, non-commercial purposes.
;;; Versions of this have been circulating on the web since
;;; about 2000; this version is authorized by MGA (Mar 2012)
;;; with the understanding no warranty is expressed or implied.
;;; As stated in the original, "no responsibility is assumed for
;;; for inaccuracies or for the success or failure of
;;; various applications to which the information herein
;;; may be applied."
;;; 
;;; SCELBAL is the only open-source, floating-point 
;;; high-level language ever implemented on Intel's first
;;; general-purpose microprocessor, the 8008.  It was
;;; published in book form:
;;;
;;;  SCELBAL: A Higher-Level Language for 8008/8080 Systems
;;;
;;; This comment must be incorporated with any version of SCELBAL
;;; downloaded, distributed, posted or disemenated.
;;;
;;; edited to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Jim Loos 05/22/2023
;;;
;;; Jump to 000H to start.

            cpu 8008                ; use "old" 8008 mnemonics
            radix 8                 ; all numbers are octal
            
            org 000
start:      JMP CONCIRC            
    

;;; Page one of SCELBAL has many constants and variables, of which the following are
;;; needed here:
	
OLDPG1:     ORG (0*400)+100
            DB 000,000		    ; SIGN INDICATOR
            DB 000		        ; INPUT DIGIT COUNTER
            DB 000		        ; TEMP STORATE
            DB 000		        ; OUTPUT DIGIT COUNTER
            DB 000 		        ; FP MODE INDICATOR
            DB 7 dup (0)	    ; NOT ASSIGNED (SHOULD BE 01 111-117>>08H)&0FFH
            DB 000,000,000,000	; FPACC EXTENSION
            DB 000,000,000,000	; FPACC LSW, NSW, MSW, EXPONENT
            DB 000,000,000,000	; FPOP  Extension
            DB 000,000,000,000	; FPOP  LSW, NSW, MSW, EXPONENT
            DB 000,000,000,000	; FLOATING POINT WORKING AREA
            DB 000,000,000,000	; (SHOULD BE AT 01 140-01-167>>08H)&0FFH
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000,000,000
            DB 10 dup (0)		; NOT ASSIGNED (SHOULD BE 01 170-01 177>>08H)&0FFH
            DB 000,000,000,000	; TEMPORARY REGISTER STORAGE AREA (D,E,H&L>>08H)&0FFH
            DB 4 dup (0)		; NOT ASSIGNED (01 204-01 207>>08H)&0FFH
            DB 000,000,120,004	; STORES FLOATING POINT CONSTANT +10.0
            DB 147,146,146,375	; STORES FLOATING POINT CONSTANT +0.1
            DB 000		        ; GETINP COUNTER

;  next are the plotting variables:
X1:         DB 0,0,0,0
Y1:         DB 0,0,0,0
;X:         DB 0,0,0,0
;Y:         DB 0,0,0,0
T:          DB 0,0,0,0

;  these are chosen to make nice picture (esp. for Mandelbrot, but also for circles>>08H)&0FFH
STEPY:     DB 147,146,146,375	; STORES FLOATING POINT CONSTANT +0.1
STEPX:     DB 147,146,146,374	; STORES FLOATING POINT CONSTANT +0.05
STARTY:    DB 000,000,300,001	; STORES FLOATING POINT CONSTANT -1.0
STARTX:    DB 000,000,300,002	; STORES FLOATING POINT CONSTANT -2.0
;
CNTX:      DB 000               ;integer controls how mnay times X1 loop executes 
CNTY:      DB 000               ;integer controls how many times Y1 loop executes 

; here is pseudo-code for the plotting driver routine 
;
;  FOR Y1=-1 TO 1 STEP 0.1
;    PRINT ascii code letter corresponding to row 
;    FOR X1=-2.0 TO 1 STEP 0.05
;     IF floating-point-exponent(X*X+Y*Y>>08H)&0FFH is odd THEN PRINT " ";
;                                                ELSE PRINT "*";
;    NEXT X1
;    PRINT
;  NEXT Y1
;

;  most of the LHIs could probably be eliminated because 
;  all the data is on the same page
	
CONCIRC:   LLI STARTY&0FFH      ;  Y1=STARTY
           LHI (STARTY>>08H)&0FFH
           CAL FLOAD
           LLI Y1&0FFH
           LHI (Y1>>08H)&0FFH
           CAL FSTORE
           LLI CNTY&0FFH        ; CNTY = 21
           LHI (CNTY>>08H)&0FFH
           LMI 025
Y1LOOP:    LAM                  ; print chr(64)
           ADI 300
           CAL ECHO

           LLI STARTX&0FFH      ;  X1 = STARTX
           LHI (STARTX>>08H)&0FFH
           CAL FLOAD
           LLI X1&0FFH
           LHI (X1>>08H)&0FFH
           CAL FSTORE
           LLI CNTX&0FFH        ; cntx = 61
           LHI (CNTX>>08H)&0FFH
           LMI 075
X1LOOP:
           LLI X1&0FFH          ; COMPUTE X1*X1+Y1*Y1
           LHI (X1>>08H)&0FFH
           CAL FLOAD
           LLI X1&0FFH
           LHI (X1>>08H)&0FFH
           CAL OPLOAD
           CAL FPMULT
           LLI T&0FFH
           LHI (T>>08H)&0FFH
           CAL FSTORE
           LLI Y1&0FFH
           LHI (Y1>>08H)&0FFH
           CAL FLOAD
           LLI Y1&0FFH
           LHI (Y1>>08H)&0FFH
           CAL OPLOAD
           CAL FPMULT
           LLI T&0FFH
           LHI (T>>08H)&0FFH
           CAL OPLOAD
           CAL FPADD
           LLI 127              ; get FP exponent
           LHI (OLDPG1>>08H)&0FFH
           LAM
           NDI 001              ;check if it is odd or even
           LAI 240              ; ' '
           JFZ MANDONE          ;if odd print " " 
           LAI 252              ;otherwise, print "*"
MANDONE:   CAL ECHO
           LLI X1&0FFH          ; X1 = X1 + STEPX
           LHI (X1>>08H)&0FFH
           CAL FLOAD
           LLI STEPX&0FFH
           LHI (STEPX>>08H)&0FFH
           ;LLI 236
           ;LHI 000
           CAL OPLOAD 
           CAL FPADD
           LLI X1&0FFH
           LHI (X1>>08H)&0FFH
           CAL FSTORE
           LLI CNTX&0FFH        ; CNTX--
           LHI (CNTX>>08H)&0FFH
           LAM
           SUI 001
           LMA
           JFZ X1LOOP           ; inner loop until CNTX == 0

           CAL CRLF             ; print CR and LF
           LLI Y1&0FFH          ; Y1 = Y1 + STEPY
           LHI (Y1>>08H)&0FFH
           CAL FLOAD
           LLI STEPY&0FFH
           LHI (STEPY>>08H)&0FFH
           CAL OPLOAD 
           CAL FPADD
           LLI Y1&0FFH
           LHI (Y1>>08H)&0FFH
           CAL FSTORE
           LLI CNTY&0FFH   ; CNTY-- 
           LHI (CNTY>>08H)&0FFH
           LAM
           SUI 001
           LMA
           JFZ Y1LOOP           ; outer loop until CNTY==0
           HLT 
           JMP CONCIRC             ; do it again

FPFLT:     LBI 027                ;For fixed to float set CPU register B to 23 decimal
FPNORM:    LAB                    ;Get CPU register B into ACC to check for special case
           LHI (OLDPG1>>08H)&0FFH ;** Set H to page of FPACC
           LLI 127                ;Set L to FPACC Exponent byte
           NDA                    ;Set CPU flags to test what was in CPU register B
           JTZ NOEXC0             ;If B was zero then do standard normalization
           LMB                    ;Else set Exponent of FPACC to 23 decimal
NOEXC0:    DCL                    ;Change pointer to MSW of FPACC
           LAM                    ;Fetch MSW of FPACC into accumulator
           LLI 100                ;Change pointer to SIGN indicator storage location
           LMA                    ;Place the MSW of FPACC there for future reference
           NDA                    ;Set CPU flags to test MSW of FPACC
           JFS ACZERT             ;If sign bit not set then jump ahead to do next test
           LBI 004                ;If sign bit set, number in FPACC is negative. Set up
           LLI 123                ;For two's complement operation
           CAL COMPLM             ;And negate the value in the FPACC to make it positive
ACZERT:    LLI 126                ;Reset pointer to MSW of FPACC
           LBI 004                ;Set precision counter to number of bytes in FPACC
LOOK0:     LAM                    ;Plus one. Fetch a byte of the FPACC.
           NDA                    ;Set CPU flags
           JFZ ACNONZ             ;If find anything then FPACC is not zero
           DCL                    ;Else decrement pointer to NSW of FPACC
           DCB                    ;Decrement precision counter
           JFZ LOOK0              ;Continue checking to see if FPACC contains anything
           LLI 127                ;Until precision counter is zero. If reach here then
           XRA                    ;Reset pointer to FPACC Exponent. Clear the ACC and
           LMA                    ;Clear out the FPACC Exponent. Value of FPACC is zip!
           RET                    ;Exit to calling routine
ACNONZ:    LLI 123                ;If FPACC has any value set pointer to LSW minus one
           LBI 004                ;Set precision counter to number of bytes in FPACC
           CAL ROTATL             ;Plus one for special cases. Rotate the contents of the
           LAM                    ;FPACC to the LEFT. Pointer will be set to MSW after
           NDA                    ;Rotate ops. Fetch MSW and see if have anything in
           JTS ACCSET             ;Most significant bit position. If so, have rotated enough
           INL                    ;If not, advance pointer to FPACC Exponent. Fetch
           LBM                    ;The value of the Exponent and decrement it by one
           DCB                    ;To compensate for the rotate left of the mantissa
           LMB                    ;Restore the new value of the Exponent
           JMP ACNONZ             ;Continue rotating ops to normalize the FPACC
ACCSET:    LLI 126                ;Set pntr to FPACC MSW. Now must provide room for
           LBI 003                ;Sign bit in nonnalized FPACC. Set precision counter.
           CAL ROTATR             ;Rotate the FPACC once to the right now.
RESIGN:    LLI 100                ;Set the pointer to SIGN indicator storage location
           LAM                    ;Fetch the original sign of the FPACC
           NDA                    ;Set CPU flags
           RFS                    ;If original sign of FPACC was positive, can exit now.

FPCOMP:    LLI 124                ; However, if original sign was negative, must now restore
           LBI 003                ;The FPACC to negative by performing two's comple-
           JMP COMPLM             ;Ment on FPACC. Return to caring rtn via COMPLM.

                                  ;Floating point ADDITION. Adds contents of FPACC to
                                  ;FPOP and leaves result in FPACC. Routine first checks
                                  ;to see if either register contains zero. If so addition
                                  ;result is already present!
FPADD:     LLI 126                ;Set L to point to MSW of FPACC
           LHI (OLDPG1>>08H)&0FFH ;** Do same for register H
           LAM                    ;Fetch MSW of FPACC to accumulator
           NDA                    ;Set CPU flags after loading op
           JFZ NONZAC             ;If accumulator non-zero then FPACC has some value
MOVOP:     LLI 124                ;But, if accumulator was zero then normalized FPACC
           LDH                    ;Must also be zero. Thus answer to addition is simply the
           LEL                    ;Value in FPOP. Set up pointers to transfer contents of
           LLI 134                ;FPOP to FPACC by pointing to the LSW of both
           LBI 004                ;Registers and perform the transfer. Then exit to calling
           JMP MOVEIT             ;Routine with answer in FPACC via MOVEIT.
NONZAC:    LLI 136                ;If FPACC was non-zero then check to see if FPOP has
           LAM                    ;Some value by obtaining MSW of FPOP
           NDA                    ;Set CPU flags after loading op. If MSW zero then
           RTZ                    ;Normalized FPOP must be zero. Answer is in FPACC!

                                  ;If neither FPACC or FPOP was zero then must perform
                                  ;addition operation. Must first check to see if two num-
                                  ;bers are within significant mnge. If not, largest number
                                  ;is answer. If numbers within range, then must align ex-
                                  ;ponents before perforrning the addition of the man-
                                  ;tissa.
CKEQEX:    LLI 127                ;Set pointer to FPACC Exponent storage location.
           LAM                    ;Fetch the Exponent value to the accumulator.
           LLI 137                ;Change the pointer to the FPOP Exponent
           CPM                    ;Compare the values of the exponents. If they are the
           JTZ SHACOP             ;Same then can immediately proceed to add operations.
           LBA                    ;If not the same, store FPACC Exponent size in regis B
           LAM                    ;Fetch the FPOP Exponent size into the ACC
           SBB                    ;Subtract the FPACC Exponent from the FPOP Exp.
           JFS SKPNEG             ;If result is positive jump over the next few instructions
           LBA                    ;If result was negative, store the result in B
           XRA                    ;Clear the accumulator
           SBB                    ;Subtract register B to negate the original value
SKPNEG:    CPI 030                ;See if difference is less than 24 decimal.
           JTS LINEUP             ;If so, can align exponents. Go do it.
           LAM                    ;If not, find out which number is largest. Fetch FPOP
           LLI 127                ;Exponent into ACC. Change pointer to FPACC Exp.
           SUM                    ;Subtract FPACC from FPOP. If result is negative then
           RTS                    ;was larger. Return with answer in FPACC.
           LLI 124                ;If result was positive, larger value in FPOP. Set pointers
           JMP MOVOP              ;To transfer FPOP into FPACC and then exit to caller.
LINEUP:    LAM                    ;Fetch FPOP Exponent into accumulator.
           LLI 127                ;Change pointer to FPACC Exponent.
           SUM                    ;Subtract FPACC Exponent from FPOP Exponent. If
           JTS SHIFT0             ;Result is negative FPACC is larger. Go shift FPOP.
           LCA                    ;If result positive FPOP larger, must shift FPACC. Store
MORACC:    LLI 127                ;Difference count in C. Reset pointer to FPACC Exp
           CAL SHLOOP             ;Call the SHift LOOP to rotate FPACC mantissa RIGHT
           DCC                    ;And INCREMENT Exponent. Decr difference counter
           JFZ MORACC             ;Continue rotate operations until diff counter is zero
           JMP SHACOP             ;Go do final alignment and perform addition process
SHIFT0:    LCA                    ;Routine to shift FPOP. Set difference count into reg. C
MOROP:     LLI 137                ;Set pointer to FPOP Exponent.
           CAL SHLOOP             ;Call the SHift LOOP to rotate FPOP mantissa RIGHT
           INC                    ;And INCREMENT Exponent. Then incr difference cntr
           JFZ MOROP              ;Continue rotate opemtions until diff counter is zero
;;; The below two instructions are changed by PATCH NR.1
;;;SHACOP:    LLI 123             ;Set pointer to FPACC LSW minus one to provide extra
;;;           LMI 000             ;Byte for addition ops. Clear that location to zero.
SHACOP:	   CAL PATCH1		      ; patch 1 inserts a few lines at 30-000
	       LAA
	
;;;        LLI 133
;;;        LMI 000                ;THIS IS PATCH #1
           LLI 127                ;Change pointer to FPACC Exponent
           CAL SHLOOP             ;Rotate FPACC mantissa RIGHT & Increment Exponent
           LLI 137                ;Change pointer to FPOP Exponent
           CAL SHLOOP             ;Rotate FPOP mantissa RIGHT & Increment Exponent
           LDH                    ;Rotate ops provide room for overflow. Now set up
           LEI 123                ;Pointers to LSW minus one for both FPACC & FPOP
           LBI 004                ;(FPOP already set after SHLOOP>>08H)&0FFH. Set precision counter
           CAL ADDER              ;Call quad precision ADDITION subroutine.
           LBI 000                ;Set CPU register B to indicate standard normalization
           JMP FPNORM             ;Go normalize the result and exit to caller.
SHLOOP:    LBM                    ;Shifting loop. First fetch Exponent currently being
           INB                    ;Pointed to and Increment the value by one.
           LMB                    ;Return the updated Exponent value to memory.
           DCL                    ;Decrement the pointer to mantissa portion MSW
           LBI 004                ;Set precision counter
FSHIFT:    LAM                    ;Fetch MSW of mantissa
           NDA                    ;Set CPU flags after load ops
           JFS ROTATR             ;If MSB not a one can do normal rotate ops
BRING1:    RAL                    ;If MSB is a one need to set up carrv bit for the negative
           JMP ROTR               ;Number case. Then make special entry to ROTATR sub

PATCH1:	   LLI 123
	       LMI 000
	       LLI 133
	       LMI 000
	       RET

                                  ;The following subroutine moves the contents of a string
                                  ;of memory locations from the address pointed to by
                                  ;CPU registers H & L to the address specified by the con-
                                  ;tents of registers D & E when the routine is entered. The
                                  ;process continues until the counter in register B is zero.
MOVEIT:    LAM                    ;Fetch a word from memory string A
           INL                    ;Advance A string pointer
           CAL SWITCH             ;Switch pointer to string B
           LMA                    ;Put word from string A into string B
           INL                    ;Advance B string pointer
           CAL SWITCH             ;Switch pointer back to string A
           DCB                    ;Decrement loop counter
           RTZ                    ;Return to calling routine when counter reaches zero
           JMP MOVEIT             ;Else continue transfer operations

                                  ;The following subroutine SUBTRACTS the
                                  ;contents of the FLOATING POINT ACCUMULATOR from the
                                  ;contents of the FLOATING POINT OPERAND and
                                  ;leaves the result in the FPACC. The routine merely
                                  ;negates the value in the FPACC and then goes to the
                                  ;FPADD subroutine just presented.
FPSUB:     LLI 124                ;Set L to address of LSW of FPACC
           LHI (OLDPG1>>08H)&0FFH ;** Set H to page of FPACC
           LBI 003                ;Set precision counter
           CAL COMPLM             ;Two's complement the value in the FPACC
           JMP FPADD              ;Now go add the negated value to perform subtraction!

                                  ;The first part of the FLOATING POINT MULTIPLI-
                                  ;CATION subroutine calls a subroutine to check the
                                  ;original signs of the numbers that are to be multi-
                                  ;plied and perform working register clearing functions.
                                  ;Next the exponents of the numbers to be multiplied
                                  ;are added together.
FPMULT:    CAL CKSIGN             ;Call routine to set up registers & ck signs of numbers
ADDEXP:    LLI 137                ;Set pointer to FPOP Exponent
           LAM                    ;Fetch FPOP Exponent into the accumulator
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
           LBI 003                ;Set precision counter
           CAL ROTATR             ;Rotate FPACC (multiplier>>08H)&0FFH RIGHT into carry bit
           CTC ADOPPP             ;If carry is a one, add multiplicand to partial-product
           LLI 146                ;Set pointer to partial-product most significant byte
           LBI 006                ;Set precision counter (p-p register is double length>>08H)&0FFH
           CAL ROTATR             ;Shift partial-product RIGHT
           LLI 102                ;Set pointer to bit counter storage location
           LCM                    ;Fetch current value of bit counter
           DCC                    ;Decrement the value of the bit counter
           LMC                    ;Restore the updated bit counter to its storage location
           JFZ MULTIP             ;If have not multiplied for 23 (deciinal>>08H)&0FFH bits, keep going
           LLI 146                ;If have done 23 (decimal>>08H)&0FFH bits, set pntr to p-p MSW
           LBI 006                ;Set precision counter (for double length>>08H)&0FFH
           CAL ROTATR             ;Shift partial-product once more to the RIGHT
           LLI 143                ;Set pointer to access 24'th bit in partial-product
           LAM                    ;Fetch the byte containing the 24'th bit
           RAL                    ;Position the 24'th bit to be MSB in the accumulator
           NDA                    ;Set the CPU flags after to rotate operation and test to
           CTS MROUND             ;See if 24'th bit of p-p is a ONE. If so, must round-off
           LLI 123                ;Now set up pointers
           LEL                    ;To perform transfer
           LDH                    ;Of the multiplication results
           LLI 143                ;From the partial-product location
           LBI 004                ;To the FPACC
	
EXMLDV:    CAL MOVEIT             ;Perform the transfer from p-p to FPACC
           LBI 000                ;Set up CPU register B to indicate regular normalization
           CAL FPNORM             ;Normalize the result of multiplication
           LLI 101                ;Now set the pointer to the original SIGNS indicator
           LAM                    ;Fetch the indicator
           NDA                    ;Exercise the CPU flags
           RFZ                    ;If indicator is non-zero, answer is positive, can exit her
           JMP FPCOMP             ;If not, answer must be negated, exit via 2's complement.

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
                                  ;different.>>08H)&0FFH
CKSIGN:    LLI 140                ;Set pointer to start of partial-product working area
           LHI (OLDPG1>>08H)&0FFH ;** Set H to proper page
           LBI 010                ;Set up a loop counter in CPU register B
           XRA                    ;Clear the accumulator

CLRNEX:    LMA                    ;Now clear out locations for the partial-product
           INL                    ;Working registers
           DCB                    ;Until the loop counter
           JFZ CLRNEX             ;Is zero
CLROPL:    LBI 004                ;Set a loop counter
           LLI 130                ;Set up pointer
CLRNX1:    LMA                    ;Clear out some extra registers so that the
           INL                    ;FPOP may be extended in length
           DCB                    ;Perform clearing ops until loop counter
           JFZ CLRNX1             ;Is zero
           LLI 101                ;Set pointer to M/D SIGNS indicator storage location
           LMI 001                ;Set initial value of SIGNS indicator to plus one
           LLI 126                ;Change pointer to MSW of FPACC
           LAM                    ;Fetch MSW of mantissa into accumulator
           NDA                    ;Test flags
           JTS NEGFPA             ;If MSB in MSW of FPACC is a one, number is negative
OPSGNT:    LLI 136                ;Set pointer to MSW of FPOP
           LAM                    ;Fetch MSW of mantissa into accumulator
           NDA                    ;Test flags
           RFS                    ;Return to caller if number in FPOP is positive
           LLI 101                ;Else change pointer to M/D SIGNS indicator
           LCM                    ;Fetch the value in the SIGNS indicator
           DCC                    ;Decrement the value by one
           LMC                    ;Restore the new value back to storage location
           LLI 134                ;Set pointer to LSW of FPOP
           LBI 003                ;Set precision counter
           JMP COMPLM             ;Two's complement value of FPOP & return to caller
NEGFPA:    LLI 101                ;Set pointer to M/D SIGNS indicator
           LCM                    ;Fetch the value in the SIGNS indicator
           DCC                    ;Decrement the value by one
           LMC                    ;Restore the new value back to storage location
           LLI 124                ;Set pointer to LSW of FPACC
           LBI 003                ;Set precision counter
           CAL COMPLM             ;Two's complement value of FPACC
           JMP OPSGNT             ;Proceed to check sign of FPOP

                                  ;The following subroutine adds the double length (six regis
                                  ;multiplicand in FPOP to the partial-product register when
                                  ;called on by the multiplication algorithm.
ADOPPP:    LEI 141                ;Pointer to LSW of partial-product
           LDH                    ;On same page as FPOP
           LLI 131                ;LSIV of FPOP which contains extended multiplicand
           LBI 006                ;Set precision counter (double length working registers>>08H)&0FFH
           JMP ADDER              ;Add multiplicand to partial-product & return to caller

MROUND:    LBI 003                ;Set up precision counter
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

ADDER:     NDA                    ;Initialize the carry bit to zero upon entry
ADDMOR:    LAM                    ;Fetch byte from register group A
           CAL SWITCH             ;Switch memory pointer to register group B
           ACM                    ;Add byte from A to byte from B with carry
           LMA                    ;Leave result in register group B
           DCB                    ;Decrement number of bytes (precision>>08H)&0FFH counter
           RTZ                    ;Return to caller when all bytes in group processed
           INL                    ;Else advance pointer for register group B
           CAL SWITCH             ;Switch memory pointer back to register group A
           INL                    ;Advance the pointer for register group A
           JMP ADDMOR             ;Continue the multi-byte addition operation

                                  ;N'th precision two's complement (negate>>08H)&0FFH
                                  ;subroutine. Performs a two's complement on the multi-byte
                                  ;registers tarting at the address pointed
                                  ; to by H & L (least significant byte>>08H)&0FFH upon entry.
COMPLM:    LAM                    ;Fetch the least significant byte of the number to ACC
           XRI 377                ;Exclusive OR to complement the byte
           ADI 001                ;Add one to form two's complement of byte
MORCOM:    LMA                    ;Restore the negated byte to memory
           RAR                    ;Save the carry bit
           LDA                    ;In CPU register D
           DCB                    ;Decrement number of bytes (precision>>08H)&0FFH counter
           RTZ                    ;Return to caller when all bytes in number processed
           INL                    ;Else advance the pointer
           LAM                    ;Fetch the next byte of the number to ACC
           XRI 377                ;Exclusive OR to complement the byte
           LEA                    ;Save complemented value in register E temporarily
           LAD                    ;Restore previous carry status to ACC
           RAL                    ;And rotate it out to the carry bit
           LAI 000                ;Clear ACC without disturbing carry status
           ACE                    ;Add in any carry to complemented value
           JMP MORCOM             ;Continue the two's complement procedure as req'd

                                  ;N'th precision rotate left subroutine. Rotates a multi-
                                  ;byte number left starting at the address initially
                                  ;specified by the contents of CPU registers H & L upon
                                  ;subroutine entry (LSW>>08H)&0FFH. First entry point will clear
                                  ;the carry bit before beginning rotate operations. Second
                                  ;entry point does not clear the carry bit.
ROTATL:    NDA                    ;Clear the carry bit at this entry point
ROTL:      LAM                    ;Fetch a byte from memory
           RAL                    ;Rotate it left (bring carry into LSB, push MSB to carry>>08H)&0FFH
           LMA                    ;Restore rotated word to memory
           DCB                    ;Decrement precision counter
           RTZ                    ;Exit to caller when finished
           INL                    ;Else advance pointer to next byte
           JMP ROTL               ;Continue rotate left operations

                                  ;N'th precision rotate
                                  ;right subroutine. Opposite of
                                  ;above subroutine.
ROTATR:    NDA                    ;Clear the carry bit at this entry point
ROTR:      LAM                    ;Fetch a byte from memory
           RAR                    ;Rotate it right (carry into MSB, LSB to carry>>08H)&0FFH
           LMA                    ;Restore rotated word to memory
           DCB                    ;Decrement precision counter
           RTZ                    ;Exit to caller when finished
           DCL                    ;Else decrement pointer to next byte
           JMP ROTR               ;Continue rotate right operations

                                  ;N'th precision subtraction subroutine.
                                  ;Number starting at location pointed to by D & E (least
                                  ;significant byte>>08H)&0FFH is subtracted from number starting at
                                  ;address specified by contents of H & L.
SUBBER:    NDA                    ;Initialize the carry bit to zero upon entry
SUBTRA:    LAM                    ;Fetch byte from register group A
           CAL SWITCH             ;Switch memory pointer to register group B
           SBM                    ;Subtract byte from group B ftom that in group A
           LMA                    ;Leave result in register group B
           DCB                    ;Decrement number of bytes (precision>>08H)&0FFH counter
           RTZ                    ;Return to caller when all bytes in group processed
           INL                    ;Else advance pointer for register group B
           CAL SWITCH             ;Switch memory pointer back to register group A
           INL                    ;Advance the pointer for register group A
           JMP SUBTRA             ;Continue the multi-byte subtraction operation

                                  ;The next subroutine will transfer the four byte
                                  ;register string (generally a number in floating point
                                  ;format>>08H)&0FFH from the starting address pointed to by CPU
                                  ;registers H & L when the subroutine is entered to
                                  ;the FPACC (floating point accumulator registers>>08H)&0FFH.
FLOAD:     LDI (OLDPG1>>08H)&0FFH ;** Set page address of FPACC
           LEI 124                ;Set address of least signficant byte of FPACC
           LBI 004                ;Set precision counter to four bytes (mantissa bytes
           JMP MOVEIT             ;Plus Exponent>>08H)&0FFH and exit via the transfer routine

                                  ;The next several subroutines are used to perform
                                  ;floating pojnt register loading and transfer operations.
FSTORE:    LEL                    ;Transfer contents of register L to E
           LDH                    ;Transfer contents of register H to D
           LLI 124                ;Set L to least significant byte of FPACC mantissa
           LHI (OLDPG1>>08H)&0FFH ;** Set page to FPACC storage area
           JMP SETIT              ;Go transfer FPACC contents to area pointed to by D&E
OPLOAD:    LDI (OLDPG1>>08H)&0FFH ;** Set page to FPOP storage area
           LEI 134                ;Set pointer to least significant byte of FPOP
SETIT:     LBI 004                ;Set precision counter. Transfer from H & L area to
           JMP MOVEIT             ;Locations pointed to by D & E


                                  ;Subroutine to exchange the contents of H & L with
                                  ;D & E.
SWITCH:    LCH                    ;Transfer register H to C temporarily
           LHD                    ;Place value of D into H
           LDC                    ;Now put former H from C into D
           LCL                    ;Transfer register L to C temporarily
           LLE                    ;Place value of E into L
           LEC                    ;Now put former L from C into E
           RET                    ;Exit to calling routine

CRLF:      LAI 215                ;Load ASCII code for carriage-return into ACC
           CAL ECHO               ;Call user provided display driver subroutine
           LAI 212                ;Load ASCII code for line-feed into ACC

            cpu 8008new             ; use "new" 8008 mnemonics
            radix 10                ; use base 10 for numbers

OUTPORT     equ 08H                 ; serial output port address            
;------------------------------------------------------------------------        
; character output subroutine
; sends the character in A out from the serial port.
; transmits 1 start bit, 8 data bits and 1 stop at 2400 bps.
; uses A and B.
; returns with the original character in A
;------------------------------------------------------------------------
ECHO:       ani 7FH                 ; mask off the most significant bit of the character
            mov b,a                 ; save the character from A to B
            xra a                   ; clear A for the start bit
            out OUTPORT             ; send the start bit
            mov a,b                 ; restore the character from B to A 
            mov a,b                 ; timing adjustment
            mvi b,0FDH              ; timing adjustment
            mvi b,0FDH              ; timing adjustment        
            call delay              ; timing adjustment
            
            ; send bits 0 through 7
            call putbit             ; transmit bit 0
            call putbit             ; transmit bit 1
            call putbit             ; transmit bit 2
            call putbit             ; transmit bit 3
            call putbit             ; transmit bit 4
            call putbit             ; transmit bit 5
            call putbit             ; transmit bit 6
            call putbit             ; transmit bit 7            

            ; send the stop bit 
            mov b,a                 ; save the character from A to B
            mvi a,1                 ; '1' for the stop bit
            out OUTPORT             ; send the stop bit 
            mov a,b                 ; restore the original character from B to A
            ori 80H                 ; restore the most significant bit of the character
            mvi b,0FCH              ; timing adjustment
            call delay              ; timing adjustment
            ret                     ; return to caller

putbit:     out OUTPORT             ; output the least significant bit of the character in A
            mvi b,0FDH              ; timing adjustment
            mvi b,0FDH              ; timing adjustment
            call delay              ; timing adjustment
            rrc                     ; shift the character in A right
            ret
            
;------------------------------------------------------------------------        
; delay in microseconds = (((255-value in B)*16)+19) * 4 microseconds
;------------------------------------------------------------------------        
delay:      inr b
            jnz delay
delay1:     ret

            cpu 8008                ; use "old" 8008 mnemonics
            radix 8                 ; use base 8 for numbers

            end start

