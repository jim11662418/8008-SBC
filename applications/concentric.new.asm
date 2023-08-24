            PAGE 0               ; suppress page headings in ASW listing file 
;;; This is a 1K Floating-Point Concentric Circle Grapher for the 8008 
;;; written in March 2013 by Mark G. Arnold. 
;;; It is intended to be an example for other 1K graphing routines 
;;; (e.g., Mandelbrot), but this one seems about the easiest 
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
;;; edited to use 'new' Intel 8008 mnemonics by Jim Loos 08/23/2023 
;;; 
;;; Jump to 000H to start. 
;;; 
;;; serial I/O at 2400 bps N-8-1 
 
 
            cpu 8008new          ; use 'new' Intel 8008 mnemonics
            radix 8              ; all numbers are octal 
 
            org 000 
start:      jmp CONCIRC 
 
 
;;; Page one of SCELBAL has many constants and variables, of which the following are 
;;; needed here: 
 
OLDPG1:     ORG (0*400)+100 
            DB 000,000           ; SIGN INDICATOR 
            DB 000               ; INPUT DIGIT COUNTER 
            DB 000               ; TEMP STORATE 
            DB 000               ; OUTPUT DIGIT COUNTER 
            DB 000               ; FP MODE INDICATOR 
            DB 7 dup (0)         ; NOT ASSIGNED (SHOULD BE 01 111-117) 
            DB 000,000,000,000   ; FPACC EXTENSION 
            DB 000,000,000,000   ; FPACC LSW, NSW, MSW, EXPONENT 
            DB 000,000,000,000   ; FPOP  Extension 
            DB 000,000,000,000   ; FPOP  LSW, NSW, MSW, EXPONENT 
            DB 000,000,000,000   ; FLOATING POINT WORKING AREA 
            DB 000,000,000,000   ; (SHOULD BE AT 01 140-01-167) 
            DB 000,000,000,000 
            DB 000,000,000,000 
            DB 000,000,000,000 
            DB 000,000,000,000 
            DB 10 dup (0)        ; NOT ASSIGNED (SHOULD BE 01 170-01 177) 
            DB 000,000,000,000   ; TEMPORARY REGISTER STORAGE AREA (D,E,H&L) 
            DB 4 dup (0)         ; NOT ASSIGNED (01 204-01 207) 
            DB 000,000,120,004   ; STORES FLOATING POINT CONSTANT +10.0 
            DB 147,146,146,375   ; STORES FLOATING POINT CONSTANT +0.1 
            DB 000               ; GETINP COUNTER 
 
;  next are the plotting variables: 
X1:         DB 0,0,0,0 
Y1:         DB 0,0,0,0 
;X:         DB 0,0,0,0 
;Y:         DB 0,0,0,0 
T:          DB 0,0,0,0 
 
;  these are chosen to make nice picture (esp. for Mandelbrot, but also for circles) 
STEPY:     DB 147,146,146,375    ; STORES FLOATING POINT CONSTANT +0.1 
STEPX:     DB 147,146,146,374    ; STORES FLOATING POINT CONSTANT +0.05 
STARTY:    DB 000,000,300,001    ; STORES FLOATING POINT CONSTANT -1.0 
STARTX:    DB 000,000,300,002    ; STORES FLOATING POINT CONSTANT -2.0 
; 
CNTX:      DB 000                ;integer controls how mnay times X1 loop executes 
CNTY:      DB 000                ;integer controls how many times Y1 loop executes 
 
; here is pseudo-code for the plotting driver routine 
; 
;  FOR Y1=-1 TO 1 STEP 0.1 
;    PRINT ascii code letter corresponding to row 
;    FOR X1=-2.0 TO 1 STEP 0.05 
;     IF floating-point-exponent(X*X+Y*Y) is odd THEN PRINT " "; 
;                                                ELSE PRINT "*"; 
;    NEXT X1 
;    PRINT 
;  NEXT Y1 
; 
 
;  most of the LHIs could probably be eliminated because 
;  all the data is on the same page 
 
CONCIRC:   mvi l,STARTY&0FFH     ;  Y1=STARTY 
           mvi h,(STARTY>>08H)&0FFH 
           call FLOAD 
           mvi l,Y1&0FFH 
           mvi h,(Y1>>08H)&0FFH 
           call FSTORE 
           mvi l,CNTY&0FFH       ; CNTY = 21 
           mvi h,(CNTY>>08H)&0FFH 
           mvi m,025 
Y1LOOP:    mov a,m               ; print chr(64) 
           adi 300 
           call ECHO 
 
           mvi l,STARTX&0FFH     ;  X1 = STARTX 
           mvi h,(STARTX>>08H)&0FFH 
           call FLOAD 
           mvi l,X1&0FFH 
           mvi h,(X1>>08H)&0FFH 
           call FSTORE 
           mvi l,CNTX&0FFH       ; cntx = 61 
           mvi h,(CNTX>>08H)&0FFH 
           mvi m,075 
X1LOOP: 
           mvi l,X1&0FFH         ; COMPUTE X1*X1+Y1*Y1 
           mvi h,(X1>>08H)&0FFH 
           call FLOAD 
           mvi l,X1&0FFH 
           mvi h,(X1>>08H)&0FFH 
           call OPLOAD 
           call FPMULT 
           mvi l,T&0FFH 
           mvi h,(T>>08H)&0FFH 
           call FSTORE 
           mvi l,Y1&0FFH 
           mvi h,(Y1>>08H)&0FFH 
           call FLOAD 
           mvi l,Y1&0FFH 
           mvi h,(Y1>>08H)&0FFH 
           call OPLOAD 
           call FPMULT 
           mvi l,T&0FFH 
           mvi h,(T>>08H)&0FFH 
           call OPLOAD 
           call FPADD 
           mvi l,127             ; get FP exponent 
           mvi h,(OLDPG1>>08H)&0FFH 
           mov a,m 
           ani 001               ;check if it is odd or even 
           mvi a,240             ; ' ' 
           jnz MANDONE           ;if odd print " " 
           mvi a,252             ;otherwise, print "*" 
MANDONE:   call ECHO 
           mvi l,X1&0FFH         ; X1 = X1 + STEPX 
           mvi h,(X1>>08H)&0FFH 
           call FLOAD 
           mvi l,STEPX&0FFH 
           mvi h,(STEPX>>08H)&0FFH 
                                 ;LLI 236 
                                 ;LHI 000 
           call OPLOAD 
           call FPADD 
           mvi l,X1&0FFH 
           mvi h,(X1>>08H)&0FFH 
           call FSTORE 
           mvi l,CNTX&0FFH       ; CNTX-- 
           mvi h,(CNTX>>08H)&0FFH 
           mov a,m 
           sui 001 
           mov m,a 
           jnz X1LOOP            ; inner loop until CNTX == 0 
 
           call CRLF             ; print CR and LF 
           mvi l,Y1&0FFH         ; Y1 = Y1 + STEPY 
           mvi h,(Y1>>08H)&0FFH 
           call FLOAD 
           mvi l,STEPY&0FFH 
           mvi h,(STEPY>>08H)&0FFH 
           call OPLOAD 
           call FPADD 
           mvi l,Y1&0FFH 
           mvi h,(Y1>>08H)&0FFH 
           call FSTORE 
           mvi l,CNTY&0FFH       ; CNTY-- 
           mvi h,(CNTY>>08H)&0FFH 
           mov a,m 
           sui 001 
           mov m,a 
           jnz Y1LOOP            ; outer loop until CNTY==0 
           HLT 
           jmp CONCIRC           ; do it again 
 
FPFLT:     mvi b,027             ;For fixed to float set CPU register B to 23 decimal 
FPNORM:    mov a,b               ;Get CPU register B into adc c to check for special case 
           mvi h,(OLDPG1>>08H)&0FFH;** Set H to page of FPACC 
           mvi l,127             ;Set L to FPACC Exponent byte 
           ana a                 ;Set CPU flags to test what was in CPU register B 
           jz NOEXC0             ;If B was zero then do standard normalization 
           mov m,b               ;Else set Exponent of FPACC to 23 decimal 
NOEXC0:    dcr l                 ;Change pointer to MSW of FPACC 
           mov a,m               ;Fetch MSW of FPACC into accumulator 
           mvi l,100             ;Change pointer to SIGN indicator storage location 
           mov m,a               ;Place the MSW of FPACC there for future reference 
           ana a                 ;Set CPU flags to test MSW of FPACC 
           jp ACZERT             ;If sign bit not set then jump ahead to do next test 
           mvi b,004             ;If sign bit set, number in FPACC is negative. Set up 
           mvi l,123             ;For two's complement operation 
           call COMPLM           ;And negate the value in the FPACC to make it positive 
ACZERT:    mvi l,126             ;Reset pointer to MSW of FPACC 
           mvi b,004             ;Set precision counter to number of bytes in FPACC 
LOOK0:     mov a,m               ;Plus one. Fetch a byte of the FPACC. 
           ana a                 ;Set CPU flags 
           jnz ACNONZ            ;If find anything then FPACC is not zero 
           dcr l                 ;Else decrement pointer to NSW of FPACC 
           dcr b                 ;Decrement precision counter 
           jnz LOOK0             ;Continue checking to see if FPACC contains anything 
           mvi l,127             ;Until precision counter is zero. If reach here then 
           xra a                 ;Reset pointer to FPACC Exponent. Clear the adc c and 
           mov m,a               ;Clear out the FPACC Exponent. Value of FPACC is zip! 
           ret                   ;Exit to calling routine 
ACNONZ:    mvi l,123             ;If FPACC has any value set pointer to LSW minus one 
           mvi b,004             ;Set precision counter to number of bytes in FPACC 
           call ROTATL           ;Plus one for special cases. Rotate the contents of the 
           mov a,m               ;FPACC to the LEFT. Pointer will be set to MSW after 
           ana a                 ;Rotate ops. Fetch MSW and see if have anything in 
           jm ACCSET             ;Most significant bit position. If so, have rotated enough 
           inr l                 ;If not, advance pointer to FPACC Exponent. Fetch 
           mov b,m               ;The value of the Exponent and decrement it by one 
           dcr b                 ;To compensate for the rotate left of the mantissa 
           mov m,b               ;Restore the new value of the Exponent 
           jmp ACNONZ            ;Continue rotating ops to normalize the FPACC 
ACCSET:    mvi l,126             ;Set pntr to FPACC MSW. Now must provide room for 
           mvi b,003             ;Sign bit in nonnalized FPACC. Set precision counter. 
           call ROTATR           ;Rotate the FPACC once to the right now. 
RESIGN:    mvi l,100             ;Set the pointer to SIGN indicator storage location 
           mov a,m               ;Fetch the original sign of the FPACC 
           ana a                 ;Set CPU flags 
           rp                    ;If original sign of FPACC was positive, can exit now. 
 
FPCOMP:    mvi l,124             ; However, if original sign was negative, must now restore 
           mvi b,003             ;The FPACC to negative by performing two's comple- 
           jmp COMPLM            ;Ment on FPACC. Return to caring rtn via COMPLM. 
 
                                 ;Floating point ADDITION. Adds contents of FPACC to 
                                 ;FPOP and leaves result in FPACC. Routine first checks 
                                 ;to see if either register contains zero. If so addition 
                                 ;result is already present! 
FPADD:     mvi l,126             ;Set L to point to MSW of FPACC 
           mvi h,(OLDPG1>>08H)&0FFH;** Do same for register H 
           mov a,m               ;Fetch MSW of FPACC to accumulator 
           ana a                 ;Set CPU flags after loading op 
           jnz NONZAC            ;If accumulator non-zero then FPACC has some value 
MOVOP:     mvi l,124             ;But, if accumulator was zero then normalized FPACC 
           mov d,h               ;Must also be zero. Thus answer to addition is simply the 
           mov e,l               ;Value in FPOP. Set up pointers to transfer contents of 
           mvi l,134             ;FPOP to FPACC by pointing to the LSW of both 
           mvi b,004             ;Registers and perform the transfer. Then exit to calling 
           jmp MOVEIT            ;Routine with answer in FPACC via MOVEIT. 
NONZAC:    mvi l,136             ;If FPACC was non-zero then check to see if FPOP has 
           mov a,m               ;Some value by obtaining MSW of FPOP 
           ana a                 ;Set CPU flags after loading op. If MSW zero then 
           rz                    ;Normalized FPOP must be zero. Answer is in FPACC! 
 
                                 ;If neither FPACC or FPOP was zero then must perform 
                                 ;addition operation. Must first check to see if two num- 
                                 ;bers are within significant mnge. If not, largest number 
                                 ;is answer. If numbers within range, then must align ex- 
                                 ;ponents before perforrning the addition of the man- 
                                 ;tissa. 
CKEQEX:    mvi l,127             ;Set pointer to FPACC Exponent storage location. 
           mov a,m               ;Fetch the Exponent value to the accumulator. 
           mvi l,137             ;Change the pointer to the FPOP Exponent 
           cmp m                 ;Compare the values of the exponents. If they are the 
           jz SHACOP             ;Same then can immediately proceed to add operations. 
           mov b,a               ;If not the same, store FPACC Exponent size in regis B 
           mov a,m               ;Fetch the FPOP Exponent size into the adc c 
           sbb b                 ;Subtract the FPACC Exponent from the FPOP Exp. 
           jp SKPNEG             ;If result is positive jump over the next few instructions 
           mov b,a               ;If result was negative, store the result in B 
           xra a                 ;Clear the accumulator 
           sbb b                 ;Subtract register B to negate the original value 
SKPNEG:    cpi 030               ;See if difference is less than 24 decimal. 
           jm LINEUP             ;If so, can align exponents. Go do it. 
           mov a,m               ;If not, find out which number is largest. Fetch FPOP 
           mvi l,127             ;Exponent into ACC. Change pointer to FPACC Exp. 
           sub m                 ;Subtract FPACC from FPOP. If result is negative then 
           rm                    ;was larger. Return with answer in FPACC. 
           mvi l,124             ;If result was positive, larger value in FPOP. Set pointers 
           jmp MOVOP             ;To transfer FPOP into FPACC and then exit to caller. 
LINEUP:    mov a,m               ;Fetch FPOP Exponent into accumulator. 
           mvi l,127             ;Change pointer to FPACC Exponent. 
           sub m                 ;Subtract FPACC Exponent from FPOP Exponent. If 
           jm SHIFT0             ;Result is negative FPACC is larger. Go shift FPOP. 
           mov c,a               ;If result positive FPOP larger, must shift FPACC. Store 
MORACC:    mvi l,127             ;Difference count in C. Reset pointer to FPACC Exp 
           call SHLOOP           ;Call the SHift LOOP to rotate FPACC mantissa RIGHT 
           dcr c                 ;And INCREMENT Exponent. Decr difference counter 
           jnz MORACC            ;Continue rotate operations until diff counter is zero 
           jmp SHACOP            ;Go do final alignment and perform addition process 
SHIFT0:    mov c,a               ;Routine to shift FPOP. Set difference count into reg. C 
MOROP:     mvi l,137             ;Set pointer to FPOP Exponent. 
           call SHLOOP           ;Call the SHift LOOP to rotate FPOP mantissa RIGHT 
           inr c                 ;And INCREMENT Exponent. Then incr difference cntr 
           jnz MOROP             ;Continue rotate opemtions until diff counter is zero 
;;; The below two instructions are changed by PATCH NR.1 
;;;SHACOP:    mvi l,123             ;Set pointer to FPACC LSW minus one to provide extra 
;;;           mvi m,000             ;Byte for addition ops. Clear that location to zero. 
SHACOP:    call PATCH1           ; patch 1 inserts a few lines at 30-000 
           mov a,a 
 
;;;        mvi l,133 
;;;        mvi m,000                ;THIS IS PATCH #1 
           mvi l,127             ;Change pointer to FPACC Exponent 
           call SHLOOP           ;Rotate FPACC mantissa RIGHT & Increment Exponent 
           mvi l,137             ;Change pointer to FPOP Exponent 
           call SHLOOP           ;Rotate FPOP mantissa RIGHT & Increment Exponent 
           mov d,h               ;Rotate ops provide room for overflow. Now set up 
           mvi e,123             ;Pointers to LSW minus one for both FPACC & FPOP 
           mvi b,004             ;(FPOP already set after SHLOOP). Set precision counter 
           call ADDER            ;Call quad precision ADDITION subroutine. 
           mvi b,000             ;Set CPU register B to indicate standard normalization 
           jmp FPNORM            ;Go normalize the result and exit to caller. 
SHLOOP:    mov b,m               ;Shifting loop. First fetch Exponent currently being 
           inr b                 ;Pointed to and Increment the value by one. 
           mov m,b               ;Return the updated Exponent value to memory. 
           dcr l                 ;Decrement the pointer to mantissa portion MSW 
           mvi b,004             ;Set precision counter 
FSHIFT:    mov a,m               ;Fetch MSW of mantissa 
           ana a                 ;Set CPU flags after load ops 
           jp ROTATR             ;If MSB not a one can do normal rotate ops 
BRING1:    ral                   ;If MSB is a one need to set up carrv bit for the negative 
           jmp ROTR              ;Number case. Then make special entry to ROTATR sub 
 
PATCH1:    mvi l,123 
           mvi m,000 
           mvi l,133 
           mvi m,000 
           ret 
 
                                 ;The following subroutine moves the contents of a string 
                                 ;of memory locations from the address pointed to by 
                                 ;CPU registers H & L to the address specified by the con- 
                                 ;tents of registers D & E when the routine is entered. The 
                                 ;process continues until the counter in register B is zero. 
MOVEIT:    mov a,m               ;Fetch a word from memory string A 
           inr l                 ;Advance A string pointer 
           call SWITCH           ;Switch pointer to string B 
           mov m,a               ;Put word from string A into string B 
           inr l                 ;Advance B string pointer 
           call SWITCH           ;Switch pointer back to string A 
           dcr b                 ;Decrement loop counter 
           rz                    ;Return to calling routine when counter reaches zero 
           jmp MOVEIT            ;Else continue transfer operations 
 
                                 ;The following subroutine SUBTRACTS the 
                                 ;contents of the FLOATING POINT ACCUMULATOR from the 
                                 ;contents of the FLOATING POINT OPERAND and 
                                 ;leaves the result in the FPACC. The routine merely 
                                 ;negates the value in the FPACC and then goes to the 
                                 ;FPADD subroutine just presented. 
FPSUB:     mvi l,124             ;Set L to address of LSW of FPACC 
           mvi h,(OLDPG1>>08H)&0FFH;** Set H to page of FPACC 
           mvi b,003             ;Set precision counter 
           call COMPLM           ;Two's complement the value in the FPACC 
           jmp FPADD             ;Now go add the negated value to perform subtraction! 
 
                                 ;The first part of the FLOATING POINT MULTIPLI- 
                                 ;CATION subroutine calls a subroutine to check the 
                                 ;original signs of the numbers that are to be multi- 
                                 ;plied and perform working register clearing functions. 
                                 ;Next the exponents of the numbers to be multiplied 
                                 ;are added together. 
FPMULT:    call CKSIGN           ;Call routine to set up registers & ck signs of numbers 
ADDEXP:    mvi l,137             ;Set pointer to FPOP Exponent 
           mov a,m               ;Fetch FPOP Exponent into the accumulator 
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
           mvi b,003             ;Set precision counter 
           call ROTATR           ;Rotate FPACC (multiplier) RIGHT into carry bit 
           cc ADOPPP             ;If carry is a one, add multiplicand to partial-product 
           mvi l,146             ;Set pointer to partial-product most significant byte 
           mvi b,006             ;Set precision counter (p-p register is double length) 
           call ROTATR           ;Shift partial-product RIGHT 
           mvi l,102             ;Set pointer to bit counter storage location 
           mov c,m               ;Fetch current value of bit counter 
           dcr c                 ;Decrement the value of the bit counter 
           mov m,c               ;Restore the updated bit counter to its storage location 
           jnz MULTIP            ;If have not multiplied for 23 (deciinal) bits, keep going 
           mvi l,146             ;If have done 23 (decimal) bits, set pntr to p-p MSW 
           mvi b,006             ;Set precision counter (for double length) 
           call ROTATR           ;Shift partial-product once more to the RIGHT 
           mvi l,143             ;Set pointer to access 24'th bit in partial-product 
           mov a,m               ;Fetch the byte containing the 24'th bit 
           ral                   ;Position the 24'th bit to be MSB in the accumulator 
           ana a                 ;Set the CPU flags after to rotate operation and test to 
           cm MROUND             ;See if 24'th bit of p-p is a ONE. If so, must round-off 
           mvi l,123             ;Now set up pointers 
           mov e,l               ;To perform transfer 
           mov d,h               ;Of the multiplication results 
           mvi l,143             ;From the partial-product location 
           mvi b,004             ;To the FPACC 
 
EXMLDV:    call MOVEIT           ;Perform the transfer from p-p to FPACC 
           mvi b,000             ;Set up CPU register B to indicate regular normalization 
           call FPNORM           ;Normalize the result of multiplication 
           mvi l,101             ;Now set the pointer to the original SIGNS indicator 
           mov a,m               ;Fetch the indicator 
           ana a                 ;Exercise the CPU flags 
           rnz                   ;If indicator is non-zero, answer is positive, can exit her 
           jmp FPCOMP            ;If not, answer must be negated, exit via 2's complement. 
 
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
CKSIGN:    mvi l,140             ;Set pointer to start of partial-product working area 
           mvi h,(OLDPG1>>08H)&0FFH;** Set H to proper page 
           mvi b,010             ;Set up a loop counter in CPU register B 
           xra a                 ;Clear the accumulator 
 
CLRNEX:    mov m,a               ;Now clear out locations for the partial-product 
           inr l                 ;Working registers 
           dcr b                 ;Until the loop counter 
           jnz CLRNEX            ;Is zero 
CLROPL:    mvi b,004             ;Set a loop counter 
           mvi l,130             ;Set up pointer 
CLRNX1:    mov m,a               ;Clear out some extra registers so that the 
           inr l                 ;FPOP may be extended in length 
           dcr b                 ;Perform clearing ops until loop counter 
           jnz CLRNX1            ;Is zero 
           mvi l,101             ;Set pointer to M/D SIGNS indicator storage location 
           mvi m,001             ;Set initial value of SIGNS indicator to plus one 
           mvi l,126             ;Change pointer to MSW of FPACC 
           mov a,m               ;Fetch MSW of mantissa into accumulator 
           ana a                 ;Test flags 
           jm NEGFPA             ;If MSB in MSW of FPACC is a one, number is negative 
OPSGNT:    mvi l,136             ;Set pointer to MSW of FPOP 
           mov a,m               ;Fetch MSW of mantissa into accumulator 
           ana a                 ;Test flags 
           rp                    ;Return to caller if number in FPOP is positive 
           mvi l,101             ;Else change pointer to M/D SIGNS indicator 
           mov c,m               ;Fetch the value in the SIGNS indicator 
           dcr c                 ;Decrement the value by one 
           mov m,c               ;Restore the new value back to storage location 
           mvi l,134             ;Set pointer to LSW of FPOP 
           mvi b,003             ;Set precision counter 
           jmp COMPLM            ;Two's complement value of FPOP & return to caller 
NEGFPA:    mvi l,101             ;Set pointer to M/D SIGNS indicator 
           mov c,m               ;Fetch the value in the SIGNS indicator 
           dcr c                 ;Decrement the value by one 
           mov m,c               ;Restore the new value back to storage location 
           mvi l,124             ;Set pointer to LSW of FPACC 
           mvi b,003             ;Set precision counter 
           call COMPLM           ;Two's complement value of FPACC 
           jmp OPSGNT            ;Proceed to check sign of FPOP 
 
                                 ;The following subroutine adds the double length (six regis 
                                 ;multiplicand in FPOP to the partial-product register when 
                                 ;called on by the multiplication algorithm. 
ADOPPP:    mvi e,141             ;Pointer to LSW of partial-product 
           mov d,h               ;On same page as FPOP 
           mvi l,131             ;LSIV of FPOP which contains extended multiplicand 
           mvi b,006             ;Set precision counter (double length working registers) 
           jmp ADDER             ;Add multiplicand to partial-product & return to caller 
 
MROUND:    mvi b,003             ;Set up precision counter 
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
 
ADDER:     ana a                 ;Initialize the carry bit to zero upon entry 
ADDMOR:    mov a,m               ;Fetch byte from register group A 
           call SWITCH           ;Switch memory pointer to register group B 
           adc m                 ;Add byte from A to byte from B with carry 
           mov m,a               ;Leave result in register group B 
           dcr b                 ;Decrement number of bytes (precision) counter 
           rz                    ;Return to caller when all bytes in group processed 
           inr l                 ;Else advance pointer for register group B 
           call SWITCH           ;Switch memory pointer back to register group A 
           inr l                 ;Advance the pointer for register group A 
           jmp ADDMOR            ;Continue the multi-byte addition operation 
 
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
           dcr b                 ;Decrement number of bytes (precision) counter 
           rz                    ;Return to caller when all bytes in number processed 
           inr l                 ;Else advance the pointer 
           mov a,m               ;Fetch the next byte of the number to adc c 
           xri 377               ;Exclusive OR to complement the byte 
           mov e,a               ;Save complemented value in register E temporarily 
           mov a,d               ;Restore previous carry status to adc c 
           ral                   ;And rotate it out to the carry bit 
           mvi a,000             ;Clear adc c without disturbing carry status 
           adc e                 ;Add in any carry to complemented value 
           jmp MORCOM            ;Continue the two's complement procedure as req'd 
 
                                 ;N'th precision rotate left subroutine. Rotates a multi- 
                                 ;byte number left starting at the address initially 
                                 ;specified by the contents of CPU registers H & L upon 
                                 ;subroutine entry (LSW). First entry point will clear 
                                 ;the carry bit before beginning rotate operations. Second 
                                 ;entry point does not clear the carry bit. 
ROTATL:    ana a                 ;Clear the carry bit at this entry point 
ROTL:      mov a,m               ;Fetch a byte from memory 
           ral                   ;Rotate it left (bring carry into LSB, push MSB to carry) 
           mov m,a               ;Restore rotated word to memory 
           dcr b                 ;Decrement precision counter 
           rz                    ;Exit to caller when finished 
           inr l                 ;Else advance pointer to next byte 
           jmp ROTL              ;Continue rotate left operations 
 
                                 ;N'th precision rotate 
                                 ;right subroutine. Opposite of 
                                 ;above subroutine. 
ROTATR:    ana a                 ;Clear the carry bit at this entry point 
ROTR:      mov a,m               ;Fetch a byte from memory 
           rar                   ;Rotate it right (carry into MSB, LSB to carry) 
           mov m,a               ;Restore rotated word to memory 
           dcr b                 ;Decrement precision counter 
           rz                    ;Exit to caller when finished 
           dcr l                 ;Else decrement pointer to next byte 
           jmp ROTR              ;Continue rotate right operations 
 
                                 ;N'th precision subtraction subroutine. 
                                 ;Number starting at location pointed to by D & E (least 
                                 ;significant byte) is subtracted from number starting at 
                                 ;address specified by contents of H & L. 
SUBBER:    ana a                 ;Initialize the carry bit to zero upon entry 
SUBTRA:    mov a,m               ;Fetch byte from register group A 
           call SWITCH           ;Switch memory pointer to register group B 
           sbb m                 ;Subtract byte from group B ftom that in group A 
           mov m,a               ;Leave result in register group B 
           dcr b                 ;Decrement number of bytes (precision) counter 
           rz                    ;Return to caller when all bytes in group processed 
           inr l                 ;Else advance pointer for register group B 
           call SWITCH           ;Switch memory pointer back to register group A 
           inr l                 ;Advance the pointer for register group A 
           jmp SUBTRA            ;Continue the multi-byte subtraction operation 
 
                                 ;The next subroutine will transfer the four byte 
                                 ;register string (generally a number in floating point 
                                 ;format) from the starting address pointed to by CPU 
                                 ;registers H & L when the subroutine is entered to 
                                 ;the FPACC (floating point accumulator registers). 
FLOAD:     mvi d,(OLDPG1>>08H)&0FFH;** Set page address of FPACC 
           mvi e,124             ;Set address of least signficant byte of FPACC 
           mvi b,004             ;Set precision counter to four bytes (mantissa bytes 
           jmp MOVEIT            ;Plus Exponent) and exit via the transfer routine 
 
                                 ;The next several subroutines are used to perform 
                                 ;floating pojnt register loading and transfer operations. 
FSTORE:    mov e,l               ;Transfer contents of register L to E 
           mov d,h               ;Transfer contents of register H to D 
           mvi l,124             ;Set L to least significant byte of FPACC mantissa 
           mvi h,(OLDPG1>>08H)&0FFH;** Set page to FPACC storage area 
           jmp SETIT             ;Go transfer FPACC contents to area pointed to by D&E 
OPLOAD:    mvi d,(OLDPG1>>08H)&0FFH;** Set page to FPOP storage area 
           mvi e,134             ;Set pointer to least significant byte of FPOP 
SETIT:     mvi b,004             ;Set precision counter. Transfer from H & L area to 
           jmp MOVEIT            ;Locations pointed to by D & E 
 
 
                                 ;Subroutine to exchange the contents of H & L with 
                                 ;D & E. 
SWITCH:    mov c,h               ;Transfer register H to C temporarily 
           mov h,d               ;Place value of D into H 
           mov d,c               ;Now put former H from C into D 
           mov c,l               ;Transfer register L to C temporarily 
           mov l,e               ;Place value of E into L 
           mov e,c               ;Now put former L from C into E 
           ret                   ;Exit to calling routine 
 
CRLF:      mvi a,215             ;Load ASCII code for carriage-return into adc c 
           call ECHO             ;Call user provided display driver subroutine 
           mvi a,212             ;Load ASCII code for line-feed into adc c 
 
            cpu 8008new          ; use "new" 8008 mnemonics 
            radix 10             ; use base 10 for numbers 
 
OUTPORT     equ 08H              ; serial output port address 
;------------------------------------------------------------------------ 
; character output subroutine 
; sends the character in A out from the serial port. 
; transmits 1 start bit, 8 data bits and 1 stop at 2400 bps. 
; uses A and B. 
; returns with the original character in A 
;------------------------------------------------------------------------ 
ECHO:       ani 7FH              ; mask off the most significant bit of the character 
            mov b,a              ; save the character from A to B 
            xra a                ; clear A for the start bit 
            out OUTPORT          ; send the start bit 
            mov a,b              ; restore the character from B to A 
            mov a,b              ; timing adjustment 
            mvi b,0FDH           ; timing adjustment 
            mvi b,0FDH           ; timing adjustment 
            call delay           ; timing adjustment 
 
                                 ; send bits 0 through 7 
            call putbit          ; transmit bit 0 
            call putbit          ; transmit bit 1 
            call putbit          ; transmit bit 2 
            call putbit          ; transmit bit 3 
            call putbit          ; transmit bit 4 
            call putbit          ; transmit bit 5 
            call putbit          ; transmit bit 6 
            call putbit          ; transmit bit 7 
 
                                 ; send the stop bit 
            mov b,a              ; save the character from A to B 
            mvi a,1              ; '1' for the stop bit 
            out OUTPORT          ; send the stop bit 
            mov a,b              ; restore the original character from B to A 
            ori 80H              ; restore the most significant bit of the character 
            mvi b,0FCH           ; timing adjustment 
            call delay           ; timing adjustment 
            ret                  ; return to caller 
 
putbit:     out OUTPORT          ; output the least significant bit of the character in A 
            mvi b,0FDH           ; timing adjustment 
            mvi b,0FDH           ; timing adjustment 
            call delay           ; timing adjustment 
            rrc                  ; shift the character in A right 
            ret 
 
;------------------------------------------------------------------------ 
; delay in microseconds = (((255-value in B)*16)+19) * 4 microseconds 
;------------------------------------------------------------------------ 
delay:      inr b 
            jnz delay 
delay1:     ret 
 
            cpu 8008new          ; use "old" 8008 mnemonics 
            radix 8              ; use base 8 for numbers 
 
            end start 
 
