            PAGE 0             ; suppress page headings in AS listing file
            
;------------------------------------------------------------------------
;
; Scelbi Basic Interpreter (SCELBAL) modified for ROM.
;
; SCELBAL interpreter downloaded from http://www.willegal.net/scelbi/scelbal.html modified
; to assemble with the AS Macro Assembler (http://john.ccac.rwth-aachen.de:8000/as/) by Hans-Åke.
;
; modified to run in a 2764 EPROM for my 8008 home-brew single board computer by Jim Loos.
;------------------------------------------------------------------------            
            
            include "bitfuncs.inc" 

            cpu 8008new             ; use "new" 8008 mnemonics
            radix 10                ; use base 10 for numbers

; when the reset pushbutton on my 8008 SBC is pressed, the reset flip-flop is set which
; generates an interrupt and clears the address latches thus, the first instruction is thus
; always fetched from address 0. the instruction at address 0 must be a single byte transfer
; instruction in order to set the program counter. i.e., it must be one of the RST opcodes.
            org 00h
            rst 1                   ; jumps to 0008h

            org 08h			        ; rst 1 jumps here
start:      mvi a,1
            out 8                   ; set serial output high (mark)
            xra a
            out 9                   ; turn off the red LED
            
; copy OLDPG1 constants from EPROM at 1D00H to RAM at 2000H
            mvi l,00h               ; initialize L to start of page
mv_oldpg1:  mvi h,hi(page1)         ; source: OLDPG1 constants in EPROM at page 1DH
            mov a,m                 ; retrieve the byte from EPROM
            mvi h,hi(OLDPG1)        ; destination: RAM at page 20H
            mov m,a                 ; store the byte in RAM
            inr l                   ; next address
            jnz mv_oldpg1           ; go back if page not complete

; copy OLDPG26 constants from EPROM at 1E00H to RAM at 2100H            
            mvi l,00h               ; initialize L to start of page
mv_oldpg26: mvi h,hi(page26)        ; source: OLDPG26 constants in EPROM at page 1EH
            mov a,m                 ; retrieve the byte from EPROM
            mvi h,hi(OLDPG26)       ; destination: RAM at page 21H
            mov m,a                 ; store the byte in RAM
            inr l                   ; next address
            jnz mv_oldpg26          ; go back if page not complete
            
; copy OLDPG27 constants from EPROM at 1F00H to RAM at 2200H            
            mvi l,00h               ; initialize L to start of page
mv_oldpg27: mvi h,hi(page27)        ; source: OLDPG27 constants in EPROM at page 1FH
            mov a,m                 ; retrieve the byte from EPROM
            mvi h,hi(OLDPG27)       ; destination: RAM at page 22H
            mov m,a                 ; store the byte in RAM
            inr l                   ; next address
            jnz mv_oldpg27          ; go back if page not complete
            
            jmp exec                ; run the SCELBAL interpreter
            
;-----------------------------------------------------------------------------------------       
; I/O routines for SCELBAL.
; According to the SECBAL Manual: "Only CPU register B and the accumulator may be used by the I/O routines.
; All the other CPU registers must contain their original values when I/O operations have been completed."       
; "... the I/O routines themselves may only utilize a maximum of two levels of nesting!"
;-----------------------------------------------------------------------------------------
; 2400 bps character input subroutine for SCELBAL
; wait for a character from the serial port. echo the character. return the character in A.
; uses A and B.
;-----------------------------------------------------------------------------------------
CINP:       in 0                    ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc CINP                 ; jump if start bit detected

            ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit
            mvi b,0                 ; initialize B
            mvi b,0                 ; tweak timing
            xra a                   ; clear the accumulator
            out 8                   ; send the start bit
            
            call getbit             ; bit 0
            call getbit             ; bit 1
            call getbit             ; bit 2
            call getbit             ; bit 3
            call getbit             ; bit 4
            call getbit             ; bit 5
            call getbit             ; bit 6
            call getbit             ; bit 7
            
            ; wait 104 cycles, then send the stop bit
            mov a,b                 ; 5 cycles save the character in A
            mvi b,252
            call delay
            mov b,a                 ; retrieve the character from A
            mvi a,1
            out 8                   ; send the stop bit
            ; wait 104 cycles.
            mov a,b                 ; restore the character to A from B
            ori 80h                 ; set the most significant bit            
            mvi b,252
            mvi b,252
            call delay
            ret                     ; return to caller with the character in A

            ; 92 cycles
getbit:     mov a,b                 ; save B in A
            mvi b,255
            mvi b,255
            call delay
            mov b,a                 ; restore B from A
            in 0                    ; get input from the serial port
            out 8                   ; echo the received bit
            rar                     ; rotate the received bit right into carry
            mov a,b                 ; restore the previously received bits
            rar                     ; rotate the newly received bit in carry right into the MSB of A
            mov b,a                 ; save the received bits in B
            ret
            
;------------------------------------------------------------------------        
; 2400 bps character output subroutine for SCELBAL
; uses A and B.
;------------------------------------------------------------------------
CPRINT:     ani 7fh                 ; clear the most signficant bit
            mov b,a                 ; save the character in B
            xra a                   ; clear A for the start bit

            out 8                   ; send the start bit
            mov a,b                 ; restore the character to A 
            mov a,b                 ; timing adjustment
            mvi b,252
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
            mvi a,1                 ; stop bit
            out 8                   ; send the stop bit 
            mvi b,252
            call delay
            ret                     ; return to caller

outbit:     out 8                   ; output the least significant bit
            mvi b,253
            call delay
            ana a                   ; timing adjustment
            rrc                     ; shift A right
            ret
            
delay:      inr b
            jnz delay
            ret                         

            cpu 8008                ; use "old" mneumonics
            RADIX 8                 ; use octal for numbers

;--------------------------------------------------------------------------------            
;;; This is the Scelbi Basic Program from 1974 known as
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
;;; (Tiny BASIC only used 16-bit integers; the MCM\70
;;; was a closed system; calculators implemented with 8008
;;; were floating-point, but not high-level.)
;;;
;;; This version is modified to assemble with the
;;; as8 assembler (using the -octal option) 
;;; for the Intel 8008 by Thomas E. Jones.
;;; This current form is made up non-relocatable so that
;;; locations of all code and data is identical to the
;;; original SCELBAL documents and patches.  It should be
;;; reasonable after debugging code to convert this to a
;;; relocatable and ROMable code with variables in RAM.
;;; This code originates from a version made by 
;;;
;;;    Steve Loboyko in 2001.
;;;
;;; This version has all 3 patches for SCELBAL (the two
;;; pasted in the original manual, and a third which was
;;; written in SCELBAL UPDATE publication, as well as
;;; a couple changes to constants which didn't actually
;;; require a patch, just changes to bytes of data or
;;; arguments to an instruction--one of these (Tucker) was 
;;; incorrect and restored to original by MGA March 2012).
;;; 
;;; This comment must be incorporated with any version of SCELBAL
;;; downloaded, distributed, posted or disemenated.            
;;;
;;; Here are labels originally attempting to make the code
;;; relocatable.  These 4 pages contain variable data
;;; which needs to be relocated from ROM to RAM.
;;; I can't vouch for ALL references to these pages in
;;; the code being switched to these labels, but they
;;; seem to be.
	
OLDPG1	    EQU	2000H             ; originally at 0100H (page 01 in octal), now relocated to 2000H - jsl  
OLDPG26	    EQU	2100H             ; originally at 1600H (page 26 in octal), now relocated to 2100H - jsl 
OLDPG27	    EQU	2200H             ; originally at 1700H (page 27 in octal), now relocated to 2200H - jsl 
OLDPG57	    EQU	2300H             ; originally at 2F00H (pahe 57 in octal), now relocated to 2300H - jsl

BGNPGRAM    EQU 24H               ; originally user program buffer began at 1B00H, now begins at 2400H - jsl
ENDPGRAM    EQU 40H               ; originally user program buffer ended at 2CFFH, now ends at 3FFFH   - jsl  

           ORG 0100H
SYNTAX:    CAL CLESYM             ;Clear the SYMBOL BUFFER area
           LLI 340                ;Set L to start of LINE NUMBER BUFFER
           LHI OLDPG26/400        ;** Set H to page of LINE NUMBER BUFFER
           LMI 000                ;Initialize line number buff by placing zero as (cc)
           LLI 201                ;Change pointer to syntax counter/pointer storage loc.
           LMI 001                ;Set pointer to first character (after cc) in line buffer
SYNTX1:    LLI 201                ;Set pointer to syntax cntr/pntr storage location
           CAL GETCHR             ;Fetch the character pointed to by contents of syntax
           JTZ SYNTX2             ;Cntr/pntr from the line input buffer. If character was
           CPI 260                ;A space, ignore. Else, test to see if character was ASCII
           JTS SYNTX3             ;Code for a decimal digit. If not a decimal digit, consider
           CPI 272                ;Line number to have been processed by jumping
           JFS SYNTX3             ;Over the remainder of this SYNTX1 section.
           LLI 340                ;If have decimal digit, set pointer to start of LINE
           CAL CONCT1             ;NUMBER BUFFER and append incoming digit there.
SYNTX2:    LLI 201                ;Reset L to syntax cntr/pntr storage location. Call sub-
           CAL LOOP               ;Routine to advance pntr and test for end of inr)ut buffer
           JFZ SYNTX1             ;If not end of input buffer, go back for next digit
           LLI 203                ;If end of buffer, only had a line number in the line.
           LMI 000                ;Set pntr to TOKEN storage location. Set TOKEN = 000.
           RET                    ;Return to caller.
SYNTX3:    LLI 201                ;Reset pointer to syntax cntr/pntr and fetch
           LBM                    ;Position of next character after the line number
           LLI 202                ;Change pntr to SCAN pntr storage location
           LMB                    ;Store address when SCAN takes up after line number
SYNTX4:    LLI 202                ;Set pntr to SCAN pntr stomge location
           CAL GETCHR             ;Fetch the character pointed to by contents of the SCAN
           JTZ SYNTX6             ;Pointer storage location. If character was ASCII code
           CPI 275                ;For space, ignore. Else, compare character with "=" sign
           JTZ SYNTX7             ;If is an equal sign, go set TOKEN for IMPLIED LET.
           CPI 250                ;Else, compare character with left parenthesis " ( "
           JTZ SYNTX8             ;If left parenthesis, go set TOKEN for implied array LET
           CAL CONCTS             ;Otherwise, concatenate the character onto the string
; MGA 4/2012 begin "fast SYNTX5" patch: 
; the following patch doubles the overall speed of execution.  
; It is similar to the approach taken on 8080 SCELBAL II in 1978 
; it adhears to the rules for patches in issue 1 of SCELBAL update 
;SYNTX6:   these four lines moved up w/o label
           LLI 202                ;Set L to SCAN pointer storage location
;           LHI OLDPG26/400        ;** Set H to page of SCAN pointer stomge location
;MGA 4/2012 except LHI needed at original place, not here 
           CAL LOOP               ;Call routine to advance pntr & test for end of In buffer
           JFZ SYNTX4             ;Go back and add another character to SYMBOL BUFF
SYNTX6:   ; MGA 4/2012 label here 

           LLI 203                ;Being constructed in the SYMBOL BUFFER. Now set
           LMI 001                ;Up TOKEN storage location to an initial value of 001.
           LHI OLDPG27/400        ;** Set H to point to start of KEYWORD TABLE.
           LLI 000                ;Set L to point to start of KEYWORD TABLE.
SYNTX5:    LDI OLDPG26/400        ;** Set D to page of SYMBOL BUFFER
           LEI 120                ;Set E to start of SYMBOL BUFFER
           CAL STRCP              ;Compare char string presently in SYMBOL BUFFER
           RTZ                    ;With entry in KEYWORD TABLE. Exit if match.
           CAL SWITCH             ;TOKEN will be set to keyword found. Else, switch
SYNTXL:    INL                    ;Pointers to get table address back and advance pntr to
           LAM                    ;KEYWORD TABLE. Now look for start of next entry
           NDI 300                ;In KEYWORD TABLE by looking for (cc) byte which
           JFZ SYNTXL             ;Will NOT have a one in the two most sig. bits. Advance
           CAL SWITCH             ;Pntr til next entry found. Then switch pointers apin so
           LLI 203                ;Table pointer is in D&E. Put addr of TOKEN in L.
           LHI OLDPG26/400        ;** And page of TOKEN in H. Fetch the value currently
           LBM                    ;In TOKEN and advance it to account for going on to
           INB                    ;The next entry in the KEYWORD TABLE.
           LMB                    ;Restore the updated TOKEN value back to storage.
           CAL SWITCH             ;Restore the keyword table pointer back to H&L.
           LAB                    ;Put TOKEN count in ACC.
           CPI 015                ;See if have tested all entries in the keyword table.
           JFZ SYNTX5             ;If not, continue checking the keyword table.
;MGA 4/2012 3 of 4 lines removed below (keep LHI)
           LHI OLDPG26/400        ;** Set H to page of SCAN pointer stomge location
; MGA 4/2012 end of "fast SYNTX5" patch: 
           LLI 203                ;And search table for KEYWORD again. Unless reach
           LMI 377                ;End of line input buffer. In which case set TOKEN=377
           RET                    ;As an error indicator and exit to calling routine.
SYNTX7:    LLI 203                ;Set pointer to TOKEN storage register. Set TOKEN
           LMI 015                ;Equal to 015 when "=" sign found for IMPLIED LET.
           RET                    ;Exit to calling routine.
SYNTX8:    LLI 203                ;Set pointer to TOKEN storage register. Set TOKEN
           LMI 016                ;Equal to 016 when "(" found for IMPLIED array LET.
           RET                    ;Exit to calling routine.


;The following are subroutines used by SYNTAX and other routines in SCELBAL.
BIGERR:    LAI 302                ;Load ASCII code for letters B and G to indicate BIG
           LCI 307                ;ERROR (for when buffer, stack,etc., overflows).
ERROR:     CAL ECHO               ;Call user provided display routine to print ASCII code
           LAC                    ;In accumulator. Transfer ASCII code from C to ACC
           CAL ECHO               ;And repeat to display error codes.
           JMP FINERR             ;Go cpmplete error message (AT LINE) as required.
GETCHR:    LAM                    ;Get pointer from memory location pointed to by H&L
           CPI 120                ;See if within range of line input buffer.
           JFS BIGERR             ;If not then have an overflow condition = error.
           LLA                    ;Else can use it as addr of character to fetch from the
           LHI OLDPG26/400        ;** LINE INPUT BUFFER by setting up H too.
           LAM                    ;Fetch the character from the line input buffer.
           CPI 240                ;See if it is ASCII code for space.
           RET                    ;Return to caller with flags set according to comparison.
CLESYM:    LLI 120                ;Set L to start of SYMBOL BUFFER.
           LHI OLDPG26/400        ;** Set H to page of SYMBOL BUFFER.
           LMI 000                ;Place a zero byte at start of SYMBOL BUFFER.
           RET                    ;To effectively clear the buffer. Then exit to caller.

;Subroutine to concatenate (append) a character to the
;SYMBOL BUFFER. Character must be alphanumeric.
CONCTA:    CPI 301                ;See if character code less than that for letter A.
           JTS CONCTN             ;If so, go see if it is numeric.
           CPI 333                ;See if character code greater than that for letter Z.
           JTS CONCTS             ;If not, have valid alphabetical character.
CONCTN:    CPI 260                ;Else, see if character in valid numeric range.
           JTS CONCTE             ;If not, have an error condition.
           CPI 272                ;Continue to check for valid number.
           JFS CONCTE             ;If not, have an error condition.
CONCTS:    LLI 120                ;If character alphanumeric, can concatenate. Set pointer
           LHI OLDPG26/400        ;** To starting address of SYMBOL BUFFER.
CONCT1:    LCM                    ;Fetch old character count in SYMBOL BUFFER.
           INC                    ;Increment the value to account for adding new
           LMC                    ;Character to the buffer. Restore updated (cc).
           LBA                    ;Save character to be appended in register B.
           CAL INDEXC             ;Add (cc) to address in H & L to get new end of buffer
           LMB                    ;Address and append the new character to buffer
           LAI 000                ;Clear the accumulator
           RET                    ;Exit to caller
CONCTE:    JMP SYNERR             ;If character to be appended not alphanumeric, ERROR!

;Subroutine to compare character strings pointed to by register pairs D & E and H & L.
STRCP:     LAM                    ;Fetch (cc) of first string.
           CAL SWITCH             ;Switch pointers and fetch length of second string (cc)
           LBM                    ;Into register B. Compare the lengths of the two strings.
           CPB                    ;If they are not the same
           RFZ                    ;Return to caller with flags set to non-zero condition
           CAL SWITCH             ;Else, exchange the pointers back to first string.
STRCPL:    CAL ADV                ;Advance the pointer to string number 1 and fetch a
           LAM                    ;Character from that string into the accumulator.
           CAL SWITCH             ;Now switch the pointers to string number 2.
           CAL ADV                ;Advance the pointer in line number 2.
STRCPE:    CPM                    ;Compare char in stxing 1 (ACC) to string 2 (memory)
           RFZ                    ;If not equal, return to cauer with flags set to non-zero
           CAL SWITCH             ;Else, exchange pointers to restore pntr to string 1
           DCB                    ;Decrement the string length counter in register B
           JFZ STRCPL             ;If not finiahed, continue testing entire string
           RET                    ;If complete match, return with flag in zero condition
STRCPC:    LAM                    ;Fetch character pointed to by pointer to string 1
           CAL SWITCH             ;Exchange pointer to examine string 2
           JMP STRCPE             ;Continue the string comparison loop

;Subroutine to advance the two byte value in CPU registers H and L.
ADV:       INL                    ;Advance value in register L.
           RFZ                    ;If new value not zero, return to caller.
           INH                    ;Else must increment value in H
           RET                    ;Before retuming to caller

;Subroutine to advance a buffer pointer and test to see if the end of the buffer has been reached.
LOOP:      LBM                    ;Fetch memory location pointed to by H & L into B.
           INB                    ;Increment the value.
           LMB                    ;Restore it back to memory.
           LLI 000                ;Change pointer to start of INPUT LINE BUFFER
           LAM                    ;Fetch buffer length (cc) value into the accumulator
           DCB                    ;Make value in B original value
           CPB                    ;See if buffer length same as that in B
           RET                    ;Return with flags yielding results of the comparison

;The following subroutine is used to input characters from the system's
;input device (such as a keyboard) into the LINE INPUT BUFFER. Routine has limited
;editing capability included. (Rubout = delete previous character(s) entered.)
STRIN:     LCI 000                ;Initialize register C to zero.
STRIN1:    CAL CINPUT             ;Call user provided device input subroutine to fetch one
           CPI 377                ;Character from the input device. Is it ASCII code for
           JFZ NOTDEL             ;Rubout? Skip to next section if not rubout.
           LAI 334                ;Else, load ASCII code for backslash into ACC.
           CAL ECHO               ;Call user display driver to present backslash as a delete
           DCC                    ;Indicator. Now decrement the input character counter.
           JTS STRIN              ;If at beginning of line do NOT decrement H and L.
           CAL DEC                ;Else, decrement H & L line pointer to erase previous
           JMP STRIN1             ;Entry, then go back for a new input.
NOTDEL:    CPI 203                ;See if character inputted was'CONTROL C'
           JTZ CTRLC              ;If so, stop inputting and go back to the EXECutive
           CPI 215                ;If not, see if character was carriage-return
           JTZ STRINF             ;If so, have end of line of input
           CPI 212                ;If not, see if character was line-feed
           JTZ STRIN1             ;If so, ignore the input, get another character
           CAL ADV                ;If none of the above, advance contents of H & L
           INC                    ;Increment the character counter
           LMA                    ;Store the new character in the line input buffer
           LAC                    ;Put new character count in the accumulator
           CPI 120                ;Make sure maximum buffer size not exceeded
           JFS BIGERR             ;If buffer size exceeded, go display BG error message
           JMP STRIN1             ;Else can go back to look for next input
STRINF:    LBC                    ;Transfer character count from C to B
           CAL SUBHL              ;Subtract B from H & L to get starting address of
           LMC                    ;The string and place the character count (cc) there
           CAL CRLF               ;Provide a line ending CR & LF combination on the
           RET                    ;Display device. Then exit to caller.

;Subroutine to subtract contents of CPU register B from
;the two byte value in CPU registers H & L.
SUBHL:     LAL                    ;Load contents of register L into the accumulator
           SUB                    ;Subtract the contents of register B
           LLA                    ;Restore the new value back to L
           RFC                    ;If no carry, then no underflow. Exit to caller.
           DCH                    ;Else must also decrement contents of H.
           RET                    ;Before retuming to caller.

;Subroutine to display a character string on the system's display device.
TEXTC:     LCM                    ;Fetch (cc) from the first location in the buffer (H & L
           LAM                    ;Pointing there upon entry) into register B and ACC.
           NDA                    ;Test the character count value.
           RTZ                    ;No display if (cc) is zero.
TEXTCL:    CAL ADV                ;Advance pointer to next location in buffer
           LAM                    ;Fetch a character from the buffer into ACC
           CAL ECHO               ;Call the user's display driver subroutine
           DCC                    ;Decrement the (cc)
           JFZ TEXTCL             ;If character counter not zero, continue display
           RET                    ;Exit to caller when (cc) is zero.

;Subroutine to provide carriage-return and line-feed combination to system's display device. 
;Routine also initializes a column counter to zero. Column counter is used by selected output
;routines to count the number of characters that have been displayed on a line.
CRLF:      LAI 215                ;Load ASCII code for carriage-return into ACC
           CAL ECHO               ;Call user provided display driver subroutine
           LAI 212                ;Load ASCII code for line-feed into ACC
           CAL ECHO               ;Call user provided display driver subroutine
           LLI 043                ;Set L to point to COLUMN COUNTER storage location
           LHI OLDPG1/400         ;** Set H to page of COLUMN COUNTER
           LMI 001                ;Initialize COLUMN COUNTER to a value of one
           LHD                    ;Restore H from D (saved by ECHO subroutine)
           LLE                    ;Restore L from E (saved by ECHO subroutine)
           RET                    ;Then exit to calling routine

;Subroutine to decrement double-byte value in CPU registers H and L.
DEC:       DCL                    ;Decrement contents of L
           INL                    ;Now increment to exercise CPU flags
           JFZ DECNO              ;If L not presently zero, skip decrementing H
           DCH                    ;Else decrement H
DECNO:     DCL                    ;Do the actual decrement of L
           RET                    ;Return to caller

;Subroutine to index the value in CPU registers H and L by the contents of CPU register B.
INDEXB:    LAL                    ;Load L into the accumulator
           ADB                    ;Add B to that value
           LLA                    ;Restore the new value to L
           RFC                    ;If no carry,  return to caller
           INH                    ;Else, increment value in H
           RET                    ;Before returning to caller

;The following subroutine is used to display the ASCII encoded character in the ACC on the
;system's display device. This routine calls a routine labeled CINPUT which must be provided
;by the user to actually drive the system's output device. The subroutine below also increments
;an output column counter each time it is used.
ECHO:      LDH                    ;Save entry value of H in register D
           LEL                    ;And save entry value of L in register E
           LLI 043                ;Set L to point to COLUMN COUNTER storage location
           LHI OLDPG1/400         ;** Set H to page of COLUMN COUNTER
           LBM                    ;Fetch the value in the COLUMN COUNTER
           INB                    ;And increment it for each character displayed
           LMB                    ;Restore the updated count in memory
           CAL CPRINT             ;tt Call the user's device driver subroutine
           LHD                    ;Restore entry value of H from D
           LLE                    ;Restore entry value of L from E
           RET                    ;Return to calling routine
           
CINPUT:	   JMP CINP               ;Reference to user defined input subroutine

EVAL:      LLI 227                ;Load L with address of ARITHMETIC STACK pointer
           LHI OLDPG1/400         ;** Set H to page of ARITHMETIC STACK pointer
           LMI 224                ;Initialize ARITH STACK pointer value to addr minus 4
           INL                    ;Advance memory pointer to FUN/ARRAY STACK pntr
           LHI OLDPG26/400        ;** Set H to page of FUN/ARRAY STACK pointer
           LMI 000                ;Initialize FUNIARRAY STACK pointer to start of stack
           CAL CLESYM             ;Initialize the SYMBOL BUFFER to empty condition
           LLI 210                ;Load L with address of OPERATOR STACK pointer
           LMI 000                ;Initialize OPERATOR STACK pointer value
           LLI 276                ;Set L to address of EVAL pointer (start of expression)
           LBM                    ;Fetch the EVAL pointer value into register B
           LLI 200                ;Set up a working pointer register in this location
           LMB                    ;And initialize EVAL CURRENT pointer
SCAN1:     LLI 200                ;Load L with address of EVAL CURRENT pointer
           CAL GETCHR             ;Fetch a character in the expression being evaluated
           JTZ SCAN10             ;If character is a space, jump out of this section
           CPI 253                ;See if character is a "+" sign
           JFZ SCAN2              ;If not, continue checking for an operator
           LLI 176                ;If yes, set pointer to PARSER TOKEN storage location
           LMI 001                ;Place TOKEN value for "+" sign in PARSER TOKEN
           JMP SCANFN             ;Go to PARSER subroutine entry point
SCAN2:     CPI 255                ;See if character is a minus ("-") sign
           JFZ SCAN4              ;If not, continue checking for an operator
           LLI 120                ;If yes, check the length of the symbol stored in the
           LAM                    ;SYMBOL BUFFER by fetching the (cc) byte
           NDA                    ;And testing to see if (cc) is zero
           JFZ SCAN3              ;If length not zero, then not a unary minus indicator
           LLI 176                ;Else, check to see if last operator was a right parenthesi
           LAM                    ;By fetching the value in the PARSER TOKEN storage
           CPI 007                ;Location and seeing if it is token value for ")"
           JTZ SCAN3              ;If last operator was I')" then do not have a unary minus
           CPI 003                ;Check to see if last operator was C4*~2
           JTZ SYNERR             ;If yes, then have a syntax error
           CPI 005                ;Check to see if last operator was exponentiation
           JTZ SYNERR             ;If yes, then have a syntax error
           LLI 120                ;If none of the above, then minus sign is unary, put
           LMI 001                ;Character string representing the
           INL                    ;Value zero in the SYMBOL BUFFER in string format
           LMI 260                ;(Character count (cc) followed by ASCII code for zero)
SCAN3:     LLI 176                ;Set L to address of PARSER TOKEN storage location
           LMI 002                ;Set PARSER TOKEN value for minus operator
SCANFN:    CAL PARSER             ;Call the PARSER subroutine to process current symbol
           JMP SCAN10             ;And operator. Then jump to continue processing.
SCAN4:     CPI 252                ;See if character fetched from expression is
           JFZ SCAN5              ;If not, continue checking for an operator
           LLI 176                ;If yes, set pointer to PARSER TOKEN storage location
           LMI 003                ;Place TOKEN value for "*" (multiplication) operator in
           JMP SCANFN             ;PARSER TOKEN and go to PARSER subroutine entry
SCAN5:     CPI 257                ;See if character fetched from expression is
           JFZ SCAN6              ;If not, continue checking for an operator
           LLI 176                ;If yes, set pointer to PARSER TOKEN storage location
           LMI 004                ;Place TOKEN value for "/" (division) operator in
           JMP SCANFN             ;PARSER TOKEN and go to PARSER subroutine entry
SCAN6:     CPI 250                ;See if character fetched from expression is
           JFZ SCAN7              ;If not, continue checking for an operator
           LLI 230                ;If yes, load L with address of FUN/ARRAY STACK
           LBM                    ;Pointer. Fetch the value in the stack pointer. Increment
           INB                    ;It to indicate number of "(" operators encountered.
           LMB                    ;Restore the updated stack pointer back to memory
           CAL FUNARR             ;Call subroutine to process possible FUNCTION or
           LLI 176                ;ARRAY variable subscript. Ihen set pointer to
           LMI 006                ;PARSER TOKEN storage and set value for operator
           JMP SCANFN             ;Go to PARSER subroutine entry point.
SCAN7:     CPI 251                ;See if character fetched from expression is
           JFZ SCAN8              ;If not, continue checking for an operator
           LLI 176                ;If yes, load L with address of PARSER TOKEN
           LMI 007                ;Set PARSER TOKEN value to reflect ")"
           CAL PARSER             ;Call the  PARSER subroutine to process current symbol
	
           CAL PRIGHT             ;Call subroutine to handle FUNCTION or ARRAY
           LLI 230                ;Load L with address of FUN/ARRAY STACK pointer
           LHI OLDPG26/400        ;** Set H to page of FUN/ARRAY STACK pointer
           LBM                    ;Fetch the value in the stack pointer. Decrement it
           DCB                    ;To account for left parenthesis just processed.
           LMB                    ;Restore the updated value back to memory.
           JMP SCAN10             ;Jump to continue processing expression.
SCAN8:     CPI 336                ;See if character fetched from expression is " t
           JFZ SCAN9              ;If not, continue checking for an operator
           LLI 176                ;If yes, load L with address of PARSER TOKEN
           LMI 005                ;Put in value for exponentiation
           JMP SCANFN             ;Go to PARSER subroutine entry point.
SCAN9:     CPI 274                ;See if character fetched is the "less than" sign
           JFZ SCAN11             ;If not, continue checking for an operator
           LLI 200                ;If yes, set L to the EVAL CURRENT pointer
           LBM                    ;Fetch the pointer
           INB                    ;Increment it to point to the next character
           LMB                    ;Restore the updated pointer value
           CAL GETCHR             ;Fetch the next character in the expression
           CPI 275                ;Is the character the "= 9 $ sign?
           JTZ SCAN13             ;If so, have 'less than or equal" combination
           CPI 276                ;Is the character the "greater than" sign?
           JTZ SCAN15             ;If so, have "less than or greater than" combination
           LLI 200                ;Else character is not part of the operator. Set L back
           LBM                    ;To the EVAL CURRENT pointer. Fetch the pointer
           DCB                    ;Value and decriment it back one character in the
           LMB                    ;Expression. Restore the original pointer value.
           LLI 176                ;Have just the 'less than" operator. Set L to the
           LMI 011                ;PARSER TOKEN storage location and set the value for
           JMP SCANFN             ;The 'less than" sign then go to PARSER entry point.
SCAN11:    CPI 275                ;See if character fetched is the "= " sign
           JFZ SCAN12             ;If not, continue checking for an operator
           LLI 200                ;If yes, set L to the EVAL CURRENT pointer
           LBM                    ;Fetch the pointer
           INB                    ;Increment it to point to the next character
           LMB                    ;Restore the updated pointer value
           CAL GETCHR             ;Fetch the next character in the expression
           CPI 274                ;Is the character the "less than" sign?
           JTZ SCAN13             ;If so, have "less than or equal" combination
           CPI 276                ;Is the character the "greater than" sign?
           JTZ SCAN14             ;If so, have "equal or greater than" combination
           LLI 200                ;Else character is not part of the operator. Set L back
           LBM                    ;To the EVAL CURRENT pointer. Fetch the pointer
           DCB                    ;Value and decrement it back one character in the
           LMB                    ;Expression. Restore the original pointer value.
           LLI 176                ;Just have '~-- " operator. Set L to the PARSER TOKEN
           LMI 012                ;Storage location and set the value for the sign.
           JMP SCANFN             ;Go to the PARSER entry point.
SCAN12:    CPI 276                ;See if character fetched is the "greater than" sign
           JFZ SCAN16             ;If not, go append the character to the SYMBOL BUFF
           LLI 200                ;If so, set L to the EVAL CURRENT pointer
           LBM                    ;Fetch the pointer
           INB                    ;Increment it to point to the next character
           LMB                    ;Restore the updated pointer value
           CAL GETCHR             ;Fetch the next character in the expression
           CPI 274                ;Is the character the "less than" sign?
           JTZ SCAN15             ;If so, have "less than or greater than" combination
           CPI 275                ;Is the character the "= " sign?
           JTZ SCAN14             ;If so, have the "equal to or greater than " combination
           LLI 200                ;Else character is not part of the operator. Set L back
           LBM                    ;To the EVAL CURRENT pointer. Fetch the pointer
           DCB                    ;Value and decrement it back one character in the
           LMB                    ;Expression. Restore the original pointer value.
           LLI 176                ;Have just the "greater than" operator. Set L to the
           LMI 013                ;PARSER TOKEN storage location and set the value for
           JMP SCANFN             ;The "greater than" sign then go to PARSER entry
SCAN13:    LLI 176                ;When have 'less than or equal" combination set L to
           LMI 014                ;PARSER TOKEN storage location and set the value.
           JMP SCANFN             ;Then go to the PARSER entry point.
SCAN14:    LLI 176                ;When have "equal to or greater than" combination set L
           LMI 015                ;To PARSER TOKEN storage location and set the value.
           JMP SCANFN             ;Then go to the PARSER entry point.
SCAN15:    LLI 176                ;When have 'less than or greater than" combination set
           LMI 016                ;L to PARSER TOKEN storage location and set value.
           JMP SCANFN             ;Then go to the PARSER entry point.
SCAN16:    CAL CONCTS             ;Concatenate the character to the SYMBOL BUFFER
SCAN10:    LLI 200                ;Set L to the EVAL CURRENT pointer storage location
           LHI OLDPG26/400        ;** Set H to page of EVAL CURRENT pointer
           LBM                    ;Fetch the EVAL CURRENT pointer value into B
           INB                    ;Increment the pointer value to point to next character
           LMB                    ;In the expression and restore the updated value.
           LLI 277                ;Set L to EVAL FINISH storage location.
           LAM                    ;Fetch the EVAL FINISH value into the accumulator.
           DCB                    ;Set B to last character processed in the expression.
           CPB                    ;See if last character was at EVAL FINISH location.
           JFZ SCAN1              ;If not, continue processing the expression. Else, jump
           JMP PARSEP             ;To final evaluation procedure and test.  (Directs routine
           HLT                    ;To a dislocated section.) Safety Halt in unused byte.
PARSER:    LLI 120                ;Load L with starting address of SYMBOL BUFFER
           LHI OLDPG26/400        ;** Load H with page of SYMBOL BUFFER
           LAM                    ;Fetch the (cc) for  contents of SYMBOL BUFFER
           NDA                    ;Into the ACC and see if buffer is  empty
           JTZ PARSE              ;If empty then no need to convert contents
           INL                    ;If not empty, advance buffer pointer
           LAM                    ;Fetch the first character in the buffer
           CPI 256                ;See if it is ASCII code for decimal sign
           JTZ PARNUM             ;If yes, consider contents of buffer to be a number
           CPI 260                ;If not decimal sign, see if first character represents
           JTS LOOKUP             ;A deciinal digit, if not, should have a variable
           CPI 272                ;Continue to test for a decimal digit
           JFS LOOKUP             ;If not, go look up the variable nwne
PARNUM:    DCL                    ;If SYMBOL BUFFER contains number, decrement
           LAM                    ;Buffer pointer back to (cc) and fetch it to ACC
           CPI 001                ;See if length of string in buffer is just one
           JTZ NOEXPO             ;If so, cannot have number with scientific notation
           ADL                    ;If not, add length to buffer pointer to
           LLA                    ;Point to last character in the buffer
           LAM                    ;Fetch the last character in buffer and see if it
           CPI 305                ;Represents letter E for Exponent
           JFZ NOEXPO             ;If not, cannot have number with scientific notation
           LLI 200                ;If yes, have part of a scientific number, set pointer to
           CAL GETCHR             ;Get the operator that follows the E and append it to
           JMP CONCTS             ;The SYMBOL BUFFER and return to EVAL routine
NOEXPO:    LLI 227                ;Load L with address of ARITHMETIC STACK pointer
           LHI OLDPG1/400         ;** Load H with page of ARITHMETIC STACK pointer
           LAM                    ;Fetch AS pointer value to ACC and add four to account
           ADI 004                ;For the number of bytes required to store a number in
           LMA                    ;Floating point format. Restore pointer to mernory.
           LLA                    ;Then, change L to point to entry position in the AS
           CAL FSTORE             ;Place contents of the FPACC onto top of the AS
           LLI 120                ;Change L to point to start of the SYMBOL BUFFER
           LHI OLDPG26/400        ;** Set H to page of the SYMBOL BUFFER
           CAL DINPUT             ;Convert number in the buffer to floating point format
           JMP PARSE              ;In the FPACC then jump to check operator sign.
LOOKUP:    LLI 370                ;Load L with address of LOOK-UP COUNTER
           LHI OLDPG26/400        ;** Load H with page of the counter
           LMI 000                ;Initialize the counter to zero
           LLI 120                ;Load L with starting address of the SYMBOL BUFFER
           LDI OLDPG27/400        ;** Load D with page of the VARIABLES TABLE
           LEI 210                ;Load E with start of the VARL433LES TABLE
           LAM                    ;Fetch the (cc) for the string in the SYMBOL BUFFER
           CPI 001                ;See if the name length is just one character. If not,
           JFZ LOOKU1             ;Should be two so proceed to look-up routine. Else,
           LLI 122                ;Change L to second character byte in the buffer and set
           LMI 000                ;It to zero to provide compatibility with entries in table
LOOKU1:    LLI 121                ;Load L with addr of first character in the SYMBOL
           LHI OLDPG26/400        ;** BUFFER. Set H to page of the SYMBOL BUFFER.
           CAL SWITCH             ;Exchange contents of D&E with H&L so that can
           LAM                    ;Fetch the first character of a name in the VARIABLES
           INL                    ;TABLE. Advance the table pointer and save the
           LBM                    ;Second byte of name in B. Then advance the pointer
           INL                    ;Again to reach first bvte of floating point forrnatted
           CAL SWITCH             ;Number in table. Now exchange D&E with H&L and
           CPM                    ;Compare first byte in table against first char in buffer
           JFZ LOOKU2             ;If not the same, go try next entry in table. If same,
           INL                    ;Advance pointer to next char in buffer. Transfer the
           LAB                    ;Character in B (second byte in table entry) to the ACC
           CPM                    ;Compare it against second character in the buffer.
           JTZ LOOKU4             ;If match, have found the name in the VARIABLES tbl.
LOOKU2:    CAL AD4DE              ;Call subroutine to add four to the pointer in D&E to
           LLI 370                ;Advance the table pointer over value bytes. Then set
           LHI OLDPG26/400        ;** Up H and L to point to LOOK-UP COUNTER.
           LBM                    ;Fetch counter value (counts number of entries tested
           INB                    ;In the VARIABLES TABLE), increment it
           LMB                    ;And restore it back to meynory
           LLI 077                ;Load L with address of SYMBOL VARIABLES counter
           LHI OLDPG27/400        ;** Do same for H. (Counts number of names in table.)
           LAB                    ;Place LOOK-UP COUNTER value in the accumulator.
           CPM                    ;Compare it with number of entries in the table.
           JFZ LOOKU1             ;If have not reached end of table, keep looking for name.
           LLI 077                ;If reach end of table without match, need to add name
           LHI OLDPG27/400        ;** To table. First set H & L to the SYMBOL
           LBM                    ;VARIABLES counter. Fetch the counter value and
           INB                    ;Increment to account for new name being added to the
           LMB                    ;Table. Restore the updated count to meinory. Also,
           LAB                    ;Move the new counter value to the accumulator and
           CPI 025                ;Check to see that table size is not exceeded. If try to
           JFS BIGERR             ;Go over 20 (decirnal) entries then have BIG error.
           LLI 121                ;Else, set L to point to first character in the SYMBOL
           LHI OLDPG26/400        ;** BUFFER and set H to proper page. Set the number
           LBI 002                ;Of bytes to be transferred into register B as a counter.
           CAL MOVEIT             ;Move the symbol name from the buffer to the
           LLE                    ;VARIABLES TABLE. Now set up H & L with value
           LHD                    ;Contained in D & E after moving ops (points to first
           XRA                    ;Byte of the value to be associated with the symbol
           LMA                    ;Name.) Clear the accumulator and place zero in all four
           INL                    ;Bytes associated with the variable name entered
           LMA                    ;In the VARIABLES TABLE
           INL                    ;In order to
           LMA                    ;Assign an
           INL                    ;Initial value
           LMA                    ;To the variable narne
           LAL                    ;Then transfer the address in L to the acc'umulator
           SUI 004                ;Subtract four to reset the pointer to start of zeroing ops
           LEA                    ;Restore the address in D & E to be in same state as if
           LDH                    ;Name was found in the table in the LOOKUP routine
LOOKU4:    CAL SAVEHL             ;Save current address to VARIABLES TABLE
           LLI 227                ;Load L with address of ARITHMETIC STACK pointer
           LHI OLDPG1/400         ;** Load H with page of the pointer
           LAM                    ;Fetch the AS pointer value to the accumulator
           ADI 004                ;Add four to account for next floating point forrnatted
           LMA                    ;Number to be stored in the stack. Restore the stack
           LLA                    ;Pointer to memory and set it up in register L too.
           CAL FSTORE             ;Place the value in the FPACC on the top of the
           CAL RESTHL             ;ARITHMETIC STACK. Restore the VARIABLES
           CAL SWITCH             ;TABLE pointer to H&L and move it to D&E. Now load
           CAL FLOAD              ;The VARIABLE value from the table to the FPACC.
PARSE:     CAL CLESYM             ;Clear the SYMBOL BUFFER
           LLI 176                ;Load L with address of PARSER TOKEN VALUE
           LAM                    ;And fetch the token value into the accumulator
           CPI 007                ;Is it token value for right parenthesis ")" ? If so, have
           JTZ PARSE2             ;Special case where must perforin ops til find a "(" !
           ADI 240                ;Else, fon-n address to HEIRARCHY IN table and
           LLA                    ;Set L to point to HEIRARCHY IN VALUE in the table
           LBM                    ;Fetch the heirarchy value from the table to register B
           LLI 210                ;Set L to OPERATOR STACK pointer storage location
           LCM                    ;Fetch the OS pointer into CPU register C
           CAL INDEXC             ;Add OS pointer to address of OS pointer storage loc
           LAM                    ;Fetch the token value for the operator at top of the OS
           ADI 257                ;And form address to HEIRARCHY OUT table
           LLA                    ;Set L to point to HEIRARCHY OUT VALUE in the
           LAB                    ;Table. Move the HEIRARCHY IN value to the ACC.
           CPM                    ;Compare the HEIRARCHY IN with the HEIRARCHY
           JTZ PARSE1             ;OUT value. If heirarchy of current operator equal to or
           JTS PARSE1             ;Less than operator on top of OS stack, perfo
           LLI 176                ;Operation indicated in top of OS stack. Else, fetch the
           LBM                    ;Current operator token value into register B.
           LLI 210                ;Load L with address of the OPERATOR STACK pntr
           LCM                    ;Fetch the stack pointer value
           INC                    ;Increment it to account for new entry on the stack
           LMC                    ;Restore the stack pointer value to memory
           CAL INDEXC             ;For in pointer to next entry in OPERATOR STACK
           LMB                    ;Place the current operator token value on top of the OS
           RET                    ;Exit back to the EVAL routine.
PARSE1:    LLI 210                ;Load L with address of the OPERATOR STACK pntr
           LAM                    ;Fetch the stack pointer value to the accumulator
           ADL                    ;Add in the value of the stack pointer address to form
           LLA                    ;Address that points to top entry in the OS
           LAM                    ;Fetch the token value at the top of the OS to the ACC
           NDA                    ;Check to see if the token value is zero for end of stack
           RTZ                    ;Exit back to the EVAL routine if stack empty
           LLI 210                ;Else, reset L to the OS pointer storage location
           LCM                    ;Fetch the pointer value
           DCC                    ;Decrement it to account for operator rernoved from
           LMC                    ;The OPERATOR STACK and restore the pointer value
           CAL FPOPER             ;Perform the operation obtained from the top of the OS
           JMP PARSE              ;Continue to compare current operator against top of OS
PARSE2:    LLI 210                ;Load L with address of the OPERATOR STACK pntr
           LHI OLDPG26/400        ;** Load H with page of the pointer
           LAM                    ;Fetch the stack pointer value to the accumulator
           ADL                    ;Add in the value of the stack pointer address to form
           LLA                    ;Address that points to top entry in the OS
           LAM                    ;Fetch the token value at the top of the 0 S to the ACC
           NDA                    ;Check to see if the token value is zero for end of stack
           JTZ PARNER             ;If end of stack, then have a parenthesis error condx
           LLI 210                ;Else, reset L to the OS pointer storage location
           LCM                    ;Fetch the pointer value
           DCC                    ;Decrement it to account for operator removed from
           LMC                    ;The OPERATOR STACK and restore the pointer value
           CPI 006                ;Check to see if token value is "(" to close parenthesis
           RTZ                    ;If so, exit back to EVAL routine.
           CAL FPOPER             ;Else, perforin the op obtained from the top of the OS
           JMP PARSE2             ;Continue to process data in parenthesis
FPOPER:    LLI 371                ;Load L with address of TEMP OP storage location
           LHI OLDPG26/400        ;** Load H with page of TEMP OP storage location
           LMA                    ;Store OP (from top of OPERATOR STACK)
           LLI 227                ;Change L to address of ARff HMETIC STACK pointer
           LHI OLDPG1/400         ;** Load H with page of AS pointer
           LAM                    ;Fetch AS pointer value into ACC
           LLA                    ;Set L to top of ARITHMETIC STACK
           CAL OPLOAD             ;Transfer number from ARffHMETIC STACK to FPOP
           LLI 227                ;Restore pointer to AS pointer
           LAM                    ;Fetch the pointer value to the ACC and subtract four
           SUI 004                ;To remove top value from the ARITHMETIC STACK
           LMA                    ;Restore the updated AS pointer to memory
           LLI 371                ;Set L to address of TEMP OP storage location
           LHI OLDPG26/400        ;** Set H to page of TEMP OP storage location
           LAM                    ;Fetch the operator token value to the ACC
           CPI 001                ;Find out which kind of operation indicated
           JTZ FPADD              ;Perforn addition if have plus operator
           CPI 002                ;If not plus, see if minus
           JTZ FPSUB              ;Perform subtraction if have minus operator
           CPI 003                ;If not minus, see if multiplication
           JTZ FPMULT             ;Perform multiplication if have multiplication operator
           CPI 004                ;If not multiplication, see if division
           JTZ FPDIV              ;Perform division if have division operator
           CPI 005                ;If not division, see if exponentiation
           JTZ INTEXP             ;Perform exponentiation if have exponentiation operator
           CPI 011                ;If not exponentiation, see if "less than" operator
           JTZ LT                 ;Perform compaison for "less than" op if indicated
           CPI 012                ;If not 'less than" see if have "equal" operator
           JTZ EQ                 ;Perforin comparison for "equal" op if indicated
           CPI 013                ;If not "equal" see if have "greater than" operator
           JTZ GT                 ;Perform comparison for "greater than" op if indicated
           CPI 014                ;If not "'greater than" see if have 'less than or equal" op
           JTZ LE                 ;Perform comparison for the combination op if indicated
           CPI 015                ;See if have "equal to or greater than" operator
           JTZ GE                 ;Perform comparison for the combination op if indicated
           CPI 016                ;See if have "less than or greater than" operator
           JTZ NE                 ;Perform comparison for the combination op if indicated
PARNER:    LLI 230                ;If cannot find operator, expression is not balanced
           LHI OLDPG26/400        ;** Set H and L to address of F/A STACK pointer
           LMI 000                ;Clear the F/A STACK pointer to re-initialize
           LAI 311                ;Load ASCII code for letter I into the accumulator
           LCI 250                ;And code for "(" character into register C
           JMP ERROR              ;Go display 1( for "Imbalanced Parenthesis") error msg
LT:        CAL FPSUB              ;Subtract contents of FPACC from FPOP to compare
           LLI 126                ;Set L to point to the MSW of the FPACC (Contains
           LAM                    ;Result of the subtraction.) Fetch the MSW of the
           NDA                    ;FPACC to the accumulator and test to see if result is
           JTS CTRUE              ;Positive or negative. Set up the FPACC as a function
           JMP CFALSE             ;Of the result obtained.
EQ:        CAL FPSUB              ;Subtract contents of FPACC from FPOP to compare
           LLI 126                ;Set L to point to the MSW of the FPACC (Contains
           LAM                    ;Result of the subtraction.) Fetch the MSW of the
           NDA                    ;FPACC to the accumulator and test to see if result is
           JTZ CTRUE              ;Equal. Set up the FPACC as a function
           JMP CFALSE             ;Of the result obtained.
GT:        CAL FPSUB              ;Subtract contents of FPACC from FPOP to compare
           LLI 126                ;Set L to point to the MSW of the FPACC (Contains
           LAM                    ;Result of the subtraction.) Fetch the MSW of the
           NDA                    ;FPACC to the accumulator and test to see if result is
           JTZ CFALSE             ;Positive, Negative, or Equal. Set up the FPACC
           JFS CTRUE              ;As a function
           JMP CFALSE             ;Of the result obtained.
LE:        CAL FPSUB              ;Subtract contents of FPACC from FPOP to compare
           LLI 126                ;Set L to point to the MSW of the FPACC (Contains
           LAM                    ;Result of the subtraction.) Fetch the MSW of the
           NDA                    ;FPACC to the accumulator and test to see if result is
           JTZ CTRUE              ;Positive, Negative, or Equal. Set up the FPACC
           JTS CTRUE              ;As a function
           JMP CFALSE             ;Of the result obtained
GE:        CAL FPSUB              ;Submit contents of FPACC from FPOP to compare
           LLI 126                ;Set L to point to the MSW of the FPACC (Contains
           LAM                    ;Result of the subtraction.) Fetch the MSW of the
           NDA                    ;FPACC to the accumulator and test to see if result is
           JFS CTRUE              ;Positive or Negative. Set up the FPACC
           JMP CFALSE             ;As a function of the result obtained
NE:        CAL FPSUB              ;Subtract contents of FPACC from FPOP to compare
           LLI 126                ;Set L to point to the MSW of the FPACC (Contains
           LAM                    ;Result of the subtraction.) Fetch the MSW of the
           NDA                    ;FPACC to the accumulator and test to see if result is
           JTZ CFALSE             ;Equal. Set up the FPACC as a function of the result.
CTRUE:
FPONE:     LLI 004                ;Load L with address of floating point value +1.0
           JMP FLOAD              ;Load FPACC with value +1.0 and exit to caller
CFALSE:    LLI 127                ;Load L with address of FPACC Exponent register
           LMI 000                ;Set the FPACC Exponent to zero and then set the
           JMP FPZERO             ;Mantissa portion of the FPACC to zero. Exit to caller.
AD4DE:     LAE                    ;Subroutine to add four to the value in register E.
           ADI 004                ;Move contents of E to the ACC and add four.
           LEA                    ;Restore the updated value back to register E.
           RET                    ;Return to the calling routine.
INTEXP:    LLI 126                ;Load L with address of WSW of FPACC (Floating Point
           LHI OLDPG1/400         ;** ACCumulator). Load H with page of FPACC.
           LAM                    ;Fetch MSW of the FPACC into the accumulator.
           LLI 003                ;Load L with address of EXP TEMP storage location
           LMA                    ;Store the FPACC MSW value in EXP TEMP location
           NDA                    ;Test contents of the MSW of the FPACC. ff zero, then
           JTZ FPONE              ;Set FPACC equal to +1.0 (any nr to zero power = 1.0!)
           CTS FPCOMP             ;If MSW indicates negative number, complement
           CAL FPFIX              ;The FPACC. Then convert floating point number to
           LLI 124                ;Fixed point. Load L with address of LSW of fixed nr
           LBM                    ;Fetch the LSW into CPU register B.
           LLI 013                ;Set L to address of EXPONENT COUNTER
           LMB                    ;Place the fixed value in the EXP CNTR to indicate
           LLI 134                ;Number of multiplications needed (power). Now set L
           LEI 014                ;To LSW of FPOP and E to address of FP TEMP (LSW)
           LHI OLDPG1/400         ;** Set H to floating point working area page.
           LDH                    ;Set D to same page address.
           LBI 004                ;Set transfer (precision) counter. Call subroutine to move
           CAL MOVEIT             ;Contents of FPOP into FP TEMP registers to save
           CAL FPONE              ;Original value of FPOP. Now set FPACC to +1.0.
           LLI 003                ;Load L with pointer to original value of FPACC
           LAM                    ;(Stored in FP TEMP) MSW and fetch contents to ACC.
           NDA                    ;Test to see if raising to a negative power. If so, divide
           JTS DVLOOP             ;Instead of multiply!
MULOOP:    LLI 014                ;Load L with address of LSW of FP TEMP (original
           CAL FACXOP             ;Value in FPOP). Move FP TEMP into FPOP.
           CAL FPMULT             ;Multiply FPACC by FPOP. Result left in FPACC.
           LLI 013                ;Load L with address of EXPONENT COUNTER.
           LBM                    ;Fetch the counter value
           DCB                    ;Decrement it
           LMB                    ;Restore it to memory
           JFZ MULOOP             ;If counter not zero, continue exponentiation process
           RET                    ;When have raised to proper power, return to caller.
DVLOOP:    LLI 014                ;Load L with address of LSW of FP TEMP (original
           CAL FACXOP             ;Value in FPOP). Move FP TEMP into FPOP.
           CAL FPDIV              ;Divide FPACC by FPOP. Result left in FPACC.
           LLI 013                ;Load L with address of EXPONENT COUNTER
           LBM                    ;Fetch the counter value
           DCB                    ;Decrement it
           LMB                    ;Restore to memory
           JFZ DVLOOP             ;If counter not zero, continue exponentiation process
           RET                    ;When have raised to proper power, return to caller.

PRIGHT:    LLI 230                ;Load L with address of F/A STACK pointer
           LHI OLDPG26/400        ;** Load H with page of F/A STACK pointer
           LAM                    ;Fetch the pointer value into the ACC
           ADL                    ;Form pointer to top of the F/A STACK
           LLA                    ;Set L to point to top of the F/A STACK
           LAM                    ;Fetch the contents of the top of the F/A STACK into
           LMI 000                ;The ACC then clear the top of the F/A STACK
           LLI 203                ;Load L with address of F /A STACK TEMP storage
           LHI OLDPG27/400        ;** Location. Set H to page of F/A STACK TEMP
           LMA                    ;Store value from top of F/A STACK into temp loc.
           NDA                    ;Test to see if token value in top of stack was zero
           RTZ                    ;If so, just had simple grouping parenthesis!
           JTS PRIGH1             ;@@ If token value minus, indicates array subscript
           CPI 001                ;For positive token value, look for appropriate function
           JTZ INTX               ;If token value for INTeger function, go do it.
           CPI 002                ;Else, see if token value for SIGN function.
           JTZ SGNX               ;If so, go do it.
           CPI 003                ;Else, see if token value for ABSolute function
           JTZ ABSX               ;If so, go do it.
           CPI 004                ;If not, see if token value for SQuare Root function
           JTZ SQRX               ;If so, go do it.
           CPI 005                ;If not, see if token value for TAB function
           JTZ TABX               ;If so, go do it.
           CPI 006                ;If not, see if token value for RaNDom function
           JTZ RNDX               ;If so, go find a random number.
           CPI 007                ;If not, see if token value for CHaRacter function
           JTZ CHRX               ;If so, go perform the function.
           CPI 010                ;Else, see if token for user defined machine language
           JTZ UDEFX              ;# Function. If so, perform the User DEfined Function
           HLT                    ;Safety halt. Program should not reach this location!

FUNARR:    LLI 120                ;Load L with starting address of SYMBOL BUFFER
           LHI OLDPG26/400        ;** Load H with page of SYMBOL BUFFER
           LAM                    ;Fetch the (cc) for contents of buffer to the ACC
           NDA                    ;See if (cc) is zero, if so buffer is empty, return to
           RTZ                    ;Caller as have simple grouping parenthesis sign
           LLI 202                ;Else set L to TEMP COUNTER location
           LHI OLDPG27/400        ;** Set H to TEMP COUNTER page
           LMI 000                ;Initialize TEMP COUNTER to zero
FUNAR1:    LLI 202                ;Load L with address of TEMP COUNTER
           LHI OLDPG27/400        ;** Load H with page of TEMP COUNTER
           LBM                    ;Fetch the counter value to register B
           INB                    ;Increment the counter
           LMB                    ;Restore the updated value to memory
           LCI 002                ;Initialize C to a value of two for future ops
           LLI 274                ;Load L with starting address (less four) of FUNCTION
           LHI OLDPG26/400        ;** LOOK-UP TABLE. Set H to table page.
           CAL TABADR             ;Find address of next entry in the table
           LDI OLDPG26/400        ;** Load D with page of SYMBOL BUFFER
           LEI 120                ;Load E with starting address of SYMBOL BUFFER
           CAL STRCP              ;Compare entry in FUNCTION LOOK-UP TABLE with
           JTZ FUNAR4             ;Contents of SYMBOL BUFFER. If find match, go set
           LLI 202                ;Up the function token value. Else, set L to the TEMP
           LHI OLDPG27/400        ;** COUNTER and set H to the proper page. Fetch the
           LAM                    ;Current counter value and see if have tried all eight
           CPI 010                ;Possible functions in the table.
           JFZ FUNAR1             ;If not, go back and check the next entry.
           LLI 202                ;If have tried all of the entries in the table, set L
           LHI OLDPG27/400        ;** As well as H to the address of the TEMP COUI,.7ER
           LMI 000                ;And reset it to zero. Now go see if have subscripted
           JMP FUNAR2             ;@@ Array (unless array capability not in program).
FAERR:     LLI 230                ;Load L with address of F/A STACK pointer
           LHI OLDPG26/400        ;** Load H with page of F/A STACK pointer
           LMI 000                ;Clear the F/A STACK pointer to reset on an error
           LAI 306                ;Load the ASCII code for letter F into the ACC
           LCI 301                ;Load the ASCII code for letter A into register C
           JMP ERROR              ;Go display the FA error message
FUNAR4:    LLI 202                ;Load L with address of TEMP COUNTER
           LHI OLDPG27/400        ;** Set H to page of TEMP COUNTER
           LBM                    ;Load value in counter to register B. This is FUNCTION
           LLI 230                ;TOKEN VALUE. Cbange- L to F/A STACK pointer.
           LHI OLDPG26/400        ;** Load H with page of F/A STACK pointer.
           LCM                    ;Fetch the F/A STACK pointer value into register C.
           CAL INDEXC             ;Form the address to the top of the F/A STACK.
           LMB                    ;Store the FUNCTION TOKEN VALUE in the F/A
           JMP CLESYM             ;STACK. Then exit by clearing the SYMBOL BUFFER.
TABADR:    LAB                    ;Move the TEMP COUNTER value from B to ACC
TABAD1:    RLC                    ;Multiply by four using this loop to form value equal
           DCC                    ;To number of bytes per entry (4) times current entry
           JFZ TABAD1             ;In the FUNCTION LOOK-UP TABLE.
           ADL                    ;Add this value to the starting address of the table.
           LLA                    ;Form pointer to next entry in table
           RFC                    ;If no carry return to caller
           INH                    ;Else, increment H before
           RET                    ;Returning to caller

INTX:      LLI 126                ;Load L with address of MSW of the FPACC
           LHI OLDPG1/400         ;** Load H with the page of the PPACC
           LAM                    ;Fetch the MSW of the FPACC into the accumulator
           NDA                    ;Test the sign of the number in the FPACC. If
           JFS INT1               ;Positive jump ahead to integerize
           LLI 014                ;If negative, load L with address of FP TEMP registers
           CAL FSTORE             ;Store the value in the FPACC in FP TEMP
           CAL FPFIX              ;Convert the value in FPACC from floating point to
           LLI 123                ;Fixed point. Load L with address of FPACC
           LMI 000                ;Extension register and clear it.
           CAL FPFLT              ;Convert fixed binary back to FP to integerize
           LLI 014                ;Load L with address of FP TEMP registers
           CAL OPLOAD             ;Load the value in FP TEMP into FPOP
           CAL FPSUB              ;Subtract integerized value from original
           LLI 126                ;Set L to address of MSW of FPACC
           LAM                    ;Fetch the MSW of the FPACC into the accumulator
           NDA                    ;See if original value and integerized value the same
           JTZ INT2               ;If so, have integer value in FP TEMP
           LLI 014                ;Else, load L with address of FP TEMP registers
           CAL FLOAD              ;Restore FPACC to original (non-integerized) value
           LLI 024                ;Set L to register containing small value
           CAL FACXOP             ;Set up to add small value to original value in FPACC
           CAL FPADD              ;Perform the addition
INT1:      CAL FPFIX              ;Convert the number in FPACC from floating point
           LLI 123                ;To fixed point. Load L with address of FPACC
           LMI 000                ;Extension register and clear it. Now convert the number
           JMP FPFLT              ;Back to floating point to integerize it and exit to caller
INT2:      LLI 014                ;Load L with address of FP TEMP registers. Transfer
           JMP FLOAD              ;Number from FP TEMP (orig) to FPACC and return.
ABSX:      LLI 126                ;Load L with address of MSW of the FPACC
           LHI OLDPG1/400         ;** Set H to page of the FPACC
           LAM                    ;Fetch the MSW of the FPACC into the accumulator
           NDA                    ;Test the sign of the number to see if it is positive.
           JTS FPCOMP             ;If negative, complement the number before returning.
           RET                    ;Else, just return with absolute value in the FPACC.
SGNX:      LLI 126                ;Load L with address of MSW of the FPACC
           LHI OLDPG1/400         ;** Load H with the page of the FPACC
           LAM                    ;Fetch the MSW of the FPACC into the accumulator
           NDA                    ;Test to see if the FPACC is zero
           RTZ                    ;Return to caller if FPACC is zero
           JFS FPONE              ;If FPACC is positive, load +1.0 into FPACC and exit
           LLI 024                ;If FPACC is negative, set up to load -1.0 into the
           JMP FLOAD              ;FPACC and exit to caller
CHRX:      CAL FPFIX              ;Convert contents of FPACC from floating point to
           LLI 124                ;Fixed point. Load L with address of LSW of fixed
           LAM                    ;Value. Fetch this byte into the accumulator.
           CAL ECHO               ;Display the value.
           LLI 177                ;Set L to address of the TAB FLAG
           LHI OLDPG26/400        ;** Set H to page of the TAB FLAG
           LMI 377                ;Set TAB FLAG (to inhibit display of FP value)
           RET                    ;Exit to caller.
TABX:      CAL FPFIX              ;Convert contents of FPACC from floating point to
TAB1:      LLI 124                ;Fixed point. Load L with address of 1,SW of fixed
           LAM                    ;Value. Fetch this byte into the accumulator.
           LLI 043                ;Load L with address of COLUMN COUNTER
           SUM                    ;Subtract value in C-OLUMN COUNTER from desired
           LLI 177                ;TAB position. Load L with address of the TAB FLAG.
           LHI OLDPG26/400        ;** Set H to page of the TAB FLAG.
           LMI 377                ;Set TAB FLAG (to inhibit display of FP value)
           JTS BACKSP             ;If beyond TAB point desired, simulate back spacing
           RTZ                    ;Return to caller if at desired TAB location
TABC:      LCA                    ;Else, put difference count in register C
           LAI 240                ;Place ASCII code for space in ACC
TABLOP:    CAL ECHO               ;Display space on output device
           DCC                    ;Decrement displacement counter
           JFZ TABLOP             ;If have not reached TAB position, continue to space
           RET                    ;Else, return to calling routine.

STOSYM:    LLI 201                ;Load L with address of ARRAY FLAG
           LHI OLDPG27/400        ;** Load H with page of ARRAY FLAG
           LAM                    ;Fetch the value of the ARRAY FLAG into the ACC
           NDA                    ;Check to see if the flag is set indicating processing an
           JTZ STOSY1             ;Array variable value. Jump ahead if flag not set.
           LMI 000                ;If ARRAY FLAG was set, clear it for next time.
           LLI 204                ;Then load L with address of array address storage loc
           LLM                    ;Fetch the array storage address as new pointer
           LHI OLDPG57/400        ;tt Set H to ARRAY VALUES page   ****************
           JMP FSTORE             ;Store the array variable value and exit to caller.
STOSY1:    LLI 370                ;Load L with address of TEMP CNTR
           LHI OLDPG26/400        ;** Load H with page of TEMP CNTR
           LMI 000                ;Initialize the TEMP CNTR by clearing it
           LLI 120                ;Load L with starting address of SYMBOL BUFFER
           LDI OLDPG27/400        ;** Load D with page of VARIABLES LOOK-UP table
           LEI 210                ;Load E with starting addr of VARIABLES LOOK-UP
           LAM                    ;Table. Fetch the (cc) for the SYMBOL BUFFER into
           CPI 001                ;The ACC and see if length of variable name is just one
           JFZ STOSY2             ;Character. If not, skip next couple of instructions.
           LLI 122                ;Else, set pointer to second character location in the
           LMI 000                ;SYMBOL BUFFER and set it to zero
STOSY2:    LLI 121                ;load L with address of first character in the SYMBOL
           LHI OLDPG26/400        ;** BUFFER. Load H with page of the buffer.
           CAL SWITCH             ;Exchange pointer to buffer for pointer to VARIABLES
           LAM                    ;LOOK-UP table. Fetch first char in a name from the
           INL                    ;Table. Advance the pointer to second char in a name.
           LBM                    ;Fetch the second character into register B.
           INL                    ;Advance the pointer to first byte of a value in the table.
           CAL SWITCH             ;Exchange table pointer for pointer to SYMBOL BUFF
           CPM                    ;Compare first character in buffer against first character
           JFZ STOSY3             ;In table entry. If no match, try next entry in the table.
           INL                    ;If match, advance pointer to second character in buffer.
           LAB                    ;Move second character obtained from table into ACC.
           CPM                    ;Compare second characters in table and buffer.
           JTZ STOSY5             ;If same, have found the variable name in the table.
STOSY3:    CAL AD4DE              ;Add four to pointer in registers D&E to skip over value
           LLI 370                ;Portion of entry in table. Load L with address of TEMP
           LHI OLDPG26/400        ;** CNTR. Load H with page of TEMP CNTR.
           LBM                    ;Fetch the counter
           INB                    ;Increment the counter
           LMB                    ;Restore it to storage
           LLI 077                ;Set L to address of VARIABLES CNTR (indicates
           LHI OLDPG27/400        ;** Number of variables currently in table.) Set H too
           LAB                    ;Move the TEMP CNTR value into the ACC. (Number of
           CPM                    ;Entries checked.) Compare with number of entries in
           JFZ STOSY2             ;The table. If have not checked all entries, try next one.
           LLI 077                ;If have checked all entries, load L with address of the
           LHI OLDPG27/400        ;** VARIABLES CNTR. Set H too. Fetch the counter
           LBM                    ;Value and incrernent it to account for
           INB                    ;New variable nwne that will now be
           LMB                    ;Added to the table. Save the new value.
           LAB                    ;Place the new counter value into the accumulator
           CPI 025                ;And check to see that adding new variable name to the
           JFS BIGERR             ;Table will not cause table overflow. Big Error if it does!
           LLI 121                ;If room available in table, set L to address of first
           LHI OLDPG26/400        ;** Caracter in the SYMBOL BUFFER. Set H too.
           LBI 002                ;Set a counter for number of characters to transfer.
           CAL MOVEIT             ;Move the variable name from buffer to table.
STOSY5:    CAL SWITCH             ;Exchange buffer pointer for table pointer.
           CAL FSTORE             ;Transfer new mathematical value into the table.
           JMP CLESYM             ;Clear the SYMBOL BUFFER and exit to calling routine.

;The subroutines below are used by some of the routines in this chapter as well as 
;other parts of the program.
SAVESY:    LLI 120                ;Load L with the address of the start of the SYMBOL
           LHI OLDPG26/400        ;** BUFFER. Load H with the page of the buffer.
           LDH                    ;Load register D with the page of the AUX SYMBOL
           LEI 144                ;BUFFER and set register E to start of that buffer.
           JMP MOVECP             ;Transfer SYMBOL BF contents to AUX SYMBOL BF

RESTSY:    LLI 144                ;Load L with address of start of AUX SYMBOL BUFF
           LHI OLDPG26/400        ;** Load H with page of AUX SYMBOL BUFFER
           LDH                    ;Set D to page of SYMBOL BUFFER (same as H)
           LEI 120                ;Load E with start of SYMBOL BUFFER
MOVECP:    LBM                    ;Load (cc) for source string (first byte in source buffer)
           INB                    ;Add one to (cc) to include (cc) byte itself
           JMP MOVEIT             ;Move the source string to destination buffer

EXEC:      LLI 352                ;Load L with address of READY message
           LHI OLDPG1/400         ;** Load H with page of READY message
           CAL TEXTC              ;Call subroutine to display the READY message

EXEC1:     LLI 000                ;Load L with starting address of INPUT LINE BUFFER
           LHI OLDPG26/400        ;** Load H with page of INPUT LINE BUFFER
           CAL STRIN              ;Call subroutine to input a line into the buffer
           LAM                    ;The STRIN subroutine will exit with pointer set to the
           NDA                    ;CHARACTER COUNT for the line inputted. Fetch the
           JTZ EXEC1              ;Value of the counter, if it is zero then line was blank.
           LLI 335                ;Load L with address of LIST in look up table
           LHI OLDPG1/400         ;Load H with address of LIST in look up table
           LDI OLDPG26/400        ;Load D with page of line input buffer
           LEI 000                ;Load E with start of line input buffer
           CAL STRCP              ;Call string compare subroutine to see if first word in
           JFZ NOLIST             ;Input buffer is LIST. Jump 3 ahead if not LIST.
           LLI 000                ;If LIST, set up pointers to start of USER PROGRAM
           LHI BGNPGRAM           ;BUFFER. (Note user could alter this starting addr)   *****

;Next portion of program will LIST the contents of the USER PROGRAM BUFFER until an end of buffer
;(zero byte) indicator is detected.
LIST:      LAM                    ;Fetch the first byte of a line in the USER PROGRAM
           NDA                    ;BUFFER and see if it is zero. If so, have finished LIST
           JTZ EXEC               ;So go back to start of Executive and display READY.
           CAL TEXTC              ;Else call subroutine to display a line of information
           CAL ADV                ;Now call subroutine to advance buffer pointer to
           CAL CRLF               ;Character count in next line. Also display a CR & LF.
           JMP LIST               ;Continue LISTing process

;If line inputted by operator did not contain a LIST comman
;continue program to see if RUN or SCRatch command.
NOLIST:    LLI 342                ;Load L with address of RUN in look up table
           LHI OLDPG1/400         ;** Load H with address of RUN in look up table
           LEI 000                ;Load E with start of line input buffer
           LDI OLDPG26/400        ;** Load D with page of line input buffer
           LEI 000                ;(Reserve 2 locs in case of patching by duplicating above)
           CAL STRCP              ;Call string compare subroutine to see if first word in
           JTZ RUN                ;Input buffer is RUN. Go to RUN routine if match.
           LDI OLDPG26/400        ;** If not RUN command, reset address pointers back
           LEI 000                ;To the start of the line input buffer
           LLI 346                ;Load L with address of SCR in look up table
           LHI OLDPG1/400         ;** Load H with page of SCR in look up table
           CAL STRCP              ;Call string compare subroutine to see if first word in
           JFZ NOSCR              ;Input buffer is SCR. If not then jump ahead.
           LHI OLDPG26/400        ;** If found SCR command then load memory pointer
           LLI 364                ;With address of a pointer storage location. Set that
           LMI BGNPGRAM           ;tt Storage location to page of start of USER PRO-  *******
           INL                    ;GRAM BUFFER. (Buffer start loc may be altered).
           LMI 000                ;Then adv pntr and do same for low addr portion of pntr
           LLI 077                ;Now set pointer to address of VARIABLES counter
           LHI OLDPG27/400        ;** Storage location. Initialize this counter by placing
           LMI 001                ;The count of one into it. Now change the memory pntr
;;;        LMI 001                ;The count of one into it. Now change the memory pntr
;;; Apparently, in Page 3 of Issue 4 of Scelbal update (1/77) they say the above should change.
;;; This makes the SCR command clear the whole variable space, otherwise one space is lost.
	       LMI 000
           LLI 075                ;To storage location for number of dimensioned arrays
           LMI 000                ;@@ And initialize to zero. (@@ = Substitute NOPs if
           LLI 120                ;@@ DIMension capability not used in package.) Also
           LMI 000                ;@@ Initialize l'st byte of array name table to zero.
           LLI 210                ;Set pointer to storage location for the first byte of the
           LMI 000                ;VARIABLES symbol table. Initialize it to zero too.
           INL                    ;Advance the pointer and zero the second location
           LMI 000                ;In the Variables table also.
           LHI BGNPGRAM           ;tt Load H with page of start of USER PROGRAM    **********
           LLI 000                ;BUFFER. (Buffer start location could be altered.)
           LMI 000                ;Clear first location to indicate end of user program.
           LHI OLDPG57/400        ;@@ Load H with page of ARRAYS storage
SCRLOP:    LMI 000                ;@@ And form a loop to clear out all the locations
           INL                    ;@@ On the ARRAYS storage page. (@@ These become
           JFZ SCRLOP             ;@@ NOPs if DIMension capability deleted fm package.)
           JMP EXEC               ;SCRatch operations completed, go back to EXEC.

;If line inputted did not contain RUN or SCRatch command, program continues by testing 
;for SAVE or LOAD commands. If it does not find either of these commands, then operator
;did not input an executive command. Program then sets up to see if the first entry in
;the line inputted is a LINE NUMBER.
NOSCR:     LEI 272                ;Load E with address of SAVE in look up table
           LDI OLDPG1/400         ;Load D with page of look up table
           LHI OLDPG26/400        ;Load H with page of input line buffer
           LLI 000                ;Set L to start of input line buffer
           CAL STRCP              ;Call string compare subroutine to see if first word in
           JTZ SAVE               ;tt Input buffer is SAVE. If so, go to user's SAVE rtn
           LLI 277                ;If not SAVE then load L with address of LOAD in look
           LHI OLDPG1/400         ;Up table and load H with page of look up table
           LDI OLDPG26/400        ;Load D with page of input line buffer
           LEI 000                ;And L to start of input line buffer
           CAL STRCP              ;Call string compare subroutine to see if first word in
           JTZ LOAD               ;tt Input buffer is LOAD. If so, go to user's LOAD rtn
           LLI 360                ;If not LOAD then set pointer to address of storage loc
           LHI OLDPG26/400        ;** For USER PROGRAM BUFFER pointer. Initialize this
           LMI BGNPGRAM           ;tt Pointer to the starting address of the program buffer.
           INL                    ;Advance memory pntr. Since pointer storage requires
           LMI 000                ;Two locations, initialize the low addr portion also.
           CAL SYNTAX             ;Call the SYNTAX subroutine to obtain a TOKEN indi-
           LLI 203                ;Cator which will be stored in this location. Upon return
           LHI OLDPG26/400        ;** From SYNTAX subroutine set memory pointer to
           LAM                    ;The TOKEN indicator storage location and fetch the
           NDA                    ;Value of the TOKEN. If the value of the syntax TOKEN
           JFS SYNTOK             ;Is positive then have a valid entry.
SYNERR:    LAI 323                ;However, if SYNTAX returns a negative value TOKEN
           LCI 331                ;Then have an error condition. Set up the letters SY in
           JMP ERROR              ;ASCII code and go to display error message to operator.
SYNTOK:    LLI 340                ;Set pointer to start of LINE NUMBER storage area
           LAM                    ;First byte there will contain the length of the line
           NDA                    ;Number character string. Fetch that value (cc).
           JTZ DIRECT             ;DIRECT If line number blank, have a DIRECT statement!
           LLI 360                ;If have a line number must get line in input buffer into
           LMI BGNPGRAM           ;tt User program buffer. Initialize pointer to user buffer.
           INL                    ;This is a two byte pointer so after initializing page addr
           LMI 000                ;Advance pointer and initialize location on page address

;If the line in the LINE INPUT BUFFER has a line number then the line is to be placed in the 
;USER PROGRAM BUFFER. It is now necessary to determine where the new line is to be placed in 
;the USER PROGRAM BUFFER. This is dictated by the value of the new line number in relation to
;the line numbers currently in the program buffer. The next portion of the program goes through
;the contents of the USER PROGRAM BUFFER comparing the values of the line numbers already stored
;against the value of the line number currently being held in the LINE INPUT BUFFER. Appropriate
;action is then taken to Insert or Append, Change, or Delete a line in the program buffer.
GETAUX:    LLI 201                ;Set memory pointer to line character pointer storage
           LHI OLDPG26/400        ;** Location and then initialize that storage location
           LMI 001                ;To point to the 1'st character in a line
           LLI 350                ;Set memory pointer to addr of start of auxiliary line
           LMI 000                ;Number storage area and initialize first byte to zero
GETAU0:    LLI 201                ;Set memory pointer to line character pointer storage loc
           CAL GETCHP             ;Fetch a char in line pointed to by line pointer
           JTZ GETAU1             ;If character is a space, skip it by going to advance pntrs
           CPI 260                ;If not a space check to see if character represents a
           JTS GETAU2             ;Valid decimal digit in the range 0 to 9 by testing the
           CPI 272                ;ASCII code value obtained. If not a deciznal digit then
           JFS GETAU2             ;Assume have obtained the line number. Go process.
           LLI 350                ;If valid decimal digit want to append the digit to the
           LHI OLDPG26/400        ;** Current string being built up in the auxiliary line
           CAL CONCT1             ;Number storage area so call sub to concat a character.
GETAU1:    LLI 201                ;Reset memory pointer to line character pntr storage loc
           LHI OLDPG26/400        ;On the appropriate page.
           LBM
           INB                    ;Fetch the pointer, increment it, and restore new value
           LMB
           LLI 360                ;Set memory pointer to pgm buff line pntr storage loc
           LHI OLDPG26/400
           LCM                    ;Bring the high order byte of this double byte pointer
           INL                    ;Into CPU register C. Then advance the memory pntr
           LLM                    ;And bring the low order byte into register L. Now trans-
           LHC                    ;Fer the higher order portion into memory pointer H.
           LAM                    ;Obtain the char cntr (cc) which indicates the length of
           DCB                    ;The line being pointed to by the user program line pntr
           CPB                    ;Compare this with the value of the chars processed so
           JFZ GETAU0             ;Far in current line. If not equal, continue getting line n
GETAU2:    LLI 360                ;Reset mem pntr to pgm buffer line pntr storage
           LHI OLDPG26/400        ;** On this page and place the high order byte
           LDM                    ;Of this pointer into CPU register D
           INL                    ;Advance the memory pointer, fetch the second
           LLM                    ;Byte of the pgm buffer line pointer into register L
           LHD                    ;Now make the memory pointer equal to this value
           LAM                    ;Fetch the first byte of a line in the program buffer
           NDA                    ;Test to see if end of contents of pgm buff (zero byte)
           JFZ NOTEND             ;If not zero continue processing. If zero have reached
           JMP NOSAME             ;End of buffer contents so go APPEND line to buffer.

PATCH3:	   LLI 201		          ; ptr to A/V storage
           LHI OLDPG27/400
	       LMI 000		          ; clear A/V flag
	       JMP EXEC

NOTEND:    LLI 350                ;Load L with addr of auxiliary line number storage loc
           LHI OLDPG26/400        ;Load H with addr of aux line number storage loc
           LDI OLDPG26/400        ;Load D with addr of line number buffer location
           LEI 340                ;Load E with address of line number buffer location
           CAL STRCP              ;Compare line nr in input buffer with line number in
           JTS CONTIN             ;User program buffer. If lesser in value keep looking.
           JFZ NOSAME             ;If greater in value then go to Insert line in pgm buffer
           LLI 360                ;If same values then must remove the line with the same
           LHI OLDPG26/400        ;** Line number from the user program buffer. Set up
           LCM                    ;The CPU memory pointer to point to the current
           INL                    ;Position in the user program buffer by retrieving that
           LLM                    ;Pointer from its storage location. Then obtain the first
           LHC                    ;Byte of data pointed to which will be the character
           LBM                    ;Count for that line (cc). Add one to the cc value to take
           INB                    ;Account of the (cc) byte itself and then remove that
           CAL REMOVE             ;Many bytes to effectively delete the line fm the user
           LLI 203                ;Program buffer. Now see if line in input buffer consists
           LHI OLDPG26/400        ;** Only of a line number by checking SYNTAX
           LAM                    ;TOKEN value. Fetch the TOKEN value from its
           NDA                    ;Storage location. If it is zero then input buffer only
           JTZ EXEC               ;Contains a line number. Action is a pure Delete.
NOSAME:    LLI 360                ;Reset memory pointer to program buffer
           LHI OLDPG26/400        ;Line pointer storage location
           LDM                    ;Load high order byte into CPU register D
           INL                    ;Advance memory pointer
           LEM                    ;Load low order byte into CPU register E
           LLI 000                ;Load L with address of start of line input buffer
           LHI OLDPG26/400        ;** Do same for CPU register H
           LBM                    ;Get length of line input buffer
           INB                    ;Advance length by one to include (cc) byte
           CAL INSERT             ;Go make room to insert line into user program buffer
           LLI 360                ;Reset memory pointer to program buffer
           LHI OLDPG26/400        ;** Line pointer storage location
           LDM                    ;Load higher byte into CPU register D
           INL                    ;Advance memory pointer
           LEM                    ;Load low order byte into CPU register E
           LLI 000                ;Load L with address of start of line input buffer
           LHI OLDPG26/400        ;** Do same for CPU register H
           CAL MOVEC              ;Call subroutine to Insert line in input buffer into the
           JMP EXEC1              ;User program buffer then go back to start of EXEC.
MOVEC:     LBM                    ;Fetch length of string in line input buffer
           INB                    ;Increment that value to provide for (cc)
MOVEPG:    LAM                    ;Fetch character from line input buffer
           CAL ADV                ;Advance pointer for line input buffer
           CAL SWITCH             ;Switch memory pointer to point to user pgm buffer
           LMA                    ;Deposit character fm input buff into user pgm buff
           CAL ADV                ;Advance pointer for user program buffer
           CAL SWITCH             ;Switch memory pntr back to point to input buffer
           DCB                    ;Decrement character counter stored in CPU register B
           JFZ MOVEPG             ;If counter does not go to zero continue transfer ops
           RET                    ;When counter equals zero return to calling routine
CONTIN:    LLI 360                ;Reset memory pointer to program buffer
           LHI OLDPG26/400        ;** Line pointer storage location
           LDM                    ;Load high order byte into CPU register D
           INL                    ;Advance memory pointer
           LEM                    ;Load low order byte into CPU register E
           LHD                    ;Now set CPU register H to high part of address
           LLE                    ;And set CPU register L to low part of address
           LBM                    ;Fetch the character counter (cc) byte fm line in
           INB                    ;Program buffer and add one to compensate for (cc)
           CAL ADBDE              ;Add length of line value to old value to get new pointer
           LLI 360                ;Reset memory pointer to program buffer
           LHI OLDPG26/400        ;** Line pointer storage location
           LMD                    ;Restore new high portion
           INL                    ;Advance memory pointer
           LME                    ;And restore new low portion
           JMP GETAUX             ;Continue til find point at which to enter new line
GETCHP:    LHI OLDPG26/400        ;** Load H with pointer page (low portion set upon
           LBM                    ;Entry). Now fetch pointer into CPU register B.
           LLI 360                ;Reset pntr to pgm buffer line pointer storage location
           LDM                    ;Load high order byte into CPU register D
           INL                    ;Advance memory pointer
           LEM                    ;Load low order byte into CPU register E
           CAL ADBDE              ;Add pointer to pgm buffer pointer to obtain address of
           LHD                    ;Desired character. Place high part of new addr in H.
           LLE                    ;And low part of new address in E.
           LAM                    ;Fetch character from position in line in user pgm buffer
           CPI 240                ;See if it is the ASCII code for space
           RET                    ;Return to caller with flags set to indicate result
REMOVE:    CAL INDEXB             ;Add (cc) plus one to addr of start of line
           LCM                    ;Obtain byte from indexed location and
           CAL SUBHL              ;Subtract character count to obtain old location
           LMC                    ;Put new byte in old location
           LAC                    ;As well as in the Accumulator
           NDA                    ;Test to see if zero byte to indicate end of user pgm buff
           JTZ REMOV1             ;If it is end of user pgm buffer, go complete process
           CAL ADV                ;Otherwise add one to the present pointer value
           JMP REMOVE             ;And continue removing chamcters from the user pgm bf
REMOV1:    LLI 364                ;Load L with end of user pgm buffer pointer storage loc
           LHI OLDPG26/400        ;** Load H with page of that pointer storage location
           LDM                    ;Get page portion of end of pgm buffer address
           INL                    ;Advance memory pointer
           LAM                    ;And get low portion of end of pgm buffer address into
           SUB                    ;Accumulator then subtract displacement value in B
           LMA                    ;Restore new low portion of end of pgm buffer address
           RFC                    ;If subtract did not cause carry can return now
           DCL                    ;Otherwise decrement memory pointer back to page
           DCD                    ;Storage location, decrement page value to give new page
           LMD                    ;And store new page value back in buffer pntr storage loc
           RET                    ;Then return to calling routine
INSERT:    LLI 364                ;Load L with end of user pgm buffer pointer storage loc
           LHI OLDPG26/400        ;** Load H with page of that pointer storage location
           LAM                    ; Get page portion of end of program buffer address
           INL                    ;Advance memory pointer
           LLM                    ;Load low portion of end of program buffer address
           LHA                    ;Into L and finish setting up memory pointer
           CAL INDEXB             ;Add (cc) of line in input buffer to form new end of
           LAH                    ;Program buffer address. Fetch new end of buffer page
           CPI ENDPGRAM           ;tt Address and see if this value would exceed user's
           JFS BIGERR             ;System capabilit'y. Go display error message if so!
           CAL SUBHL              ;Else restore original value of end of buffer address
INSER1:    LCM                    ;Bring byte pointed to by H & L into CPU register C
           CAL INDEXB             ;Add displacement value to current memory pointer
           LMC                    ;Store the byte in the new location
           CAL SUBHL              ;Now subtract displacement value from H & L
           CAL CPHLDE             ;Compare this with the address stored in D & E
           JTZ INSER3             ;If same then go finish up Insert operation
           CAL DEC                ;Else set pointer to the byte before the byte just
           JMP INSER1             ;Processed and continue the Insert operation
INSER3:
INCLIN:    LLI 000                ;Load L with start of line input buffer
           LHI OLDPG26/400        ;** Load H with page of start of line input buffer
           LBM                    ;Fetch length of the line in line input buffer
           INB                    ;Increment value by one to include (cc) byte
           LLI 364                ;Set memory pointer to end of user pgrn buffer pointer
           LDM                    ;Storage location on same page and fetch page address
           INL                    ;Of this pointer into D. Then advance memory pointer
           LEM                    ;And get low part of this pointer into CPU register E.
           CAL ADBDE              ;Now add displacement (cc) of line in input buffer to
           LME                    ;The end of program buffer pointer. Replace the updated
           DCL                    ;Low portion of the new pointer value back in stomge
           LMD                    ;And restore the new page value back into storage
           RET                    ;Then return to calling routine
CPHLDE:    LAH                    ;Subroutine to compare if the contents of CPU registers
           CPD                    ;H & L are equal to registers D & E. First compare
           RFZ                    ;Register H to D. Return with flags set if not equal. If
           LAL                    ;Equal continue by comparing register L to E.
           CPE                    ;IF L equals E then H & L equal to D & E so return to
           RET                    ;Calling routines with flags set to equality status
ADBDE:     LAE                    ;Subroutine to add the contents of CPU register B (single
           ADB                    ;Byte value) to the double byte value in registers D & E.
           LEA                    ;First add B to E to form new least significant byte
           RFC                    ;Restore new value to E and exit if no carry resulted
           IND                    ;If had a carry then must increment most significant byte
           RET                    ;In register D before returning to calling routine
CTRLC:     LAI 336                ;Set up ASCII code for t (up arrow) in Accumulator.
           LCI 303                ;Set up ASCII code for letter 'C' in CPU register C.
           JMP ERROR              ;Go display the 'Control C' condition message.
FINERR:    LLI 340                ;Load L with starting address of line number storage area
           LHI OLDPG26/400        ;** Load H with page of line number storage area
           LAM                    ;Get (cc) for line number string. If length is zero meaning
           NDA                    ;There is no line number stored in the buffer then jump
           JTZ FINER1             ;Ahead to avoid displaying "AT LINE" message
           LLI 366                ;Else load L with address of start of "AT LINE" message
           LHI OLDPG1/400         ;** Stored on this page
           CAL TEXTC              ;Call subroutine to display the "AT LINE" message
           LLI 340                ;Now reset L to starting address of line number storage
           LHI OLDPG26/400        ;** Area and do same for CPU register H
           CAL TEXTC              ;Call subroutine to display the line number
FINER1:    CAL CRLF               ;Call subroutine to provide a carriage-return and line-feed
	       JMP PATCH3 
       
;;; The following is the old code, before patch 3
;;;        JMP EXEC               ;To the display device then return to EXECUTIVE.
DVERR:     LAI 304                ;Set up ASCII code for letter 'D' in Accumulator
           LCI 332                ;Set up ASCII code for letter 'Z' in CPU register C
           JMP ERROR              ;Go display the 'DZ' (divide by zero) error message
FIXERR:    LAI 306                ;Set up ASCII code for letter 'F' in Accumulator
           LCI 330                ;Set up ASCII code for letter 'X' in CPU register C
           JMP ERROR              ;Go display the 'FX' (FiX) error message
NUMERR:    LAI 311                ;Set up ASCII code for letter 'I' in Accumulator
           LCI 316                ;Set up ASCII code for letter 'N' in CPU register C
           LLI 220                ;Load L with address of pointer used by DINPUT
           LHI OLDPG1/400         ;** Routine. Do same for register H.
           LMI 000                ;Clear the location
           JMP ERROR              ;Go display the'IN'(Illegal Number) error message

;The following subroutine, used by various sections of SCELBAL, will search the 
;LINE INPUT BUFFER for a character string which is contained in a buffer starting
;at the address pointed to by CPU registers H & L when the subroutine is entered.
INSTR:     LDI OLDPG26/400        ;**Set D to starting page of LINE INPUT BUFFER
           LEI 000                ;Load E with starting location of LINE INPUT BUFFER
INSTR1:    CAL ADVDE              ;Advancer D & E pointer to the next location (input
           CAL SAVEHL             ;Buffer). Now save contents of d, E, H & L vefore the
           LBM                    ;Compare operations. Get length of TEST buffer in B.
           CAL ADV                ;Advance H & L buffer to first char in TEST buffer.
           CAL STRCPC             ;Compare contents of TEST buffer against input buffer
           JTZ RESTHL             ;For length B. If match, restore pntrs and exit to caller.
           CAL RESTHL             ;If no match, restore pointers for loop test.
           LLI 000                ;Load L with start of input buffer (to get the char cntr).
           LHI OLDPG26/400        ;**Load H with page of input buffer.
           LAM                    ;Get length of buffer (cc) into the accumulator.
           CPE                    ;Compare with current input buffer pointer value.
           JTZ INSTR2             ;If at end of buffer, jump ahead.
           CAL RESTHL             ;Else restore test string address (H&L) and input buffer
           JMP INSTR1             ;Address (D&E). Look gor occurrence of test string in ln.
           HLT                    ;Safety halt. If program reaches here have system failure.
INSTR2:    LEI 000                ;If reach end of input buffer without finding a match
           RET                    ;Load E with 000 as an indicator and return to caller.
ADVDE:     INE                    ;Subroutine to advance the pointer in the register
           RFZ                    ;Pair D & E. Advance contents of E. Return if not zero.
           IND                    ;If register E goes to 0 when advanced, then advance
           RET                    ;Register D too. Exit to calling routine.

RUN:       LLI 073                ;Load L with addr of GOSUB/RETURN stack pointer
           LHI OLDPG27/400        ;** Load H with page of same pointer
           LMI 000                ;Initialize the GOSUB/RETURN stack pointer to zero
           LLI 205                ;Load L with addr of FOR/NEXT stack pointer
           LMI 000                ;Initialize the FOR/NEXT stack pointer to zero
           LLI 360                ;Load L with addr of user pgm buffer line pointer
           LHI OLDPG26/400        ;** Load H with page of user pgm buffer line pointer
           LMI BGNPGRAM           ;tt Initialize pointer (may be altered by user)   *******
           INL                    ;Advance memory pointer to low portion of user pgm
           LMI 000                ;Buffer pointer and initialize to start of buffer
           JMP SAMLIN             ;Start executing user program with first line in buffer
NXTLIN:    LLI 360                ;Load L with addr of user program buffer line pointer
           LHI OLDPG26/400        ;** Load H with page of user pgm buffer line pointer
           LDM                    ;Place page addr of pgm buffer line pointer in D
           INL                    ;Advance the memory pointer
           LEM                    ;Place low addr of pgm buffer line pointer in E
           LHD                    ;Also put page addr of pgm buffer line pointer in H
           LLE                    ;And low addr of pgm buffer line pointer in L
           LBM                    ;Now fetch the (cc) of current line into register B
           INB                    ;Add one to account for (cc) byte itself
           CAL ADBDE              ;Add value in B to D&E to point to next line in
           LLI 360                ;User program buffer. Reset L to addr of user logrn
           LHI OLDPG26/400        ;** Buffer pointer storage location. Store the new
           LMD                    ;Updated user pgm line pointer in pointer storage
           INL                    ;Location. Store both the high portion
           LME                    ;And low portion. (Now points to next line to be
           LLI 340                ;Processed from user program buffer.) Change pointer
           LHI OLDPG26/400        ;** To address of line number buffer. Fetch the last
           LAM                    ;Line number (length) processed. Test to see if it was
           NDA                    ;Blank. If it was blank
           JTZ EXEC               ;Then stop processing and return to the Executive
           LAA                    ;Insert two effective NOPs here
           LAA                    ;In case of patching
SAMLIN:    LLI 360                ;Load L with addr of user program buffer line pointer
           LHI OLDPG26/400        ;** Load H with page of same pointer
           LCM                    ;Fetch the high portion of the pointer into register C
           INL                    ;Advance the memory pointer
           LLM                    ;Fetch the low portion of the pointer into register L
           LHC                    ;Now move the high portion into register H
           LDI OLDPG26/400        ;** Set D to page of line input buffer
           LEI 000                ;Set E to address of start of line input buffer
           CAL MOVEC              ;Move the line ftom the user program buffer into the
           LLI 000                ;Line input buffer. Now reset the pointer to the start
           LHI OLDPG26/400        ;** Of the line input buffer.
           LAM                    ;Fetch the first byte of the line input buffer (cc)
           NDA                    ;Test (cc) value to see if fetched a blank line
           JTZ EXEC               ;If fetched a blank line, return to the Executive
           CAL SYNTAX             ;Else call subrtn to strip off line nr & set statement toke

DIRECT:    LLI 203                ;Load L with address of syntax TOKEN storage location
           LHI OLDPG26/400        ;** Load H with page of syntax TOKEN location
           LAM                    ;Fetch the TOKEN value into the accumulator
           CPI 001                ;Is it token value for REM statement? If so, ignore the
           JTZ NXTLIN             ;Current line and go on to the next line in pgm buffer.
           CPI 002                ;Is it token value for IF statement?
           JTZ IF                 ;If yes, then go to the IF statement routine.
           CPI 003                ;Is it token value for LET statement? (Using keyword)
           JTZ LET                ;If yes, then go to the LET statement routine.
           CPI 004                ;Is it token value for GOTO statement?
           JTZ GOTO               ;If yes, then go to the GOTO statement routine.
           CPI 005                ;Is it token value for PRINT statement?
           JTZ PRINT              ;If yes, then go to the PRINT statement routine.
           CPI 006                ;Is it token value for INPUT statement?
           JTZ INPUT              ;If yes, then go to the INPUT statement routine.
           CPI 007                ;Is it token value for FOR statement?
           JTZ FOR                ;If yes, then go to the FOR statement routine.
           CPI 010                ;Is it token value for NEXT statement?
           JTZ NEXT               ;If yes, then go to the NEXT statement routine.
           CPI 011                ;Is it token value for GOSUB statement?
           JTZ GOSUB              ;If yes, then go to the GOSUB statement routine.
           CPI 012                ;Is it token value for RETURN statement?
           JTZ RETURN             ;If yes, then go to the RETURN statement routine.
           CPI 013                ;Is it token value for DIM statement?
           JTZ DIM                ;If yes, then go to the DIM statement routine.
           CPI 014                ;Is it token value for END statement?
           JTZ EXEC               ;If yes, then go back to the Executive, user pgm finished!
           CPI 015                ;Is it token value for IMPLIED LET statement?
           JTZ LET0               ;If yes, then go to special LET entry point.
           CPI 016                ;@@ Is it token value for ARRAY IMPLIED LET?
           JFZ SYNERR             ;If not, then assume a syntax error condition.
           CAL ARRAY1             ;@@ Else, perform array storage set up subroutine.
           LLI 206                ;@@ Set L to array pointer storage location.
           LHI OLDPG26/400        ;@@ * * Set H to array pointer storage location.
           LBM                    ;@@ Fetch array pointer to register B.
           LLI 202                ;@@ Change memory pointer to syntax pntr storage loc.
           LMB                    ;@@ Save array pointer value there.
           CAL SAVESY             ;@@ Save array name in auxiliary symbol buffer
           JMP LET1
PRINT:     LLI 202                ;Load L with address of SCAN pointer storage location
           LHI OLDPG26/400        ;** Load H with page of SCAN pointer
           LAM                    ;Fetch the pointer value (last character scanned by the
           LLI 000                ;SYNTAX routine). Change pointer to line buffer (cc).
           CPM                    ;Compare pointer value to buffer length. If not equal
           JTS PRINT1             ;Then line contains more than stand alone PRINT state-
           CAL CRLF               ;Ment. However, if just have PRINT statement then issue
           JMP NXTLIN             ;A carriage-return & line-feed combination, then exit.
PRINT1:    CAL CLESYM             ;Initialize the SYMBOL buffer for new entry.
           LLI 202                ;Load L with address of SCAN buffer pointer
           LHI OLDPG26/400        ;** Load H with page of SCAN pointer
           LBM                    ;Pointer points to last char scanned by SYNTAX. Need
           INB                    ;To increment it to point to next char in statement line.
           LLI 203                ;Load L with address of former TOKEN value. Use it as
           LMB                    ;Storage location for a PRINT statement pointer.
PRINT2:    LLI 203                ;Set memory pointer to PRINT pointer storage location
           CAL GETCHR             ;Fetch character in input buffer pointed to by PRINT
           CPI 247                ;Pointer. See if it is ASCII code for single quote mark.
           JTZ QUOTE              ;If so, go to QUOTE section to process text string.
           CPI 242                ;If not, see if it is ASCII code for double quote mark.
           JTZ QUOTE              ;If so, go to QUOTE section to process text string.
           CPI 254                ;If not, see if it is ASCII code for comma sign.
           JTZ PRINT3             ;If so, go evaluate expression.
           CPI 273                ;If not, see if it is ASCII code for semi-colon sign.
           JTZ PRINT3             ;If so, go evaluate expression.
           LLI 203                ;Load L with address of PRINT pointer storage location.
           CAL LOOP               ;Increment pointer and test for end of line.
           JFZ PRINT2             ;If not end of line, fetch the next character.
PRINT3:    LLI 202                ;Load L with address of SCAN pointer storage location
           LBM                    ;Fetch value of the pointer (last letter of KEYWORD)
           INB                    ;Add one to point to first character of expression
           LLI 276                ;Load L with addr of EVAL pointer storage location
           LMB                    ;Store addr at which EVAL should start scanning
           LLI 203                ;Load L with address of PRINT pointer
           LBM                    ;Which points to field terminator
           DCB                    ;Decrement pointer value to last character of expression
           LLI 277                ;Load L with address of EVAL FINISH pntr storage loc.
           LMB                    ;Place address value of last char in PRINT field there
           LLI 367                ;Load L with address of QUOTE flag
           LAM                    ;Fetch the value of the QUOTE flag into the ACC
           NDA                    ;Test the QUOTE flag status
           JTZ PRINT4             ;If field not quoted, proceed to evaluate expression
           LMI 000                ;If field quoted, then clear the QUOTE flag for next field
           JMP PRINT6             ;And skip the evaluation procedure
PRINT4:    CAL EVAL               ;Evaluate the current PRINT field
           LLI 177                ;Then load L,with address of the TAB flag
           LHI OLDPG26/400        ;** Load H with the page of the TAB flag
           LAM                    ;Fetch the value of the TAB flag into the accumulator
           NDA                    ;Test the TAB flag
           LLI 110                ;Change L to the FIXED/FLOAT flag location
           LHI OLDPG1/400         ;** Change H to the FIXED/FLOAT flag page
           LMI 377                ;Set FIXED/FLOAT flag to fixed point
PRINT5:    CTZ PFPOUT             ;If TAB flag not set, display value of expression
           LLI 177                ;Load L with address of TAB flag
           LHI OLDPG26/400        ;** Load H with page of TAB flag
           LMI 000                ;Reset TAB flag for next PRINT field
PRINT6:    LLI 203                ;Load L with address of PRINT pointer stomge location
           CAL GETCHR             ;Fetch the character pointed to by the PRINT pointer
           CPI 254                ;See if the last character scanned was a comma sign
           CTZ PCOMMA             ;If so, then display spaces to next TA.B location
           LLI 203                ;Reset L to address of PRINT pointer storage location
           LHI OLDPG26/400        ;** Reset H to page of PRINT pointer stomge location
           LBM                    ;Fetch the value of the pointer into register B
           LLI 202                ;Change L to SCAN pointer storage location
           LMB                    ;Place end of last field processed into SCAN pointer
           LLI 000                ;Change pointer to start of line input buffer
           LAB                    ;Place pntr to last char scanned into the accumulator
           CPM                    ;Compare this value to the (cc) for the line buffer
           JTS PRINT1             ;If not end of line, continue to process next field
           LLI 000                ;If end of line, fetch the last character in the line
           CAL GETCHR             ;And check to see if it
           CPI 254                ;Was a comma. If it was, go on to the next line in the
           JTZ NXTLIN             ;User program buffer without displaying a CR & LF.
           CPI 273                ;If not a comma, check to see if it was a semi-colon.
           JTZ NXTLIN             ;If so, do not provide a CR & LF combination.
           CAL CRLF               ;If not comma or semi-colon, provide CR & LF at end
           JMP NXTLIN             ;Of a PRINT statement. Go process next line of pgrm.
QUOTE:     LLI 367                ;Load L with address of QUOTE flag
           LMA                    ;Store type of quote in flag storage location
           CAL CLESYM             ;Initialize the SYMBOL buffer for new entry
           LLI 203                ;Load L with address of PRINT pointer
           LBM                    ;Fetch the PRINT pointer into register B
           INB                    ;Add one to advance over quote character
           LLI 204                ;Load L with address of QUOTE pointer
           LMB                    ;Store the beginning of the QUOTE field pointer
QUOTE1:    LLI 204                ;Load L with address of QUOTE pointer
           CAL GETCHR             ;Fetch the next character in the TEXT field
           LLI 367                ;Load L with the QUOTE flag (type of quote)
           CPM                    ;Compare to see if latest character this quote mark
           JTZ QUOTE2             ;If so, finish up this quote field
           CAL ECHO               ;If not, display the character as part of TEXT
           LLI 204                ;Reset L to QUOTE pointer storage location
           CAL LOOP               ;Increment QUOTE pointer and test for end of line
           JFZ QUOTE1             ;If not end of line, continue processing TEXT field
QUOTER:    LAI 311                ;If end of line before closing quote mark have an error
           LCI 321                ;So load ACC with I and register C with Q
           LLI 367                ;Load L with the address of the QUOTE flag
           LHI OLDPG26/400        ;** Load H with the page of the QUOTE flag
           LMI 000                ;Clear the QUOTE flag for future use
           JMP ERROR              ;Go display the IQ (Illegal Quote) error message
QUOTE2:    LLI 204                ;Load L with address of QUOTE pointer
           LBM                    ;Fetch the QUOTE pointer into register B
           LLI 202                ;Load L with address of SCAN pointer storage location
           LMB                    ;Store former QUOTE vointer as start of next field
           LAB                    ;Place QUOTE pointer into the accumulator
           LLI 000                ;Change L to point to start of the input line buffer
           CPM                    ;Compare QUOTE pointer value with (cc) value
           JFZ PRINT1             ;If not end of line, process next PRINT field
           CAL CRLF               ;Else display a CR & LF combination at the end of line
           LLI 367                ;Load L with the address of the TAB flag
           LHI OLDPG26/400        ;** Load H with the page of the TAB flag
           LMI 000                ;Clear the TAB flag for future use
           JMP NXTLIN             ;Go process next line of the program.

;The following subroutines are utilized by the PRINT routine.
PFPOUT:    LLI 126                ;Load L with the address of the FPACC MSW (Floating
           LHI OLDPG1/400         ;** Point ACC). Load H with page of the FPACC MSW.
           LAM                    ;Fetch the FPACC MSW into the accumulator. Test to
           NDA                    ;See if the FPACC MSW is zero. If so, then simply go and
           JTZ ZERO               ;Display the value "0"
           INL                    ;Else advance the pointer to the FPACC Exponent
           LAM                    ;Fetch the FPACC Exponent into the accumulator
           NDA                    ;See if any exponent value. If not, mantissa is in range
           JTZ FRAC               ;0.5 to 1.0. Treat number as a fraction.
           JMP FPOUT              ;Else perform regular numerical output routine.
ZERO:      LAI 240                ;Load ASCII code for space into the ACC
           CAL ECHO               ;Display the space
           LAI 260                ;Load ASCII code for 0 into the ACC
           JMP ECHO               ;Display 0 and exit to calling routine
FRAC:      LLI 110                ;Load L with address of FIXED/FLOAT flag
           LMI 000                ;Reset it to indicate floating point mode
           JMP FPOUT              ;Display floating point number and return to caller
PCOMMA:    LLI 000                ;Load L with address of (cc) in line input buffer
           LAM                    ;Fetch the (cc) for the line into the ACC
           LLI 203                ;Change pointer to PRINT pointer storage location
           SUM                    ;Subtract value of PRINT pointer from line (cc)
           RTS                    ;If at end of buffer, do not TAB
           LLI 043                ;If not end, load L with address of COLUMN COUNTER
           LHI OLDPG1/400         ;** Set H to page of COLUMN COUNTER
           LAM                    ;Fetch COLUMN COUNTER into the accumulator
           NDI 360                ;Find the last TAB position (multiple of 16 decimal)
           ADI 020                ;Add 16 (decimal) to get new TAB position
           SUM                    ;Subtract current position from next TAB position
           LCA                    ;Store this value in register C as a counter
           LAI 240                ;Load the ACC with the ASCII code for space
PCOM1:     CAL ECHO               ;Display the space
           DCC                    ;Decrement the loop counter
           JFZ PCOM1              ;Continue displaying spaces until loop counter is zero
           RET                    ;Then return to calling routine
LET0:      CAL SAVESY             ;Entry point for IMPLIED LET statement. Save the
           LLI 202                ;Variable (to left of the equal sign). Set L to the SCAN
           LHI OLDPG26/400        ;** Pointer. Set H to the page of the SCAN pointer.
           LBM                    ;Fetch value of SCAN pointer. (Points to = sign in In bf)
           LLI 203                ;Change pointer to LET pointer (was TOKEN value)
           LMB                    ;Place the SCAN pointer value into the LET pointer
           JMP LET5               ;Continue processing the LET statement line
LET:       CAL CLESYM             ;Initialize the SYMBOL BUFFER for new entry
           LLI 144                ;Load L with address of start of AUX SYMBOL BUFF
           LHI OLDPG26/400        ;** Load H with page of AUX SYMBOL BUFFER
           LMI 000                ;Initialize AUX SYMBOL BUFFER
LET1:      LLI 202                ;Entry point for ARRAY IMPLIED LET statement.
           LHI OLDPG26/400        ;** Set pointer to SCAN pointer storage location
           LBM                    ;Fetch the SCAN pointer value (last letter scanned by
           INB                    ;SYNTAX subroutine) and add one to next character
           LLI 203                ;Change L to LET pointer storage location
           LMB                    ;Store former SCAN value (updated) in LET pointer
LET2:      LLI 203                ;Set L to gtorage location of LET pointer
           CAL GETCHR             ;Fetch the character pointed to by the LET pointer
           JTZ LET4               ;If character is a space, ignore it
           CPI 275                ;See if character is the equal (=) sign
           JTZ LET5               ;If so, go process other side of the statement (after
           CPI 250                ;@@ If not, see if character is a right parenthesis
           JFZ LET3               ;If not, continue looking for equal sign
           CAL ARRAY              ;@@ If so, have subscript. Call array set up subroutine.
           LLI 206                ;@@ Load L with address of ARRAY pointer
           LHI OLDPG26/400        ;@@ ** Load H with page of ARRAY pointer
           LBM                    ;@@ Fetch value (points to ")" character of subscript)
           LLI 203                ;@@ Load L with address of LET pointer
           LMB                    ;@@ Place ARRAY pointer value as new LET pointer
           JMP LET4               ;@@ Continue to look for = sign in statement line
LET3:      LLI 144                ;Reset L to start of AUX SYMBOL BUFFER
           LHI OLDPG26/400        ;** Load H with page of AUX SYMBOL BUFFER
           CAL CONCT1             ;Concatenate character to the AUX SYMBOL BUFFER
LET4:      LLI 203                ;Load L with address of LET pointer storage location
           CAL LOOP               ;Add one to pointer and test for end of line input buffer
           JFZ LET2               ;If not end of line, continue looking for the equal sign
LETERR:    LAI 314                ;If do not find an equal sign in the LET statement line
           LCI 305                ;Then have a LE (Let Error). Load the code for L and E
           JMP ERROR              ;Into registers ACC and C and go display the error msg.
LET5:      LLI 203                ;When find the equal sign, reset L to point to the LET
           LHI OLDPG26/400        ;** Pointer and H to the proper page. Fetch the pointer
           LBM                    ;Value into register B and add one to advance pointer
           INB                    ;Over the equal sign to first char in the expression.
           LLI 276                ;Set L to point to the address of the EVAL pointer
           LMB                    ;Set EVAL pointer to start evaluating right after the
           LLI 000                ;Equal sign. Now change L to start of line input buffer.
           LBM                    ;Fetch the (cc) value into register B. (Length of line.)
           LLI 277                ;Load L with EVAL FINISH pointer storage location.
           LMB                    ;Set it to stop evaluating at end of the line.
           CAL EVAL               ;Call the subroutine to evaluate the expression.
           CAL RESTSY             ;Restore the name of the variable to receive new value.
           CAL STOSYM             ;Store the new value for the variable in variables table.
           JMP NXTLIN             ;Go process next line of the program.
GOTO:      LLI 350                ;Load L with start of AUX LINE NR BUFFER
           LHI OLDPG26/400        ;** Load H with page of AUX LINE NR BUFFER
           LMI 000                ;Initialize the AUX LINE NR BUFFER to zero
           LLI 202                ;Load L with address of SCAN pointer storage location
           LBM                    ;Fetch pointer value (last char scanned by SYNTAX)
           INB                    ;Add one to skip over the last 0 in GOTO keyword
           LLI 203                ;Change pointer to GOTO pointer (formerly TOKEN)
           LMB                    ;Store the updated SCAN pointer as the GOTO pointer
GOTO1:     LLI 203                ;Load L with address of GOTO pointer
           CAL GETCHR             ;Fetch the character pointed to by the GOTO pointer
           JTZ GOTO2              ;If character was a space, ignore it
           CPI 260                ;See if character is in the range of a decimal digit
           JTS GOTO3              ;If not, must have end of the line number digit string
           CPI 272                ;Continue to test for decitnal digit
           JFS GOTO3              ;If not, mugt have end of the line number digit string
           LLI 350                ;If valid decimal digit, load L with addr of AUX LINE
           CAL CONCT1             ;NR BUFFER and concatenate digit to the buffer.
GOTO2:     LLI 203                ;Reset pointer to GOTO pointer storage location
           CAL LOOP               ;Advance the pointer value and test for end of line
           JFZ GOTO1              ;If not end of line, fetch next digit in GOTO line number
GOTO3:	   LLI 360                ;Set L to user program buffer pointer storage location
           LHI OLDPG26/400        ;** Set H to page of program buffer pointer
           LMI BGNPGRAM           ;Initialize high part of pointer to start of pgm buffer
           INL                    ;Advance the memory point
           LMI 000                ;Initialize the low part of pointer to start of pgm buffer
GOTO4:     CAL CLESYM             ;Clear the SYMBOL BUFFER
           LLI 204                ;Load L with address of GOTO SEARCH pointer
           LMI 001                ;Initialize to one for first char of line
GOTO5:     LLI 204                ;Load L with address of GOTO SEARCH pointer
           CAL GETCHP             ;Fetch character pointed to by GOTO SEARCH pointer
           JTZ GOTO6              ;From line pointed to in user program buffer. Ignore
           CPI 260                ;Spaces. Check to see if character is a decirnal digit.
           JTS GOTO7              ;If not, then have processed line number at the start of
           CPI 272                ;The current line. Continue the check for a valid decimal
           JFS GOTO7              ;Digit. If have a decirnal digit then concatenate the digit
           CAL CONCTS             ;Onto the current string in the SYMBOL BUFFER,
GOTO6:     LLI 204                ;Change L to the address of the GOTO SEARCH pointer
           LHI OLDPG26/400        ;** And H to the proper page of the pointer
           LBM                    ;Fetch the GOTO SEARCH pointer value
           INB                    ;Increment the GOTO SEARCH pointer
           LMB                    ;And restore it back to memory
           LLI 360                ;Change L to address of user program buffer pointer
           LCM                    ;Save the high part of this pointer value in register C
           INL                    ;Advance L to the low part of the pgrn buffer pointer
           LLM                    ;Now load it into L
           LHC                    ;And transfer C into H to point to start of the line
           LAM                    ;Fetch the (cc) of the current line being pointed to in the
           DCB                    ;User pgm buff. Decrernent B to previous value. Compare
           CPB                    ;GOTO SEARCH pointer value to length of current line.
           JFZ GOTO5              ;If not end of line then continue getting current line nr.
GOTO7:     LLI 120                ;Load L with address of start of the SYMBOL BUFFER
           LHI OLDPG26/400        ;Set H to the page of the SYMBOL BUFFER
           LDI OLDPG26/400        ;Set D to the page of the AUX LINE NR BUFFER
           LEI 350                ;Set E to the start of the AUX LINE NR BUFFER
           CAL STRCP              ;Compare GOTO line number against current line nr.
           JTZ SAMLIN             ;If they match, found GOTO line. Pick up ops there!
           LLI 360                ;Else, set L to user program buffer pntr storage location
           LHI OLDPG26/400        ;** Set H to page of user program buffer pointer
           LDM                    ;Fetch the high part of this pointer into register D
           INL                    ;Advance the memory pointer
           LEM                    ;Fetch the low part into register E
           LHD                    ;Transfer the pointer to H
           LLE                    ;And L. Fetch the (cc) of the current line into register
           LBM                    ;B and then add one to account for the (cc) byte to get
           INB                    ;Total length of the current line in the user pgm buffer
           CAL ADBDE              ;Add the total length to the pointer value in D & E
           LLI 360                ;To get the starting address of the next line in the user
           LHI OLDPG26/400        ;** User program buffer. Place the new value for the user
           LMD                    ;Program buffer pointer back into the user program
           INL                    ;Buffer pointer storage locations so that it points to the
           LME                    ;Next line to be processed in the user program buffer.
           LLI 364                ;Load L with address of end of user pgm buffer storage
           LAD                    ;Location (page address) and fetch end of buffer page.
           CPM                    ;Compare this with next line pointer (updated).
           JFZ GOTO4              ;If not end of buffer, keep looking for the specified line
           INL                    ;If have same page addresses, check the low address
           LAE                    ;Portions to see if
           CPM                    ;Have reached end of user program buffer
           JFZ GOTO4              ;If not, continue looking. If end of buffer without
GOTOER:    LAI 325                ;Finding specified line, then have an error condition.
           LCI 316                ;Load ACC and register C with code for "UN" and go
           JMP ERROR              ;Display "Undefined Line" error message.
IF:        LLI 202                ;Set L to SCAN pointer storage location.
           LHI OLDPG26/400        ;** Load H to page of SCAN pointer storage location.
           LBM                    ;Fetch the SCAN pointer value to register B.
           INB                    ;Add one to advance pointer over last char scanned.
           LLI 276                ;Change L to address of EVAL pointer. Set up EVAL
           LMB                    ;Pointer to begin evaluation with next char in the line.
           CAL CLESYM             ;Clear the SYMBOL BUFFER.
           LLI 320                ;Set L to starting address of THEN in look-up table.
           LHI OLDPG1/400         ;** Set H to page of the look-up table.
           CAL INSTR              ;Search for occurrence of THEN in the line input buffer.
           LAE                    ;Transfer register E to ACC. If THEN not found
           NDA                    ;The value in E will be zero.
           JFZ IF1                ;If THEN found, can evaluate the IF expression.
           LLI 013                ;If THEN not found, set L to Auting address of GOTO
           LHI OLDPG27/400        ;** In the KEYWORD look-up table. Set H to table
           CAL INSTR              ;Search for occurrence of GOTO in the line input buffer.
           LAE                    ;Transfer E to ACC. If GOTO not found
           NDA                    ;The value in E will be zero.
           JFZ IF1                ;If GOTO found, can evaluate the IF expression.
IFERR:     LAI 311                ;Set ASCII code for letter I in ACC
           LCI 306                ;And code for letter F in register C
           JMP ERROR              ;Go display the IF error message
IF1:       LLI 277                ;Load L with addr of EVAL FINISH pointer storage loc
           LHI OLDPG26/400        ;** Load H with page of storage location
           DCE                    ;Subtract one from pointer in E and set the EVAL
           LME                    ;FINISH pointer so that it will evaluate up to the THEN
           CAL EVAL               ;Or GOTO directive. Evaluate the expression.
           LLI 126                ;Load L with address of FPACC Most Significant Word
           LHI OLDPG1/400         ;** Load H with page of FPACC MSW
           LAM                    ;Fetch the FPACC MSW into the accumulator
           NDA                    ;Test the value of the FPACC MSW
           JTZ NXTLIN             ;If it is zero, IF condition failed, ignore rest of line.
           LLI 277                ;If not, load L with addr of EVAL FINISH pointer
           LHI OLDPG26/400        ;** Set H to the appmpriate page
           LAM                    ;Fetch the value in the EVAL FINISH pointer
           ADI 005                ;Add five to skip over THEN or GOTO directive
           LLI 202                ;Change L to SCAN pointer stomge location
           LMA                    ;Set up the SCAN pointer to location after THEN or
           LBA                    ;GOTO directive. Also put this value in register B.
           INB                    ;Add one to the value in B to point to next character
           LLI 204                ;After THEN or GOTO. Change L to addr of THEN pntr
           LMB                    ;Storage location and store the pointer value.
IF2:       LLI 204                ;Load L with the address of the THEN pointer
           CAL GETCHR             ;Fetch the character pointed to by the THEN pointer
           JFZ IF3                ;If character is not a space, exit this loop
           LLI 204                ;If fetch a space, ignore. Reset L to the THEN pointer
           CAL LOOP               ;Add one to the THEN pointer and test for end of line
           JFZ IF2                ;If not end of line, keep looking for a character other
           JMP IFERR              ;Than a space. If reach end of line first, then error
IF3:       CPI 260                ;When find a character see if it is numeric.
           JTS IF4                ;If not numeric, then should have a new type of
           CPI 272                ;Statement. If numeric, then should have a line number.
           JTS GOTO               ;So process as though have a GOTO statement!
IF4:       LLI 000                ;Load L with addr of start of line input buffer.
           LAM                    ;Fetch the (cc) byte to get length of line value.
           LLI 204                ;Change L to current value of THEN pointer (where first
           SUM                    ;Non-space char. found after THEN or GOTO). Subtract
           LBA                    ;This value from length of line to get remainder. Now
           INB                    ;Have length of second statement portion. Add one for
           LCM                    ;(cc) count. Save THEN pointer value in register C.
           LLI 000                ;Reset L to start of line input buffer. Now put length of
           LMB                    ;Second statement into (cc) position of input buffer.
           LLC                    ;Set L to where second statement starts.
           LDI OLDPG26/400        ;** Set D to page of line input buffer.
           LEI 001                ;Set E to first character position of line input buffer.
           CAL MOVEIT             ;Move the second statement up in line to become first!
           LLI 202                ;Load L with address of new SCAN pointer. Load
           LMI 001                ;It with starting position for SYNTAX scan.
           CAL SYNTX4             ;Use special entry to SYNTAX to get new TOKEN value.
           JMP DIRECT             ;Process the second statement in the original line.
GOSUB:     LLI 340                ;Load L with start of LINE NUMBER BUFFER
           LHI OLDPG26/400        ;Fetch (cc) of cuffent line number into register D
           LDM                    ;Fetch high value (page) of pgm line pointer to D
           IND                    ;Test contents of register by first incrementing
           DCD                    ;And then decrementing the value in the register
           JTZ GOSUB1             ;If no line number, then processing a DIRECT statement
           LLI 360                ;Else, load L with address of user pgm buff line pointer
           LDM                    ;Fetch high value (page) of pgm line pointer to D
           INL                    ;Advance the memory pointer
           LEM                    ;Fetch the low part of pgm line pointer to E
GOSUB1:    LLI 073                ;Set L to address of GOSUB STACK POINTER
           LHI OLDPG27/400        ;** Set H to page of GOSUB STACK POINTER
           LAM                    ;Fetch value in GOSUB stack pointer to ACC
           ADI 002                ;Add two to current stack pointer for new data to be
           CPI 021                ;Placed on the stack and see if stack overflows
           JFS GOSERR             ;If stack filled, have an error condition
           LMA                    ;Else, store updated stack pointer
           LLI 076                ;Load L with address of start of stack less offset (2)
           ADL                    ;Add GOSUB stack pointer to base address
           LLA                    ;To get pointer to top of stack (page byte)
           LMD                    ;Store page part of pgm buffer line pointer in stack
           INL                    ;Advance pointer to next byte in stack
           LME                    ;Store low part of pgm buffer line pointer in stack
           JMP GOTO               ;Proceed from here as though processing a GOTO
RETURN:    LLI 073                ;Set L to address of GOSUB STACK POINTER
           LHI OLDPG27/400        ;** Set H to page of GOSUB STACK POINTER
           LAM                    ;Fetch the value of GOSUB stack pointer to ACC
           SUI 002                ;Subtract two for data to be removed from stack
           JTS RETERR             ;If stack underflow, then have an error condition
           LMA                    ;Restore new stack pointer to memory
           ADI 002                ;Add two to point to previous top of stack
           LLI 076                ;Load L with address of start of GOSUB stack less two
           ADL                    ;Add address of previous top of stack to base value
           LLA                    ;Set pointer to high address value in the stack
           LDM                    ;Fetch the high address value from stack to register D
           IND                    ;Exercise the register contents to see if high address
           DCD                    ;Obtained is zero. If so, original GOSUB statement was
           JTZ EXEC               ;A DIRECT statement. Must return to Executive!
           INL                    ;Else, advance pointer to get low address value from the
           LEM                    ;Stack into CPU register E.
           LLI 360                ;Load L with address of user pgm line pointer storage
           LHI OLDPG26/400        ;** Location. Load H with page of user pgm line pntr.
           LMD                    ;Put high address from stack into pgm line pointer.
           INL                    ;Advance the memory pointer
           LME                    ;Put low address from stack into pgrn line pointer.
           JMP NXTLIN             ;Execute the next line after originating GOSUB line!
GOSERR:    LAI 307                ;Load ASCII code for letter G into accumulator
           LCI 323                ;Load ASCII code for letter S into register C
           JMP ERROR              ;Go display GoSub (GS) error message.
RETERR:    LAI 322                ;Load ASCII code for letter R into accumulator
           LCI 324                ;Load ASCII code for letter T into register C
           JMP ERROR              ;Go display ReTurn (RT) error message.
INPUT:     CAL CLESYM             ;Clear the SYMBOL BUFFER
           LLI 202                ;Load L with address of SCAN pointer storage location
           LBM                    ;Fetch value of SCAN pointer to register B
           INB                    ;Increment value to point to next chamcter
           LLI 203                ;Change L to point to INPUT pointer (formerly TOKEN)
           LMB                    ;Updated SCAN pointer becomes INPUT pointer
INPUT1:    LLI 203                ;Load L with address of INPUT pointer
           CAL GETCHR             ;Fetch a character from the line input buffer
           JTZ INPUT3             ;If character is a space, ignore it. Else,
           CPI 254                ;See if character is a comma. If so, process the
           JTZ INPUT4             ;Variable that preceeds the comma.
           CPI 250                ;If not, see if character is a left parenthesis.
           JFZ INPUT2             ;If not, continue processing to build up symbolic variable
           CAL ARRAY2             ;@@ If so, call array subscripting subroutine
           LLI 206                ;@@ Load L with address of array set up pointer
           LHI OLDPG26/400        ;@@ ** Load H with page of array set up pointer
           LBM                    ;@@ Fetch pointer value (point to ")" of subscript)
           LLI 203                ;@@ Change pointer to address of INPUT pointer
           LMB                    ;@@ Update INPUT pointer
           JMP INPUT3             ;@@ Jump over concatenate instruction below
INPUT2:    CAL CONCTS             ;Concatenate character to SYMBOL BUFFER
INPUT3:    LLI 203                ;Load L with address of INPUT pointer
           CAL LOOP               ;Increment INPUT pointer and test for end of line
           JFZ INPUT1             ;If not end of line, go get next character
           CAL INPUTX             ;If end of buffer, get input for variable in the SYMBOL
           CAL STOSYM             ;BUFFER and store the value in the VARIABLES table
           JMP NXTLIN             ;Then continue to interpret next statement line
INPUT4:    CAL INPUTX             ;Get input from user for variable in SYMBOL BUFFER
           CAL STOSYM             ;Store the inputted value in the VARIABLES table
           LHI OLDPG26/400        ;** Set H to page of INPUT pointer
           LLI 203                ;Set L to location of INPUT pointer
           LBM                    ;Fetch pointer value for last character examined
           LLI 202                ;Change L to point to SCAN pointer storage location
           LMB                    ;Update the SCAN pointer
           JMP INPUT              ;Continue processing statement line for next variable
INPUTX:    LLI 120                ;Load L with start of SYMBOL BUFFER (contains cc)
           LAM                    ;Fetch the (cc) (length of symbol in the buffer) to ACC
           ADL                    ;Add (cc) to base address to set up
           LLA                    ;Pointer to last character in the SYMBOL BUFFER
           LAM                    ;Fetch the last character in the SYMBOL BUFFER
           CPI 244                ;See if the last chamcter was a $ sign
           JFZ INPUTN             ;If not a $ sign, get variable value as a numerical entry
           LLI 120                ;If $ sign, reset L to start of the SYMBOL BUFFER
           LBM                    ;Fetch the (cc) for the variable in the SYMBOL BUFF
           DCB                    ;Subtract one from (cc) to chop off the $ sign
           LMB                    ;Restore the new (cc) for the SYMBOL BUFFER
           CAL FP0                ;Call subroutine to zero the floating point accumulator
           CAL CINPUT             ;Input one character from system input device
           LLI 124                ;Load L with address of the LSW of the FPACC
           LMA                    ;Place the ASCII code for the character inputted there
           JMP FPFLT              ;Convert value to floating point format in FPACC
INPUTN:    LLI 144                ;Load L with address of start of AUX SYMBOL BUFF
           LHI OLDPG26/400        ;** Load H with page of AUX SYMBOL BUFFER
           LAI 277                ;Load accumulator with ASCII code for ? mark
           CAL ECHO               ;Call output subroutine to display the ? mark
           CAL STRIN              ;Input string of characters (number) fm input device
           JMP DINPUT             ;Convert decimal string into binary floating point nr.
FP0:       LHI OLDPG1/400         ;** Load H with floating point working registers page
           JMP CFALSE             ;Zero the floating point accumulator & exit to caller
FOR:       LLI 144                ;Load L with address of AUX SYMBOL BUFFER
           LHI OLDPG26/400        ;** Load H with page of AUX SYMBOL BUFFER
           LMI 000                ;Initialize buffer by clearing first byte
           LLI 146                ;Load L with location of second character in buffer
           LMI 000                ;Clear that location in case of single character variable
           LLI 205                ;Load L with address of FOR/NEXT STACK pointer
           LHI OLDPG27/400        ;** Load H with page of FOR/NEXT STACK pointer
           LBM                    ;Fetch the FOR/NEXT STACK pointer
           INB                    ;Increment it in preparation for pushing operation
           LMB                    ;Restore it back to its storage location
           LLI 360                ;Load L with address of user pgrn buffer line pointer
           LHI OLDPG26/400        ;** Set H to page of line pointer
           LDM                    ;Fetch page address of pgm buffer line pntr into D
           INL                    ;Advance the memory pointer to pick up low part
           LEM                    ;Fetch low address of pgm buffer line pntr into E
           LAB                    ;Restore updated FOR/NEXT STACK pointer to ACC
           RLC                    ;Rotate it left to multiply by two, then rotate it again to
           RLC                    ;Multiply by four. Add this value to the base address of
           ADI 134                ;The FOR/NEXT STACK to point to the new top of
           LLA                    ;The FOR/NEXT STACK and set up to point to stack
           LHI OLDPG27/400        ;** Set H for page of the FOR/NEXT STACK
           LMD                    ;Store the page portion of the user pgrn buffer line pntr
           INL                    ;In the FORINEXT STACK, advance register 4 then
           LME                    ;Store the low portion of the pgrn line pntr on the stack
           LLI 325                ;Change L to point to start of TO string which is stored
           LHI OLDPG1/400         ;** In a text strings storage area on this page
           CAL INSTR              ;Search the statement line for the occurrence of TO
           LAE                    ;Register E wiU be zero if TO not found. Move E to ACC
           NDA                    ;To make a test
           JFZ FOR1               ;If TO found then proceed with FOR statement
FORERR:    LAI 306                ;Else have a For Error. Load ACC with ASCII code for
           LCI 305                ;Letter F and register C with code for letter E.
           JMP ERROR              ;Then go display the FE message.
FOR1:      LLI 202                ;Load L with address of SCAN pointer storage location
           LHI OLDPG26/400        ;** Set H to page of the SCAN pointer
           LBM                    ;Fetch pointer value to ACC (points to letter R in the
           INB                    ;For directive). Increment it to point to next character
           LLI 204                ;In the line. Change register L and set this value up
           LMB                    ;As an updated FOR pointer.
           LLI 203                ;Set L to address of TO pointer (formerly TOKEN)
           LME                    ;Save pointer to TO in the TO pointer!
FOR2:      LLI 204                ;Load L with address of the FOR pointer
           CAL GETCHR             ;Fetch a character from the statement line
           JTZ FOR3               ;If it is a space, ignore it
           CPI 275                ;Test to see if character is the "=" sign
           JTZ FOR4               ;If so, variable name is in the AUX SYMBOLBUFFER
           LLI 144                ;If not, then set L to point to start of the AUX SYMBOL
           CAL CONCT1             ;BUFFER and concatenate the character onto the buffer
FOR3:      LLI 204                ;Reset L to address of the FOR pointer
           CAL LOOP               ;Increment the pointer and see if end of line
           JFZ FOR2               ;If not end of line, continue looking for the "=" sign
           JMP FORERR             ;If reach end of line before "=" sign, then have error
FOR4:      LLI 204                ;Set L with address of the FOR pointer
           LBM                    ;Fetch pointer value to ACC (pointing to sign)
           INB                    ;Increment it to skip over the "=" sign
           LLI 276                ;Set L to address of the EVAL pointer
           LMB                    ;Restore the updated pointer to storage
           LLI 203                ;Set L to the address of the TO pointer
           LBM                    ;Fetch pointer value to ACC (pointing to letter T in TO)
           DCB                    ;Decrement it to point to character before the T in TO
           LLI 277                ;Set L to EVAL FINISH pointer storage location
           LMB                    ;Store the EVAL FINISH pointer value
           CAL EVAL               ;Evaluate the expression between the "=" sign and TO
           CAL RESTSY             ;Directive. Place the variable name in the variables table.
           LLI 144                ;Load L with starting address of the AUX SYMBOL BF
           LHI OLDPG26/400        ;** Load H with the page of the AUX SYMBOL BUFF
           LAM                    ;Fetch the (cc) for the name in the buffer
           CPI 001                ;See if the symbol (name) length is just one character
           JFZ FOR5               ;If not, go directly to place name in FOR/NEXT STACK
           LLI 146                ;If so, set L to point to second character location in the
           LMI 000                ;AUX SYMBOL BUFFER and set it equal to zero.
           JMP FOR5               ;This jump directs program over ontrs/cntrs/table area
           
FPFIX:     LLI 126                ;Set L to point to MSW of FPACC
           LHI OLDPG1/400         ;** Set H to point to page of FPACC
           LAM                    ;Fetch MSW of FPACC
           LLI 100                ;Change pointer to SIGN indicator on same page
           LMA                    ;Place MSW of FPACC into SIGN indicator
           NDA                    ;Now test sign bit of MSW of FPACC
           CTS FPCOMP             ;Two's complement value in FPACC if negative
           LLI 127                ;Change pointer to FPACC Exponent register
           LAI 027                ;Set accumulator to 23 (decimal) for number of bits
           LBM                    ;Load FPACC Exponent into CPU register B
           INB                    ;Exercise the value in register B
           DCB                    ;To set CPU flags
           JTS FPZERO             ;If FPACC Exponent is negative set FPACC to zero
           SUB                    ;Subtract value of FPACC Exponent from 23 decimal
           JTS FIXERR             ;If Exp larger than 23 decimal cannot convert
           LCA                    ;Else place result in register C as counter for number
FPFIXL:    LLI 126                ;Of rotate ops. Set pointer to MSW of FPACC
           LBI 003                ;Set precision counter (number of bytes in mantissa)
           CAL ROTATR             ;Rotate FPACC right the number of places indicated
           DCC                    ;By count in register C to effectively rotate all the
           JFZ FPFIXL             ;Significant bits to the left of the floating point decimal
           JMP RESIGN             ;Point. Go check original sign & negate answer if req'd.

;Following subroutine clears the FPACC to the zero condition.
FPZERO:    LLI 126                ;Set L to point to MSW of FPACC
           XRA                    ;Clear the accumulator
           LMA                    ;Set the MSW of FPACC to zero
           DCL                    ;Decrement the pointer
           LMA                    ;Set the next significant word of FPACC to zero
           DCL                    ;Decrement the pointer
           LMA                    ;Set the LSW of FPACC to zero
           DCL                    ;Decrement the pointer
           LMA                    ;Set the auxiliary FPACC byte to zero
           RET                    ;Exit to calling routine

;The next instruction is a special entry point to the FPNORM subroutine that is used
;when a number is converted from fixed to floating point. The FPNORM label is the entry
;point when a number already in floating point fonnat is to be normalized.
FPFLT:     LBI 027                ;For fixed to float set CPU register B to 23 decimal
FPNORM:    LAB                    ;Get CPU register B into ACC to check for special case
           LHI OLDPG1/400         ;** Set H to page of FPACC
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

;Floating point ADDITION. Adds contents of FPACC to FPOP and leaves result in FPACC. 
;Routine first checks to see if either register contains zero. If so addition
;result is already present!
FPADD:     LLI 126                ;Set L to point to MSW of FPACC
           LHI OLDPG1/400         ;** Do same for register H
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

;If neither FPACC or FPOP was zero then must perform addition operation. Must first check
;to see if two numbers are within significant mnge. If not, largest number is answer. 
;If numbers within range, then must align exponents before perforrning the addition of the mantissa.
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
;;;SHACOP: LLI 123                ;Set pointer to FPACC LSW minus one to provide extra
;;;        LMI 000                ;Byte for addition ops. Clear that location to zero.
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
           LBI 004                ;(FPOP already set after SHLOOP). Set precision counter
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

;The following subroutine moves the contents of a string of memory locations from the address
;pointed to by CPU registers H & L to the address specified by the contents of registers D & E
;when the routine is entered. The process continues until the counter in register B is zero.
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
           LHI OLDPG1/400         ;** Set H to page of FPACC
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
           CAL ROTATR             ;Rotate FPACC (multiplier) RIGHT into carry bit
           CTC ADOPPP             ;If carry is a one, add multiplicand to partial-product
           LLI 146                ;Set pointer to partial-product most significant byte
           LBI 006                ;Set precision counter (p-p register is double length)
           CAL ROTATR             ;Shift partial-product RIGHT
           LLI 102                ;Set pointer to bit counter storage location
           LCM                    ;Fetch current value of bit counter
           DCC                    ;Decrement the value of the bit counter
           LMC                    ;Restore the updated bit counter to its storage location
           JFZ MULTIP             ;If have not multiplied for 23 (deciinal) bits, keep going
           LLI 146                ;If have done 23 (decimal) bits, set pntr to p-p MSW
           LBI 006                ;Set precision counter (for double length)
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
;different.)
CKSIGN:    LLI 140                ;Set pointer to start of partial-product working area
           LHI OLDPG1/400         ;** Set H to proper page
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
           LBI 006                ;Set precision counter (double length working registers)
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

FPDIV:     CAL CKSIGN             ;Call routine to set up registers & ck signs of numbers
           LLI 126                ;Set pointer to MSW of FPACC (divisor)
           LAM                    ;Fetch MSW of FPACC to accumulator
           NDA                    ;Exercise CPU flags
           JTZ DVERR              ;If MSW of FPACC is zero go display 'DZ' error message
SUBEXP:    LLI 137                ;Set pointer to FPOP (dividend) Exponent
           LAM                    ;Get FPOP Exponent into accumulator
           LLI 127                ;Change pointer to FPACC (divisor) Exponent
           SUM                    ;Subtract divisor exponent from dividend exponent
           ADI 001                ;Add one for algorithm compensation
           LMA                    ;Place result in FPACC Exponent
SETDCT:    LLI 102                ;Set pointer to bit counter storage location
           LMI 027                ;Initialize bit counter to 23 decimal

;Main division algorithm for mantissas
DIVIDE:    CAL SETSUB             ;Go subtmct divisor from dividend
           JTS NOGO               ;If result is negative then place a zero bit in quotient
           LEI 134                ;If result zero or positive then move remainder after
           LLI 131                ;Subtraction from working area to become new dividend
           LBI 003                ;Set up moving pointers and initialize precision counter
           CAL MOVEIT             ;Perform the transfer
           LAI 001                ;Place a one into least significant bit of accumulator
           RAR                    ;And rotate it out into the carry bit
           JMP QUOROT             ;Proceed to rotate the carry bit into the current quotient
NOGO:      XRA                    ;When result is negative, put a zero in the carry bit, then
QUOROT:    LLI 144                ;Set up pointer to LSW of quotient register
           LBI 003                ;Set precision counter
           CAL ROTL               ;Rotate carry bit into quotient by using special entry to
           LLI 134                ;ROTATL subroutine. Now set up pointer to dividend
           LBI 003                ;LSW and set precision counter
           CAL ROTATL             ;Rotate the current dividend to the left
           LLI 102                ;Set pointer to bit counter storage location
           LCM                    ;Fetch the value of the bit counter
           DCC                    ;Decrement the value by one
           LMC                    ;Restore the new counter value to storage
           JFZ DIVIDE             ;If bit counter is not zero, continue division process
           CAL SETSUB             ;After 23 (decimal) bits, do subtraction once more for
           JTS DVEXIT             ;Possible rounding. Jump ahead if no rounding required.
           LLI 144                ;If rounding required set pointer to LSW of quotient
           LAM                    ;Fetch LSW of quotient to accumulator
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
           LBI 003                ;If not, set precision counter
           CAL ROTATR             ;And rotate quotient to the right to clear the sign bit
           LLI 127                ;Set pointer to FPACC Exponent
           LBM                    ;Fetch FPACC exponent
           INB                    ;Increment the value to compensate for the rotate right
           LMB                    ;Restore the updated exponent value
DVEXIT:    LLI 143                ;Set up pointers
           LEI 123                ;To transfer the quotient into the FPACC
           LBI 004                ;Set precision counter
                                  ;THIS IS A CORRECTION FOUND IN THE NOTES
           JMP EXMLDV             ;And exit through FPMULT routine at EXMLDV

;Subroutine to subtract divisor from dividend. Used by main DIVIDE subroutine.
SETSUB:    LEI 131                ;Set pointer to LSW of working area
           LDH                    ;On same page as FPACC
           LLI 124                ;Set pointer to LSW of FPACC (divisor)
           LBI 003                ;Set precision counter
           CAL MOVEIT             ;Perform transfer
           LEI 131                ;Reset pointer to LSW of working area (now divisor)
           LLI 134                ;Reset pointer to LSW of FPOP (dividend)
           LBI 003                ;Set precision counter
           CAL SUBBER             ;Subtract divisor from dividend
           LAM                    ;Get MSW of the result of the subtraction operations
           NDA                    ;Exercise CPU flags
           RET                    ;Return to caller with status
ADDER:     NDA                    ;Initialize the carry bit to zero upon entry
ADDMOR:    LAM                    ;Fetch byte from register group A
           CAL SWITCH             ;Switch memory pointer to register group B
           ACM                    ;Add byte from A to byte from B with carry
           LMA                    ;Leave result in register group B
           DCB                    ;Decrement number of bytes (precision) counter
           RTZ                    ;Return to caller when all bytes in group processed
           INL                    ;Else advance pointer for register group B
           CAL SWITCH             ;Switch memory pointer back to register group A
           INL                    ;Advance the pointer for register group A
           JMP ADDMOR             ;Continue the multi-byte addition operation

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
           DCB                    ;Decrement number of bytes (precision) counter
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
;subroutine entry (LSW). First entry point will clear
;the carry bit before beginning rotate operations. Second
;entry point does not clear the carry bit.
ROTATL:    NDA                    ;Clear the carry bit at this entry point
ROTL:      LAM                    ;Fetch a byte from memory
           RAL                    ;Rotate it left (bring carry into LSB, push MSB to carry)
           LMA                    ;Restore rotated word to memory
           DCB                    ;Decrement precision counter
           RTZ                    ;Exit to caller when finished
           INL                    ;Else advance pointer to next byte
           JMP ROTL               ;Continue rotate left operations


;N'th precision rotate right subroutine. Opposite of above subroutine.
ROTATR:    NDA                    ;Clear the carry bit at this entry point
ROTR:      LAM                    ;Fetch a byte from memory
           RAR                    ;Rotate it right (carry into MSB, LSB to carry)
           LMA                    ;Restore rotated word to memory
           DCB                    ;Decrement precision counter
           RTZ                    ;Exit to caller when finished
           DCL                    ;Else decrement pointer to next byte
           JMP ROTR               ;Continue rotate right operations

;N'th precision subtraction subroutine.
;Number starting at location pointed to by D & E (least
;significant byte) is subtracted from number starting at
;address specified by contents of H & L.
SUBBER:    NDA                    ;Initialize the carry bit to zero upon entry
SUBTRA:    LAM                    ;Fetch byte from register group A
           CAL SWITCH             ;Switch memory pointer to register group B
           SBM                    ;Subtract byte from group B ftom that in group A
           LMA                    ;Leave result in register group B
           DCB                    ;Decrement number of bytes (precision) counter
           RTZ                    ;Return to caller when all bytes in group processed
           INL                    ;Else advance pointer for register group B
           CAL SWITCH             ;Switch memory pointer back to register group A
           INL                    ;Advance the pointer for register group A
           JMP SUBTRA             ;Continue the multi-byte subtraction operation

;The next subroutine will transfer the four byte
;register string (generally a number in floating point
;format) from the starting address pointed to by CPU
;registers H & L when the subroutine is entered to
;the FPACC (floating point accumulator registers).
FLOAD:     LDI OLDPG1/400         ;** Set page address of FPACC
           LEI 124                ;Set address of least signficant byte of FPACC
           LBI 004                ;Set precision counter to four bytes (mantissa bytes
           JMP MOVEIT             ;Plus Exponent) and exit via the transfer routine

;The next several subroutines are used to perform
;floating pojnt register loading and transfer operations.
FSTORE:    LEL                    ;Transfer contents of register L to E
           LDH                    ;Transfer contents of register H to D
           LLI 124                ;Set L to least significant byte of FPACC mantissa
           LHI OLDPG1/400         ;** Set page to FPACC storage area
           JMP SETIT              ;Go transfer FPACC contents to area pointed to by D&E
OPLOAD:    LDI OLDPG1/400         ;** Set page to FPOP storage area
           LEI 134                ;Set pointer to least significant byte of FPOP
SETIT:     LBI 004                ;Set precision counter. Transfer from H & L area to
           JMP MOVEIT             ;Locations pointed to by D & E

;The next subroutine perforins a double transfer opera-
;tion. It first transfers the contents of the FPACC into
;the FPOP. It then transfers new DB (as pointed to by
;H & L upon entry to the subroutine) into the FPACC.
FACXOP:    CAL SAVEHL             ;Save contents of H & L upon entry to subroutine
           LLI 124                ;Set pointer to FPACC LSW
           LHI OLDPG1/400         ;** Set pointer to page of FPACC
           CAL OPLOAD             ;Transfer FPACC to FPOP
           CAL RESTHL             ;Recover original subroutine entry values for H & L
           JMP FLOAD              ;Transfer registers pointed to by H & L into the FPACC

;Subroutine to save the contents of CPU registers D, E, H
;and L in a temporary storage area in memory.
SAVEHL:    LAH                    ;Transfer value in H to ACC
           LBL                    ;And value in L to B
           LLI 200                ;Now set L to start of tempomry storage locations
           LHI OLDPG1/400         ;** And set H to storage area page
           LMA                    ;Save A (entry value of H) in memory
           INL                    ;Advance pointer
           LMB                    ;Save B (entry value of L) in memory
           INL                    ;Advance pointer
           LMD                    ;Save D in memory
           INL                    ;Advance pointer
           LME                    ;Save E in memory
           LHA                    ;Restore entry value of H
           LLB                    ;Restore entry value of L
           RET                    ;Exit to calling routine

;Subroutine to restore the contents of CPU registers D,
;E, H and L from temporary storage in memory.
RESTHL:    LLI 200                ;Set L to start of temporary storage locations
           LHI OLDPG1/400         ;** Set H to storage area page
           LAM                    ;Fetch stored value for li iii ACC
           INL                    ;Advance pointer
           LBM                    ;Fetch stored value for L into B
           INL                    ;Advance pointer
           LDM                    ;Fetch stored value for T.)
           INL                    ;Advance pointer
           LEM                    ;Fetch stored value for
           LHA                    ;Restore  saved value for H
           LLB                    ;Restore saved value for L
           LAM                    ;Leave stored value for E in ACC
           RET                    ;Exit to calling routine

;Subroutine to exchange the contents of H & L with D & E.
SWITCH:    LCH                    ;Transfer register H to C temporarily
           LHD                    ;Place value of D into H
           LDC                    ;Now put former H from C into D
           LCL                    ;Transfer register L to C temporarily
           LLE                    ;Place value of E into L
           LEC                    ;Now put former L from C into E
           RET                    ;Exit to calling routine
           
GETINP:    LHI OLDPG1/400         ;** Set H to page of GETINP character counter
           LLI 220                ;Set L to address of GETINP character counter
           LCM                    ;Load counter value into CPU register C
           INC                    ;Exercise the counter in order
           DCC                    ;To set CPU flags. If counter is non-zero, then indexing
           JFZ NOT0               ;Register (GETINP counter) is all set so jump ahead.
           LLE                    ;But, if counter zero, then starting to process a new
           LHD                    ;Character string. Transfer char string buffer pointer into
           LCM                    ;H & L and fetch the string's character count value (cc)
           INC                    ;Increment the (cc) by one to take account of (cc) byte
           CAL INDEXC             ;Add contents of regis C to H & L to point to end of the
           LMI 000                ;Character string in buffer and place a zero byte marker
NOT0:      LLI 220                ;Set L back to address of GETINP counter which is used
           LHI OLDPG1/400         ;** As an indexing value. Set H to correct page.
           LCM                    ;Fetch the value of GETINP counter into register C
           INC                    ;Increment the value in C
           LMC                    ;Restore the updated value for future use
           LLE                    ;Bring the base address of the character string buffer into
           LHD                    ;CPU registers H & L
           CAL INDEXC             ;Add contents of register C to form indexed address of
           LAM                    ;Next character to be fetched as input. Fetch the next
           NDA                    ;Character. Exercise the CPU flags.
           LHI OLDPG1/400         ;** Restore page pointer to floating point working area
           RFZ                    ;If character is non-zero, not end of string, exit to calle
           LLI 220                ;If zero character, must reset GETINP counter for next
           LMI 000                ;String. Reset pointer and clear GETINP counter to zero
           RET                    ;Then exit to calling routine

;Following subroutine causes register C to be used as an
;indexing register. Value in C is added to address in H
;and L to form new address.
INDEXC:    LAL                    ;Place value from register L into accumulator
           ADC                    ;Add quantity in register C
           LLA                    ;Restore updated value back to L
           RFC                    ;Exit to caller if no carry from addition
           INH                    ;But, if have carry then must increment register H
           RET                    ;Before returning to calling routine

;Main Decimal INPUT subroutine to convert strings of
;ASCII characters representing decimal fixed or floating
;point numbers to binary floating point numbers.
DINPUT:    LEL                    ;Save entry value of register L in E. (Pointer to buffer
           LDH                    ;Containing ASCII character string.) Do same for H to D.
           LHI OLDPG1/400         ;** Set H to page of floating point working registers
           LLI 150                ;Set L to start of decirnal-to-binary working area
           XRA                    ;Clear the accumulator
           LBI 010                ;Set up a loop counter
CLRNX2:    LMA                    ;Deposit zero in working area to initialize
           INL                    ;Advance the memory pointer
           DCB                    ;Decrement the loop counter
           JFZ CLRNX2             ;Clear working area until loop counter is zero
           LLI 103                ;Set pointer to floating point temporary registers and
           LBI 004                ;Indicators working area. Set up a loop counter.
CLRNX3:    LMA                    ;Deposit zero in working area to initialize
           INL                    ;Advance the memory pointer
           DCB                    ;Decrement the loop counter
           JFZ CLRNX3             ;Clear working area until loop counter is zero
           CAL GETINP             ;Fetch a character from the ASCII chax string buffer
           CPI 253                ;(Typically the SYMBOL/TOKEN buffer). See if it is
           JTZ NINPUT             ;Code for + sign. Jump ahead if code for + sign.
           CPI 255                ;See if code for minus (-) sign.
           JFZ NOTPLM             ;Jump ahead if not code for minus sign. If code for
           LLI 103                ;Minus sign, set pointer to MINUS flag storage location.
           LMA                    ;Set the MINUS flag to indicate a minus number
NINPUT:    CAL GETINP             ;Fetch another character from the ASCII char string
NOTPLM:    CPI 256                ;See if character represents a period (decimal point) in
           JTZ PERIOD             ;Input string. Jump ahead if yes.
           CPI 305                ;If not period, see if code for E as in Exponent
           JTZ FNDEXP             ;Jump ahead if yes.
           CPI 240                ;Else see if code for space.
           JTZ NINPUT             ;Ignore space character, go fetch another character.
           NDA                    ;If none of the above see if zero byte
           JTZ ENDINP             ;Indicating end of input char string. If yes, jumn ahead.
           CPI 260                ;If not end of string, check to see
           JTS NUMERR             ;If character represents
           CPI 272                ;A valid decimal number (0 to 9)
           JFS NUMERR             ;Display error message if not a valid digit at this point!
           LLI 156                ;For valid digit, set pointer to MSW of temporary
           LCA                    ;Decimal to binary holding registers. Save character in C.
           LAI 370                ;Form mask for sizing in accumulator. Now see if
           NDM                    ;Holding register has enough room for the conversion of
           JFZ NINPUT             ;Another digit. Ignore the input if no more room.
           LLI 105                ;If have room in register then set pointer to input digit
           LBM                    ;Counter location. Fetch the present value.
           INB                    ;Increment it to account for incoming digit.
           LMB                    ;Restore updated count to storage location.
           CAL DECBIN             ;Call the DECimal to BINary conversion routine to add
           JMP NINPUT             ;In the new digit in holding registers. Continue inputting.
PERIOD:    LBA                    ;Save character code in register B
           LLI 106                ;Set pointer to PERIOD indicator storage location
           LAM                    ;Fetch value in PERIOD indicator
           NDA                    ;Exercise CPU flags
           JFZ NUMERR             ;If already have a period then display error message
           LLI 105                ;If not, change pointer to digit counter storage location
           LMA                    ;Clear the digit counter back to zero
           INL                    ;Advance pointer to PERIOD indicator
           LMB                    ;Set the PERIOD indicator
           JMP NINPUT             ;Continue processing the input character string
FNDEXP:    CAL GETINP             ;Get next character in Exponent
           CPI 253                ;See if it is code for + sign
           JTZ EXPINP             ;Jump ahead if yes.
           CPI 255                ;If not + sign, see if minus sign
           JFZ NOEXPS             ;If not minus sign then jump ahead
           LLI 104                ;For minus sign, set pointer to EXP SIGN indicator
           LMA                    ;Set the EXP SIGN indicator for a minus exponent
EXPINP:    CAL GETINP             ;Fetch the next character in the decimal exponent
NOEXPS:    NDA                    ;Exercise the CPU flags
           JTZ ENDINP             ;If character inputted was zero, then end of input string
           CPI 260                ;If not end of string, check to see
           JTS NUMERR             ;If character represents
           CPI 272                ;A valid decimal number (0 to 9)
           JFS NUMERR             ;Display error message if not a valid digit at this point!
           NDI 017                ;Else trim the ASCII code to BCD
           LBA                    ;And save in register B
           LLI 157                ;Set pointer to input exponent storage location
           LAI 003                ;Set accumulator equal to three
           CPM                    ;See if any previous digit in exponent greater than three
           JTS NUMERR             ;Display error message if yes
           LCM                    ;Else save any previous value in register C
           LAM                    ;And also place any previous value in accumulator
           NDA                    ;Clear the carry bit with this instruction
           RAL                    ;Single precision multiply by ten algorithm
           RAL                    ;Two rotate lefts equals times four
           ADC                    ;Adding in the digit makes total times five
           RAL                    ;Rotating left again equals times ten
           ADB                    ;now add in digit just inputted
           LMA                    ;Restore the value to exponent storage location
           JMP EXPINP             ;Go get any additional exponent int)ut
ENDINP:    LLI 103                ;Set pointer to mantissa SIGN indicator
           LAM                    ;Fetch the SIGN indicator to the acclimulator
           NDA                    ;Exercise the CPU flags
           JTZ FININP             ;If SIGN indicator is zero, go finish up as nr is positive
           LLI 154                ;But, if indicator is non-zero, number is negative
           LBI 003                ;Set pntr to LSW of storage registers, set precision entr
           CAL COMPLM             ;Negate the triple-precision number in holding registers
FININP:    LLI 153                ;Set pointer to input storage LS~V minus one
           XRA                    ;Clear the accumulator
           LMA                    ;Clear the LSW minus one location
           LDH                    ;Set register D to floating point working page
           LEI 123                ;Set E to address of FPACC LSW minus one
           LBI 004                ;Set precision counter
           CAL MOVEIT             ;Move number from input register to FPACC
           CAL FPFLT              ;Now convert the binary fixed point to floating point
           LLI 104                ;Set pointer to Exponent SIGN indicator location
           LAM                    ;Fetch the value of the EXP SIGN indicator
           NDA                    ;Exercise the CPU flags
           LLI 157                ;Reset pointer to input exponent storage location
           JTZ POSEXP             ;If EXP SIGN indicator zero, exponent is positive
           LAM                    ;Else, exponent is negative so must negate
           XRI 377                ;The value in the input exponent storage location
           ADI 001                ;By performing this two's complement
           LMA                    ;Restore the negated value to exponent storage location
POSEXP:    LLI 106                ;Set pointer to PERIOD indicator storage location
           LAM                    ;Fetch the contents of the PERIOD indicator
           NDA                    ;Exercise the CPU flags
           JTZ EXPOK              ;If PERIOD indicator clear, no decimal point involved
           LLI 105                ;If have a decimal point, set pointer to digit counter
           XRA                    ;Storage location. Clear the accumulator.
           SUM                    ;And get a negated value of the digit counter in ACC
EXPOK:     LLI 157                ;Change pointer to input exponent storage location
           ADM                    ;Add this value to negated digit counter value
           LMA                    ;Restore new value to storage location
           JTS MINEXP             ;If new value is minus, skip over next subroutine
           RTZ                    ;If new value is zero, no further processing required

;Following subroutine will multiply the floating point
;binary number stored in FPACC by ten tirnes the
;value stored in the deciinal exponent storage location.
FPX10:     LLI 210                ;Set pointer to registers containing floating point
           LHI OLDPG1/400         ;** Binary representation of 10 (decimal).
           CAL FACXOP             ;Transfer FPACC to FPOP and 10 (dec) to FPACC
           CAL FPMULT             ;Multiply FPOP (formerly FPACC) by 10 (decimal)
           LLI 157                ;Set pointer to decimal exponent storage location
           LCM                    ;Fetch the exponent value
           DCC                    ;Decrement
           LMC                    ;Restore to storage
           JFZ FPX10              ;If exponent value is not zero, continue multiplication
           RET                    ;When exponent is zero can exit. Conversion completed.

;Following subroutine will multiply the floating point
;binary number stored in PPACC by 0.1 times the value
;(negative) stored in the decimal exponent storage location
MINEXP:
FPD10:     LLI 214                ;Set pointer to registers containing floating point
           LHI OLDPG1/400         ;** Binary representation of 0.1 (decimal).
           CAL FACXOP             ;Transfer FPACC to FPOP and 0.1 (dec) to FPACC
           CAL FPMULT             ;Multitply FPOP (formerly FPACC) by 0.1 (decimal)
           LLI 157                ;Set pointer to decimal exponent storage location
           LBM                    ;Fetch the exponent value
           INB                    ;Increment
           LMB                    ;Restore to storage
           JFZ FPD10              ;If exponent value is not zero, continue multiplication
           RET                    ;When exponent is zero can exit. Conversion completed.

;Following subroutine is used to convert decimal characters to binary fixed point
;in a triple-precision format.
DECBIN:    CAL SAVEHL             ;Save entry value of D, E, H and L in memory
           LLI 153                ;Set pointer to temporary storage location
           LAC                    ;Restore character inputted to accumulator
           NDI 017                ;Trim ASCII code to BCD
           LMA                    ;Store temporarily
           LEI 150                ;Set pointer to working area LSW of multi-byte register
           LLI 154                ;Set another pointer to LSW of conversion register
           LDH                    ;Make sure D set to page of working area
           LBI 003                ;Set precision counter
           CAL MOVEIT             ;Move original value of conversion register to working
           LLI 154                ;Register. Reset pointer to LSW of conversion register.
           LBI 003                ;Set precision counter
           CAL ROTATL             ;Rotate register left, (Multiplies value by two.)
           LLI 154                ;Reset pointer to LSW.
           LBI 003                ;Set precision counter
           CAL ROTATL             ;Multiply by two again (total now times four).
           LEI 154                ;Set pointer to LSW of conversion register.
           LLI 150                ;Set pointer to LSW of working register (original value).
           LBI 003                ;Set precision counter.
           CAL ADDER              ;Add original value to rotated value (now times five).
           LLI 154                ;Reset pointer to LSW
           LBI 003                ;Set precision counter
           CAL ROTATL             ;Multiply by two once more (total now times ten).
           LLI 152                ;Set pointer to clear working register locatiotis
           XRA                    ;Clear the accumulator
           LMA                    ;Clear MSW of working register
           DCL                    ;Decrement pointer
           LMA                    ;Clear next byte
           LLI 153                ;Set pointer to current digit storage location
           LAM                    ;Fetch the current digit
           LLI 150                ;Change pointer to LSW of working register
           LMA                    ;Deposit the current digit in LSW of working register
           LEI 154                ;Set pointer to conversion register LSW
           LBI 003                ;Set precision counter
           CAL ADDER              ;Add current digit to conversion register to complete
           JMP RESTHL             ;Conversion. Exit to caller by restoring CPU registers.
FPOUT:     LHI OLDPG1/400         ;** Set H to working area for floating point routines
           LLI 157                ;Set pointer to decimal exponent storage location
           LMI 000                ;Initialize storage location to zero
           LLI 126                ;Change pointer to FPACC (number to be outputted)
           LAM                    ;And fetch MSW of FPACC
           NDA                    ;Test the contents of MSW of FPACC
           JTS OUTNEG             ;If most significant bit of MSW is a one, have a minus nr.
           LAI 240                ;Else number is positive, set ASCII code for space for a
           JMP AHEAD1             ;Positive number and go display a space
OUTNEG:    LLI 124                ;If number in FPACC is negative must negate in order
           LBI 003                ;To display. Set pntr to LSW of FPACC & set prec. cntr.
           CAL COMPLM             ;Negate the number in the FPACC to make it positive
           LAI 255                ;But load ACC with ASCII code for minus sign
AHEAD1:    CAL ECHO               ;Call user display driver to output space or minus sign
           LLI 110                ;Set pointer to FIXED/FLOAT indicator
           LAM                    ;Fetch value of FIXED/FLOAT indicator
           NDA                    ;Test contents of indicator. If contents are zero, calling
           JTZ OUTFLT             ;Routine has directed floating point output format.
           LLI 127                ;If indicator non-zero, fixed point fonnat requested if
           LAI 027                ;Possible. Point to FPACC Exponent. Put 23 decimal in
           LBM                    ;Accumulator. Fetch FPACC Exponent into register B
           INB                    ;And exercise the register to test its
           DCB                    ;Original contents. If FPACC Exponent is negative in
           JTS OUTFLT             ;Value then go to floating point output forrnat. If value
           SUB                    ;Is positive, subtract value from 23 (decimal). If result
           JTS OUTFLT             ;Negative, number is too big to use fixed format.
           JMP OUTFIX             ;Else, can use fixed format so skip next routine
OUTFLT:    LLI 110                ;Set pointer to FIXED/FLOAT indicator.
           LMI 000                ;Clear indicator to indicate floating point output format
           LAI 260                ;Load ASCII code for '0' into accumulator
           CAL ECHO               ;Call user display driver to output '0' as first character
           LAI 256                ;Number string. Now load ASCII code for decimal point.
           CAL ECHO               ;Call user display driver to output '.'as second character.
OUTFIX:    LLI 127                ;Set pointer to FPACC Exponent
           LAI 377                ;Load accumulator with minus one
           ADM                    ;Add value in FPACC Exponent
           LMA                    ;Restore compensated exponent value

;Next portion of routine establishes the value for the
;decimal exponent that will be outputted by processing
;the binary exponent value in the FPACC.
DECEXT:    JFS DECEXD             ;If compensated exponent value is zero or positive
           LAI 004                ;Then go multiply FPACC by 0.1 (decimal). Else,
           ADM                    ;Add four to the exponent value.
           JFS DECOUT             ;If exponent now zero or positive, ready to output
           LLI 210                ;If exponent negative, multiply FPACC by 10 (decimal)
           LHI OLDPG1/400         ;** Set pointer to registers holding 10 (dec) in binary
           CAL FACXOP             ;Floating point format. Set up for multiplication.
           CAL FPMULT             ;Perform the multiplication. Answer in FPACC.
           LLI 157                ;Set pointer to decimal exponent storage location.
           LCM                    ;Each time the FPACC is multiplied by ten, need to
           DCC                    ;Decrement the value in the decinial exponent storage
           LMC                    ;Location. (This establishes decimal exponent value!)
DECREP:    LLI 127                ;Reset pointer to FPACC Exponent
           LAM                    ;Fetch value in exponent
           NDA                    ;Test value
           JMP DECEXT             ;Repeat process as required
DECEXD:    LLI 214                ;If exponent is positive, multiply FPACC by 0.1
           LHI OLDPG1/400         ;** Set pointer to registers holding 0.1 dec in binary
           CAL FACXOP             ;Floating point format. Set up for multipli(-ation.
           CAL FPMULT             ;Perform the multiplication. Answer in FPACC.
           LLI 157                ;Set pointer to decimal exponent storage location.
           LBM                    ;Each time the FPACC is multiplied by one tenth, need
           INB                    ;To increment the value in the decimal exponent storage
           LMB                    ;Location. (This establishes decimal exponent value!)
           JMP DECREP             ;Repeat process as required

;The next section outputs the mantissa
;(or fixed point number) by converting the value remaining
;in the FPACC (after the decimal exponent equivalent has
;been extracted from the original value if required by the
;previous routines) to a string of decirnal digits.
DECOUT:    LEI 164                ;Set pointer to LSW of output working register
           LDH                    ;Set D to same page value as H
           LLI 124                ;Set pointer to LSW of FPACC
           LBI 003                ;Set precision counter
           CAL MOVEIT             ;Move value in FPACC to output working register
           LLI 167                ;Set pointer to MSW plus one of output working register
           LMI 000                ;Clear that location to 0
           LLI 164                ;Set pointer to LSW of output working register
           LBI 003                ;Set precision counter
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
           LAM                    ;Fetch BCD of digit to be outputted
           NDA                    ;Exercise CPU flags
           JFZ OUTDGX             ;If not zero, go display the digit
           LLI 110                ;If zero, change pointer to FIXED/FLOAT indicator
           LAM                    ;Fetch the indicator into the accumulator
           NDA                    ;Test value of indicator
           JTZ OUTZER             ;If in floating point mode, go display the digit
           LLI 157                ;Else change pointer to decimal exponent storage
           LCM                    ;Location, which, for fixed point, will have a positive
           DCC                    ;Value for all digits before the decimal point. Decrement
           INC                    ;And increment to exercise flags. See if count is positive.
           JFS OUTZER             ;If positive, must display any zero digit.
           LLI 166                ;If not, change pointer to MSW of working register
           LAM                    ;And test to see if any significant digits coming up
           NDI 340                ;By forming a mask and testing for presence of bits
           JFZ OUTZER             ;If more significant digits coming up soon, display the
           RET                    ;Zero digit. Else, exit to calling routine. Finished.
OUTZER:    XRA                    ;Clear the accumulator to restore zero digit value
OUTDGX:    ADI 260                ;Add 260 (octal) to BCD code in ACC to form ASCII
           CAL ECHO               ;Code and call the user's display driver subroutine
DECRDG:    LLI 110                ;Set pointer to FIXED/FLOAT indicator storage
           LAM                    ;Fetch the indicator to the accumulator
           NDA                    ;Exercise the CPU flags
           JFZ CKDECP             ;If indicator non-zero, doing fixed point output
           LLI 107                ;Else, get output digit counter
           LCM
           DCC                    ;Decrement the digit counter & restore to storage
           LMC
           JTZ EXPOUT             ;When digit counter is zero, go take care of exponent
PUSHIT:    CAL OUTX10             ;Else push next BCD digit out of working register
           JMP OUTDGS             ;And continue the outputting process
CKDECP:    LLI 157                ;For fixed point output, decimal exponent serves as
           LCM                    ;Counter for number of digits before decimal point
           DCC                    ;Fetch the counter and decrement it to account for
           LMC                    ;Current digit being processed. Restore to storage.
           JFZ NODECP             ;If count does not go to zero, jump ahead.
           LAI 256                ;When count reaches zero, load ASCII code for period
           CAL ECHO               ;And call user's display driver to display decimal point
NODECP:    LLI 107                ;Set pointer to output digit counter storage location
           LCM                    ;Fetch the digit counter
           DCC                    ;Decrement the value
           LMC                    ;Restore to storage
           RTZ                    ;If counter reaches zero, exit to caller. Finished.
           JMP PUSHIT             ;Else continue to output the number.
ZERODG:    LLI 157                ;If first digit of floating point number is a zero, set
           LCM                    ;Pointer to decimal exponent storage location.
           DCC                    ;Decrement the value to compensate for skipping
           LMC                    ;Display of first digit. Restore to storage.
           LLI 166                ;Change pointer to MSW of output working register
           LAM                    ;Fetch MSW of output working register
           NDA                    ;Test the contents
           JFZ DECRDG             ;If non-zero, continue outputting
           DCL                    ;Else decrement pointer to next byte in working register
           LAM                    ;Fetch its contents
           NDA                    ;Test
           JFZ DECRDG             ;If non-zero, continue outputting
           DCL                    ;Else decrement pointer to LSW of working register
           LAM                    ;Fetch its contents
           NDA                    ;Test
           JFZ DECRDG             ;If non-zero, continue outputting
           LLI 157                ;If decimal mantissa is zero, set pointer to decirnal
           LMA                    ;Exponent storage and clear it
           JMP DECRDG             ;Finish outputting

;Following routine multiplies the binary number in the
;output working register by ten to push the most significant digit out to the MSW+L byte.
OUTX10:    LLI 167                ;Set pointer to work ing register M SW+ 1
           LMI 000                ;Clear it in preparation for receiving next digit pushed
           LLI 164                ;Into it. Change pointer to working register LSW.
           LDH                    ;Set up register D to same page as H.
           LEI 160                ;Set second pointer to LSW of second working register
           LBI 004                ;Set precision counter
           CAL MOVEIT             ;Move first working register into second
           LLI 164                ;Reset pointer to LSW of first working register
           LBI 004                ;Set precision counter
           CAL ROTATL             ;Rotate contents of first working register left (X 2)
           LLI 164                ;Reset pointer to LSW
           LBI 004                ;Reset precision counter
           CAL ROTATL             ;Rotate contents left again (X 4)
           LLI 160                ;Set pointer to LSW of original value in 2'nd register
           LEI 164                ;Set pointer to LSW of rotated value
           LBI 004                ;Set precision counter
           CAL ADDER              ;Add rotated value to original value (X 5)
           LLI 164                ;Reset pointer to LSW of first working register
           LBI 004                ;Set precision counter
           CAL ROTATL             ;Rotate contents left again (X 10)
           RET                    ;Exit to calling routine

;The final group of routines in the floating point output
;section take care of outputting the decimal exponent
;portion of floating point numbers.
EXPOUT:    LLI 157                ;Set pointer to decimal exponent storage location
           LAM                    ;Fetch value to the accumulator
           NDA                    ;Test the value
           RTZ                    ;If zero, then no exponent portion. Exit to caller.
           LAI 305                ;Else, load ACC with ASCII code for letter E.
           CAL ECHO               ;Display E for Exponent via user's display driver rtn
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
           INB                    ;Increment register B as a counter
           JMP SUB12              ;Repeat loop to form tens value of decimal exponent
TOMUCH:    LAI 260                ;Load base ASCII value for digit into the accumulator
           ADB                    ;Add to the count in B to forin tens digit of decimal
           CAL ECHO               ;Exponent. Display via user's driver subroutine
           LAM                    ;Fetch remainder of decimal exponent value
           ADI 260                ;Add in ASCII base value to form final digit
           CAL ECHO               ;Display second digit of decirnal exponent
           RET                    ;Finished outputting. Return to caller.
           
;;; The following is PATCH NR.1
PATCH1:	   LLI 123
           LMI 000
           LLI 133
           LMI 000
           RET

NEXT:      LLI 144                ;Load L with start of AUX SYMBOL BUFFER
           LHI OLDPG26/400        ;** Set H to page of AUX SYMBOL BUFFER
           LMI 000                ;Initialize AUX SYMBOL BUFFER by clearing first byte
           LLI 202                ;Change L to address of SCAN pointer
           LBM                    ;Fetch pointer value to CPU register B
           INB                    ;Add one to the current pointer value
           LLI 201                ;Load L with address of NEXT pointer storage location
           LMB                    ;Place the updated SCAN pointer as the NEXT pointer
NEXT1:     LLI 201                ;Reset L to address of NEXT pointer storage location
           CAL GETCHR             ;Fetch the character pointed to by the NEXT pointer
           JTZ NEXT2              ;If the character is a space, ignore it
           LLI 144                ;Else, load L with start of AUX SYMBOL BUFFER
           CAL CONCT1             ;Concatenate the character onto the AUX SYMBOL BF
NEXT2:     LLI 201                ;Reset L to address of NEXT pointer storage location
           CAL LOOP               ;Advance the NEXT pointer and see if end of line
           JFZ NEXT1              ;Fetch next character in line if not end of line
           LLI 144                ;When reach end of line, should have variable name
           LAM                    ;In the AUX SYMBOL BUFFER. Fetch the (cc) for
           CPI 001                ;The buffer and see if variable name is just one letter
           JFZ NEXT3              ;If more than one proceed directly to look for name
           LLI 146                ;In FOR/NEXT STACK. If have just a one letter name
           LMI 000                ;Then set second character in buffer to zero
NEXT3:     LLI 205                ;Load L with address of FOR/NEXT STACK pointer
           LHI OLDPG27/400        ;** Set H to page of FOR/NEXT STACK pointer
           LAM                    ;Fetch the FOR/NEXT STACK pointer value to ACC
           RLC                    ;Rotate value left to multiply by two. Then rotate it
           RLC                    ;Left again to multiply by four. Add base address plus
           ADI 136                ;Two to form pointer to variable name in top of stack
           LHI OLDPG27/400        ;** Set H to page of FOR/NEXT STACK
           LLA                    ;Move pointer value from ACC to CPU register L
           LDI OLDPG26/400        ;** Set register D to page of AUX SYMBOL BUFFER
           LEI 145                ;Set register E to first character in the buffer
           LBI 002                ;Set B to serve as a character counter
           CAL STRCPC             ;See if variable name in the NEXT statement same as
           JTZ NEXT4              ;That stored in the top of the FOR/NEXT STACK
FORNXT:    LAI 306                ;Load ACC with ASCII code for letter F
           LCI 316                ;Load register C with ASCII code for letter N
           JMP ERROR              ;Display For/Next (FN) error message if required
NEXT4:     LLI 360                ;Load L with address of user program line pointer
           LHI OLDPG26/400        ;** Load H with page of user pgm line pntr storage loc.
           LDM                    ;Fetch the page portion of the line pointer into D
           INL                    ;Advance the memory pointer
           LEM                    ;Fetch the low portion of the line pointer into E
           INL                    ;Advance pntr to AUXILIARY LINE POINTER storage
           LMD                    ;Location and store value of line pointer there too (page)
           INL                    ;Advance pointer to second byte of AUXILIARY line
           LME                    ;Pointer and store value of line pointer (low portion)
           LLI 205                ;Load L with address of FOR/NEXT STACK pointer
           LHI OLDPG27/400        ;** Set H to page of FOR/NEXT STACK pointer
           LAM                    ;Fetch the FOR/NEXT STACK pointer value to ACC
           RLC                    ;Rotate value left to multiply by two. Then rotate it
           RLC                    ;Left again to multiply by four. Add base address to
           ADI 134                ;Form pointer to top of FOR/NEXT STACK and place
           LLA                    ;The pointer value into CPU register L. Fetch the page
           LDM                    ;Address of the associated FOR statement line pointer
           INL                    ;Into register D. Advance the pointer and fetch the low
           LEM                    ;Address value into register E. Prepare to change user
           LLI 360                ;Program line pointer to the FOR statement line by
           LHI OLDPG26/400        ;** Setting H & L to the user pgrn line pntr storage loc.
           LMD                    ;Place the page value in the pointer storage location
           INL                    ;Advance the memory pointer
           LME                    ;Place the low value in the pointer storage location
           LHD                    ;Now set up H and L to point to the start of the
           LLE                    ;Associated FOR statement line in the user pgm buffer
           LDI OLDPG26/400        ;** Change D to point to the line input buffer
           LEI 000                ;And set L to the gtart of the line input buffer
           CAL MOVEC              ;Move the associated FOR statement line into the input
           LLI 325                ;Line buffer. Set L to point to start of TO string which is
           LHI OLDPG1/400         ;** Stored in a text strings storage area on this page
           CAL INSTR              ;Search the statement line for the occurrence of TO
           LAE                    ;Register E will be zero if TO not found. Move E to ACC
           NDA                    ;To make a test. If TO found then proceed to set up for
           JTZ FORNXT             ;Evaluation. If TO not found, then have error condition.
           ADI 002                ;Advance the pointer over the characters in TO string
           LLI 276                ;Change L to point to EVAL pointer storage location
           LHI OLDPG26/400        ;** Set H to page of EVAL pointer. Set up the starting
           LMA                    ;Position for the EVAL subroutine (after TO string)
           LLI 330                ;Set L to point to start of STEP string which is stored
           LHI OLDPG1/400         ;** In text stxings storage area on this page. Search the
           CAL INSTR              ;Statement line for the occurrence of STEP
           LAE                    ;Register E will be zero if STEP not found. Move E to
           NDA                    ;The accumulator to make a test. If STEP found must
           JFZ NEXT5              ;Evaluate expression after STEP to get STEP SIZE.
           LLI 004                ;Else, have an IMPLIED STEP SIZE of 1.0. Set pointer
           LHI OLDPG1/400         ;** To start of storage area for 1.0 in floating point
           CAL FLOAD              ;Format and call subroutine to load FPACC with 1.0
           LLI 304                ;Set L to start of FOR/NEXT STEP SIZE storage loc.
           CAL FSTORE             ;Store the value 1.0 in the F/N STEP SIZE registers
           LLI 000                ;Change L to the start of the input line buffer
           LHI OLDPG26/400        ;** Set H to the page of the input line buffer
           LBM                    ;Fetch the (cc) into CPU register B (length of FOR line)
           LLI 277                ;Change L to EVAL FINISH pointer stomge location
           LMB                    ;Set the EVAL FINISH pointer to the end of the line
           CAL EVAL               ;Evaluate the LIMIT expression to obtain FOR LIMIT
           LLI 310                ;Load L with address of start of F/N LIMIT registers
           LHI OLDPG1/400         ;** Load H with page of FOR/NEXT LIMIT registers
           CAL FSTORE             ;Store the FOR/NEXT LIMIT value
           JMP NEXT6              ;Since have IMPLIED STEP jump ahead
NEXT5:     DCE                    ;MGA 3/21/12 lab here. When have STEP directive, subtract one from pointer
           LLI 277                ;To get to character before S in STEP. Save this value in
           LHI OLDPG26/400        ;** The EVAL FINISH pointer stomge location to serve
           LME                    ;As evaluation end location when obtaining TO Iiinit
           CAL EVAL               ;Evaluate the LIMIT expression to obtain FOR LIMIT
           LLI 310                ;Load L with address of start of FIN LIMIT registers
           LHI OLDPG1/400         ;** Load H with page of FORINEXT LIMIT registers
           CAL FSTORE             ;Store the FOR/NEXT LIMIT value
           LLI 277                ;Reset L to EVAL FINISH pointer storage location
           LHI OLDPG26/400        ;** Set H to page of EVAL FINISH pointer storage loc.
           LAM                    ;Fetch the pointer value (character before S in STEP)
           ADI 005                ;Add five to change pointer to character after P in STEP
           DCL                    ;Decrement L to point to EVAL (start) pointer
           LMA                    ;Set up the starting position for the EVAL subroutine
           LLI 000                ; Load L with starting address of the line input buffer
           LBM                    ;Fetch the (cc) for the line input buffer (line length)
           LLI 277                ;Change L to the EVAL FINISH storage location
           LMB                    ;Set the EVAL FINISH pointer
           CAL EVAL               ;Evaluate the STEP SIZE expression
           LLI 304                ;Load L with address of start of F/N STEP registers
           LHI OLDPG1/400         ;** Set H to page of FIN STEP registers
           CAL FSTORE             ;Store the FOR/NEXT STEP SIZE value
NEXT6:     LLI 144                ;Load L with address of AUX SYMBOL BUFFER
           LHI OLDPG26/400        ;** Set H to page of the AUX SYMBOL BUFFER
           LMI 000                ;Initialize AUX SUMBOL BUFFER with a zero byte
           LLI 034                ;Set L to start of FOR string which is stored in the
           LHI OLDPG27/400        ;** KEYWORD look-up table on this page
           CAL INSTR              ;Search the statement line for the FOR directive
           LAE                    ;Register E will be zero if FOR not found. Move E to
           NDA                    ;ACC and -make test to see if FOR directive located
           LLI 202                ;Load L with address of SCAN pointer
           LHI OLDPG26/400        ;** Load H with page of SCAN pointer
           LMA                    ;Set up pointer to occurrence of FOR directive in line
           JTZ FORNXT             ;If FOR not found, have an error condition
           ADI 003                ;If have FOR, add three to advance pointer over FOR
           LLI 203                ;Set L to point to F/N pointer storage location
           LMA                    ;Set F/N pointer to character after FOR directive
NEXT7:     LLI 203                ;Set L to point to FIN pointer storage location
           CAL GETCHR             ;Fetch a character from position pointed to by FIN pntr
           JTZ NEXT8              ;If character is a space, ignore it
           CPI 275                ;Else, test to see if character is "=" sign
           JTZ NEXT9              ;If yes, have picked up variable name, jump ahead
           LLI 144                ;If not, set L to the start of the AUX SYMBOL BUFFER
           CAL CONCT1             ;And store the character in the AUX SYMBOL BUFFER
NEXT8:     LLI 203                ;Load L with address of the F/N pointer
           CAL LOOP               ;Increment the pointer and see if end of the line
           JFZ NEXT7              ;If not, continue fetching characters
           JMP FORNXT             ;If end of line before "=" sign then have error condx
NEXT9:     LLI 202                ;Load L with address of SCAN pointer
           LHI OLDPG26/400        ;** Load H with page of SCAN pointer
           LAM                    ;Fetch pointer value to ACC (points to start of FOR
           ADI 003                ;Directive) and add three to move pointer over FOR
           LLI 276                ;Directive. Change L to EVAL pointer storage location
           LMA                    ;Set EVAL pointer to character after FOR in line
           LLI 203                ;Load L with address of FIN pointer storage location
           LBM                    ;Fetch pointer to register B (points to "=" sign) and
           DCB                    ;Decrement the pointer (to character before "=" sign)
           LLI 277                ;Load L with address of EVAL FINISH pointer
           LMB                    ;Set EVAL FINISH pointer
           CAL EVAL               ;Call subroutine to obtain current value of the variable
           LLI 304                ;Load L with address of start of F/N STEP registers
           LHI OLDPG1/400         ;** Set H to page of F/N STEP registers
           CAL FACXOP             ;Call subroutine to set up FP registers for addition
           CAL FPADD              ;Add FIN STEP size to current VARIABLE value
           LLI 314                ;Load L with address of FIN TEMP storage registers
           LHI OLDPG1/400         ;**Set H to page of FIN TEMP storage registers
           CAL FSTORE             ;Save the result of the addition in F/N TEMP registers
           LLI 310                ;Load L with starting address of F/N LIMIT registers
           CAL FACXOP             ;Call subroutine to set up FP registers for subtraction
           CAL FPSUB              ;Subtract F/N LIMIT value from VARIABLE value
           LLI 306                ;Set pointer to MSW of F/N STEP registers
           LAM                    ;Fetch this value into the ACC
           NDA                    ;Test to see if STEP value might be zero
           LLI 126                ;Load L with address of MSW of FPACC
           LAM                    ;Fetch this value into the ACC
           JTZ FORNXT             ;If STEP size was zero, then endless loop, an error condx
           JTS NEXT11             ;If STEP size less than zero make alternate test on limit
           NDA                    ;Test the contents of the MSW of the FPACC
           JTS NEXT12             ;Continue FORINEXT loop if current variable value is
           JTZ NEXT12             ;Less than or equal to the F/N LIMIT value
NEXT10:    LLI 363                ;If out of LIMIT range, load L with address of the AUX
           LHI OLDPG26/400        ;** PGM LINE pointer. (Contains pointer to the NEXT
           LEM                    ;Statement line that initiated this routine.) Fetch the
           DCL                    ;Low part of the address into E, decrement the memory
           LDM                    ;And get the page part of the address into CPU register
           DCL                    ;Decrement memory pointer to the low portion of the
           LME                    ;User pgm buffer line pointer (regular pointer) and set it
           DCL                    ;With the value from the AUX line pntr, decrement the
           LMD                    ;Pointer and do the same for the page portion
           LLI 205                ;Set L to address of FOR/NEXT STACK pointer
           LHI OLDPG27/400        ;** Set H to page of FOR/NEXT STACK pointer
           LBM                    ;Fetch and decrement the
           DCB                    ;FOR/NEXT STACK pointer value
           LMB                    ;To perform effective popping operation
           JMP NXTLIN             ;Statement line after NEXT statement is done next
NEXT11:    NDA                    ;When F/N STEP is negative, reverse test so that if the
           JFS NEXT12             ;Variable value is greater than or equal to the F/N LIMIT
           JMP NEXT10             ;The FOR/NEXT loop continues. Else it is finished.
NEXT12:    LLI 314                ;Load L with address of FIN TEMP storage registers
           LHI OLDPG1/400         ;** Set H to FIN TEMP storage registers page
           CAL FLOAD              ;Transfer the updated variable value to the FPACC
           CAL RESTSY             ;Restore the variable name and value
           CAL STOSYM             ;In the VARIABLES table. Exit routine so that
           JMP NXTLIN             ;Statement line after FOR statement is done next

BACKSP:    LAI 215                ;Load ASCII code for carriage-return into the ACC
           CAL ECHO               ;Display the carriage-return
           CAL ECHO               ;Repeat to provide extra time if TTY
           LLI 043                ;Load L with address of COLUMN COUNTER
           LHI OLDPG1/400         ;** Set H to page of COLUMN COUNTER
           LMI 001                ;Set COLUMN COUNTER to first column
           LLI 124                ;Set L to address containing desired TAB position
           LAM                    ;Fetch the desired TAB position value
           NDA                    ;Test to see if it is
           RTS                    ;Negative or zero
           RTZ                    ;In which case return to caller
           JMP TAB1               ;Else, proceed to perform the TAB operation.
	
FOR5:      LLI 205                ;Load L with address of the FOR/NEXT STACK pointer
           LHI OLDPG27/400        ;** Load H with page of the FOR/NEXT STACK pntr
           LAM                    ;Fetch the stack pointer to the ACC.
           RLC                    ;Rotate it left to multiply by two, then rotate it again to
           RLC                    ;Multiply by four. Add this value to the base address
           ADI 136                ;Plus two of the base address to point to the next part of
           LEA                    ;The FOR/NEXT STACK. Place this value in register E.
           LDH                    ;Set D to the FORINEXT STACK area page.
           LLI 145                ;Load L with the address of the first character in the
           LHI OLDPG26/400        ;** AUX SYMBOL BUFFER and set up H to this page.
           LBI 002                ;Set up register B as a number of bytes to move counter.
           CAL MOVEIT             ;Move the variable name into the FOR/NEXT STACK.
           CAL STOSYM             ;Store initial variable value in the VARIABLES TABLE.
           JMP NXTLIN             ;Continue with next line in user program buffer.

PARSEP:    LLI 176                ;Load L with PARSER TOKEN storage location. Set
           LMI 000                ;The value indicating end of expression. Call the
           CAL PARSER             ;PARSER subroutine for final time for the expression.
           LLI 227                ;Change L to point to the ARITH STACK pointer.
           LHI OLDPG1/400         ;** Set H to the page of the ARITH STACK pointer.
           LAM                    ;Fetch the ARITH STACK pointer value.
           CPI 230                ;Should indicate only one value (answer) in stack.
           RTZ                    ;Exit with answer in FPACC if ARITH STACK is O.K.
           JMP SYNERR             ;Else have a syntax error!
	
SQRX:      LLI 014                ;Load L with address of FP TEMP registers
           LHI OLDPG1/400         ;** Set H to page of FP TEMP. Move contents of FPACC
           CAL FSTORE             ;[Argument of SQR(X)] into FP TEMP for storage.
           LLI 126                ;Load L with MSW of FPACC
           LAM                    ;Fetch the MSW into the accumulator
           NDA                    ;Check the sign of the number in the FPACC
           JTS SQRERR             ;If number negative, cannot take square root
           JTZ CFALSE             ;If number is zero, return with zero value in FPACC
           LLI 017                ;Load L with address of FP TEMP Exponent register
           LAM                    ;Fetch the Exponent value into the ACC
           NDA                    ;Check sign of the Fxponent
           JTS NEGEXP             ;If Exponent less than zero, process negative Exponent
           RAR                    ;If Exponent positive, rotate right to divide by two
           LBA                    ;And save the result in CPU register B
           LAI 000                ;Clear the accumulator without disturbing Carry bit
           RAL                    ;Rotate Carry bit into the ACC to save remainder
           LMA                    ;Store the remainder back in FP TEMP Exponent reg.
           JMP SQREXP             ;Jump to continue processing
NEGEXP:    LBA                    ;For negative Exponent, form two Is complement by
           XRA                    ;Placing the positive value in CPU register B, clearing
           SUB                    ;The accumulator, and then subtracting B from the ACC
           NDA                    ;Clear the Carry bit after the complementing operation
           RAR                    ;Rotate the value right to divide by two
           LBA                    ;Save the result in CPU register B
           LAI 000                ;Clear the accumulator without disturbing Carry bit
           ACA                    ;Add Carry bit to the accumulator as remainder
           LMA                    ;Store the remainder back in FP TEMP Exponent reg
           JTZ NOREMD             ;If remainder was zero skip ahead. If not, increment the
           INB                    ;Result of the divide by two ops to compen for negative
NOREMD:    XRA                    ;Clear the accumulator
           SUB                    ;Subtract the quotient of the divide by two op to
           LBA                    ;Form two's complement and save the result in register B
SQREXP:    LLI 013                ;Load L with address of TEMP register
           LMB                    ;Store Fxponent quotient from above ops in TEMP
           LLI 004                ;Load L with address of FP registers containing +1.0
           LEI 034                ;Load E with address of SQR APPROX working registers
           LDH                    ;Set D to same page as H
           LBI 004                ;Set up register B as a number of bytes to move counter
           CAL MOVEIT             ;Transfer value +1.0 into SQR APPROX registers
           CAL CFALSE             ;Now clear the FPACC registers
           LLI 044                ;Load L with address of LAST SQR APPROX temp regs.
           CAL FSTORE             ;Initialize the LAST SQR APPROX regs to value of zero
SQRLOP:    LLI 034                ;Load L with address of SQR APPROX working registers
           CAL FLOAD              ;Transfer SQR APPROX into the FPACC
           LLI 014                ;Load L with address of SQR ARG storage registers
           CAL OPLOAD             ;Transfer SQR ARG into the FPOP
           CAL FPDIV              ;Divde SQR ARG by SQR APPROX (Fon-n X/A)
           LLI 034                ;Load L with address of SQR APPROX registers
           CAL OPLOAD             ;Transfer SQR APPROX into the FPOP
           CAL FPADD              ;Add to form value (X/A + A)
           LLI 127                ;Load L with address of FPACC Exponent register
           LBM                    ;Fetch Exponent value into CPU register B
           DCB                    ;Subtract one to effectively divide FPACC by two
           LMB                    ;Restore to memory. (Now have ((X/A + A) /2)
           LLI 034                ;Load L with address of SQR APPROX registers
           CAL FSTORE             ;Store contents of FPACC as new SQR APPROX
           LLI 044                ;Load L with address of LAST SQR APPROX registers
           CAL OPLOAD             ;Transfer LAST SQR APPROX into the FPOP
           CAL FPSUB              ;Subtract (LAST SQR APPROX - SQR APPROX)
           LLI 127                ;Load L with address of FPACC Exponent
           LAM                    ;Fetch the Exponent into the accumulator
           CPI 367                ;See if difference less than 2 to the minus ninth

;;; The below is changed for PATCH 2
;;; following is the original code
;;;        JTS SQRCNV             ;If so, approximation has converged
;;; Now is the new line
	       JMP PATCH2
;;;;       DCL
;;;;       LAM
;;;;       NDA
;;;;       JTZ SQRCNV             ;THIS IS PATCH #2
SQR1:	   LLI 034                ;Else, load L with address of SQR APPROX
           LDH                    ;Set D to same page as H
           LEI 044                ;And E with address of LAST SQR APPROX
           LBI 004                ;Set up register B as a number of bytes to move counter
           CAL MOVEIT             ;Transfer SQR APPROX into LAST SQR APPROX
           JMP SQRLOP             ;Continue ops until approximation converges
SQRCNV:    LLI 013                ;Load L with address of TEMP register. Fetch the
           LAM                    ;Exponenent quotient store there into accumulator.
           LLI 037                ;Change L to point to SQR APPROX exponent.
           ADM                    ;Add SQR APPROX exponent to quotient value.
           LMA                    ;Store sum back in SQR APPROX Exponent register.
           LLI 034                ;Load L with address of SQR APPROX. Transfer the
           JMP FLOAD              ;SQR APPROX into FPACC as answer and exit.
SQRERR:    LAI 323                ;Load ASCII code for letter S into the accumulator.
           LCI 321                ;Load ASCII code for letter Q into CPU register C.
           JMP ERROR              ;Display the SQuare root (SQ) error message.

RNDX:      LLI 064                ;Load L with address of SEED storage registers
           LHI OLDPG1/400         ;** Set H to page for floating point working registers
           CAL FLOAD              ;Transfer SEED into the FPACC
           LLI 050                ;Load L with address of random constant A
           CAL OPLOAD             ;Transfer random constant A into the FPOP
           CAL FPMULT             ;Multiply to form (SEED * A)
           LLI 060                ;Load L with address of random constant C
           CAL OPLOAD             ;Transfer random constant C into the FPOP
           CAL FPADD              ;Add to fom (SEED * A) + C
           LLI 064                ;Load L with address of SEED storage registers
           CAL FSTORE             ;Store I (SEED * A) + C] in former SEED registers
           LLI 127                ;Load L with address of FPACC Exponent register
           LAM                    ;Fetch Exponent value into the accumulator
           SUI 020                ;Subtract 16 (decimal) to effectively divide by 65,536
           LMA                    ;Now FPACC = [((SEED * A) + C)/65,536]
           CAL FPFIX              ;Convert floating to fixed point to obtain integer part
           LLI 123                ;Load L with address of FPACC Extension register
           LMI 000                ;Clear the FPACC Extension register
           LLI 127                ;Load L with address of FPACC Exponent
           LMI 000                ;Clear the FPACC Exponent register
           CAL FPFLT              ;Fetch INT(((SEED * A) + C)/65,536) into the FPACC
           LLI 127                ;Load L with address of FPACC Exponent
           LAM                    ;Fetch FPACC Exponent into the accumulator
           ADI 020                ;Add 16 (decimal) to effectively multiply by 65,536
           LMA                    ;(65,536 * INT[ ((SEED * A) + C)/65,5361) in FPACC
           LLI 064                ;Load L with address of [(SEED * A) + C]
           CAL OPLOAD             ;Transfer it into FPOP. Subtract FPACC to form
           CAL FPSUB              ;[(SEED * A) + C] MOD 65,536
           LLI 064                ;Load L with address of former SEED registers
           CAL FSTORE             ;Store SEED MOD 65,536 in place of [(SEED * A) + Cl
           LLI 127                ;Load L with address of FPACC Exponent
           LAM                    ;Fetch FPACC Exponent into the ACC and subtract
           SUI 020                ;16 (decimal) to form (SEED MOD 65,536)/65,536
           LMA                    ;So that random number in FPACC is between
           RET                    ;0.0 and +1.0 and exit to calling routine

;;; following is PATCH 2
PATCH2:	  JTS SQRCNV
	      DCL
	      LAM
	      NDA
	      JTZ SQRCNV
	      JMP SQR1
       
;; OPTIONAL ARRAY ROUTINES
PRIGH1:    LLI 126                ;Load L with address of the MSW in the FPACC
           LHI OLDPG1/400         ;** Set H to page of FPACC
           LAM                    ;Fetch MSW of FPACC into the ACC.
           NDA                    ;Test to see if value in FPACC is positive.
           JTS OUTRNG             ;If not, go display error message.
           CAL FPFIX              ;If O.K. then convert floating point to fixed point
           LLI 124                ;Load L with address of LSAL of converted value
           LAM                    ;Fetch the LSW of the value into the ACC
           SUI 001                ;Subtract one from the value to establish proper
           RLC                    ;Origin for future ops. Now rotate the value twice
           RLC                    ;To effectively multiply by four. Save the
           LCA                    ;Calculated result in CPU register C
           LLI 203                ;Load L with address of F/A STACK TEMP
           LHI OLDPG27/400        ;** Load H with page of F/A STACK TEMP
           LAM                    ;Fetch the value into the accumulator
           XRI 377                ;Complement the value
           RLC                    ;Rotate the value twice to multiply by four (the number
           RLC                    ;Of bytes per entry in the ARRAY VARIABLES table).
           ADI 120                ;Add the starting address of the ARRAY VARIABLES
           LHI OLDPG27/400        ;** TABLE to forin pointer. Set page address in H.
           LLA                    ;Point to the name in the ARRAY VARIABLES
           INL                    ;Increment the pointer value twice to move over the
           INL                    ;Name in the table and point to starting address for the
           LAM                    ;Array values in the ARRAY VALUES table. Fetch this
           ADC                    ;Address to the ACC. Now add in the figure calculated
           LLA                    ;To reach desired subscripted data storage location. Set
           LHI OLDPG57/400        ;tt The pointer to that location. Load the floating point
           JMP FLOAD              ;Value stored there into the FPACC and exit to caller.

FUNAR2:    LLI 202                ;Load L with address of TEMP COUNTER
           LHI OLDPG27/400        ;** Load H with page of counter
           LBM                    ;Fetch the counter value
           INB                    ;Increment the value
           LMB                    ;Restore the value to memory
           LCI 002                ;Initialize register C to a value of two for future ops
           LLI 114                ;Load L with address of start of ARRAY VARIABLES
           LHI OLDPG27/400        ;** TABLE (less four). Set H to page of the table.
           CAL TABADR             ;Calculate address of start of next narne in table.
           LDI OLDPG26/400        ;** Load D with page of the SYMBOL BUFFER
           LEI 120                ;Set E to starting address of the SYMBOL BUFFER
           CAL STRCP              ;Compare name in ARRAY VARIABLES table to the
           JTZ FUNAR3             ;Contents of the SYMBOL BUFFER. If match, go set up
           LLI 202                ;Array token value. Else, reset L to address of TEMP
           LHI OLDPG27/400        ;** COUNTER. Set H to page of TEMP COUNTER.
           LAM                    ;Fetch the counter value into the accumulator.
           LLI 075                ;Change L to number of arrays storage location.
           CPM                    ;Compare number of entries checked against number
           JFZ FUNAR2             ;Possible. Keep searching table if not finished.
           JMP FAERR              ;If finished and no match than have F/A error condx.
FUNAR3:    LLI 202                ;Load L with address of TEMP COUNTER
           LHI OLDPG27/400        ;** Load H with page of counter.
           XRA                    ;Clear the accumulator. Subtract the value in the TEMP
           SBM                    ;COUNTER from zero to obtain two's complement.
           LMA                    ;Place this back in counter location as ARRAY TOKEN
           JMP FUNAR4             ;VALUE (negative). Go place the value on F/A STACK.
	
OUTRNG:    LAI 317                ;Load the ASCII code for letter 0 into the accumulator
           LCI 322                ;Load the ASCII code for letter R into register C
           JMP ERROR              ;Go display Out of Range (OR) error message.
	
ARRAY:     CAL RESTSY             ;Transfer contents of AUX SYMBOL BUFFER into the
           JMP ARRAY2             ;SYMBOL BUFFER. (Entry when have actual LET)
ARRAY1:    LLI 202                ;Load L with address of SCAN pointer
           JMP ARRAY3             ;Proceed to process. (Entry point for IMPLIED LET)
ARRAY2:    LLI 203                ;Load L with address of LET pointer
ARRAY3:    LHI OLDPG26/400        ;** Set H to pointer page
           LBM                    ;Fetch pointer to location where "(" found in statement
           INB                    ;Line. Increment it to point to next character in the line.
           LLI 276                ;Load L with address of EVAL pointer and load it with
           LMB                    ;The starting address for the EVAL routine
           LLI 206                ;Change L to address of ARRAY SETUP pointer
           LMB                    ;And also store address in that location
ARRAY4:    LLI 206                ;Load L with address of ARRAY SETUP pointer
           CAL GETCHR             ;Fetch character pointed to by ARRAY SETUP pntr
           CPI 251                ;See if character is ")" ? If so, then have located
           JTZ ARRAY5             ;End of the subscript. If not, reset
           LLI 206                ;to the ARRAY SETUP pointer. Increment the
           CAL LOOP               ;Pointer and test for the end of the statement line.
           JFZ ARRAY4             ;If not end of line, continue looking for right paren.
           LAI 301                ;If reach end of line before right parenthesis than load
           LCI 306                ;ASCII code for letters A and F and display message
           JMP ERROR              ;Indicating Array Forrnat (AF) error condition
ARRAY5:    LLI 206                ;Load L with address of ARRAY SETUP pointer
           LBM                    ;Fetch pointer (pointing to ")"sign) into register B
           DCB                    ;Decrement it to move back to end of subscript number
           LLI 277                ;Load L with address of EVAL FINISH pointer location
           LMB                    ;Place the pointer value in the EVAL FINISH pointer
           LLI 207                ;Load L with address of LOOP COUNTER
           LMI 000                ;Initialize LOOP COUNTER to value of zero
ARRAY6:    LLI 207                ;Load L with address of LOOP COUNTER
           LHI OLDPG26/400        ;** Load H with page of LOOP COUNTER
           LBM                    ;Fetch the counter value
           INB                    ;Increment it
           LMB                    ;Restore the counter value to memory
           LCI 002                ;Set up counter in register C for future ops
           LLI 114                ;Load L with address of start of ARRAY VARIABLES
           LHI OLDPG27/400        ;** Table less four). Set H to page of the table.
           CAL TABADR             ;Calculate the address of next entry in the table
           LEI 120                ;Load register E with starting address of SYMBOL BUFF
           LDI OLDPG26/400        ;** Set D to page of SYMBOL BUFFER
           CAL STRCP              ;Compare entry in table against contents of SYMBOL BF
           JTZ ARRAY7             ;If match, have found array naine in the table.
           LLI 207                ;Else, set L to address of the LOOP COUNTER
           LHI OLDPG26/400        ;** Set H to page of the LOOP COUNTER
           LAM                    ;Fetch the counter value to the ACC
           LLI 075                ;Change L to the counter containing number of arrays
           LHI OLDPG27/400        ;** Set H to the proper page
           CPM                    ;Compare number of arrays to count in LOOP CNTR
           JFZ ARRAY6             ;If more entries in the table, continue looking for match
           JMP FAERR              ;If no matching name in table then have an error condx.
ARRAY7:    CAL EVAL               ;Call subroutine to evaluate subscript expression
           CAL FPFIX              ;Convert the subscript value obtained to fixed forrnat
           LLI 207                ;Load L with address of LOOP COUNTER
           LHI OLDPG26/400        ;** Set H to page of the LOOP COUNTER
           LBM                    ;Fetch the value in the LOOP COUNTER into the ACC
           LCI 002                ;Set up counter in register C future ops
           LLI 114                ;Load L with address of ARRAY VARIABLES
           LHI OLDPG27/400        ;** Table less four). Set H to page of the table.
           CAL TABADR             ;Calculate the address of entry in the table
           INL                    ;Advance the ARRAY VARIABLES table pointer twice
           INL                    ;To advance pointer over array name.
           LCM                    ;Fetch array base address in ARRAY VALUES table
           LLI 124                ;Load L with address of subscript value
           LHI OLDPG1/400         ;** Set H to page of subscript value
           LAM                    ;Fetch the subscript value into the accumulator
           SUI 001                ;Subtract one from subscript value to allow for zero
           RLC                    ;Origin. Now multiply by four
           RLC                    ;Using rotates (number of bytes required for each entry
           ADC                    ;In the ARRAY VALUES table). Add in base address to
           LLI 204                ;The calculated value to form final address in the
           LHI OLDPG27/400        ;** ARRAY VALUES table. Now set H & L to TEMP
           LMA                    ;ARRAY ELEMENT storage location & store the addr.
           LLI 201                ;Change L to point to ARRAY FLAG
           LMI 377                ;Set the ARRAY FLAG for future use
           RET                    ;Exit to calling routine

DIM:       CAL CLESYM             ;Initialize the SYMBOL BUFFER to cleared condition
           LLI 202                ;Load L with address of SCAN pointer
           LBM                    ;Fetch SCAN pointer value into register B
           INB                    ;Add one to the SCAN pointer value
           LLI 203                ;Change L to DIM pointer (formerly TOKEN) storage
           LMB                    ;Store the updated SCAN pointer as the DIM pointer
DIM1:      LLI 203                ;Load L with the address of DIM pointer storage location
           CAL GETCHR             ;Fetch a character from the line input buffer
           JTZ DIM2               ;If character fetched is a space, ignore it
           CPI 250                ;Else see if character is "(" left parenthesis
           JTZ DIM3               ;If so, should have ARRAY VARIABLE naine in buffer
           CAL CONCTS             ;If not, append the character to the SYMBOL BUFFER
DIM2:      LLI 203                ;Load L with the address of DIM pointer stomge location
           CAL LOOP               ;Increment the pointer and see if end of line
           JFZ DIM1               ;If not end of line, fetch next character
           JMP DIMERR             ;Else have a DIMension error condition
DIM3:      LLI 206                ;Load L with address of ARRAY pointer storage loc
           LMI 000                ;Initialize ARRAY pointer to starting value of zero
DIM4:      LLI 206                ;Load L with address of ARRAY pointer storage loc
           LHI OLDPG26/400        ;** Set H to page of ARRAY pointer storage location
           LAM                    ;Fetch value in ARRAY pointer to ACC (effectively
           RLC                    ;Represents number of arrays defined in pgm). Rotate
           RLC                    ;Left twice to multiply by four (niunber of bytes per
           ADI 114                ;entry in ARRAY VARIABLES table). Add to base
           LHI OLDPG27/400        ;** Address to form pointer to ARRAY VARIA.BLES
           LLA                    ;Table and set up H & L as the memory pointer.
           LEI 120                ;Load E with starting address of the SYMBOL BUFFER
           LDI OLDPG26/400        ;** Load D with the page address of the SYMBOL BUFF
           CAL STRCP              ;Compare contents of SYMBOL BF to entry in ARRAY
           JTZ DIM9               ;VARIABLES table. If same, have duplicate array name.
           LLI 206                ;Else, load L with address of ARRAY pointer storage
           LHI OLDPG26/400        ;** Load H with page of ARRAY pointer storage
           LBM                    ;Fetch the ARRAY pointer value to register B
           INB                    ;Increment the value
           LMB                    ;Restore it to ARRAY pointer storage location
           LLI 075                ;Change L to number of arrays storage location
           LHI OLDPG27/400        ;** Set H to page of the number of arrays stomge loc
           LAM                    ;Fetch the number of arrays value to the ACC
           DCB                    ;Restore B to previous count
           CPB                    ;Compare number of arrays tested against nr defined
           JFZ DIM4               ;If not equal, continue searching ARRAY VARIABLES
           LLI 075                ;Table. When table searched with no match, then must
           LHI OLDPG27/400        ;** Append naine to table. First set pointer to number
           LBM                    ;Of arrays storage location. Fetch that value and
           INB                    ;Add one to account for new name being added.
           LMB                    ;Restore the updated value back to memory.
           LLI 076                ;Change pointer to ARRAY TEMP pointer storage
           LMB                    ;Store pointer to current array in ARRAY TEMP too.
           LLI 206                ;Load L with address of ARRAY pointer stomge loc.
           LHI OLDPG26/400        ;** Set H to page of ARRAY pointer storage location
           LMB                    ;And update it also for new array being added.
           LAM                    ;Fetch the current ARRAY pointer value to the ACC
           RLC                    ;Multiply it times four by performing two rotate left
           RLC                    ;Operations and add it to base value to form address in
           ADI 114                ;The ARRAY VARIABLES table. Place the low part
           LEA                    ;Of this calculated address value into register E.
           LDI OLDPG27/400        ;** Set register D to the page of the table.
           LLI 120                ;Load L with the start of the SYMBOL BUFFER
           LHI OLDPG26/400        ;** Load H with the page of the SYMBOL BUFFER
           CAL MOVEC              ;Move the array name from the SYMBOL BUFFER to
           CAL CLESYM             ;The ARRAY VARIABLES table. Then clear the
           LLI 203                ;SYMBOL BUFFER. Reset L to the DIM pointer storage
           LHI OLDPG26/400        ;** Location. Set H to the DIM pointer page.
           LBM                    ;Fetch the pointer value (points to "(" part of DIM
           INB                    ;Statement). Increment the pointer to next character in
           LLI 204                ;The line input buffer. Cbange L to DIMEN pointer.
           LMB                    ;Store the updated DIM pointer in DIMEN storage loc.
DIM5:      LLI 204                ;Set L to DIMEN pointer storage location
           CAL GETCHR             ;Fetch character in line input buffer
           JTZ DIM6               ;Ignore character for space
           CPI 251                ;If not space, see if character is right parenthesis
           JTZ DIM7               ;If yes, process DIMension size (array length)
           CPI 260                ;If not, see if character is a valid decimal number
           JTS DIMERR             ;If not valid number, have DIMension error condition
           CPI 272                ;Continue testing for valid decitnal number
           JFS DIMERR             ;If not valid number, then DIMension error condition
           CAL CONCTS             ;If valid decirnal number, append digit to SYMBOL BF
DIM6:      LLI 204                ;Set L to DIMEN pointer storage location
           CAL LOOP               ;Advance the pointer value and check for end of the line
           JFZ DIM5               ;If not end of line, continue fetching DIMension size
           JMP DIMERR             ;If end of line before right parenthesis, have error condx.
DIM7:      LLI 120                ;Load L with address of start of SYMBOL BUFFER
           LHI OLDPG26/400        ;** Load H with page of SYMBOL BUFFER. (Now
           CAL DINPUT             ;Contains DIMension size.) Convert buffer to floating
           CAL FPFIX              ;Point number and then reformat to fixed point.
           LLI 124                ;Load L with address of LSW of fixed point number
           LAM                    ; And fetch the low order byte of the nr into the ACC
           RLC                    ;Rotate it left two tirnes to multiply it by four (the
           RLC                    ;Number of bytes required to store a floating point nr).
           LCA                    ;Store this value in CPU register C temporarily
           LLI 076                ;Set L to ARRAY TEMP storage location.
           LHI OLDPG27/400        ;** Set H to ARRAY TEMP pointer page.
           LAM                    ;Fetch the value in ARRAY TEMP (points to ARRAY
           SUI 001                ;VARIABLES table). Subtract one from the pointer
           RLC                    ;Value and multiply the result by four using rotate left
           RLC                    ;Instructions. Add this value to a base address
           ADI 122                ;(Augmented by two) to point to ARRAY VALUES
           LLA                    ;Pointer storage location in the ARRAY VARIABLES
           LHI OLDPG27/400        ;Table and set the pointer up in registers H & L.
           LBM                    ;Fetch the starting address in the ARRAY VALUES
           ADI 004                ;Table for the previous array into register B. Now add
           LLA                    ;Four to the ARRAY VARIABLES table pointer to
           LAB                    ;Point to curront ARRAY VALUES starting address.
           ADC                    ;Add the previous array starting address plus number of
           LMA                    ;Bytes required and store as starting loc for next array
DIM8:      LLI 204                ;Set L to address of DIMEN pointer storage location
           LHI OLDPG26/400        ;** Set H to page of DIMEN pointer
           LBM                    ;Fetch pointer value (points to ") " in line)
           LLI 203                ;Change L to DIM pointer storage location
           LMB                    ;Store former DIMEN value back in DIM pointer
DIM9:      LLI 203                ;Load L with address of DIM pointer storage location
           CAL GETCHR             ;Fetch a character from the line input buffer
           CPI 254                ;See if character is a comma (,) sign
           JTZ DIM10              ;If yes, have another array being defined on the line
           LLI 203                ;If not, reset L to the DIM pointer
           CAL LOOP               ;Increment the pointer and see if end of the line
           JFZ DIM9               ;If not end of the line, keep looking for a comma
           JMP NXTLIN             ;Else exit the DIM statement routine to continue pgm
DIM10:     LLI 203                ;Set L to DIM pointer storage location
           LBM                    ;Fetch pointer value (points to comma sign just found)
           LLI 202                ;Load L with address of SCAN pointer storage location
           LMB                    ;Place DIM pointer into the-SCAN pointer
           JMP DIM                ;Continue processing DIM statement line for next array
DIMERR:    LAI 304                ;On error condition, load ASCII code for letter D in ACC
           LCI 305                ;And ASCII code for letter E in CPU register C
           JMP ERROR              ;Go display the Dirnension Error (DE) message.

;;; no user defined functions yet, stop here if we see one.
UDEFX:	   HLT

save:	
load:	   JMP EXEC		            ; By default, save and load isn't implemented.

;this page gets copied from EPROM to RAM at 2000H as OLDPG1           
            ORG 1D00H
page1:      DB 000,000,000,000
            DB 000,000,100,001	        ; STORES FLOATING POINT CONSTANT +1.0
            DB 000,000,000
            DB 000		                ; EXPONENT COUNTER
            DB 000,000,000,000	        ; STORES FLOATING POINT NUMBER TEMPORARILLY
            DB 000,000,000,000
            DB 000,000,300,001	        ; STORES FLOATING POINT CONSTANT -1.0
            DB 000,000,000,000	        ; SCRATCH PAD AREA (16 BYTES)
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000,000,000
            DB 001,120,162,002	        ; STORES RANDOM NUMBER GENERATOR CONSTANT VALUE
            DB 000,000,000,000
            DB 003,150,157,014	        ; STORES RANDOM NUMBER GENERATOR CONSTANT VALUE
            DB 000,000,000,000	        ; SCRATCH PAD AREA (12 BYTES) (01 064-077)
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000		            ; SIGN INDICATOR
            DB 000			            ; BITS COUNTER
            DB 000,000		            ; SIGN INDICATOR
            DB 000		                ; INPUT DIGIT COUNTER
            DB 000		                ; TEMP STORAGE
            DB 000		                ; OUTPUT DIGIT COUNTER
            DB 000 		                ; FP MODE INDICATOR
            DB 000,000,000,000,000,000,000  ; NOT ASSIGNED
            DB 000,000,000,000	        ; FPACC EXTENSION
            DB 000,000,000,000	        ; FPACC LSW, NSW, MSW, EXPONENT
            DB 000,000,000,000	        ; FPOP  Extension
            DB 000,000,000,000	        ; FPOP  LSW, NSW, MSW, EXPONENT
            DB 000,000,000,000	        ; FLOATING POINT WORKING AREA
            DB 000,000,000,000	        
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000,000,000
            DB 000,000,000,000,000,000,000,000  ; 8 Bytes NOT ASSIGNED
            DB 000,000,000,000	        ; TEMPORARY REGISTER STORAGE AREA (D,E,H&L)
            DB 000,000,000,000          ;  NOT ASSIGNED
            DB 000,000,120,004	        ; STORES FLOATING POINT CONSTANT +10.0
            DB 147,146,146,375	        ; STORES FLOATING POINT CONSTANT +0.1
            DB 000			            ; GETINP COUNTER
            DB 000,000,000,000,000,000  ; NOT ASSIGNED 
            DB 000			            ; ARITHMETIC STACK POINTER (01 227)
            DB 000			            ; ARITHMETIC STACK (NOT CLEAR HOW LONG)
            db 21h dup 0
            DB 004			            ; CC FOR SAVE
            DB 'S'+200
            DB 'A'+200
            DB 'V'+200
            DB 'E'+200
            DB 004			            ; CC FOR LOAD
            DB 'L'+200
            DB 'O'+200
            DB 'A'+200
            DB 'D'+200
            DB 000,000,000,000	        ; UNCLEAR WHAT THIS IS (01 304-01 317) ZEROS
            DB 000,000,000,000	        ; (PROBABLY STEP, FOR/NEXT, AND ARRAY PTR TEMP)
            DB 000,000,000,000
            DB 4
            DB 'T'+200
            DB 'H'+200
            DB 'E'+200
            DB 'N'+200
            DB 2
            DB 'T'+200
            DB 'O'+200
            DB 4
            DB 'S'+200
            DB 'T'+200
            DB 'E'+200
            DB 'P'+200
            DB 4
            DB 'L'+200
            DB 'I'+200
            DB 'S'+200
            DB 'T'+200
            DB 3
            DB 'R'+200
            DB 'U'+200
            DB 'N'+200
            DB 3
            DB 'S'+200
            DB 'C'+200
            DB 'R'+200
            DB 013		                ; CC FOR "READY" MESSAGE
            DB 224,215,212	            ; CTRL-T, CARRIAGE RETURN, LINE FEED
            DB 'R'+200
            DB 'E'+200
            DB 'A'+200
            DB 'D'+200
            DB 'Y'+200
            DB 215,212,212	            ; CARRIAGE RETURN, LINE FEED, LINE FEED;
            DB 011
            DB ' '+200
            DB 'A'+200
            DB 'T'+200
            DB ' '+200
            DB 'L'+200
            DB 'I'+200
            DB 'N'+200
            DB 'E'+200
            DB ' '+200        
            
;this page gets copied from EPROM to RAM at 2100H as OLDPG26            
           ORG 1E00H
page26:    DB 000			    ; CC FOR INPUT LINE BUFFER
           DB 117 dup 0 		; 79 Bytes THE INPUT LINE BUFFER
	       DB 000,000,000,000	; THESE ARE SYMBOL BUFFER STORAGE
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000	; THESE LOCATIONS ARE AUXILIARY SYMBOL BUFFER
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000
	       DB 000		        ; TEMP SCAN STORAGE REGISTER
	       DB 000		        ; TAB FLAG
	       DB 000		        ; EVAL CURRENT TEMP REG.
	       DB 000		        ; SYNTAX LINE NUMBER
	       DB 000		        ; SCAN TEMPORARY REGISTER
	       DB 000		        ; STATEMENT TOKEN
	       DB 000,000		    ; TEMPORARY WORKING REGISTERS
	       DB 000,000		    ; ARRAY POINTERS
	       DB 000		        ; OPERATOR STACK POINTER
	       DB 17 dup 0	        ; 15 Bytes OPERATOR STACK
	       DB 000		        ; FUN/ARRAY STACK POINTER
	       DB 7 dup 0	        ; FUNCTION/ARRAY STACK

	       ;; HEIRARCHY TABLE (FOR OUT OF STACK OPS) USED BY PARSER ROUTINE.
	       DB 000		        ; EOS
	       DB 003		        ; PLUS SIGN
	       DB 003		        ; MINUS SIGN
	       DB 004		        ; MULTIPLICATION SIGN
	       DB 004		        ; DIVISION SIGN
	       DB 005		        ; EXPONENT SIGN
	       DB 006		        ; LEFT PARENTHESIS
	       DB 001		        ; RIGHT PARENTHESIS
	       DB 002		        ; NOT ASSIGNED
	       DB 002		        ; LESS THAN SIGN
	       DB 002		        ; Equal sign
	       DB 002		        ; GREATER THAN SIGN
	       DB 002		        ; LESS THAN OR EQUAL COMBO
	       DB 002		        ; EQUAL OR GREATER THAN
	       DB 002		        ; LESS THAN OR GREATER THAN

	       ;; HEIRARCHY TABLE (FOR INTO STACK OPS)
	       ;; USED BY PARSER ROUTINE.
	       DB 000		        ; EOS
	       DB 003		        ; PLUS SIGN
	       DB 003		        ; MINUS SIGN
	       DB 004		        ; MULTIPLICATION SIGN
	       DB 004		        ; DIVISION SIGN
	       DB 005		        ; EXPONENTIATION SIGN
	       DB 001		        ; LEFT PARENTHESIS
	       DB 001		        ; RIGHT PARENTHESIS
	       DB 002		        ; NOT ASSIGNED
	       DB 002		        ; LESS THAN SIGN
	       DB 002		        ; EQUAL SIGN
	       DB 002		        ; GREATER THAN SIGN
	       DB 002		        ; LESS THAN OR EQUAL SIGN
	       DB 002		        ; EQUAL TO OR GREATER THAN
	       DB 002		        ; LESS THAN OR GREATER THAN
	       DB 000		        ; EVAL START POINTER
	       DB 000		        ; EVAL FINISH POINTER

	       ;; FUNCTION NAMES TABLE
	       DB 3
	       DB 'I'+200
	       DB 'N'+200
	       DB 'T'+200
	       DB 3
	       DB 'S'+200
	       DB 'G'+200
	       DB 'N'+200
	       DB 3
	       DB 'A'+200
	       DB 'B'+200
	       DB 'S'+200
	       DB 3
	       DB 'S'+200
	       DB 'Q'+200
	       DB 'R'+200
	       DB 3
	       DB 'T'+200
	       DB 'A'+200
	       DB 'B'+200
	       DB 3
	       DB 'R'+200
	       DB 'N'+200
	       DB 'D'+200
	       DB 3
	       DB 'C'+200
	       DB 'H'+200
	       DB 'R'+200
	       DB 3
	       DB 'U'+200
	       DB 'D'+200
	       DB 'F'+200
	       DB 000,000,000,000	; LINE NUMBER BUFFER STORAGE
	       DB 000,000,000,000
	       DB 000,000,000,000	; AUX LINE NUMBER BUFFER
	       DB 000,000,000,000
          
          ;;; The following DB is a change in page 3 of Scelbal update issue 4
          ;;; which apparently makes the "INSERT" command work correctly, the
          ;;; first time (later SCR commands load 33 into this spot) 
	       DB 033 		        ; USER PGM LINE PTR (PG)
	       DB 000 		        ; USER PGM LINE PTR (LOW)
	       DB 000 		        ; AUX PGM LINE PTR (PG)
	       DB 000 		        ; AUX PGM LINE PTR (LOW)
	       DB 000 		        ; END OF USER PGM BUFFER PTR (PG)
	       DB 000 		        ; END OF USER PGM BUFFER PTR (LOW)
	       DB 000		        ; PARENTHESIS COUNTER (366)
	       DB 000		        ; QUOTE INDICATOR
	       DB 000		        ; TABLE COUNTER (370)
           db 0,0,0,0,0,0,0
           
;this page gets copied from EPROM to RAM at 2200H as OLDPG27           
	       ORG 1F00H
page27:    DB 3
	       DB 'R'+200
	       DB 'E'+200
	       DB 'M'+200
	       DB 2
	       DB 'I'+200
	       DB 'F'+200
	       DB 3
	       DB 'L'+200
	       DB 'E'+200
	       DB 'T'+200
	       DB 4
	       DB 'G'+200
	       DB 'O'+200
	       DB 'T'+200
	       DB 'O'+200
	       DB 5
	       DB 'P'+200
	       DB 'R'+200
	       DB 'I'+200
	       DB 'N'+200
	       DB 'T'+200
	       DB 5
	       DB 'I'+200
	       DB 'N'+200
	       DB 'P'+200
	       DB 'U'+200
	       DB 'T'+200
	       DB 3
	       DB 'F'+200
	       DB 'O'+200
	       DB 'R'+200
	       DB 4
	       DB 'N'+200
	       DB 'E'+200
	       DB 'X'+200
	       DB 'T'+200
	       DB 5
	       DB 'G'+200
	       DB 'O'+200
	       DB 'S'+200
	       DB 'U'+200
	       DB 'B'+200
	       DB 6
	       DB 'R'+200
	       DB 'E'+200
	       DB 'T'+200
	       DB 'U'+200
	       DB 'R'+200
	       DB 'N'+200
	       DB 3
	       DB 'D'+200
	       DB 'I'+200
	       DB 'M'+200
	       DB 3
	       DB 'E'+200
	       DB 'N'+200
	       DB 'D'+200
	       DB 0			        ; END OF TABLE

	       DB 000			    ; GOSUB STACK POINTER
	       DB 000		        ; NOT ASSIGNED;
	       DB 000			    ; NUMBER OF ARRAYS COUNTER
	       DB 000			    ; ARRAY POINTER
	       DB 000			    
	       DB 000,000,000,000	; USED AS THE GOSUB STACK
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000	; USED AS ARRAY VARIABLES TABLE
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000

	       DB 000,000,000,000	; USED FOR FOR/NEXT STACK STORAGE
	       DB 000,000,000,000	; SHOULD BE 140 TO 177
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000,000,000,000
	       DB 000		        ; FOR/NEXT STACK POINTER
	       DB 000		        ; ARRAY/VARIABLE FLAG
	       DB 000  		        ; STOSYM COUNTER
	       DB 000		        ; FUN/ARRAY STACK POINTER (203
	       DB 000		        ; ARRAY VALUES POINTER
	       DB 3 dup 0	        ; NOT USED (SHOULD BE 205-207)
	       DB 000		        ; USED AS VARIABLES SYMBOL TABLE
	       DB 167 dup 0	        ; 119 Bytes (SHOULD BE 211-377 RESERVED)
           
           radix 10
           cpu 8008new         ; new 8008 mnemonics           
        
