            PAGE 0             ; suppress page headings in ASW listing file
;---------------------------------------------------------------------------------------------------------------------------------
; Copyright 2022 Jim Loos
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sub-license, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do
; so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
; IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;---------------------------------------------------------------------------------------------------------------------------------
            
;------------------------------------------------------------------------
; Serial Monitor for 8008 SBC
;
; Commands:
;   B - Binary file download (no error checking)
;   C - Call subroutine
;   D - Dump memory
;   E - Examine/modify memory
;   F - Fill memory
;   G - Goto address
;   H - Intel Hex file download
;   I - Input from port
;   J - Jump to address
;   O - Output to port
;   S - SCELBAL
;
; V2.0 uses 16K of the 32K 27256 EPROM. the Serial Monitor occupies the first 8K of the EPROM. 
; the Scelbi BASIC Interpreter (SCELBAL) occupies the second 8K of the EPROM. when reset, the SBC
; starts executing code from the first 8K of EPROM (Monitor). when 'S' is entered at the Monitor's 
; prompt, the SBC 'bank switches' to select the second 8K of the 27256 EPROM for execution (SCELBAL).
;
; assemble both 'monitor.asm' and 'scelbal-in-eprom.asm' with the AS Macro Assembler 
; (http://john.ccac.rwth-aachen.de:8000/as/). use the assembler's P2BIN utility to generate two 
; binary files. concatenate the resulting two binary files into one file for your EPROM programmer.
; one way is to use the DOS 'COPY' command: 'copy /b monitor.bin + scelbal-in-eprom.bin eprom.bin'.
; I prefer to use the concatenate function in the Freeware Hex Editor 'HxD' by Maël Hörz
; (https://mh-nexus.de/en/hxd/)
;
; serial I/O at 2400 bps N-8-1  
;------------------------------------------------------------------------            

            include "bitfuncs.inc" 

            cpu 8008new             ; use "new" 8008 mnemonics
            
; temporary storage for registers;            
save_H:     equ 1FF0H
save_L:     equ 1FF1H
save_B:     equ 1FF2H
save_C:     equ 1FF3H
save_D:     equ 1FF4H
save_E:     equ 1FF5H
esccount    equ 1FF6H
jmp_addr:   equ 1FFCH

ESCAPE      equ 1BH
RETURN      equ 0DH

; when the reset pushbutton is pressed, the flip-flop is set which generates an interrupt
; and clears the address latches. thus, the first instruction is thus always fetched from 
; address 0. the instruction at address 0 must be a single byte transfer instruction in 
; order to set the program counter. i.e., it must be one of the RST opcodes.
            org 2000H                   ; start of EPROM
            rst 1

            org 2008H                   ; rst 1 jumps here
            jmp start
            
start:      xra a                       ; clear A
            out 10                      ; clear the EPROM bank switch address outputs A13 and A14
            out 09                      ; turn off orange LEDs            
            mvi a,1
            out 08                      ; set serial output high (mark)
            in 1                        ; reset the bootstrap flip-flop internal to GAL22V10 #2
         
; perform a checksum test of the EPROM by summing the contents of EPROM locations 2000-3FFFH.
; assuming that the P2BIN utility was run with the 's' option, the sum of all EPROM locations, if
; read correctly, should be zero.
            ;xra a                       ; clear A
            ;mvi h,20H                   ; first EPROM address
            ;mvi l,00H
romcksum:   ;add m                       ; add the EPROM contents to A
            ;inr l                       ; next EPROM address
            ;jnz romcksum                ; loop back if not finished with this page of the EPROM
            ;inr h                       ; else, next page of the EPROM
            ;mov b,a                     ; save the sum in A to B temporarily
            ;mov a,h
            ;cpi 40H                     ; has H reached 40H? (the end of the EPROM)
            ;mov a,b                     ; restore the sum to A from B
            ;jnz romcksum                ; loop back if not at the end of the EPROM
            ;ora a                       ; set flags
            ;jz romcksumOK               ; the sum in A should be zero, jump if the EPROM checksum is OK
            ;mvi h,hi(epromtxt)          ; else, print the error message and then halt
            ;mvi l,lo(epromtxt) 
            ;call puts         
            ;hlt
            
romcksumOK: mvi h,hi(esccount)          ; clear the escape key counter
            mvi l,lo(esccount) 
            mvi m,0
            
            mvi h,hi(titletxt)          ; display the title
            mvi l,lo(titletxt) 
            call puts     
            
menu:       mvi h,hi(menutxt)           ; display the menu
            mvi l,lo(menutxt) 
            call puts
            
prompt:     mvi h,hi(prompttxt)         ; prompt for input
            mvi l,lo(prompttxt) 
            call puts            
prompt0:    call getch                  ; get input command from user
            cpi ':'
            jz hexdl1a                  ; hex file download started
            cpi ESCAPE                  ; is the input the escape key?
            jnz prompt1                 ; nope
            mvi h,hi(esccount)  
            mvi l,lo(esccount) 
            mov b,m
            inr b                       ; yes, increment the escape key count
            mov m,b
            jmp prompt0                 ; go back for more imput     
prompt1:    cpi '?'                     ; is the input '?'
            jnz prompt2                 ; nope 
            mvi h,hi(esccount)  
            mvi l,lo(esccount) 
            mov a,m
            mvi m,0            
            cpi 2                       ; was the escape key pressed twice in succession?
            jnz menu                    ; nope, display the menu
            mvi h,hi(copytxt)           ; escape, escape followed by '?' displays the copyright notice
            mvi l,lo(copytxt) 
            call puts
            jmp prompt

prompt2:    mvi h,hi(esccount)  
            mvi l,lo(esccount) 
            mvi m,0
            cpi 'a'                     ; is the input character below 'a'?
            jc $+5                      ; skip the next instruction if the character is already upper case
            sui 20H                     ; else, convert to the character to upper case
            call putch                  ; echo the character
            cpi 'B'
            jz bindl                    ; binary file download
            cpi 'C'
            jz callsub                  ; call subroutine
            cpi 'D'
            jz dump                     ; dump memory
            cpi 'E'
            jz examine                  ; examine/modify memory
            cpi 'F'
            jz fill                     ; fill memory
            cpi 'G'
            jz goto                     ; goto address
            cpi 'H'
            jz hexdl                    ; Intel hex file download
            cpi 'I'
            jz input                    ; input from port
            cpi 'J'
            jz jump                     ; jump to address
            cpi 'O'
            jz output                   ; output to port
            cpi 'S'
            jz scelbal                  ; jump to SCELBAL
            cpi 0DH
            jz menu                     ; display the menu
            mvi a,'?'
            call putch                  ; whaaat??
            jmp prompt
            
;------------------------------------------------------------------------
; dump a page of memory in hex and ascii
; space key aborts display.
;------------------------------------------------------------------------
dump:       mvi h,hi(dumptxt)
            mvi l,lo(dumptxt)
            call puts     
            call get_addr               ; get the four digit address
            jc prompt                   ; exit prematurely if space, enter or escape
            mvi h,hi(columntxt)
            mvi l,lo(columntxt)
            call puts     
            mvi h,hi(save_H)
            mvi l,lo(save_H)            
            mov b,m                     ; move the high byte of the address into B
            inr l
            mov a,m                     ; move the high byte of the address into A
            ani 0F0H                    ; start on a 16 byte boundry            
            mov l,a                     ; move it to L
            mov h,b
dump2:      call crlf                   ; start on a new line
            mov a,h
            call write_hex              ; write the high byte of the address
            mov a,l
            call write_hex              ; write the low byte of the address
            call space
            
            ; write one line of 16 bytes in hex and then in ascii
dump3:      in 0
            rar
            jnc prompt                  ; abort if start bit detected
            mov a,m                     ; retrieve the byte from memory
            call write_hex              ; write it as two hex digits
            call space
            inr l
            mov a,l
            ani 0FH                     ; 16 bytes?
            jz dump4                    ; move on to print ascii characters
            jmp dump3                   ; otherwise, next address
            
            ; ascii characters
dump4:      call space
            in 0
            rar
            jnc prompt                  ; abort if start bit detected
            mov a,l                     
            sui 16
            mov l,a                     ; back to the starting address for this line
dump5:      mov a,m                     ; retrieve the byte from memory
            cpi 32                      ; control character?
            jc dump6                    ; jump if the byte from memory is < 32
            cpi 128                     ; extended ascii?
            jc dump7                    ; jump if the byte from memory is < 128
dump6:      mvi a,'.'                   ; print '.' for non-printable sacii
dump7:      call putch
            in 0
            rar
            jnc prompt
            inr l                       ; next address
            jz prompt                   ; exit if finished with this page
            mov a,l                     ; next address
            ani 0FH                     ; 16 bytes?
            jz dump2                    ; jump if end of line
            jmp dump5                   ; otherwise, next memory address
            
;------------------------------------------------------------------------
; fill a block of memory with a value
;------------------------------------------------------------------------
fill:       mvi h,hi(filltxt)
            mvi l,lo(filltxt)
            call puts
            call get_addr               ; get the four digit address
            jc prompt                   ; exit prematurely if space, enter or escape            
            call get_count              ; get the four digit count
            jc prompt                   ; exit prematurely if space, enter or escape            
            mvi h,hi(valuetxt)
            mvi l,lo(valuetxt)
            call puts                   ; prompt for the value
            call get_two                ; get the value used to fill in A
            mvi h,hi(save_B)
            mvi l,lo(save_B)
            mov b,m                     ; retrieve the count high byte from memory
            inr l
            mov c,m                     ; retrieve the count low byte from memory
            mvi h,hi(save_H)
            mvi l,lo(save_H)
            mov d,m                     ; retrieve the address high byte from memory
            inr l
            mov e,m                     ; retrieve the address low byte from memory
            mov h,d                     ; get the address high byte from D
            mov l,e                     ; get the address low byte from E
fillloop:   mov m,a                     ; save the value in memory
            inr l                       ; increment L
            jnz fillloop1
            inr h                       ; increment H
fillloop1:  call decBC
            mov d,a                     ; save the fill byte in D
            mov a,c                     ; get the count low byte 
            ora b                       ; OR with the count high byte
            mov a,d                     ; restore the fill byte from D
            jnz fillloop
            jmp prompt

;------------------------------------------------------------------------
; examine/modify memory.
; space increments memory pointer without affecting value.
; enter or escape exits.
;------------------------------------------------------------------------        
examine:    mvi h,hi(examinetxt)  
            mvi l,lo(examinetxt) 
            call puts 
            call get_addr               ; get the four digit address
            jc prompt                   ; exit prematurely if space, enter or escape            
            call crlf
            mvi h,hi(save_H)
            mvi l,lo(save_H)
            mov d,m                     ; retrieve the address high byte from memory
            inr l
            mov e,m                     ; retrieve the address low byte from memory
            mov h,d                     ; get the address high byte from D
            mov l,e                     ; get the address low byte from E
examine1:   call crlf
            mov a,h
            call write_hex              ; high byte of the address
            mov a,l
            call write_hex              ; low byte of the address
            call save_HL
            mvi h,hi(arrowtxt)
            mvi l,lo(arrowtxt)
            call puts    
            call restore_HL
            mov a,m
            call write_hex              ; value stored at memory
            call save_HL
            mvi h,hi(newvaluetxt)
            mvi l,lo(newvaluetxt)
            call puts    
            call restore_HL
examine3:   call get_two                ; two hex digits
            jc examine4                 ; jump if space, enter or escape
            mov m,a                     ; else save the new value in memory at this address
            inr l                       ; next address
            jnz examine1
            inr h
            jmp examine1
examine4:   cpi ' '                     ; space?
            jnz examine5
            inr l                       ; next address
            jnz examine1
            inr h
            jmp examine1
examine5:   cpi 0DH                     ; enter?
            jz  prompt
            cpi 1BH                     ; escape?
            jz  prompt
            jmp examine3
            
;------------------------------------------------------------------------
; load a binary file into memory using the Tera Term "Send file" function.
; use this function cautiously as there is, of course, no error checking.
; when using the Tera Term "Send file" function, make sure that:
;   1. the serial port transmit delay is set to at least 2 msec/char
;   2. the "Binary" option check box on the Send File dialog box is checked.
; the download is assumed to be finished when no characters have been received
; from Tera Term for 3 seconds.
; uses BC as the "idle" counter.
;------------------------------------------------------------------------            
bindl:      mvi h,hi(binloadtxt)  
            mvi l,lo(binloadtxt) 
            call puts 
            
            call get_addr               ; get the four digit hex load address
            jc prompt                   ; exit prematurely if space, enter or escape            

            mvi h,hi(dnldtxt)
            mvi l,lo(dnldtxt)
            call puts                   ; prompt for download
           
            mvi h,hi(save_H)
            mvi l,lo(save_H)
            mov d,m                     ; retrieve the start address high byte from memory into D
            inr l
            mov e,m                     ; retrieve the start address low byte from memory into E
            mov h,d                     ; get the start address high byte into H
            mov l,e                     ; get the start address low byte into L

            call getch                  ; get the first byte of the file from the serial port
            mov m,a                     ; write the first byte to memory
            inr l                       ; increment the low byte of the address pointer
            jnz bindl0                  ; go get next byte
            inr h                       ; increment the high byte of the address pointer

bindl0:     mvi b,40H                   ; initialize "idle" counter (BC) to 16284
            mvi c,0
bindl1:     in 0                        ; get input from the serial port
            rar                         ; rotate the received serial bit right into carry
            jnc bindl2                  ; jump if start bit has been detected
            dcr c                       ; else decrement the low byte of the "idle" counter
            jnz bindl1
            dcr b                       ; secrement the high byte of the "idle" counter
            jnz bindl1
            jmp finished                ; the "idle" counter has reached zero (no characters for 3 seconds)

bindl2:     call getch1                 ; start bit has been detected, get the byte from the serial port
            mov m,a                     ; write the byte to memory
            inr l                       ; increment the low byte of the address pointer
            jnz bindl0                  ; go back for the next byte
            inr h                       ; increment the high byte of the address pointer 
            jmp bindl0                  ; go back for the next byte
            
;------------------------------------------------------------------------
; load an Intel HEX file into memory using the Tera Term "Send file" function.
; uses D for the record's byte count. uses B to hold the record's checksum.
; when the download is finished, jump to the address contained in the last record.
; when using the Tera Term "Send file" function, make sure that:
;   1. the serial port transmit delay is set to at least 1 mSec/char
;   2. the "Binary" option check box on the Send File dialog box is NOT checked.
;------------------------------------------------------------------------            
hexdl:      mvi h,hi(hexloadtxt)  
            mvi l,lo(hexloadtxt) 
            call puts 

            mvi h,hi(waittxt)
            mvi l,lo(waittxt)
            call puts                   ; prompt for download
            
hexdl1:     call getche                 ; get the first character of the record and echo it
            cpi ':'                     ; start of record character?
            jnz hexdl1                  ; loop until start of record found
            
hexdl1a:    call hexbyte                ; get byte count
            cpi 0                       ; zero byte count?
            jz waitend                  ; zero means last record             
            mov d,a                     ; save the byte count in D
            mov b,a                     ; save as the checksum in B
            
            call hexbyte                ; get address hi byte
            mov h,a                     ; save hi byte in H
            add b                       ; add to the checksum
            mov b,a                     ; save the checksum in B
            call hexbyte                ; get address lo byte
            mov l,a                     ; save lo byte in L
            add b                       ; add to the checksum
            mov b,a                     ; save the checksum in B
            
            call hexbyte                ; get record type
            cpi 1                       ; end of file?
            jz waitend                  ; record type 1 means end of file
            mov c,a                     ; save record type in C
            add b                       ; add record type to checksum
            mov b,a                     ; save the checksum
            mov a,c                     ; restore the record type from C
            
hexdl2:     call hexbyte                ; get the next data byte
            mov m,a                     ; store it in memory
            add b                       ; add to the checksum
            mov b,a                     ; save the checksum
            inr l                       ; increment lo byte of address pointer
            jnz hexdl3
            inr h                       ; increment hi byte of address pointer
hexdl3:     dcr d
            jz hexdl4                   ; all bytes in this record downloaded
            jmp hexdl2                  ; go back for next data byte
            
hexdl4:     call hexbyte                ; get the checksum byte
            add b                       ; add to the checksum
; Since the record's checksum byte is the two's complement and therefore the additive inverse
; of the data checksum, the verification process can be reduced to summing all decoded byte 
; values, including the record's checksum, and verifying that the LSB of the sum is zero.             
            jnz cksumerr                ; non zero means checksum error
            jmp hexdl1                  ; else, go back for the next record
            
; get the last record
waitend:    call hexbyte                ; get the last address hi byte
            mov h,a                     ; save hi byte in H
            call hexbyte                ; get the last address lo byte
            mov l,a                     ; save lo byte in L
            call save_HL                ; save HL for later
            
            call hexbyte                ; get the last record type
            call hexbyte                ; get the last checksum

finished:   mvi h,hi(loadedtxt)
            mvi l,lo(loadedtxt)
            call puts                   ; print "File loaded."
            call restore_HL
            jmp jump1                   ; jump to the address in the last record

cksumerr:   mvi h,hi(errortxt)
            mvi l,lo(errortxt)
            call puts                   ; print "Checksum error."
            hlt                         ; stay here until reset

;------------------------------------------------------------------------
; get two hex digits from the serial port during the hex download and 
; convert them into a byte returned in A. 
; uses A, C and E
;------------------------------------------------------------------------
hexbyte:    call getche             ; get the first character and echo it
            call ascii2hex          ; convert to hex nibble
            rlc                     ; rotate into the most significant nibble
            rlc
            rlc
            rlc
            ani 0F0H                ; mast out least signifficant nibble
            mov c,a                 ; save the first digit in C as the most significant nibble
            
            call getche             ; get the second character and echo it
            call ascii2hex          ; convert to hex nibble
            ani 0FH                 ; mask out the most significant bits
            ora c                   ; combine the two nibbles
            ret
            
;------------------------------------------------------------------------
; go to a memory address (same as Jump to a memory address)
;------------------------------------------------------------------------           
goto:       mvi h,hi(gototxt)
            mvi l,lo(gototxt)
            call puts
            call get_four               ; get the four digit address into HL
            jc prompt                   ; exit if escape            
            jmp jump1                   ; continue below
            
;------------------------------------------------------------------------
; jump to a memory address
;------------------------------------------------------------------------           
jump:       mvi h,hi(jumptxt)
            mvi l,lo(jumptxt)
            call puts
            call get_four               ; get the four digit address into HL
            jc prompt                   ; exit if escape
jump1:      mov d,h                     ; save the high byte of the address in D
            mov e,l                     ; save the low byte of the address in E
            mvi h,hi(jmp_addr)
            mvi l,lo(jmp_addr)
            mvi m,44H                   ; store the "jmp" instruction at jmp_addr
            inr l                       ; next memory location
            mov m,e                     ; store the low byte of jump address at jmp_addr+1
            inr l                       ; next memory location
            mov m,d                     ; store the high byte of jump address at jmp_addr+2
            call crlf                   ; start of a new line
            jmp jmp_addr                ; go jump!
          
;------------------------------------------------------------------------            
; call a subroutine
;------------------------------------------------------------------------                   
callsub:    mvi h,hi(calltxt)
            mvi l,lo(calltxt)
            call puts
            call get_four               ; get the four digit address into HL
            jc prompt                   ; exit if escape
            mov d,h                     ; save the high byte of the address in D
            mov e,l                     ; save the low byte of the address in E
            mvi h,hi(jmp_addr)
            mvi l,lo(jmp_addr)
            mvi m,46H                   ; store "CALL" instruction at jmp+addr
            inr l
            mov m,e                     ; store the low byte of the subroutine address at jmp_addr+1
            inr l                       ; next memory location
            mov m,d                     ; store the high byte of the subroutine address at jmp_addr+2
            inr l
            mvi m,07H                   ; store "RET" instruction at jmp_addr+3
            call crlf                   ; start of a new line
            call jmp_addr               ; call the subroutine
            jmp prompt
            
;------------------------------------------------------------------------            
; get a byte from an input port
;------------------------------------------------------------------------            
input:      mvi h,hi(inputtxt)
            mvi l,lo(inputtxt)
            call puts
            mvi h,hi(porttxt)
            mvi l,lo(porttxt)
            call puts    
            call get_two
            jc prompt                   ; exit if escape            
            ani 00000111B
            rlc
            ori 01000001B
            mvi h,hi(jmp_addr)
            mvi l,lo(jmp_addr)
            mov m,a                   ; store the "IN" instruction at jmp_addr
            inr l
            mvi m,07H                 ; store the "RET" instruction at jmp_addr+1
            call jmp_addr             ; execute the "IN" instruction
            mov d,a                   ; save the input data in E
            mvi h,hi(arrowtxt)
            mvi l,lo(arrowtxt)
            call puts
            mov a,d
            call write_hex
            call crlf
            jmp prompt

;------------------------------------------------------------------------            
; send a byte to an output port
;------------------------------------------------------------------------            
output:     mvi h,hi(outputtxt)
            mvi l,lo(outputtxt)
            call puts
            mvi h,hi(porttxt)
            mvi l,lo(porttxt)
            call puts    
            call get_two
            jc prompt                   ; exit if escape            
            mov d,a                     ; save the port address in D
            mvi h,hi(bytetxt)
            mvi l,lo(bytetxt)
            call puts   
            call get_two
            jc prompt                   ; exit if escape            
            mov e,a                     ; save the date byte in E
            mov a,d                     ; recall the address from D
            ani 00011111B               ; construct the "OUT" instruction
            rlc
            ori 01000001B
            mvi h,hi(jmp_addr)
            mvi l,lo(jmp_addr)
            mov m,a                   ; store the "OUT" instruction at jmp_addr
            inr l
            mvi m,07H                 ; store the "RET" instruction at jmp_addr+1
            mov a,e                   ; recall the data byte from E
            call jmp_addr             ; execute the "OUT" instruction
            call crlf                 ; start of a new line            
            jmp prompt            

;------------------------------------------------------------------------            
; load code into RAM which, when executed, will set the bits of output port 10
; to control the A14 and A13 address lines of the EPROM. when executed, the code
; in RAM sets the address lines to select the second 8K of EPROM and then jumps
; to address 2000H which is the start of the SCELBAL BASIC interpreter.
;------------------------------------------------------------------------            
scelbal:    mvi h,hi(scelbaltxt)
            mvi l,lo(scelbaltxt)
            call puts
            mvi h,hi(jmp_addr)
            mvi l,lo(jmp_addr)
            mvi m,55H                 ; store "OUT 10" instruction at 'jmp_addr'  
            inr l
            mvi m,44H                 ; store "JMP" instruction at 'jmp_addr'+1
            inr l
            mvi m,00H                 ; store "00H" (the lo byte of address 2000H) at 'jmp_addr'+2
            inr l
            mvi m,20H                 ; store "20H" (the hi byte of address 2000H) at 'jmp_addr'+3
            mvi a,01H                 ; value to clear A14, set A13 to select the second 8K of EPROM
            jmp jmp_addr              ; go set the address bits and then jump to the beginning SCELBAL in the second 8K of EPROM
            
;------------------------------------------------------------------------            
; subroutine to decrement double-byte value in BC
;------------------------------------------------------------------------
decBC:     dcr c                        ; decrement contents of C
           inr c                        ; now increment C to set/reset flags
           jnz decbc1                   ; if C not presently zero, skip decrementing B
           dcr b                        ; else decrement B
decbc1:    dcr c                        ; do the actual decrement of C
           ret
           
;------------------------------------------------------------------------            
; get a four digit address (in hex) and store it the high byte at "save_H" and 
; the low byte at "save_L". 
; on return BC contains the address and HL points to "save_H".
; uses A, BC, DE and HL.
;------------------------------------------------------------------------           
get_addr:   mvi h,hi(addresstxt)
            mvi l,lo(addresstxt)
            call puts                   ; prompt for the address
            call get_four               ; get the address
            rc                          ; return prematurely if escape key
            mov b,h
            mov c,l
            mvi h,hi(save_H)
            mvi l,lo(save_H)
            mov m,b                     ; save the address high byte in memory
            inr l                       ; next memory location
            mov m,c                     ; save the address low byte in memory
            dcr l
            ret
            
;------------------------------------------------------------------------            
; get a four digit count (in hex) and store it the high byte at "save_B" 
; and the low byte at "save_C"
; on return BC contains the count and HL points to "save_B".
; uses A, BC, DE and HL.
;------------------------------------------------------------------------                      
get_count:  mvi h,hi(hcounttxt)
            mvi l,lo(hcounttxt)
            call puts                   ; prompt for the count
            call get_four               ; get the count
            rc                          ; return prematurely if space, enter or escape
            mov b,h
            mov c,l
            mvi h,hi(save_B)
            mvi l,lo(save_B)
            mov m,b                     ; save the high byte of the count in memory
            inr l
            mov m,c                     ; save the low byte of the count in memory
            dcr l
            ret

;------------------------------------------------------------------------      
; save the contents of H and L in a temporary storage area in memory.
; uses H and L, D and E.
;------------------------------------------------------------------------
save_HL:   mov d,h                ; transfer value in H to A
           mov e,l                ; and value in L to E
           mvi h,hi(save_H)       ; and set H to storage area page           
           mvi l,lo(save_H)       ; now set L to temporary storage locations
           mov m,d                ; save A (entry value of H) in memory
           inr l                  ; advance pointer
           mov m,e                ; save E (entry value of L) in memory
           ret

;------------------------------------------------------------------------
; restore the contents H and L from temporary storage in memory.
; uses H and L, D and E.
;------------------------------------------------------------------------
restore_HL:mvi h,hi(save_H)       ; and set L to storage area page
           mvi l,lo(save_H)       ; now set L to start of temporary storage locations
           mov d,m                ; fetch stored value for H into A
           inr l                  ; advance pointer
           mov e,m                ; fetch stored value for L into E
           inr l                  ; advance pointer
           mov h,d                ; restore  saved value for H
           mov l,e                ; restore saved value for L
           ret

;------------------------------------------------------------------------      
; save the contents of B and C in a temporary storage area in memory.
; uses H and L
;------------------------------------------------------------------------
save_BC:   mvi h,hi(save_B)       ; and set H to storage area page           
           mvi l,lo(save_B)       ; now set L to temporary storage locations
           mov m,b                ; save B in memory
           inr l                  ; advance pointer
           mov m,c                ; save C in memory
           ret

;------------------------------------------------------------------------
; restore the contents B and C from temporary storage in memory.
; uses H and L
;------------------------------------------------------------------------
restore_BC:mvi h,hi(save_B)       ; and set L to storage area page
           mvi l,lo(save_B)       ; now set L to start of temporary storage locations
           mov b,m                ; fetch stored value for B
           inr l                  ; advance pointer
           mov c,m                ; fetch stored value for C
           inr l                  ; advance pointer
           ret
           
;------------------------------------------------------------------------      
; save the contents of D and E in a temporary storage area in memory.
; uses H and L
;------------------------------------------------------------------------
save_DE:   mvi h,hi(save_D)       ; and set H to storage area page           
           mvi l,lo(save_D)       ; now set L to temporary storage locations
           mov m,d                ; save D in memory
           inr l                  ; advance pointer
           mov m,e                ; save E in memory
           ret

;------------------------------------------------------------------------
; restore the contents D and E from temporary storage in memory.
; uses DE and HL
;------------------------------------------------------------------------
restore_DE:mvi h,hi(save_D)       ; and set L to storage area page
           mvi l,lo(save_D)       ; now set L to start of temporary storage locations
           mov d,m                ; fetch stored value for D
           inr l                  ; advance pointer
           mov e,m                ; fetch stored value for E
           inr l                  ; advance pointer
           ret
           
;------------------------------------------------------------------------        
; enter decimal digits until terminated with carriage return or escape. 
; returns with carry set if escape, otherwise returns with the binary 
; value in HL and carry clear.
; uses A, B, DE and HL
;------------------------------------------------------------------------        
get_dec:    mvi h,0
            mvi l,0
get_dec1:   call getch      ; get input from serial
            cpi RETURN      ; carriage return?
            jnz get_dec2
            ana a           ; clear carry
            ret
            
get_dec2:   cpi ESCAPE      ; escape?
            jnz get_dec3 
            mvi a,1
            rrc             ; set carry flag
            ret
            
get_dec3:   cpi '0'
            jc get_dec1     ; go back for another digit if the digit in A is less than 0
            cpi '9'+1
            jnc get_dec1    ; go back for another digit if the digit in A is greater than 9
            call putch      ; since it's legit, echo the digit
            sui 30H         ; convert the ASCII decimal digit in A to binary
            mov b,a         ; save the decimal digit in B
        
            mov d,h
            mov e,l         ; copy HL into DE
        
            ; double HL (effectively multiplying HL by 2)
            mov a,l
            add l
            mov l,a
            mov a,h
            adc h
            mov h,a

            ; double HL again (effectively multiplying HL by 4)
            mov a,l
            add l
            mov l,a
            mov a,h
            adc h
            mov h,a
        
            ; add DE (containing the original value of HL) to HL (effectively multiplying HL by 5)
            mov a,l
            add e
            mov l,a
            mov a,h
            adc d
            mov h,a
        
            ; double HL again (effectively multiplying HL by 10)
            mov a,l
            add l
            mov l,a
            mov a,h
            adc h
            mov h,a
        
            ; add the new digit (saved in B) to HL
            mov a,l
            add b
            mov l,a
            mov a,h
            mvi d,0
            adc d
            mov h,a         
        
            jmp get_dec1      ; go back for the next decimal digit
            
;------------------------------------------------------------------------        
; print the 8 bit binary number in A as three decimal digits.
; leading zeros are suppressed.
; uses A, BC and DE.
;------------------------------------------------------------------------        
prndec8:    mvi e,0         ; clear the leading zero flag (suppress zeros)
            mvi d,100       ; power of 10, starts as 100
prndec8a:   mvi c,'0'-1     ; C serves as the counter (starts at 1 less than ascii zero)
prndec8b:   inr c
            sub d           ; subtract power of 10
            jnc prndec8b    ; go back for another subtraction if the difference is still positive
            add d           ; else , add back the power of 10
            mov b,a         ; save the difference in B
            mov a,c         ; get the counter from C
            cpi '1'         ; is it zero?
            jnc prndec8c    ; jump if the counter is greater than ascii zero
            mov a,e         ; recall the leading zero flag from E
            ora a           ; set flags according to the leading zero flag
            mov a,c         ; restore the counter from C
            jz prndec8d     ; skip printing the digit if the leading zero flag is zero
prndec8c:   call putch      ; else, print the digit
            mvi e,0FFH      ; set the leading zero flag
prndec8d:   mov a,d
            sui 90          ; reduce power of ten from 100 to 10
            mov d,a
            mov a,b         ; recall the difference from B
            jnc prndec8a    ; go back for the tens digit
            adi '0'         ; else, convert the ones digit to ascii
            call putch      ; print the last digit
            ret
        
;------------------------------------------------------------------------                
; print the 16 bit binary number in HL as five decimal digits.
; leading zeros are suppressed.
; uses A, HL, BC and DE.
;------------------------------------------------------------------------        
prndec16:   mvi b,0         ; clear the leading zero flag
            mvi d,hi(10000)
            mvi e,lo(10000) ; DE now contains 10000
            call subtr      ; count and print the ten thousands digit
            mvi d,hi(1000)
            mvi e,lo(1000)  ; DE now contains 1000            
            call subtr      ; count and print the thousands digit
            mvi d,0
            mvi e,100       ; DE now contains 100
            call subtr      ; count and print the hundreds digit
            mvi d,0
            mvi e,10        ; DE now contains 10
            call subtr      ; count and print the tens digit
            mov a,l         ; get the units digit
            adi '0'         ; convert the units digit to ascii
            jmp putch       ; print the units digit
        
; count and print the number of times the power of ten in DE can be subtracted from HL
subtr:      mvi c,'0'-1     ; initialize the counter in C
subtr1:     inr c           ; increment the counter
            mov a,l
            sub e           ; subtract E from L
            mov l,a
            mov a,h
            sbb d           ; subtract D from H
            mov h,a
            jnc subtr1      ; continue subtracting until underflow
        
            ; underflow occured, add the power of ten back to HL
            mov a,l
            add e           ; add E back to L
            mov l,a
            mov a,h
            adc d           ; add D back to H
            mov h,a
            mov a,c
        
            ; check for zero
            cpi '1'
            jnc subtr2      ; jump if the count in C is greater than zero
            mov a,b         ; else, recall the leading zero flag
            ora a           ; set flags
            mov a,c         ; recall the count
            rz              ; return if the leading zero is zero
            jmp putch       ; else, print the digit
        
subtr2:     mvi b,0FFH      ; set the leading zero flag
            jmp putch       ; print the digit
           
;------------------------------------------------------------------------
; reads four hex digits from the serial port and converts them into two
; bytes returned in H and L.  enter key exits with fewer than four digits.
; returns with carry flag set if escape key is pressed.
; in addition to H and L, uses A, BC and E.
;------------------------------------------------------------------------
get_four:   call get_hex            ; get the first character
            jnc get_four2           ; not space, enter nor escape
            cpi 1BH                 ; escape key?
            jnz get_four            ; go back for another try
get_four1:  mvi a,1
            rrc                     ; set the carry flag
            mvi a,1BH
            mvi h,0
            mvi l,0
            ret                     ; return with escape in A and carry set
; the first digit is a valid hex digit 0-F
get_four2:  call ascii2hex          ; convert to hex nibble
            rlc                     ; rotate into the most significant nibble
            rlc
            rlc
            rlc
            ani 0F0H                ; mast out least signifficant nibble
            mov l,a                 ; save the first nibble in L
            
; get the second character            
get_four3:  call get_hex            ; get the second character
            jnc get_four5
            cpi 1BH                 ; escape key?
            jz get_four1
            cpi 0DH                 ; enter key?
            jnz get_four3
            mov a,l                 ; recall the first nibble from L
            rrc                     ; rotate back to least significant nibble
            rrc
            rrc
            rrc
            ani 0FH                 ; mask out most significant nibble
            mov l,a                 ; put the first digit in L
get_four4:  mvi h,0                 ; clear H
            sub a                   ; clear the carry flag
            ret
            
; the second character is a valid hex digit 0-F            
get_four5:  call ascii2hex          ; convert to hex nibble
            ani 0FH                 ; mask out the most significant bits
            ora l                   ; combine the two nibbles
            mov l,a                 ; save the first two digits in L

; the first two digits are in L. get the third character
get_four6:  call get_hex            ; get the third character
            jnc get_four7           ; not space, escape nor enter
            cpi 1BH                 ; escape key?
            jz get_four1
            cpi 0DH                 ; enter key?
            jnz get_four6           ; go back for another try
            jmp get_four4           ; exit with carry set
            
; the third character is a valid hex digit 0-F            
get_four7:  call ascii2hex          ; convert to hex nibble
            rlc                     ; rotate into the most significant nibble
            rlc
            rlc
            rlc
            ani 0F0H                ; mast out least signifficant nibble
            mov h,a                 ; save the nibble in H

; the first two digits are in L. the third digit is in H. get the fourth character
get_four8:  call get_hex            ; get the fourth character
            jnc get_four9
            cpi 1BH                 ; escape key?
            jz get_four1
            cpi 0DH                 ; enter key?
            jnz get_four8           ; go back for another try

; enter key pressed...            
            mov a,h                 ; retrieve the third digit from H
            rrc                     ; rotate the third digit back to least significant nibble
            rrc
            rrc
            rrc
            ani 0FH                 ; mask out most significant nibble
            mov h,a
; the first two digits are in L, the third digit is in H
            mov b,h                 ; save the third digit in B
            mov c,l                 ; save the first two digits in C
            
            mov a,l
            rlc                     ; rotate the second digit to the most sifnificant nibble
            rlc
            rlc
            rlc
            ani 0F0H                ; mask bits
            ora h                   ; combine the second and third digits
            mov l,a                 ; second and third digits now in L
            
            mov a,c                 ; get the first two digits from C
            rrc                     ; rotate the first digit to the least significant nibble
            rrc
            rrc
            rrc
            ani 0FH                 ; mask out the most significant bits
            mov h,a                 ; first digit now in H
            sub a                   ; clear the carry flag
            ret
            
; the fourth character is a valid hex digit 0-F            
get_four9:  call ascii2hex          ; convert to hex nibble
            ani 0FH                 ; mask out the most significant bits
            ora h                   ; combine the two nibbles
            mov c,l                 ; save the first two digits in C
            mov l,a                 ; save the last two digits in L
            mov h,c                 ; save the first two digits in H
            sub a                   ; clear the carry flag
            ret

;------------------------------------------------------------------------
; get two hex digits from the serial port and convert them into a
; byte returned in A.  enter key exits if fewer than two digits.
; returns with carry flag set if escape key is pressed.
; uses A, BC and E
;------------------------------------------------------------------------
get_two:    call get_hex            ; get the first character
            jc get_two5             ; jump if space, enter or escape

; the first character is a valid hex digit 0-F
            call ascii2hex          ; convert to hex nibble
            rlc                     ; rotate into the most significant nibble
            rlc
            rlc
            rlc
            ani 0F0H                ; mast out least signifficant nibble
            mov c,a                 ; save the first digit in C as the most significant nibble
            
            call get_hex            ; get the second character
            jnc get_two2
            cpi 0DH                 ; enter key?
            jnz get_two5            ; jump if space or escape
            mov a,c                 ; retrieve the first digit
            rrc                     ; rotate the first digit back the the least significant nibble
            rrc
            rrc
            rrc
            ani 0FH                 ; mask out the most significant nibble
            mov b,a                 ; save the first digit in B
            jmp get_two3
            
; the second character is a valid hex digit 0-F            
get_two2:   call ascii2hex          ; convert to hex nibble
            ani 0FH                 ; mask out the most significant bits
            ora c                   ; combine the two nibbles
            mov b,a
get_two3:   sub a                   ; clear the carry flag
            mov a,b
            ret

; return with carry flag set
get_two5:   mov b,a
            mvi a,1
            rrc                     ; set the carry flag
            mov a,b
            ret
            
;------------------------------------------------------------------
; get an ASCII hex character 0-F in A from the serial port.
; echo the character if it's a valid hex digit.
; return with the carry flag set if ENTER, ESCAPE, or SPACE
; uses A, B, and E
;------------------------------------------------------------------
get_hex:    call getch          
            ani 01111111B           ; mask out most significant bit
            cpi 0DH
            jz get_hex3             ; jump if enter key
            cpi 1BH
            jz get_hex3             ; jump if escape key
            cpi 20H
            jz get_hex3             ; jump if space
            cpi '0'
            jc get_hex              ; try again if less than '0'
            cpi 'a'
            jc get_hex1             ; jump if already upper case...
            sui 20H                 ; else convert to upper case
get_hex1:   cpi 'G'
            jnc get_hex             ; try again if greater than 'F'
            cpi ':'
            jc get_hex2             ; continue if '0'-'9'
            cpi 'A'
            jc get_hex              ; try again if less than 'A'
            
get_hex2:   mov b,a                 ; save the character in B
            call putch              ; echo the character
            sub a                   ; clear the carry flag
            mov a,b                 ; restore the character
            ret                     ; return with carry cleared and character in a

get_hex3:   mov b,a
            mvi a,1
            rrc                     ; set carry flag
            mov a,b
            ret                     ; return with carry set and character in a  
            
;-------------------------------------------------------------------------
; write the byte in A to the serial port as two ASCII hex characters.
; uses A, D and E.
;-------------------------------------------------------------------------
write_hex:  mov d,a                 ; save the byte in D
            rrc                     ; rotate most significant nibble into lower 4 bits
            rrc
            rrc
            rrc
            call hex2ascii          ; convert the most significand digit to ascii
            call putch              ; print the most significant digit
            mov a,d                 ; restore
            call hex2ascii
            call putch
            ret

;------------------------------------------------------------------------
; convert the lower nibble in A to an ASCII hex character returned in A.
; uses A and E.
;------------------------------------------------------------------------
hex2ascii:  ani 0FH                 ; mask all but the lower nibble
            mov e,a                 ; save the nibble in E
            sui 10
            mov a,e
            jc hex2ascii1           ; jump if the nibble is less than 10
            adi 7                   ; add 7 to convert to A-F
hex2ascii1: adi 30H
            ret
            
;------------------------------------------------------------------------
; convert an ascii character in A to its hex equivalent.
; return value in lower nibble, upper nibble zeros
; uses A and E.
;------------------------------------------------------------------------
ascii2hex:  cpi 'a'
            jc ascii2hex1           ; jump if already upper case...
            sui 20H                 ; else convert to upper case
ascii2hex1: sui 30H
            mov e,a                 ; save the result in b
            sui 0AH                 ; subtract 10 decimal
            jc  ascii2hex2
            mov a,e                 ; restore the value
            sui 7
            mov e,a
ascii2hex2: mov a,e
            ret            
            
;------------------------------------------------------------------------        
; serially print carrage return and line feed
; uses A and E.
;------------------------------------------------------------------------
crlf:       mvi a,0DH
            call putch
            mvi a,0AH
            jmp putch
            
;------------------------------------------------------------------------        
; serially print a space
; uses A and E.
;------------------------------------------------------------------------
space:      mvi a,' '
            jmp putch            

;------------------------------------------------------------------------        
; serially print the null terminated string whose address is in HL.
; uses A and E and HL      
;------------------------------------------------------------------------
puts:       mov a,m
            ana a
            rz                      ; end of string
            call putch
            inr l                   ; next character
            jnz puts
            inr h
            jmp puts

INPORT      equ 0                   ; serial input port address
OUTPORT     equ 08H                 ; serial output port address
;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. 
; receives 1 start bit, 8 data bits and 1 stop bit at 2400 bps.
; do not echo. return the character in A.
; uses A and E.
;-----------------------------------------------------------------------------------------
getch:      in INPORT               ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc getch                ; jump if start bit not detected (input is high)

            ; start bit detected. wait 52 cycles (1/2 bit time)
getch1:     mvi e,0                 ; initialize E
            mvi e,0                 ; timing
            xra a                   ; clear the accumulator
            mvi a,0                 ; timing
            call delay1             ; timing
            mvi e,0                 ; timing
            
            call getbit             ; receive bit 0
            call getbit             ; receive bit 1
            call getbit             ; receive bit 2
            call getbit             ; receive bit 3
            call getbit             ; receive bit 4
            call getbit             ; receive bit 5
            call getbit             ; receive bit 6
            call getbit             ; receive bit 7
            
            ; wait 104 cycles for the stop bit
            mov a,e                 ; save the character from E to A
            mvi e,0FEH              ; timing
            call delay              ; timing
            mov e,a                 ; save the character from A to E
            mvi a,1                 ; timing
            mvi a,1                 ; timing
            ; wait 104 cycles.
            mov a,e                 ; restore the character from E to A
            mvi e,0FEH              ; timing
            call delay              ; timing
            ret

getbit:     mov a,e                 ; save the received bits from E to A
            mvi e,0FFH              ; timing
            call delay              ; timing
            mov e,a                 ; restore the received bits from A to E
            ana a                   ; timing
            in INPORT               ; get input from the serial port
            in INPORT               ; timing
            rar                     ; rotate the received bit right into carry
            mov a,e                 ; restore the previously received bits from E to A
            rar                     ; rotate the newly received bit in carry right into the MSB of A
            mov e,a                 ; save the received bits in E
            ret        

;-----------------------------------------------------------------------------------------
; wait for a character from the serial port. 
; receives 1 start bit, 8 data bits and 1 stop bit at 2400 bps.
; echo each bit as it is received. return the received character in A.
; uses A and E.
;-----------------------------------------------------------------------------------------
getche:     in INPORT               ; get input from serial port
            rar                     ; rotate the received serial bit right into carry
            jc getche               ; jump if start bit not detected (input is high)

            ; start bit detected. wait 52 cycles (1/2 bit time) then send start bit
            mvi e,0                 ; initialize E
            mvi e,0                 ; timing
            xra a                   ; clear the accumulator
            out OUTPORT             ; send the start bit
            call delay1             ; timing
            mvi e,0                 ; timing
            
            ; receive and echo bits 0 through 7
            call getbitecho         ; receive/echo bit 0
            call getbitecho         ; receive/echo bit 1
            call getbitecho         ; receive/echo bit 2
            call getbitecho         ; receive/echo bit 3
            call getbitecho         ; receive/echo bit 4
            call getbitecho         ; receive/echo bit 5
            call getbitecho         ; receive/echo bit 6
            call getbitecho         ; receive/echo bit 7
            
            ; wait 104 cycles, then send the stop bit
            mov a,e                 ; save the character from E to A
            mvi e,0FEH              ; timing
            call delay              ; timing    
            mov e,a                 ; restore the character from A to E
            mvi a,1                 ; '1' for the stop bit
            out OUTPORT             ; send the stop bit
            mov a,e                 ; restore the character from E to A
            ani 7FH                 ; mask out the most significant bit
            ;mvi e,0FEH              ; timing
            ;call delay              ; timing
            ret

getbitecho: mov a,e                 ; save the received bits from E to A
            mvi e,0FFH              ; timing
            call delay              ; timing
            mov e,a                 ; restore the received bits from A to E
            ana a                   ; timing adjustment
            in INPORT               ; get input from the serial port
            out OUTPORT             ; echo the received bit
            rar                     ; rotate the received bit right into carry
            mov a,e                 ; restore the previously received bits from E to A
            rar                     ; rotate the newly received bit in carry right into the MSB of A
            mov e,a                 ; save the received bits in E
            ret

;------------------------------------------------------------------------        
; sends the character in A out from the serial port.
; transmits 1 start bit, 8 data bits and 1 stop at 2400 bps.
; uses A and E.
;------------------------------------------------------------------------
putch:      mov e,a                 ; save the character from A to E
            xra a                   ; clear A for the start bit

            out OUTPORT             ; send the start bit
            mov a,e                 ; restore the character from E to A 
            mov a,e                 ; timing
            mvi e,0FDH              ; timing
            mvi e,0FDH              ; timing   
            call delay
            
            call putbit             ; transmit bit 0
            call putbit             ; transmit bit 1
            call putbit             ; transmit bit 2
            call putbit             ; transmit bit 3
            call putbit             ; transmit bit 4
            call putbit             ; transmit bit 5
            call putbit             ; transmit bit 6
            call putbit             ; transmit bit 7            

            ; send the stop bit 
            mov e,a                 ; save the character from A to E
            mvi a,1                 ; '1' for the stop bit
            out OUTPORT             ; send the stop bit 
            mov a,e                 ; restore the character from E to A
            mvi e,0FCh              ; timing
            call delay              ; timing
            ret

putbit:     out OUTPORT             ; output the least significant bit of the sharacter in A
            mvi e,0FDH              ; timing
            mvi e,0FDH              ; timing
            call delay              ; timing
            rrc                     ; shift the bits of the character in A right
            ret
            
;------------------------------------------------------------------------        
; delay in microseconds = (((255-value in E)*16)+19) * 4 microseconds
;------------------------------------------------------------------------        
delay:      inr e
            jnz delay
delay1:     ret        
            
;------------------------------------------------------------------------                    
titletxt:   db  "\r\r"
            db  "Serial Monitor for Intel 8008 SBC V2.0\r"
            db  "Assembled on ",DATE," at ",TIME,"\r",0
menutxt:    db  "\r"
            db  "B - Binary file download\r"
            db  "C - Call subroutine\r"
            db  "D - Dump RAM\r"
            db  "E - Examine/Modify RAM\r"            
            db  "F - Fill RAM\r"
            db  "H - Hex file download\r"
            db  "G - Go to address\r"
            db  "I - Input byte from port\r"
            db  "J - Jump to address\r"
            db  "O - Output byte to port\r"
            db  "S - SCELBAL\r",0

prompttxt:  db  "\r>>",0
dumptxt:    db  "ump memory\r",0
examinetxt: db  "xamine memory\r",0
filltxt:    db  "ill memory\r",0
jumptxt:    db  "ump to address: (in hex) ",0 
calltxt:    db  "all subroutine at address: ",0 
gototxt:    db  "o to address: (in hex) ",0
inputtxt    db  "nput byte from port",0
outputtxt   db  "utput byte to port",0 
binloadtxt: db  "inary file download\r",0 
scelbaltxt: db  "CELBAL\r",0
hexloadtxt: db  "ex file download\r",0
addresstxt: db  "\rAddress: (in hex) ",0 
hcounttxt:  db  "  Count: (in hex) ",0
valuetxt:   db  "  Value: (in hex) ",0   
columntxt:  db  "\r     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F",0
dnldtxt:    db  "\rWaiting for the binary file download.\r",0   
waittxt:    db  "\rWaiting for the Intel hex file download.\r",0
loadedtxt:  db  "\rFile loaded.\r",0  
errortxt:   db  "\rChecksum error!\r",0
arrowtxt:   db  " --> ",0
newvaluetxt:db  "  New: ",0
porttxt     db  "\rPort address: (in hex) ",0
bytetxt     db  "\rOutput byte:  (in hex) ",0
copytxt     db  "\rIntel 8008 SBC Monitor © Copyright 2022 by Jim Loos\r",0
epromtxt    db "\rEPROM checksum error!\r",0

            db 4000H-$ dup (0)      ; fill the rest of the EPROM space with zeros  
           
           end
