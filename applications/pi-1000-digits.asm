;; SCELPie, SCELBI 8008 Pi/e
;;
;; Egan Ford (egan@sense.net)
;;
;; Mar, 04 2013
;;
;; SCELPie will compute 1000 decimal digits of Pi or e.
;;
;; All SCELPie computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; Memory required:  4K
;;
;; Pi performance(mm:ss) @ 500 KHz:  25:23
;;
;; e  performance(mm:ss) @ 500 KHz:  07:04
;;
;; C snippets:  J.W. Stumpel, May 1991
;;
;; Assembler:  Macro Assembler AS V1.42
;;
;; To assemble:
;;
;;    asl -L -gnuerrors -u -x -x -U pie.s
;;    p2hex -r \$-\$ pie.p pie.hex
;;

                        ; Assembler: Macro Assembler AS V1.42
    cpu     8008        ; Intel 8008

;; constants (R/O)

dec_len =   1000        ; 1000 decimal digits
bin_len =   418         ; ceil(1001 / log(256)) + 1 (+ 1 if odd) = 418
                        ;   Duff's Device 4x unrolled loops assume
                        ;   bin_len % 4 = 2
bin_end =   bin_len-1
                        ; counting up is easier
bcd_len =   65536-(dec_len/2)
unrollc =   (bin_len/4)+1   ; the number of 4x unrolled loop is deterministic

cr  =   0Dh             ; ASCII CR


;; start of global macros/functions

; Description:  Return hi 8-bits of a label
;               e.g.: hi(label)

hi  function x,(x>>8)&255


; Description:  Return lo 8-bits of a label
;               e.g.: lo(label)

lo  function x,x&255


; Description:  Load H/L registers with a 16-bit value,
;               low byte in L, high byte in H
; Input:        ptr (16-bit) immediate
; Registers:    H, L

ldhli   macro   ptr
    lli lo(ptr)     ; L <- I
    lhi hi(ptr)     ; H <- I
    endm


; Description:  Load B/C registers with a 16-bit value,
;               low byte in C, high byte in B
; Input:        ptr (16-bit) immediate
; Registers:    H, L

ldbci   macro   ptr
    lci lo(ptr)     ; C <- I
    lbi hi(ptr)     ; B <- I
    endm


; Description:  Load D/E registers with a 16-bit value,
;               low byte in E, high byte in D
; Input:        ptr (16-bit) immediate
; Registers:    H, L

lddei   macro   ptr
    lei lo(ptr)     ; E <- I
    ldi hi(ptr)     ; D <- I
    endm


; Description:  Swap HL with BC and vv. through A
; Input:        implied
; Registers:    A, B, C, H, L

swhlbc  macro
    lah         ; A <- H
    lhb         ; H <- B
    lba         ; B <- A
    lal         ; A <- L
    llc         ; L <- C
    lca         ; C <- A
    endm


; Description:  Swap HL with BC and vv. through E
; Input:        implied
; Registers:    B, C, E, H, L

swhlbce macro
    leh         ; E <- H
    lhb         ; H <- B
    lbe         ; B <- E
    lel         ; E <- L
    llc         ; L <- C
    lce         ; C <- E
    endm


; Description:  Swap HL with DE and vv. through A
; Input:        implied
; Registers:    A, D, E, H, L

swhlde  macro
    lah         ; A <- H
    lhd         ; H <- D
    lda         ; D <- A
    lal         ; A <- L
    lle         ; L <- E
    lea         ; E <- A
    endm


; Description:  Store HL to memory (ptr) (little endian)
; Input:        ptr (16-bit) immediate
; Registers:    D, E, H, L

sthlm   macro   ptr
    ldh         ; D <- H, backup HL to DE
    lel         ; E <- L
    ldhli   ptr     ; HL <- PTR
    lme         ; M <- E (L)
    inl         ; HL++ (keep ptr even!)
    lmd         ; M <- D (H)
    lhd         ; H <- D, restore HL from DE
    lle         ; L <- E
    endm


; Description:  Load HL from memory (ptr) (little endian)
; Input:        ptr (16-bit) immediate
; Registers:    A, H, L

ldhlm   macro   ptr
    ldhli   ptr     ; HL <- PTR
    lam         ; A <- M (L)
    inl         ; HL++ (keep ptr even!)
    lhm         ; H <- M (H)
    lla         ; L <- A
    endm


; Description:  Load BC from memory (ptr) (little endian)
; Input:        ptr (16-bit) immediate
; Registers:    A, B, C

ldbcm   macro   ptr
    ldhli   ptr     ; HL <- PTR
    lam         ; A <- M (L)
    inl         ; HL++ (keep ptr even!)
    lbm         ; B <- M (H)
    lca         ; C <- A
    endm


; Description:  Convert and print a base 256 number (ptr_a) as base 10/100.
; Input:    a (16-bit) immediate address at start of an array
; Calls:        print_mp
; Registers:    A, B, C, D, E, H, L

mprint  macro   a       ; print (a) base 100
    ldhli   a       ; HL <- ptr
    cal print_mp
    endm


; Description:  Set initial value left of decimal of "a" to "b"
; Input:        a (16-bit), b (8-bit)
;               a = immediate address at start of array
;               b = immediate integer to be set left of decimal point
; Calls:        set_mp
; Registers:    A, B, C, D, E, H, L

mset    macro   a, b        ; a = b
    ldhli   a
    lbi b       ; load B with initial value
    cal set_mp
    endm


; Description:  Divide (a) by an 8-bit quantity (a = a / b)
; Input:        a (16-bit), b (8-bit)
;               a = immediate address at start of array
;               b = immediate address of divisor
; Calls:        div_mp
; Return:       If a = 0 (array of zeros), then carry is set,
;               otherwise carry is clear.
; Registers:    A, B, C, D, E, H, L

mdiv    macro   a, b        ; a = a / b
    lddei   a
    ldbci   b       ; load BC ptr to divisor
    cal div_mp
    endm


; Description:  Divide (a) by a 16-bit quantity (a = a / b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate address at start of array
;               b = immediate address of divisor
; Calls:        div16_mp
; Return:       If a = 0 (array of zeros), then carry is set,
;               otherwise carry is clear.
; Registers:    A, B, C, D, E, H, L

mdiv16  macro   a, b        ; a = a / b
    lddei   a
    ldbci   b       ; load BC ptr to divisor
    cal div16_mp
    endm


; Description:  Left shift array (a)
; Input:        a (16-bit)
;               a = immediate address at start of array
; Calls:        asl_mp
; Registers:    A, D, E, H, L

masl    macro   a       ; a = a * 2
    ldhli   a
    cal asl_mp
    endm


; Description:  Transfer array (b)[0,sizeof(b)] to array (a) (a = b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate address at start of array
;               b = immediate address at start of array
; Calls:        copy_mp
; Registers:    A, B, C, D, E, H, L

mcopy   macro   a, b        ; a = b
    ldbci   a
    ldhli   b
    cal copy_mp
    endm


; Description:  Transfer array (b)[0,sizeof(b)] to array (a) (a = b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate pointer at start of array
;               b = immediate address at start of array
; Calls:        copy_mp
; Registers:    A, B, C, D, E, H, L

mcopym  macro   a, b        ; a = b
    ldbcm   a
    ldhli   b
    cal copy_mp
    endm


; Description:  Add array b to array a (a = a + b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate address at start of array
;               b = immediate address at start of array
; Calls:        add_mp
; Registers:    A, B, C, D, E, H, L

madd    macro   a, b        ; a = a + b
    ldbci   a
    ldhli   b
    cal add_mp
    endm


; Description:  Add array b to array a (a = a + b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate pointer at start of array
;               b = immediate address at start of array
; Calls:        add_mp
; Registers:    A, B, C, D, E, H, L

maddm   macro   a, b        ; a = a + b
    ldbcm   a
    ldhli   b
    cal add_mp
    endm


; Description:  Subtract array b from array a (a = a - b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate address at start of array
;               b = immediate address at start of array
; Calls:        sub_mp
; Registers:    A, B, C, D, E, H, L

msub    macro   a, b        ; a = a - b
    ldhli   a
    ldbci   b
    cal sub_mp
    endm


; Description:  Subtract array b from array a (a = a - b)
; Input:        a (16-bit), b (16-bit)
;               a = immediate pointer at start of array
;               b = immediate address at start of array
; Calls:        sub_mp
; Registers:    A, B, C, D, E, H, L

msubm   macro   a, b        ; a = a - b
    ldhlm   a
    ldbci   b
    cal sub_mp
    endm


; Description:  Compute arctan(1/b) and store at (a).
; Input:        a (16-bit), b (8-bit)
;               a = immediate address at start of array
;               b = immediate integer to be set left of decimal point
; Calls:        atan_mp
; Registers:    A, B, C, D, E, H, L

matan   macro   a, b        ; a = atan(1/b)
    ldhli   a
    lbi b
    cal atan_mp
    endm


; Description:  Print arg number of CRs
; Input:        arg = integer number of CRs
; Calls:        rst 7, crout
; Registers:    A

printcr macro   arg
    rept    arg
    rst 7
    endm
    endm


; Description:  Increment HL +1
; Input:        implied
; Registers:    H, L

inhl    macro   {noexpand}
    inl         ; L = L + 1
    jfz m0      ; if not 0 skip
    inh         ;   H = H + 1
m0:
    endm


; Description:  Decrement HL -1
; Input:        implied
; Registers:    H, L

dchl    macro   {noexpand}
    inl         ; L++
    dcl         ; L-- hack to set zero flag
    jfz m0      ; if not zero just dec L, otherwise
    dch         ;   dec both, H--
m0: dcl         ; L--
    endm


; Description:  Work around for rst issue with Macro Assembler AS V1.42.
;               Macro Assembler AS expects the arguement to rst to be a
;               memory address.  The 8008 specs use the numbers 0 - 7
;               representing hardcoded addresses
; Input:        0 - 7

rst macro   arg,{noexpand}  ; asl does not support rst #
    db  arg*010o+5  ;   this is the hack for that
    endm


;; RESET fast calls, 5 states vs. 8 for CAL.  Store varable here too; watch for
;;   overlap with rst calls.

    org 00h         ; rst 0
bcd_cnt:            ; counter used for print_mp
    db  0, 0        ; 0-1
beg_ptr:            ; 16-bits used to store pointers to arrays
cur_ptr:            ; 16-bits used to store pointers to arrays
    db  0, 0        ; 2-3
end_ptr:            ; 16-bits used to store pointers to arrays
    db  0, 0        ; 4-5
n:  db  0, 0        ; 6-7, used by atan_mp
divisor:            ; divisor for div_mp and div16_mp
    db  0, 0        ; 8-9
    db  0           ; A (place holder)
regx:   db  0       ; B, the x in atan(1/x)
x2: db  0           ; C, cached x*x
ptr_a:  db  0, 0    ; D-E, pointer to array used by atan_mp
    db  0           ; F (place holder)
                    ; table of squares
sqrtbl: db  0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
    db  121, 144, 169, 196, 225
pitext: db  "\r1000 DIGITS OF PI = \0"

    org 38h         ; rst 7
crout:  lai cr      ; A <- CR
    cal cout        ; print it
    ret


;; START of execution

    org 40h         ; 100 octal, SCELBI starts here
main:
    ldhli   pitext  ; load pitext address
    cal print       ; print pitext message
    cal pi          ; compute Pi and store in mp_a
    ;cal    e       ; compute e and store in mp_a
    mprint  mp_a    ; print Pi
    printcr 2       ; 2 CRs
    hlt             ; halt, all done


;; start of subs

; Description:  Output BCD number
;
; Input:        A = BCD number (00 - 99)
;
; Output:       BCD number
;
; Registers:    A, B

prbcd:  lba         ; B <- A
    rrc             ; shift right 4 bits
    rrc
    rrc
    rrc
    cal +           ; do MSB
    lab             ; then LSB
/   ndi 00Fh        ; mask off
    ori 030h        ; make ASCII
    cal cout        ; print it
    ret


; Description:  Convert BIN/HEX to BCD.
;
; Input:        A = BIN/HEX (00h - 63h)
;
; Output:       A = BCD (00 - 99)
;
; Registers:    A, B, C

bintobcd:
    lba
    lci -1          ; start counter at -1
/   inc             ; C=C+1
    sui 10          ; A=A-10
    jfs -           ; loop if positive
    lab             ; A <- B, restore A
    jmp ++          ; jump to dcc
/   adi 6           ; A=A+6
/   dcc             ; if a < 10, then c will be -1 and end loop
    jfs --          ;  othersize add 6 for every 10
    ret


; Description:  Output a string.
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       String to screen
;
; Registers:    A, H, L

print:  lam         ; fetch next message byte
    nda             ; A & A to update status flags
    rtz             ; return if zero found
    cal cout        ; otherwise display byte
    inl             ; point to next byte
    jfz print       ; is it page boundary?
    inh             ;   if so increment page
    jmp print       ;   and then recycle.


; Description:  Set mp array initial value:  a = b
;
; Input:        H/L (hi/lo) address to array (a)
;               B = value left of decimal
;
; Output:       a[0] = B
;               a[1] - a[length-1] = 0
;
; Registers:    A, B, E, H, L

set_mp:
    lmb             ; set inital value, rest of array will be zero
    ldi unrollc     ; count down to 0
    xra             ; a = 0
    jmp ++          ; Duff's Device hardcoded for fixed array length
                    ;   bin_len % 4 = 2
/   inl             ; L += 1
    lma             ; load M with A (0)
    inl             ; L += 1
    lma             ; load M with A (0)
    inhl            ; H/L += 1
    lma             ; load M with A (0)
/   inl             ; L += 1
    lma             ; load M with A (0)
    dcd             ; D=D-1 
    jfz --          ; D=0?
    ret


; Description:  Multiprecision left shift:  a = a * 2
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       a = a * 2
;
; Registers:    A, D, E, H, L

asl_mp:
                ; compute end of array since we have
                ;   to go backwards
    lal         ; A <- L
    adi lo(bin_len) ; add bin_len lo and store it
    lla         ; L <- A
    lah         ; A <- H
    aci hi(bin_len) ; add with carry ben_len hi and store it
    lha         ; H <- A
    ldi unrollc     ; count down to 0
    ora         ; clear carry
    jmp ++      ; Duff's Device hardcoded for fixed array length
/   dchl        ; HL--
    lam         ; A <- M
    ral         ; rol
    lma         ; M <- A
    dcl         ; L--
    lam         ; A <- M
    ral         ; rol
    lma         ; M <- A
/   dcl         ; L--
    lam         ; A <- M
    ral         ; rol
    lma         ; M <- A
    dcl         ; L--
    lam         ; A <- M
    ral         ; rol
    lma         ; M <- A
    dcd         ; D=D-1 
    jfz --      ; D=0?
    ret


; Description:  Compute pi using the Gregory expansion of Machin's arctan
;               formula and save in array mp_a.
;
;   pi = 4 * (4 *      atan(1/5)           -           atan(1/239)          )
;
;
;   __      /   / 1     1      1       \       / 1       1        1        \\
;   || = 4 | 4 |  - - ---- + ---- - ... |  -  | --- - ------ + ------ - ... ||
;          |   |  5      3      5       |     | 239        3        5       ||
;           \   \     3x5    5x5       /       \      3x239    5x239       //
;
;
; Input:        None
;
; Output:       mp_a = pi = 4 * (4 * atan(1/5) - atan(1/239))
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        matan/atan_mp, masl/asl_mp, msub/sub_mp

pi: matan   mp_a,5      ; a = atan(1/5)
    masl    mp_a        ; a = a * 4
    masl    mp_a
    matan   mp_b,239    ; b = atan(1/239)
    msub    mp_a,mp_b   ; a = a - b
    masl    mp_a        ; a = a * 4
    masl    mp_a
    ret


;; tables for faster math must be page aligned

    rept    256-$&0FFh
    db  0
    endm

mt100_lo:
    db  000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
    db  040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
    db  080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
    db  0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch
    db  000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
    db  040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
    db  080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
    db  0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch
    db  000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
    db  040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
    db  080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
    db  0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch
    db  000h,064h,0C8h,02Ch,090h,0F4h,058h,0BCh,020h,084h,0E8h,04Ch,0B0h,014h,078h,0DCh
    db  040h,0A4h,008h,06Ch,0D0h,034h,098h,0FCh,060h,0C4h,028h,08Ch,0F0h,054h,0B8h,01Ch
    db  080h,0E4h,048h,0ACh,010h,074h,0D8h,03Ch,0A0h,004h,068h,0CCh,030h,094h,0F8h,05Ch
    db  0C0h,024h,088h,0ECh,050h,0B4h,018h,07Ch,0E0h,044h,0A8h,00Ch,070h,0D4h,038h,09Ch

mt100_hi:
    db  000h,000h,000h,001h,001h,001h,002h,002h,003h,003h,003h,004h,004h,005h,005h,005h
    db  006h,006h,007h,007h,007h,008h,008h,008h,009h,009h,00Ah,00Ah,00Ah,00Bh,00Bh,00Ch
    db  00Ch,00Ch,00Dh,00Dh,00Eh,00Eh,00Eh,00Fh,00Fh,010h,010h,010h,011h,011h,011h,012h
    db  012h,013h,013h,013h,014h,014h,015h,015h,015h,016h,016h,017h,017h,017h,018h,018h
    db  019h,019h,019h,01Ah,01Ah,01Ah,01Bh,01Bh,01Ch,01Ch,01Ch,01Dh,01Dh,01Eh,01Eh,01Eh
    db  01Fh,01Fh,020h,020h,020h,021h,021h,021h,022h,022h,023h,023h,023h,024h,024h,025h
    db  025h,025h,026h,026h,027h,027h,027h,028h,028h,029h,029h,029h,02Ah,02Ah,02Ah,02Bh
    db  02Bh,02Ch,02Ch,02Ch,02Dh,02Dh,02Eh,02Eh,02Eh,02Fh,02Fh,030h,030h,030h,031h,031h
    db  032h,032h,032h,033h,033h,033h,034h,034h,035h,035h,035h,036h,036h,037h,037h,037h
    db  038h,038h,039h,039h,039h,03Ah,03Ah,03Ah,03Bh,03Bh,03Ch,03Ch,03Ch,03Dh,03Dh,03Eh
    db  03Eh,03Eh,03Fh,03Fh,040h,040h,040h,041h,041h,042h,042h,042h,043h,043h,043h,044h
    db  044h,045h,045h,045h,046h,046h,047h,047h,047h,048h,048h,049h,049h,049h,04Ah,04Ah
    db  04Bh,04Bh,04Bh,04Ch,04Ch,04Ch,04Dh,04Dh,04Eh,04Eh,04Eh,04Fh,04Fh,050h,050h,050h
    db  051h,051h,052h,052h,052h,053h,053h,053h,054h,054h,055h,055h,055h,056h,056h,057h
    db  057h,057h,058h,058h,059h,059h,059h,05Ah,05Ah,05Bh,05Bh,05Bh,05Ch,05Ch,05Ch,05Dh
    db  05Dh,05Eh,05Eh,05Eh,05Fh,05Fh,060h,060h,060h,061h,061h,062h,062h,062h,063h,063h


; Description:  Compute arctan(1/N) using the Gregory expansion of Machin's
;               arctan formula and save in array (ptr_a).
;
;
;                               / 1     1      1       \
;               arctan(1/N) =  |  - - ---- + ---- - ... |
;                              |  N      3      5       |
;                               \     3xN    5xN       /
;
;
; Input:        H/L (hi/lo) pointer to array
;               B = N (8 bit)
;
; Output:       H/L (hi/lo) pointer to array = arctan(1/N)
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        mset/set_mp, mdiv/div_mp, maddm/add_mp, msubm/sub_mp,
;               mdiv16/div16_mp, mcopy(m)/copy_mp
;
; Globals:      mp_x (16-bit), mp_y (16-bit)
;
; C Algorithm:
;
; void atanbig(bignum A, unsigned short x)
; {
;     bignum X, Y;
;     unsigned short n = 1;
;
;     setbig(X, 1, 0);
;     divbig(X, x);
;     copybig(A, X);
;     x *= x;
;     while (1) {
;         n += 2;
;         divbig(X, x);
;         copybig(Y, X);
;         if (!divbig(Y, n))      // dividend = 0
;             break;
;         if (n & 2)
;             subbig(A, Y);
;         else
;             addbig(A, Y);
;     }
; }

atan_mp:
                ; ptr_a to point to arg
    ldh         ; backup HL -> DE
    lel
    ldhli   ptr_a
    lme         ; store HL(DE) in ptr_a, lo
    inl
    lmd         ; hi

    ldhli   regx        ; save B
    lmb         ; M (regx) <- B

    lab         ; A <- B
    cpi 16      ; if A/B < 16 then get square
    jfc +
    ldhli   sqrtbl      ; look up square
    lal         ; A <- L
    adb         ; A = A + B
    lla         ; L <- A
    lam         ; load A with square
/   ldhli   x2      ; HL -> x2
    lma         ; store square

    ldhli   n       ; n = 1 little endian
    lmi 1
    inl
    lmi 0

    mset    mp_x,1      ; x = 1
    mdiv    mp_x,regx   ; x /= regx
    mcopym  ptr_a,mp_x  ; a = x

atan_mp_loop:           ; main loop

    ldhli   n       ; n = n + 2
    lbm         ; B <- M lo(n)
    inb         ; B++, n now even
    jfz +       ; n not zero
    inl         ; point to hi
    lcm         ; C <- M hi(n)
    inc         ; C++
    lmc         ; store hi(n)
    dcl         ; point to lo
/   inb         ; B++, n now odd
    lmb         ; M lo(n) <- B

    ldhli   regx        ; x = x / regx ^ 2
    lam         ; A <- M (regx)
    cpi 16      ; if > 16 /x/x othersize /x2
    jtc +       ; x already x*x, one div required
    mdiv    mp_x,x2     ; x >= 16, then do two div
/   mdiv    mp_x,x2     ;   (faster than 16 bit div)

    mcopy   mp_y,mp_x   ; y = x

    ldhli   n+1     ; if n < 256 then div else div16
    lam         ; A <- M hi(n)
    nda         ; update zero flag
    jfz +       ; >= 256 use 16 bit div
    mdiv    mp_y,n      ; < 256 use 8 bit div
    rtc         ; dividend = 0, done
    jmp ++      ; still working on it
/   mdiv16  mp_y,n      ; div16
    rtc         ; dividend = 0, done

/   ldhli   n       ; add or sub?
    lam         ; A <- M lo(n)
    ndi 2       ; odd/even check on 2nd bit
    jtz +       ; add it
    msubm   ptr_a,mp_y  ; a = a - y
    jmp atan_mp_loop    ; back to top
/   maddm   ptr_a,mp_y  ; a = a + y
    jmp atan_mp_loop    ; back to top


; Description:  Compute e using the infinite series:
;
;               oo
;               =====
;               \      1
;       e   =    >    --  = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
;               /     k!
;               =====
;               k = 0
;
;               and save in array mp_a.
;
; Input:        None
;
; Output:       mp_a = e = 1 + 1/1! + 1/2! + 1/3! + 1/4! + ...
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        mset/set_mp, mdiv/div_mp, mdiv16/div16_mp, madd/add
;
; Globals:      n, mp_a, mp_x
;
; C Algorithm:
;
; int n = 1;
; setbig(A, 1, 0);
; setbig(X, 1, 0);
; while(divbig(X, n++))         // while(dividend != 0)
;     addbig(A, X);

e:  ldhli   n       ; n = 0 little endian
    xra         ; A = 0
    lma         ; M <- A
    inl         ; point to hi
    lma         ; M <- A
    mset    mp_a,1      ; a = 1
    mset    mp_x,1      ; x = 1
e_loop:
    ldhli   n+1     ; n = n + 1
    lcm         ; C <- M hi(n)
    dcl         ; point to lo
    lbm         ; B <- M lo(n)
    inb         ; B++, n now even
    lmb         ; M lo(n) <- B
    jfz +       ; n not zero
    inl         ; point to hi
    inc         ; C++
    lmc         ; store hi(n)
/   lac         ; A <- C hi(n)
    nda         ; update zero flag
    jfz +       ; >= 256 use 16 bit div
    mdiv    mp_x,n      ; < 256 use 8 bit div
    rtc         ; dividend = 0, done
    jmp ++      ; still working on it
/   mdiv16  mp_x,n      ; div16
    rtc         ; dividend = 0, done
/   madd    mp_a,mp_x   ; a = a + x
        jmp     e_loop


; Description:  Multiprecision transfer:  a = b
;
; Input:        B/C (hi/lo) address to array (a)
;               H/L (hi/lo) address to array (b)
;
; Output:       a = b
;
; Registers:    A, B, C, D, E, H, L

copy_mp:
    ldi unrollc     ; count down to 0
    jmp ++      ; Duff's Device hardcoded for fixed array length
/   lam         ; A <- M (src)
    swhlbce         ; HL <> BC
    lma         ; M <- A (dst)
    inl         ; L += 1 (dst)
    swhlbce         ; BC <> HL
    inl         ; L += 1 (src)
    lam         ; A <- M (src)
    swhlbce         ; HL <> BC
    lma         ; M <- A (dst)
    inhl            ; HL += 1 (dst)
    swhlbce         ; BC <> HL
    inhl            ; HL += 1 (src)
/   lam         ; A <- M (src)
    swhlbce         ; HL <> BC
    lma         ; M <- A (dst)
    inl         ; L += 1 (dst)
    swhlbce         ; BC <> HL
    inl         ; L += 1 (src)
    lam         ; A <- M (src)
    swhlbce         ; HL <> BC
    lma         ; M <- A (dst)
    inl         ; L += 1 (dst)
    swhlbce         ; BC <> HL
    inl         ; L += 1 (src)
    dcd         ; D=D-1 
    jfz --      ; D=0?
    ret


; Description:  Multiprecision subtraction:  a = a - b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to array (b)
;
; Output:       a = a - b
;
; Registers:    A, B, C, D, E, H, L
;
; Globals:      end_ptr

sub_mp:
                ; compute end of array since we have
                ;   to go backwards
    lal         ; A <- L
    adi lo(bin_len) ; add bin_len lo and store it
    lla         ; L <- A
    lah         ; A <- H
    aci hi(bin_len) ; add with carry ben_len hi and store it
    lha         ; H <- A
    lac         ; A <- C
    adi lo(bin_len) ; add bin_len lo and store it
    lca         ; C <- A
    lab         ; A <- B
    aci hi(bin_len) ; add with carry ben_len hi and store it
    lba         ; B <- A
    ldi unrollc     ; count down to 0
    ora         ; clear carry
    jmp ++      ; Duff's Device hardcoded for fixed array length
/   dchl            ; HL-- (a)
    lam         ; A <- M (a)
    swhlbce         ; HL <> BC
    dchl            ; HL-- (b)
    sbm         ; A = A - B
    swhlbce         ; BC <> HL
    lma         ; M = A - B
    dcl         ; H-- (a)
    lam         ; A <- M (a)
    swhlbce         ; HL <> BC
    dcl         ; H-- (b)
    sbm         ; A = A - B
    swhlbce         ; BC <> HL
    lma         ; M = A - B
/   dcl         ; H-- (a)
    lam         ; A <- M (a)
    swhlbce         ; HL <> BC
    dcl         ; H-- (b)
    sbm         ; A = A - B
    swhlbce         ; BC <> HL
    lma         ; M = A - B
    dcl         ; H-- (a)
    lam         ; A <- M (a)
    swhlbce         ; HL <> BC
    dcl         ; H-- (b)
    sbm         ; A = A - B
    swhlbce         ; BC <> HL
    lma         ; M = A - B
    dcd         ; D=D-1 
    jfz --      ; D=0?
    ret


; Description:  Multiprecision addition:  a = a + b
;
; Input:        B/C (hi/lo) address to array (a)
;               H/L (hi/lo) address to array (b)
;
; Output:       a = a + b
;
; Registers:    A, B, C, D, E, H, L
;
; Globals:      end_ptr

add_mp:
                ; compute end of array since we have
                ;   to go backwards
    lal         ; A <- L
    adi lo(bin_len) ; add bin_len lo and store it
    lla         ; L <- A
    lah         ; A <- H
    aci hi(bin_len) ; add with carry ben_len hi and store it
    lha         ; H <- A
    lac         ; A <- C
    adi lo(bin_len) ; add bin_len lo and store it
    lca         ; C <- A
    lab         ; A <- B
    aci hi(bin_len) ; add with carry ben_len hi and store it
    lba         ; B <- A
    ldi unrollc     ; count down to 0
    ora         ; clear carry
    jmp ++      ; Duff's Device hardcoded for fixed array length
/   dchl            ; HL-- (b)
    lam         ; A <- M (b)
    swhlbce         ; HL <> BC
    dchl            ; HL-- (a)
    acm         ; B = B + A 
    lma         ; M = B + A
    swhlbce         ; BC <> HL
    dcl         ; L-- (b)
    lam         ; A <- M (b)
    swhlbce         ; HL <> BC
    dcl         ; L-- (a)
    acm         ; B = B + A 
    lma         ; M = B + A
    swhlbce         ; BC <> HL
/   dcl         ; L-- (b)
    lam         ; A <- M (b)
    swhlbce         ; HL <> BC
    dcl         ; L-- (a)
    acm         ; B = B + A 
    lma         ; M = B + A
    swhlbce         ; BC <> HL
    dcl         ; L-- (b)
    lam         ; A <- M (b)
    swhlbce         ; HL <> BC
    dcl         ; L-- (a)
    acm         ; B = B + A 
    lma         ; M = B + A
    swhlbce         ; BC <> HL
    dcd         ; D=D-1 
    jfz --      ; D=0?
    ret


; Description:  Skip leading zeros (used by div_mp and div16_mp)
;
; Input:        D/E (hi/lo) address to array (a)
;
; Output:       None
;
; Registers:    A, D, E, H, L

skipzeros:
    ldhli   end_ptr     ; HL to point to end_ptr
    lae         ; A <- E
    adi lo(bin_end) ; add bin_end lo and store it
    lma         ; end_ptr(lo) <- A
    inl         ; L++
    lad         ; A <- D
    aci hi(bin_end) ; add with carry bin_end hi and store it
    lma         ; end_ptr(hi) <- A
    lle         ; L <- E
    lhd         ; H <- D
    ldi bin_len/2   ; number of pairs to test
    jmp ++      ; Duff's Device hardcoded for fixed array length
/   lam         ; A <- M
    nda         ; update zero flag
    jfz even
    inl         ; L += 1
    lam         ; A <- M
    nda         ; update zero flag
    jfz odd
    inhl            ; HL += 1
    dcd         ; D--
/   lam         ; A <- M
    nda         ; update zero flag
    jfz even
    inl         ; L += 1
    ;ind            ; D += 1
    lam         ; A <- M
    nda         ; update zero flag
    jfz odd
    inl         ; L += 1
    dcd         ; D--
    jfz --      ; D=0?
    lai 1       ; set carry
    rar         ;   roll 1 in to carry
    ret         ;   all zeros, return
odd:    dchl            ; HL--
even:   ora         ; clear carry
    ret


; Description:  Multiprecision 16-bit division:  a = a / b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to divisor (b)
;
; Output:       a = a / b
;
; Registers:    A, B, C, D, E, H, L
;
; Globals:      end_ptr, beg_ptr, cur_ptr, divisor
;
; Calls:        skipzeros
;
; C Algorithm:  see div_mp

div16_mp:
    cal skipzeros   ; skip leading zeros for speed
    rtc         ; return if carry set (all zeros)

                ; HL pointing to array MS 16-bit Digit
                ; A, D, E disposable
                ; BC has pointer to divisor

                ; copy (BC) 2 bytes to divisor 2 bytes
                ; load divisor into DE from (BC) ptr
    swhlbc          ; swap BC pointer with HL
                ; load divisor (BC) in to DE
    lem         ; E <- M (lo)
    inl         ; L++
    ldm         ; D <- M (hi)
    ldhli   divisor     ; store DE in divisor (little endian)
    lme         ; M <- E (lo) little endian
    inl         ; L++
    lmd         ; M <- D (hi)
    swhlbc          ; swap HL pointer with BC

    lbi 0       ; set carry/remainder to 0, dividend+0
    lci 0       ; set carry/remainder to 0, dividend+1

                ; HL pointing to array MS 16-bit Digit
                ; BC to be 16-bit carry/remainder/dividend
                ; BC to be untouched!!!

/   sthlm   cur_ptr     ; backup array pointer
    ldm         ; load MSB from array (dividend+2)
    inl         
    lem         ; load LSB from array (dividend+3)
    ldhlm   divisor     ; HL = divisor

                ; HL backuped to cur_ptr
                ; BCDE = dividend+0-3, big endian
                ; HL = 16-bit divisor

                ; 32/16 division
    rept    16      ; do it, 16 times
    lae         ; A <- E dividend+3
    ada         ; asl (A = A + A)
    lea         ; E <- A dividend+3
    lad         ; A <- D dividend+2
    ral         ; rol
    lda         ; D <- A dividend+2
    lac         ; A <- C dividend+1
    ral         ; rol
    lca         ; C <- A dividend+1
    lab         ; A <- B dividend+0
    ral         ; rol
    lba         ; B <- A dividend+0
                ; trial division, nothing changed
    lac         ; A <- C
    sul         ; C = C - L (dividend+1 - divisor(lo))
    lab         ; A <- B
    sbh         ; B = B - H (dividend+0 - divisor(hi)) - carry
    jtc m0      ; too small
                ; do it again, but this time save it
    lba         ; B <- A, (hi) done, just save it
    lac         ; A <- C, (lo)
    sul         ; C = C - L (dividend+1 - divisor(lo))
    lca         ; C <- A
    ine         ; E++, dividend+3
m0:
    endm            ; end 32/16 division

    ldhlm   cur_ptr     ; restore HL from cur_ptr
    lmd         ; M <- D, store hi (dividend+2)
    inl         ; and
    lme         ; M <- E, store lo (dividend+3)

                ; check for end do not touch BC, need for chain
    lddei   end_ptr     ; DE <- end_ptr
    swhlde          ; HL <-> DE
    lam         ; A <- lo(end_ptr)
    cpe         ; A = E?, lo(HL)
    jfz +       ; no match, back to top
    inl         ; check hi addr
    lam         ; A <- hi(end_ptr)
    cpd         ; A = D?, hi(HL)
    rtz         ; carry = 0, return if match, all done
/   swhlde          ; HL <-> DE
    inhl            ; next digit
    jmp --      ; back to top


; Description:  Multiprecision 8-bit division:  a = a / b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to divisor (b)
;
; Output:       a = a / b
;
; Registers:    A, B, C, D, E, H, L
;
; Globals:      end_ptr, beg_ptr
;
; Calls:        skipzeros, div8(macro)
;
; C Algorithm:
;
; short divbig(number, x)
; bignum number;
; unsigned short x;
; {
;     dword result;
;     short j = 0;
;     unsigned short rest = 0;
;
;     while (number[j] == 0 && j < MAXSIZE)
;         j++;
;     if (j == MAXSIZE)
;         return (0);
;     while (j < MAXSIZE) {
;         result.w.lo = number[j];
;         result.w.hi = rest;
;         number[j] = result.L / x;
;         rest = result.L % x;
;         j++;
;     }
;     return (1);
; }

div8    macro           ; Input:  B = lo, C = hi, E = divisor
                ; Output: B = lo, C = hi
    lbm         ; B <- M, dividend lo
    rept    8
    lab         ; A <- B
    ada         ; asl (A = A + A)
    lba         ; B <- A, dividend lo
    lac         ; A <- C, dividend hi
    ral         ; rol A (hi)
    jtc m0      ; 9th bit hi?  Sub it.
    cpe         ; A - E, if A < E set carry
    jtc m1      ; too small
m0: sue         ; A = A - E
    inb         ; B++
m1: lca         ; C <- A
    endm
    lmb         ; M <- B save it
    endm

div_mp:
    cal skipzeros   ; skip leading zeros for speed
    rtc         ; return if carry set (all zeros)
                ; set divisor (E) from BC ptr
    swhlbc          ; swap HL pointer with BC
    lem         ; E <- M
    swhlbc          ; swap BC pointer with HL
    lci 0       ; set carry to 0
                ; we know D*2 is even and bin_len is also even
                ; so unroll twice
/   div8            ; div M / E
    inl         ; L++
    div8            ; div M / E
    inhl            ; HL++
    dcd         ; D--
    jfz -       ; D = 0?, if not back to /
    ora         ; clear carry
    ret


; Description:  Print an mp array base 10/100
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       (a) base 10/100 out to screen
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        bintobcd, prbcd, mult100(macro)
;
; Globals:      beg_ptr, end_ptr, bcd_cnt, mt100_lo, mt100_hi

mult100 macro
    lam         ; get A (page aligned table offset)
    lbh         ; BC = HL
    lcl
    lhi hi(mt100_lo)    ; H to point to low table
    lla         ; L = A (position of A*100)
    lam         ; A <- (HL), LSB A = A*100
    ade         ; LSB A = A + E, add mp carry
    inh         ; H++ to get MSB
    lem         ; E <- (HL), MSB E = A*100
    jfc m0      ; if carry set from add, then
    ine         ;   E++
m0:
    lhb         ; HL = BC
    llc
    lma         ; M <- A, store LSB
    endm

print_mp:
                ; HL now set to MSB
    lam         ; load A with MSB
    cpi 10      ; is < 10, then use ASCII method
    jtc +       ;   otherwise, use 2 digit BCD method
    cal bintobcd    ; convert to BCD
    cal prbcd       ; print it
    jmp ++      ; skip over ASCII method
/   adi 030h        ; make ASCII
    cal cout
/   lai '.'     ; print decimal point
    cal cout
    lmi 0       ; zero MSB
    sthlm   beg_ptr     ; save HL value to location beg_ptr
                ; compute end of array since we have
                ;   to go backwards
    lbl         ; B <- L, store (lo) in B for now
    lal         ; A <- L
    adi lo(bin_len) ; add bin_len lo and store it
    lla         ; L <- A
    lah         ; A <- H
    aci hi(bin_len) ; add with carry ben_len hi and store it
    lha         ; H <- A
    sthlm   end_ptr     ; save HL value to location end_ptr
                ; setup bcd counter for outer loop
    ldhli   bcd_cnt     ; load H/L with bcd_cnt address
    lmi lo(bcd_len) ; load bcd_cnt lo
    inl         ; inc pointer
    lmi hi(bcd_len) ; load bcd_cnt hi
    ldb         ; D <- B, ok D to store lo(beg_ptr)

print_mp1:          ; main loop
                ; multi array x 100
                ; loop from LSB to MSB
                ; 16-bit product = array[i] * 100 + carry_mp;
                ; array[i] = product lo
                ; carry_mp = product hi
                ;
                ; C Algorithm:
                ;
                ; while (j >= 0) {
                ;    result.L = (long) number[j] * 100 + carry;
                ;    number[j] = result.w.lo;
                ;    carry = result.w.hi;
                ;    j--;
                ; }

    ldhlm   end_ptr     ; load HL value from location end_ptr
                ; backwards loop (LSB to MSB)
    lei 0       ; E = carry = 0
                ; E is the carry from mult100 to mult100
                ; A,B,C,E (carry) used for mult100
                ; D and E must not be touched!
    ldi unrollc     ; count down to 0
    jmp ++      ; mp_array * 100 loop
/   dchl            ; HL--
    mult100         ; M = lo(M*100), E = hi(M*100)
    dcl         ; L--
    mult100         ; M = lo(M*100), E = hi(M*100)
/   dcl         ; L--
    mult100         ; M = lo(M*100), E = hi(M*100)
    dcl         ; HL--
    mult100         ; M = lo(M*100), E = hi(M*100)
    dcd         ; D=D-1 
    jfz --      ; D=0?
                ; HL now set to MSB
    lam         ; load A with MSB
    cal bintobcd    ; convert to BCD
    cal prbcd       ; print base 100 digit
    lmi 0       ; zero MSB

                ; check for all decimal digits printed
    ldhli   bcd_cnt     ; load H/L with bcd_cnt address
                ;   works with bcd_len = 65536-(dec_len/2)
                ;   counting up is easier
    lbm         ; B <- M, load lo into B
    inb         ; B++, update zero flag
    lmb         ; M <- B, store lo from B
    jfz print_mp1   ; if not zero keep going, otherwise
    inl         ;   inc pointer and
    lbm         ;   load hi, B <- M
    inb         ; B++, update zero flag
    rtz         ; if zero, done, otherwise
    lmb         ;   M <- B, store hi from B
    jmp print_mp1   ;   and keep going

            cpu 8008new             ; use "new" 8008 mnemonics

OUTPORT     equ 08H                 ; serial output port address
            
;------------------------------------------------------------------------        
; 2400 bps character output subroutine
; uses A and B.
; returns with the original character in A
;------------------------------------------------------------------------
cout:       ani 7FH                 ; mask off the most significant bit of the character
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

            cpu 8008                ; use "old" mneumonics for SCELBAL



;; Memory allocation for mp arrays:

    org $+(4-$&3)   ; force arrays on /4 boundary
mp_a:   org $+bin_len
    org $+(4-$&3)   ; force arrays on /4 boundary
mp_b:   org $+bin_len
    org $+(4-$&3)   ; force arrays on /4 boundary
mp_x:   org $+bin_len
    org $+(4-$&3)   ; force arrays on /4 boundary
mp_y:   org $+bin_len

    end 0040H
    