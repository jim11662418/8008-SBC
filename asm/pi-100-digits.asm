;; SCELPi, SCELBI 8008 Pi 1K
;;
;; Egan Ford (egan@sense.net)
;;
;; Mar, 14 2013 (Pi day 2013 limited edition!)
;;
;; SCELPi will compute 120 decimal digits of Pi.
;;
;; All SCELPie computation is base 256 and then converted to base 10 for
;; display (technically base 100, but they both display the same).  Arrays
;; of bytes are used to represent multiprecision numbers.  In the comments
;; below "array" refers to a big endian multiprecision number.
;;
;; Memory required:  1K
;;
;; Time to solution(mm:ss) @ 500 KHz:  00:21.4
;;
;; C snippets:  J.W. Stumpel, May 1991
;;
;; Assembler:  Macro Assembler AS V1.42
;;
;; To assemble:
;;
;;    asl -L -gnuerrors -u -x -x -U pi1k.s
;;    p2hex -r \$-\$ pi1k.p pi1k.hex
;;


                ; Assembler: Macro Assembler AS V1.42
    cpu     8008        ; Intel 8008

;; constants (R/O)

dec_len =   100     ; 100 decimal digits
bin_len =   44      ; ceil(100 / log(256)) + 1 (+ 1 if odd) = 44
bin_end =   bin_len-1
                ; counting up is easier
bcd_len =   65536-(dec_len/2)
cr  =   0Dh     ; ASCII CR

serial  =   1


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
    ldhli   a
    ldbci   b       ; load BC ptr to divisor
    cal div_mp
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


cout    macro
    if serial == 0
    out outport
    else
    cal soutput
    endif
    endm


;; RESET fast calls, 5 states vs. 8 for CAL.  Store varable here too; watch for
;;   overlap with rst calls.

    org 28h     ; rst 0
bcd_cnt:            ; counter used for print_mp
    db  0, 0        ; 0-1
end_ptr:            ; 16-bits used to store pointers to arrays
n:  db  0       ; 2, used by atan_mp
regx:   db  0       ; 3, the x in atan(1/x)
x2: db  0       ; 4, cached x*x
ptr_a:  db  0, 0        ; 5-6, pointer to array used by atan_mp
                ; table of squares
sqrtbl: db  0, 1, 4, 9, 16, 25, 36, 49

    org 38h     ; rst 7
crout:  lai cr      ; A <- CR
    cout            ; print it
    ret


;; START of execution

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
; Output:       mp_a = pi = 4 * (2 * atan(1/3) - atan(1/7))
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        matan/atan_mp, masl/asl_mp, msub/sub_mp


    org 40h     ; 100 octal, SCELBI starts here
main:
    ldhli   pitext      ; load pitext address
    cal print       ; print pitext message

    matan   mp_a,5      ; a = atan(1/5)
    masl    mp_a        ; a = a * 4
    masl    mp_a
    matan   mp_b,239    ; b = atan(1/239)
    msub    mp_a,mp_b   ; a = a - b
    masl    mp_a        ; a = a * 4
    masl    mp_a

    mprint  mp_a        ; print Pi
    printcr 2       ; 2 CRs

    hlt         ; halt, all done

pitext: db  "\r100 DIGITS OF PI = \0"


;; start of subs

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
;               mcopy(m)/copy_mp
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

    mset    mp_x,1      ; x = 1
    mdiv    mp_x,regx   ; x /= regx
    mcopym  ptr_a,mp_x  ; a = x

atan_mp_loop:           ; main loop

    ldhli   n       ; n = n + 2
    lam         ; A <- M lo(n)
    adi 2       ; A += 2
    lma         ; M lo(n) <- A

    ldhli   regx        ; x = x / regx ^ 2
    lam         ; A <- M (regx)
    cpi 16      ; if > 16 /x/x othersize /x2
    jtc +       ; x already x*x, one div required
    mdiv    mp_x,x2     ; x >= 16, then do two div
/   mdiv    mp_x,x2     ;   (faster than 16 bit div)
    

    ;mdiv   mp_x,x2     ; x = x / x2

    mcopy   mp_y,mp_x   ; y = x

    mdiv    mp_y,n      ; y = y / n
    rtc         ; dividend = 0, done

/   ldhli   n       ; add or sub?
    lam         ; A <- M lo(n)
    ndi 2       ; odd/even check on 2nd bit
    jtz +       ; add it
    msubm   ptr_a,mp_y  ; a = a - y
    jmp atan_mp_loop    ; back to top
/   maddm   ptr_a,mp_y  ; a = a + y
    jmp atan_mp_loop    ; back to top


; Description:  Multiprecision transfer:  a = b
;
; Input:        B/C (hi/lo) address to array (a)
;               H/L (hi/lo) address to array (b)
;
; Output:       a = b
;
; Registers:    A, B, C, D, E, H, L

copy_mp:
    ldi bin_len     ; count down to 0
/   lam         ; A <- M (src)
    swhlbce         ; HL <> BC
    lma         ; M <- A (dst)
    inhl            ; HL += 1 (dst)
    swhlbce         ; BC <> HL
    inhl            ; HL += 1 (src)
    dcd         ; D=D-1 
    jfz -       ; D=0?
    ret


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
    lmb         ; set inital value, rest of array will be zero
    ldi bin_end     ; count down to 0
    xra         ; a = 0
/   inhl            ; H/L += 1
    lma         ; load M with A (0)
    dcd         ; D=D-1 
    jfz -       ; D=0?
    ret


; Description:  Multiprecision addition:  a = a + b
;
; Input:        B/C (hi/lo) address to array (a)
;               H/L (hi/lo) address to array (b)
;
; Output:       a = a + b
;
; Registers:    A, B, C, D, E, H, L

add_mp:
                ; compute end of array since we have
    cal fullback    ;   to go backwards
    ldi bin_len     ; count down to 0
    ora         ; clear carry
/   dchl            ; HL-- (b)
    lam         ; A <- M (b)
    swhlbce         ; HL <> BC
    dchl            ; HL-- (a)
    acm         ; B = B + A 
    lma         ; M = B + A
    swhlbce         ; BC <> HL
    dcd         ; D=D-1 
    jfz -       ; D=0?
    ret


; Description:  Multiprecision subtraction:  a = a - b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to array (b)
;
; Output:       a = a - b
;
; Registers:    A, B, C, D, E, H, L

sub_mp:
                ; compute end of array since we have
    cal fullback    ;   to go backwards
    ldi bin_len     ; count down to 0
    ora         ; clear carry
/   dchl            ; HL-- (a)
    lam         ; A <- M (a)
    swhlbce         ; HL <> BC
    dchl            ; HL-- (b)
    sbm         ; A = A - B
    swhlbce         ; BC <> HL
    lma         ; M = A - B
    dcd         ; D=D-1 
    jfz -       ; D=0?
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
    cal halfback    ;   to go backwards
    ldi bin_len     ; count down to 0
    ora         ; clear carry
/   dchl            ; HL--
    lam         ; A <- M
    ral         ; rol
    lma         ; M <- A
    dcd         ; D=D-1 
    jfz -       ; D=0?
    ret


; Description:  Set pointers for reverse loops

fullback:
    lac         ; A <- C
    adi lo(bin_len) ; add bin_len lo and store it
    lca         ; C <- A
    lab         ; A <- B
    aci hi(bin_len) ; add with carry bin_len hi and store it
    lba         ; B <- A
halfback:
    lal         ; A <- L
    adi lo(bin_len) ; add bin_len lo and store it
    lla         ; L <- A
    lah         ; A <- H
    aci hi(bin_len) ; add with carry bin_len hi and store it
    lha         ; H <- A
    ret


; Description:  Multiprecision 8-bit division:  a = a / b
;
; Input:        H/L (hi/lo) address to array (a)
;               B/C (hi/lo) address to divisor (b)
;
; Output:       a = a / b
;
; Registers:    A, B, C, D, E, H, L
;
; Calls:        div8(macro)
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
    cal div816
    endm
    lmb
    endm

div816:
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
    ret

div_mp:
                ; skip leading zeros
    xra         ; A = 0
    ldi bin_len     ; array len
/   cpm         ; is zero?
    jfz notz
    inhl            ; HL += 1
    dcd         ; D--
    jfz -       ; D=0?
    lai 1       ; set carry
    rar         ;   roll 1 in to carry
    ret         ;   all zeros, return
notz:
    swhlbc          ; swap HL pointer with BC
    lem         ; E <- M
    swhlbc          ; swap BC pointer with HL
    lci 0       ; set carry to 0
/   div8            ; div M / E
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
; Globals:      end_ptr, bcd_cnt, mt100_lo, mt100_hi

aslb    macro {noexpand}
    lab         ; A <- B
    ada         ; asl (A = A + A)
    lba         ; B <- A
    endm

rolc    macro {noexpand}
    lac         ; A <- C
    ral         ; rol
    lca         ; C <- A
    endm

mult100 macro {noexpand}    ; 100 = 1100100B, so
                ;   a = ((x*2+x)*8+x)*4
                ; IOW, shift and add,
                ; 1 first MSB, is the initial load
                ; 1 shift (2x) and add +x
                ; 0 shift (2x)
                ; 0 shift (2x)
                ; 1 shift (2x) and add +x
                ; 0 shift (2x)
                ; 0 shift (2x)
                ; end of digits

                ; 1 initial load
    lci 0       ; C = MSB
    lam         ; B/A = LSB
                ; 1 shift (*2) ...
    ada         ; *2, asl (A = A + A)
    lba         ; B <- A
;   rolc            ; A <- C, * 2,  C <- A
;               ; ... and add +x
;   lab         ; A <- B
;   adm         ; A = A + M
;   jfc m1      ; no carry
;   inc         ; inc MSB
;m1:                ; 0 shift (*2)
;   ada         ; *2, asl (A = A + A)
;   lba         ; B <- A
;   rolc            ; A <- C, * 2,  C <- A
;               ; 0 shift (*2)
;   aslb            ; A <- B, A & A (clear carry), * 2, B <- A
;   rolc            ; A <- C, * 2,  C <- A
    cal mdup
                ; 1 shift (*2) ...
    aslb            ; A <- B, A & A (clear carry), * 2, B <- A
;   rolc            ; A <- C, * 2,  C <- A
;               ; ... and add +x    
;   lab         ; A <- B
;   adm         ; A = A + M
;   jfc m2      ; no carry
;   inc         ; inc MSB
;m2:                ; 0 shift (*2)
;   ada         ; *2, asl (A = A + A)
;   lba         ; B <- A
;   rolc            ; A <- C, * 2,  C <- A
;               ; 0 shift (*2)
;   aslb            ; A <- B, A & A (clear carry), * 2, B <- A
;   rolc            ; A <- C, * 2,  C <- A
    cal mdup
                ; end of x 100
                ; add carry for chain computation
    lab         ; A <- B
    ade         ; A = A + E (carry_mp)
    jfc m3      ; no carry
    inc         ; inc MSB
m3: lma         ; M <- A, store LSB
    lec         ; E <- C, tranfer MSB
    endm

mdup:
    rolc            ; A <- C, * 2,  C <- A
    lab         ; A <- B
    adm         ; A = A + M
    jfc md      ; no carry
    inc         ; inc MSB
md:             ; 0 shift (*2)
    ada         ; *2, asl (A = A + A)
    lba         ; B <- A
    rolc            ; A <- C, * 2,  C <- A
                ; 0 shift (*2)
    aslb            ; A <- B, A & A (clear carry), * 2, B <- A
    rolc            ; A <- C, * 2,  C <- A
    ret


print_mp:
                ; HL now set to MSB
    lam         ; load A with MSB
    cpi 10      ; is < 10, then use ASCII method
    jtc +       ;   otherwise, use 2 digit BCD method
    cal bintobcd    ; convert to BCD
    cal prbcd       ; print it
    jmp ++      ; skip over ASCII method
/   adi 030h        ; make ASCII
    cout
/   lai '.'     ; print decimal point
    cout
    lmi 0       ; zero MSB
    cal halfback    ; compute end of array since we have
                ;   to go backwards
    sthlm   end_ptr     ; save HL value to location end_ptr
                ; setup bcd counter for outer loop
    ldhli   bcd_cnt     ; load H/L with bcd_cnt address
    lmi lo(bcd_len) ; load bcd_cnt lo
    inl         ; inc pointer
    lmi hi(bcd_len) ; load bcd_cnt hi

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
    ldi bin_len     ; count down to 0
/   dchl            ; HL--
    mult100
    dcd         ; D=D-1 
    jfz -       ; D=0?
                ; HL now set to MSB
    lam         ; load A with MSB
    cal bintobcd    ; convert to BCD
    ;cal    prbcd       ; print base 100 digit
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


; Description:  Convert BIN/HEX to BCD.
;
; Input:        A = BIN/HEX (00h - 63h)
;
; Output:       A = BCD (00 - 99)
;
; Registers:    A, B, C

bintobcd:
    lba
    lci -1      ; start counter at -1
/   inc         ; C=C+1
    sui 10      ; A=A-10
    jfs -       ; loop if positive
    lab         ; A <- B, restore A
    jmp ++      ; jump to dcc
/   adi 6       ; A=A+6
/   dcc         ; if a < 10, then c will be -1 and end loop
    jfs --      ;  othersize add 6 for every 10
    ;ret            ; fall through to prbcd


; Description:  Output BCD number to outport.
;
; Input:        A = BCD number (00 - 99)
;
; Output:       BCD number to outport
;
; Registers:    A, E

prbcd:  lea         ; E <- A
    rrc         ; shift right 4 bits
    rrc
    rrc
    rrc
    cal +       ; do MSB
    lae         ; then LSB
/   ndi 00Fh        ; mask off
    ori 030h        ; make ASCII
    cout            ; print it
    ret


; Description:  Output a string.
;
; Input:        H/L (hi/lo) address to array (a)
;
; Output:       String to screen
;
; Registers:    A, H, L

print:  lam         ; fetch next message byte
    nda         ; A & A to update status flags
    rtz         ; return if zero found
    cout            ; otherwise display byte
    inl         ; point to next byte
    jfz print       ; is it page boundary?
    inh         ;   if so increment page
    jmp print       ;   and then recycle.


            cpu 8008new             ; use "new" 8008 mnemonics

OUTPORT     equ 08H                 ; serial output port address
            
;------------------------------------------------------------------------        
; 2400 bps character output subroutine
; uses A and B.
; returns with the original character in A
;------------------------------------------------------------------------
soutput:    ani 7FH                 ; mask off the most significant bit of the character
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

mp_a:   org $+bin_len
mp_b:   org $+bin_len
mp_x:   org $+bin_len
mp_y:   org $+bin_len

    end 0040h