; ============================================================
; DE10-Lite $MODMAX10 - FULL MERGED PROJECT (UI UPDATED + TIMER FREEZE FIX)
; - LCD Dual-Page UI (latest, no flashing cursor) + RESET button
; - Start/Stop pulses from UI: start_pulse / stop_pulse
; - Reset pulse from UI: reset_pulse (also forces stop + clears elapsed)
; - Reflow FSM uses UI parameters (soak/refl temp+time)
; - MAX10 ADC (CH2) temperature measurement (°C*100)
; - 3-level power: 0%, 20% time-proportioning, 100%
; - SSR OUTPUT = P1.5
; - Timer0 ISR @ 1ms: 200ms flag, 1s flag, PWM timing
;   AND elapsed time increments ONLY while run_flag=1  <-- FREEZE FIX
; - 7-seg: HEX3..HEX0 temperature XXX.X, HEX5 shows fsm_state(0..5)
;
; IMPORTANT FIX FOR YOUR "redefine st1..st5" ERROR:
; - We DO NOT use names st1..st7 anymore
; - We use ui_st1..ui_st7 for the LCD status line
; ============================================================

$MODMAX10

; ----------------------------
; VECTORS
; ----------------------------
org 0000h
    ljmp main

org 000Bh
    ljmp Timer0_ISR

; ----------------------------
; CLOCK / TIMER0 (1ms tick)
; ----------------------------
CLK          EQU 33333333
T0_RATE      EQU 1000
T0_RELOAD    EQU (65536 - (CLK/(12*T0_RATE)))

MS1000_L     EQU 0E8h      ; 1000 = 0x03E8
MS1000_H     EQU 03h

; ============================================================
; LCD/UI PINS (DO NOT CHANGE)
; ============================================================
ELCD_RS equ P1.7
ELCD_E  equ P1.1
ELCD_D4 equ P0.7
ELCD_D5 equ P0.5
ELCD_D6 equ P0.3
ELCD_D7 equ P0.1

; Buttons (active LOW)
BTN_LEFT_BIT    equ P2.0
BTN_RIGHT_BIT   equ P2.1
BTN_UP_BIT      equ P2.2
BTN_DOWN_BIT    equ P2.3
BTN_PAGE_BIT    equ P2.4
BTN_START_BIT   equ P2.5
BTN_RESET_BIT   equ P2.6

; ============================================================
; SSR output
; ============================================================
SSR_OUT equ P1.5

; ============================================================
; ADC regs (MAX10)
; ============================================================
; If your assembler complains, uncomment:
; ADC_C DATA 0A1h
; ADC_L DATA 0A2h
; ADC_H DATA 0A3h

; ============================================================
; INCLUDES
; ============================================================
$NOLIST
$include(LCD_4bit_DE10Lite_no_RW.inc)
$LIST
$include(math32.asm)

; ============================================================
; RAM
; ============================================================
dseg at 30h

; ---- UI profile params (0..999) ----
soak_temp_lo:   ds 1
soak_temp_hi:   ds 1
soak_time_lo:   ds 1
soak_time_hi:   ds 1
refl_temp_lo:   ds 1
refl_temp_hi:   ds 1
refl_time_lo:   ds 1
refl_time_hi:   ds 1

cursor_field:   ds 1    ; 0..3
page_sel:       ds 1    ; 0=profile, 1=status

; ---- UI scratch ----
val_lo:         ds 1
val_hi:         ds 1
dig_hund:       ds 1
dig_tens:       ds 1
dig_ones:       ds 1

; ---- elapsed time (driven by Timer0 ISR) ----
tick50_cnt:     ds 1    ; keep for compatibility (unused)
elapsed_lo:     ds 1
elapsed_hi:     ds 1

min_b:          ds 1
sec_b:          ds 1
tmp_q:          ds 1
tmp_r:          ds 1

; ---- math32 / temp ----
x:              ds 4
y:              ds 4
bcd:            ds 5
temp_x100_lo:   ds 1
temp_x100_hi:   ds 1

; ---- Timer flags/counters ----
ms_count_lo:    ds 1
ms_count_hi:    ds 1

; ---- FSM ----
fsm_state:      ds 1     ; 0..5
pwm_mode:       ds 1     ; 0/20/100
state_sec_lo:   ds 1
state_sec_hi:   ds 1
thr_soak_lo:    ds 1
thr_soak_hi:    ds 1
thr_refl_lo:    ds 1
thr_refl_hi:    ds 1

; ============================================================
; BIT FLAGS (DEFINE ONCE ONLY)
; ============================================================
bseg
; UI state bits (for LCD "State:" char)
ui_st1:          dbit 1
ui_st2:          dbit 1
ui_st3:          dbit 1
ui_st4:          dbit 1
ui_st5:          dbit 1
ui_st6:          dbit 1
ui_st7:          dbit 1

; UI interface bits (UI produces)
run_flag:        dbit 1
start_pulse:     dbit 1
stop_pulse:      dbit 1
reset_pulse:     dbit 1

; math32 uses mf
mf:              dbit 1

; ISR flags
flag_200ms:      dbit 1
flag_1s:         dbit 1

; main control latch for PWM/FSM
on_flag:         dbit 1

; ============================================================
; CONSTANT STRINGS + 7SEG LUT
; ============================================================
cseg

STR_SOAK:   db 'Soak:',0
STR_RELF:   db 'Refl:',0
STR_TIME:   db 'Time:',0
STR_STATE:  db 'State:',0

myLUT:
    DB 0xC0, 0xF9, 0xA4, 0xB0, 0x99
    DB 0x92, 0x82, 0xF8, 0x80, 0x90
    DB 0x88, 0x83, 0xC6, 0xA1, 0x86, 0x8E
SEG_BLANK EQU 0FFh

; ============================================================
; Delay ~50ms @ 33.33 MHz (from your UI)
; ============================================================
Wait50ms:
    mov R0, #30
W50_L3:
    mov R1, #74
W50_L2:
    mov R2, #250
W50_L1:
    djnz R2, W50_L1
    djnz R1, W50_L2
    djnz R0, W50_L3
    ret

; ============================================================
; TIMER0 INIT + ISR (1ms tick)
; - ms_count 0..999
; - flag_200ms at 200/400/600/800/1000
; - flag_1s at 1000, resets ms_count
; - elapsed++ only while run_flag=1  (FREEZE FIX)
; - PWM apply uses on_flag + pwm_mode
; ============================================================
Timer0_Init_1ms:
    anl TMOD, #0F0h
    orl TMOD, #01h

    mov TH0, #high(T0_RELOAD)
    mov TL0, #low(T0_RELOAD)

    setb ET0
    setb EA
    setb TR0
    ret

Timer0_ISR:
    push acc
    push psw

    mov TH0, #high(T0_RELOAD)
    mov TL0, #low(T0_RELOAD)

    ; ms_count++
    inc ms_count_lo
    mov a, ms_count_lo
    jnz T0_chk_marks
    inc ms_count_hi

T0_chk_marks:
    ; flag_200ms at 200/400/600/800/1000
    mov a, ms_count_hi
    cjne a, #00h, T0_chk_400
    mov a, ms_count_lo
    cjne a, #0C8h, T0_chk_400
    setb flag_200ms
    sjmp T0_chk_1000

T0_chk_400:
    mov a, ms_count_hi
    cjne a, #01h, T0_chk_600
    mov a, ms_count_lo
    cjne a, #090h, T0_chk_600
    setb flag_200ms
    sjmp T0_chk_1000

T0_chk_600:
    mov a, ms_count_hi
    cjne a, #02h, T0_chk_800
    mov a, ms_count_lo
    cjne a, #058h, T0_chk_800
    setb flag_200ms
    sjmp T0_chk_1000

T0_chk_800:
    mov a, ms_count_hi
    cjne a, #03h, T0_chk_1000
    mov a, ms_count_lo
    cjne a, #020h, T0_chk_1000
    setb flag_200ms

T0_chk_1000:
    mov a, ms_count_lo
    cjne a, #MS1000_L, T0_pwm
    mov a, ms_count_hi
    cjne a, #MS1000_H, T0_pwm

    setb flag_1s

    clr a
    mov ms_count_lo, a
    mov ms_count_hi, a

    ; elapsed++ only while RUN (Start pressed)
    jnb run_flag, T0_pwm          ; <-- FREEZE when STOP
    inc elapsed_lo
    mov a, elapsed_lo
    jnz T0_pwm
    inc elapsed_hi

; ---- PWM apply ----
T0_pwm:
    jnb on_flag, PWM_OFF

    mov a, pwm_mode
    cjne a, #0, PWM_not0
PWM_OFF:
    clr SSR_OUT
    sjmp T0_done

PWM_not0:
    cjne a, #100, PWM_20
    setb SSR_OUT
    sjmp T0_done

PWM_20:
    ; ON for first 200ms each 1s window
    mov a, ms_count_hi
    jnz PWM20_OFF
    mov a, ms_count_lo
    clr c
    subb a, #0C8h
    jc  PWM20_ON
PWM20_OFF:
    clr SSR_OUT
    sjmp T0_done
PWM20_ON:
    setb SSR_OUT

T0_done:
    pop psw
    pop acc
    reti

; ============================================================
; TEMPERATURE MEASUREMENT (MAX10 ADC CH2) -> x = °C*100, bcd for 7seg
; ============================================================
Measure_Temp_UpdateBCD:
    mov ADC_C, #2

    mov x+3, #0
    mov x+2, #0
    mov x+1, ADC_H
    mov x+0, ADC_L

    ; x = (ADC*5000)/4096 mV
    Load_y(5000)
    lcall mul32
    Load_y(4096)
    lcall div32

    ; x *= 1000 -> uV
    Load_y(1000)
    lcall mul32

    ; x = (uV*100)/12464 -> °C*100
    Load_y(100)
    lcall mul32
    Load_y(12464)
    lcall div32

    ; +22.00C
    Load_y(2200)
    lcall add32

    mov temp_x100_lo, x+0
    mov temp_x100_hi, x+1

    lcall hex2bcd
    ret

; ============================================================
; 7-seg display: XXX.X on HEX3..HEX0
; HEX5 displays fsm_state (0..5), HEX4 blank
; ============================================================
Display_Temp_7seg_XXX_X:
    mov dptr, #myLUT

    ; HEX3 hundreds (bcd+2 low nibble) blank if 0
    mov a, bcd+2
    anl a, #0FH
    jz  DT_blank_hund
    movc a, @a+dptr
    mov HEX3, a
    sjmp DT_tens
DT_blank_hund:
    mov HEX3, #SEG_BLANK

DT_tens:
    ; HEX2 tens (bcd+1 high nibble) blank if leading
    mov a, bcd+1
    swap a
    anl a, #0FH
    mov R7, a
    mov a, HEX3
    cjne a, #SEG_BLANK, DT_tens_show
    mov a, R7
    jz  DT_blank_tens
DT_tens_show:
    mov a, R7
    movc a, @a+dptr
    mov HEX2, a
    sjmp DT_ones
DT_blank_tens:
    mov HEX2, #SEG_BLANK

DT_ones:
    ; HEX1 ones (bcd+1 low) with dp ON (active-low => clear bit7)
    mov a, bcd+1
    anl a, #0FH
    movc a, @a+dptr
    anl a, #7FH
    mov HEX1, a

    ; HEX0 tenths (bcd+0 high)
    mov a, bcd+0
    swap a
    anl a, #0FH
    movc a, @a+dptr
    mov HEX0, a
    ret

Display_State_HEX5:
    mov HEX4, #SEG_BLANK
    mov dptr, #myLUT
    mov a, fsm_state
    anl a, #0FH
    movc a, @a+dptr
    mov HEX5, a
    ret

; ============================================================
; ================== UPDATED UI BLOCK (LATEST + RESET) ==================
; ============================================================

; Read buttons -> A bitmask (1=pressed)
; bit0 LEFT, bit1 RIGHT, bit2 UP, bit3 DOWN, bit4 PAGE, bit5 START, bit6 RESET
ReadButtons:
    mov a,#0
    jb  BTN_LEFT_BIT,  RB_NL
    orl a,#01h
RB_NL:
    jb  BTN_RIGHT_BIT, RB_NR
    orl a,#02h
RB_NR:
    jb  BTN_UP_BIT,    RB_NU
    orl a,#04h
RB_NU:
    jb  BTN_DOWN_BIT,  RB_ND
    orl a,#08h
RB_ND:
    jb  BTN_PAGE_BIT,  RB_NP
    orl a,#10h
RB_NP:
    jb  BTN_START_BIT, RB_NS
    orl a,#20h
RB_NS:
    jb  BTN_RESET_BIT, RB_DONE
    orl a,#40h
RB_DONE:
    ret

WaitRelease:
WR_LOOP:
    lcall ReadButtons
    jnz WR_LOOP
    ret

; Clamp 0..999
Clamp_0_999:
    mov a, val_hi
    clr c
    subb a, #03h
    jc  CL_OK
    jnz CL_SET999
    mov a, val_lo
    clr c
    subb a, #0E7h
    jc  CL_OK
    jz  CL_OK
CL_SET999:
    mov val_hi,#03h
    mov val_lo,#0E7h
CL_OK:
    ret

IncVal_Clamp:
    inc val_lo
    mov a, val_lo
    jnz IV_OK
    inc val_hi
IV_OK:
    lcall Clamp_0_999
    ret

DecVal_Clamp:
    mov a, val_hi
    orl a, val_lo
    jz  DV_DONE
    mov a, val_lo
    jnz DV_NB
    dec val_hi
DV_NB:
    dec val_lo
DV_DONE:
    ret

; val -> 3 digits ASCII (DESTROYS val)
ValTo3Digits:
    mov dig_hund,#'0'
    mov dig_tens,#'0'
    mov dig_ones,#'0'

VT_HUND_LOOP:
    mov a,val_hi
    jnz VT_HUND_SUB
    mov a,val_lo
    clr c
    subb a,#100
    jc  VT_TENS_START
VT_HUND_SUB:
    mov a,val_lo
    clr c
    subb a,#100
    mov val_lo,a
    mov a,val_hi
    subb a,#0
    mov val_hi,a
    inc dig_hund
    sjmp VT_HUND_LOOP

VT_TENS_START:
VT_TENS_LOOP:
    mov a,val_hi
    jnz VT_TENS_SUB
    mov a,val_lo
    clr c
    subb a,#10
    jc  VT_ONES
VT_TENS_SUB:
    mov a,val_lo
    clr c
    subb a,#10
    mov val_lo,a
    mov a,val_hi
    subb a,#0
    mov val_hi,a
    inc dig_tens
    sjmp VT_TENS_LOOP

VT_ONES:
    mov a,val_lo
    add a,#'0'
    mov dig_ones,a
    ret

LCD_Print3Digits:
    mov a,dig_hund
    lcall ?WriteData
    mov a,dig_tens
    lcall ?WriteData
    mov a,dig_ones
    lcall ?WriteData
    ret

; Cursor positioning (first digit of each field)
SetCursorToField:
    mov a, cursor_field
    cjne a,#0, SC_F1
    Set_Cursor(1,6)
    ret
SC_F1:
    cjne a,#1, SC_F2
    Set_Cursor(1,11)
    ret
SC_F2:
    cjne a,#2, SC_F3
    Set_Cursor(2,6)
    ret
SC_F3:
    Set_Cursor(2,11)
    ret

LoadFieldToVal:
    mov a,cursor_field
    cjne a,#0, LF1
    mov val_lo, soak_temp_lo
    mov val_hi, soak_temp_hi
    ret
LF1:
    cjne a,#1, LF2
    mov val_lo, soak_time_lo
    mov val_hi, soak_time_hi
    ret
LF2:
    cjne a,#2, LF3
    mov val_lo, refl_temp_lo
    mov val_hi, refl_temp_hi
    ret
LF3:
    mov val_lo, refl_time_lo
    mov val_hi, refl_time_hi
    ret

StoreValToField:
    mov a,cursor_field
    cjne a,#0, SF1
    mov soak_temp_lo, val_lo
    mov soak_temp_hi, val_hi
    ret
SF1:
    cjne a,#1, SF2
    mov soak_time_lo, val_lo
    mov soak_time_hi, val_hi
    ret
SF2:
    cjne a,#2, SF3
    mov refl_temp_lo, val_lo
    mov refl_temp_hi, val_hi
    ret
SF3:
    mov refl_time_lo, val_lo
    mov refl_time_hi, val_hi
    ret

; Page0 render (no flashing)
RenderProfilePage:
    mov a,#0Ch
    lcall ?WriteCommand

    Set_Cursor(1,1)
    Send_Constant_String(#STR_SOAK)

    mov val_lo, soak_temp_lo
    mov val_hi, soak_temp_hi
    lcall ValTo3Digits
    lcall LCD_Print3Digits
    mov a,#'C'
    lcall ?WriteData
    mov a,#' '
    lcall ?WriteData

    mov val_lo, soak_time_lo
    mov val_hi, soak_time_hi
    lcall ValTo3Digits
    lcall LCD_Print3Digits
    mov a,#'s'
    lcall ?WriteData

    mov R7,#3
P0_R1PAD:
    mov a,#' '
    lcall ?WriteData
    djnz R7, P0_R1PAD

    Set_Cursor(2,1)
    Send_Constant_String(#STR_RELF)

    mov val_lo, refl_temp_lo
    mov val_hi, refl_temp_hi
    lcall ValTo3Digits
    lcall LCD_Print3Digits
    mov a,#'C'
    lcall ?WriteData
    mov a,#' '
    lcall ?WriteData

    mov val_lo, refl_time_lo
    mov val_hi, refl_time_hi
    lcall ValTo3Digits
    lcall LCD_Print3Digits
    mov a,#'s'
    lcall ?WriteData

    mov R7,#3
P0_R2PAD:
    mov a,#' '
    lcall ?WriteData
    djnz R7, P0_R2PAD

    mov a,#0Eh
    lcall ?WriteCommand
    lcall SetCursorToField
    ret

; elapsed/60 => min,sec (destroys elapsed while computing)
DivMod60_16:
    mov tmp_q,#0
DM60_LOOP:
    mov a, elapsed_hi
    jnz DM60_SUB
    mov a, elapsed_lo
    clr c
    subb a,#60
    jc  DM60_DONE
DM60_SUB:
    mov a, elapsed_lo
    clr c
    subb a,#60
    mov elapsed_lo,a
    mov a, elapsed_hi
    subb a,#0
    mov elapsed_hi,a
    inc tmp_q
    sjmp DM60_LOOP
DM60_DONE:
    mov min_b, tmp_q
    mov sec_b, elapsed_lo
    ret

Print2DigitsA:
    mov tmp_q,#'0'
P2D_LOOP:
    clr c
    subb a,#10
    jc  P2D_DONE
    inc tmp_q
    sjmp P2D_LOOP
P2D_DONE:
    add a,#10
    mov tmp_r,a
    mov a,tmp_q
    lcall ?WriteData
    mov a,tmp_r
    add a,#'0'
    lcall ?WriteData
    ret

; Determine state display char using ui_st1..ui_st7
GetStateChar:
    mov R7,#0
    mov R6,#0

    jb ui_st1, GS1
    sjmp GS2
GS1: inc R7
     mov R6,#1
GS2: jb ui_st2, GS3
     sjmp GS4
GS3: inc R7
     mov R6,#2
GS4: jb ui_st3, GS5
     sjmp GS6
GS5: inc R7
     mov R6,#3
GS6: jb ui_st4, GS7
     sjmp GS8
GS7: inc R7
     mov R6,#4
GS8: jb ui_st5, GS9
     sjmp GS10
GS9: inc R7
     mov R6,#5
GS10: jb ui_st6, GS11
      sjmp GS12
GS11: inc R7
      mov R6,#6
GS12: jb ui_st7, GS_DONE
      sjmp GS_DONE2
GS_DONE:
    inc R7
    mov R6,#7
GS_DONE2:
    mov a,R7
    jz  GS_NONE
    cjne a,#1, GS_ERR
    mov a,R6
    add a,#'0'
    ret
GS_NONE:
    mov a,#'0'
    ret
GS_ERR:
    mov a,#'E'
    ret

RenderStatusPage:
    mov a,#0Ch
    lcall ?WriteCommand

    mov R4, elapsed_lo
    mov R5, elapsed_hi
    lcall DivMod60_16
    mov elapsed_lo, R4
    mov elapsed_hi, R5

    Set_Cursor(1,1)
    Send_Constant_String(#STR_TIME)
    mov a, min_b
    lcall Print2DigitsA
    mov a,#'m'
    lcall ?WriteData
    mov a, sec_b
    lcall Print2DigitsA
    mov a,#'s'
    lcall ?WriteData

    mov R7,#5
S1_PAD:
    mov a,#' '
    lcall ?WriteData
    djnz R7, S1_PAD

    Set_Cursor(2,1)
    Send_Constant_String(#STR_STATE)
    lcall GetStateChar
    lcall ?WriteData
    mov a,#' '
    lcall ?WriteData

    jb run_flag, SHOW_START
SHOW_STOP:
    mov a,#'S'
    lcall ?WriteData
    mov a,#'t'
    lcall ?WriteData
    mov a,#'o'
    lcall ?WriteData
    mov a,#'p'
    lcall ?WriteData
    sjmp S2_PAD
SHOW_START:
    mov a,#'S'
    lcall ?WriteData
    mov a,#'t'
    lcall ?WriteData
    mov a,#'a'
    lcall ?WriteData
    mov a,#'r'
    lcall ?WriteData
    mov a,#'t'
    lcall ?WriteData

S2_PAD:
    mov R7,#7
S2_PADLOOP:
    mov a,#' '
    lcall ?WriteData
    djnz R7, S2_PADLOOP
    ret

; START/STOP toggle (debounced)
HandleStartStop:
    clr start_pulse
    clr stop_pulse

    jb  BTN_START_BIT, HS_DONE
    lcall Wait50ms
    jb  BTN_START_BIT, HS_DONE
    lcall WaitRelease

    jb run_flag, HS_TO_STOP
HS_TO_START:
    setb run_flag
    setb start_pulse
    sjmp HS_DONE
HS_TO_STOP:
    clr run_flag
    setb stop_pulse
HS_DONE:
    ret

; RESET (debounced)
HandleReset:
    clr reset_pulse

    jb  BTN_RESET_BIT, HR_DONE
    lcall Wait50ms
    jb  BTN_RESET_BIT, HR_DONE
    lcall WaitRelease

    ; Stop
    clr run_flag
    setb stop_pulse
    setb reset_pulse

    ; Clear elapsed time
    mov elapsed_lo,#0
    mov elapsed_hi,#0

    ; Optional UI behavior
    mov page_sel,#0
    mov cursor_field,#0

HR_DONE:
    ret

HandlePageToggle:
    jb  BTN_PAGE_BIT, HP_DONE
    lcall Wait50ms
    jb  BTN_PAGE_BIT, HP_DONE
    lcall WaitRelease
    mov a,page_sel
    xrl a,#1
    mov page_sel,a
HP_DONE:
    ret

HandleEditorButtons:
    lcall ReadButtons
    jz HEB_DONE
    lcall Wait50ms
    lcall ReadButtons
    jz HEB_DONE

    jb ACC.0, HEB_LEFT
    jb ACC.1, HEB_RIGHT
    jb ACC.2, HEB_UP
    jb ACC.3, HEB_DOWN
    sjmp HEB_WAITREL

HEB_LEFT:
    mov a,cursor_field
    jnz HEBL_OK
    mov cursor_field,#3
    sjmp HEB_WAITREL
HEBL_OK:
    dec cursor_field
    sjmp HEB_WAITREL

HEB_RIGHT:
    mov a,cursor_field
    cjne a,#3, HEBR_OK
    mov cursor_field,#0
    sjmp HEB_WAITREL
HEBR_OK:
    inc cursor_field
    sjmp HEB_WAITREL

HEB_UP:
    lcall LoadFieldToVal
    lcall IncVal_Clamp
    lcall StoreValToField
    sjmp HEB_WAITREL

HEB_DOWN:
    lcall LoadFieldToVal
    lcall DecVal_Clamp
    lcall StoreValToField
    sjmp HEB_WAITREL

HEB_WAITREL:
    lcall WaitRelease
HEB_DONE:
    ret

; ============================================================
; FSM SUPPORT: threshold = UI_tempC * 100 (val<<6 + val<<5 + val<<2)
; ============================================================
MulValBy100_ToThr:
    ; returns R4:R3 = val*100
    mov R3, #0
    mov R4, #0

    ; term1 val<<6
    mov R0, val_lo
    mov R1, val_hi
    mov R2, #6
MV_SHL6:
    clr c
    mov a, R0
    rlc a
    mov R0, a
    mov a, R1
    rlc a
    mov R1, a
    djnz R2, MV_SHL6
    mov a, R3
    add a, R0
    mov R3, a
    mov a, R4
    addc a, R1
    mov R4, a

    ; term2 val<<5
    mov R0, val_lo
    mov R1, val_hi
    mov R2, #5
MV_SHL5:
    clr c
    mov a, R0
    rlc a
    mov R0, a
    mov a, R1
    rlc a
    mov R1, a
    djnz R2, MV_SHL5
    mov a, R3
    add a, R0
    mov R3, a
    mov a, R4
    addc a, R1
    mov R4, a

    ; term3 val<<2
    mov R0, val_lo
    mov R1, val_hi
    mov R2, #2
MV_SHL2:
    clr c
    mov a, R0
    rlc a
    mov R0, a
    mov a, R1
    rlc a
    mov R1, a
    djnz R2, MV_SHL2
    mov a, R3
    add a, R0
    mov R3, a
    mov a, R4
    addc a, R1
    mov R4, a
    ret

UpdateThresholds_FromUI:
    mov val_lo, soak_temp_lo
    mov val_hi, soak_temp_hi
    lcall MulValBy100_ToThr
    mov thr_soak_lo, R3
    mov thr_soak_hi, R4

    mov val_lo, refl_temp_lo
    mov val_hi, refl_temp_hi
    lcall MulValBy100_ToThr
    mov thr_refl_lo, R3
    mov thr_refl_hi, R4
    ret

; FSM constants
SAFETY_SEC_LO EQU 32h      ; 50s
SAFETY_SEC_HI EQU 00h

; FSM helpers
FSM_ClearStateBits:
    clr ui_st1
    clr ui_st2
    clr ui_st3
    clr ui_st4
    clr ui_st5
    clr ui_st6
    clr ui_st7
    ret

FSM_SetStateBits:
    ; map fsm_state 0..5 -> ui_st1..ui_st6 (ui_st7 unused)
    lcall FSM_ClearStateBits
    mov a, fsm_state
    cjne a, #0, FSB_1
    setb ui_st1
    ret
FSB_1:
    cjne a, #1, FSB_2
    setb ui_st2
    ret
FSB_2:
    cjne a, #2, FSB_3
    setb ui_st3
    ret
FSB_3:
    cjne a, #3, FSB_4
    setb ui_st4
    ret
FSB_4:
    cjne a, #4, FSB_5
    setb ui_st5
    ret
FSB_5:
    setb ui_st6
    ret

FSM_EnterState:
    clr a
    mov state_sec_lo, a
    mov state_sec_hi, a
    ret

IncStateSeconds:
    inc state_sec_lo
    mov a, state_sec_lo
    jnz ISS_done
    inc state_sec_hi
ISS_done:
    ret

; Compare temp_x100 >= (R1:R0)
Temp_GE_Thr:
    clr c
    mov a, temp_x100_hi
    subb a, R1
    jc  TGE_false
    jnz TGE_true
    mov a, temp_x100_lo
    clr c
    subb a, R0
    jc  TGE_false
TGE_true:
    setb c
    ret
TGE_false:
    clr c
    ret

; Compare state_sec >= (R1:R0)
StateSec_GE_Target:
    clr c
    mov a, state_sec_hi
    subb a, R1
    jc  SGE_false
    jnz SGE_true
    mov a, state_sec_lo
    clr c
    subb a, R0
    jc  SGE_false
SGE_true:
    setb c
    ret
SGE_false:
    clr c
    ret

; ===== UPDATED: DO NOT CLEAR elapsed =====
FSM_IDLE_ONLY:
    mov fsm_state, #0
    mov pwm_mode, #0
    clr on_flag

    clr a
    mov state_sec_lo, a
    mov state_sec_hi, a

    ; elapsed_lo/hi 不清零
    lcall FSM_SetStateBits
    ret

FSM_ResetToIdle:
    mov fsm_state, #0
    mov pwm_mode, #0
    clr on_flag

    clr a
    mov state_sec_lo, a
    mov state_sec_hi, a

    ; elapsed_lo/hi 不清零
    lcall FSM_SetStateBits
    ret

; ============================================================
; FSM DISPATCH TABLE (AJMP entries, index = fsm_state*2)
; ============================================================
FSM_Dispatch:
    AJMP ST0
    AJMP ST1
    AJMP ST2
    AJMP ST3
    AJMP ST4
    AJMP ST5

FSM_Step_1Hz:
    ; consume start/stop/reset pulses from UI
    jb  reset_pulse, F_RST
    sjmp F_CHKSTART
F_RST:
    clr reset_pulse
    clr on_flag
    lcall FSM_ResetToIdle
    ret

F_CHKSTART:
    jb  start_pulse, F_START
    sjmp F_CHKSTOP
F_START:
    clr start_pulse
    setb on_flag
    mov fsm_state, #1
    mov pwm_mode, #100
    lcall FSM_EnterState
    sjmp F_RUN

F_CHKSTOP:
    jb  stop_pulse, F_STOP
    sjmp F_RUNCHK
F_STOP:
    clr stop_pulse
    clr on_flag
    lcall FSM_ResetToIdle
    ret

F_RUNCHK:
    jb  on_flag, F_RUN
    ljmp FSM_IDLE_ONLY

F_RUN:
    lcall UpdateThresholds_FromUI
    lcall IncStateSeconds

    mov a, fsm_state
    anl a, #07h
    rl  a              ; A = A*2
    mov dptr, #FSM_Dispatch
    jmp @a+dptr

; ---- STATE 0 IDLE ----
ST0:
    mov pwm_mode, #0
    jnb on_flag, ST0_done
    mov fsm_state, #1
    mov pwm_mode, #100
    lcall FSM_EnterState
ST0_done:
    lcall FSM_SetStateBits
    ret

; ---- STATE 1 RAMP_TO_SOAK (100%) ----
ST1:
    mov pwm_mode, #100

    ; safety: if >=50s and not reached thr_soak => abort
    mov R0, #SAFETY_SEC_LO
    mov R1, #SAFETY_SEC_HI
    lcall StateSec_GE_Target
    jnc ST1_chk_temp

    mov R0, thr_soak_lo
    mov R1, thr_soak_hi
    lcall Temp_GE_Thr
    jc  ST1_to2
    lcall FSM_ResetToIdle
    ret

ST1_chk_temp:
    mov R0, thr_soak_lo
    mov R1, thr_soak_hi
    lcall Temp_GE_Thr
    jnc ST1_stay
ST1_to2:
    mov fsm_state, #2
    mov pwm_mode, #20
    lcall FSM_EnterState
ST1_stay:
    lcall FSM_SetStateBits
    ret

; ---- STATE 2 HOLD_SOAK (20%) for soak_time ----
ST2:
    mov pwm_mode, #20
    mov R0, soak_time_lo
    mov R1, soak_time_hi
    lcall StateSec_GE_Target
    jnc ST2_stay
    mov fsm_state, #3
    mov pwm_mode, #100
    lcall FSM_EnterState
ST2_stay:
    lcall FSM_SetStateBits
    ret

; ---- STATE 3 RAMP_TO_REFL (100%) until thr_refl ----
ST3:
    mov pwm_mode, #100
    mov R0, thr_refl_lo
    mov R1, thr_refl_hi
    lcall Temp_GE_Thr
    jnc ST3_stay
    mov fsm_state, #4
    mov pwm_mode, #20
    lcall FSM_EnterState
ST3_stay:
    lcall FSM_SetStateBits
    ret

; ---- STATE 4 HOLD_REFL (20%) for refl_time ----
ST4:
    mov pwm_mode, #20
    mov R0, refl_time_lo
    mov R1, refl_time_hi
    lcall StateSec_GE_Target
    jnc ST4_stay
    mov fsm_state, #5
    mov pwm_mode, #0
    lcall FSM_EnterState
ST4_stay:
    lcall FSM_SetStateBits
    ret

; ---- STATE 5 COOL (0%) until temp < 60.01C ----
ST5:
    mov pwm_mode, #0
    mov R0, #071h       ; 6001 = 0x1771
    mov R1, #017h
    lcall Temp_GE_Thr
    jc  ST5_keep
    lcall FSM_ResetToIdle
    ret
ST5_keep:
    lcall FSM_SetStateBits
    ret

; ============================================================
; MAIN
; ============================================================
main:
    mov SP,#7Fh

    ; LCD pins outputs
    mov P0MOD,#10101010b

    ; P1 outputs: P1.7, P1.5, P1.1  => 1010 0010b
    mov P1MOD,#10100010b

    ; Buttons inputs
    mov P2MOD,#00000000b

    ; SSR off
    clr SSR_OUT

    ; LCD init
    lcall ELCD_4BIT

    ; init UI defaults
    mov soak_temp_hi,#0
    mov soak_temp_lo,#150
    mov soak_time_hi,#0
    mov soak_time_lo,#90
    mov refl_temp_hi,#0
    mov refl_temp_lo,#230
    mov refl_time_hi,#0
    mov refl_time_lo,#40

    mov cursor_field,#0
    mov page_sel,#0

    ; clear bits
    clr run_flag
    clr start_pulse
    clr stop_pulse
    clr reset_pulse
    clr flag_200ms
    clr flag_1s
    clr on_flag

    ; clear UI state bits
    clr ui_st1
    clr ui_st2
    clr ui_st3
    clr ui_st4
    clr ui_st5
    clr ui_st6
    clr ui_st7

    ; init counters
    clr a
    mov ms_count_lo, a
    mov ms_count_hi, a
    mov elapsed_lo, a
    mov elapsed_hi, a
    mov state_sec_lo, a
    mov state_sec_hi, a
    mov fsm_state, #0
    mov pwm_mode, #0

    lcall FSM_SetStateBits

    ; start Timer0
    lcall Timer0_Init_1ms

    ; Reset ADC
    mov ADC_C, #80h
    lcall Wait50ms

; ----------------------------
; MAIN LOOP
; ----------------------------
MAIN_LOOP:
    ; UI always active
    lcall HandleReset
    lcall HandlePageToggle
    lcall HandleStartStop

    ; FAST sync:
    ; - makes SSR/PWM stop immediately when you press STOP
    jb  run_flag, FAST_ON
    clr on_flag
    sjmp FAST_DONE
FAST_ON:
    setb on_flag
FAST_DONE:

    ; Render page
    mov a, page_sel
    jz  DO_PAGE0
DO_PAGE1:
    lcall RenderStatusPage
    sjmp AFTER_RENDER
DO_PAGE0:
    lcall RenderProfilePage
    lcall HandleEditorButtons

AFTER_RENDER:
    ; 200ms tasks
    jb  flag_200ms, DO_200MS
    sjmp CHECK_1S
DO_200MS:
    clr flag_200ms
    lcall Measure_Temp_UpdateBCD
    lcall Display_Temp_7seg_XXX_X
    lcall Display_State_HEX5

CHECK_1S:
    jb  flag_1s, DO_1S
    ljmp LOOP_PACE
DO_1S:
    clr flag_1s
    lcall FSM_Step_1Hz

LOOP_PACE:
    ; small pacing to reduce LCD rewrite rate
    lcall Wait50ms
    ljmp MAIN_LOOP

END
