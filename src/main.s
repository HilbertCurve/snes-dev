;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.smart ; keep track of registers widths
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.include "./consts.inc"
.include "./init.inc"
.include "./ppu.inc"
.include "./input.inc"
;-------------------------------------------------------------------------------

.export GameLoop

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank
	jsr GetJoypadInput
	lda JOYHELD1
	and #BUTTON_A ; filter highest bit; corresponds to A button
	cpa #BUTTON_A
	bne GameLoop ; stall unless button A is pressed
	sep #$20 ; keeping track of regs is hard D:
HorUpdate:
	lda HOR_SPEED
	bpl CheckRightBounds
CheckLeftBounds:
	lda OAMMIRROR
	clc
	adc HOR_SPEED
	bcs UpdateHorPos ; branch if we be underflowin
	    stz OAMMIRROR
	    bra InvertHorSpeed
CheckRightBounds:
	lda OAMMIRROR
	clc
	adc HOR_SPEED
	cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
	bcc UpdateHorPos ; if we at place...
	    lda #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
	    sta OAMMIRROR
	    bra InvertHorSpeed
UpdateHorPos:
	sta OAMMIRROR
	bra VerUpdate
InvertHorSpeed:
	lda HOR_SPEED
	eor #$ff
	inc
	sta HOR_SPEED
	bra VerUpdate

VerUpdate:
	lda VER_SPEED
	bpl CheckUpBounds
CheckDownBounds:
	lda OAMMIRROR + $01
	clc
	adc VER_SPEED
	bcs UpdateVerPos ; branch if we be underflowin
	    stz OAMMIRROR + $01
	    bra InvertVerSpeed
CheckUpBounds:
	lda OAMMIRROR + $01
	clc
	adc VER_SPEED
	cmp #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
	bcc UpdateVerPos ; if we at place...
	    lda #(SCREEN_BOTTOM - 2 * SPRITE_SIZE)
	    sta OAMMIRROR + $01
	    bra InvertVerSpeed
UpdateVerPos:
	sta OAMMIRROR + $01
	bra Return
InvertVerSpeed:
	lda VER_SPEED
	eor #$ff
	inc
	sta VER_SPEED
	bra Return
Return:
	; mirror pos of other sprites
	lda OAMMIRROR
	; store sprite below
	sta OAMMIRROR + $08
	; store right-most sprites
	clc
	adc #$08
	sta OAMMIRROR + $04
	sta OAMMIRROR + $0c
	; mirror pos of other sprites
	lda OAMMIRROR + $01
	; store sprite to right
	sta OAMMIRROR + $05
	; store right-most sprites
	clc
	adc #$08
	sta OAMMIRROR + $09
	sta OAMMIRROR + $0d

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------


;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           NMIHandler, $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
