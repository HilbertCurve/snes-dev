;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.A8
.I16
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
.A16
	rep #$20 ; set to 16 bit mode

	tsx
	pea JOYHELD1
	pea JOYTRIGGER1
	pea JOYPAD1
	jsr GetJoypad1
	txs

	.byte $42, $00 ; breakpoint

	; here we skip everything if neither left nor right is pressed
	lda JOYPAD1
	and #(BUTTON_LEFT + BUTTON_RIGHT)
	cmp #$0000
.A8
	sep #$20
	bne CheckLeft
		; we slow down and skip to UpdateHor if no buttons are pressed
		lda HOR_SPEED
		cmp #$00
		beq CheckB ; if we're at zero speed, move nowhere
		lda HOR_SPEED
		cmp #$00
		bpl PositiveSpeed
	NegativeSpeed:
		inc
		sta HOR_SPEED
		bra UpdateHor
	PositiveSpeed:
		dec
		sta HOR_SPEED
		bra UpdateHor

CheckLeft:
.A16
	rep #$20 ; set to 16 bit mode
	lda JOYPAD1
	and #BUTTON_LEFT ; filter highest bit; corresponds to A button
	cpa #BUTTON_LEFT
.A8 ; we buggin this now
	sep #$20
	bne NoLeft ; stall unless button A is pressed
		lda HOR_SPEED ; load horizontal speed
		dec ; slow down
		eor #$ff ; negate
		inc
		cpa #SPRITE_SPEED ; compare to max speed
		beq UpdateHor ; if equal, continue
			lda HOR_SPEED ; load horizontal speed
			dec ; slow down
			sta HOR_SPEED
			bra UpdateHor
NoLeft:
CheckRight:
.A16
	rep #$20
	lda JOYPAD1
	and #BUTTON_RIGHT ; filter highest bit; corresponds to A button
	cpa #BUTTON_RIGHT
.A8 ; we buggin this now
	sep #$20
	bne UpdateHor ; stall unless button A is pressed
		lda HOR_SPEED ; load horizontal speed
		inc ; slow down
		cpa #SPRITE_SPEED ; compare to max speed
		beq UpdateHor ; if equal, continue
			sta HOR_SPEED
			bra UpdateHor
UpdateHor:
	lda OAMMIRROR
	clc
	adc HOR_SPEED
	sta OAMMIRROR
CheckB:
	lda #$01
	cmp GROUNDED
	bne Falling
.A16
	rep #$20
	lda JOYPAD1
	and #BUTTON_B ; filter highest bit; corresponds to A button
	cpa #BUTTON_B
.A8 ; we buggin this now
	sep #$20
	bne Falling ; stall unless button A is pressed
		lda #JUMP_SPEED
		sta VER_SPEED
Falling:
	lda OAMMIRROR + $01 ; get y pos of sprite
	clc ; clear carry
	sbc VER_SPEED ; subtract height of sprite
	cmp #(SCREEN_BOTTOM - SPRITE_SIZE * 2 - 1) ; cmp with sprite height
	bpl Grounded ; if we got sub carry, snap to ground
		stz GROUNDED
		sta OAMMIRROR + $01
		lda VER_SPEED ; increase falling speed
		cmp #(MAX_FALLING_SPEED)
		beq Return
		dec
		sta VER_SPEED
		bra Return ; otherwise, continue as normal
Grounded:
	lda #$01
	sta GROUNDED
	stz VER_SPEED
	lda #(SCREEN_BOTTOM - SPRITE_SIZE * 2)
	sta OAMMIRROR + $01

HorUpdate:
UpdateHorPos:
VerUpdate:
UpdateVerPos:
	bra Return
Return:
.A8 ; we buggin this now
	sep #$20
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
	; store bottom-most sprites
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
