.p816
.A16
.I16

.include "./consts.inc"

.export GetJoypad1
.export GetJoypad2

; GetJoypad1
; waits for joypad input from SNES joypad 1, stores them in associated consts
; Inputs:
;	JoypadPressed: .word, JoypadTriggered: .word, JoypadHeld: .word
; Calling: save stack ptr to x
; clobbers A, Y; sets A to 16 bit mode
.proc GetJoypad1
	phx ; save old stack ptr
	phd ; save old Direct reg
	tsc ; transfer stack to tsc
	tcd

	JoypadPressed = $07
	JoypadTriggered = $09
	JoypadHeld = $0b

	rep #$20 ; set to 16 bit mode

WaitForJoypad1:
        lda HVBJOY                          ; get joypad status
        and #$0001                            ; check whether joypad done reading...
        beq WaitForJoypad1 ; ...if not, wait a bit more

        lda STDCNTRL1L                      ; get new input from this frame
	ldy JoypadPressed
	; store new buttons
	sta (JoypadPressed) ; JOYPAD1 now has new stuff
	; get new buttons
	tya
	eor (JoypadPressed) ; filter unchanged buttons
	and (JoypadPressed) ; filter buttons not-pressed
	sta (JoypadTriggered)
	tya ; get buttons pressed last frame...
	and (JoypadPressed) ; filter buttons not pressed
	sta (JoypadHeld) ; ... store in held buttons

	pld ; restore stuff
	plx
	rts
.endproc

; GetJoypad1
; waits for joypad input from SNES joypad 2, stores them in associated consts
; Inputs:
;	JoypadPressed: .word, JoypadTriggered: .word, JoypadHeld: .word
; Calling: save stack ptr to x
; clobbers A, Y; sets A to 16 bit mode
.proc GetJoypad2
	phx ; save old stack ptr
	phd ; save old Direct reg
	tsc ; transfer stack to tsc
	tcd

	JoypadPressed = $07
	JoypadTriggered = $09
	JoypadHeld = $0b

	rep #$20 ; set to 16 bit mode

WaitForJoypad2:
        lda HVBJOY                          ; get joypad status
        and #$0001                            ; check whether joypad done reading...
        beq WaitForJoypad2 ; ...if not, wait a bit more

        lda STDCNTRL2L                      ; get new input from this frame
	ldy JoypadPressed
	; store new buttons
	sta (JoypadPressed) ; JOYPAD1 now has new stuff
	; get new buttons
	tya
	eor (JoypadPressed) ; filter unchanged buttons
	and (JoypadPressed) ; filter buttons not-pressed
	sta (JoypadTriggered)
	tya ; get buttons pressed last frame...
	and (JoypadPressed) ; filter buttons not pressed
	sta (JoypadHeld) ; ... store in held buttons

	pld ; restore stuff
	plx
	rts
.endproc
