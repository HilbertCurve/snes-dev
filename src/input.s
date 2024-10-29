.p816
.smart ; keep track of registers widths

.include "./consts.inc"

.export GetJoypadInput

; GetJoypadInput
; waits for joypad input from SNES joypad 1, stores them in associated consts
; no inputs; stack not moved, so no stack stuff needed except jsr and rts
; clobbers A, Y; sets A to 16 bit mode
.proc GetJoypadInput
WaitForJoypad:
        lda HVBJOY                          ; get joypad status
        and #$01                            ; check whether joypad done reading...
        beq WaitForJoypad; ...if not, wait a bit more

	rep #$20 ; set to 16 bit mode
        lda STDCNTRL1L                      ; get new input from this frame
	ldy JOYPAD1
	; store new buttons
	sta JOYPAD1 ; JOYPAD1 now has new stuff
	; get new buttons
	tya
	eor JOYPAD1 ; filter unchanged buttons
	and JOYPAD1 ; filter buttons not-pressed
	sta JOYTRIGGER1
	tya ; get buttons pressed last frame...
	and JOYPAD1 ; filter buttons not pressed
	sta JOYHELD1 ; ... store in held buttons

	rts
.endproc
