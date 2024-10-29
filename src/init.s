.p816
.smart ; keep track of registers widths

.include "./consts.inc"
.include "./ppu.inc"
.include "./assets.inc"
.include "./main.inc"

.export ResetHandler

;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.segment "CODE"

.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        rep #$10                ; set X and Y to 16-bit
        sep #$20                ; set A to 8-bit
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI
        ; set the stack pointer to $1fff
        ldx #$1fff              ; load X with $1fff
        txs                     ; copy X to stack pointer

        ; load sprites into VRAM
        tsx                     ; save current stack pointer
        pea $0000               ; push VRAM destination address to stack
        pea SpriteData          ; push sprite source address to stack
        lda #$80                ; push number of bytes (128/$80) to transfer to stack
        pha
        jsr LoadVRAM            ; transfer VRAM data
        txs                     ; "delete" data on stack by restoring old stack pointer

	tsx
	pea $0000
	lda #$80
	pha
	jsr ClearCGRAM
	txs

        ; load color data into CGRAM
        tsx                     ; save current stack pointer
	ldy #$0080 ; push dest place
	phy
        pea ColorData ; push sprite source address to stack
	lda #$00 ; give ourselves some buffer
	pha
        lda #$08                ; push number of bytes (128/$80) to transfer to stack
        pha
        jsr LoadCGRAM; transfer VRAM data
        txs                     ; "delete" data on stack by restoring old stack pointer

        ; load sprite data into OAMRAM
        ; jsr LoadOAMRAM          ; transfer sprite data to OAMRAM
	ldx #$00 ; x is our increment for loading into OAMMIRROR
        ; OAM data for first sprite
        lda # (SCREEN_RIGHT/2 - SPRITE_SIZE)       ; horizontal position of first sprite
        sta OAMMIRROR, X
	inx ; increment
        lda # (SCREEN_BOTTOM/2 - SPRITE_SIZE)       ; vertical position of first sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$00                ; name of first sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMMIRROR, X
	inx ; increment
        ; OAM data for second sprite
        lda # (SCREEN_RIGHT/2)           ; horizontal position of second sprite
        sta OAMMIRROR, X
	inx ; increment
        lda # (SCREEN_BOTTOM/2 - SPRITE_SIZE)       ; vertical position of second sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$01                ; name of second sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMMIRROR, X
	inx ; increment
        ; OAM data for third sprite
        lda # (SCREEN_RIGHT/2 - SPRITE_SIZE)       ; horizontal position of third sprite
        sta OAMMIRROR, X
	inx ; increment
        lda # (SCREEN_BOTTOM/2)           ; vertical position of third sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$02                ; name of third sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMMIRROR, X
	inx ; increment
        ; OAM data for fourth sprite
        lda # (SCREEN_RIGHT/2)           ; horizontal position of fourth sprite
        sta OAMMIRROR, X
	inx ; increment
        lda # (SCREEN_BOTTOM/2)           ; vertical position of fourth sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$03                ; name of fourth sprite
        sta OAMMIRROR, X
	inx ; increment
        lda #$00                ; no flip, prio 0, palette 0
        sta OAMMIRROR, X
	inx ; increment
	
	; for rest of oammirror, store zeros
	lda #$ff
HideNonusedSprites:
        ; OAM data for fourth sprite
        sta OAMMIRROR, X
	inx ; increment
	cpx #OAMMIRROR_SIZE
	bne HideNonusedSprites

	; correct extra OAM byte stuff
	ldx #$0200
	lda #$00
	sta OAMMIRROR, X

        sep #$20                ; set A to 8-bit
	lda #SPRITE_SPEED
	sta HOR_SPEED
	sta VER_SPEED

        .byte $42, $00          ; debugger breakpoint

        ; make Objects visible
        lda #$10
        sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc

; InitSNES
; initializes SNES RAM to all zero values

