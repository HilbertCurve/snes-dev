;----- Aliases/Labels ----------------------------------------------------------
; these are aliases for the Memory Mapped Registers we will use
INIDISP     = $2100     ; inital settings for screen
OBJSEL      = $2101     ; object size $ object data area designation
OAMADDL     = $2102     ; address for accessing OAM
OAMADDH     = $2103
OAMDATA     = $2104     ; data for OAM write
VMAINC      = $2115     ; VRAM address increment value designation
VMADDL      = $2116     ; address for VRAM read and write
VMADDH      = $2117
VMDATAL     = $2118     ; data for VRAM write
VMDATAH     = $2119
CGADD       = $2121     ; address for CGRAM read and write
CGDATA      = $2122     ; data for CGRAM write
TM          = $212c     ; main screen designation
NMITIMEN    = $4200     ; enable flaog for v-blank
MDMAEN      = $420b     ; DMA enable register
RDNMI       = $4210     ; read the NMI flag status
DMAP0       = $4300     ; DMA control register, channel 0
BBAD0       = $4301     ; DMA destination register, channel 0
A1T0L       = $4302     ; DMA source address register low, channel 0
A1T0H       = $4303     ; DMA source address register high, channel 0
A1T0B       = $4304     ; DMA source address register bank, channel 0
DAS0L       = $4305     ; DMA size register low, channel 0
DAS0H       = $4306     ; DMA size register high, channel 0
;-------------------------------------------------------------------------------

;----- Memory Map WRAM ---------------------------------------------------------
HOR_SPEED   = $0300     ; the horizontal speed
VER_SPEED   = $0301     ; the vertical speed
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM
;-------------------------------------------------------------------------------

;----- Game Constants ----------------------------------------------------------
    ; we use these constants to check for collisions with the screen boundaries
SCREEN_LEFT     = $00   ; left screen boundary = 0
SCREEN_RIGHT    = $ff   ; right screen boundary = 255
SCREEN_TOP      = $00   ; top screen boundary = 0 
SCREEN_BOTTOM   = $df   ; bottom screen boundary = 223
    ; a simple constant to define the sprite movement speed 
SPRITE_SPEED    = $02   ; the sprites will move 2 pixel per frame
    ; this makes the code a bit more readable
SPRITE_SIZE     = $08   ; sprites are 8 by 8 pixel
OAMMIRROR_SIZE  = $0220 ; OAMRAM can hold data for 128 sprites, 4 bytes each
;-------------------------------------------------------------------------------

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
;-------------------------------------------------------------------------------

;----- Includes ----------------------------------------------------------------
.segment "SPRITEDATA"
SpriteData: .incbin "Sprite.vra"
ColorData:  .incbin "Sprite.pal"
;-------------------------------------------------------------------------------

.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
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
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
; .smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank
	lda HOR_SPEED
	bpl CheckRightBounds
CheckLeftBounds:
	lda OAMMIRROR
	clc
	adc HOR_SPEED
	bcs UpdatePos ; branch if we be underflowin
	    stz OAMMIRROR
	    bra InvertHorSpeed
CheckRightBounds:
	lda OAMMIRROR
	clc
	adc HOR_SPEED
	cmp #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
	bcc UpdatePos ; if we at place...
	    lda #(SCREEN_RIGHT - 2 * SPRITE_SIZE)
	    sta OAMMIRROR
	    bra InvertHorSpeed
UpdatePos:
	sta OAMMIRROR
	bra Return
InvertHorSpeed:
	lda HOR_SPEED
	eor #$ff
	inc
	sta HOR_SPEED
	bra Return

        ; here we would place all of the game logic
        ; and loop forever

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

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank; uses DMA channel to quick-store stuff to OAMRAM
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI

	lda #%00000010          ; set DMA channel 0
        sta DMAP0
        lda #$04                ; set destination to OAMDATA
        sta BBAD0   
        ldx #OAMMIRROR		; get address of OAMRAM mirror
        stx A1T0L               ; set low and high byte of address 
        stz A1T0B               ; set bank to zero, since the mirror is in WRAM
        ldx #OAMMIRROR_SIZE ; set the number of bytes to transfer 
        stx DAS0L

        lda #$01                ; start DMA transfer 
        sta MDMAEN
        ; this is where we would do graphics update

        rti
.endproc
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
;   Load sprite data into VRAM
;   Parameters: NumBytes: .byte, SrcPointer: .addr, DestPointer: .addr
;-------------------------------------------------------------------------------
.proc   LoadVRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        NumBytes    = $07       ; number of bytes to transfer
        SrcPointer  = $08       ; source address of sprite data
        DestPointer = $0a       ; destination address in VRAM

        ; set destination address in VRAM, and address increment after writing to VRAM
        ldx DestPointer         ; load the destination pointer...
        stx VMADDL              ; ...and set VRAM address register to it
        lda #$80
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH

        ; loop through source data and transfer to VRAM
        ldy #$0000              ; set register Y to zero, we will use Y as a loop counter and offset
VRAMLoop:
        lda (SrcPointer), Y  ; get bitplane 0/2 byte from the sprite data
        sta VMDATAL             ; write the byte in A to VRAM
        iny                     ; increment counter/offset
        lda (SrcPointer), Y  ; get bitplane 1/3 byte from the sprite data
        sta VMDATAH             ; write the byte in A to VRAM
        iny                     ; increment counter/offset
        cpy NumBytes            ; check whether we have written $04 * $20 = $80 bytes to VRAM (four sprites)
        bcc VRAMLoop            ; if X is smaller than $80, continue the loop

        ; all done
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Load color data into CGRAM
;   NumBytes: .byte, SrcPointer: .addr, DestPointer: .addr
;-------------------------------------------------------------------------------
.proc   LoadCGRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        NumBytes    = $07       ; number of bytes to transfer
        SrcPointer  = $09       ; source address of sprite data
        DestPointer = $0b       ; destination address in VRAM

        ; set CGDRAM destination address
        lda DestPointer         ; get destination address
        sta CGADD               ; set CGRAM destination address

        ldy #$0000              ; set Y to zero, use it as loop counter and offset
CGRAMLoop:
        lda (SrcPointer), Y  ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        iny                     ; increase counter/offset
        lda (SrcPointer), Y  ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        iny                     ; increase counter/offset
        cpy NumBytes            ; check whether 32/$20 bytes were transfered
        bcc CGRAMLoop           ; if not, continue loop

        ; all done
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
;-------------------------------------------------------------------------------

.proc   ClearCGRAM
        phx                     ; save old stack pointer
        ; create frame pointer
        phd                     ; push Direct Register to stack
        tsc                     ; transfer Stack to... (via Accumulator)
        tcd                     ; ...Direct Register.
        ; use constants to access arguments on stack with Direct Addressing
        NumBytes    = $07       ; number of bytes to transfer
        DestPointer = $08       ; destination address in VRAM

        ; set CGDRAM destination address
        lda DestPointer         ; get destination address
        sta CGADD               ; set CGRAM destination address

        ldy #$0000              ; set Y to zero, use it as loop counter and offset
CGRAMLoop:
        lda #$00  ; get the color low byte
        sta CGDATA              ; store it in CGRAM
        iny                     ; increase counter/offset
        lda #$00  ; get the color high byte
        sta CGDATA              ; store it in CGRAM
        iny                     ; increase counter/offset
        cpy NumBytes            ; check whether 32/$20 bytes were transfered
        bcc CGRAMLoop           ; if not, continue loop

        ; all done
        pld                     ; restore caller's frame pointer
        plx                     ; restore old stack pointer
        rts
.endproc
;-------------------------------------------------------------------------------
;   Load sprite informations into OAMRAM
;-------------------------------------------------------------------------------
.proc   LoadOAMRAM
        ; set up OAM data

        rts
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
