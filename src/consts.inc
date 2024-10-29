.ifndef SNES_DEV_CONSTS
SNES_DEV_CONSTS = 1

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
HVBJOY      = $4212     ; H/V blank flags and standard controller enable flag
STDCNTRL1L  = $4218     ; data for standard controller 1
STDCNTRL1H  = $4219
STDCNTRL2L  = $421a     ; data for standard controller 2
STDCNTRL2H  = $421b
DMAP0       = $4300     ; DMA control register, channel 0
BBAD0       = $4301     ; DMA destination register, channel 0
A1T0L       = $4302     ; DMA source address register low, channel 0
A1T0H       = $4303     ; DMA source address register high, channel 0
A1T0B       = $4304     ; DMA source address register bank, channel 0
DAS0L       = $4305     ; DMA size register low, channel 0
DAS0H       = $4306     ; DMA size register high, channel 0
;-------------------------------------------------------------------------------
;----- Joypad Buttons ----------------------------------------------------------
BUTTON_B =  $8000
BUTTON_Y =  $0002
BUTTON_SELECT =  $0004
BUTTON_START =  $0008
BUTTON_RIGHT =  $0100
BUTTON_LEFT =  $0200
BUTTON_UP =  $0400
BUTTON_DOWN =  $0800

BUTTON_A =  $1000
BUTTON_X =  $2000
BUTTON_L = $4000
BUTTON_R = $8000

;----- Memory Map WRAM ---------------------------------------------------------
HOR_SPEED   = $0300     ; the horizontal speed
VER_SPEED   = $0301     ; the vertical speed
JOYPAD1     = $0302     ; data read from joypad 1
JOYTRIGGER1 = $0304     ; trigger read from joypad 1
JOYHELD1    = $0306     ; held buttons read from joypad 1
JOYPAD2     = $0308     ; data read from joypad 1
JOYTRIGGER2 = $030a     ; trigger read from joypad 1
JOYHELD2    = $030c     ; held buttons read from joypad 1
GROUNDED = $0310 ; check to see if you're on ground or not
OAMMIRROR   = $0400     ; location of OAMRAM mirror in WRAM
;-------------------------------------------------------------------------------

;----- Game Constants ----------------------------------------------------------
    ; we use these constants to check for collisions with the screen boundaries
SCREEN_LEFT     = $00   ; left screen boundary = 0
SCREEN_RIGHT    = $ff   ; right screen boundary = 255
SCREEN_TOP      = $00   ; top screen boundary = 0 
SCREEN_BOTTOM   = $df   ; bottom screen boundary = 223
    ; a simple constant to define the sprite movement speed 
SPRITE_SPEED    = $04   ; the sprites will move 4 pixel per frame
MAX_FALLING_SPEED = $f9 ; two's complement of $07
JUMP_SPEED = $07
    ; this makes the code a bit more readable
SPRITE_SIZE     = $08   ; sprites are 8 by 8 pixel
OAMMIRROR_SIZE  = $0220 ; OAMRAM can hold data for 128 sprites, 4 bytes each
;-------------------------------------------------------------------------------

.endif
