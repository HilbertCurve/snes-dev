.p816
.smart ; keep track of registers widths

.export SpriteData
.export ColorData

.segment "SPRITEDATA"
SpriteData: .incbin "assets/Madeline.vra"
ColorData:  .incbin "assets/Madeline.pal"
