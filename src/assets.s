.p816
.smart ; keep track of registers widths

.export SpriteData
.export ColorData

.segment "SPRITEDATA"
SpriteData: .incbin "assets/Sprite.vra"
ColorData:  .incbin "assets/Sprite.pal"
