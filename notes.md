Building:
ca65 --cpu 65816 -o nihil.o nihil.s

Linking:
ld65 -C memmap.cfg nihil.o -o nihil.smc

