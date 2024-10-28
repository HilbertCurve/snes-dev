#!/usr/bin/bash

ca65 --cpu 65816 -s -o ${1}.o ${1}.s
ld65 -C memmap.cfg -o ${1}.smc ${1}.o
bsnes ${1}.smc
