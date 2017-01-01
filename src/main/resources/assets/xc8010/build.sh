#!/bin/sh
./assembler ../../../../../assembler/bootldr.asm bootldr.bin '$0400' &&
./assembler ../../../../../assembler/forth.asm forth.bin '$0500' &&
./assembler ../../../../../assembler/forth.asm extforth.bin '$0500' -Cdisk_ext &&
./assembler ../../../../../assembler/forth.asm minforth.bin '$0500' -Cmin
