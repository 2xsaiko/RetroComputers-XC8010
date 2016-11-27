#!/bin/sh
./assembler ../../../../../assembler/bootldr.asm bootldr.bin '$0400' &&
./assembler ../../../../../assembler/forth.asm forth.bin '$0500'
