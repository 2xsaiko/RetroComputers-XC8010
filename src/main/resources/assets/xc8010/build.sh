#!/bin/sh

function die() {
  tput bold
  tput setaf 1
  echo "$@"
  exit 1
}

./assembler ../../../../../assembler/bootldr.asm bootldr.bin '$0400' || die "Failed to compile bootldr.bin."
./assembler ../../../../../assembler/forth.asm forth.bin '$0500' || die "Failed to compile forth.bin."
./assembler ../../../../../assembler/forth.asm extforth.bin '$0500' -Cdisk_ext -Cmath_ext -Cdefer || die "Failed to compile extforth.bin."
./assembler ../../../../../assembler/forth.asm minforth.bin '$0500' -Cmin || die "Failed to compile minforth.bin."
