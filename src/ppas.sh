#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /media/fa/FA-FLASH/git-projects/lazrdp/lazrdp
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2     -L. -o /media/fa/FA-FLASH/git-projects/lazrdp/lazrdp -T /media/fa/FA-FLASH/git-projects/lazrdp/link.res -e _start
if [ $? != 0 ]; then DoExitLink /media/fa/FA-FLASH/git-projects/lazrdp/lazrdp; fi
IFS=$OFS
