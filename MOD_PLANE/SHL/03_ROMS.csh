#!/usr/bin/csh

set PHOME=$argv[1]
source $PHOME/SHL/01_READDIR.csh
set Tday=`cat $PHOME/ymd.dat`

mkdir -p $DRST/$Tday
mkdir -p $DXML/$Tday
$DEXE/34_ROMS.x < $PHOME/HOME.inp

echo "======================" >>& $DLOG/Log_$Tday.txt
echo "========35_RELOC==========" >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt

$DEXE/35_RELOC.x < $PHOME/HOME.inp
