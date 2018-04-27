#!/usr/bin/csh

set PHOME=$argv[1]
source $PHOME/SHL/01_READDIR.csh
set Tday=`cat $PHOME/ymd.dat`

mkdir -p $DRST/$Tday
mkdir -p $DXML/$Tday
$DEXE/23_vib_max.x < $PHOME/HOME.inp
