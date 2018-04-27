#!/usr/bin/csh

set PHOME=$argv[1]
source $PHOME/SHL/01_READDIR.csh
set Tday=`cat $PHOME/ymd.dat`

mkdir -p $PRE2/$Tday
$DEXE/22_vib_bspline.x < $PHOME/HOME.inp
