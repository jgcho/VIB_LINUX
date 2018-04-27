#!/usr/bin/csh

set PHOME=$argv[1]
source $PHOME/SHL/01_READDIR.csh
set Tday=`cat $PHOME/ymd.dat`

mkdir -p $PRE1/$Tday
$DEXE/21_ts_pre.x < $PHOME/HOME.inp
