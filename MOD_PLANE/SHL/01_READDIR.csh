#!/usr/bin/csh

set PHOME=/data/app/MOD_PLANE
set lm=`cat $PHOME/dir.inp`

set DINP=$PHOME/$lm[1]
set DLOG=$PHOME/$lm[2]
set DSRC=$PHOME/$lm[3]
set DEXE=$PHOME/$lm[4]
set DSHL=$PHOME/$lm[5]
set PRE0=$lm[6]
set DRST=$PHOME/$lm[7]
set DXML=$lm[8]
