#!/usr/bin/csh

set PHOME=/data/app/MOD_POINT
set lm=`cat $PHOME/dir.inp`

set DINP=$PHOME/$lm[1]
set DLOG=$PHOME/$lm[2]
set DSRC=$PHOME/$lm[3]
set DEXE=$PHOME/$lm[4]
set DSHL=$PHOME/$lm[5]
set PRE0_1=$lm[6]
set PRE1=$PHOME/$lm[7]
set PRE2=$PHOME/$lm[8]
set DRST=$PHOME/$lm[9]
set DXML=$lm[10]
