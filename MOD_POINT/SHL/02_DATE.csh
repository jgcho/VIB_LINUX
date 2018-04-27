#!/usr/bin/csh

set PHOME=$argv[1]

echo `date '+%Y%m%d'`> $PHOME/ymd.dat
echo `date '+%Y%m%d' -d '1 day ago'`> $PHOME/ymd-1.dat

