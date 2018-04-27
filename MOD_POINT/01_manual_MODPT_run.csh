#!/usr/bin/csh

source /data/app/MOD_POINT/SHL/01_READDIR.csh

csh $DSHL/02_DATE.csh $PHOME

set Tday=`cat $PHOME/ymd.dat`

echo $Tday > $DLOG/Log_$Tday.txt

#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "START Compile 21 Preprocessing" >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#gfortran -o $DEXE/21_ts_pre.x $DSRC/21_ts_pre.f90 >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#echo "END Compile 21 Preprocessing" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "Start 21 Preprocessing" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
csh $DSHL/03_21_ts_pre.csh $PHOME >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END 21 Preprocessing" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt

#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "START Compile 22 Vib. Modeling" >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#gfortran -o $DEXE/22_vib_bspline.x $DSRC/22_vib_bspline.f90  >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#echo "END Compile 22 Vib. Modeling" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "Start 22 Vib. Modeling" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
csh $DSHL/04_22_vib.csh $PHOME >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END 22 Vib. Modeling" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "START Compile 23 Vib. Max write" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
gfortran -o $DEXE/23_vib_max.x $DSRC/23_vib_max.f90  >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END Compile 23 Vib. Max write" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "Start 23 Vib. Max write" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
csh $DSHL/05_23_vib_max.csh $PHOME >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END 23 Vib. Max write" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "START Convert euckr to utf-8, use /usr/bin/iconv" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
#/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/${Tday}_euckr.xml -o $DXML/$Tday/$Tday.xml >>& $DLOG/Log_$Tday.txt
/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/${Tday}_org.xml -o $DXML/$Tday/$Tday.xml >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END Convert euckr to utf-8, use /usr/bin/iconv" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt

