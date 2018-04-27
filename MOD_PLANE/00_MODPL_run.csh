#!/usr/bin/csh

source /data/app/MOD_PLANE/SHL/01_READDIR.csh

csh $DSHL/02_DATE.csh $PHOME

set Tday=`cat $PHOME/ymd.dat`
echo $Tday > $DLOG/Log_$Tday.txt

#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "START Compile ROMS to VIBRIO" >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#gfortran -o $DEXE/34_ROMS.x $DSRC/34_ROMS.f90 >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#echo "END Compile ROMS to VIBRIO" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo " " >>& $DLOG/Log_$Tday.txt
#
#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "START Compile ROMS to VIBRIO" >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#gfortran -o $DEXE/35_RELOC.x $DSRC/35_RELOC.f90 >>& $DLOG/Log_$Tday.txt
#echo "----------------------" >>& $DLOG/Log_$Tday.txt
#echo "END Compile ROMS to VIBRIO" >>& $DLOG/Log_$Tday.txt
#echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
#echo "======================" >>& $DLOG/Log_$Tday.txt
#echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "Start PLANE Modeling" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
csh $DSHL/03_ROMS.csh $PHOME >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END PLANE Modeling" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt

echo "======================" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "START Convert euckr to utf-8, use /usr/bin/iconv" >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/ROMS_${Tday}_1_euckr.xml -o $DXML/$Tday/ROMS_${Tday}_1.xml >>& $DLOG/Log_$Tday.txt
/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/ROMS_${Tday}_2_euckr.xml -o $DXML/$Tday/ROMS_${Tday}_2.xml >>& $DLOG/Log_$Tday.txt
/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/ROMS_${Tday}_3_euckr.xml -o $DXML/$Tday/ROMS_${Tday}_3.xml >>& $DLOG/Log_$Tday.txt

/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/ROMS_${Tday}_1_HD_euckr.xml -o $DXML/$Tday/ROMS_${Tday}_1_HD.xml >>& $DLOG/Log_$Tday.txt
/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/ROMS_${Tday}_2_HD_euckr.xml -o $DXML/$Tday/ROMS_${Tday}_2_HD.xml >>& $DLOG/Log_$Tday.txt
/usr/bin/iconv -f euc-kr -t utf-8 $DXML/$Tday/ROMS_${Tday}_3_HD_euckr.xml -o $DXML/$Tday/ROMS_${Tday}_3_HD.xml >>& $DLOG/Log_$Tday.txt
echo "----------------------" >>& $DLOG/Log_$Tday.txt
echo "END Convert euckr to utf-8, use /usr/bin/iconv" >>& $DLOG/Log_$Tday.txt
echo `date '+%Y-%m-%d %H:%M:%S'` >>& $DLOG/Log_$Tday.txt
echo "======================" >>& $DLOG/Log_$Tday.txt
echo " " >>& $DLOG/Log_$Tday.txt
