!234567
      integer nm,n,nn
      
      integer,allocatable,dimension(:)::ikey
      character*7,allocatable,dimension(:)::cloc
      integer,allocatable,dimension(:)::inf    ! 1:On, 2:Off
      integer,allocatable,dimension(:)::iflag  ! 1:KHOA, 2:NFRDI      
      integer,allocatable,dimension(:)::ioc    ! 1:west, 2:south, 3:north
      integer,allocatable,dimension(:)::item,isal,iatm
      real,allocatable,dimension(:)::utmx,utmy
      real*8,allocatable,dimension(:)::rlon,rlat
      character*120,allocatable,dimension(:)::chname
      integer,allocatable,dimension(:)::intv,iarea,iregn,irep1,irep2
      integer,allocatable,dimension(:)::idxw,ividx
      real*8,allocatable,dimension(:)::probw,tempw,saltw,atemw

      character*120 cHOME,cINP,cLOG,cSRC,cEXE,cSHL
      character*120 cpre0_1,cpre1,cpre2,crst,cXML

      character*8 ctoday
      real*8 tmin(3),tmax(3),smin(3),smax(3),tamin(3),tamax(3)
      real*8 beta0(3),beta11(3),beta21(3),beta31(3)
      real*8 beta12(3),beta22(3),beta32(3)
      real*8 beta13(3),beta23(3),beta33(3)
      real rlv(5),val(5)


      call init
      call setwfn
      nn=0
      do n=1,nm
!      	if (inf(n).eq.1) then
      		nn=nn+1
      	  call readf
!        endif
      enddo
      write(*,*) "end read"
      nn=0
      do n=1,nm
!      	if (inf(n).eq.1) then
      		nn=nn+1
          call writef
!        endif
      enddo
      write(52,'(a)') "</result>"
      write(53,'(a)') "</result>"
      write(*,*) "end write"

CONTAINS

!#############################################################
      subroutine samp
      endsubroutine

!#############################################################
      subroutine writef
      
      character*3 cnum
      character*120 ctmp
      character*10 creal
      character*3 cint
      integer nw
      
      write(*,*) n,trim(cloc(n))
      if (idxw(n).ge.1) then ! 해당정점이 정상
      	nw=n
      elseif (idxw(irep1(n)).ge.1) then ! 대체정점 1이 정상
      	nw=irep1(n)
      elseif (idxw(irep2(n)).ge.1) then ! 대체정점 2가 정상
      	nw=irep2(n)
      else ! 해당정점, 대체정점 1, 2가 모두 비정상이면 그냥 해당정점으로 표기
        nw=n
      endif

      write(51,5101) cloc(n),rlat(n),rlon(n),idxw(nw),probw(nw) &
      ,tempw(nw),saltw(nw),atemw(nw),utmx(n),utmy(n),ividx(nw) &
      ,trim(chname(n)),iarea(n),iregn(n),n,nw
      write(61,5101) cloc(n),rlat(n),rlon(n),idxw(n),probw(n) &
      ,tempw(n),saltw(n),atemw(n),utmx(n),utmy(n),ividx(n) &
      ,trim(chname(n)),iarea(n),iregn(n),n
 5101 format(a,',',2(f10.6,','),i1,',',f7.4,',',3(f6.1,','),f8.1,',',f9.1 &
             ,',',i3,',',a,',',i1,',',i1,',',i3,',',i3)
 
      ! 52 : org, 53 : replace
      write(cnum,'(i3)') nn
      write(ctmp,'(a,a,a)') "  <data id='",trim(adjustl(cnum)),"'>"
      write(52,'(a)') trim(ctmp)
      write(53,'(a)') trim(ctmp)

      write(52,'(3a)') "    <obs_post_id>",cloc(n),"</obs_post_id>"
      write(53,'(3a)') "    <obs_post_id>",cloc(n),"</obs_post_id>"

      write(ctmp,'(a,f9.6,a)') "    <obs_lat>",rlat(n),"</obs_lat>"
      write(52,'(a)') trim(ctmp)
      write(53,'(a)') trim(ctmp)

      write(ctmp,'(a,f10.6,a)') "    <obs_lon>",rlon(n),"</obs_lon>"
      write(52,'(a)') trim(ctmp)
      write(53,'(a)') trim(ctmp)

      write(ctmp,'(a,i1,a)') "    <v_index>",idxw(n),"</v_index>"
      write(52,'(a)') trim(ctmp)
      write(ctmp,'(a,i1,a)') "    <v_index>",idxw(nw),"</v_index>"
      write(53,'(a)') trim(ctmp)

      write(creal,'(f7.4)') probw(n)
      write(ctmp,'(3a)') "    <v_isolation>",trim(adjustl(creal)),"</v_isolation>"
      write(52,'(a)') trim(ctmp)
      write(creal,'(f7.4)') probw(nw)
      write(ctmp,'(3a)') "    <v_isolation>",trim(adjustl(creal)),"</v_isolation>"
      write(53,'(a)') trim(ctmp)
      
      write(cint,'(i3)') ividx(n)
      write(ctmp,'(3a)') "    <v_score>",trim(adjustl(cint)),"</v_score>"
      write(52,'(a)') trim(ctmp)
      write(cint,'(i3)') ividx(nw)
      write(ctmp,'(3a)') "    <v_score>",trim(adjustl(cint)),"</v_score>"
      write(53,'(a)') trim(ctmp)
      
      write(creal,'(f6.1)') tempw(n)
      write(ctmp,'(3a)') "    <temp>",trim(adjustl(creal)),"</temp>"
      write(52,'(a)') trim(ctmp)
      write(creal,'(f6.1)') tempw(nw)
      write(ctmp,'(3a)') "    <temp>",trim(adjustl(creal)),"</temp>"
      write(53,'(a)') trim(ctmp)
      
      write(creal,'(f6.1)') saltw(n)
      write(ctmp,'(3a)') "    <salt>",trim(adjustl(creal)),"</salt>"
      write(52,'(a)') trim(ctmp)
      write(creal,'(f6.1)') saltw(nw)
      write(ctmp,'(3a)') "    <salt>",trim(adjustl(creal)),"</salt>"
      write(53,'(a)') trim(ctmp)
      
      write(creal,'(f6.1)') atemw(n)
      write(ctmp,'(3a)') "    <atem>",trim(adjustl(creal)),"</atem>"
      write(52,'(a)') trim(ctmp)
      write(creal,'(f6.1)') atemw(nw)
      write(ctmp,'(3a)') "    <atem>",trim(adjustl(creal)),"</atem>"
      write(53,'(a)') trim(ctmp)
      
      write(ctmp,'(3a)') "    <obs_post_name><![CDATA[",trim(chname(n)),"]]></obs_post_name>"
      write(52,'(a)') trim(ctmp)
      write(53,'(a)') trim(ctmp)
      
      write(ctmp,'(a,i1,a)') "    <AREA>",iarea(n),"</AREA>"
      write(52,'(a)') trim(ctmp)
      write(53,'(a)') trim(ctmp)
      
      write(ctmp,'(a,i1,a)') "    <REGION>",iregn(n),"</REGION>"
      write(52,'(a)') trim(ctmp)
      write(53,'(a)') trim(ctmp)
      
      write(52,'(a)') "  </data>"
      write(53,'(a)') "  </data>"

      endsubroutine !writef

!#############################################################
      subroutine readf
      
      logical lfn
      integer lm
      character*120 cread,ctmp
      real*8,allocatable,dimension(:)::proba,tempa,salta,atema,idexa
      real*8 probx
      
      !write(cread,'(7a)') "./",trim(cpre2),"/",ctoday,"/",trim(cloc(n)),".csv"
      write(cread,'(8a)') trim(cHOME),"/",trim(cpre2),"/",ctoday,"/",trim(cloc(n)),".csv"
      
      inquire (file=trim(cread), exist = lfn)
      if (.not.lfn) goto 320

      write(*,*) n,trim(cread)
      open(31,file=trim(cread),status='old')
     	read(31,*,end=319) 
     	read(31,*,end=319)
     	lm=0
  311 read(31,*,end=319) tm
        lm=lm+1
      goto 311
  319 continue
      allocate(proba(lm))
      allocate(idexa(lm))
      allocate(tempa(lm))
      allocate(salta(lm))
      allocate(atema(lm))
      
      rewind(31)
      read(31,*)
      read(31,*)
      probx=-999.
      tempx=-999.
      do l=1,lm
      	read(31,*) tm,proba(l),idexa(l),tempa(l),salta(l),atema(l)
      	if (proba(l).gt.probx) then
      		lmx=l
      		probx=proba(l)
      	endif
      	if (tempa(l).gt.tempx) then
      		ltx=l
      		tempx=tempa(l)
      	endif
      enddo
      
      !write(*,*) n,trim(cread),' ',trim(chname),lmx
      probw(n)=proba(lmx)
      idxw(n)=idexa(lmx)
      tempw(n)=tempa(ltx)
      saltw(n)=salta(lmx)
      atemw(n)=atema(lmx)
      call prob2ividx(probw(n))
      
      close(31)
      deallocate(proba)
      deallocate(idexa)
      deallocate(tempa)
      deallocate(salta)
      deallocate(atema)
  
  320 continue
      if (.not.lfn) then
      	probw(n)=-9.9
      	idxw(n)=0
      	tempw(n)=-9.9
      	saltw(n)=-9.9
      	atemw(n)=-9.9
      	write(*,'(a,i3,a,a)') "23 NO FILE: ",n," ",trim(cread)
      endif
      
      endsubroutine ! readf
      
!#############################################################
      subroutine setwfn
      
      character*120 cout
      character*4 cyear
      character*2 cmon,cday
      
      cyear=ctoday(1:4)
      cmon=ctoday(5:6)
      cday=ctoday(7:8)
      !write(*,*) cyear,cmon,cday
      
      !write(cout,'(7a)') "./",trim(crst),"/",ctoday,"/",ctoday,".csv"
      write(cout,'(8a)') trim(cHOME),"/",trim(crst),"/",ctoday,"/",ctoday,".csv"
      !write(*,*) trim(cout)
      open(51,file=trim(cout),status="unknown")
      write(51,'(a)') ctoday
      write(51,'(a)') 'loc,lat,lon,index,rate,temp,salt,atem,utmx,utmy,&
      score,name,area,region,no.,nrep'

      write(cout,'(8a)') trim(cHOME),"/",trim(crst),"/",ctoday,"/",ctoday,"_org.csv"
      open(61,file=trim(cout),status="unknown")
      write(61,'(a)') ctoday
      write(61,'(a)') 'loc,lat,lon,index,rate,temp,salt,atem,utmx,utmy,&
      score,name,area,region,no.,'

      !write(cout,'(7a)') "./",trim(crst),"/",ctoday,"/",ctoday,"_euckr.xml"
      write(cout,'(8a)') trim(cXML),"/",ctoday,"/",ctoday,"_org.xml"
      open(52,file=trim(cout))
      write(52,'(a)') "<?xml version='1.0' encoding='utf-8'?>"
      write(52,'(a)') "<result>"
      write(52,'(a)') "  <meta>"
      write(52,'(7a)') "    <record_day>",cyear,"-",cmon,"-",cday,"</record_day>"
      write(52,'(a)') "    <col1>obs_post_id</col1>"
      write(52,'(a)') "    <col2>obs_lat</col2>"
      write(52,'(a)') "    <col3>obs_lon</col3>"
      write(52,'(a)') "    <col4>v_index</col4>"
      write(52,'(a)') "    <col5>v_isolation</col5>"
      write(52,'(a)') "    <col6>v_score</col6>"
      write(52,'(a)') "    <col7>temp_degC</col7>"
      write(52,'(a)') "    <col8>salt_psu</col8>"
      write(52,'(a)') "    <col9>atem_degC</col9>"
      write(52,'(a)') "    <col10>obs_post_name</col10>"
      write(52,'(a)') "    <col11>AREA</col11>"
      write(52,'(a)') "    <col12>REGION</col12>"
      write(52,'(a)') "  </meta>"
      
      write(cout,'(8a)') trim(cXML),"/",ctoday,"/",ctoday,"_euckr.xml"
      open(53,file=trim(cout))
      write(53,'(a)') "<?xml version='1.0' encoding='utf-8'?>"
      write(53,'(a)') "<result>"
      write(53,'(a)') "  <meta>"
      write(53,'(7a)') "    <record_day>",cyear,"-",cmon,"-",cday,"</record_day>"
      write(53,'(a)') "    <col1>obs_post_id</col1>"
      write(53,'(a)') "    <col2>obs_lat</col2>"
      write(53,'(a)') "    <col3>obs_lon</col3>"
      write(53,'(a)') "    <col4>v_index</col4>"
      write(53,'(a)') "    <col5>v_isolation</col5>"
      write(53,'(a)') "    <col6>v_score</col6>"
      write(53,'(a)') "    <col7>temp_degC</col7>"
      write(53,'(a)') "    <col8>salt_psu</col8>"
      write(53,'(a)') "    <col9>atem_degC</col9>"
      write(53,'(a)') "    <col10>obs_post_name</col10>"
      write(53,'(a)') "    <col11>AREA</col11>"
      write(53,'(a)') "    <col12>REGION</col12>"
      write(53,'(a)') "  </meta>"

      endsubroutine ! setwfn

!#############################################################
      subroutine init
      
      character*120 ctmp,cdum
      
      read(*,'(a)') cHOME
      cdum=trim(cHOME)//'/dir.inp'
      open(12,file=trim(cdum))
      read(12,'(a)') cINP
      read(12,'(a)') cLOG
      read(12,'(a)') cSRC
      read(12,'(a)') cEXE
      read(12,'(a)') cSHL
      read(12,'(a)') cpre0_1
      read(12,'(a)') cpre1
      read(12,'(a)') cpre2
      read(12,'(a)') crst
      read(12,'(a)') cXML
      close(12)
      
      cdum=trim(cHOME)//'/ymd.dat'
      open(11,file=trim(cdum),status='old')
      !open(11,file='ymd.dat',status='old')
      read(11,*) ctoday
      close(11)
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/21_ts.csv'
      open(21,file=trim(cdum),status='old')
      !open(21,file='21_ts_v170720.csv',status='old')
      read(21,*)
  211 read(21,*,end=219) nm
      goto 211
  219 continue
      !write(*,*) nt
      allocate(ikey(nm))
      allocate(cloc(nm))
      allocate(inf(nm))
      allocate(iflag(nm))
      allocate(ioc(nm))
      allocate(item(nm))
      allocate(isal(nm))
      allocate(iatm(nm))
      allocate(utmx(nm))
      allocate(utmy(nm))
      allocate(rlon(nm))
      allocate(rlat(nm))
      allocate(chname(nm))
      allocate(intv(nm))
      allocate(iarea(nm))
      allocate(iregn(nm))
      allocate(irep1(nm))
      allocate(irep2(nm))
      
      allocate(idxw(nm),ividx(nm))
      allocate(probw(nm),tempw(nm),saltw(nm),atemw(nm))
      
      rewind(21)
      read(21,*)
      do n=1,nm
      	read(21,*) ikey(n),cloc(n),inf(n),iflag(n),ioc(n), &
      	           item(n),isal(n),iatm(n),utmx(n),utmy(n), &
      	           rlon(n),rlat(n),chname(n),intv(n),idum,idum, &
      	           iarea(n),iregn(n),irep1(n),irep2(n)
        !write(*,*) ikey(n),cloc(n),chname(n)
      enddo
      close(21)
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/coefficient2.inp'
      open(22,file=trim(cdum),status='old')
      !open(unit=22,file='coefficient2.inp',status='old')
      read(22,*)tmin(1),tmax(1)
      read(22,*)tmin(2),tmax(2)
      read(22,*)tmin(3),tmax(3)
      read(22,*)smin(1),smax(1)
      read(22,*)smin(2),smax(2)
      read(22,*)smin(3),smax(3)
      read(22,*)tamin(1),tamax(1)
      read(22,*)tamin(2),tamax(2)
      read(22,*)tamin(3),tamax(3)
      read(22,*)beta0(1),  beta0(2), beta0(3)
      read(22,*)beta11(1),beta11(2),beta11(3)
      read(22,*)beta21(1),beta21(2),beta21(3)
      read(22,*)beta31(1),beta31(2),beta31(3)
      read(22,*)beta12(1),beta12(2),beta12(3)
      read(22,*)beta22(1),beta22(2),beta22(3)
      read(22,*)beta32(1),beta32(2),beta32(3)
      read(22,*)beta13(1),beta13(2),beta13(3)
      read(22,*)beta23(1),beta23(2),beta23(3)
      read(22,*)beta33(1),beta33(2),beta33(3)
      read(22,*)
      rlv(1)=0.
      val(1)=0.
      read(22,*)rlv(2),val(2)
      read(22,*)rlv(3),val(3)
      read(22,*)rlv(4),val(4)
      read(22,*)rlv(5),val(5)
      close(22)
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/coefficient3.inp'
      open(unit=23,file=trim(cdum),status='old')
      read(23,*)
      do i=1,10
        read(23,*) 
      enddo
      read(23,*)
      rlv(1)=0.
      val(1)=0.
      read(23,*)rlv(2),val(2)
      read(23,*)rlv(3),val(3)
      read(23,*)rlv(4),val(4)
      read(23,*)rlv(5),val(5)
      close(23)
      
      endsubroutine ! init

!##############################################################################
!# Julianday convert --- START
!##############################################################################
      subroutine juld(iy,im,id,ih,mn,tm)
      integer iy,im,id,ih,mn,k
      real day(12)
      data day/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./

      day(2)=28.
      if ( mod(iy,4).eq.0 .and. (mod(iy,100).ne.0 &
       .or. mod(iy,400).eq.0) ) then 
        day(2)=29. 
      endif

      tm=0. 

      if (im-1.eq.0) then 
        tm=0.
      else !if (im-1.eq.0) then 
        do k=1,im-1 
          tm=tm+day(k) 
        enddo
      endif !if (im-1.eq.0) then
      tm=tm+float(id) + (float(ih)/24.) + (float(mn)/1440.)
      return
      endsubroutine
!##############################################################################
!# Julianday convert --- END
!##############################################################################

!#############################################################
      subroutine prob2ividx(probt)
      real*8 probt
      
      mlast=1
      m1=1
  100 continue
      m2=m1+1
      if (probt.gt.rlv(m2)) then
      	m1=m2
      	goto 100
      else
      	mlast=m1
      endif
      tdiff=rlv(m2)-rlv(m1)
      wtm1=(rlv(m2)-probt)/tdiff
      wtm2=(probt-rlv(m1))/tdiff
      vidx=wtm1*val(m1) + wtm2*val(m2)
      if (vidx.lt.0.) vidx=-9.
      ividx(n)=nint(vidx)
      !write(*,*) probt,ividx
      return
      endsubroutine ! prob2ividx

end