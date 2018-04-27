!234567
! 출력 정보 : probmx,tempmx의 주변 격자 평균
      integer iav
      parameter(im=4,km=3,iav=9) ! k=3일예보, !iav=주변9개격자 평균
      real*8 rlv(5),val(5)
      integer irmax,jrmax,l,lm
      character*8 ctoday,cday(km+1)
      real days,daye

      !real,allocatable,dimension(:)::rlon,rlat,xx,yy
      integer,allocatable,dimension(:,:)::iflag
      real*8,allocatable,dimension(:,:)::probmx,tempmx
      !integer,allocatable,dimension(:)::idxmx,iscore
      
      real tmpprob,t
      integer itmpidx,itmpscore

      character*120 cHOME
      character*120 cINP,cLOG,cSRC,cEXE,cSHL,cROMS,cRST,cXML

! ROMS HD variables +++++++
      integer i1mx,j1mx ! Old result max i,j
      real,allocatable,dimension(:)::rlon2,rlat2,xx2,yy2
      integer,allocatable,dimension(:)::i2,j2
      integer,allocatable,dimension(:,:)::ip2,jp2
! ROMS HD variables -------
            
      call init
      
      do k=1,km
      	call rwproc
      enddo

CONTAINS

!#############################################################
      subroutine samp
      endsubroutine

!#############################################################
      subroutine rwproc
      character*120 cin,cou,coux,ctmp
      character*5 cnum
      real*8 aprob,atemp
      logical lfn
      
      write(cin,'(8a,i1,a)') trim(cHOME),'/',trim(cRST),'/',ctoday &
      ,'/ROMS_',ctoday,'_',k,'.csv'
      write(cou,'(8a,i1,a)') trim(cHOME),"/",trim(cRST),"/",ctoday, &
        "/ROMS_",ctoday,"_",k,"_HD.csv"
      write(coux,'(6a,i1,a)') trim(cXML),"/",ctoday, &
        "/ROMS_",ctoday,"_",k,"_HD_euckr.xml"

      open(51,file=trim(cou))
      write(51,'(4a)') "CurrentDay=",ctoday,",PredictDay=",cday(k)
      write(51,'(a)') "i,j,lon,lat,utmx,utmy,idx,score,isol,temp,flag,"
      
      open(52,file=trim(coux))
      write(52,'(a)') "<?xml version='1.0' encoding='utf-8'?>"
      write(52,'(a)') "<result>"
      write(52,'(a)') "  <meta>"
      write(52,'(7a)') "    <CurrentDay>",ctoday,"</CurrentDay>"
      write(52,'(7a)') "    <PredictDay>",cday(k),"</PredictDay>"
      write(52,'(a)') "    <col1>i</col1>"
      write(52,'(a)') "    <col2>j</col2>"
      write(52,'(a)') "    <col3>lon</col3>"
      write(52,'(a)') "    <col4>lat</col4>"
      write(52,'(a)') "    <col5>utmx</col5>"
      write(52,'(a)') "    <col6>utmy</col6>"
      write(52,'(a)') "    <col7>v_index</col7>"
      write(52,'(a)') "    <col8>v_score</col8>"
      write(52,'(a)') "    <col9>v_isolation</col9>"
      write(52,'(a)') "    <col10>temp_degC</col10>"
      write(52,'(a)') "    <col11>flag</col11>"
      write(52,'(a)') "  </meta>"
            
      inquire (file=trim(cin), exist = lfn)
      if (.not.lfn) then
      	write(*,'(i2,2a)') k," Not found:",trim(cin)
      	write(*,'(i2,2a)') k," Dummy write:",trim(cou)
      	write(*,'(i2,2a)') k," Dummy write:",trim(coux)
      	itmpidx=1
      	itmpscore=0
      	aprob=0.
      	atemp=0.
      	iflgw=0
      	do l=1,lm
      		write(51,5101) i2(l),j2(l),rlon2(l),rlat2(l),xx2(l),yy2(l) &
      	  ,itmpidx,itmpscore,aprob,atemp,iflagw
      	  
      	  if (itmpidx.gt.1) then
      	    write(cnum,'(i5)') l
            write(ctmp,'(a,a,a)') "  <data id='",trim(adjustl(cnum)),"'>"
            write(52,'(a)') trim(ctmp)
            write(52,'(a,i3.3,a)') "    <i>",i2(l),"</i>"
            write(52,'(a,i3.3,a)') "    <j>",j2(l),"</j>"
            write(52,'(a,f8.4,a)') "    <lon>",rlon2(l),"</lon>"
            write(52,'(a,f7.4,a)') "    <lat>",rlat2(l),"</lat>"
            write(52,'(a,f8.1,a)') "    <utmx>",xx2(l),"</utmx>"
            write(52,'(a,f9.1,a)') "    <utmy>",yy2(l),"</utmy>"
            write(52,'(a,i1,a)') "    <v_index>",itmpidx,"</v_index>"
            write(52,'(a,i2.2,a)') "    <v_score>",itmpscore,"</v_score>"
            write(52,'(a,f6.3,a)') "    <v_isolation>",aprob,"</v_isolation>"
            write(52,'(a,f4.1,a)') "    <temp>",atemp,"</temp>"
            write(52,'(a,i2.2,a)') "    <flag>",iflagw,"</flag>"
            write(52,'(a)') "  </data>"
          endif
      	enddo
      	goto 210
      endif
      
      write(*,'(i2,x,a)') k,trim(cin)
      write(*,'(i2,x,a)') k,trim(cou)
      write(*,'(i2,x,a)') k,trim(coux)
      
      iflag=-999
      probmx=0.
      tempmx=-999.
      open(21,file=trim(cin))
      read(21,*)
      read(21,*)
  211 read(21,*,end=219) i,j,dum,dum,dum,dum,idum,idum,probmx(i,j),tempmx(i,j),iflag(i,j)
      goto 211
  219 continue
      close(21)

      do l=1,lm
      	aprob=0.
      	atemp=0.
      	icnt=0
      	do m=1,iav
      		aprob=aprob + probmx(ip2(m,l),jp2(m,l))
      		atemp=atemp + tempmx(ip2(m,l),jp2(m,l))
      		if (tempmx(ip2(m,l),jp2(m,l)).gt.0.) icnt=icnt+1
      	enddo
      	if (icnt.eq.0) then
      	  aprob=0.
      	  atemp=0.
      	else
      	  aprob=aprob/float(icnt)
      	  atemp=atemp/float(icnt)
        endif
      	
      	itmpidx=1
      	itmpscore=0
      	call calscore(aprob)
      	
      	write(51,5101) i2(l),j2(l),rlon2(l),rlat2(l),xx2(l),yy2(l) &
      	,itmpidx,itmpscore,aprob,atemp,iflag(ip2(1,l),jp2(1,l))
      	if (itmpidx.gt.1) then
      	  write(cnum,'(i5)') l
          write(ctmp,'(a,a,a)') "  <data id='",trim(adjustl(cnum)),"'>"
          write(52,'(a)') trim(ctmp)
          write(52,'(a,i3.3,a)') "    <i>",i2(l),"</i>"
          write(52,'(a,i3.3,a)') "    <j>",j2(l),"</j>"
          write(52,'(a,f8.4,a)') "    <lon>",rlon2(l),"</lon>"
          write(52,'(a,f7.4,a)') "    <lat>",rlat2(l),"</lat>"
          write(52,'(a,f8.1,a)') "    <utmx>",xx2(l),"</utmx>"
          write(52,'(a,f9.1,a)') "    <utmy>",yy2(l),"</utmy>"
          write(52,'(a,i1,a)') "    <v_index>",itmpidx,"</v_index>"
          write(52,'(a,i2.2,a)') "    <v_score>",itmpscore,"</v_score>"
          write(52,'(a,f6.3,a)') "    <v_isolation>",aprob,"</v_isolation>"
          write(52,'(a,f4.1,a)') "    <temp>",atemp,"</temp>"
          write(52,'(a,i2.2,a)') "    <flag>",iflag(ip2(1,l),jp2(1,l)),"</flag>"
          write(52,'(a)') "  </data>"
        endif
      enddo
  
  210 continue ! if (.not.lfn) then

      close(51)
      write(52,'(a)') "</result>"
      close(52)

 5101 format(i3.3,",",i3.3,",",f8.4,",",f7.4,",",f8.1,",",f9.1,",", &
             i1,",",i2.2,",",f6.3,",",f4.1,",",i2.2,",")

      endsubroutine ! rwproc

!#############################################################
      subroutine calscore(p)
      
      real*8 p
      
      if (p.ge.0. .and. p.le.rlv(2)) then
     	  itmpidx=1
      elseif (p.gt.rlv(2) .and. p.le.rlv(3)) then
       	itmpidx=2
      elseif (p.gt.rlv(3) .and. p.le.rlv(4)) then
       	itmpidx=3
      elseif (p.gt.rlv(4) .and. p.le.rlv(5)) then
       	itmpidx=4
      else
        itmpidx=0
      endif
      
      
      mlast=1
      m1=1
  100 continue
      m2=m1+1
      if (p.gt.rlv(m2)) then
      	m1=m2
      	goto 100
      else
      	mlast=m1
      endif
      tdiff=rlv(m2)-rlv(m1)
      wtm1=(rlv(m2)-p)/tdiff
      wtm2=(p-rlv(m1))/tdiff
      vidx=wtm1*val(m1) + wtm2*val(m2)
      if (vidx.lt.0.) vidx=-9.
      itmpscore=nint(vidx)
      if (itmpscore.eq.100) itmpscore=99
      endsubroutine ! calscore(p)

!#############################################################
      subroutine init
      
      character*120 cdum

      real mday(12)
      data mday/31,28,31,30,31,30,31,31,30,31,30,31/
      
      irmax=0
      jrmax=0
      
      read(*,'(a)') cHOME
      cdum=trim(cHOME)//'/dir.inp'
      open(12,file=trim(cdum))
      read(12,'(a)') cINP
      read(12,'(a)') cLOG
      read(12,'(a)') cSRC
      read(12,'(a)') cEXE
      read(12,'(a)') cSHL
      read(12,'(a)') cROMS
      read(12,'(a)') cRST
      read(12,'(a)') cXML
      close(12)
      
      cdum=trim(cHOME)//'/ymd.dat'
      open(11,file=trim(cdum),status='old')
      read(11,*) ctoday
      close(11)
      read(ctoday(1:4),*) iy
      read(ctoday(5:6),*) mm
      read(ctoday(7:8),*) id
      call juld(iy,mm,id,0,0,tm)
      days=tm
      daye=tm+2.
      
      cday(1)=ctoday
      
      mday(2)=28
      if ( mod(iy,4).eq.0 .and. (mod(iy,100).ne.0 &
       .or. mod(iy,400).eq.0) ) then 
        mday(2)=29 
      endif
      id=id+1
      if (id.gt.mday(mm)) then
      	id=1
      	mm=mm+1
      	if (mm.eq.13) then
      		mm=1
      		iy=iy+1
      	endif
      endif
      write(cday(2),'(i4.4,i2.2,i2.2)') iy,mm,id
      id=id+1
      if (id.gt.mday(mm)) then
      	id=1
      	mm=mm+1
      	if (mm.eq.13) then
      		mm=1
      		iy=iy+1
      	endif
      endif
      write(cday(3),'(i4.4,i2.2,i2.2)') iy,mm,id
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/coefficient_ROMS.inp'
      open(unit=12,file=trim(cdum),status='old')
      !open(unit=12,file='coefficient_ROMS.inp',status='old')
      read(12,*)
      do i=0,im
        !read(12,*) imod(i),b0(i),b1(i)
        read(12,*) 
      enddo
      read(12,*)
      rlv(1)=0.
      val(1)=0.
      read(12,*)rlv(2),val(2)
      read(12,*)rlv(3),val(3)
      read(12,*)rlv(4),val(4)
      read(12,*)rlv(5),val(5)
      close(12)
      close(12)
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/ROMS_HD.CSV'
      open(13,file=trim(cdum))
      read(13,*) i1mx,j1mx
      allocate(iflag(i1mx,j1mx))
      allocate(probmx(i1mx,j1mx))
      allocate(tempmx(i1mx,j1mx))
      !===========================
      read(13,*)
      l=0
  131 read(13,*,end=139) i
        l=l+1
      goto 131
  139 continue
      lm=l
      write(*,*) "Max Line:",lm
      
      allocate(rlon2(lm),rlat2(lm),xx2(lm),yy2(lm))
      allocate(i2(lm),j2(lm),ip2(iav,lm),jp2(iav,lm))
      
      !allocate(rlon(lm),rlat(lm))
      !allocate(xx(lm),yy(lm),iflag(lm))
      !allocate(probmx(lm),tempmx(lm))
      !allocate(idxmx(lm),iscore(lm))
      
      iflag=-999
      probmx=0.
      tempmx=-999.
      !idxmx=1
      !iscore=0
      
      
      rewind(13)
      read(13,*)
      read(13,*)
      l=1
  132 read(13,*,end=138) i2(l),j2(l),xx2(l),yy2(l) &
        ,(ip2(i,l),jp2(i,l),i=1,iav),rlon2(l),rlat2(l)
      l=l+1
      goto 132
  138 continue
      close(13)
!      write(*,*) i2(2),j2(2),xx2(2),ip2(3,2),jp2(3,2)
      
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


end