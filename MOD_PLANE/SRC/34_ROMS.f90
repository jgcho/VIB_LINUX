!234567
      parameter(im=4,km=3) ! im=model의 수, k=3일예보
      integer imod(0:im)
      real*8 b0(0:im),b1(0:im)
      real rlv(5),val(5)
      integer irmax,jrmax,l
      character*8 ctoday,cday(km+1)
      real days,daye

      real,allocatable,dimension(:,:)::rlon,rlat,xx,yy
      integer,allocatable,dimension(:,:)::iflag
      real,allocatable,dimension(:,:,:)::probmx,tempmx
      integer,allocatable,dimension(:,:,:)::idxmx,iscore
      
      
      real tmpprob,t
      integer itmpidx,itmpscore

      character*120 cHOME,cINP,cLOG,cSRC,cEXE,cSHL,cROMS,cRST,cXML
            
      call init

      l=0
      do j=1,jrmax
      	do i=1,irmax
      		if (iflag(i,j).ge.0) then
      			call readf(i,j)
          endif
      	enddo
      enddo
      call writef
      !write(*,*) l

CONTAINS

!#############################################################
      subroutine samp
      endsubroutine

!#############################################################
      subroutine writef
      character*120 cou
      character*5 cnum
      character*120 ctmp
      
      do k=1,km
        write(cou,'(8a,i1,a)') trim(cHOME),"/",trim(cRST),"/",ctoday, &
        "/ROMS_",ctoday,"_",k,".csv"
        open(51,file=trim(cou))
        write(51,'(4a)') "CurrentDay=",ctoday,",PredictDay=",cday(k)
        write(51,'(a)') "i,j,lon,lat,utmx,utmy,idx,score,isol,temp,flag,"

        write(cou,'(6a,i1,a)') trim(cXML),"/",ctoday, &
        "/ROMS_",ctoday,"_",k,"_euckr.xml"
        write(*,*) "WRITE: ",trim(cou)
        open(52,file=trim(cou))
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
        
        nn=1
        do j=1,jrmax
        	do i=1,irmax
        		if (iflag(i,j).ge.0) then
        			write(51,5101) i,j,rlon(i,j),rlat(i,j),xx(i,j),yy(i,j), &
              idxmx(i,j,k),iscore(i,j,k),probmx(i,j,k),tempmx(i,j,k), &
              iflag(i,j)
              
              write(cnum,'(i5)') nn
              write(ctmp,'(a,a,a)') "  <data id='",trim(adjustl(cnum)),"'>"
              write(52,'(a)') trim(ctmp)
              write(52,'(a,i3.3,a)') "    <i>",i,"</i>"
              write(52,'(a,i3.3,a)') "    <j>",j,"</j>"
              write(52,'(a,f8.4,a)') "    <lon>",rlon(i,j),"</lon>"
              write(52,'(a,f7.4,a)') "    <lat>",rlat(i,j),"</lat>"
              write(52,'(a,f8.1,a)') "    <utmx>",xx(i,j),"</utmx>"
              write(52,'(a,f9.1,a)') "    <utmy>",yy(i,j),"</utmy>"
              write(52,'(a,i1,a)') "    <v_index>",idxmx(i,j,k),"</v_index>"
              write(52,'(a,i2.2,a)') "    <v_score>",iscore(i,j,k),"</v_score>"
              write(52,'(a,f6.3,a)') "    <v_isolation>",probmx(i,j,k),"</v_isolation>"
              write(52,'(a,f4.1,a)') "    <temp>",tempmx(i,j,k),"</temp>"
              write(52,'(a,i2.2,a)') "    <flag>",iflag(i,j),"</flag>"
              write(52,'(a)') "  </data>"
              nn=nn+1
            endif
        	enddo
        enddo
        close(51)
        write(52,'(a)') "</result>"
        close(52)
      enddo

 5101 format(i3.3,",",i3.3,",",f8.4,",",f7.4,",",f8.1,",",f9.1,",", &
             i1,",",i2.2,",",f6.3,",",f4.1,",",i2.2,",")

      endsubroutine ! writef

!#############################################################
      subroutine readf(i,j)
      logical lfn
      character*120 cin
      character*8 cymd
      
      !write(cin,'(5a,i3.3,i3.3,a)') "./",trim(croms),"/",ctoday, &
      write(cin,'(4a,i3.3,i3.3,a)') trim(cROMS),"/",ctoday, &
      "/ROMS_",i,j,".csv"
      
      inquire (file=trim(cin), exist = lfn)
      probmx(i,j,:)=0.
      tempmx(i,j,:)=0.
     	idxmx(i,j,:)=1
     	iscore(i,j,:)=0
      if (.not.lfn) then
      	write(*,*) "PASS: ",trim(cin)
      	goto 420
      endif

      l=l+1
      write(*,*) "READ: ",trim(cin)
      open(21,file=trim(cin),err=420,status="old")
        	
  211 read(21,*,end=219) cymd,ihr,t

        if (cymd.eq.cday(1)) then
        	k=1
        elseif (cymd.eq.cday(2)) then
        	k=2
        elseif (cymd.eq.cday(3)) then
        	k=3
        else
          k=4
        endif
        
        call QCglobal(i,j,k,cymd,ihr,t)
        
        call logist
        if (tmpprob.gt.probmx(i,j,k)) then
        	probmx(i,j,k)=tmpprob
        	tempmx(i,j,k)=t
        	idxmx(i,j,k)=itmpidx
        	iscore(i,j,k)=itmpscore
        endif
        
      goto 211
  219 continue
      close(21)
      !if (l.eq.700) write(*,*) cymd,ihr,t,i,j
      
  420 continue
      
      endsubroutine ! readf(i,j)

!#############################################################
      subroutine logist
      
      if (iflag(i,j).eq.11) then  ! 점이-외해
      	b0a=b0(imod(1)) !점이 통영
        b1a=b1(imod(1))
        cala=b0a + b1a*t
        tmpprob=exp(cala)/(1+exp(cala))
        tmpprob=tmpprob*0.5

      	call calscore(tmpprob)
      
      elseif (iflag(i,j).eq.7) then  ! 부산-점이
      	b0a=b0(imod(1)) !점이 통영
        b1a=b1(imod(1))
        cala=b0a + b1a*t
        tmpproba=exp(cala)/(1+exp(cala))
        
        b0b=b0(imod(4)) !부산
        b1b=b1(imod(4))
        calb=b0b + b1b*t
        tmpprobb=exp(calb)/(1+exp(calb))
        tmpprob=tmpproba*0.5 + tmpprobb*0.5
        
        call calscore(tmpprob)
      
      elseif (iflag(i,j).eq.6) then  ! 마산-점이
      	b0a=b0(imod(1)) !점이 통영
        b1a=b1(imod(1))
        cala=b0a + b1a*t
        tmpproba=exp(cala)/(1+exp(cala))
        
        b0b=b0(imod(3)) !마산
        b1b=b1(imod(3))
        calb=b0b + b1b*t
        tmpprobb=exp(calb)/(1+exp(calb))
        tmpprob=tmpproba*0.5 + tmpprobb*0.5
        
        call calscore(tmpprob)
      
      elseif (iflag(i,j).eq.5) then  ! 여수-점이
      	b0a=b0(imod(1)) !점이 통영
        b1a=b1(imod(1))
        cala=b0a + b1a*t
        tmpproba=exp(cala)/(1+exp(cala))
        
        b0b=b0(imod(2)) !여수
        b1b=b1(imod(2))
        calb=b0b + b1b*t
        tmpprobb=exp(calb)/(1+exp(calb))
        tmpprob=tmpproba*0.5 + tmpprobb*0.5
        
        call calscore(tmpprob)
      
      elseif (iflag(i,j).ge.1 .and. iflag(i,j).le.4) then  ! 점이통영,여수,마산,부산
      	b0a=b0(imod(iflag(i,j))) !
        b1a=b1(imod(iflag(i,j)))
        cala=b0a + b1a*t
        tmpprob=exp(cala)/(1+exp(cala))
       
        call calscore(tmpprob)

      elseif (iflag(i,j).eq.0) then  ! 외해
        tmpprob=0.
        
        call calscore(tmpprob)
      
      endif

      endsubroutine ! logist

!#############################################################
      subroutine calscore(p)
      
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
      !open(11,file='ymd.dat',status='old')
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
        read(12,*) imod(i),b0(i),b1(i)
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
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/out_roms_temp_utm_all.csv'
      open(13,file=trim(cdum))
      read(13,*)
  131 read(13,*,end=139) i,j
        if (i.gt.irmax) irmax=i
        if (j.gt.jrmax) jrmax=j
      goto 131
  139 continue
      write(*,*) "Max grid point:",irmax,jrmax
      
      allocate(rlon(irmax,jrmax),rlat(irmax,jrmax))
      allocate(xx(irmax,jrmax),yy(irmax,jrmax),iflag(irmax,jrmax))
      allocate(probmx(irmax,jrmax,km+1),tempmx(irmax,jrmax,km+1))
      allocate(idxmx(irmax,jrmax,km+1),iscore(irmax,jrmax,km+1))
      
      iflag=-999
      probmx=0.
      tempmx=-999.
      idxmx=1
      iscore=0
      
      
      rewind(13)
      read(13,*)
  132 read(13,*,end=138) i,j,rlon(i,j),rlat(i,j),dum,xx(i,j),yy(i,j),iflag(i,j)
      goto 132
  138 continue
      close(13)
      
      endsubroutine ! init

!#############################################################
      subroutine QCglobal(i,j,k,cymd,ihr,t)
      character*8 cymd
      character*120 cqout
      
      if (t.lt.0. .or. t.gt.40.) then
      	write(*,'(2a,i3.3,3i5,f10.2)') 'CHECK LOG:',cymd,ihr,i,j,k,t
      	write(cqout,'(6a)') trim(cHOME),'/',trim(cLOG),"/QCROMS_",ctoday,".LOG"
        OPEN(89,FILE=trim(cqout),POSITION='APPEND')
        write(89,'(a,i3.3,3i5,f10.2)') cymd,ihr,i,j,k,t
        close(89)
      	t=0.
      endif

      return
      endsubroutine ! QCglobal

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