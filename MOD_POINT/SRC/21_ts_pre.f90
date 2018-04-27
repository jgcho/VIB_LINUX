!234567
      integer nt,n ! nt: no. of locations
      character*8 ctoday,cyestday
      real days,daye
      
      integer,allocatable,dimension(:)::ikey
      character*7,allocatable,dimension(:)::cloc
      integer,allocatable,dimension(:)::inf    ! 1:On, 2:Off
      integer,allocatable,dimension(:)::iflag  ! 1:KHOA, 2:NFRDI      
      integer,allocatable,dimension(:)::ioc    ! 1:west, 2:south, 3:north
      integer,allocatable,dimension(:)::item,isal,iatm
      real,allocatable,dimension(:)::utmx,utmy
      real*8,allocatable,dimension(:)::rlon,rlat
      character*120,allocatable,dimension(:)::chname
      integer,allocatable,dimension(:)::intv,iarea,iregn
      real temp(12,31,0:23,0:59),salt(12,31,0:23,0:59),atem(12,31,0:23,0:59)

      character*120 cHOME,cINP,cLOG,cSRC,cEXE,cSHL
      character*120 cpre0_1,cpre1,cpre2,crst
      
      character*11 cval(3)                               ! 170712 yssong data qc  
      integer isav(1000)
      real tempmx(1000,12),tempav(1000,12),tempmn(1000,12)  ! 170712 yssong data qc 
      real saltmx(1000,12),saltav(1000,12),saltmn(1000,12)  ! 170712 yssong data qc 
      real atemmx(1000,12),atemav(1000,12),atemmn(1000,12)  ! 170712 yssong data qc 
      real valmx(1000,5,12),valavg(1000,5,12),valmn(1000,5,12),valstd(1000,5,12),gradcrt(5)    ! 170712 yssong data qc 
      integer ntemp,nsalt,natem  ! 170712 yssong data qc,  (no. of total data per day)
      
      integer nday(12),iy
      data nday/31,28,31,30,31,30,31,31,30,31,30,31/
      
      call init

      call qcinp  ! 170712 yssong data qc 
      
      nday(2)=28
    	if ( mod(iy,4).eq.0 .and. (mod(iy,100).ne.0 &
        .or. mod(iy,400).eq.0) ) then 
        nday(2)=29 
      endif
      
      do n=1,nt
        temp=-999.
        salt=-999.
        atem=-999.
        !if (inf(n).eq.1) then
          !write(*,*) trim(cloc(n)),' ',trim(cloc(item(n))),' ',trim(cloc(isal(n))),' ',trim(cloc(iatm(n)))
          call setwfn(cloc(n),rlon(n),rlat(n),chname(n))
          call readfn(1,cloc(item(n)),temp,ntemp) ! 1:temp
          call readfn(2,cloc(isal(n)),salt,nsalt) ! 2:salt
          call readfn(3,cloc(iatm(n)),atem,natem) ! 3:atem
          
          call qc(1,n,iy,temp,ntempbad)           ! 170712 yssong data qc 
          call qc(2,n,iy,salt,nsaltbad)           ! 170712 yssong data qc 
          call qc(3,n,iy,atem,natembad)           ! 170712 yssong data qc 
          call qcwrt(n,iy,ntemp,nsalt,natem,ntempbad,nsaltbad,natembad) ! 170712 yssong data qc          
          !
          call writef
        !endif
      enddo

CONTAINS

!#############################################################
      subroutine samp
      endsubroutine

!#############################################################
      subroutine writef

        do moa=1,12
          do ida=1,nday(moa)
          	do iha=0,23
          		it=0
          		is=0
          		ia=0
          		tempa=0.
          		salta=0.
          		atema=0.
          		do ima=0,59
          			if (temp(moa,ida,iha,ima).gt.0. .and. temp(moa,ida,iha,ima).lt.30.) then
          				it=it+1
          				tempa=tempa + temp(moa,ida,iha,ima)
          			endif
          			if (salt(moa,ida,iha,ima).gt.0. .and. salt(moa,ida,iha,ima).lt.36.) then
          				is=is+1
          				salta=salta + salt(moa,ida,iha,ima)
          			endif
          			if (atem(moa,ida,iha,ima).gt.-20. .and. salt(moa,ida,iha,ima).lt.40.) then
          				ia=ia+1
          				atema=atema + atem(moa,ida,iha,ima)
          			endif
          		enddo
         		  if (it.gt.0) then
         		    tempa=tempa/float(it)
         		  else
         		    tempa=-999.
         		  endif
       		    if (is.gt.0) then
       		      salta=salta/float(is)
       		    else
       		      salta=-999.
       		    endif
       		    if (ia.gt.0) then
       		      atema=atema/float(ia)
       		    else
       		      atema=-999.
       		    endif
       		    call juld(iy,moa,ida,iha,0,tm)
       		    if (tm.ge.days .and. tm.lt.daye) then
       		      write(51,5101) tm,tempa,salta,atema,it,iy,moa,ida,iha,0
       		    endif
          	enddo
          enddo
        enddo
        close(51)
 5101 format(f10.5,',',3(f6.1,','),i3,',',i4,',',3(i2.2,','),i2.2)
 !5101 format(4i4,2f10.1)
      endsubroutine !writef

!#############################################################
      subroutine readfn(icol,ctloc,valt,ndata)
      
      logical lfn
      integer icol
      character*7 ctloc
      character*120 cin
      real val(3) ! 1:temp, 2:salt, 3:atem
      real valt(12,31,0:23,0:59)
      integer ndata   ! 170712 yssong data qc 
      
      ndata=0         ! 170712 yssong data qc 
      
      !write(*,*) icol,cloc(n),cloc(item(n))
      cin=trim(cpre0_1)//"/"//ctoday//"/"//trim(ctloc)//".csv"
      !cin="./"//trim(cpre0_1)//"/"//ctoday//"/"//trim(ctloc)//".csv"
      
      inquire (file=trim(cin), exist = lfn)
      if (.not.lfn) goto 320
      
      write(*,*) icol,trim(cin)
      open(31,file=trim(cin))
      read(31,*,end=319)
      read(31,*,end=319)
  311 read(31,*,end=319) tm,val(1),val(2),val(3),iyd,imd,idd,ihd,mnd
        valt(imd,idd,ihd,mnd)=val(icol)
        call juld(iyd,imd,idd,ihd,mnd,tm)                      ! 170712 yssong data qc 
        if(tm.ge.days.and.tm.lt.daye)then                  ! 170712 yssong data qc 
          ndata=ndata+1      
        endif    
        goto 311
  319 continue
      !write(*,*) 
      !if(ih.eq.0.and.mn.eq.0) ndata=ndata-1              ! 170712 yssong data qc (yesterday last data 00:00:00 duplicated with today start 00:00:00)
      
      close(31)
      !write(*,*) n,trim(cin)
      
  320 continue
      if (.not.lfn) then
      	write(*,'(a,i3,a,a)') "21 NO FILE: ",icol," ",trim(cin)
      endif
      
      endsubroutine ! readfn

!#############################################################
      subroutine setwfn(ctloc,rtlon,rtlat,cthname)
      character*7 ctloc
      real*8 rtlon,rtlat
      character*120 cthname
      character*120 cout

      write(cout,'(8a)') trim(cHOME),"/",trim(cpre1),"/",ctoday,"/",trim(ctloc),".csv"
      !write(cout,'(7a)') "./",trim(cpre1),"/",ctoday,"/",trim(ctloc),".csv"
      open(51,file=trim(cout),status="unknown")
      write(51,5111) ctloc,rtlat,rtlon,trim(cthname)
      write(51,'(6a)') ',',trim(chname(item(n))),',',trim(chname(isal(n))), &
                      ',',trim(chname(iatm(n)))
      write(51,'(a)') '   yearday,  temp,  salt,  atem,   y, m, d, h'

 5111 format(a,',',f10.6,',',f10.6,',',a)

      endsubroutine ! setwfn

!#############################################################
      subroutine init
      
      character*120 cdum
      
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
      close(12)
      
      cdum=trim(cHOME)//'/ymd.dat'
      open(11,file=trim(cdum),status='old')
      read(11,*) ctoday
      close(11)
      
      cdum=trim(cHOME)//'/ymd-1.dat'
      open(11,file=trim(cdum),status='old')
      read(11,*) cyestday
      close(11)
      read(cyestday(1:4),*) iy
      read(cyestday(5:6),*) im
      read(cyestday(7:8),*) id
      call juld(iy,im,id,0,0,tm)
      days=tm+0.19    ! 0.19=05h old 0.375=09h
      daye=tm+1.19
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/21_ts.csv'
      open(21,file=trim(cdum),status='old')
      read(21,*)
  211 read(21,*,end=219) nt
      goto 211
  219 continue
      write(*,*) "No. of Stations=",nt
      allocate(ikey(nt))
      allocate(cloc(nt))
      allocate(inf(nt))
      allocate(iflag(nt))
      allocate(ioc(nt))
      allocate(item(nt))
      allocate(isal(nt))
      allocate(iatm(nt))
      allocate(utmx(nt))
      allocate(utmy(nt))
      allocate(rlon(nt))
      allocate(rlat(nt))
      allocate(chname(nt))
      allocate(intv(nt))
      allocate(iarea(nt))
      allocate(iregn(nt))
      
      rewind(21)
      read(21,*)
      do n=1,nt
      	read(21,*) ikey(n),cloc(n),inf(n),iflag(n),ioc(n), &
      	           item(n),isal(n),iatm(n),utmx(n),utmy(n), &
      	           rlon(n),rlat(n),chname(n),intv(n) !,iarea(n),iregn(n)
        !write(*,*) ikey(n),cloc(n),trim(chname(n)),intv(n)
      enddo
      close(21)
      
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


!##############################################################################
      subroutine qcinp

      character*120 cdum
      character*120 qclog
      character*120 qcmis,qcbad
      character*20 station
      real valgmx(3),valgmn(3),valgavg(3)
      
      write(qclog,'(8a)') trim(cHOME),'/',trim(cLOG),"/QC_",ctoday,".LOG"
      write(qcmis,'(8a)') trim(cHOME),'/',trim(cLOG),"/QC_mis_",ctoday,".LOG"
      write(qcbad,'(8a)') trim(cHOME),'/',trim(cLOG),"/QC_bad_",ctoday,".LOG"
      write(*,*) trim(qclog)
      write(*,*) trim(qcmis)
      write(*,*) trim(qcbad)

      cdum=trim(cHOME)//'/'//trim(cINP)//'/QC.INP'
      open(101,file=trim(cdum))
      open(200,file=trim(qclog))
      open(201,file=trim(qcmis))
      open(202,file=trim(qcbad))
      write(201,8996)
      write(202,8997)
      
      cval(1)='Water Temp.'
      cval(2)='Salinity   '
      cval(3)='Air Temp.  '

      do ndum=1,15
        read(101,*)
      enddo
      do i=1,3
!        read(101,*) valgmn(i),valgavg(i),valgmx(i),gradcrt(i)
        read(101,*) valgmn(i),valgmx(i)
        write(*,*) "QC global=",i, valgmn(i),valgmx(i)
      enddo
      read(101,*)         
      
      do n=1,nt
          read(101,*)
          read(101,*) ndum,isav(n),station  ! isav=1 use predefined average values for bad data, =0 set to null -999.
          read(101,*)
          do imon=1,12
!            read(101,*) ndum,(valmx(n,i,imon),valavg(n,i,imon),valmn(n,i,imon),valstd(n,i,imon),i=1,3)   ! i =1: tem,  2: salinity, 3: air temp.
            read(101,*) ndum,(valmx(n,i,imon),valavg(n,i,imon),valmn(n,i,imon),valstd(n,i,imon),i=1,1)   ! i =1: tem,  2: salinity, 3: air temp.
            !write(*,*) ndum
            do i=1,3
              if(valmx(n,i,imon).eq.-999.0) valmx(n,i,imon)=valgmx(i)  
              if(valmn(n,i,imon).eq.999.0) valmn(n,i,imon)=valgmn(i)              
              if(valstd(n,i,imon).eq.-999.0) valstd(n,i,imon)=0.0 
              if(valavg(n,i,imon).eq.-999.0) valavg(n,i,imon)=valgavg(i)
            enddo    
          enddo
          read(101,*)
      enddo         
      close(101)
      write(200,8998) iy,im,id,im,id+1
      write(200,8999) 
8996  format('bad_item                station      date   time      val     name')
8997  format('bad_item                station      date   time bad_id  val    valb  crt_max  crt_min   grad  crt_grad name')  
8998  format('Realtime Monitoring data QC log file in : ',i4,1x,i2,'/',i2,1x,'09:00',' ~ ',i2,'/',i2,1x,'08:59')
8999  format('no.    file     total T-miss S-miss Ta-miss T-bad  S-bad  Ta-bad bad_data station ')      
      endsubroutine
!##############################################################################
!# data qc information read --- END
!##############################################################################

!##############################################################################
      subroutine qc(istyp,n,iy,val,ndatabad)  ! n = station ID,  istyp =1: tem,  2: salinity, 3: air temp.  

      real val(12,31,0:23,0:59)
      integer ndatabad,istyp
      
! initialize
      valb=-999.0
      ndatabad=0
      nmiss=0
      grad=0.0

! 전체의 n과 QC의 n이 일치하지 않음. 전체 DO loop를 n으로 순환하기 때문에 자료가 없는 곳 때문에 하나씩 밀림. 이부분이 수정되면 if(valmx(n,i,imon).eq.-999.0) 수정해야함    
        do moa=1,12
          crtmx=valavg(n,istyp,moa)+3.*valstd(n,istyp,moa)                          ! statistics check
          crtmn=valavg(n,istyp,moa)-4.*valstd(n,istyp,moa)                          ! statistics check

          do ida=1,nday(moa)
          	do iha=0,23
              call juld(iy,moa,ida,iha,0,tm)
              do ima=0,59
                if (tm.ge.days .and. tm.lt.daye) then
                  if(val(moa,ida,iha,ima).ne.-999.0.and.valb.ne.-999.0)then
                    grad=abs(val(moa,ida,iha,ima)-valb)                             ! gradient compute
                  else
                    grad=-999.0
                  endif
                  if(isbadb.ne.0) grad=-999.0                                       ! if isbadb ne 0, then previous data was bad
                  gradcrt2=gradcrt(istyp)*0.058*2.*float(nmiss)                     ! gradient criteria per each data (nmiss * 1min) (0.058 is criteria per min for tem, sal, atem)
                  if(gradcrt2.lt.0.1) gradcrt2=0.1
                  isbad=0
                  if (val(moa,ida,iha,ima).lt.crtmn.or.val(moa,ida,iha,ima).gt.crtmx) isbad=1   ! statistics check
                  if (grad.ne.-999.0.and.grad.gt.gradcrt2) isbad=2                              ! gradient check  

! print missing or bad data
                  if (isbad.gt.0)then
                    if (val(moa,ida,iha,ima).eq.-999.)then     ! missing data
                      write(201,8998) cval(istyp),cloc(n),n,moa,ida,iha,ima,val(moa,ida,iha,ima),chname(n)
                    else                                       ! bad data
                      write(202,8999) trim(cval(istyp)),trim(cloc(n)) &
                      ,n,moa,ida,iha,ima,isbad,val(moa,ida,iha,ima) &
                      ,valb,crtmx,crtmn,grad,gradcrt2,trim(chname(n))
                      ndatabad=ndatabad+1
                    endif 
                    if (isav(n).eq.1.and.val(moa,ida,iha,ima).ne.-999.0) then
                    	val(moa,ida,iha,ima)=(valavg(n,istyp,moa)+valmx(n,istyp,moa))/2.
                    endif
                  endif
                  if(val(moa,ida,iha,ima).ne.-999.0.and.isbad.eq.0)then
                    valb=val(moa,ida,iha,ima)
                    nmiss=1
                  else
                    nmiss=nmiss+1
                  endif  
                  isbadb=isbad                  
                endif        
              enddo
            enddo
          enddo
        enddo

8998  format(a11,' at station',a9,1x,i3,','i2,'/',i2,'.',2x,i2,':',i2,3x,f7.1,4x,a15)        
8999  format(a11,' at station',a9,1x,i3,','i2,'/',i2,'.',2x,i2,':',i2,3x,i1,1x,5(f7.2,1x),f7.3,4x,a15)
      endsubroutine
!##############################################################################
!# data qc information read --- END
!##############################################################################

!##############################################################################
      subroutine qcwrt(n,iy,ntemp,nsalt,natem,ntempbad,nsaltbad,natembad)  ! n = station ID
      integer ntemp,nsalt,natem
      integer ntempmiss,nsaltmiss,natemmiss,ntempbad,nsaltbad,natembad
      character*7 cisav
      
      ntotal=24*60/intv(n)
      ntempmiss=24*60/intv(n)-(ntemp)
      nsaltmiss=24*60/intv(n)-(nsalt)
      natemmiss=24*60/intv(n)-(natem)
      !if (n.eq.63) write(*,*) natem,natemmiss

      if(isav(n).eq.0) cisav='exclude'
      if(isav(n).eq.1) cisav='replace'      
      write(200,8996) n,cloc(n),24*60/intv(n),ntempmiss,nsaltmiss,natemmiss,ntempbad,nsaltbad,natembad,cisav,chname(n)

8996  format(i3,1x,a9,3x,7(i4,3x),a7,2x,a15)
      endsubroutine
!##############################################################################
!# data qc information read --- END
!##############################################################################

end