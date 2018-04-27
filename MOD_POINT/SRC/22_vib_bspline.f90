!234567
      integer nt,n,nn ! nt: no. of locations
      
      logical lfn
      
      integer,allocatable,dimension(:)::ikey
      character*7,allocatable,dimension(:)::cloc
      integer,allocatable,dimension(:)::inf    ! 1:On, 2:Off
      integer,allocatable,dimension(:)::iflag  ! 1:KHOA, 2:NFRDI      
      integer,allocatable,dimension(:)::ioc    ! 1:west, 2:south, 3:north
      integer,allocatable,dimension(:)::item,isal,iatm
      real,allocatable,dimension(:)::utmx,utmy
      real*8,allocatable,dimension(:)::rlon,rlat
      character*120,allocatable,dimension(:)::chname
      integer,allocatable,dimension(:)::imod
      
      character*120 cHOME,cINP,cLOG,cSRC,cEXE,cSHL
      character*120 cpre0_1,cpre1,cpre2,crst
      
      character*8 ctoday
      real*8 tmin(3),tmax(3),smin(3),smax(3),tamin(3),tamax(3)
      real*8 beta0(3),beta11(3),beta21(3),beta31(3)
      real*8 beta12(3),beta22(3),beta32(3)
      real*8 beta13(3),beta23(3),beta33(3)
      real*8 rlv1,rlv2,rlv3
      real*8 b0(10),b1(10)

      call init

!      nn=0
      do n=1,nn
!      	if (inf(n).eq.1) then
          call setfn
!          call bspline2
          call logist
!        endif
      enddo

CONTAINS

!#############################################################
      subroutine samp
      endsubroutine

!#############################################################
      subroutine logist

      if (.not.lfn) goto 330

  311 read(31,*,end=319) tm,t,s,ta,idm,iy,im,id,ih
      !write(*,*) tm,t,s,ta,idm,iy,im,id,ih
      
      nt=ioc(n)
      b0c=b0(imod(n))
      b1c=b1(imod(n))
      calc=b0c + b1c*t
      p=exp(calc)/(1+exp(calc))
      if (b0c.eq.0. .and. b1c.eq.0.) p=0.
!            write(*,*) imod(n),cloc(n),b0c,b1c,p,t
      
      if (p.ge.0. .and. p.lt.rlv1) then
     	  indexa=1
      elseif (p.ge.rlv1 .and. p.lt.rlv2) then
       	indexa=2
      elseif (p.ge.rlv2 .and. p.lt.rlv3) then
       	indexa=3
      elseif (p.ge.rlv3 .and. p.lt.1.) then
       	indexa=4
      else
        indexa=0
      endif
      
      if (t.lt.-0.3) then
        indexa=0
        p=0.
      endif
        
      write(51,5102) tm,p,indexa,t,s,ta,iy,im,id,ih
      goto 311
  319 continue
      
      close(31)
  
  330 continue
      close(51)
      
 5102 format(f10.5,',',f7.4,',',i1,',',3(f6.1,','),i8,',',3(i4,','))

      endsubroutine ! logist

!#############################################################

!#############################################################
      subroutine bspline2

  311 read(31,*,end=319) tm,t,s,ta,idm,iy,im,id,ih
      !write(*,*) tm,t,s,ta,idm,iy,im,id,ih
      
      nt=ioc(n)
      if (t.ge.tmin(nt) .and. s.ge.smin(nt) .and. ta.ge.tamin(nt) .and. &
          t.le.tmax(nt) .and. s.le.smax(nt) .and. ta.le.tamax(nt)) then

        u1=(t-tmin(nt))/(tmax(nt)-tmin(nt))
        u2=(s-smin(nt))/(smax(nt)-smin(nt))
        u3=(ta-tamin(nt))/(tamax(nt)-tamin(nt))
        
        if(u1.lt.0.5)then
            b11=(1.-2.*u1)**2.
            b21=2.*u1*(2.-3.*u1)
            b31=2.*u1**2.
        else
            b11=0.
            b21=2.*(1.-u1)**2.
            b31=-2.*(1.-4.*u1+3.*u1**2.)
        endif  
        if(u2.lt.0.5)then
            b12=(1.-2.*u2)**2.
            b22=2.*u2*(2.-3.*u2)
            b32=2.*u2**2.
        else
            b12=0.
            b22=2.*(1.-u2)**2.
            b32=-2.*(1.-4.*u2+3.*u2**2.)
        endif
        if(u3.lt.0.5)then
            b13=(1.-2.*u3)**2.
            b23=2.*u3*(2.-3.*u3)
            b33=2.*u3**2.
        else
            b13=0.
            b23=2.*(1.-u3)**2.
            b33=-2.*(1.-4.*u3+3.*u3**2.)
        endif
    
        p1=(beta0(nt) + beta11(nt)*b11 + beta21(nt)*b21 + beta31(nt)*b31 &
         + beta12(nt)*b12 + beta22(nt)*b22 + beta32(nt)*b32 + beta13(nt)*b13 &
         + beta23(nt)*b23 + beta33(nt)*b33)
        p=exp(p1) / (1. + exp(p1))
        
        if (p.gt.0. .and. p.lt.rlv1) then
      	  indexa=1
        elseif (p.ge.rlv1 .and. p.lt.rlv2) then
        	indexa=2
        elseif (p.ge.rlv2 .and. p.lt.rlv3) then
        	indexa=3
        elseif (p.ge.rlv3 .and. p.lt.1.) then
        	indexa=4
        else
          indexa=0
        endif
        
      else
        p=-9.99
        indexa=0
      endif

      write(51,5102) tm,p,indexa,t,s,ta,iy,im,id,ih
      goto 311
  319 continue
      close(51)
      close(31)
 5102 format(f10.5,',',f7.4,',',i1,',',3(f6.1,','),i8,',',3(i4,','))

      endsubroutine ! bspline2
      
!!#############################################################
      subroutine setfn
      
      character*120 cin,cout
      character*120 ctmp
      
      !write(cin,'(7a)') "./",trim(cpre1),"/",ctoday,"/",trim(cloc(n)),".csv"
      write(cin,'(8a)') trim(cHOME),"/",trim(cpre1),"/",ctoday,"/",trim(cloc(n)),".csv"
      
      inquire (file=trim(cin), exist = lfn)
      if (.not.lfn) goto 320
      
      write(*,*) trim(cin)
      open(31,file=trim(cin),status="unknown")
      read(31,*) 
      read(31,*)
      read(31,*)
      
  320 continue
      if (.not.lfn) then
      	write(*,'(a,i3,a,a)') "22 NO FILE: ",n," ",trim(cin)
      endif
      
      !write(cout,'(7a)') "./",trim(cpre2),"/",ctoday,"/",trim(cloc(n)),".csv"
      write(cout,'(8a)') trim(cHOME),"/",trim(cpre2),"/",ctoday,"/",trim(cloc(n)),".csv"
      write(*,*) trim(cout)
      open(51,file=trim(cout),status="unknown")
      write(51,5101) cloc(n),rlat(n),rlon(n),trim(chname(n))
 5101 format(a,',',2(f10.6,','),a)
      write(51,'(a)') 'yearday,prob,idexa    ,temp  ,salt  ,atem       ,y   ,m   ,d   ,h'

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
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/21_ts.csv'
      open(21,file=trim(cdum),status='old')
      !open(21,file='21_ts_v170720.csv',status='old')
      read(21,*)
  211 read(21,*,end=219) nn
      goto 211
  219 continue
      !write(*,*) nt
      allocate(ikey(nn))
      allocate(cloc(nn))
      allocate(inf(nn))
      allocate(iflag(nn))
      allocate(ioc(nn))
      allocate(item(nn))
      allocate(isal(nn))
      allocate(iatm(nn))
      allocate(utmx(nn))
      allocate(utmy(nn))
      allocate(rlon(nn))
      allocate(rlat(nn))
      allocate(chname(nn))
      allocate(imod(nn))
      
      rewind(21)
      read(21,*)
      do n=1,nn
      	read(21,*) ikey(n),cloc(n),inf(n),iflag(n),ioc(n), &
      	           item(n),isal(n),iatm(n),utmx(n),utmy(n), &
      	           rlon(n),rlat(n),chname(n),idum,idum,imod(n)
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
      read(22,*)rlv1
      read(22,*)rlv2
      read(22,*)rlv3
      close(22)
      
      cdum=trim(cHOME)//'/'//trim(cINP)//'/coefficient3.inp'
      open(unit=23,file=trim(cdum),status='old')
      read(23,*)
      do i=1,10
        read(23,*) idum,b0(i),b1(i)
      enddo
      read(23,*)
      read(23,*)rlv1
      read(23,*)rlv2
      read(23,*)rlv3
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


end