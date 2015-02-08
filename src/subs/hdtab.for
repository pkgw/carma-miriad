c***********************************************************************
c  Routines used by INVERT to build the header of a dataset.
c
c  History:
c    25oct94 rjs  Original version.
c    23nov94 rjs  Fix labelling of velocity axis for velocity linetype.
c    25apr95 rjs  Write "object" rather than "source" item.
c     1nov95 rjs  Added HdDefSiz
c     1jul99 rjs  Create somewhat better headers.
c    28jul99 rjs  Initialise pbtype.
c    09may00 rjs  Do not allow default image size if pbtype=SINGLE.
c    28jun05 rjs  Check the u,v coordinates to see if NCP projection is
c                 best.
c
c $Id$
c***********************************************************************

      subroutine HdInit(mfs1,mosaic1)

      logical mfs1,mosaic1
c-----------------------------------------------------------------------
      include 'hdtab.h'
c-----------------------------------------------------------------------
      mfs    = mfs1
      mosaic = mosaic1
      doinit = .true.

      end

c***********************************************************************

      subroutine HdChk(tno,uvw)

      integer tno
      double precision uvw(3)
c-----------------------------------------------------------------------
c  Check that the header is the same.
c-----------------------------------------------------------------------
      include 'hdtab.h'
      include 'mirconst.h'

      real TOL
      parameter (TOL = (1.0/3600.0)*D2R)

      real t2,rtemp,vsource
      double precision t1,dv,v0,epsi,r0,line(6),dtemp
      character s*16
c-----------------------------------------------------------------------
c     Get all the values we can possibly want when we see a file for the
c     first time.
      if (doinit) then
        call HdFirst(tno)
        doinit = .false.
      endif

      if (count.eq.0) then
        mcount = min(2*mcount+1,MAXCOUNT)

c       Form an average time and observatory velocity.
        call uvrdvrd(tno,'time',dtemp,0d0)
        obstime = obstime + dtemp
        call uvrdvrr(tno,'vsource',vsource,0.0)
        call uvrdvrr(tno,'veldop',rtemp,0.0)
        vobs  = vobs  + rtemp - vsource
        naver = naver + 1

c       Check for a change in the spectral axis.
        if (ctype3(1:4).eq.'VELO') then
          call uvinfo(tno,'line',line)
          nchan = nint(line(2))
          if (.not.rChange) then
            call uvfit1(tno,'restfreq',nchan,r0,epsi)
            rConst  = rConst .and. epsi.lt.restfreq*1d-3
            rChange = abs(restfreq-r0).gt.restfreq*1d-3
          endif
          if (.not.vChange) then
            if (nchan.eq.1) then
              call uvinfo(tno,'velocity',v0)
              call uvinfo(tno,'bandwidth',dv)
              dv = DCMKS * (dv / restfreq) * 1d-3
            else
              call uvfit2(tno,'velocity',nchan,dv,v0,epsi)
              VLinear = VLinear .and. (epsi.lt.0.1d0*abs(cdelt3))
            endif
            vChange = max(abs(crval3-v0),abs(cdelt3-dv)).gt.
     *                                        0.1d0*abs(cdelt3)
          endif
        endif

c       Check for a change in RA.
        if (.not.xChange .and. .not.mosaic) then
          call uvrdvrd(tno,'dec',t1,0d0)
          call uvrdvrr(tno,'dra',t2,0.0)
          t2 = t2 / cos(t1)
          call uvrdvrd(tno,'ra',t1,0d0)
          t1 = t1 + t2
          xChange = abs(crval1-t1).gt.TOL
        endif

c       Check for a change in DEC.
        call uvrdvrd(tno,'dec',t1,0d0)
        call uvrdvrr(tno,'ddec',t2,0.0)
        t1 = t1 + t2
        if (.not.yChange .and. .not.mosaic) then
          yChange = abs(crval2-t1).gt.TOL
        endif

c       Accumulate info to determine whether its an e-w array.
        sumlumv = sumlumv + (cos(t1)*uvw(2)+sin(t1)*uvw(3))**2
        sumuuvv = sumuuvv + uvw(2)*uvw(2) + uvw(3)*uvw(3)

c       Check for a change in source name.
        if (.not.sChange) then
          call uvrdvra(tno,'source',s,source)
          sChange = s.ne.source
        endif
      endif

      Count = mod(Count+1,mcount)

      end

c***********************************************************************

      subroutine HdFirst(tno)

      integer tno
c-----------------------------------------------------------------------
      integer CHANNEL,WIDE,VELOCITY
      parameter (CHANNEL=1,WIDE=2,VELOCITY=3)

      include 'hdtab.h'
      include 'mirconst.h'

      double precision line(6),epsi
      real dra,ddec
      integer itype
c-----------------------------------------------------------------------
c     Initialise logicals used to check whether things are consistent.
      rConst  = .true.
      rChange = .false.
      vChange = .false.
      vLinear = .true.
      sChange = .false.
      xChange = .false.
      yChange = .false.
      count   = 0
      mcount  = 0

c     Handle the first two axes.
      if (.not.mosaic) then
        call uvrdvrd(tno,'ra', crval1,0d0)
        call uvrdvrd(tno,'dec',crval2,0d0)
        call uvrdvrr(tno,'dra', dra, 0.0)
        call uvrdvrr(tno,'ddec',ddec,0.0)
        crval1 = crval1 + dra/cos(crval2)
        crval2 = crval2 + ddec
        call uvrdvrd(tno,'pntra', obsra,crval1)
        call uvrdvrd(tno,'pntdec',obsdec,crval2)
      endif
      call pbRead(tno,pbtype)
      call uvrdvrr(tno,'epoch',epoch,1950.0)

c     Miscellaneous.
      call uvrdvra(tno,'source',source,' ')
      call uvrdvra(tno,'telescop',telescop,' ')
      call uvrdvra(tno,'observer',observer,' ')

c     Initialise.
      sumuuvv = 0.0
      sumlumv = 0.0
      vobs    = 0.0
      obstime = 0.0
      naver   = 0

c     Handle the spectral axis.
      call uvrdvra(tno,'veltype',ctype3,' ')
      call uvinfo(tno,'line',line)
      itype = nint(line(1))
      if (itype.eq.CHANNEL) then
        ltype = 'channel'
      else if (itype.eq.WIDE) then
        ltype = 'wide'
      else if (itype.eq.VELOCITY) then
        ltype = 'velocity'
      else
        call bug('f','Unrecognised line type')
      endif
      nchan = nint(line(2))
      lstart = line(3)
      lwidth = line(4)
      lstep  = line(5)

      call uvfit1(tno,'bandwidth',nchan,cdelt3,epsi)
      if (ltype.ne.'wide') then
        call uvfit1(tno,'restfreq',nchan,restfreq,epsi)
        rConst = epsi.le.restfreq*1d-3
      else
        restfreq = 0d0
      endif

      if (mfs) then
        ctype3(1:4) = 'FREQ'
      else if (restfreq.eq.0) then
        if (nchan.gt.1) then
          call uvfit2(tno,'sfreq',nchan,cdelt3,crval3,epsi)
          VLinear = epsi.le.0.1d0*abs(cdelt3)
        else
          call uvinfo(tno,'sfreq',crval3)
        endif
        ctype3(1:4) = 'FREQ'
      else
        if (nchan.gt.1) then
          call uvfit2(tno,'velocity',nchan,cdelt3,crval3,epsi)
          VLinear = epsi.le.0.1d0*abs(cdelt3)
        else
          call uvinfo(tno,'velocity',crval3)
          cdelt3 = DCMKS * (cdelt3 / restfreq) * 1d-3
        endif
        ctype3(1:4) = 'VELO'
      endif

      end

c***********************************************************************

      subroutine HdDone(tno)

      integer tno
c-----------------------------------------------------------------------
      include 'hdtab.h'
c-----------------------------------------------------------------------
      Count  = 0
      mcount = 1

      end

c***********************************************************************

      subroutine HdSet(dosin,doncp,cellx,celly,ra0,dec0,freq0)

      logical   dosin,doncp
      real      cellx, celly, freq0
      double precision ra0, dec0
c-----------------------------------------------------------------------
c  Set remaining hdtab variables and do some basic checks.
c
c  Input:
c    dosin      Force use of SIN projection.
c    doncp      Force use of NCP projection.
c    cellx      cdelt1
c    celly      cdelt2
c    ra0,dec0   Resets for crval1,crval2 in mosaicing mode only.
c    freq0      Reset for reference frequency in mfs mode only.
c-----------------------------------------------------------------------
      include 'hdtab.h'
      include 'mirconst.h'

      real DECLIM, TOL
      parameter (DECLIM=3.0*D2R, TOL = 0.01)
c-----------------------------------------------------------------------

      cdelt1 = cellx
      cdelt2 = celly

      if (mosaic) then
        crval1 = ra0
        crval2 = dec0
      endif
      if (mfs) crval3 = freq0

      if (dosin .or.
     *    (abs(crval2).lt.DECLIM .or.
     *    sumlumv/sumuuvv.gt.TOL).and..not.doncp) then
        ctype1 = 'RA---SIN'
        ctype2 = 'DEC--SIN'
      else
        ctype1 = 'RA---NCP'
        ctype2 = 'DEC--NCP'
      endif

c     Check for things that have changed.
      if (.not.rConst) call bug('w',
     *    'Rest frequencies varied between channels by > 0.1%')
      if (rChange) call bug('w',
     *    'Rest frequencies varied by > 0.1% while reading data')
      if (.not.vLinear) call bug('w',
     *    'Channel frequencies/velocities deviated by > 10% from '//
     *    'linearity')
      if (vChange) call bug('w',
     *    'Channel velocities varied by > 10% while reading data')
      if (xChange) call bug('w',
     *    'The source RA changed by > 1 arcsec while reading data')
      if (yChange) call bug('w',
     *    'The source DEC changed by > 1 arcsec while reading data')

      end

c***********************************************************************

      subroutine HdCoObj(coObj)

      integer coObj
c-----------------------------------------------------------------------
c  Generate a coordinate object.
c-----------------------------------------------------------------------
      include 'hdtab.h'
c-----------------------------------------------------------------------
      call coCreate(3, coObj)
      call coAxSet(coObj,1,ctype1,0d0,crval1,cdelt1)
      call coAxSet(coObj,2,ctype2,0d0,crval2,cdelt2)
      call coAxSet(coObj,3,ctype3,1d0,crval3,cdelt3)
      call coSetd(coObj,'vobs',dble(vobs/naver))
      if (restfreq.ne.0) call coSetd(coObj,'restfreq',restfreq)
      call coSetd(coObj,'obstime',obstime/naver)
      call coSetd(coObj,'epoch',dble(epoch))
      if (nchan.eq.1 .or. mfs) call coSeta(coObj,'cellscal','CONSTANT')
      call coReinit(coObj)

      end

c***********************************************************************

      subroutine HdDefSiz(nx,ny)

      integer nx,ny
c-----------------------------------------------------------------------
c  Determine the default size of an image.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'hdtab.h'
      real fwhm,cutoff,maxrad
      integer coObj,pbObj
c-----------------------------------------------------------------------
c     Determine the FWHM of the telescope.
      if (pbtype.eq.' ' .or. pbtype.eq.'SINGLE') then
        call bug('f',
     *    'Unknown telescope -- cannot determine default image size')
      else
        call HdCoObj(coObj)
        call pbInit(pbObj,pbtype,coObj)
        call pbInfo(pbObj,fwhm,cutoff,maxrad)
        call pbFin(pbObj)
        call coFin(coObj)
      endif

      if (nx.gt.0.and.ny.gt.0) then
        nx = max(nint(abs(nx*fwhm/cdelt1)),1)
        ny = max(nint(abs(ny*fwhm/cdelt2)),1)
      else
        nx = max(nint(abs(fwhm/cdelt1)),1)
        ny = max(nint(abs(fwhm/cdelt2)),1)
      endif
      end

c***********************************************************************

      subroutine HdWrite(tno,rms,nx,ny)

      integer tno
      real rms
      integer nx,ny
c-----------------------------------------------------------------------
c  Write all the header rubbish out to an image dataset.
c-----------------------------------------------------------------------
      include 'hdtab.h'
c-----------------------------------------------------------------------
      call wrhda(tno,'ltype', ltype)
      call wrhdr(tno,'lstart',lstart)
      call wrhdr(tno,'lwidth',lwidth)
      call wrhdr(tno,'lstep', lstep)

      if (telescop.ne.' ') call wrhda(tno,'telescop',telescop)
      if (source.ne.' ')  call wrhda(tno,'object',  source)
      if (observer.ne.' ') call wrhda(tno,'observer',observer)
      if (rms.gt.0) call wrhdr(tno,'rms',rms)
      if (.not.mosaic) then
        if (obsra.ne.crval1 .or. obsdec.ne.crval2) then
          call mosInit(nx,ny)
          call mosSet(1,obsra,obsdec,rms,pbtype)
          call mosSave(tno)
        else if (pbtype.ne.telescop) then
          call wrhda(tno,'pbtype',pbtype)
        endif
      endif

      end
