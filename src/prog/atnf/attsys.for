c************************************************************************
	program attsys
c
	implicit none
c
c= attsys - Apply or un-apply on-line Tsys values.
c& rjs
c: uv analysis
c+
c	ATTSYS either applies or removes the Tsys weighting from correlation
c	data.
c
c	NOTE: If you are using two IF bands when observing, ATTSYS cannot
c	be used after you have split or copied the file down to single-IF
c	datasets.
c@ vis
c	The names of the input uv data sets. No default.
c@ out
c	The name of the output uv data set. No default.
c@ options
c	Extra processing options. Several options can be given,
c	separated by commas. Minimum match is supported. Possible values
c	are:
c	  apply     Apply the Tsys correction to the data. This is the
c	            default.
c	  unapply   Undo the Tsys correction in the data. Note that you
c	            cannot use the "apply" and "unapply" options
c	            simultaneously.
c	  auto      Use the "tcorr" variable to determine whether Tsys
c	            has been applied or not.
c	            NOTE: Information needed for options=auto is lost if you
c	            copy or split a dataset. If you are going to use 
c	            options=auto, you generally have to do it on the file
c	            resulting from atlod.
c--
c  History:
c    17jul00 rjs  Original version.
c    25may02 rjs  Added options=auto
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='AtTsys: version 1.0 25-May-02')
c
	integer lVis,lOut,vupd,pol,npol,i1,i2
	logical updated,doapply,auto
	character vis*64,out*64,type*1
	integer nschan(MAXWIN),nif,nchan,nants,length,tcorr,na
	real xtsys(MAXANT*MAXWIN),ytsys(MAXANT*MAXWIN)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(5)
c
c  Externals.
c
	logical uvvarUpd
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call GetOpt(doapply,auto)
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
c
c  Get ready to copy the data.
c
	call uvopen(lVis,vis,'old')
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvvarIni(lVis,vupd)
	call uvvarSet(vupd,'xtsys')
	call uvvarSet(vupd,'ytsys')
	call uvvarSet(vupd,'nschan')
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'ATTSYS: Miriad '//version)
	call hisinput(lOut,'ATTSYS')
	call hisclose(lOut)
c
c  Get first record.
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
c
c  If auto mode has been requested, check that the "tcorr" variable
c  is present.
c
	if(auto)then
	  call uvprobvr(lVis,'tcorr',type,length,updated)
	  if(length.ne.1)call bug('f',
     *		'Required info for options=auto is missing')
	endif
	call uvrdvri(lVis,'nants',na,0)
c
	dowhile(nchan.gt.0)
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
c
	  if(uvvarUpd(vupd))then
	    call uvprobvr(lVis,'nschan',type,length,updated)
	    nif = length
	    if(type.ne.'i'.or.length.le.0.or.length.gt.MAXWIN)
     *	      call bug('f','Invalid nschan parameter')
	    call uvgetvri(lVis,'nschan',nschan,nif)
c
	    call uvprobvr(lVis,'xtsys',type,length,updated)
	    nants = length/nif
	    if(nants*nif.ne.length.or.nants.le.0.or.nants.gt.MAXANT
     *	      .or.type.ne.'r')call bug('f','Invalid tsys parameter')
	    if(na.ne.nants)
     *		call bug('f','Inconsistency in number of IFs')
	    call uvgetvrr(lVis,'xtsys',xtsys,nants*nif)
	    call uvprobvr(lVis,'ytsys',type,length,updated)
	    if(nants*nif.ne.length.or.type.ne.'r')
     *			      call bug('f','Invalid ytsys parameter')
	    call uvgetvrr(lVis,'ytsys',ytsys,nants*nif)
	  endif
c
	  call basant(preamble(5),i1,i2)
	  if(auto)then
	    if(doapply)then
	      call uvrdvri(lVis,'tcorr',tcorr,0)
	    else
	      call uvrdvri(lVis,'tcorr',tcorr,1)
	    endif
	    if(doapply.eqv.(tcorr.eq.0))call tsysap(data,nchan,nschan,
     *		xtsys,ytsys,nants,nif,doapply,i1,i2,pol)
	  else
	    call tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *						doapply,i1,i2,pol)
	  endif
c
	  call varCopy(lVis,lOut)
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *						doapply,i1,i2,pol)
c
	implicit none
	integer nchan,nants,nif,nschan(nif),i1,i2,pol
	real xtsys(nants,nif),ytsys(nants,nif)
	logical doapply
	complex data(nchan)
c
c------------------------------------------------------------------------
	integer XX,YY,XY,YX
	parameter(XX=-5,YY=-6,XY=-7,YX=-8)
	integer i,j,k
	real T1T2
c
	i = 0
	do k=1,nif
	  if(i+nschan(k).gt.nchan)call bug('f','Invalid description')
	  do j=1,nschan(k)
	    i = i + 1
	    if(pol.eq.XX)then
	      T1T2 = xtsys(i1,k)*xtsys(i2,k)
	    else if(pol.eq.YY)then
	      T1T2 = ytsys(i1,k)*ytsys(i2,k)
	    else if(pol.eq.XY)then
	      T1T2 = xtsys(i1,k)*ytsys(i2,k)
	    else if(pol.eq.YX)then
	      T1T2 = ytsys(i1,k)*xtsys(i2,k)
	    else
	      call bug('f','Invalid polarization code')
	    endif
c
	    if(doapply)then
	      data(i) = data(i)*sqrt(T1T2)/50.0
	    else
	      data(i) = data(i)*50.0/sqrt(T1T2)
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine getopt(doapply,auto)
c
	implicit none
	logical doapply,auto
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*10
	logical present(NOPTS)
c
	data opts/'apply     ','unapply   ','automatic '/
c
	call options('options',opts,present,NOPTS)
	if(present(1).and.present(2))call bug('f',
     *	  'Cannot both apply and unapply Tsys correction')
	doapply = .not.present(2)
	auto    = present(3)
c
	end
