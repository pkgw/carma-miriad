c************************************************************************
	program uvmodel
	implicit none

c= uvmodel - Add, subtract, etc, a model from a uv data set.
c& mchw
c: uv analysis
c+
c	UVMODEL is a MIRIAD task which modifies a visibility dataset by a model.
c	Allowed operations are adding, subtracting, multiplying, dividing,
c	replacing, and polarization calibration or simulation.
c	The model is specified in the image domain, so that its
c	Fourier transform is first computed before application to the
c	visibilities. The model may be either an image (e.g., a CLEAN
c	component image) or a point source.
c
c	An example is as follows. UVMODEL could be used to remove CLEAN
c	components from a visibility data file.  The residual data base could
c	then be examined for anomalous points, which could in turn be clipped.
c	UVMODEL could then be reapplied to add the CLEAN components back into
c	the visibility data base for re-imaging. 
c@ vis
c	Input visibility data file. No default
c@ model
c	Input model cube. The default is a point source model.
c	This will generally be a deconvolved map, formed from the visibility
c	data being modified. It should be made with ``channel'' linetype.
c	The model should have units of JY/PIXEL and be weighted by the primary
c	beam. The task DEMOS can be used to extract primary beam weighted
c	models from a mosaiced image.
c@ select
c	The standard uv selection subcommands. The default is all data.
c@ options
c	This gives extra processing options. Several values can be given
c	(though many values are mutually exclusive), separated by commas.
c	Option values can be abbreviated to uniqueness.
c	Possible options are (no default):
c	  add       Form: out = vis + model
c	  subtract  Form: out = vis - model
c	  multiply  Form: out = vis * model
c	  divide    Form: out = vis / model
c	  replace   Form: out = model
c	  flag      Form: out = vis, but flag data where the difference
c	            between vis and model is greater than "sigma" sigmas.
c	  polcal    Correct polarization leakage using a total intensity model.
c			out = vis - model * (polcor(q,i)+conjg(polcor(p,j)))
c			where p,q are the polarization and i,j the antennas.
c	  poleak    Simulate polarization leakage using a total intensity model.
c			out = model * (polcor(q,i)+conjg(polcor(p,j)))
c			where p,q are the polarization and i,j the antennas.
c	  unflag    Unflag any flagged data in the output.
c	  autoscale Adjust the scale of the model to minimise the difference
c	            between the model and the visibility.
c	  apriori   Use flux from flux table or data file planet info.
c	  imhead    Much ``header'' information in the uv file is ignored -- the
c	            model information is used instead. In particular, the
c	            observing center of the uv file is taken to be the reference
c	            pixel of the model. By default, if the reference pixel
c	            and the uv data observing center are different, a phase
c	            shift is applied to the model visibilities to align them.
c	            The ``imhead'' option prevents this shifting.
c	  selradec  This causes UVMODEL to select only those visibilities
c	            whose observing center is within plus or minus three
c	            pixels of the model reference pixel. This is needed
c	            if there are multiple pointings or multiple sources in
c	            the input uv file. By default no observing center
c	            selection is performed.
c	  polarized The source is polarized. By default the source is
c	            assumed to be unpolarized. For a polarized source,
c	            UVMODEL cannot perform polarization conversion. That is,
c	            if the model is of a particular polarization, then the
c	            visibility file should contain that sort of polarization.
c	            For example, if the model is Stokes-Q, then the visibility
c	            file should contain Stokes-Q.
c	  mfs       This is used if there is a single plane in the input
c	            model, which is assumed to represent the data at all
c	            frequencies. This should also be used if the model has
c	            been derived using MFCLEAN.
c	  zero      Use the value zero for the model if it cannot be 
c	            calculated. This can be used to avoid flagging the 
c	            data in the outer parts of the u-v-plane when subtracting
c	            a low resolution model.
c	  imag      Process imaginary part of the model. 
c	            (multiply (0+j)*(real_fft+j*imag_fft)
c	The operations add, subtract, multiply, divide, replace and flag are
c	mutually exclusive. The operations flag and unflag are also mutually
c	exclusive.
c
c	The unflag option should be used with caution. Data in the output
c	may still be flagged, if it was not possible to calculate the
c	model.
c@ polcor
c	The instrumental polarization for LR and RL data (or XY and YX).
c	Four values for each antenna, being the real and imaginary parts for
c	leakage into L and R polarizations respectively. See options=polcal.
c	The leakage table is usually obtained from the tasks PCAL, or GPCAL.
c@ clip
c	Clip level. Pixels in the model below this level are set to zero.
c	The default is not to perform any clipping.
c@ flux
c	If MODEL is blank, then the flux (Jy) of a point source model should
c	be specified here. Also used as the default flux in the apriori
c	option. The default is 1 (assuming the model parameter is not given).
c@ offset
c	The RA and DEC offsets (arcseconds) of the point source from the
c	observing center. A point source to the north and east has positive
c	offsets. Defaults are zero.
c@ line
c	The visibility linetype to use, in the standard form, viz:
c	  type,nchan,start,width,step
c	Generally if there is an input model, this defaults to the linetype
c	parameters used to construct the map. For a point source or planet
c	model, the default is all channels. If you wish to override these
c	defaults, or if the info is not present in the header, this parameter
c	can be useful.
c@ sigma
c	For options=flag, UVMODEL flags those points in the output that
c	differ by more than "sigma" sigmas. The default is 100.
c@ out
c	Output visibility data file name. No default. The output file will
c	contain only as many channels as there are planes in the model
c	cube. The various uv variables which describe the windows are
c	adjusted accordingly.
c--
c
c  History:
c    rjs  28mar90 Original version.
c    rjs  31mar90 Changed maxchan. apriori option. Concat. with char*(*) bug.
c	 	  Improved handling of flagged data and the unflag option.
c    rjs  12apr90 Added support for "wide" and "velocity" linetypes.
c    rjs  24apr90 Checked that "out" keyword was set.
c    pjt   2may90 maxdim.h now defines maxchan
c    mchw 26may90 Corrected code in main and WindUpd for model image.
c    mchw 19nov90 Added option "imhead" to copy image header to uvvariables.
c    mchw 15dec90 Checked for variable "nants" in uvdata.
c    rjs  11mar91 Copy "chi" variable, and change "npols" to "npol".
c    rjs  25mar91 Added "line" task parameter.
c    mchw 27mar91 Better bookkeeping in WindUpd. Added more variables.
c    rjs   1jul91 Corrected calculation of wwidth in the output file.
c    mjs  04aug91 Replaced local maxants with maxdim.h MAXANT
c    rjs  29aug91 Complete rework of WindUpd.
c    nebk 29aug91 Make user specify an option.
c    rjs   1nov91 Added options=mfs and polarized. Simple polarisation
c		  processing.
c    rjs  29jan92 New call sequence to model.
c    rjs  30mar92 Added option selradec.
c    rjs  26apr92 Better model processing.
c    rjs  15jun92 Use "Var" routines to simplify copying uv files.
c    rjs  23jun92 Doc and message changes only.
c    mchw 04aug92 Corrected sign of vstart written into uvdata.
c    rjs  17aug92 Updated call sequence to var routines, plus other
c		  tidying.
c    mchw 04sep92 Fixed a missing argument in uvVarCpy.
c    rjs  15feb93 Changes to make ra,dec variables double.
c    rjs  29mar93 Fiddles with the sigma.
c    rjs  23dec93 Minimum match of linetype name.
c    rjs  31jan95 Changes to support w-axis.
c    mhw  05jan96 Add zero option to avoid flagging outer uvplane
c    mchw 25jul96 Polarization calibration.
c    mchw 29aug96 No Polarization selection of uv-data for options=polcal.
c    mchw 20may97 Swap p,q in  options=polcal.
c    mchw 22may97 copy across telescop and not pbfwhm in options=imhead.
c    mchw 05nov97 Add options=poleak.
c    pjt  17mar01 documented the change Mel made with increased maxsels in 98
c    mchw 09jan08 Add options=imaginary to handle imaginary image.
c    mchw 20aug08  include 'mem.h'
c    mchw 03apr09 out(1) = sqrt(abs(sigma2)) in subroutine header.
c    pkgw 15mar11 Use scrrecsz() to allow very large scratchfiles
c    pjt  27aug13 Ensure correct type ptrdiff for scrread
c  Bugs:
c    * Polarisation processing is pretty crude.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	character version*(*)
	parameter(version='version 1.0 27-aug-2013')
	integer maxsels,nhead,nbuf
	parameter(maxsels=1024,nhead=1,nbuf=5*maxchan+nhead)
c
	character vis*64,modl*64,out*64,oper*8,ltype*32,type*1
	character flag1*8,flag2*8
	logical unflag,autoscal,apriori,updated,imhead,defline
	logical mfs,doPol,selradec,doclip,zero,doimag
	real sels(maxsels),offset(2),flux,clip,sigma,lstart,lstep,lwidth
	integer nsize(3),nchan,nread,nvis,length,i
	integer tvis,tmod,tscr,tout,vcopy,pol
	ptrdiff ip
	double precision preamble(5)
	complex data(maxchan)
	logical flags(maxchan)
	real buffer(nbuf)
	real polcor(4*MAXANT)
	integer npolcor,npol
c	complex polcor(2,MAXANT)/2*MAXANT*cmplx(0.,0.)/
c
	logical calcrms,dounflag
	common/uvmodcom/calcrms,dounflag
c
c  Externals.
c
	external header,calget
	logical keyprsnt
c
c  Get the input parameters.
c
	call output('Uvmodel: '//version)
	call keyini
	call keya('vis',vis,' ')
	call SelInput('select',sels,maxsels)
	call GetOpt(oper,unflag,autoscal,apriori,imhead,mfs,doPol,
     *		selradec,zero,doimag)
	call keya('model',modl,' ')
	doclip = keyprsnt('clip')
	call keyr('clip',clip,0.)
	call keyr('flux',flux,1.)
	call keyr('sigma',sigma,100.)
	call keyr('offset',offset(1),0.)
	call keyr('offset',offset(2),0.)
	call keyline(ltype,nchan,lstart,lwidth,lstep)
	call keya('out',out,' ')
	call mkeyr('polcor',polcor,4*MAXANT,npolcor)
	call keyfin
c
c  Check the input parameters.
c
	if(vis.eq.' ')
     *	  call bug('f','Input visibility file name must be given')
	if(modl.eq.' '.and.(flux.eq.0.or.imhead))
     *	  call bug('f','There was no input model')
	if(out.eq.' ')
     *	  call bug('f','Output visibility file name must be given')
	if(oper.eq.'flag'.and.sigma.le.0)
     *	  call bug('f','Sigma must be positive valued.')
	if(modl.eq.' '.and.abs(offset(1))+abs(offset(2)).eq.0)
     *	  call output('Model is a point source at the observing center')
c
c  Miscellaneous initialisation.
c
	call uvopen(tvis,vis,'old')
c
c  Determine the flags to the ModelIni and Model routines.
c
	if(oper.ne.'polcal'.and.oper.ne.'poleak') flag1 = 's'
	if(selradec) flag1(2:2) = 'p'
	if(doPol)    flag1(3:3) = 't'
	flag2 = ' '
	if(autoscal) flag2(1:1) = 'a'
	if(apriori)  flag2(2:2) = 'c'
	if(imhead)   flag2(3:3) = 'h'
	if(mfs)	     flag2(4:4) = 'm'
	if(doclip)   flag2(5:5) = 'l'
	if(zero)     flag2(6:6) = 'z'
c       
	calcrms = oper.eq.'flag'
	dounflag = unflag
	pol = 0
c
c  Determine the default linetype from the uvdata if needed.
c
	Defline = ltype.eq.' '
	if(Defline)then
	  call uvprobvr(tvis,'corr',type,length,updated)
	  if(type.eq.'j'.or.type.eq.'r')then
	    ltype = 'channel'
	  else
	    call uvprobvr(tvis,'wcorr',type,length,updated)
	    if(type.ne.'c')call bug('f',
     *		'Visibility file contains neither corr nor wcorr')
	    ltype = 'wide'
	  endif
	  nchan = 0
	  lstart = 1.
	  lstep = 1.
	  lwidth = 1.
	endif
c
c  Do the model calculation for point source, or for model image.
c
	if(Modl.eq.' ')then
	  call SelApply(tvis,sels,.true.)
	  call uvset(tvis,'data',ltype,nchan,lstart,lwidth,lstep)
	  call Model(flag2,tvis,0,offset,flux,tscr,
     *		nhead,header,calget,nchan,nvis)
	else
	  call xyopen(tmod,Modl,'old',3,nsize)
	  if(Defline)then
	    call rdhda(tmod,'ltype',ltype,ltype)
	    call rdhdr(tmod,'lstart',lstart,lstart)
	    call rdhdr(tmod,'lwidth',lwidth,lwidth)
	    call rdhdr(tmod,'lstep',lstep,lstep)
	    if(.not.mfs)nchan = nsize(3)
	  endif
	  call uvset(tvis,'data',ltype,nchan,lstart,lwidth,lstep)
	  call ModelIni(tmod,tvis,sels,flag1)
	  call Model(flag2,tvis,tmod,offset,Clip,tscr,
     *		nhead,header,calget,nchan,nvis)
	  call GetPol(tmod,doPol,pol)
	endif
c
c  We have computed the model. Reset the input file and ready ourselves for
c  a copy operation.
c
	call uvrewind(tvis)
	call uvset(tvis,'coord','nanosec',0,0.,0.,0.)
	call uvset(tvis,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
	call uvopen(tout,out,'new')
	call uvset(tout,'preamble','uvw/time/baseline',0,0.,0.,0.)
	if(imhead)then
	  call ImHedIni(tvis,vcopy)
	else
	  call VarInit(tvis,ltype)
	endif
	call VarOnit(tvis,tout,ltype)
	if(.not.(pol.eq.0.or.oper.eq.'polcal'.or.oper.eq.'poleak'))then
	  call wrhdi(tout,'npol',1)
	  call wrhdi(tout,'pol',pol)
	  call uvputvri(tout,'npol',1,1)
	  call uvputvri(tout,'pol',pol,1)
	endif
c
c  Perform the copying.
c
	length = 5*nchan + nhead
	call scrrecsz(tscr,length)
	do i=1,nvis
	  call uvread(tvis,preamble,data,flags,maxchan,nread)
	  if(nread.ne.nchan) call bug('f',
     *	    'No. channels  unexpectedly changed, when rereading data')
	  ip = i-1
	  call scrread(tscr,buffer,ip,1)
	  call process(oper,buffer(1)*sigma,buffer(nhead+1),
     *		data,flags,nchan,tvis,preamble(5),maxant,polcor,doimag)
	  if(imhead)then
	    if(i.eq.1) call ImHed1st(tmod,tout,nchan)
	    call uvVarCpy(vcopy,tout)
	  else
	    call VarCopy(tvis,tout)
	  endif
	  if(oper.eq.'polcal'.or.oper.eq.'poleak')then
	    call uvgetvri(tvis,'npol',npol,1)
	    call uvgetvri(tvis,'pol',pol,1)
	    call uvputvri(tout,'npol',npol,1)
	    call uvputvri(tout,'pol',pol,1)
	  endif
	  call uvwrite(tout,preamble,data,flags,nchan)
	enddo
	call scrclose(tscr)
c
c  Make the history of the output and close up shop.
c
	call hdcopy(tvis,tOut,'history')
	call hisOpen(tOut,'append')
	call hisWrite(tOut,'UVMODEL: Miriad UvModel '//version)
	call hisInput(tOut,'UVMODEL')
	call hisClose(tOut)
	if(Modl.ne.' ') call xyclose(tmod)
	call uvClose(tvis)
	call uvClose(tOut)
	end
c************************************************************************
	subroutine GetPol(tmod,doPol,pol)
c
	implicit none
	integer tmod,pol
	logical doPol
c
c  Determine the polarisation type of the output.
c
c  Input:
c    tmod	Handle of the input model.
c    doPol	Is the source polarised?
c  Output:
c    pol	Polarisation of the output vis data.
c------------------------------------------------------------------------
	integer PolI
	parameter(PolI=1)
c
	integer naxis,i
	character ctype*16,num*2
	real crval,crpix,cdelt
c
c  Externals.
c
	character itoaf*2
c
c  Determine the polarisation type of the model.
c
	pol = PolI
	if(doPol)then
	  call rdhdi(tmod,'naxis',naxis,0)
	  do i=3,naxis
	    num = itoaf(i)
	    call rdhda(tmod,'ctype'//num,ctype,' ')
	    if(ctype.eq.'STOKES')then
	      call rdhdr(tmod,'crval'//num,crval,1.)
	      call rdhdr(tmod,'crpix'//num,crpix,1.)
	      call rdhdr(tmod,'cdelt'//num,cdelt,1.)
	      pol = nint( crval + (1-crpix)*cdelt )
	    endif
	  enddo
	endif
	end
c************************************************************************
	subroutine header(tvis,preamble,data,flags,nchan,
     *						accept,Out,nhead)
	implicit none
	integer tvis,nchan,nhead
	complex data(nchan)
	logical flags(nchan),accept
	real Out(nhead)
	double precision preamble(5)
c
c  This is a service routine called by the model subroutines. It is
c  called every time a visibility is read from the data file.
c
c  Input:
c    tvis	Handle of the visibility file.
c    nhead	The value of nhead
c    nchan	The number of channels.
c    preamble	Preamble returned by uvread.
c    data	A complex array of nchan elements, giving the correlation data.
c		Not used.
c    flags	The data flags. Not used.
c  Output:
c   out		The nhead values to save with the data. One value is
c		returned:
c		  out(1) -- rms error.
c   accept	This determines whether the data is accepted or discarded.
c		It is always accepted unless the baseline number looks bad.
c------------------------------------------------------------------------
	integer i
	double precision sigma2
c
	logical calcrms,dounflag
	common/uvmodcom/calcrms,dounflag
c
c  Unflag the data if necessary.
c
	if(dounflag)then
	  do i=1,nchan
	    flags(i) = .true.
	  enddo
	endif
c
c  Determine the rms error, if needed.
c
	if(calcrms)then
	  call uvinfo(tvis,'variance',sigma2)
      out(1) = sqrt(abs(sigma2))
c      print *, 'variance',sigma2, 'rms= ',out(1)
c 03apr2009:  out(1) = sigma2 replaced by out(1) = sqrt(abs(sigma2))
	  accept = sigma2.gt.0
	else
	  Out(1) = 0.
	  accept = .true.
	endif
	end
c************************************************************************
	subroutine process(oper,rms,buffer,
     *		data,flags,nchan,tvis,preamble,maxant,polcor,doimag)
c
	implicit none
	integer nchan,tvis,maxant
	character oper*(*)
	real rms,buffer(5,nchan),real_fft,imag_fft
	complex data(nchan)
	logical flags(nchan),doimag
	double precision preamble
	complex polcor(2,MAXANT)
c
c  Perform the desired operation on the data.
c
c  Input:
c    oper	Operation to perform.
c    rms	Estimate of the visibility rms (only if oper='flag').
c    nchan	Number of channels.
c    buffer	The visibility, model and flag data, returned by the
c		model subroutine.
c    tvis	Handle of the visibility file.
c    preamble	Preamble returned by uvread.
c    polcor 	The instrumental polarization for LR and RL data.
c    doimag  process imaginary part of the model.
c  Input/Output:
c       On input, these are the original values. On output, these are the values
c       after doing whatever operation is called for.
c    data	The data read from the visibility file.
c    flags	The flags read from the visibility file.
c------------------------------------------------------------------------
	integer i,ant1,ant2,pol,p,q,PolRL,PolLR,PolLL,PolRR
	parameter(PolRR=-1,PolLL=-2,PolRL=-3,PolLR=-4)
	real temp,rms2
c
c process imaginary part of the model. (multiply (0+j)*(real_fft+j*imag_fft)
c
      if(doimag)then
        do i=1,nchan
          real_fft = buffer(3,i)
          imag_fft = buffer(4,i)
          buffer(3,i) = -imag_fft
          buffer(4,i) =  real_fft
        enddo
      endif
c
c  Do a replace operation.
c
	if(oper.eq.'replace')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i) = cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Add operation.
c
	else if(oper.eq.'add')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i)=data(i)+cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Subtract operation.
c
	else if(oper.eq.'subtract')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i)=data(i)-cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Multiply operation.
c
	else if(oper.eq.'multiply')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i)=data(i)*cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Divide operation.
c
	else if(oper.eq.'divide')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    if(flags(i))data(i)=data(i)/cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Flag operation.
c
	else if(oper.eq.'flag')then
	  rms2 = 2*rms*rms
	  do i=1,nchan
	    temp = (real (data(i))-buffer(3,i))**2 +
     *		   (aimag(data(i))-buffer(4,i))**2
	    flags(i) = flags(i).and.temp.lt.rms2
	  enddo
c
c  Subtract instrumental polarization.
c
	else if(oper.eq.'polcal')then
	  call uvrdvri(tvis,'pol',pol,0)
	  if(pol.eq.PolRL.or.pol.eq.PolLR)then
	    call BasAnt(preamble,ant1,ant2)
	    p = pol + 5
	    q = mod(p,2) + 1
	    do i=1,nchan
	      flags(i) = buffer(5,i).gt.0
	      data(i)=data(i)-cmplx(buffer(3,i),buffer(4,i))
     *		* (polcor(q,ant1)+conjg(polcor(p,ant2)))
	    enddo
	  endif
c
c  Simulate instrumental polarization.
c
	else if(oper.eq.'poleak')then
	  call uvrdvri(tvis,'pol',pol,0)
	  if(pol.eq.PolRL.or.pol.eq.PolLR)then
	    call BasAnt(preamble,ant1,ant2)
	    p = pol + 5
	    q = mod(p,2) + 1
	    do i=1,nchan
	      flags(i) = buffer(5,i).gt.0
	      data(i) = cmplx(buffer(3,i),buffer(4,i))
     *		* (polcor(q,ant1)+conjg(polcor(p,ant2)))
	    enddo
	    else
	    if(pol.eq.PolRR.or.pol.eq.PolLL)then
	    do i=1,nchan
	      flags(i) = buffer(5,i).gt.0
              data(i) = cmplx(buffer(3,i),buffer(4,i))
	    enddo
	    endif
	  endif
	else
	  call bug('f','Unrecognised operation, in Process')
	endif
	end
c************************************************************************
	subroutine GetOpt(oper,unflag,autoscal,apriori,imhead,mfs,doPol,
     *	  selradec,zero,doimag)
c
	implicit none
	character oper*(*)
	logical unflag,autoscal,apriori,imhead,mfs,doPol,selradec,zero
	logical doimag
c
c  Get the various processing options.
c
c  Output:
c    oper	One of 'add','subtract','multiply','divide','replace','flag',
c		'polcal', or 'poleak'. The default is 'add'.
c    unflag	Determine whether we are to unflag the data.
c    autoscal	Determine whether we are to autoscale the model.
c    apriori	Determine whether it is a a priori model, given by the
c		flux table, and data file planet info.
c    imhead	Copy image header items to uvvariables.
c    selradec	Input uv file contains multiple pointings or multiple
c		sources.
c    zero       Use zero if the model cannot be calculated (instead of 
c               flagging the data)
c    doimag  process imaginary part of the model.
c------------------------------------------------------------------------
	integer i,j
	integer nopt
	parameter(nopt=17)
	character opts(nopt)*9
	logical present(nopt)
	data opts/    'add      ','divide   ','flag     ','multiply ',
     *	  'replace  ','subtract ','polcal   ','poleak   ',
     *    'autoscale','unflag   ','apriori  ',
     *	  'imhead   ','mfs      ','polarized','selradec ','zero     ',
     *	  'imag     '/
	call options('options',opts,present,nopt)
c
	j = 0
	do i=1,8
	  if(present(i))then
	    if(j.ne.0)call bug('f',
     *	        'Options '//opts(j)//' and '//opts(i)//
     *		' are mutually exclusive')
	    j = i
	  endif
	enddo
	if(j.eq.0) call bug ('f', 'You must specify an option')
	oper = opts(j)
c	
	autoscal = present(9)
	unflag   = present(10)
	apriori  = present(11)
	imhead   = present(12)
	mfs      = present(13)
	doPol    = present(14)
	selradec = present(15)
	zero     = present(16)
	doimag   = present(17)
	if(oper.eq.'flag'.and.unflag)
     *	  call bug('f','You cannot use options=flag,unflag')
	if(selradec.and.imhead)then
	  call bug('w','Giving options=selradec,imhead together?')
	  call bug('w','Do you know what you are doing?')
	endif
	end
c************************************************************************
	subroutine ImHedIni(tIn,vcopy)
c
	implicit none
	integer tIn,vcopy
c
c  Initialise the copying of variables which change when the imhead option
c  is used.
c
c  Input:
c    tIn	Handle of the input visibility file.
c  Output:
c    vcopy	Handle of the uv variables to be copied across.
c------------------------------------------------------------------------
	integer i
c
	integer nvar
	parameter(nvar=49)
	character var(nvar)*8
c
c  Variables to copy whenever they change.
c
	data var/     'airtemp ','antdiam ','antpos  ','atten   ',
     *	   'axisrms ','chi     ','corbit  ','corbw   ','corfin  ',
     *	   'cormode ','coropt  ','cortaper','dewpoint','evector ',
     *	   'focus   ','freq    ','freqif  ','inttime ','ivalued ',
     *	   'jyperk  ','latitud ','longitu ','lo1     ','lo2     ',
     *	   'lst     ','mount   ','nants   ','ntemp   ','ntpower ',
     *	   'observer','on      ','operator','phaselo1','phaselo2',
     *	   'phasem1 ','plangle ','plmaj   ','plmin   ','pltb    ',
     *	   'precipmm','relhumid','temp    ','tpower  ','ut      ',
     *	   'veltype ','version ','winddir ','windmph ','xyphase '/
c------------------------------------------------------------------------
c
c  Copy the variables that have changed.
c
	call uvvarini(tIn,vcopy)
	do i=1,nvar
	  call uvvarset(vcopy,var(i))
	enddo
	end
c************************************************************************
	subroutine ImHed1st(tmod,tout,nchan)
c
	implicit none
	integer tmod,tout,nchan
c
c  Copy image header items to output uvvariables.
c
c  Input:
c    tmod	Handle of the input model.
c    tout	Handle of the output file.
c    nchan	Number of channels.
c------------------------------------------------------------------------
	double precision ckms
	parameter(ckms=299792.458d0)
	double precision restfreq,sdf,sfreq,ddata,crval,cdelt
	real vobs,crpix,data,vstart
	character*9 ctype,ascii
c
c  Get coordinates from the input model.
c
	call rdhda(tmod,'ctype1',ctype,' ')
	if(ctype(1:2).ne.'RA') call bug('f','axis 1 must be RA')
	call rdhdd(tmod,'crval1',ddata,0.d0)
	call uvputvrd(tout,'ra',ddata,1)
	call rdhdd(tmod,'obsra',ddata,ddata)
	call uvputvrd(tout,'obsra',ddata,1)
c
	call rdhda(tmod,'ctype2',ctype,' ')
	if(ctype(1:3).ne.'DEC') call bug('f','axis 2 must be DEC')
	call rdhdd(tmod,'crval2',ddata,0.d0)
	call uvputvrd(tout,'dec',ddata,1)
	call rdhdd(tmod,'obsdec',ddata,ddata)
	call uvputvrd(tout,'obsdec',ddata,1)
c
c  Get info from the input model to compute sfreq and sdf.
c
	call rdhda(tmod,'ctype3',ctype,' ')
	if(ctype(1:4).ne.'VELO' .and. ctype(1:4).ne.'FELO' .and.
     *						ctype(1:4).ne.'FREQ')
     *        call bug('f','axis must be VELO, FELO, or FREQ.')
	call rdhdd(tmod,'cdelt3',cdelt,0.d0)
	if(cdelt.eq. 0.) call bug('f','cdelt is 0 or not present.')
	call rdhdd(tmod,'restfreq',restfreq,0.d0)
	call rdhdr(tmod,'vobs',vobs,0.)
	call rdhdd(tmod,'crval3',crval,0.d0)
	call rdhdr(tmod,'crpix3',crpix,1.)
	if(ctype(1:4).eq.'FREQ')then
	  sfreq = crval + cdelt*(1-crpix)
	  sdf = cdelt
	else
	  if(restfreq.eq.0)call bug('f','Restfreq missing')
	  vstart = crval + cdelt*(1-crpix)
	  sfreq = restfreq*(1.d0-dble(vstart+vobs)/ckms)
	  sdf = -restfreq*cdelt/ckms
	endif
c
c  Write info to the output file.
c
	call uvputvri(tout,'nspect',1,1)
	call uvputvri(tout,'ischan',1,1)
	call uvputvri(tout,'nschan',nchan,1)
	call uvputvrd(tout,'restfreq',restfreq,1)
	call uvputvrr(tout,'vsource',0.,1)
	call uvputvrr(tout,'veldop',vobs,1)
	call uvputvrd(tout,'sdf',sdf,1)
	call uvputvrd(tout,'sfreq',sfreq,1)
c
c  Copy miscelaneous stuff across.
c
	call rdhdr(tmod,'epoch',data,0.)
	call uvputvrr(tout,'epoch',data,1)
c
	call rdhda(tmod,'telescop',ascii,' ')
	if(ascii.ne.' ')call uvputvra(tout,'telescop',ascii)
c
	call rdhda(tmod,'object',ascii,' ')
	call uvputvra(tout,'source',ascii)
c
c	call rdhdr(tmod,'pbfwhm',data,0.)
c	call uvputvrr(tout,'pbfwhm',data,1)
c
	end
