c***********************************************************************
      program invert
c
c= invert - Transform multi-pointing visibility data into a map
c& rjs
c: map making
c+
c       INVERT is a MIRIAD task that forms images from visibilities.
c       INVERT can form continuum images or spectral line cubes.  It can
c       generate images/cubes for several polarisations, as well as
c       handling multi-frequency synthesis and mosaicing observations.
c       INVERT can also form complex-valued images from non-Hermitian
c       data (e.g. holography data).  Appropriate point-spread functions
c       can also be generated.
c@ vis
c       Input visibility data files.  Several files can be given.
c       No default.
c@ map
c       Output map (image) file name.  Each output file consists of a
c       single polarization/Stokes parameter.  If several different
c       pols/Stokes images are being made, then several file names
c       should be given.  No default.
c@ beam
c       Output beam (point-spread function) file name.  The default is
c       not to make a beam.
c@ imsize
c       The size of the output dataset.  The default is to image out to
c       primary beam half power points. Add 'beam' as the third value
c       to specify the image size in primary beam FWHMs (e.g., 
c       imsize=2,2,beam).
c     . For options=mosaic, an image of this size is made for each 
c       pointing before a linear mosaic operation is performed.
c@ cell
c       Image cell size, in arcsec.  If two values are given, they give
c       the RA and DEC cell sizes.  If only one value is given, the
c       cells are made square.  The default is about one third of the 
c       resolution of the resultant images. Add 'res' as the third value,
c       to specify the the cellsize in terms of the number of pixels per 
c       resolution element (e.g., cell=5,5,res)
c@ offset
c       When not mosaicing, this gives the sky position to shift to the
c       center of the output images.  The position is specified as an
c       offset (in arcsec) from the observing center.  The default is to
c       perform no shifting.
c
c       When mosaicing, this gives the sky coordinate (RA and DEC) of
c       the reference pixel in the imaging process.  The value can be
c       given in the form hh:mm:ss,dd:mm:ss, or as decimal hours and
c       degrees.  INVERT applies appropriate shifts to make this
c       location fall on a pixel.  The default is a central observing
c       center.
c@ fwhm
c       This determines a gaussian taper to apply to the visibility
c       data.  It specifies the FWHM of an image-domain gaussian --
c       tapering the visibility data is equivalent to convolving with
c       this image-domain gaussian.
c
c       Either one or two values can be given, in arcsec, being the FWHM
c       in the RA and DEC directions.  If only one value is given, the
c       taper is assumed to be symmetric.  The default is no taper.
c
c       The signal-to-noise ratio will be optimised in the output image
c       if this parameter is set to the FWHM of typical image features
c       of interest.
c
c       If you are more accustomed to giving this parameter in the uv
c       plane (as AIPS requires), then:
c         fwhm(image plane) = 182 / fwhm(uv plane)
c       where the image plane fwhm is measured in arcseconds, and the uv
c       plane fwhm is measured in kilowavelengths.
c@ sup
c       Sidelobe suppression area, given in arcseconds.  This parameter
c       gives the area around a source where INVERT attempts to suppress
c       sidelobes.  Two values (for the RA and DEC directions
c       respectively) can be given.  If only one value is given, the
c       suppression area is made square.  The default is to suppress
c       sidelobes in an area as large as the field being mapped.
c
c       Note that uniform weighting can produce images with spuriously
c       high noise, especially for mfs imaging. It is recommended
c       to use robust~0.5 if sup is non zero or unset.
c
c       The suppression area is essentially an alternate way of
c       specifying the weighting scheme being used.  Suppressing
c       sidelobes in the entire field corresponds to uniform weighting
c       (so the default corresponds to uniform weighting).  Natural
c       weighting gives the best signal to noise ratio, at the expense
c       of no sidelobe suppression.  Natural weighting corresponds to
c       SUP=0.  Values between these extremes give a tradeoff between
c       signal to noise and sidelobe suppression, and roughly correspond
c       to AIPS "super-uniform" weighting. [A better way to move between
c       these extremes is to leave sup unset and vary the robust 
c       parameter from -2 to 2.]
c@ robust
c       Brigg's visibility weighting robustness parameter.  This
c       parameter can be used to down-weight excessive weight being
c       given to visibilities in relatively sparsely filled regions of
c       the $u-v$ plane when using uniform weighting.  Most useful 
c       settings are in the range [-2,2], with values less than -2 
c       corresponding to very little down-weighting, and values greater 
c       than +2 reducing the weighting to natural weighting.
c
c       Sidelobe levels and beam-shape degrade with increasing values of
c       robustness, but the theoretical noise level will also decrease.
c
c       The default is no down-weighting (robust=-infinity).
c@ line
c       Standard "line" parameter, with the normal defaults.  In
c       particular, the default is to image all channels.  See the help
c       on "line" for more information.
c       The "line" parameter consists of a string followed by up to
c       four numbers, viz:
c
c         linetype,nchan,start,width,step
c
c       where 'linetype' is one of "channel", "wide", "velocity" or
c       "felocity".
c@ ref
c       Line type of the reference channel, specified in a similar to
c       the "line" parameter.  Specifically, it is in the form:
c         linetype,start,width
c       Before mapping, the visibility data are divided by the reference
c       channel.  The default is no reference channel.
c@ select
c       This allows a subset of the uv data to be used in the mapping
c       process.  See the Users Manual for information on how to specify
c       this parameter.  The default is to use all data.
c@ stokes
c       Standard polarisation/Stokes parameter selection.  See the help
c       on "stokes" for more information.  Several polarisations can be
c       given.  The default is "ii" (i.e. Stokes-I, given the assumption
c       that the source is unpolarised).
c@ options
c       This gives extra processing options.  Several options can be
c       given (abbreviated to uniqueness), and separated by commas:
c         nocal     Do not apply gains table calibration to the data.
c         nopol     Do not apply polarisation leakage corrections.
c         nopass    Do not apply bandpass table calibration to the data.
c         double    Normally INVERT makes the beam patterns the same
c                   size as the output image.  This option causes the
c                   beam patterns to be twice as large.
c         systemp   Weight each visibility in inverse proportion to the
c                   noise variance.  Normally visibilities are weighted
c                   in proportion to integration time.  Weighting based
c                   on the noise variance optimises the signal-to-noise
c                   ratio (provided the measures of the system
c                   temperature are reliable!).
c         fsystemp  Like systemp, but use frequency dependent Tsys.
c                   You need to run atrecal before invert to create the
c                   systempf variable containing the Tsys spectrum.
c                   Atrecal requires autocorrelations to be present.
c                   This option only works in combination with the 
c                   mfs option.
c         mfs       Perform multi-frequency synthesis.  The causes all
c                   the channel data to be used in forming a single map.
c                   The frequency dependence of the uv coordinate is
c                   thus used to give better uv coverage and/or avoid
c                   frequency smearing.  For this option to produce
c                   useful maps, the intensity change over the frequency
c                   band must be small.  Set the 'line' parameter to
c                   select the channels that you wish to grid.
c         sdb       Generate the spectral dirty beam as well as the
c                   normal beam, when MFS processing.  The default is
c                   only to create the normal beam.  If the spectral
c                   dirty beam is created, this is saved as an extra
c                   plane in the beam dataset.
c         mosaic    Process multiple pointings, and generate a linear
c                   mosaic of these pointings. For single pointings
c                   to be combined with linmos you can use this to 
c                   specify a common reference position with the
c                   offset parameter. Observations using OTF mosaicing
c                   always need to specify this to ensure the moving
c                   beam is handled properly.
c         imaginary Make imaginary image for non-Hermitian data
c                   (holography).
c         amplitude Produce a image using the data amplitudes only.  The
c                   phases of the data are set to zero.
c         phase     Produce an image using the data phase only.  The
c                   amplitudes of the data are set to 1.
c         sin       Label the output map and beam as a SIN projection.
c                   Default is NCP unless non-east-west baselines are
c                   present or the field centre is within 3 deg of the
c                   celestial equator (because NCP blows up near the
c                   equator).  Note that this option simply changes
c                   ctype1 and ctype2 in the header, the translation
c                   only being correct to first order about the field
c                   centre.  A similar result could be obtained by
c                   running 'puthd' on the output map, e.g.
c                     puthd in=<map>/ctype1 value=RA---SIN
c                     puthd in=<map>/ctype2 value=DEC--SIN
c                   and likewise for the beam
c         ncp       Force invert to use the NCP projection even when
c                   significant non E-W baselines are present. 
c                   Use with care..
c@ mode
c       This determines the algorithm to be used in imaging.
c       Possible values are:
c         fft    The conventional grid-and-FFT approach.  This is the
c                default and by far the fastest.
c         dft    Use a discrete Fourier transform.  This avoids aliasing
c                but at a hugh time penalty.
c         median This uses a median approach.  This is generally robust
c                to bad data and sidelobes, has a even larger time
c                penalty and produces images that cannot be deconvolved.
c       NOTE: Dft and median modes are not supported with
c       options=mosaic.
c@ slop
c       NOTE: This parameter should be used with caution!  See the Users
c       Guide for more information on its applicability.
c
c       When forming spectral cubes, INVERT normally insists that all
c       channels in a given visibility spectrum must be good before
c       accepting the spectrum for imaging.  This keyword allows this
c       rule to be relaxed.  It consists of two parts: a tolerance and
c       a method for replacing the bad channels.
c
c       The tolerance is a value between 0 and 1, giving the fraction of
c       channels that INVERT will tolerate as being bad before the
c       spectrum is totally discarded.  The default is 0, indicating
c       that INVERT will not tolerate any bad channels.  A value of 1
c       indicates that INVERT will accept a spectrum as long as there is
c       at least one good channel.
c
c       The replacement method is either the value `zero' or
c       'interpolate', indicating that the bad channels are either to be
c       replaced with 0, or to be estimated by linear interpolation of
c       two adjacent good channels.  See the Users Guide for the merits
c       and evils of the two approaches.  The default is 'zero'.
c
c$Id$
c--
c  History
c    rjs        89  Initial version
c    nebk  29apr89  Added option to shift map centre from phase centre.
c    nebk  31may89  Change IMSIZE to two dimensions and
c                   put third dimension of image into LINE.
c    rjs   27jun89  Protected the case of cdelt3 being left as zero.
c    nebk  27jul89  Fixed bug which confused fwhmx and fwhmy.
c    rjs   16aug89  Improved some formatting.
c    rjs   18oct89  Support of uv selection.  Added needed changes to
c                   support the new calling sequence for planet
c                   rotation/scaling.
c    rjs    7nov89  Increased line buffer size in HISTORY.
c    rjs   13nov89  Multiple input files, systemp weighting, better
c                   history comments.
c    rjs   14feb90  Handle multi-pointing files.  More checking that
c                   source, channel velocity, ra and dec remain
c                   constant.  Corrected pi!  Replaced velocalc with
c                   uvfit.  More statistics.
c    rjs   22feb90  Corrected bug which did not discard out of range
c                   visibilities. Cosmetic changes.
c    rjs   23mar90  Changes to support applying calibration on the fly.
c                   Version number.
c    rjs   28mar90  Changed the default of JyperK.
c    rjs   29mar90  Corrected spelling mistake "sytemp" in the options
c                   list.
c    pjt    2may90  included maxdim.h in getvis for maxchan
c    mchw  21may90  Better error messages.
c    mchw  10Jul90  Worked on documentation.
c    rjs   16oct90  Checks that the data is cross-correlation data.
c    mchw  09Nov90  Added pbfwhm to map header.
c    mchw  20dec90  Added theoretical rms noise to history.
c                   Minor docs and dots in AppWts.
c    rjs   10jan91  An extra check for zero visibilities to map.
c    rjs    5feb91  New call sequence to the uvdat routines. Able to
c                   map one stokes parameter. "sloppy" option.
c    rjs   15mar91  Grid beam the same time as the maps. Multiple
c                   pols/stokes at a time. Bandwidth synthesis.
c    rjs   18apr91  Fixed documentation comment. Changed a common block.
c                   Corrected call sequence of hclose.
c    rjs   23apr91  Reverted default line-type to 1 channel only.
c    rjs    1may91  Doc improvements (??) and an extra user message.
c    rjs   11jun91  More doc improvements (??) and extra user messages.
c    rjs   28jun91  Flag to perform polarisation leakage correction.
c    rjs    3jul91  Changes to checking for data in files, to appease
c                   Lauren.  Changed crval1,crval2 to double to appease
c                   pjt.
c    rjs   28aug91  An extra check of the input parameters.
c    nebk  30aug91  Improve documentation for options=mfs
c    rjs   12sep91  Slight change for systemp weighting.
c    rjs   19sep91  Check if the output files already exit.
c    rjs   18feb92  Documentation enhancement to appease lgm.
c    rjs   18mar92  Better memory allocation.
c    rjs    3apr92  Calls to MemBuf.
c    nebk  05may92  Tell user when finished
c    rjs   26may92  Write btype keyword. Change spectral index sign
c                   convention for mfs beams.
c    mchw  09jun92  Check RA & DEC change lt 1% of cdelt's.  Improve doc
c                   shift.
c    rjs   11jun92  More doc changes.
c    rjs   25jun92  Single channel gets labelled with frequency.
c    rjs    1jul92  Doc changes only.
c    rjs   27jul92  Fiddles with the velocity/frequency labelling, and
c                   on where the linetype is retrieved from.
c    mchw  14aug92  Changed systemp weighting to include JyperK.
c    rjs   26aug92  Add nopass option.
c    rjs   29aug92  Add "slow" and "vslow" options.
c    rjs   25sep92  Recalculate the bandwidth often. Better description
c                   of systemp weighting.
c    mchw  11feb93  Read uvvariables ra, dec as double precision.
c    rjs   29mar93  Use uvinfo(...,'variance'...) to get rms.  Fix
c                   erroneous calls to uvrdvrd.
c    rjs   29jun93  Tell user whats going wrong when uvinfo fails to
c                   determine variance, when systemp weighting used.
c    mchw  39jun93  Option to make imaginary image for non Hermitian
c                   data.
c    rjs    1jul93  Doc changes and merge of mchw/rjs versions.
c    rjs   21jul93  Get rid of calls to uvinfo(..,'frequency'...).
c                   Better error messages (suggested by Lauren Likkel).
c                   Noise fiddle (use uvDatGtr(..'variance'..).
c    rjs   23jul93  Only write pbfwhm parameter if its valid.
c    rjs   24aug93  Change "shift" to "offset", to be consistent.
c    rjs   31aug93  vsloppy option.
c    rjs   24sep93  Long time bug dealing with insufficient space in the
c                   weight array under certain conditions.
c    rjs    8oct93  Increase buffer size.
c    rjs   15nov93  Image sizes do not need to be powers of 2.  Double
c                   option.
c    rjs   13jan93  Use double precision to avoid roundoff error, when
c                   beam scale factor.
c    rjs    9aug94  Remember if its an E-W array. Also minor change to
c                   usage of dra, to bring it into line with whats
c                   written in the uv var "bible".
c    rjs   11aug94  Better scaling for sloppy and vsloppy options.  Also
c                   describe this in the help.
c    rjs   17aug94  Slightly better determination of the offset.
c    rjs   16sep94  Doc only.
c    rjs   16sep94  Started rewrite -- to support mosaicing, and to
c                   generally tidy it up.
c    rjs   27oct94  First released version. with mosaicing.
c    rjs   18nov94  Eliminate rounding error problem in calculating
c                   freq0.  Better messages for natural weighting.  ref
c                   linetype.
c    rjs   28nov94  Determine the weights in a pointing-dependent
c                   manner.
c    rjs    3dec94  Some changes to make it work nicerer with single
c                   pointing mosaics.
c    rjs   19dec94  Restore amplitude and phase options.
c    rjs    4feb95  Changed subroutine name only.
c    rjs   18aug95  Check a beam is being made with options=sdb.  Beam
c                   axis order for options=sdb,mfs,mosaic for npnt=1.
c                   Interpolate mode with the slop factor.
c                   Robust parameter.
c    nebk  29sep95  Fix typo for irked user; "unform -> uniform"
c    rjs   30oct95  slop=xxx,interp was not workin g as advertised.
c    rjs    1nov95  Default value for imsize.  Better default cell and
c                   sup.  Sub-uniform weighting.
c    rjs   12nov95  Check imsize somewhat better.
c    rjs   13dec95  Eliminate min image size.
c    rjs   29feb96  Call xyflush after each plane.
c    rjs   12jul96  Before forgiving if beam too big -- just make it
c                   smaller.
c    rjs   20jun97  Correct handling of multiple stokes in slopintp.
c    rjs   07jul97  Change coaxdesc to coaxget.
c    rjs   01jul99  CHanges in call sequence to hdfiddle.
c    rjs   29jun05  Handle changes in calling sequence to mostab/hdtab
c                   routines.
c    rjs   03apr09  Change way of accessing scrio to help access larger
c                   files.
c    mhw   07nov11  Add warning for uniform weighting and mfs
c    mhw   17jan12  Handle larger files by using ptrdiff type more
c    mhw   06mar12  Add fsystemp option
c    mhw   03jun13  Add beam and res options to imsize and cellsize
c    mhw   03mar14  Fix bug in theoretical rms for large datasets
c    pjt   10feb15  Handle large mosaics 
c  Bugs:
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'
      include 'mem.h'
c
      integer MAXPOL,MAXRUNS
      parameter(MAXPOL=4,MAXRUNS=4*MAXDIM)
c
      real cellx,celly,fwhmx,fwhmy,freq0,slop,supx,supy,ppbx,ppby
      real umax,vmax,wdu,wdv,tu,tv,rms,robust
      real ChanWt(MAXPOL*MAXCHAN)
      character maps(MAXPOL)*64,beam*64,uvflags*16,mode*16,vis*64
      character line*64, version*72,smnx*10,smny*10
      double precision ra0,dec0,offset(2),lmn(3),x(2)
      integer i,j,k,nmap,tscr,nvis,nchan,npol,npnt,coObj,pols(MAXPOL)
      integer nx,ny,bnx,bny,mnx,mny,wnu,wnv
      integer nbeam,nsave,ndiscard,offcorr,nout
      logical defWt,Natural,doset,systemp(2),mfs,doimag,mosaic,sdb,idb
      logical double,doamp,dophase,dosin,doncp,dobeam,dores
c
      integer tno,tvis
      integer nMMap
      ptrdiff UWts,Map,MMap,nUWts
c
      integer nRuns,Runs(3,MAXRUNS)
c
      integer NSLOP
      parameter(NSLOP=2)
      character slops(NSLOP)*12,slopmode*12,resstr*8,beamstr*8
c
c  Externals.
c
      logical keyprsnt
      integer nextpow2,len1
      character itoaf*10, polsc2p*3, versan*72
c
      data slops/'zero        ','interpolate '/
c-----------------------------------------------------------------------
      version = versan ('invert',
     :                  '$Revision$',
     :                  '$Date$')
c
c  Get the input parameters. Convert all angular things into
c  radians as soon as possible!!
c
      call keyini
      call keya('beam',beam,' ')
      call mkeya('map',maps,MAXPOL,nmap)
      if(nmap.eq.0)call bug('f','An output must be given')

      call GetOpt(uvflags,double,systemp,mfs,sdb,mosaic,doimag,
     *        doamp,dophase,dosin,doncp,mode)
      idb = beam.ne.' '.and.doimag
      sdb = beam.ne.' '.and.sdb
      call uvDatInp('vis',uvflags)
c
      doset = keyprsnt('offset')
      if(mosaic)then
        call keyt('offset',offset(1),'hms',0.d0)
        call keyt('offset',offset(2),'dms',0.d0)
      else
        call keyd('offset',offset(1),0.d0)
        call keyd('offset',offset(2),0.d0)
        offset(1) = offset(1) * dpi/180/3600
        offset(2) = offset(2) * dpi/180/3600
      endif
c
      call keyr('cell',cellx,0.)
      call keyr('cell',celly,cellx)
      call keya('cell',resstr,' ')
      dores = resstr(1:1).eq.'r'
      if (dores) then
        if (cellx.lt.2.5.or.celly.lt.2.5) then
          call bug('w','Adjusting cellsize to avoid undersampling')
          cellx=max(2.5,cellx)
          celly=max(2.5,celly)
        endif
      else
        cellx = abs(cellx * pi/180/3600)
        celly = abs(celly * pi/180/3600)
      endif
      call keyr('fwhm',fwhmx,0.)
      call keyr('fwhm',fwhmy,fwhmx)
      fwhmx = fwhmx * pi/180/3600
      fwhmy = fwhmy * pi/180/3600
c
      call keyi('imsize',nx,0)
      call keyi('imsize',ny,nx)
      call keya('imsize',beamstr,' ')
      dobeam = beamstr(1:1).eq.'b'
      if(max(nx,ny).gt.MAXDIM)call bug('f','Output image too big')
c
      defWt = .not.keyprsnt('sup')
      call keyr('sup',supx,0.)
      call keyr('sup',supy,supx)
      supx = supx * pi/180/3600
      supy = supy * pi/180/3600
      if(min(supx,supy).lt.0)call bug('f','Invalid sup parameter')
      call keyr('robust',robust,-10.0)
      if(robust.gt.4.and.max(supx,supy).gt.0)then
        call bug('i','Robust value resulting in natural weights')
        supx = 0
        supy = 0
        defWt = .false.
      endif
      if (robust.eq.-10.and.(supx.gt.0.or.defWt)) then
        call bug('i',
     *  'Using uniform weighting with robust unset is not recommended') 
        if (mfs) call bug('i',' especially not for mfs data')
      endif
      call keyr('slop',slop,0.)
      if(slop.lt.0.or.slop.gt.1)call bug('f','Invalid slop value')
      call keymatch('slop',nslop,slops,1,slopmode,nout)
      if(nout.eq.0)slopmode = slops(1)
      call keyfin
c
c  Check the number of polarisations, and check that there is a
c  consistent number of output files. Also check that none of the output
c  files already exist.
c
      call uvDatGti('npol',npol)
      if(npol.eq.0)then
        npol = 1
        call uvDatSet('stokes',0)
      endif
      call uvDatGti('pols',pols)
      if(npol.ne.nmap)call bug('f',
     *  'Bad number of maps for the requested polarisations')
c
      if(beam.ne.' ')call assertf(beam,.false.,
     *    'Dataset already exists: '//beam)
      do i=1,npol
        call assertf(maps(i),.false.,
     *    'Dataset already exists: '//maps(i))
      enddo
c
c  Determine the max u and v values to map.
c
      if(cellx*celly.gt.0.and..not.dores)then
        umax = 0.5 / cellx
        vmax = 0.5 / celly
      else
        umax = 1e20
        vmax = 1e20
      endif
c
c  Load the visibility data.
c
      call output('Reading the visibility data ...')
      if(mosaic)call MosCIni
      call HdInit(mfs,mosaic)
      call scropen(tscr)
      call GetVis(doimag,systemp,mosaic,mfs,npol,tscr,slop,slopmode,
     *        vis,nvis,nchan,umax,vmax,ChanWt,MAXCHAN,freq0)
c
c  Set appropriate values for cellx and celly if needed. Try to make
c  the pixels square if the X and Y resolutions are approx the same.
c
      if (dores) then
        ppbx = cellx
        ppby = celly
      else
        ppbx = 3
        ppby = 3
      endif
      if(cellx*celly.le.0..or.dores)then
        cellx = max( 0.25 / umax, 0.3*fwhmx) *3/ppbx
        celly = max( 0.25 / vmax, 0.3*fwhmy) *3/ppby
        if(max(cellx,celly).lt.2*min(cellx,celly))then
          cellx = min(cellx,celly)
          celly = cellx
        endif
      endif
      cellx = -cellx
c
c  Give the "Hd" routines the header information, and create an initial
c  coordinate object for the output.
c
      if(mosaic)then
        call MosChar(ra0,dec0,npnt)
        if(doset)then
          ra0 = offset(1)
          dec0 = offset(2)
        endif
        call output('Number of pointings: '//itoaf(npnt))
      else
        npnt = 1
      endif
      if(npnt.ne.1.and.mode.ne.'fft')
     *  call bug('f','Only mode=fft is supported with options=mosaic')
      call HdSet(dosin,doncp,cellx,celly,ra0,dec0,freq0)
      call HdCoObj(coObj)
c
c  Determine the default image size, if needed.
c
      if(nx*ny.eq.0.or.dobeam)call HdDefSiz(nx,ny)
c
c  Fiddle the sizes and determine the size of the output beam.
c
      nx = min(nx,MAXDIM)
      ny = min(ny,MAXDIM)
      if(double)then
        if(2*max(nx,ny)-1.gt.MAXDIM)call bug('w',
     *    'Reducing beam size to be maximum image size')
        bnx = min(2*nx - 1,MAXDIM)
        bny = min(2*ny - 1,MAXDIM)
        if (nextpow2(bnx).gt.MAXDIM) bnx = nx
        if (nextpow2(bny).gt.MAXDIM) bny = ny
      else
        bnx = nx
        bny = ny
      endif
c
c  Tell about the mean frequency, if necessary.
c
      if(mfs)then
        write(line,'(a,1pg9.3)')'Mean Frequency(GHz):    ',freq0
        call output(line)
      endif
c
c  Do the geometry and shift calculations. At the end of this, coObj
c  fully describes the coordinate system of the output.
c
      if(mosaic)then
        call output('Doing the geometry calculations ...')
        call MosGinit(coObj,nx,ny,nchan,mnx,mny)
        lmn(1) = 0
        lmn(2) = 0
        lmn(3) = 1
        if(max(mnx,mny).gt.MAXDIM) then
           write(*,*) 'MAXDIM:',MAXDIM,mnx,mny
           call bug('f','Mosaiced image size is too big for MAXDIM')
        endif
      else
        mnx = nx
        mny = ny
        call coLMN(coObj,'ow/ow',offset,lmn)
        call coCvt(coObj,'ow/ow',offset,'op/op',x)
        call coSetd(coObj,'crpix1',dble(nx/2+1)-x(1))
        call coSetd(coObj,'crpix2',dble(ny/2+1)-x(2))
        call coReinit(coObj)
      endif
c
c  Determine some things for the weighting process, and then go
c  and determine the weights (if its not natural weighting).
c
      call WtIni(defWt,supx,supy,bnx,bny,cellx,celly,
     *  fwhmx,fwhmy,umax,vmax,Natural,wnu,wnv,wdu,wdv,tu,tv)
c
      if(.not.Natural)then
        call output('Calculating the weights ...')
        nUWts = (wnu/2+1) * wnv
        nUWts = nUWts * npnt
        write(*,*) 'PJT1',wnu,wnv,npnt,nUWts
        call Memallox(UWts,nUWts,'r')
        call WtCalc(tscr,memr(UWts),wdu,wdv,wnu,wnv,npnt,
     *                                        nvis,npol*nchan)
        if(robust.gt.-4)
     *    call WtRobust(robust,memr(UWts),wnu,wnv,npnt)
      else
         UWts = 1
         nUWts = 0
      endif
c
c  Apply the weights, shifts and geometric corrections, and then free
c  the weighting array.
c
      if(mosaic)then
        call output('Applying weights and geometry corrections ...')
      else
        call output('Applying the weights ...')
      endif
      call Wter(tscr,Natural,memr(UWts),wdu,wdv,wnu,wnv,npnt,Tu,Tv,
     *  nvis,npol,nchan,mosaic,idb,sdb,doamp,dophase,freq0,Rms,
     *  ChanWt,lmn,umax,vmax,cellx,celly)
c
      if(nUWts.gt.0)call MemFrex(UWts,nUWts,'r')
      if(mosaic)call mosGFin
c
c  Tell the user about the noise level in the output images.
c
      write(line,'(a,1pg10.3)')'Theoretical rms noise:',
     *                                Rms*sqrt(real(npnt))
      call output(line)
      if(npnt.gt.1)then
        call output(' ... assuming pointings do not overlap')
        Rms = 0
      endif
c
c  Reopen the first visibility dataset, to extract history from.
c
      call uvopen(tvis,vis,'old')
c
c  Determine some things about beams.
c    nbeam      The number of beams to be gridded.
c    nsave      The number to be saved.
c    ndiscard   The number to be discarded (after gridding).
c    offcorr    Offset, in each visibility record, of the first
c               correlation to grid.
c
      nbeam = 1
      nsave = 1
      if(sdb)then
        nsave = 2
        nbeam = 2
      else if(idb)then
        nsave = 1
        nbeam = 2
      else if(beam.eq.' ')then
        nsave = 0
      endif
      ndiscard = nbeam - nsave
      offcorr = 5 - nbeam
c
c  Initialise the mapper, and start mapping!
c
      call MapIni(mode,tscr,nvis,npnt,umax,vmax,
     *                        offcorr,nbeam+npol*nchan)
      call MapDef(nbeam,bnx,bny)
      call MapDef(npol*nchan,nx,ny)
c
c  Create space for the mosaiced image, if needed.
c
      if(mosaic)then
         smnx = itoaf(mnx)
         smny = itoaf(mny)
         call output('Mosaic image size: '//
     *       smnx(1:len1(smnx)) // ' x ' //
     *       smny(1:len1(smny)) )

         nMMap = mnx*mny
         call MemAllop(MMap,nMMap,'r')
      else
         nMMap = 0
      endif
c
c  Make the appropriate beams.
c
      if(nsave.gt.0)then
        call output('Forming the beam ...')
        call BeamMake(tno,beam,coObj,mosaic,sdb,bnx,bny,
     *  npnt,tvis,version)
      else
        call output('Determining normalisation factor ...')
      endif
c
      do i=1,nbeam
        if(i.eq.1)call MapScale(i)
        if(i.gt.ndiscard)then
          call Mapper(i,Map)
          call BmWrite(tno,i-ndiscard,memr(Map),bnx,bny,npnt,mosaic)
        endif
      enddo
      if(nsave.gt.0)call xyclose(tno)
c
c  Map all the images, mosaicing them when necessary.
c
      k = nbeam
      do j=1,npol
        call output('Forming Stokes '//polsc2p(pols(j))//'image ...')
        call coAxSet(coObj,4,'STOKES',1.d0,dble(pols(j)),1.d0)
        call ImMake(tno,maps(j),coObj,mosaic,mnx,mny,nchan,
     *    tvis,version,rms)
        do i=1,nchan
          k = k + 1
          call xysetpl(tno,1,i)
          call Mapper(k,Map)
          if(mosaic)then
            if(nchan.gt.1)then
              call output('Mosaicing plane '//itoaf(i))
            else
              call output('Mosaicing the image ...')
            endif
            call MosMIni(coObj,real(i))
            call Mosaicer(memr(Map),memr(MMap),nx,ny,npnt,mnx,mny,
     *          Runs,MAXRUNS,nRuns)
            call MosMFin
            call PutRuns(tno,Runs,nRuns,0,0,mnx,mny)
          else
            MMap = Map
          endif
          call DatWrite(tno,memr(MMap),mnx,mny)
        enddo
        call xyclose(tno)
      enddo
c
c  All said and done. Tidy up and exit.
c
      if(nMMap.gt.0)call MemFrep(MMap,nMMap,'r')
      call MapFin
      call uvclose(tvis)
      call scrclose(tscr)
      call output('Completed 100% !')
c
      end
c***********************************************************************
      subroutine BmWrite(tno,i,Dat,nx,ny,npnt,mosaic)
c
      integer tno,i,nx,ny,npnt
      logical mosaic
      real Dat(nx,ny,npnt)
c
c  Write out a beam dataset.
c-----------------------------------------------------------------------
      integer ndims,n(2),k
      logical mosaic1
c
      mosaic1 = mosaic.and.npnt.gt.1
      ndims = 0
      if(mosaic1.and.i.gt.1)then
        ndims = 2
      else if(mosaic1.or.i.gt.1)then
        ndims = 1
      endif
      if(ndims.gt.0) n(ndims) = i
c
      do k=1,npnt
        if(mosaic1)n(1) = k
        if(ndims.gt.0)call xysetpl(tno,ndims,n)
        call DatWrite(tno,Dat(1,1,k),nx,ny)
      enddo
c
      end
c***********************************************************************
      subroutine DatWrite(tno,Dat,nx,ny)
c
      integer tno,nx,ny
      real Dat(nx,ny)
c
c  The routine that does the real work in writing out the data.
c-----------------------------------------------------------------------
      integer j
c
      do j=1,ny
        call xywrite(tno,j,Dat(1,j))
      enddo
      call xyflush(tno)
      end
c***********************************************************************
      subroutine BeamMake(tno,beam,coIn,mosaic,sdb,bnx,bny,npnt,
     *  tvis,version)
c
      integer tno,bnx,bny,npnt,coIn,tvis
      character beam*(*),version*(*)
      logical mosaic,sdb
c
c  Create an output beam dataset.  It takes a bit of thinking to
c  determine the dimensionality of the output.
c-----------------------------------------------------------------------
      integer naxis,imsize(5),coOut
      double precision crpix,crval,cdelt
      character ctype*16
c
      call coDup(coIn,coOut)
      imsize(1) = bnx
      imsize(2) = bny
      naxis = 2
      call coSetd(coOut,'crpix1',dble(bnx/2+1))
      call coSetd(coOut,'crpix2',dble(bny/2+1))
      call coAxGet(coOut,3,ctype,crpix,crval,cdelt)
      if(mosaic.and.sdb.and.npnt.eq.1)then
        naxis = naxis + 1
        imsize(naxis) = 2
        call coAxSet(coOut,naxis,'SDBEAM',1.d0,0.d0,1.d0)
        naxis = naxis + 1
        imsize(naxis) = npnt
        call coAxSet(coOut,naxis,'POINTING',1.d0,1.d0,1.d0)
      else
        if(mosaic)then
          naxis = naxis + 1
          imsize(naxis) = npnt
          call coAxSet(coOut,naxis,'POINTING',1.d0,1.d0,1.d0)
        endif
        if(sdb)then
          naxis = naxis + 1
          imsize(naxis) = 2
          call coAxSet(coOut,naxis,'SDBEAM',1.d0,0.d0,1.d0)
        endif
      endif
      naxis = naxis + 1
      imsize(naxis) = 1
      call coAxSet(coOut,naxis,ctype,crpix,crval,cdelt)
      call coReinit(coOut)
c
      call xyopen(tno,beam,'new',naxis,imsize)
      call HdFiddle(tvis,tno,version,mosaic,coOut,'beam',0.0,bnx,bny)
      call coFin(coOut)
      end
c***********************************************************************
      subroutine ImMake(tno,map,coIn,mosaic,nx,ny,nchan,
     *  tvis,version,rms)
c
      integer tno,coIn,nx,ny,nchan,tvis
      real rms
      logical mosaic
      character map*(*),version*(*)
c
c  Create an output image dataset -- this is pretty easy!
c-----------------------------------------------------------------------
      integer imsize(4),naxis
c
      imsize(1) = nx
      imsize(2) = ny
      imsize(3) = nchan
      imsize(4) = 1
      naxis = 4
c
      call xyopen(tno,map,'new',naxis,imsize)
      call HdFiddle(tvis,tno,version,mosaic,coIn,'intensity',
     *                                                rms,nx,ny)
      end
c***********************************************************************
      subroutine HdFiddle(tvis,tno,version,mosaic,coIn,btype,
     *                                                rms,nx,ny)
c
      integer tvis,tno,coIn,nx,ny
      logical mosaic
      character version*(*),btype*(*)
      real rms
c
c  Make the header of the output dataset.
c-----------------------------------------------------------------------
      character line*64
c
c  Call the various routines which handle these sorts of things.
c
      call hdWrite(tno,rms,nx,ny)
      call coWrite(coIn,tno)
      call wrbtype(tno,btype)
      call wrhda(tno,'bunit','JY/BEAM')
c
c  Write the mosaic table, if needed.
c
      if(mosaic)call MosSave(tno)
c
c  Write the history file.
c
      call hdcopy(tvis,tno,'history')
      call hisOpen(tno,'append')
      line = 'INVERT: Miriad '//version
      call hisWrite(tno,line)
      call hisInput(tno,'INVERT')
      call hisClose(tno)
c
      call xyflush(tno)
c
      end
c***********************************************************************
      subroutine WtCalc(tvis,Wts,wdu,wdv,wnu,wnv,npnt,nvis,nchan)
c
      integer tvis,wnu,wnv,nvis,nchan,npnt
      real Wts(wnv,wnu/2+1,npnt),wdu,wdv
c
c  Calculate the weight to be applied to each visibility.
c
c  Input:
c    tvis       Handle of the visibility scratch file.
c    wnu,wnv    Full size of the weights array.
c    wdu,wdv    Cell increments (wavelengths).
c    nvis       Number of visibilities.
c    nchan      Number of channels.
c    npnt       Number of pointings.
c
c  Output:
c    Wts        Array containing the visibility weights.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer InU,InV,InW,InWt,InPnt,InRms2,InFreq,InData
      parameter(InU=0,InV=1,InW=2,InPnt=3,InWt=6,InRms2=4)
      parameter(InFreq=5,InData=8)
      integer Maxrun
      parameter(Maxrun=8*MAXCHAN+20)
      integer i,id,j,VispBuf, VisSize,u,v,k,ktot,l,ltot,ipnt
      real Visibs(Maxrun)
      ptrdiff offset
c
c  Determine the number of visibilities perr buffer.
c
      VisSize = InData + 2*nchan
      VispBuf = maxrun/VisSize
      if(VispBuf.lt.1)call bug('f','Too many channels for me!')
c
c  Zero out the array.
c
      do ipnt=1,npnt
        do j=1,wnu/2+1
          do i=1,wnv
            Wts(i,j,ipnt) = 0.
          enddo
        enddo
      enddo
c
c  Accumulate the weight function.
c
      call scrrecsz(tVis,VisSize)
      k = 0
      ktot = nvis
      dowhile(k.lt.ktot)
        ltot = min(VispBuf,ktot-k)
        offset = k
        call scrread(tvis,Visibs,offset,ltot)
        do l=1,ltot*VisSize,VisSize
          if(Visibs(l+InU).lt.0)then
            u = nint(-Visibs(l+InU)/wdu) + 1
            v = nint(-Visibs(l+InV)/wdv) + wnv/2 + 1
          else
            u = nint(Visibs(l+InU)/wdu) + 1
            v = nint(Visibs(l+InV)/wdv) + wnv/2 + 1
          endif
          ipnt = nint(Visibs(l+InPnt))
          Wts(v,u,ipnt) = Wts(v,u,ipnt) + Visibs(l+InWt)
        enddo
        k = k + ltot
      enddo
c
c  Correct the first row.
c
      do ipnt=1,npnt
        id = wnv
        do i=1,wnv/2+1
          Wts(i, 1,ipnt) = Wts(i,1,ipnt) + Wts(id,1,ipnt)
          Wts(id,1,ipnt) = Wts(i,1,ipnt)
          id = id - 1
        enddo
      enddo
c
      end
c***********************************************************************
      subroutine WtIni(defWt,supx,supy,nx,ny,cellx,celly,
     *  fwhmx,fwhmy,umax,vmax,Natural,wnu,wnv,wdu,wdv,tu,tv)
c
      logical defWt,Natural
      real supx,supy,cellx,celly,fwhmx,fwhmy,wdu,wdv,tu,tv,umax,vmax
      integer nx,ny,wnu,wnv
c
c  Determine some things about the weighting process, and tell the
c  user as much.
c
c  Input:
c    defWt      True if the default weighting scheme is to be used.
c    supx,supy  Sidelobe suppression region (radians).
c    nx,ny      Output image size.
c    cellx,celly Image cell size.
c    fwhmx,fwhmy Image-domain taper.
c    umax,vmax   Maximum baselines in u and v.
c  Output:
c    wnu,wnv    Size of the weights grid.
c    wdu,wdv    Weight grid cell size.
c    tu,tv      Taper parameters.
c    Natural    True if natural weighting is being used.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      integer n(2),length,i,nxd,nyd
      character line*64
      logical Uni
c
c  Externals.
c
      integer nextpow2
c
      nxd = nextpow2(nx)
      nyd = nextpow2(ny)
      call Sizes(defWt,supx,nxd,cellx,fwhmx,umax,wnu,wdu,tu)
      call Sizes(defWt,supy,nyd,celly,fwhmy,vmax,wnv,wdv,tv)
c
      Natural = wnu.le.2.and.wnv.le.2
      Uni     = abs(1-nxd*abs(cellx*wdu)).lt.0.01.and.
     *          abs(1-nyd*abs(celly*wdv)).lt.0.01
c
      n(1) = nint(3600*180/pi/wdu)
      n(2) = nint(3600*180/pi/wdv)
      call mitoaf(n,2,line,length)
      i = index(line(1:length),',')
      line(i:i) = 'x'
      call output('Sidelobe suppression area is '//line(1:length)//
     *        ' arcsec')
      if(Natural)then
        call output(' ... this corresponds to natural weighting')
      else if(Uni)then
        call output(' ... this corresponds to uniform weighting')
      else
        call output(' ... this corresponds to a super-uniform '//
     *                                        'weighting')
      endif
c
      end
c***********************************************************************
      subroutine Sizes(defWt,sup,gn,cell,fwhm,uvmax,wn,wd,T)
c
      real cell,sup,wd,fwhm,T,uvmax
      integer gn,wn
      logical defWt
c
c  Determine various cell and size parameters, which are independent of
c  whether we are dealing with the x or y axis.
c
c  Input:
c    defWt      True if we are to use the default weighting scheme.
c    cell       Image cell size (arcseconds).
c    uvmax
c    sup        Suppression region (arcseconds).
c    fwhm       Gausian taper fwhm (arcseconds).
c    gn         Image dimension (pixels).
c
c  Output:
c    wn         Dimension of weights array (pixels).
c    wd         Uv plane weights cell size (wavelengths).
c    T          Taper exponent parameter (nepers/wavelength**2).
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'
      real gd
c
c  Externals.
c
      character itoaf*10
c
      T = - Fwhm**2 * (pi**2 / (4.*log(2.)))
      gd = 1/abs(Cell * gn)
c
      if(Sup.gt.0)then
        wd = 1/sup
        wn = 2*nint( uvmax / wd ) + 1
      else if(defWt)then
        wd = gd
        wn = 2*nint( uvmax / wd ) + 1
      else
        wd = 1/abs(Cell)
        wn = 1
      endif
c
      if(gn.gt.maxdim)call bug('f',
     *  'Maximum permitted image size is '//itoaf(maxdim))
c
      end
c***********************************************************************
      subroutine WtRobust(robust,UWts,wnu,wnv,npnt)
c
      integer wnv,wnu,npnt
      real robust,UWts(wnv,wnu/2+1,npnt)
c
c  Use Brigg's scheme to make the weights robust.
c-----------------------------------------------------------------------
      integer i,j,k
      real SumW,SumW2,t,Wav,S2
c
c  Determine the mean weight.
c
      SumW = 0
      SumW2 = 0
      do k=1,npnt
        do j=1,wnu/2+1
          do i=1,wnv
            t = UWts(i,j,k)
            SumW = SumW + t
            SumW2 = SumW2 + t*t
          enddo
        enddo
      enddo
      if(SumW.eq.0)call bug('f','Something is screwy in WtRobust')
c
      Wav = SumW2 / SumW
      S2 = 12.5 * 10.0**(-2*robust)/Wav
c
      do k=1,npnt
        do j=1,wnu/2+1
          do i=1,wnv
            UWts(i,j,k) = 1 + UWts(i,j,k)*S2
          enddo
        enddo
      enddo
c
      end
c***********************************************************************
      subroutine Wter(tscr,Natural,UWts,wdu,wdv,wnu,wnv,npnt,Tu,Tv,
     *  nvis,npol,nchan,mosaic,idb,sdb,doamp,dophase,freq0,Rms2,
     *  Slop,lmn,umax,vmax,cellx,celly)
c
      integer tscr,wnu,wnv,nvis,npol,nchan,npnt
      logical Natural,sdb,idb,mosaic,doamp,dophase
      real Tu,Tv,wdu,wdv,UWts(wnv,wnu/2+1,npnt),cellx,celly
      real Rms2,freq0,umax,vmax,Slop(npol*nchan)
      double precision lmn(3)
c
c  Apply weights and calculate RMS noise for each pointing.
c
c  Input:
c    tscr       Scratch file of the visibility data.
c    Natural    True if natural weighting is to be used.
c    Tu,Tv      Scale factors for determining taper.
c    UWts       If its not natural weighting, this contains the
c               uniform weight information.
c    wnu,wnv    Weight array size.
c    npnt       Number of pointings.
c    wdu,wdv    Weight cell size.
c    nvis       Number of visibilities.
c    npol       Number of polarisations.
c    nchan      Number of channels.
c    mosaic     True if we are mosaicing.
c    sdb        True if a spectral dirty beam is to be created.
c    idb        True if an imaginary dirty beam is to be created.
c    freq0      Reference frequency, when calculating spectral dirty
c               beam.
c    slop       Slop scale factors.
c    lmn        Direction cosines of place to shift to.
c    doamp      True if amplitude-only imaging.
c    dophase    True if phase-only imaging.
c  Output:
c    Rms2       An estimate of the rms noise in the output map.
c    umax,vmax  Maximum u and v values.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer InU,InV,InW,InPnt,InRms,InFreq,InWt,InData
      parameter(InU=0,InV=1,InW=2,InPnt=3,InRms=4,InFreq=5,InWt=6,
     *          InData=8)
      integer maxrun
      parameter(maxrun=8*MAXCHAN+8)
c
      double precision SumWt,RMS2d
      real Wts(maxrun/(InData+2)),Vis(maxrun),logFreq0,Wt,t
      integer i,j,k,l,size,step,n,u,v,offcorr,nbeam,ncorr,ipnt
      logical doshift
      ptrdiff offset
c
c  Miscellaneous initialisation.
c
      RMS2d = 0
      SumWt = 0
      doshift = abs(lmn(1)) + abs(lmn(2)).gt.0
c
      umax = 0
      vmax = 0
      if(sdb) logFreq0 = log(Freq0)
c
      if(sdb.or.idb)then
        nbeam = 2
      else
        nbeam = 1
      endif
      offcorr = InData - 2*nbeam + 1
      ncorr = nbeam + npol*nchan
c
      size = 2*npol*nchan + InData
      step = maxrun/size
      if(step.le.0)
     *  call bug('f','Too many channels for buffer in AppWts')
c
c  Do the real work.
c
      call scrrecsz(tscr,size)
      do l=1,nvis,step
        n = min(nvis-l+1,step)
        offset = l-1
        call scrread(tscr,Vis,offset,n)
c
c  Calculate the basic weight, either natural or pseudo-uniform.
c
        if(Natural)then
          k = 1
          do i=1,n
            Wts(i) = Vis(k+InWt)
            Vis(k+InWt) = 1
            k = k + size
          enddo
        else
          k = 1
          do i=1,n
            if(Vis(k+InU).gt.0)then
              u = nint(Vis(k+InU)/wdu)         + 1
              v = nint(Vis(k+InV)/wdv) + wnv/2 + 1
            else
              u = nint(-Vis(k+InU)/wdu)         + 1
              v = nint(-Vis(k+InV)/wdv) + wnv/2 + 1
            endif
            ipnt = nint(Vis(k+InPnt))
            Wts(i) = Vis(k+InWt) / UWts(v,u,ipnt)
            Vis(k+InWt) = 1
            k = k + size
          enddo
        endif
c
c  Include a taper in the weights, if necessary.
c
        if(abs(Tu)+abs(Tv).gt.0)then
          k = 1
          do i=1,n
            Wts(i) = Wts(i) * exp( Tu*Vis(k+InU)*Vis(k+InU) +
     *                             Tv*Vis(k+InV)*Vis(k+InV))
            k = k + size
          enddo
        endif
c
c  Apply geometry and shift corrections.
c
        if(mosaic)then
          call MosGeom(size/2,n,nchan,npol,Vis,Wts)
        else if(doshift)then
          call WtShift(size/2,n,nchan*npol,Vis,lmn)
        endif
c
c  Normalise the uv coordinates, and determine the rms noise (for
c  a mosaicing observation, this is the mean over all fields).
c
        k = 1
        do i=1,n
          Vis(k+InU) = Vis(k+InU) * cellx
          Vis(k+InV) = Vis(k+InV) * celly
          umax = max( umax, abs(Vis(k+InU)) )
          vmax = max( vmax, abs(Vis(k+InV)) )
          SumWt = SumWt + Wts(i)
          RMS2d = RMS2d + Wts(i)*Wts(i)*Vis(k+InRms)
          k = k + Size
        enddo
c
c  Do amplitude and phase processing, if needed.
c
        if(doamp)then
          do i=1,n
            k = (i-1)*size + InData + 1
            do j=1,npol*nchan
              Vis(k) = sqrt(Vis(k)**2+Vis(k+1)**2)
              Vis(k+1) = 0
              k = k + 2
            enddo
          enddo
        endif
        if(dophase)then
          do i=1,n
            k = (i-1)*size + InData + 1
            do j=1,npol*nchan
              t = sqrt(Vis(k)**2+Vis(k+1)**2)
              if(t.gt.0)then
                Vis(k)   = Vis(k)/t
                Vis(k+1) = Vis(k+1)/t
              endif
              k = k + 2
            enddo
          enddo
        endif
c
c  Generate the "correlations" needed for the spectral dirty beam
c  and the imaginary dirty beam, if needed. Note that we reverse the
c  normal beam and the special beams.
c
        if(sdb)then
          k = 1
          do i=1,n
            Vis(k+InData-2) = Vis(k+InFreq) - logfreq0
            Vis(k+InData-1) = 0
            Vis(k+InData-4) = 1
            Vis(k+InData-3) = 0
            k = k + size
          enddo
        else if(idb)then
          k = 1
          do i=1,n
            Vis(k+InData-4) = 1
            Vis(k+InData-3) = 0
            Vis(k+InData-2) = 0
            Vis(k+InData-1) = 1
            k = k + size
          enddo
        endif
c
c  Apply the weights to the data.
c
        do j=1,ncorr
          k = 2*(j-1) + offcorr
          if(j.le.nbeam)then
            Wt = 1
          else
            Wt = Slop(j-nbeam)
          endif
          do i=1,n
            Vis(k)   = Wt*Wts(i)*Vis(k)
            Vis(k+1) = Wt*Wts(i)*Vis(k+1)
            k = k + size
          enddo
        enddo
c
c  All done. Write out the results.
c
        offset = l-1
        call scrwrite(tscr,Vis,offset,n)
      enddo
c
c  Finish up the RMS noise estimates.
c
      if(SumWt.gt.0)then
        RMS2 = sqrt(RMS2d / SumWt/SumWt )
      else
        RMS2 = 0
      endif
c
      end
c***********************************************************************
      subroutine WtShift(size,n,nchan,Vis,lmn)
c
      integer size,n,nchan
      complex Vis(size,n)
      double precision lmn(3)
c
c  Apply shift to the data.
c
c  Input:
c    size       Size of each visibility record in complex elements.
c    n          Number of visibility records.
c    nchan      Total number of channels (actually npol*nchan).
c    lmn        Direction cosines of place to shift to.
c  Input/Output:
c    Vis        The visibilities.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      integer InUV,InWPnt,InData
      parameter(InUV=1,InWPnt=2,InData=5)
c
      real theta,uu,vv,ww
      complex fac
      integer i,k
c
c  Consistency check.
c
      if(InData-1+nchan.ne.size)
     *        call bug('f','Inconsistent, in WtShift')
c
c Shift the data to a given lmn coordinate.
c
      do k=1,n
        uu = real (Vis(InUV,k))
        vv = aimag(Vis(InUV,k))
        ww = real (Vis(InWPnt,k))
        theta = -2*pi*(uu*lmn(1) + vv*lmn(2) + ww*(lmn(3)-1))
        fac = cmplx(cos(theta),sin(theta))
c
        do i=InData,InData+nchan-1
          Vis(i,k) = Vis(i,k) * fac
        enddo
      enddo
c
      end
c***********************************************************************
      subroutine GetOpt(uvflags,double,systemp,mfs,sdb,mosaic,doimag,
     *        doamp,dophase,dosin,doncp,mode)
c
      character uvflags*(*),mode*(*)
      logical systemp(2),mfs,sdb,doimag,mosaic,double,doamp,dophase,
     * dosin,doncp
c
c  Get extra processing options.
c
c-----------------------------------------------------------------------
      integer NOPTS, NMODES
      parameter (NOPTS=14, NMODES=3)

      integer nmode
      logical present(NOPTS)
      character modes(NMODES)*8, opts(NOPTS)*9

      data opts/'nocal    ','nopol    ','nopass   ','double   ',
     *          'systemp  ','mfs      ','sdb      ','mosaic   ',
     *          'imaginary','amplitude','phase    ','sin      ',
     *          'ncp      ','fsystemp '/
      data modes/'fft     ','dft     ','median  '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

c     Processing flags for the uvDat routines.
      uvflags = 'xwplds3r'
      if(.not.present(1))uvflags(9:9)   = 'c'
      if(.not.present(2))uvflags(10:10) = 'e'
      if(.not.present(3))uvflags(11:11) = 'f'

c     Extra processing options.
      double  = present(4)
      systemp(1) = present(5)
      mfs     = present(6)
      sdb     = present(7)
      mosaic  = present(8)
      doimag  = present(9)
      doamp   = present(10)
      dophase = present(11)
      dosin   = present(12)
      doncp   = present(13)
      systemp(2) = present(14).and.mfs

c     Check options.
      if(sdb.and..not.mfs)call bug('f',
     *  'Option=sdb not meaningful without options=mfs')

      if(doimag.and.sdb)call bug('f',
     *  'I cannot cope with options=imaginary,sdb simultaneously')
      if(systemp(1).and.systemp(2)) call bug('f',
     *  'Please choose only one of systemp and fsystemp')
      if(present(14).and..not.mfs) call bug('w',
     *  'The fsystemp option is ignored unless mfs is specified')
      if(present(12).and.present(13)) call bug('f',
     *  'Choose at most one of options sin and ncp')

c     Imaging algorithm.
      call keymatch('mode',NMODES,modes,1,mode,nmode)
      if(nmode.eq.0)mode = modes(1)

      end
c***********************************************************************
      subroutine GetVis(doimag,systemp,mosaic,mfs,npol,tscr,slop,
     *        slopmode,vis,nvis,nchan,umax,vmax,ChanWt,mchan,freq0)
c
      logical doimag,systemp(2),mosaic,mfs
      integer npol,tscr,nvis,nchan,mchan
      real umax,vmax,freq0,slop,ChanWt(npol*mchan)
      character vis*(*),slopmode*(*)
c
c  Get the data to be imaged.  Writes out a scratch file with records
c    u,v,w,pointing,rms**2,log(freq),wt,0,r1,i1,r2,i2,...
c
c
c  Input:
c    doimag     Make imaginary map.
c    systemp    Use weights proportional to 1/rms**2 (1:scalar,2:spectr)
c    mosaic     Accept multiple pointings.
c    mfs        Multi-frequency synthesis option.
c    slop       Slop factor.
c    slopmode   Slop mode.
c    npol       Number of polarisations.
c    mchan      Max number of channels to handle.
c    tscr       Handle of the scratch file.
c  Input/Output:
c    umax,vmax  Max u,v spacing.
c  Output:
c    nchan      Number of channels.
c    nvis       The number of visibilities read.
c    ChanWt     Extra weighting factor for each channel.
c    freq0      MFS reference frequency.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXPOL,MAXLEN
      parameter(MAXPOL=4,MAXLEN=4+MAXPOL*MAXCHAN)
      integer tno,pnt,nzero,nread,i,j,nbad,nrec,ncorr,VisSize
      ptrdiff offset
      complex data(MAXCHAN,MAXPOL),out(MAXLEN),ctemp
      logical flags(MAXCHAN,MAXPOL),more
      real uumax,vvmax,rms2,Wt,SumWt,rms2f(MAXCHAN),Wtf(MAXCHAN)
      double precision uvw(5),dSumWt,dfreq0
      character num*10
c
c  Externals.
c
      character itoaf*10
      integer len1
c
c  Get the first record.
c
      tno = 0
      call GetRec(tno,uvw,data,flags,npol,MAXCHAN,nchan)
c
      if(mfs)then
        ncorr = npol
        call output('Making MFS images')
      else if(nchan.eq.1)then
        ncorr = npol
        call output('Making single plane images')
      else
        ncorr = npol*nchan
        num = itoaf(nchan)
        i = len1(num)
        call output('Making cubes with '//num(1:i)//' planes')
      endif
      VisSize = 2*(ncorr + 4)
      call scrrecsz(tscr,VisSize)
c
      nread = nchan
      more = nchan.gt.0
      if(.not.mfs.and.nchan.gt.mchan)
     *  call bug('f','Too many channels for me to cope with')
      if(nread.eq.0)call bug('f','No data to process')
      call uvDatGta('name',vis)
c
c  Initialise everything.
c
      do i=1,nchan*npol
        ChanWt(i) = 0
      enddo
      SumWt = 0
      dSumWt = 0
c
      offset = 0
      nvis = 0
      nbad = 0
      uumax = umax
      vvmax = vmax
      umax = 0
      vmax = 0
      dfreq0 = 0
c
c  Loop over all the data.
c  Determine whether to accept this record, and copy it to the output
c  buffer. Then write it out if needed.
c
      dowhile(more)
c
        call uvDatGtr('variance',rms2)
        if (systemp(2)) call uvDatGtv('variancef',rms2f,nchan)
        if(systemp(1))then
          if(rms2.gt.0)then
            Wt = 1/rms2
          else
            Wt = 0
          endif
        else if (systemp(2)) then
          do i=1,nchan
            if (rms2f(i).gt.0) then
               Wtf(i) = 1/rms2f(i)
            else
              Wtf(i) = 0
            endif
          enddo
        else
          call uvrdvrr(tno,'inttime',Wt,0.0)
        endif
c
c  Multiply by sqrt(-1) if needed.
c
        if(doimag)then
          do j=1,npol
            do i=1,nchan
              ctemp = data(i,j)
              data(i,j) = cmplx(-aimag(ctemp),real(ctemp))
            enddo
          enddo
        endif
c
c  Process it all.
c
        if(mfs)then
          call ProcMFS (tno,uvw,Wt,rms2,Wtf,rms2f,systemp(2),data,flags,
     *        npol,MAXCHAN,nread,nvis,nbad,out,MAXLEN,nrec,ncorr,
     *        uumax,vvmax,umax,vmax,dSumWt,dfreq0)
        else
          call ProcSpec(tno,uvw,Wt,rms2,data,flags,
     *        npol,MAXCHAN,nread,nvis,nbad,out,MAXLEN,nrec,ncorr,
     *        uumax,vvmax,umax,vmax,SumWt,ChanWt,slop,slopmode)
        endif
c
c  Process an accepted record.
c
        if(nrec.gt.0)then
          call HdChk(tno,uvw)
          if(mosaic)then
            call MosChk(tno,pnt)
          else
            pnt = 1
          endif
c
c  Correct the pointing centre if needed.
c
          if(pnt.ne.1)then
            j = 2
            do i=1,nrec
              Out(j) = cmplx(real(Out(j)),real(pnt))
              j = j + ncorr + 4
            enddo
          endif
c
          call scrwrite(tscr,Out,offset,nrec)
          offset = offset + nrec
        endif
        call GetRec(tno,uvw,data,flags,npol,MAXCHAN,nread)
        more = nread.eq.nchan.or.(mfs.and.nread.gt.0)
      enddo
c
c  Say how many records were rejected.
c
      call output('Visibilities accepted: '//itoaf(nvis))
      if(nbad.gt.0)
     *  call bug('w','Visibilities rejected: '//itoaf(nbad))
      if(nvis.eq.0)call bug('f','No visibilities to map')
c
c  Fiddle slop gain factor. Get MFS mean frequency.
c
      if(mfs)then
        nchan = 1
        freq0 = exp(dfreq0/dSumWt)
        do i=1,npol
          ChanWt(i) = 1
        enddo
      else
        nzero = 0
        do i=1,npol*nchan
          if(ChanWt(i).le.0)then
            nzero = nzero + 1
          else
            ChanWt(i) = SumWt / ChanWt(i)
          endif
        enddo
        if(nzero.gt.0)call bug('w',
     *    'Number of channels with no good data: '//itoaf(nzero))
      endif
c
      end
c***********************************************************************
      subroutine ProcMFS(tno,uvw,Wt,rms2,Wtf,rms2f,systempf,data,flags,
     *        npol,mchan,nchan,nvis,nbad,out,MAXLEN,nrec,ncorr,
     *        uumax,vvmax,umax,vmax,SumWt,freq0)
c
      integer tno,nchan,npol,mchan,nvis,nbad,MAXLEN,nrec,ncorr
      double precision uvw(3)
      real rms2,rms2f(nchan),uumax,vvmax,umax,vmax,Wt,Wtf(nchan)
      double precision freq0,SumWt
      complex data(mchan,npol),out(MAXLEN)
      logical flags(mchan,npol),systempf
c
c  Process a visibility spectrum in MFS mode.
c
c  Input:
c    tno        Handle of the input dataset.
c    nchan      Number of channels.
c    npol       Number of polarisations.
c    mchan      First dim of data and flags.
c    Data       Visibility data.
c    flags      Flags associated with the visibility data.
c    Wt         Basic weight.
c    Wtf        Weight spectrum
c    rms2       Noise variance.
c    rms2f      Noise variance spectrum
c    systempf   Use variance and weight spectrum 
c    uumax,vvmax u,v limits.
c    ncorr      Number of correlations in each output record.
c  Input/Output:
c    SumWt      Sum of all the weights.
c    umax,vmax  Max value for abs(u),abs(v).
c    nvis       Number of good visibilities.
c    nbad       Number of bad visibilities.
c  Output:
c    out        A record consisting of
c               u,v,w,pointing,rms**2,log(freq),wt,0,r1,i1,r2,i2,...
c    nrec       Number of records.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer i,j,nlen
      logical ok
      real u,v,uu,vv,ww,f,t
      double precision sfreq(MAXCHAN)
c
c  Check whether the weight is positive.
c
      if(ncorr.ne.npol)call bug('f','Something is screwy in ProcMFS')

      ok = Wt.gt.0
      nlen = 0
c
c  Collapse all the polarisation flags into one flag vector.
c
      if(ok)then
        do j=2,npol
          do i=1,nchan
            flags(i,1) = flags(i,1).and.flags(i,j)
          enddo
        enddo
c
c  Get frequency information.
c
        call uvinfo(tno,'sfreq',sfreq)
        uu = uvw(1) / sfreq(1)
        vv = uvw(2) / sfreq(1)
        ww = uvw(3) / sfreq(1)
c
c  Copy them to the output buffer.
c
        do i=1,nchan
          f = sfreq(i)
          u = abs(uu * f)
          v = abs(vv * f)
          ok = (.not.systempf).or.Wtf(i).gt.0
          ok = ok.and.flags(i,1).and.u.lt.uumax.and.v.lt.vvmax
          if(ok)then
            if(nlen+4+npol.gt.MAXLEN)call bug('f',
     *                'Buffer overflow, in ProcMFS')
            t = log(f)
            out(nlen+1) = cmplx(uu*f,vv*f)
            out(nlen+2) = cmplx(ww*f,1.0)
            if (systempf)then
              out(nlen+3) = cmplx(rms2f(i),t)
              out(nlen+4) = Wtf(i)
              freq0 = freq0 + Wtf(i) * t
              SumWt = SumWt + Wtf(i)
            else
              out(nlen+3) = cmplx(rms2,t)
              out(nlen+4) = Wt
              freq0 = freq0 + Wt * t
              SumWt = SumWt + Wt
            endif
            umax = max(u,umax)
            vmax = max(v,vmax)
c
            nlen = nlen + 4
            do j=1,npol
              nlen = nlen + 1
              out(nlen) = data(i,j)
            enddo
            nvis = nvis + 1
          else
            nbad = nbad + 1
          endif
        enddo
      else
        nbad = nbad + nchan
      endif
c
      nrec = nlen / (4 + ncorr)
c
      end
c***********************************************************************
      subroutine ProcSpec(tno,uvw,Wt,rms2,data,flags,
     *        npol,MAXCHAN,nchan,nvis,nbad,out,MAXLEN,nrec,ncorr,
     *        uumax,vvmax,umax,vmax,SumWt,ChanWt,slop,slopmode)
c
      integer tno,nchan,npol,MAXCHAN,nvis,nbad,MAXLEN,nrec,ncorr
      double precision uvw(3)
      real rms2,umax,vmax,uumax,vvmax,ChanWt(npol*nchan),slop,Wt,SumWt
      complex data(MAXCHAN,npol),out(MAXLEN)
      logical flags(MAXCHAN,npol)
      character slopmode*(*)
c
c  Process a visibility spectrum.
c
c  Input:
c    tno        Handle of the input dataset.
c    nchan      Number of channels.
c    npol       Number of polarisations.
c    Data       Visibility data.
c    flags      Flags associated with the visibility data.
c    slop       Slop tolerance.
c    slopmode   Slop mode.
c    Wt         Basic weight.
c    rms2       Noise variance.
c    uumax,vvmax Max u,v value to map.
c    ncorr      Number of correlations in the output record.
c  Input/Output:
c    ChanWt     Channel "slop" normalisation.
c    SumWt      Sum of all the weights.
c    umax,vmax  Max value for u,v.
c    nvis       Number of good visibilities.
c    nbad       Number of bad visibilities.
c  Output:
c    out        A record consisting of
c               u,v,w,pointing,rms**2,0,wt,0,r1,i1,r2,i2,...
c-----------------------------------------------------------------------
      integer i,j,badcorr,pnt
      real u,v
      logical ok,somebad
c
c  Is the weight positive?
c
      ok = Wt.gt.0
      if(ncorr.ne.npol*nchan)
     *        call bug('f','Something screwy in ProcSpec')
c
c  Count the number of bad correlations.
c
      if(ok)then
        somebad = .false.
        do j=1,npol
          badcorr = 0
          do i=1,nchan
            if(.not.flags(i,j))badcorr = badcorr + 1
          enddo
          somebad = somebad.or.badcorr.gt.0
          ok = ok.and.badcorr.le.slop*nchan.and.badcorr.lt.nchan
        enddo
      endif
c
c  Interpolate the bad correlations, if that is required.
c
      if(ok.and.somebad.and.slopmode.eq.'interpolate')
     *  call SlopIntp(Data,flags,nchan,npol,MAXCHAN)
c
c  If we are to accept it, then zero out any bad correlations.
c
      u = abs(uvw(1))
      v = abs(uvw(2))
      if(ok.and.u.lt.uumax.and.v.lt.vvmax)then
        if(ncorr+4.gt.MAXLEN)
     *        call bug('f','Buffer overflow, in ProcSpec')
        out(1) = cmplx(real(uvw(1)),real(uvw(2)))
        out(2) = cmplx(real(uvw(3)),1.0)
        out(3) = rms2
        out(4) = wt
        umax = max(u,umax)
        vmax = max(v,vmax)
        SumWt = SumWt + Wt
        pnt = 1
        do j=1,npol
          do i=1,nchan
            if(flags(i,j))then
              ChanWt(pnt) = ChanWt(pnt) + Wt
              out(pnt+4) = Data(i,j)
            else
              out(pnt+4) = 0
            endif
            pnt = pnt + 1
          enddo
        enddo
        nrec = 1
        nvis = nvis + 1
      else
        nrec = 0
        nbad = nbad + 1
      endif
c
      end
c***********************************************************************
      subroutine SlopIntp(Data,flags,nchan,npol,MAXCHAN)
c
      integer nchan,npol,MAXCHAN
      complex Data(MAXCHAN,npol)
      logical flags(MAXCHAN,npol)
c
c  Linearly interpolate bad channels.
c
c  Input:
c    nchan
c    npol
c  Input/Output:
c    data
c    flags
c-----------------------------------------------------------------------
      integer i,j,i0,id
      real fac
      logical badpatch
c
      do j=1,npol
        i0 = 0
        badpatch = .false.
c
        do i=1,nchan
c
c  If its the end of a bad patch, then interpolate the solution.
c
          if(badpatch.and.flags(i,j))then
            if(i0.eq.0)then
              do id=1,i-1
                data(id,j) = data(i,j)
              enddo
            else
              fac = 1./real(i-i0)
              do id=i0+1,i-1
                data(id,j) =
     *                fac * ( (id-i0)*data(i0,j) + (i-id)*data(i,j) )
              enddo
            endif
          endif
c
          badpatch = .not.flags(i,j)
          if(.not.badpatch) i0 = i
          flags(i,j) = .true.
        enddo
c
c  Replicate visibilities for a bad patch at the end of the spectrum.
c
        if(badpatch)then
          if(i0.eq.0)call bug('f','Inconsistency in SlopIntp')
          do id=i0+1,nchan
            data(id,j) = data(i0,j)
          enddo
        endif
      enddo
c
      end
c***********************************************************************
      subroutine GetRec(tno,uvw,data,flags,npol,MAXCHAN,nchan)
c
      integer npol,MAXCHAN,nchan,tno
      double precision uvw(5)
      complex data(MAXCHAN,npol)
      logical flags(MAXCHAN,npol)
c
c  Get a record from the input dataset.
c-----------------------------------------------------------------------
      integer nread,i
      logical more,first
c
c  Externals.
c
      logical uvDatOpn
c
      nread = 0
      more = .true.
      first = .false.
c
      dowhile(nread.eq.0.and.more)
        if(tno.ne.0)then
          call uvDatRd(uvw,data,flags,MAXCHAN,nread)
          if(nread.eq.0)then
            call uvDatCls
            call MosCDone(tno)
            call HdDone(tno)
          else
            more = .false.
          endif
        else
          nread = 0
        endif
c
        if(nread.eq.0)then
          more = uvDatOpn(tno)
          first = more
        endif
      enddo
c
c  Read the rest of the polarisation records.
c
      if(nread.gt.0)then
        do i=2,npol
          call uvDatRd(uvw,data(1,i),flags(1,i),nread,nchan)
          if(nread.ne.nchan)call bug('f',
     *        'Number of channels differ between polarisations')
        enddo
      endif
c
      nchan = nread
      if(first)call VarChk(tno)
c
      end
c***********************************************************************
      subroutine VarChk(tno)
c
      integer tno
c
c  Check whether we can successfully compute the noise variance.
c-----------------------------------------------------------------------
      integer CHANNEL,WIDE
      parameter(CHANNEL=1,WIDE=2)
      real rtemp
      character name*32
      double precision line(6)
      integer itype
c
      call uvDatGtr('variance',rtemp)
      if(rtemp.le.0)then
        call uvDatGta('name',name)
        call bug('w','Noise variance cannot be determined for '//name)
        call uvrdvrr(tno,'inttime',rtemp,0.)
        if(rtemp.le.0)call bug('w',
     *    '... variable inttime is missing or non-positive')
        call uvrdvrr(tno,'jyperk',rtemp,0.)
        if(rtemp.le.0)call bug('w',
     *    '... variable jyperk is missing or non-positive')
        call uvinfo(tno,'line',line)
        itype = nint(line(1))
        if(itype.eq.WIDE)then
          call uvrdvrr(tno,'wsystemp',rtemp,0.)
          if(rtemp.le.0)call bug('w',
     *      '... variable wsystemp is missing or non-positive')
        else
          call uvrdvrr(tno,'systemp',rtemp,0.)
          if(rtemp.le.0)call bug('w',
     *      '... variable systemp is missing or non-positive')
        endif
        call bug('w','Set the variable(s) using puthd')
      endif
c
      end
