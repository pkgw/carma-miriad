c********1*********2*********3*********4*********5*********6*********7**
      program varmaps
      implicit none
c
c= VARMAPS  Image uvdata versus selected x-axis and y-axis uv-variables.
c& pjt
c: image conversion, analysis.
c+
c       VARMAPS makes a Miriad image of uvdata or single dish data
c	versus selected x-axis and y-axis uv-variables.
c	Data is smoothed and added into an image with the given 
c       beam, cell, and imsize
c	E.g. VARMAPS can map autocorrelation data versus dra and ddec. 
c       and thus create single dish maps.
c	Related tasks:
c	  	UVCAL options=holo replace [u,v] with [dazim,delev]
c	  	INVERT convolves uvdata into a grid and makes a 2D FFT.
c               VARMAP more general (non-single dish) mapping routine
c@ vis
c       The input uv-dataset name. No default.
c@ select
c       This selects which visibilities to be used. Default uses
c       all the data. See the Users Guide for information about
c       how to specify uv data selection.
c       e.g. 'select=ant(1)(5),-time(00:00,01:09)'
c@ line
c       Linetype of the data in the format line,nchan,start,width,step
c       "line" can be `channel', `wide' or `velocity'.
c       Default is channel,0,1,1,1, which uses all the spectral
c       channels. For line=channel, select only one spectral window
c       if the channels are not contiguous in velocity.
c       Flagged data are not used.
c       e.g. line=channel,6,1,15,15 makes the 6 band averages for CARMA.
c
c@ xaxis
c	x-axis uvvariable used for x-axis.
c	An optional second argument gives the index of the uvvariable.
c	The default index=1.
c	The default units are defined in an appendix of the Users Guide.
c	xaxis can also be 'u' or 'v' (nanosecs). Default xaxis=dra,1
c	e.g. xaxis=dazim,1
c
c@ yaxis
c	y-axis uvvariable used for y-axis.
c	An optional second argument gives the index of the uvvariable.
c	The default index=1.
c	The default units are defined in an appendix of the Users Guide.
c	yaxis can also be 'u' or 'v' (nanosecs). Default yaxis=ddec,1
c	e.g. yaxis=delev,1
c
c@ zaxis
c	Visibility 'amplitude', 'phase', 'real', or 'imaginary'.
c	Default zaxis=real
c	No calibration is applied by VARMAPS. 
c
c@ scale
c       Scaling factor applied to the data. Default:  1.
c       See also options=jyperk below.
c
c@ out
c	Output image or image cube. No default.
c
c@ imsize
c	The size of the output image x-axis and y-axis. If only one value is
c	given it is used for both axes. The 3rd axis is
c	determined by the number of channels in the selected linetype.
c
c@ xcell
c     Image pixel size along x-axis.
c     No interpolation into adjacent pixels is made. Use the same pixel
c     size as any gridding of the uvvariable. e.g. the dazim and delev
c     step size used to aquire pointing data.
c     An optional second argument causes conversion of the units. 
c     Possible values for the units (a minimum match algorithm is used)
c     are "arcsec", "arcmin", "degrees" and "hours".
c     No default. e.g. xcell=30,arcsec
c
c
c@ ycell
c     Image pixel size along xaxis.
c     No interpolation into adjacent pixels is made. Use the same pixel
c     size as any gridding of the uvvariable. e.g. the delev step used
c     to aquire pointing data, or the delay step in a delay search etc.
c     An optional second argument causes conversion of the units. 
c     Possible values for the units (a minimum match algorithm is used)
c     are "arcsec", "arcmin", "degrees" and "hours".
c     Default is to use the same value and units as xcell.
c
c@ xbeam
c     Smoothing beam in X. Will use same units are xcell. No default.
c@ ybeam
c     Smoothing beam in Y. Will use same units are ycell. 
c     Default is to use the same value and units as xbeam.
c@ size
c     One or two numbers:
c     Number of neighbor pixels to look around for smoothing. This means
c     an area of 2*size+1 by 2*size+1 pixels around the center pixel
c     will be used for contributions to smoothing. This should probably
c     be something a little larger than beam/cell.
c     A second size is used to put a guard around the outermost observed
c     points. If not given, it defaults to the neighbor pixel count.      
c     Addition softer tapering (see below) can be applied to this, treating
c     each cell as if there was a pointing.
c     Default: 0 
c@ mode
c     Smoothing mode.
c     0 = gaussian
c     1 = cone
c
c@ cutoff
c     Level at which the gaussian is cut off to 0.  Default: 0.000001
c
c@ soft
c     Factor by which FWHM should be multiplied for the soft edge
c     under options=soft. Default: 1
c
c@ ants
c     Antenna to use for options=systemp weights. Although this was
c     likely given in select=ant(X) as well, due to the MIRIAD
c     selection mechanism, it needs to be specified here as well.
c     No default.
c
c@ options
c       This gives extra processing options. Several options can be given,
c       each separated by commas. They may be abbreviated to the minimum
c       needed to avoid ambiguity. Some options cannot be choosen together.
c       sum     - not weights, only useful for regular grids.
c       taper1  - old style tapering,doesn't seem to work well
c       taper2  - new style tapering using one boundary layer
c       edge    - sharp edge, it will cut signal outside the bounding box 
c       inttime - create integration time map in channel 1
c       debug   - lots of verbose output 
c       systemp - weight by systemp
c       jyperk  - trust the Jy/K in the header and convert to Jy.
c                 optionally scale= can be used to force
c       
c
c--
c  History:
c     pjt  28jan11  Initial version, cloned off varmap
c     pjt  18mar11  write flags as well
c     pjt  28mar11  taper,edge
c     pjt   4apr11  added scale=
c     pjt   6sep11  added soft= 
c     pjt  13nov12  fixed init problem in maps, rearrange pixel filling
c     pjt  19nov12  masking with better tapering
c     pjt  28nov12  more tapering tinkering and fixing
c     pjt  30nov12  renaming to taper1 , taper2
c     pjt   4dec12  options=systemp,jyperk
c     pjt  11dec12  options=taper1 now better weighted
c-----------------------------------------------------------------------
       include 'maxdim.h'
       include 'mirconst.h'
       character*(*) version
       parameter(version='VARMAPS: version 18-jul-2013')
       integer MAXSELS
       parameter(MAXSELS=512)
       integer MAXVIS
       parameter(MAXVIS=10000)
       integer MAXCHAN2
       parameter(MAXCHAN2=384)
       integer MAXVPP
       parameter(MAXVPP=MAXVIS/4)
       integer MAXSIZE
       parameter(MAXSIZE=256)
       real sels(MAXSELS)
       complex data(MAXCHAN2)
       logical flags(MAXCHAN2)
       double precision preamble(4)
       integer lIn,nchan,nread,nvis,ngrid
       real start,width,step
       character*128 vis,out,linetype,line
       character*10 xaxis,yaxis,zaxis,xunit,yunit
       integer idata(MAXANT)
       real rdata(MAXANT)
       double precision ddata(MAXANT)
       integer lout,nsize(3),i,j,k,l,ng,i1,j1,id,jd,size,size2
       real cell(2),beam(2),beam2(2),beam3(2)
       real stacks(MAXVIS,MAXCHAN2), buffer(MAXCHAN2)
       real    xstacks(MAXVIS), ystacks(MAXVIS)
       integer istacks(MAXVIS), jstacks(MAXVIS)
       integer idx(MAXSIZE,MAXSIZE,MAXVPP+1)
       real array(MAXSIZE,MAXSIZE,MAXCHAN2)
       real weight(MAXSIZE,MAXSIZE,MAXCHAN2)
       logical mask(MAXSIZE,MAXSIZE)
       real tsys(MAXANT), wtsys(MAXVIS), tsysmin, tsysmax, jyperk
       real x,y,z,x0,y0,datamin,datamax,f,w,cutoff, xscale,yscale,scale
       real softfac, sumg2, sumg3, sumg2min, sumg3min,wmax
       character*1 xtype, ytype, type
       integer length, xlength, ylength, xindex, yindex, cnt, mode,nmask
       integer imin,jmin,imax,jmax,iant
       logical updated,sum,debug,hasbeam,doweight,dotaper1,dotaper2,edge
       logical doimap,dotsys,dojyperk,do0
       logical masktest
       external masktest
c

       integer nout, nopt
       parameter(nopt=8)
       character opts(nopt)*10
       data opts/'arcseconds','arcminutes','radians   ',
     *        'degrees   ','hours     ','dms       ',
     *        'hms       ','time      '/
c
c  Get the input parameters.
c
       call output(version)
       call keyini
       call keyf ('vis',vis,' ')
       call keyline(linetype,nchan,start,width,step)
       call SelInput ('select',sels,maxsels)
       call keya ('out',out,' ')
       call keyr ('scale',scale,1.0)
       call keya ('xaxis',xaxis,'dra')
       call keyi ('xaxis',xindex,1)
       call keya ('yaxis',yaxis,'ddec')
       call keyi ('yaxis',yindex,1)
       call keya ('zaxis',zaxis,'real')
       call keyi ('imsize',nsize(1),16)
       call keyi ('imsize',nsize(2),nsize(1))
       call keyr ('xcell',cell(1),0.)
       call keymatch('xcell',nopt,opts,1,xunit,nout)
       if(nout.eq.0)xunit = ' '
       call keyr ('xbeam',beam(1),0.)
       call keyr ('ycell',cell(2),cell(1))
       call keymatch('ycell',nopt,opts,1,yunit,nout)
       if(nout.eq.0)yunit = xunit
       call keyr ('ybeam',beam(2),beam(1))
       call keyi ('size',size,0)
       call keyi ('size',size2,size)
       call keyi ('mode',mode,0)
       call keyr ('cutoff',cutoff,0.00000001)
       call keyr ('soft', softfac, 1.0)
       call keyi ('ants',iant,-1)
c        options=  sum debug taper    edge soft     inttime none
       call GetOpt(sum,debug,dotaper1,edge,dotaper2,doimap,
     *             dotsys,dojyperk,do0)
       call keyfin
c
c  Check that the inputs are reasonable.
c
       if (vis.eq.' ') call bug('f','Input name must be given')
       if (out.eq.' ') call bug('f','Output image missing')
       if (nchan.gt.0) nchan = min (nchan,MAXCHAN2)
       if (nsize(1)*nsize(2).le.0)
     *		call bug('f','imsize unreasonable')
       if (cell(1)*cell(2).le.0)
     *		call bug('f','cell size must be given')
       hasbeam  = beam(1)*beam(2) .gt. 0.0
       do j=1,2
         if (nsize(j).gt.MAXSIZE) then
 	   write(line,'(a,i2,a,i4)') 'Map axis',j,' larger than',MAXSIZE 
            call bug('f',line)
          endif
       enddo
       write(line,'(a,a)')
     *	 'Mapping ', zaxis 
       call output(line)
       write(line,'(a,a,i3,4x,  a,g12.4,2x,a)')
     *   ' x-axis: ', xaxis, xindex, '   pixel size =', cell(1), xunit
       call output(line)
       write(line,'(a,a,i3,4x,  a,g12.4,2x,a)')
     *   ' y-axis: ', yaxis, yindex, '   pixel size =', cell(2), yunit
       call output(line)
       if (xunit.ne.' ') then
          call units(cell(1),xunit)
          call units(beam(1),xunit)
       endif
       if (yunit.ne.' ') then
          call units(cell(2),yunit)
          call units(beam(2),yunit)
       endif
       if (dotsys .and. iant.lt.1) then
          call bug('f','options=systemp needs ants=')
       endif

       xscale = 1.0
       yscale = 1.0
       if (xaxis.eq.'dra') xscale=-1.0
c              beam2 is for in-point smoothing, beam3 for softened edge smoothing
       beam2(1) = beam(1)*beam(1) / 2.77259
       beam2(2) = beam(2)*beam(2) / 2.77259
       beam3(1) = beam2(1)*softfac*softfac
       beam3(2) = beam2(2)*softfac*softfac
       sumg2 = 0.0;
       sumg3 = 0.0;
       sumg2min = -1.0
       sumg3min = -1.0
       do jd=-size,size
          do id=-size,size
             w = (id*cell(1))**2/beam2(1) + (jd*cell(2))**2/beam2(2)
             sumg2 = sumg2 + exp(-w)
             if (jd.eq.-size .and. id.eq.-size) sumg2min = sumg2
          enddo
       enddo
       do jd=-size,size
          do id=-size,size
             w = (id*cell(1))**2/beam3(1) + (jd*cell(2))**2/beam3(2)
             sumg3 = sumg3 + exp(-w)
             if (jd.eq.-size .and. id.eq.-size) sumg3min = sumg3
          enddo
       enddo
       write(*,*) 'SUM G2,G3=',sumg2,sumg3,sumg2min,sumg3min
       write(*,*) 'cell,beam: ',cell(1),beam(1)
       write(*,*) 'beam2: ',beam2(1)
       write(*,*) 'beam3: ',beam3(1)
c
c  Open an old visibility file, and apply selection criteria.
c
       call uvopen (lIn,vis,'old')
       if(linetype.ne.' ')
     *   call uvset (lIn,'data',linetype,nchan,start,width,step)
       call uvset (lIn,'coord','nanosec',0, 0.0, 0.0, 0.0)
       call uvset (lIn,'planet', ' ', 0, 0.0, 0.0, 0.0)
       call SelApply(lIn,sels,.true.)
c
c  Read the first record and check the data type.
c
       call uvread (lIn, preamble, data, flags, MAXCHAN2, nread)
       if(nread.le.0) call bug('f','No data found in the input.')

c      @ todo: these appear to be read wrong?  3.5m instead of 10m
       call uvgetvrr(lIn,'jyperk',jyperk,1)
       write(*,*) 'first jyperk=',jyperk

       if(index(linetype,'wide').gt.0)then
         call uvprobvr(lIn,'wcorr',type,length,updated)
       else
         call uvprobvr(lIn,'corr',type,length,updated)
       endif
       if(type.eq.'r') call bug('i','data is real')
       if(type.eq.'j'.or.type.eq.'c') call bug('i','data is complex')
c
c  Check out xaxis and yaxis variables and length
c
       if(xaxis.ne.'u'.and.xaxis.ne.'v')then
          call uvprobvr(lIn,xaxis,xtype,xlength,updated)
        if(xtype.ne.'r' .and. xtype.ne.'i' .and. xtype.ne.'d')
     *    call bug('f','valid xaxis not in input file')
        if(xindex.lt.1. .or. xindex.gt.xlength)
     *    call bug('f','x-axis index is not between 1 and xlength')
       endif
       if(yaxis.ne.'u'.and.yaxis.ne.'v')then
          call uvprobvr(lIn,yaxis,ytype,ylength,updated)
        if(ytype.ne.'r' .and. ytype.ne.'i'.and. ytype.ne.'d')
     *    call bug('f','valid yaxis not in input file')
        if(yindex.lt.1. .or. yindex.gt.ylength)
     *    call bug('f','y-axis index is not between 1 and ylength')
       endif
c
c  Open the output array
c
      nsize(3) = nread
      write(line,'(a,i16)') 'Opening XY file...nsize(3)=',nread
      call output(line)
      call xyopen(lOut,Out,'new',3,nsize)
      call maphead(lIn,lOut,nsize,cell,xaxis,yaxis,
     *   xunit,yunit,linetype,dojyperk)

      do j=1,MAXSIZE
         do i=1,MAXSIZE
            idx(i,j,1) = 0
            mask(i,j) = .FALSE.
         enddo
      enddo
      nvis = 0
      ngrid = 0 
      tsysmin =  9999.
      tsysmax = -9999.
      jyperk = 1.0
      if (dojyperk) write(*,*) "Jy/K scaling applied"

c
c  Read through the uvdata 
c
      do while(nread.gt.0)
         nvis = nvis + 1
         if(nread.ne.nsize(3))
     *		 call bug('f','Number of channels has changed.')
         if (dojyperk) call uvgetvrr(lIn,'jyperk',jyperk,1)
c         write(*,*) 'jyperk=',jyperk

c
c  Get the selected axes.
c
         if(xaxis.eq.'u') then
	    x = preamble(1)
         else if(xaxis.eq.'v')then
	    x = preamble(2)
         else if(xtype.eq.'i')then
            call uvgetvri(lIn,xaxis,idata,xlength)
	    x = idata(xindex)
         else if(xtype.eq.'r')then
            call uvgetvrr(lIn,xaxis,rdata,xlength)
	    x = rdata(xindex)
         else if(xtype.eq.'d')then
            call uvgetvrd(lIn,xaxis,ddata,xlength)
	    x = ddata(xindex)
         else
	    call bug('f','Invalid xaxis')
         endif
c
         if(yaxis.eq.'u') then
	    y = preamble(1)
         else if(yaxis.eq.'v')then
	    y = preamble(2)
         else if(ytype.eq.'i')then
            call uvgetvri(lIn,yaxis,idata,ylength)
            y = idata(yindex)
         else if(ytype.eq.'r')then
            call uvgetvrr(lIn,yaxis,rdata,ylength)
            y = rdata(yindex)
         else if(ytype.eq.'d')then
            call uvgetvrd(lIn,yaxis,ddata,ylength)
            y = ddata(yindex)
         else
	    call bug('f','Invalid yaxis')
         endif
         x = xscale * x
         y = yscale * y

c update the weights, if there are new tsys
c         if(dotsys) call getwtsys(lIn, MAXANT, tsys)


c
c  Grid the data, and stack them away for later retrieval
c  Note nsize()/2 needs to be integer division to make
c  reference pixel at the center of a pixel for both odd
c  and even sized axes.
c
         i = nint(x/cell(1) + nsize(1)/2 + 1)
         j = nint(y/cell(2) + nsize(2)/2 + 1)
         if(i.ge.1.and.i.le.nsize(1).and.j.ge.1.and.j.le.nsize(2))then
	    ngrid = ngrid + 1
            if (ngrid.EQ.MAXVIS) call bug('w','MAXVIS not big enough')
            if (ngrid.LE.MAXVIS) then
               xstacks(ngrid) = x
               ystacks(ngrid) = y
               istacks(ngrid) = i
               jstacks(ngrid) = j
c                  read the weights (update happened earlier)
               if(dotsys) then
                  call getwtsys(lIn, MAXANT, tsys)
                  wtsys(ngrid) = 1000.0/tsys(iant)
                  if (tsys(iant).gt.tsysmax) tsysmax = tsys(iant)
                  if (tsys(iant).lt.tsysmin) tsysmin = tsys(iant)
c                  write(*,*) 'TSYS: ',ngrid,(tsys(k),k=1,6),wtsys(ngrid)
               else
                  wtsys(ngrid) = 1.0
               endif
c
               cnt = idx(i,j,1) + 1
               if (debug) write(*,*) 'Adding ',i,j,nvis,ngrid,cnt,nread
               if (cnt.ge.MAXVPP) call bug('f',
     *                           'Too many scans for MAXVPP')
               idx(i,j,1) = cnt
               idx(i,j,cnt+1) = ngrid
               do k=1,nread
                  if(flags(k)) then
                     if(index(zaxis,'re').gt.0) then
                        z = real(data(k))
                     else if(index(zaxis,'im').gt.0) then
                        z = aimag(data(k))
                     else if(index(zaxis,'am').gt.0) then
                        z = cabs(data(k))
                     else if(index(zaxis,'ph').gt.0) then
                        z = 180./pi*atan2(aimag(data(k)),real(data(k)))
                     else
                        call bug('f','Unknown zaxis')
                     end if
                     stacks(ngrid,k) = z * scale * jyperk
                  else
                     stacks(ngrid,k) = 0.0
                  end if
               enddo
            end if
         end if
         call uvread(lIn, preamble, data, flags, MAXCHAN2, nread)
      enddo
      if (ngrid.gt.MAXVIS) then
         write(*,*) 'Found ',ngrid,' scans to add. MAXVIS too small'
         call bug('f','Increase your MAXVIS')
      endif

c
c Optionally go over all the indexed stacks in an (i,j) cell, and 
c mean or median filter those into a single (thus cnt=1) stack per
c pointing. Right now we do this in imstack, but another option
c is to do this here  (@todo)
c

      do j=1,MAXSIZE
         do i=1,MAXSIZE
            do k=1,MAXCHAN2
               weight(i,j,k) = 0.0
               array(i,j,k)  = 0.0
            enddo
            cnt = idx(i,j,1)
            if (cnt.gt.0) then
               if (debug) write(*,*) i,j,cnt
               do l=1,cnt
                  ng = idx(i,j,l+1)
                  stacks(ng,1) = cnt
               enddo
               do jd=-size2,size2
                  j1=j+jd
                  do id=-size2,size2
                     i1=i+id
                     mask(i1,j1) = .TRUE.
                  enddo
               enddo
            end if
         enddo
      enddo

      nmask = 0
      do j=1,MAXSIZE
         do i=1,MAXSIZE
            if (mask(i,j)) nmask = nmask + 1
         enddo
      enddo
      write(*,*) 'Found ',nmask, ' cells masked good. size:',size,size2

c
c Retrieve all the uv scans from the stacks and smooth them into each
c grid point
c

      do j=1,MAXSIZE
         do i=1,MAXSIZE
            cnt = idx(i,j,1)
            if (cnt.gt.0) then
               do l=1,cnt
                  ng = idx(i,j,l+1)
                  x = xstacks(ng)
                  y = ystacks(ng)
                  do jd=-size,size
                     j1 = j + jd
                     y0 = (j1-1 - nsize(2)/2 ) * cell(2)
                     do id=-size,size
                        i1 = i + id
                        x0 = (i1-1 - nsize(1)/2 ) * cell(1)
                        if (i1.ge.1 .and. i1.le.nsize(1) .and. 
     *                      j1.ge.1 .and. j1.le.nsize(2)) then
                           if (hasbeam) then
                              if (mode.eq.0) then
                                 w = (x-x0)*(x-x0)/beam2(1)+
     *                               (y-y0)*(y-y0)/beam2(2)
                                 w = exp(-w)
                              else 
                                 w = ((x-x0)/beam(1))**2 +
     *                               ((y-y0)/beam(2))**2
                                 if (w.LT.1.0) then
                                    w = 1-sqrt(w)
                                 else
                                    w = 0.0
                                 end if
                              end if
                           else
                              w = 1.0
                           end if
                           if (w.lt.cutoff) w = 0.0
                           if (.NOT.mask(i1,j1)) w = 0.0
                           if(debug)write(*,*) i,j,i1,j1,cnt,ng,w
                           if(debug)write(*,*) x,y,mask(i1,j1),cutoff
                           if (w.gt.0.0) then
                              w = w * wtsys(ng)
                              do k=1,nsize(3)
                                 array(i1,j1,k) =  array(i1,j1,k) + 
     *                                w*stacks(ng,k)
                                 weight(i1,j1,k) = weight(i1,j1,k) + 
     *                                w
                              end do
                           end if
                        end if
                     end do
                  end do
               end do
            end if
         end do
      end do
      
c--


c     
c  Normalize the data back, and compute final minmax
c  If you want to taper the edges by FWHM, it will do that
c  when no original pointings were seen in those cells
c  For this we need to compute the bounding box in cell space
c  where we've seen pointings
c  Or taper as below
c
      imin=MAXSIZE+1
      jmin=MAXSIZE+1
      imax=0
      jmax=0

      cnt = 0
      do i=1,MAXSIZE
         do j=1,MAXSIZE
            if(idx(i,j,1).gt.0) then
               cnt = cnt + 1
               if(i.lt.imin) imin=i
               if(j.lt.jmin) jmin=j
               if(i.gt.imax) imax=i
               if(j.gt.jmax) jmax=j
            end if
         enddo
      enddo
      write(*,*) 'Count: ',cnt
      write(*,*) 'BoundingBox: ',imin,imax,jmin,jmax
      write(line,'(a)') 'Averaging the XY pixels...'
      call output(line)
      datamin = 1.E10
      datamax = -1.E10
      if (sum) then
         doweight = .FALSE.
      else
         doweight = .TRUE.
      endif
      do i=1,MAXSIZE
         do j=1,MAXSIZE
            do k=1,nsize(3)
               if(weight(i,j,k).ne.0.)then
                  if (edge) then
                     if (i.lt.imin .or. i.gt.imax .or.
     *                   j.lt.jmin .or. j.gt.jmax) then
                        array(i,j,k) = 0.0
                     end if
                  end if
                  if (doweight) then
                     array(i,j,k) = array(i,j,k) / weight(i,j,k)
                  end if
                  if(array(i,j,k).gt.datamax) datamax=array(i,j,k)
                  if(array(i,j,k).lt.datamin) datamin=array(i,j,k)
               end if
            end do
         end do
      end do

c--  now deal with the tapering off the outer masked (mask=false) area
c    and only grab tapered signal from the inner (mask=true) regions

c    The dotaper1 loops over all points that have no signal yet, and 
c    tries to steal some signal from the inner portion

      if (dotaper1) then
         write(*,*) 'Old edge tapering using the mask - new dec 2012'
         do j=1,MAXSIZE
            y = (j-1 - nsize(2)/2 ) * cell(2)
            do i=1,MAXSIZE
               x = (i-1 - nsize(1)/2 ) * cell(1)
               wmax = 0.0
               if (.not.mask(i,j)) then
                  do jd=-size,size
                     j1 = j + jd
                     y0 = (j1-1 - nsize(2)/2 ) * cell(2)
                     do id=-size,size
                        i1 = i + id
                        x0 = (i1-1 - nsize(1)/2 ) * cell(1)
                        if (i1.ge.1 .and. i1.le.nsize(1) .and. 
     *                      j1.ge.1 .and. j1.le.nsize(2) .and.
     *                      masktest(MAXSIZE,mask,i1,j1)) then
c     *                     mask(i1,j1)) then
                           w = (x-x0)*(x-x0)/beam3(1)+
     *                         (y-y0)*(y-y0)/beam3(2)
                           w = exp(-w)
                           if (w.gt.wmax) wmax = w
                           if (w.lt.cutoff) w = 0.0
                           if (w.gt.0.0) then
                              do k=1,nsize(3)
                                 array(i,j,k) = 
     *                             array(i,j,k) + w*array(i1,j1,k)
                                 weight(i,j,k) = 
     *                             weight(i,j,k) + w
                              end do
                           end if
                        end if
                     end do
                  end do
                  do k=1,nsize(3)
                     if (weight(i,j,k).gt.0) then
                        array(i,j,k) = array(i,j,k)/weight(i,j,k)*wmax
c                        array(i,j,k) = array(i,j,k)/weight(i,j,k)
                     end if
                  end do
               end if
            end do
         end do
      end if


c--  the dotaper2 loops over the signal edge and then tries to spill that
c    into the non-signal area
        
      if (dotaper2) then
         write(*,*) 'New tapering at the edge using the mask'
         do j=1,MAXSIZE
            y = (j-1 - nsize(2)/2 ) * cell(2)
            do i=1,MAXSIZE
               x = (i-1 - nsize(1)/2 ) * cell(1)
               if (masktest(MAXSIZE,mask,i,j)) then
                  do jd=-size,size
                     j1 = j + jd
                     y0 = (j1-1 - nsize(2)/2 ) * cell(2)
                     do id=-size,size
                        i1 = i + id
                        x0 = (i1-1 - nsize(1)/2 ) * cell(1)
                        if (i1.ge.1 .and. i1.le.nsize(1) .and. 
     *                      j1.ge.1 .and. j1.le.nsize(2) .and.
     *                     .not.mask(i1,j1)) then
                           w = (x-x0)*(x-x0)/beam3(1)+
     *                         (y-y0)*(y-y0)/beam3(2)
                           w = exp(-w)
                           if (w.lt.cutoff) w = 0.0
                           if (w.gt.0.0) then
                              do k=1,nsize(3)
                                 array(i1,j1,k) = 
     *                             array(i1,j1,k) + w*array(i,j,k)
                                 weight(i1,j1,k) = 
     *                             weight(i1,j1,k) + 1
                              end do
                           end if
                        end if
                     end do
                  end do
               end if
            end do
         end do
c--                          normalize the edge cells that got signal
         if (.true.) then
            do j=1,MAXSIZE
               do i=1,MAXSIZE
                  if (.not.mask(i,j)) then
                     do k=1,nsize(3)
                        if (weight(i,j,k).gt.0) then
                           array(i,j,k) = array(i,j,k) / weight(i,j,k)
                        end if
                     end do
                  end if
               end do
            end do
         end if
      end if
      
c--   create super hard edges if so desired

      if (.not.doimap) then
         do j=1,MAXSIZE
            do i=1,MAXSIZE
               array(i,j,1) = 0.0
            end do
         end do
      end if
c     
c  Write the image and it's header.
c
      call putimage(lOut,MAXSIZE,MAXCHAN2,nsize,array)
      call wrhdr(lOut,'datamin',datamin)
      call wrhdr(lOut,'datamax',datamax)
c
c  Write summary.
c
      write(line,'(a,i6)') ' number of records read= ',nvis
      call output(line)
      write(line,'(a,i6)') ' number of records mapped= ',ngrid
      call output(line)
      write(line,'(a,f7.1,1x,f7.1)') 'Datamin/max: ',datamin,datamax
      call output(line)
      if (dotsys) then
         write(line,'(a,f7.1,1x,f7.1)') 'Range in systemp: ',
     *         tsysmin,tsysmax
         call output(line)
      endif
c
c  Write the history file.
c

      call hdcopy(lIn,lOut,'history')
      call hisopen(lOut,'append')
      call hiswrite(lOut, 'VARMAPS '//version)
      call hisinput(lOut, 'VARMAPS')
      call hisclose(lOut)
c
c  Close the files after writing history
c
      call uvclose(lIn)
      call xyclose(lOut)
c
      end
c********1*********2*********3*********4*********5*********6*********7**
      subroutine maphead(lIn,lOut,nsize,cell,xaxis,yaxis,
     *   xunit,yunit,linetype,jyperk)
      implicit none
      integer	lin,lout,nsize(3)
      real cell(2)
      character*(*) xaxis,yaxis,xunit,yunit,linetype
      logical jyperk
c  Inputs:
c    lIn	The handle of the autocorrelation data.
c    lOut	The handle of the output image.
c    nsize	The output image size.
c    cell	The output image cell size.
c    xaxis,yaxis  x- and y-axis
c    xunit,yunit  x- and y-axis units
c    linetype	linetype
c-------------------------------------------------------------------------
      include 'maxdim.h'
      character source*16,telescop*16
      double precision ra,dec,restfreq,wfreq,wwidth,velocity(MAXCHAN)
      real epoch
c
c  Get source parameters.
c
      call uvrdvra(lIn,'source',source,' ')
      call uvrdvrd(lIn,'ra',ra,0.d0)
      call uvrdvrd(lIn,'dec',dec,0.d0)
      call uvrdvrd(lIn,'restfreq',restfreq,0.d0)
      call uvrdvrr(lIn,'epoch',epoch,0.d0)
      call uvrdvra(lIn,'telescop',telescop,' ')
c
c  Write header values.
c  Note if jyperk is used, the beam is not specified
c
      call wrhdi(lOut,'naxis',3)
      call wrhdi(lOut,'naxis1',nsize(1))
      call wrhdi(lOut,'naxis2',nsize(2))
      call wrhdi(lOut,'naxis3',nsize(3))
      call wrhdd(lOut,'crpix1',dble(nsize(1)/2+1))
      call wrhdd(lOut,'crpix2',dble(nsize(2)/2+1))
      call wrhdd(lOut,'crpix3',1.0d0)
      call wrhdd(lOut,'cdelt1',dble(-cell(1)))
      call wrhdd(lOut,'cdelt2',dble(cell(2)))
      if (jyperk) then
         call wrhda(lOut,'bunit','Jy')
      else
         call wrhda(lOut,'bunit','K')
      endif
      call wrhdd(lOut,'crval1',ra)
      call wrhdd(lOut,'crval2',dec)
      call wrhdd(lOut,'restfreq',restfreq)
      call wrhdr(lOut,'epoch',epoch)
      call wrhda(lOut,'object',source)
      call wrhda(lOut,'telescop',telescop)
c     
c  Select axis values.
c
      if(xunit.eq.'arcseconds')then
         call wrhda(lOut,'ctype1','RA---SIN')
      else
         call wrhda(lOut,'ctype1','ANGLE')
      endif
      if(yunit.eq.'arcseconds')then
         call wrhda(lOut,'ctype2','DEC--SIN')
      else
         call wrhda(lOut,'ctype2','ANGLE')
      endif
c     
      if(index(linetype,'wide').eq.0)then
         call uvinfo(lIn,'velocity', velocity)
         call wrhda(lOut,'ctype3','VELO-LSR')
         call wrhdd(lOut,'crval3',velocity(1))
         call wrhdd(lOut,'cdelt3',velocity(2)-velocity(1))
      else
         call uvrdvrd(lIn,'wfreq',wfreq,0.d0)
         call uvrdvrd(lIn,'wwidth',wwidth,1.d0)
         call wrhda(lOut,'ctype3','FREQUENCY')
         call wrhdd(lOut,'crval3',wfreq)
         call wrhdd(lOut,'cdelt3',wwidth)
      endif
c     
      end
c********1*********2*********3*********4*********5*********6*********7**
      subroutine putimage(lOut,nxy,nz,nsize,array)
      implicit none
      integer lOut,nsize(3), nxy,nz
c @TODO:   this ought to be MAXSIZE,MAXSIZE,MAXCHAN2
      real array(nxy,nxy,nz)
c     Inputs:
c     lOut	The handle of the output image.
c     nsize	The output image size.
c     array	image values.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real row(MAXDIM)
      logical flags(MAXDIM)
      integer i,j,imin,imax,jmin,jmax,ioff,joff,k
c     
      imin = 1
      imax = nsize(1)
      ioff = 0
      jmin = 1
      jmax = nsize(2)
      joff = 0
c     
c  write out image.
c
      do k=1,nsize(3)
         call xysetpl(lOut,1,k)
         do j=1,nsize(2)
            if((j .ge. jmin) .and. (j .le. jmax)) then
               do i=1,nsize(1)
                  row(i)=array(i-ioff,j-joff,k)
                  if (row(i).eq.0.0) then
                     flags(i) = .FALSE.
                  else
                     flags(i) = .TRUE.
                  endif
               enddo
            endif
            call xywrite(lOut,j,row)
            call xyflgwr(lOut,j,flags)
	 enddo
      enddo
      end
c********1*********2*********3*********4*********5*********6*********7**
      subroutine GetOpt(sum,debug,taper1,edge,taper2,imap,systemp,
     *                  jyperk,do0)
      implicit none
      logical sum,debug,taper1,edge,taper2,imap,systemp,jyperk,do0
c     
c  Determine extra processing options.
c
c  Output:
c    sum      Sum the data in each pixel. Default is to average.
c    debug    More debug output
c------------------------------------------------------------------------
      integer nopt
      parameter(nopt=9)
      character opts(nopt)*8
      logical present(nopt)
      data opts/'sum     ',
     *          'debug   ',
     *          'taper1  ',
     *          'taper2  ',
     *          'edge    ',
     *          'inttime ',
     *          'systemp ',
     *          'jyperk  ',
     *          'none    '/
c     
      call options('options',opts,present,nopt)
      sum     = present(1)
      debug   = present(2)
      taper1  = present(3)
      taper2  = present(4)
      edge    = present(5)
      imap    = present(6)
      systemp = present(7)
      jyperk  = present(8)
      do0     = present(9)
c     
      end
c********1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE units(d,unit)
      implicit none
      real d
      character unit*(*)
c
c The following units are understood and converted to:
c
c   arcmin, arcsec, degrees, hours, radians
c
c------------------------------------------------------------------------
      include 'mirconst.h'
c
      IF(unit.EQ.'radians'.OR.unit.eq.' ') THEN
         continue
      ELSE IF(unit.EQ.'arcminutes') THEN
         d = d * DPI / (180d0 * 60d0)
      ELSE IF(unit.EQ.'arcseconds') THEN
         d = d * DPI / (180d0 * 3600d0)
      ELSE IF(unit.EQ.'degrees') THEN
         d = d * DPI / 180d0
      ELSE IF(unit.EQ.'hours') THEN
         d = d * DPI / 12.d0
      ELSE
         call bug('w','Unrecognised units, in UNITS')
      ENDIF

      END
c********1*********2*********3*********4*********5*********6*********7**
      LOGICAL FUNCTION masktest(mdim, mask, i0, j0)
      implicit none
      integer mdim, i0, j0
      logical mask(mdim,mdim)
c
      integer n,id,jd, i1,j1


      masktest = .FALSE.
      if (.not.mask(i0,j0)) return

c+
c      masktest = .TRUE.
c      if(n.eq.0) return
c-

      n = 0
      do jd=-1,1
         j1 = j0+jd
         do id=-1,1
            i1=i0+id
            if (.not.mask(i1,j1)) n = n + 1
         end do
      end do

c   if there were any masked pixels, we're at an edge
      if (n.gt.0) masktest = .TRUE.

      return
      end
c-----------------------------------------------------------------------
      subroutine getwtsys(lIn,mant,tsys)
      implicit none
      integer lIn, mant
      real tsys(mant)
c
      include 'maxdim.h'
c
      logical updated
      integer length,nants,nchan,nspect,i,j,i2
      integer nschan(MAXWIN),ischan(MAXWIN)
      real systemp(MAXWIN*MAXANT)
      character type*1

      call uvprobvr(lIn,'systemp',type,length,updated)
      if (updated .and. length.gt.0) then
         call uvgetvri(lIn,'nants',nants,1)
         call uvgetvri(lIn,'nspect',nspect,1)
         call uvgetvri(lIn,'nchan',nchan,1)
         call uvgetvri(lIn,'nschan',nschan,nspect)
         call uvgetvri(lIn,'ischan',ischan,nspect)
         call uvgetvrr(lIn,'systemp',systemp,nants*nspect)
         do i=1,nants
            tsys(i) = systemp(i)
         end do
      end if

      end
c-----------------------------------------------------------------------
