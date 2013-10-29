c************************************************************************
	program histo
	implicit none
c
c= histo - Find statistics of image and plot simple histogram
c& mchw
c: map analysis
c+
c	HISTO is a Miriad task which finds the minimum, maximum, mean and
c	rms deviation from the mean for the selected region of the image.
c	It also displays a simple histogram which is useful for determining
c	the clip levels for clean and selfcal.
c       If the beam is known, the beam area (pi/4/ln2*bmaj*bmin) is used
c       to convert any JY/BEAM to a total flux in JY.
c       See also:  IMHIST, IMSTAT
c@ in
c	The input file name. No default.
c@ region
c	The region of interest of the input. Default is the entire input.
c	See the Users Manual for instructions on how to specify this.
c	Pixel blanking is not supported; all pixels in the selected region
c	are used.
c@ range
c	The range in pixel values over which the histogram is calculated.\
c       The minmax, mean and dispersion are now also computed in this range. 
c	The default is the image minima and maxima.
c@ nbin
c	The number of bins used in the histogram. Default is 16.
c--
c
c  History:
c    03nov89 mchw  changed 'All pixels constant' to Fortran stop 
c    30apr90 rjs   Changed call sequence to BoxInput.
c    26may90 mchw  changed rms to rms deviation from the mean.
c    10apr92 nebk  Add flux density to output
c     1may92 rjs   Increase maxruns. Various trivial other mods.
c     2mar93 rjs   Use maxnax.h.
c    19nov93 rjs   Better summary of flux or sum.
c    31jan96 nebk  More grace when no valid pixels in region.  
c    08oct96 nebk  Make accumulation sums double precision
c    28feb97 nebk  Add object to output
c    14may99 rjs   Increase MAXRUNS.
c    15feb01 pjt   look within range to find min/max/mean/dispersion
c    21oct03 pjt   check for JY/BEAM in the first 7 chars only
c    14jun11 pjt   add SPITZER MJy/sr check
c    15oct11 pjt   merged in '02jan05 rjs ' 
c    01dec11 pkgw  Increase NBINMAX from 40 to 2048
c    29oct13 pjt   Fix summing when range is selected
c
c @todo
c    is converting real to integer of  indx =   ok? or use RINT?
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	integer NBINDEF,NBINMAX,MAXBOXES,MAXRUNS
	character version*(*)
	parameter(NBINDEF=16,NBINMAX=2048,MAXBOXES=2048)
	parameter(MAXRUNS=100*MAXDIM)
	parameter(VERSION = 'version 29-oct-2013 PJT' )
c
	character file*128,asterisk*30,line*128,coord*64,bunit*32,
     +   object*32
	integer nsize(MAXNAX),plane(MAXNAX),maxv(MAXNAX),minv(MAXNAX)
	integer blc(MAXNAX),trc(MAXNAX)
	integer i,j,k,under,over,bin(NBINMAX),maxbin
	integer naxis,indx,lun,npoints,length,nbin
	integer boxes(MAXBOXES),runs(3,MAXRUNS),nruns
	real dat(MAXDIM),rmax,rmin,blo,bhi,x,xinc,r
	real bscale,bmaj,bmin,barea,cdelt1,cdelt2
        double precision sum0,sum1,sum2,av,rms
	logical first, done, newmin, newmax, norange,flg(MAXDIM)
c
c  Externals.
c
	logical keyprsnt
	character itoaf*10
	integer len1
c
c  Open the input file, and make sure that I can handle it.
c
	call output( 'Histo: '//version )
	call keyini
	call keya('in',file,' ')
	if(file.eq.' ')call bug('f','Input file must be given')
	call BoxInput('region',file,boxes,MAXBOXES)
	norange = .not.keyprsnt('range')
        call keyr('range',blo,0.0)
        call keyr('range',bhi,blo)
        call keyi('nbin',nbin,nbindef)
        if(nbin.le.1 .or. nbin.gt.NBINMAX)
     *	  call bug('f','Bad number of bins')
	call keyfin
c
	call xyopen(lun,file,'old',MAXNAX,nsize)
	call rdhdi(lun,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
	if(nsize(1).gt.MAXDIM)call bug('f','Input file too big for me')
        call rdhda(lun,'object',object,' ')

c
c  Determine the min and max value, if needed.
c
        if(norange.or.bhi.le.blo)
     *	  call ImMinMax(lun,naxis,nsize,blo,bhi)
	if(blo.eq.bhi)then
	  call xyclose(lun)
	  write(line,'(''All pixels are '',1pg10.3)')blo
	  call output(line)
	  stop
	endif
	bscale = nbin/(bhi-blo)
c
c  Set up the region of interest.
c
	call BoxMask(lun,boxes,MAXBOXES)
	call BoxSet(boxes,MAXNAX,nsize,' ')
	call BoxInfo(boxes,MAXNAX,blc,trc)
c
c  Initialise.
c
	over = 0
	under = 0
	sum0=0.
	sum1=0.
	sum2=0.
	first = .true.
	done = .false.
	npoints = 0
c
	do i=1,nbin
	  bin(i) = 0
	enddo
c
	do i=1,MAXNAX
	  plane(i) = blc(i)
	enddo
c
c  Calculate the histogram.
c
	dowhile(.not.done)
	  call BoxRuns(MAXNAX-2,plane(3),' ',boxes,runs,maxruns,nruns,
     *					blc(1),trc(1),blc(2),trc(2))
	  call xysetpl(lun,MAXNAX-2,plane(3))
	  newmin = .false.
	  newmax = .false.
	  j = 0
	  do k=1,nruns
	    if(Runs(1,k).ne.j)then
	      j = Runs(1,k)
	      call xyread(lun,j,Dat)
	    endif
c
	    if(first)then
	      rmax = dat(Runs(2,k))
	      rmin = rmax
	      maxv(1) = Runs(2,k)
	      minv(1) = Runs(2,k)
	      maxv(2) = Runs(1,k)
	      minv(2) = Runs(1,k)
	      newmin = .true.
	      newmax = .true.
	      first = .false.
	    endif
c
	    npoints = npoints + Runs(3,k) - Runs(2,k) + 1
	    do i=Runs(2,k),Runs(3,k)
	      x = dat(i)
	      if(x.gt.bhi)then
	        over = over + 1
	      else if(x.lt.blo)then
	        under = under + 1
	      else
	        indx = bscale * (x-blo) + 1
	        indx = max(min(nbin,indx),1)
	        bin(indx) = bin(indx) + 1
	      endif
	      if(x.ge.blo .and. x.le.bhi) then
		if(x.gt.rmax) then
		  rmax = x
		  maxv(1) = i
		  maxv(2) = j
		  newmax = .true.
	        else if(x.lt.rmin) then
		  rmin = x
		  minv(1) = i
		  minv(2) = j
		  newmin = .true.
	        endif
		sum0 = sum0 + 1.0
		sum1 = sum1 + x
		sum2 = sum2 + x*x
	      endif
	    enddo
	  enddo
	  if(newmax)call copyindx(MAXNAX-2,plane(3),maxv(3))
	  if(newmin)call copyindx(MAXNAX-2,plane(3),minv(3))
	  call planeinc(MAXNAX-2,blc(3),trc(3),plane(3),done)
	enddo
c
c  Determine average, rms etc.
c
        if (npoints.gt.0) then
  	  av = sum1/sum0
  	  rms = sqrt(abs(sum2/sum0-av*av))
        else
          call bug ('f', 'No valid pixels in selected region')
        end if
c
        call rdhdr(lun,'bmaj',bmaj,0.0)
        call rdhdr(lun,'bmin',bmin,0.0)
        call rdhdr(lun,'cdelt1',cdelt1,0.0)
        call rdhdr(lun,'cdelt2',cdelt2,0.0)
        call rdhda(lun,'bunit',bunit,' ')
c       beam area = pi/4/ln(2)*bmaj*bmin
        barea = 0.0
        if (cdelt1*cdelt2.ne.0.0) 
     *    barea = 1.1331 * bmaj * bmin / abs(cdelt1*cdelt2)
c
        if (object.ne.' ') then
          line = 'Object  '//object
          call output (line)
        end if

        if (barea.gt.0.0 .and. bunit(1:7).eq.'JY/BEAM') then
          write(line,100) av,rms,sum1/barea
        else if (bunit.eq.'MJy/sr') then
          write(line,100) av,rms,sum1*abs(cdelt1*cdelt2)*1e6
	else if (bunit.eq.'JY/PIXEL') then
	  write(line,100) av,rms,sum1
        else
          write(line,101)av,rms,sum1
        end if
 100    format('Mean', 1pe13.5,' Rms',1pe13.5,' Flux', 1pe13.5, ' Jy')
 101	format('Mean', 1pe14.6,' Rms',1pe15.5,' Sum ', 1pe13.5)
	k = len(line)
	j = min(len1(line),k-1)
	line(j+1:k) = ' ('//itoaf(npoints)
	j = min(len1(line),k-1)
	line(j+1:k) = ' points)'
	call output(line)
c
	call getcoord(maxv,naxis,coord,length)
	write(line,102)rmax,coord(1:length)
 102	format('Maximum value', 1pe18.6, 4X, 'at ',a)
	call output(line)
c
	call getcoord(minv,naxis,coord,length)
	write(line,103)rmin,coord(1:length)
 103	format('Minimum value', 1pe18.6, 4X, 'at ',a)
	call output(line)
c
	write(line,104)
 104	format('  Bin    Value          Number')
	call output(' ')
	call output(line)
c
c  Determine the max number of counts in a bin.
c
	maxbin = 0
	do i=1,nbin
	  maxbin = max(maxbin,bin(i))
	enddo
	xinc = (bhi - blo)/nbin
	x = blo
	if(maxbin.gt.0)then
	  r = 29./real(maxbin)
	else
	  r = 1
	endif
c
c  Format histogram.
c
	asterisk = '******************************'
	write(line,'(7x,a,3x,i10)')'Underflow',under
	call output(line)
	do i=1,nbin
	  j = nint( r * bin(i) )+1
	  write(line,200)i,x,bin(i),asterisk(1:j)
  200	  format(i5,1x,1pe13.6,i10,1x,a)
	  call output(line)
	  x = x + xinc
	enddo
	write(line,'(7x,a,4x,i10)')'Overflow',over
	call output(line)
c
c  Thats all folks. Close up and go home.
c
	call xyclose(lun)
	end
c************************************************************************
	subroutine getcoord(indx,n,line,length)
c
	implicit none
	integer n,indx(n),length
	character line*(*)
c
c  Encode a pixel index into ascii. Format it in a neat way.
c
c------------------------------------------------------------------------
	integer maxl
	parameter(maxl=8)
	character txt*(maxl)
	integer i,l
c
	length = 1
	line(1:1) = '('
	do i=1,n
	  write(txt,'(i8)')indx(i)
	  l = 1
	  dowhile(txt(l:l).eq.' ')
	    l = l + 1
	  enddo
	  if(length + maxl - l + 2.gt.len(line))
     *	    call bug('f','Text buffer overflow')
	  line(length+1:length+maxl-l+1) = txt(l:maxl)
	  length = length + maxl - l + 1
	  line(length+1:length+1) = ','
	  length = length + 1
	enddo
	line(length:length) = ')'
	end
c************************************************************************
	subroutine copyindx(n,from,to)
c
	implicit none
	integer n,from(n),to(n)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  to(i) = from(i)
	enddo
	end
c************************************************************************
	subroutine planeinc(n,blc,trc,plane,done)
c
	implicit none
	integer n,blc(n),trc(n),plane(n)
	logical done
c
c  Move to the next plane.
c
c------------------------------------------------------------------------
	integer k
c
	k = 1
	done = .true.
c
	do while(done.and.k.le.n)
	  done = plane(k).ge.trc(k)
	  if(done)then
	    plane(k) = blc(k)
	  else
	    plane(k) = plane(k) + 1
	  endif
	  k = k + 1
	enddo
	end
