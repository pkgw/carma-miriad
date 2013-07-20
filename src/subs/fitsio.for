c***********************************************************************
c  A collection of routines to manipulate FITS files.
c
c  Basic FITS Data I/O:
c  ====================
c
c  FXYOPEN      - Open or create a file.
c  FXYCLOSE     - Close up shop.
c  FXYREAD,FXYWRITE - Perform i/o on a row of data. For old files this
c                 can be random (sequential is more efficient though)
c                 For new files it must be sequential.
c  FXYFLGRD,FXYFLGWR - Read and write pixel blanking info.
c  FXYSETPL     - Set the plane to access.
c
c  FUVOPEN      - Open FITS uv file.
c  FUVCLOSE     - Close FITS uv file.
c  FUVREAD/FUVWRITE - Read/write uv data.
c  FUVRDHD      - Read some header info.
c  FUVWRHD      - Writes some header info.
c
c  Manipulation of FITS tables.
c  ============================
c  FTABLOC      - Locate a particular FITS table.
c  FTABINFO     - Information about values in the current table.
c  FTABGETI     - Get integer data from the table.
c  FTABGETR     - Get real data from the table.
c  FTABGETC     - Get complex data from the table.
c  FTABGETD     - Get double precision data from the table.
c  FTABGETA     - Get ascii data from the table.
c
c  Manipulation of FITS headers.
c  =============================
c  FITRHHDA     - Locate and decode ascii keyword for old file.
c  FITRDHDI     - Locate and decode integer keyword for old file.
c  FITRDHDR     - Locate and decode real keyword for old file.
c  FITRDHDL     - Locate and decode logical keyword for old file.
c
c  FITWRHDA     - Write an ASCII valued keyword for a new file.
c  FITWRHDI     - Write an integer keyword for a new file.
c  FITWRHDR     - Write a real keyword for a new file.
c  FITWRHDL     - Write a logical keyword for a new file.
c  FITWRHDH     - Write a valueless or comment-like keyword.
c
c  Internal FITS header handling routines
c  ======================================
c  FITOPEN      - Opens or creates a FITS file. Does some checks.
c  FITCLOSE     - Closes a FITS file.
c  FITBLANK     - Set/return blanking mode.
c
c  FITHDINI     - Initialise reading of an old header.
c  FITHDFIN     - Complete a new header.
c  FITSRCH      - Searches for a keyword in the header of an old file.
c  FITCDIO      - Reads/writes a card to header (sequential). For old
c                 files this must not be called after row i/o starts.
c                 Can be called at any time for new files.
c
c  Parameters - FITS Handling.
c  ===========================
c  maxnax       For images, this gives the max number of dimensions. So
c               of the code will not work if this is more than 9.
c  dBypPix      The default number of bytes per pixel when creating an
c               output FITS file. Much code implicitly assumes its 4.
c  maxcards     Size of card cache.
c  maxsize      Size of array, used to temporarily buffer data.
c  maxcol       Max. number of columns in a table.
c  maxidx       Max number of tables that are remembered.
c
c  Variables - FITS handling.
c  ==========================
c  item         I/O system handle.
c  new          True if file is new.
c  BypPix       Number of bytes per pixel (either 2 or 4 is supported).
c  ncards       The number of the last card to be read or written (cards
c               are 80 bytes long).
c  carray       This is used as a cache of header cards, and other
c               miscellaneous functions.
c  curcard      This gives the number of the first card found in the
c               card cache.
c  curlu        This gives the index of the file which currently has
c               cards in the card cache.  -1 indicates that the cache is
c               invalid.
c  DatBase      Offset in bytes of the data of interest.  For images,
c               this points to the current plane of interest.  For uv
c               data, this points to the first data byte.
c
c  Variables - Common to Map and UV Files.
c  =======================================
c  bscale       Fits data scale parameters.  Often BZERO is zero, so
c  bzero        special code often optimises this case.
c
c  Variables - Maps Only.
c  ======================
c  naxis(maxnax)Fits parameters, NAXIS1 to NAXIS7.
c
c  Variables - UV Data Only.
c  =========================
c  For FITS random files, the random parameters, such as U are given
c  as two numbers, which are independently scaled. To get the actual
c  number, we add the two components together after scaling, viz:
c               U =   scales1(u)*data(indices1(u))
c                   + scales2(u)*data(indices2(u)) + zeros(u)
c  In reality, either one or both of these might be missing, in
c  which case this routine returns a zero instead. This is probably
c  OK for most practical cases, though no good if either U or V is
c  missing.
c
c  The order of the random parameters in INDICES, SCALES and ZEROS
c  is hardcoded to be u1,u2,v1,v2,w1,w2,baseline1,baseline2,date1,date2,
c  in that order. Output to the caller is similarly hard coded.
c
c  indices1     indices(i) points to the random parameter corresponding
c  indices2     to the i'th in the list (u,v,w,bl,t).  Zero, one or two
c               occurrences of a random parameter are correctly handled.
c
c  scales1      scales(i) is the scale factor for the first random u
c  scales2      parameter. Similarly for the others.
c
c  zeros        Similarly.
c  nRanFile     No. of random parameters in the disk file.
c  nRanProg     No. of random parameters wanted by the programmer.
c  ncomplex     No. of elements per correlation in the disk file.
c               Either 2 or 3 (real, image and possibly weight).
c  visibs       No. visibilities.
c  pols         No. polarizations.
c  freqs        No. frequency channels.
c
c  Bugs and Shortcomings:
c    * IF frequency axis is not handled on output of uv data.
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c    mhw  02jul12  Read/write of images with dimensions up to MAXDIM
c    rjs  16feb13  Support really large FITS files (>200GB)
c
c $Id$
c***********************************************************************

c* FxyOpen -- Open a FITS image file.
c& rjs
c: fits
c+
      subroutine fxyopen(lu,name,status,naxis,nsize)

      integer lu,naxis,nsize(naxis)
      character name*(*),status*(*)
c  ---------------------------------------------------------------------
c  Open a FITS image file and ready it for use.
c
c  Inputs:
c    name       Name of the map to be opened.
c    status     Either 'old' or 'new', depending whether an old image
c               is being read, or a new image is being written.
c    naxis      Dimension of nsize.
c
c  Input/Output:
c    The following are input if creating a new image (i.e.
c    status='new'), or output when reading an old image.
c
c    nsize      An array containing dimension information. For an old
c               file, this is filled with the values of NAXIS1,
c               NAXIS2, ...  If there are fewer than NAXIS dimensions,
c               then it is filled with ones.
c
c  Output:
c    lu         File descriptor of the image opened.
c-----------------------------------------------------------------------
      integer bitpix,i,temp,ndim,Bytes
      integer size3(3)
      logical dofloat
      include 'fitsio.h'

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
c  General checking.
c
      if (naxis.gt.maxnax) call bug('f','Too many dimensions')
c
c  Handle the old file.
c
      if (status.ne.'new') then
        call fitopen(lu,name,status)
        call fitrdhdi(lu,'BITPIX', bitpix,0)
        if (bitpix.ne.16 .and. abs(bitpix).ne.32) then
           if (bitpix.eq.-64) then
              call bug('w','Losing precision for BITPIX=-64')
           else
              call bug('f','Unsupported value for BITPIX')
           endif
        endif
        Bytes = abs(BitPix)/8
        dofloat = BitPix.lt.0
        call fitrdhdi(lu,'NAXIS',ndim,0)
        if (ndim.le.0 .or. ndim.gt.maxnax)
     *                call bug('f','Too many dims for me')

        do i = 1,max(naxis,ndim)
          if (i.le.ndim) then
            call fitrdhdi(lu,'NAXIS'//itoaf(i),temp,1)
            if (temp.le.0) call bug('f','File is not an image')
          else
            temp = 1
          endif
          if (i.le.naxis) then
            nsize(i) = temp
          else if (temp.gt.1) then
            call bug('f','Too many dims for this application')
          endif
        enddo
        call mpSet(PixBase3(1,lu),DatBase3(1,lu))
c         ... PixBase(lu) = DatBase(lu)
        call fitrdhdr(lu,'BSCALE', bscale(lu),1.0)
        call fitrdhdr(lu,'BZERO',  bzero(lu), 0.0)
c
c  Handle the new case.
c
      else
        ndim = naxis
        Bytes = abs(dBypPix)
        call mpCvtim(size3,Bytes)
c         ... size = Bytes
        do i = 1, ndim
          call mpMulmi(size3,nsize(i))
c           ... size = size * nsize(i)
        enddo
        call fitopen(lu,name,status)
        call fitsize(lu,size3)
        dofloat = dBypPix.lt.0
        call fitwrhdi(lu,'BITPIX',8*dBypPix)
        call fitwrhdi(lu,'NAXIS',naxis)
        do i = 1, naxis
          call fitwrhdi(lu,'NAXIS'//itoaf(i),nsize(i))
        enddo
        call fitwrhdl(lu,'EXTEND',.true.)
        call fitwrhdr(lu,'BSCALE', 1.0)
        call fitwrhdr(lu,'BZERO',  0.0)
        bscale(lu) = 1.0
        bzero(lu)  = 0.0
      endif
c
c  Fill in some info.
c
      float(lu) = dofloat
      BypPix(lu) = Bytes
      do i = 1, naxis
        axes(i,lu) = nsize(i)
      enddo
      do i = naxis+1, maxnax
        axes(i,lu) = 1
      enddo

      end

c***********************************************************************

c* FxySetpl -- Select the plane of interest in a FITS image.
c& rjs
c: fits
c+
      subroutine fxysetpl(lu,naxis,nsize)

      integer lu,naxis,nsize(naxis)
c  ---------------------------------------------------------------------
c  This sets the plane of a multi-plane image that we wish to access.
c
c  Inputs:
c    lu         File descriptor.
c    naxis      Dimension of nsize.
c    nsize      Array containing the index of the plane to be accessed.
c               NSIZE(1) corresponds to the index into the third
c               dimension.
c-----------------------------------------------------------------------
      integer i
      integer size3(3)
      include 'fitsio.h'

      external mpSign
      integer  mpSign
c-----------------------------------------------------------------------
c
c  Finish the header of a new file.
c
      if (mpSign(DatBase3(1,lu)).eq.0) call fithdfin(lu)

      if (naxis+2.gt.maxnax) call bug('f','Too many dims in FXYSETPL')
      call mpCvtim(size3,0)
      do i = naxis,1,-1
        if (nsize(i).lt.1 .or. nsize(i).gt.axes(i+2,lu))
     *        call bug('f','Dimension error in FXYSETPL')
        call mpAddmi(size3,nsize(i)-1)
        call mpMulmi(size3,axes(i+1,lu))
c         .. size = ( size + nsize(i) - 1 ) * axes(i+1,lu)
      enddo
      call mpMulmi(size3,axes(1,lu)*BypPix(lu))
      call mpAddmm(size3,DatBase3(1,lu))
      call mpSet(PixBase3(1,lu),size3)
c       ... PixBase(lu) = BypPix(lu) * size * axes(1,lu) + DatBase(lu)

      end

c***********************************************************************

c* FitBlank -- Set and check the FITS blank mode.
c& rjs
c: fits
c+
      logical function FitBlank(lu,mode)

      integer lu
      logical mode
c  ---------------------------------------------------------------------
c  FitFlg returns the current blanking mode.  If .true., this indicates
c  that the FITS file (either input or output) could contain blanked
c  pixels or correlations.
c
c  For a new file, the "mode" argument can be used to indicate whether
c  the output could contain blanked values.  To set a value as blanked,
c  you must call the appropriate "flgwr" routine.  If you are going to
c  call the flgwr routines, you must indicate that blanking is to be
c  handled before any data is written.
c
c  For old files, the "mode" argument is ignored.
c
c  Input:
c    mode
c  NOTE: This assumes that floating point pixels are being written!!!
c-----------------------------------------------------------------------
      include 'fitsio.h'
c-----------------------------------------------------------------------
      if (new(lu) .and. BlankVal(lu).eq.0 .and. mode) then
        call fitwrhdi(lu,'BLANK',-1)
        BlankVal(lu) = -1
      endif

      FitBlank = BlankVal(lu).ne.0

      end

c***********************************************************************

c* FxyRead -- Read a row of data from a FITS image.
c& rjs
c: fits
c+
      subroutine fxyread(lu,indx,data)

      integer lu,indx
      real data(*)
c  ---------------------------------------------------------------------
c  Read a row of data from a FITS image.
c
c  Input:
c    lu         File descriptor.
c    indx       Index of the row to access.
c  Output:
c    data       Array containing the pixel info.
c-----------------------------------------------------------------------
      integer i,length,iostat,blank
      integer offset3(3),off3(3)
      real bs,bz
      include 'fitsio.h'
c-----------------------------------------------------------------------
c  Check that it is the right sort of operation for this file.
c
      if (new(lu)) call bug('f','Cannot read new FITS file')
      if (axes(1,lu).gt.MAXSIZE)
     *  call bug('f','First dimension too big, in FXYREAD')
c
c  Copy the data, doing the conversion on the way.  Optimise for the
c  case where BZERO is 0.
c
      length = axes(1,lu)
      call mpSet(offset3,PixBase3(1,lu))
c      call mpAddmi(offset3,BypPix(lu)*(indx-1)*length)
      call mpCvtim(off3,length)
      call mpMulmi(off3,BypPix(lu)*(indx-1))
      call mpAddmm(offset3,off3)
      bs = bscale(lu)
      bz = bzero(lu)
      blank = BlankVal(lu)
c
c  Do the floating point case. Blank the data if needed.
c
      if (float(lu)) then
        if (BypPix(lu).eq.4) then
          call hread3r(item(lu),data,offset3,BypPix(lu)*length,iostat)
          if (iostat.ne.0) call bugno('f',iostat)
          call hread3i(item(lu), array,offset3,BypPix(lu)*length,
     *                                                      iostat)
          if (iostat.ne.0) call bugno('f',iostat)
          call fnanflag(data,array,length)
        else
          call hread3d(item(lu),darray,offset3,BypPix(lu)*length,
     *                                                      iostat)
         if (iostat.ne.0) call bugno('f',iostat)
          do i = 1, length
             data(i) = darray(i)
          enddo
        endif
c
c  Scale if need be.
c
        if (bs.ne.1 .or. bz.ne.0) then
          do i = 1, length
            data(i) = bs * data(i) + bz
          enddo
        endif
c
c  Do the scaled integer case. Blank if needed.
c
      else
        if (BypPix(lu).eq.2) then
          call hread3j(item(lu),array,offset3,BypPix(lu)*length,
     *                                                        iostat)
        else
          call hread3i(item(lu),array,offset3,BypPix(lu)*length,
     *                                                        iostat)
        endif
        if (iostat.ne.0) call bugno('f',iostat)
        if (bz.eq.0) then
          do i = 1, length
            data(i) = bs * array(i)
          enddo
        else
          do i = 1, length
            data(i) = bs * array(i) + bz
          enddo
        endif

        if (blank.ne.0) then
          do i = 1, length
            if (array(i).eq.blank) data(i) = 0
          enddo
        endif

      endif

      end

c***********************************************************************

c* FxyFlgRd -- Read a row of pixel flags from a FITS image.
c& rjs
c: fits
c+
      subroutine fxyflgrd(lu,indx,flags)

      integer lu,indx
      logical flags(*)
c  ---------------------------------------------------------------------
c  Read a row of pixel flags from a FITS image.
c
c  Input:
c    lu         File descriptor.
c    indx       Index of the row to access.
c  Output:
c    flags      Output pixel flags.
c-----------------------------------------------------------------------
      integer i,length,iostat,blank
      integer offset3(3),off3(3)
      include 'fitsio.h'
c-----------------------------------------------------------------------
c
c  Check that it is the right sort of operation for this file.
c
      if (new(lu)) call bug('f','Cannot read new FITS file')
      if (axes(1,lu).gt.MAXSIZE)
     *  call bug('f','First dimension too big, in FITXYFLGRD')
c
c  Initialise.
c
      length = axes(1,lu)
      call mpSet(offset3,PixBase3(1,lu))
c      call mpAddmi(offset3,BypPix(lu)*(indx-1)*length)
      call mpCvtim(off3,length)
      call mpMulmi(off3,BypPix(lu)*(indx-1))
      call mpAddmm(offset3,off3)
      blank = BlankVal(lu)
c
c  If there was no BLANK keyword, assume all the data are good.
c
      if (blank.eq.0) then
        do i = 1, length
          flags(i) = .true.
        enddo
c
c  Otherwise reread the data, and determine the flagged values.
c
      else
        if (BypPix(lu).eq.2) then
          call hread3j(item(lu),array,offset3,BypPix(lu)*length,
     *                                                        iostat)
        else if (BypPix(lu).eq.4) then
          call hread3i(item(lu),array,offset3,BypPix(lu)*length,
     *                                                        iostat)
        else if (BypPix(lu).eq.8) then
          call hread3d(item(lu),darray,offset3,BypPix(lu)*length,
     *                                                        iostat)
        endif
        if (iostat.ne.0) call bugno('f',iostat)
        if (float(lu).and.BypPix(lu).eq.4) then
          do i = 1, length
            flags(i) = (2139095040.gt.array(i) .or.
     *                  array(i).gt.2147483647) .and.
     *                 (-8388608.gt.array(i) .or.
     *                  array(i).gt.-1)
          enddo
        else if (BypPix(lu).eq.8) then
          do i = 1, length
            flags(i) = darray(i).ne.blank
          enddo
        else
          do i = 1, length
            flags(i) = array(i).ne.blank
          enddo
        endif
      endif

      end

c***********************************************************************

c* FxyWrite -- Write a row of a FITS image.
c& rjs
c: fits
c+
      subroutine fxywrite(lu,indx,data)

      integer lu,indx
      real data(*)
c  ---------------------------------------------------------------------
c  Write a row of data to the image file.
c
c  Input:
c    lu         File descriptor.
c    indx       Index of the row to access.
c    data       Array containing the pixel info.
c
c  NOTE: This assumes that float(lu), bscale(lu) and bzero(lu) are
c        .true., 1.0 and 0.0 respectively!  THIS ASSUMPTION depends on
c        the code in fxyopen!
c-----------------------------------------------------------------------
      integer iostat
      integer offset3(3),off3(3)
      include 'fitsio.h'

      external mpSign
      integer  mpSign
c-----------------------------------------------------------------------
c  Check that it is the right sort of operation for this file.
c
      if (.not.new(lu)) call bug('f','Cannot write old FITS file')
c
c  If it's a new file, and this is the first call to perform data i/o on
c  it (not header i/o), handle the header properly.
c
      if (mpSign(DatBase3(1,lu)).eq.0) then
        call fithdfin(lu)
        call mpSet(PixBase3(1,lu),DatBase3(1,lu))
      endif
c
c  This assumes that we have floating point pixels.
c
      call mpSet(offset3,PixBase3(1,lu))
c      call mpAddmi(offset3,BypPix(lu)*(indx-1)*axes(1,lu))
      call mpCvtim(off3,axes(1,lu))
      call mpMulmi(off3,BypPix(lu)*(indx-1))
      call mpAddmm(offset3,off3)
      call hwrite3r(item(lu),data,offset3,BypPix(lu)*axes(1,lu),
     *                                                        iostat)
      if (iostat.ne.0) call bugno('f',iostat)

      end

c***********************************************************************

c* FxyFlgWr -- Write flags for a row of a FITS image.
c& rjs
c: fits
c+
      subroutine fxyflgwr(lu,indx,flags)

      integer lu,indx
      logical flags(*)
c  ---------------------------------------------------------------------
c  Write a row of pixel flags for a FITS image.
c
c  Input:
c    lu         File descriptor.
c    indx       Index of the row to access.
c    flags      The pixel flags.
c
c  NOTE: This assumes that float(lu), bscale(lu) and bzero(lu) are
c        .true., 1.0 and 0.0 respectively!  THIS ASSUMPTION depends on
c        the code in fxyopen!
c-----------------------------------------------------------------------
      integer iostat,k,kmax,l,lmax,blank,i
      integer offset3(3),off3(3)
      include 'fitsio.h'

      external isrchl, mpSign
      integer  isrchl, mpSign
c-----------------------------------------------------------------------
c  Check that it is the right sort of operation for this file.
c
      if (.not.new(lu)) call bug('f','Cannot write old FITS file')
      if (BlankVal(lu).eq.0)
     *  call bug('f','FXYFLG must be falled before FXYFLGWR')
c
c  If it's a new file, and this is the first call to perform data i/o on
c  it (not header i/o), handle the header properly.
c
      if (mpSign(DatBase3(1,lu)).eq.0) then
        call fithdfin(lu)
        call mpSet(PixBase3(1,lu),DatBase3(1,lu))
      endif
c
c  This assumes that we have floating point pixels.
c
      Blank = BlankVal(lu)
      call mpSet(offset3,PixBase3(1,lu))
c      call mpAddmi(offset3,BypPix(lu)*(indx-1)*axes(1,lu))
      call mpCvtim(off3,axes(1,lu))
      call mpMulmi(off3,BypPix(lu)*(indx-1))
      call mpAddmm(offset3,off3)
c
c  Convert the flags into a run-length encoding, and then write out
c  the magic value blanked version.
c
      lmax = 0
      kmax = axes(1,lu)
      k = isrchl(kmax,flags,.false.)
      do while (k.le.kmax)
        l = isrchl(kmax-k+1,flags(k),.true.) - 1
        if (l.gt.lmax) then
          do i = lmax+1, l
            array(i) = Blank
          enddo
          lmax = l
        endif
        call mpSet(off3,offset3)
        call mpAddmi(off3,BypPix(lu)*(k-1))
        call hwrite3i(item(lu),array,off3,BypPix(lu)*l,iostat)
        if (iostat.ne.0) call bugno('f',iostat)
        k = k + l
        if (k.le.kmax) k = isrchl(kmax-k+1,flags(k),.false.) + k - 1
      enddo

      end

c***********************************************************************

      integer function isrchl(n,array,target)

      integer n
      logical array(n),target
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        if (array(i).eqv.target) goto 200
      enddo
 200  isrchl = i

      end

c***********************************************************************

c* FxyClose -- Close a FITS image file.
c& rjs
c: fits
c+
      subroutine fxyclose(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Close up a fits file. Good night.
c
c  Input:
c    lu         File descriptor.
c-----------------------------------------------------------------------
      call fitclose(lu)

      end

c***********************************************************************

c* FuvOpen -- Open a FITS uv file.
c& rjs
c: fits
c+
      subroutine fuvopen(lu,name,status,nvis,npol,nfreq)

      integer lu,nvis,npol,nfreq
      character name*(*),status*(*)
c  ---------------------------------------------------------------------
c  Open a FITS uv file and ready it for i/o.
c
c  Inputs:
c    name       The name of the file to be opened.
c    status     Either 'old' or 'new'.
c
c  Inputs or Outputs:
c    These are input when status='new', and output when status='old'.
c    nvis       The number of visibilities in the file.
c    npol       The number of polarizations (from 1 to 4).
c    nfreq      The number of frequency channels.
c
c  Output:
c    lu         File descriptor.
c-----------------------------------------------------------------------
      integer bitpix,naxis,n1,Bytes,nProgRan,nFileRan
      integer ipol,ifreq,icmplx,iif,ncmplx,nif,nt1,nt2,nt3
      integer itemp3(3)
      logical groups,dofloat
      include 'fitsio.h'

      external mpCmp
      integer  mpCmp
c-----------------------------------------------------------------------
c  Handle the old file.
c
      if (status.ne.'new') then
        call fitopen(lu,name,status)
c
c  Check all the neccessary keywords are correct.
c
        call fitrdhdi(lu,'BITPIX', bitpix,0)
        if (BitPix.ne.16 .and. abs(BitPix).ne.32)
     *    call bug('f','Unsupported value for Bitpix')
        Bytes = abs(BitPix)/8
        dofloat = BitPix.lt.0
        call fitrdhdi(lu,'NAXIS',  naxis, 0)
        call fitrdhdi(lu,'NAXIS1', n1,0)
        call fitrdhdl(lu,'GROUPS', groups,.false.)
        call fitrdhdi(lu,'GCOUNT', nvis,  0)
        if (naxis.lt.2 .or. n1.ne.0 .or. .not.groups .or. nvis.le.0)
     *    call bug('f','Unknown file format when opening uv file')
c
c  Get the size and indices of the complex, polarization and frequency
c  axes.
c
        nt2 = 1
        nt3 = 1
        call fuvget(lu,naxis,ncmplx,icmplx,npol,ipol,
     *    nfreq,ifreq,nif,iif)
        if (ncmplx.ne.2 .and. ncmplx.ne.3)
     *    call bug('f','Cannot handle this COMPLEX axis')
        if ((icmplx.gt.ipol .and. ipol.gt.0) .or.
     *     (icmplx.gt.ifreq .and. ifreq.gt.0) .or.
     *     (icmplx.gt.iif .and. iif.gt.0))
     *    call bug('f','Cannot handle this ordering or COMPLEX axis')
        if (   (ipol.lt.ifreq .or. ipol*ifreq.eq.0) .and.
     *         (ipol.lt.iif .or. ipol*iif.eq.0)) then
          if (ifreq.lt.iif .or. ifreq*iif.eq.0) then
            nt1 = 1
            nt2 = 1
            nt3 = 1
          else
            nt1 = ncmplx*npol
            nt2 = nif
            nt3 = nfreq
          endif
        else if ((ifreq.lt.ipol .or. ifreq*ipol.eq.0) .and.
     *         (ifreq.lt.iif .or. ifreq*iif.eq.0)) then
          if (ipol.lt.iif .or. ipol*iif.eq.0) then
            nt1 = ncmplx
            nt2 = nfreq
            nt3 = npol
          else
            nt1 = ncmplx
            nt2 = nfreq*nif
            nt3 = npol
          endif
        else
          call bug('f','Cannot handle this ordering of axes')
        endif
        nfreq = nfreq * nif
c
c  Get all of the scale factors associated with the random parameters,
c  and the scaling associated with the data. The FITS file gives u, v,
c  and w in seconds.
c
        call fitrdhdi(lu,'PCOUNT',nFileRan,0)
        if (nFileRan.le.0)
     *    call bug('f','No random parameters available')
        nProgRan = 0
c
c  Check that the file is not shorter than implied.
c
        call mpCvtim(itemp3,nvis)
        call mpMulmi(itemp3,bytes)
        call mpMulmi(itemp3,nFileRan+ncmplx*npol*nfreq)
        if (mpCmp(DatSize3(1,lu),itemp3).lt.0) then
          call bug('f','File size too small in fituvopen')
        endif

        call fitrdhdr(lu,'BSCALE', bscale(lu),1.0)
        call fitrdhdr(lu,'BZERO',  bzero(lu), 0.0)
        if (ncmplx.eq.3) then
          call fuvwt(lu,WtScal(lu))
        else
          WtScal(lu) = 0
        endif
c
c  Handle a new file.
c
      else
        nt1 = 1
        nt2 = 1
        nt3 = 1
        nProgRan = 0
        nFileRan = 0
        ncmplx = 3
        dofloat = dBypPix.lt.0
        Bytes = abs(dBypPix)
        if (nvis.le.0 .or. npol.le.0 .or. npol.gt.4 .or. nfreq.le.0)
     *    call bug('f','Either NVIS, NPOL or NFREQ bad in FUVOPEN')
        call fitopen(lu,name,status)
        call fitwrhdi(lu,'BITPIX', 8*dBypPix)
        call fitwrhdi(lu,'NAXIS',  6)
        call fitwrhdi(lu,'NAXIS1', 0)
        call fitwrhdi(lu,'NAXIS2', ncmplx)
        call fitwrhdi(lu,'NAXIS3', npol)
        call fitwrhdi(lu,'NAXIS4', nfreq)
        call fitwrhdi(lu,'NAXIS5', 1)
        call fitwrhdi(lu,'NAXIS6', 1)
        call fitwrhdl(lu,'EXTEND', .true.)
        call fitwrhdl(lu,'GROUPS', .true.)
        call fitwrhdi(lu,'GCOUNT', nvis)
        call fitwrhdr(lu,'BSCALE', 1.0)
        call fitwrhdr(lu,'BZERO',  0.0)
        WtScal(lu) = 1.0
      endif
c
c  Finish up by saving the appropriate parameters.
c
      nRanFile(lu) = nFileRan
      nRanProg(lu) = nProgRan
      BypPix(lu) = Bytes
      float(lu) = dofloat
      visibs(lu) = nvis
      ncomplex(lu) = ncmplx
      pols(lu)   = npol
      freqs(lu)  = nfreq
      nts1(lu) = nt1
      nts2(lu) = nt2
      nts3(lu) = nt3

      end

c***********************************************************************

c* FuvSetPa -- Set random parameters expected by the FITS uv routines.
c& rjs
c: fits
c+
      subroutine fuvSetPa(lu,nparam,params)

      integer lu,nparam
      character params(*)*(*)
c  ---------------------------------------------------------------------
c  This sets the random parameters that the calling program wants
c  returned.
c
c  Input:
c    lu         Handle of the input uv FITS file.
c    nparam     Number of random parameters to be returned.
c    params     The names of the random parameters.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer size3(3)

      integer snparam
      parameter (snparam=5)
      character sparams(snparam)*8
      data sparams/'UU      ','VV      ','WW      ',
     *             'BASELINE','DATE    '/
c-----------------------------------------------------------------------
c  Remember the number of random parameters.
c
      if (nRanProg(lu).ne.0) call bug('f','Called fuvSetPa twice')
      nRanProg(lu) = nparam
      if (nparam.le.0) nRanProg(lu) = snparam
      if (nRanProg(lu).gt.MAXRAN)
     *  call bug('f','Too many random parameters')
c
c  Now set the ones that we are interested in.
c
      if (new(lu)) then
        nRanFile(lu) = nRanProg(lu)
        call fitwrhdi(lu,'PCOUNT',nRanFile(lu))
        if (nparam.le.0) then
          call fuvWrPa(lu,nRanFile(lu),sparams,TimOff(lu))
        else
          call fuvWrPa(lu,nRanFile(lu),params,TimOff(lu))
        endif
        call mpCvtim(size3,visibs(lu))
        call mpMulmi(size3,BypPix(lu))
        call mpMulmi(size3,
     *        nRanFile(lu)+ncomplex(lu)*pols(lu)*freqs(lu))
c         ... size = BypPix(lu) * Visibs(lu) *
c     *   ... (nRanFile(lu) + ncomplex(lu)*pols(lu)*freqs(lu))
        call fitsize(lu,size3)
      else
        if (nparam.le.0) then
          call fuvRdPa(lu,nRanFile(lu),nRanProg(lu),sparams,
     *        indices1(1,lu),indices2(1,lu),
     *        scales1(1,lu),scales2(1,lu),zeros(1,lu),TimOff(lu))
        else
          call fuvRdPa(lu,nRanFile(lu),nRanProg(lu),params,
     *        indices1(1,lu),indices2(1,lu),
     *        scales1(1,lu),scales2(1,lu),zeros(1,lu),TimOff(lu))
        endif
      endif

      end

c***********************************************************************

      subroutine fuvWrPa(lu,nRanFile,params,TimOff)

      integer lu,nRanFile
      double precision TimOff
      character params(nRanFile)*(*)
c-----------------------------------------------------------------------
c  Write out the description of the random parameters.
c
c  Input:
c    lu
c    nRanFile
c    params
c    TimOff
c-----------------------------------------------------------------------
      integer i
      character num*2

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
      do i = 1, nRanFile
        num = itoaf(i)
        call fitwrhda(lu,'PTYPE'//num,params(i))
        call fitwrhdr(lu,'PSCAL'//num,1.0)
        if (params(i).eq.'DATE') then
          call fitwrhdd(lu,'PZERO'//num,TimOff)
        else
          call fitwrhdr(lu,'PZERO'//num,0.0)
        endif
      enddo

      end

c***********************************************************************

      subroutine fuvRdPa(lu,nRanFile,nRanProg,params,
     *        indices1,indices2,scales1,scales2,zeros,TimOff)

      integer lu,nRanProg,nRanFile
      character params(nRanProg)*(*)
      integer indices1(nRanProg),indices2(nRanProg)
      real scales1(nRanProg),scales2(nRanProg),zeros(nRanProg)
      double precision TimOff
c-----------------------------------------------------------------------
c  Get all the indices and scale parameters associated with the random
c  parameters.  Give warnings if random parameters, which are not
c  understood, are found.
c
c  Input:
c    lu         Logical unit of file.
c    nRanProg
c    nRanFile
c    params
c
c  Outputs:
c    indices1   This is an array giving the offset into a FITS
c               visibility of first occurrence of various random
c               parameters.
c    indices2   Offset into a FITS visibility of second occurrence of
c               random parameter.
c    scales1    Scale factor associated with first occurrence.
c    scales2    Scale factor associated with second occurrence.
c    zero       Total offset.
c    TimOff     Offset time to add.
c
c-----------------------------------------------------------------------
      character ptype*8,umsg*64
      integer i,j,Tindx
      logical found,getjday
      real bs,bz
      logical fdiv
      double precision jday,freq,time1

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
c  Initialise the thingos for the random parameters.
c
      getjday = .false.
      TimOff = 0
      Tindx = 0
      do i = 1, nRanProg
        indices1(i) = 0
        indices2(i) = 0
        scales1(i)  = 0
        scales2(i)  = 0
        zeros(i) = 0
      enddo
c
c  Fill in the scale parameters of all the random parameters.
c
      do i = 1, nRanFile
        call fitrdhda(lu,'PTYPE'//itoaf(i),ptype,' ')
        call fitrdhdr(lu,'PSCAL'//itoaf(i),bs,1.0)
        call fitrdhdr(lu,'PZERO'//itoaf(i),bz,0.0)
c
c  Convert some somewhat different forms to something standard.
c
        fdiv = .false.
        if (ptype(1:3).eq.'UU' .or. ptype(1:3).eq.'UU-') then
          fdiv = ptype.eq.'UU-L'
          ptype = 'UU'
        else if (ptype(1:3).eq.'VV' .or. ptype(1:3).eq.'VV-') then
          fdiv = ptype.eq.'VV-L'
          ptype = 'VV'
        else if (ptype(1:3).eq.'WW' .or. ptype(1:3).eq.'WW-') then
          fdiv = ptype.eq.'WW-L'
          ptype = 'WW'
        else if (ptype.eq.'TIME1') then
          getjday = .true.
          ptype = 'DATE'
        endif
c
c  Check if we want this parameter.
c
        j = 0
        found = .false.
        do while (j.lt.nRanProg .and. .not.found)
          j = j + 1
          found = params(j).eq.ptype
        enddo
c
c  Remember it if we need it.
c
        if (found) then
          if (fdiv) then
            call fuvFreq(lu,freq)
            bz = bz / freq
            bs = bs / freq
          endif
          if (ptype.eq.'DATE') then
            TimOff = TimOff + bz
            TIndx = j
          else
            zeros(j) = zeros(j) + bz
          endif
          if (indices1(j).eq.0) then
            scales1(j) = bs
            indices1(j) = i
          else if (indices2(j).eq.0) then
            scales2(j) = bs
            indices2(j) = i
          else
            call bug('f', 'Cannot handle 3 scale factors for '//ptype)
          endif
        else
          call bug('w','Ignored random parameter '//ptype)
        endif
      enddo
c
c  Add to the date if we need to.
c
      if (getjday) then
        call fitdate(lu,'DATE-OBS',jday)
        if (jday.eq.0) then
          call bug('w','Not observation date info present')
          call bug('w','Assuming observation date is 01/01/90')
          call dayjul('01/01/90',jday)
        endif
        TimOff = TimOff + jday
      endif
c
c  Fiddle the time, if needed, to avoid a rounding problem.
c
      if (Tindx.ne.0) then
        if (indices2(Tindx).ne.0) then
          call fuvGrand(lu,Tindx,Time1)
          Time1 = nint(Time1)
          if (abs(Time1).gt.100) then
            zeros(Tindx) = zeros(Tindx) - Time1
            TimOff = TimOff + Time1
          endif
        endif
      endif
c
c  Check what we have.
c
      do j = 1, nRanProg
        if (indices1(j).eq.0) then
          umsg = 'Random parameter not found in FITS file: '//
     *                                                params(j)
          call bug('w',umsg)
        endif
      enddo

      end

c***********************************************************************

c* FuvGetT0 -- Get time offset for a UV FITS file.
c& rjs
c: fits
c+
      double precision function fuvGetT0(lu)

      integer lu
c  ---------------------------------------------------------------------
c  This returns the offset that needs to be added to the time to get
c  correct Julian time.
c
c  Input:
c    lu         The handle of the input uv file.
c  Output:
c    fuvToff    Time offset.
c-----------------------------------------------------------------------
      include 'fitsio.h'
c-----------------------------------------------------------------------
      if (nRanProg(lu).eq.0) call fuvSetPa(lu,0,' ')
      fuvGetT0 = TimOff(lu)

      end

c***********************************************************************

c* FuvSetT0 -- Set time offset for a UV FITS file.
c& rjs
c: fits
c+
      subroutine fuvSetT0(lu,T0)

      integer lu
      double precision T0
c  ---------------------------------------------------------------------
c  This returns the offset that needs to be added to the time to get
c  correct Julian time.
c
c  Input:
c    lu         The handle of the input uv file.
c    T0         The offset time.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      double precision jday0
      character string*24
c-----------------------------------------------------------------------
      jday0 = int(T0 - 0.5d0) + 0.5d0
      call Julday(jday0,'T',string)
      call fitwrhda(lu,'DATE-OBS',string)
      TimOff(lu) = T0

      end

c***********************************************************************

c* FuvWrhd -- Save UV FITS file coordinate information.
c& rjs
c: fits
c+
      subroutine fuvwrhd(lu,coord)

      integer lu
      double precision coord(3,4)
c  ---------------------------------------------------------------------
c  Save coord info about a UV file in the output.
c
c  Inputs:
c    lu         Handle of the UV FITS file.
c    coord      Coordinate information
c-----------------------------------------------------------------------
      include 'fitsio.h'

      integer   iax
      character cax*2, ctypes(4)*8

      external  itoaf
      character itoaf*2

      data ctypes/'STOKES  ','FREQ    ','RA      ','DEC     '/
c-----------------------------------------------------------------------
      call fitwrhdd(lu, 'CRPIX2', 1d0)
      call fitwrhdd(lu, 'CDELT2', 1d0)
      call fitwrhdd(lu, 'CRVAL2', 1d0)
      call fitwrhda(lu, 'CTYPE2', 'COMPLEX')

      do iax = 1, 4
        cax = itoaf(iax+2)
        call fitwrhdd(lu, 'CRPIX'//cax, coord(uvCrpix,iax))
        call fitwrhdd(lu, 'CDELT'//cax, coord(uvCdelt,iax))
        call fitwrhdd(lu, 'CRVAL'//cax, coord(uvCrval,iax))
        call fitwrhda(lu, 'CTYPE'//cax, ctypes(iax))
      enddo

      end

c***********************************************************************

c* FuvRdhd -- Get coordinate information about a UV FITS file.
c& rjs
c: fits
c+
      subroutine fuvrdhd(lu, coord)

      integer lu
      double precision coord(3,4)
c  ---------------------------------------------------------------------
c  Get coordinate information about a UV file.
c
c  Input:
c    lu         Handle of the uv FITS file.
c  Output:
c    coord      Coordinate information.
c
c  Though this could be done by an "ordinary" subroutine, put it here
c  because its so commonly done, and requires a bit of fiddling around.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer    MAXNO
      parameter (MAXNO=99)

      integer   iax, indx, found, naxis
      character cax*2, ctype*12

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
c     Get the number of dimensions, and do some checking.
      call fitrdhdi(lu, 'NAXIS', naxis, 0)
      if (naxis.le.1) call bug('f','Failed to find NAXIS in file')
      if (naxis.gt.MAXNO) then
        call bug('w','NAXIS greater than maximum, some dims ignored')
        naxis = MAXNO
      endif

c     Look for things that I am interested in.
      found = 0
      iax = 1
      do while (iax.lt.naxis .and. found.lt.4)
        iax = iax + 1
        cax = itoaf(iax)
        call fitrdhda(lu, 'CTYPE'//cax, ctype, ' ')
        if (ctype.eq.'STOKES') then
          indx = uvStokes
        else if (ctype.eq.'FREQ') then
          indx = uvFreq
        else if (ctype(1:2).eq.'RA') then
          indx = uvRa
        else if (ctype(1:3).eq.'DEC') then
          indx = uvDec
        else
          indx = 0
        endif

        if (indx.ne.0) then
          found = found + 1
          call fitrdhdd(lu, 'CRPIX'//cax, coord(uvCrpix,indx), 0d0)
          call fitrdhdd(lu, 'CDELT'//cax, coord(uvCdelt,indx), 1d0)
          call fitrdhdd(lu, 'CRVAL'//cax, coord(uvCrval,indx), 0d0)
        endif
      enddo

      if (found.ne.4)
     *    call bug('f','STOKES, FREQ, RA or DEC missing in header')

      end

c***********************************************************************

c* FuvRead -- Read visibility data from UV FITS file.
c& rjs
c: fits
c+
      subroutine fuvread(lu,visdat,number,count)

      integer lu,number,count
      real visdat(*)
c  ---------------------------------------------------------------------
c  Read some uv data.
c  The loops in this subroutine feel unusual because are inside out,
c  so that vectorization can be increased.
c
c  Inputs:
c    lu         Logical unit of file.
c    number     Number of the first visibility to read.
c    count      The number of visibilities to read.
c
c  Output:
c    Visdat     Output buffer containing the visibilities.
c
c  Internal Variables:
c    VispBuf    Visibilities per buffer.
c    PixPnt     Pixel offset of place into file where i/o is going on.
c    PixpWd     Pixels per Word.
c    PpVisf     Pixels per Visibility in the File.
c    PpVisp     Pixels per Visibility in the Program.
c
c-----------------------------------------------------------------------
      integer k,ktot,PpVisf,PpVisp,VispBuf,n,ltot,iostat,nts4
      integer offset3(3)
      logical dotran
      include 'fitsio.h'
c-----------------------------------------------------------------------
      if (new(lu)) call bug('f','Cannot read from new FITS file')
      if (nRanProg(lu).eq.0) call fuvSetPa(lu,0,' ')
      nts4 = nts1(lu)*nts2(lu)*nts3(lu)
      dotran = nts4.gt.1
      nts4 = ncomplex(lu)*pols(lu)*freqs(lu)/nts4
c
c  Initialise.
c
      PpVisf = nRanFile(lu)+ ncomplex(lu)*pols(lu)*freqs(lu)
      PpVisp = nRanProg(lu)+ 3           *pols(lu)*freqs(lu)
      VispBuf = maxsize/PpVisf
      if (VispBuf.le.0)
     *  call bug('f','Cannot fit a visibility into the buffer')
c
c  Perform Data I/O. K points to the next visibility to transfer, KTOT
c  points beyond the last.
c
      k = 0
      ktot = count
      call mpCvtim(offset3,number-1)
      call mpMulmi(offset3,BypPix(lu)*PpVisf)
      call mpAddmm(offset3,DatBase3(1,lu))
c       ... offset = BypPix(lu) * (number-1) * PpVisf + DatBase(lu)
      do while (k.lt.ktot)
        n = min(VispBuf, ktot-k)
        ltot = n * PpVisf
        if (float(lu)) then
          call hread3r(item(lu),rarray,offset3,4*ltot,iostat)
          if (dotran) call fuvtranr(rarray,rarrayd,
     *        nts1(lu),nts2(lu),nts3(lu),nts4,ppvisf,n)
          if (BlankVal(lu).ne.0) then
            call hread3i(item(lu),array,offset3,4*ltot,iostat)
            if (dotran) call fuvtrani(array,arrayd,
     *        nts1(lu),nts2(lu),nts3(lu),nts4,ppvisf,n)
          endif
          call fuvrtrn2(lu,n,rarray,array,
     *                PpVisf,visdat(k*PpVisp+1),PpVisp)
        else
          if (BypPix(lu).eq.2) then
            call hread3j(item(lu),array,offset3,2*ltot,iostat)
          else
            call hread3i(item(lu),array,offset3,4*ltot,iostat)
          endif
          if (dotran) call fuvtrani(array,arrayd,
     *        nts1(lu),nts2(lu),nts3(lu),nts4,ppvisf,n)
          call fuvrtrn1(lu,n,array,PpVisf,visdat(k*PpVisp+1),PpVisp)
        endif
c
c  Increment the number of visibilites read.
c
        k = k + n
        call mpAddmi(offset3,BypPix(lu)*ltot)
      enddo

      end

c***********************************************************************

c* FuvWrite -- Write data to a UV FITS file.
c& rjs
c: fits
c+
      subroutine fuvwrite(lu,visdat,number,count)

      integer lu,number,count
      real visdat(*)
c  ---------------------------------------------------------------------
c  Write some uv data. This performs a sequential write, starting at the
c  current pointer.
c  The loops in this subroutine feel unusual because are inside out,
c  so that vectorization can be increased.
c
c  Inputs:
c    lu         Logical unit of file.
c    number     Number of the first visibility to read.
c    count      The number of visibilities to read.
c
c  Output:
c    Visdat     Output buffer containing the visibilities.
c
c  NOTE: This assumes that the data are being written out in floating
c        point form, wwith no scale factors!!
c
c  Internal Variables:
c    PpVisf     Pixels per Visibility in the File.
c
c-----------------------------------------------------------------------
      character line*32
      integer PpVisf,length,iostat
      integer offset3(3)
      include 'fitsio.h'

      external mpSign
      integer  mpSign
c-----------------------------------------------------------------------
      if (.not.new(lu)) call bug('f','Cannot write to old FITS file')
      if (nRanProg(lu).le.0) call fuvSetPa(lu,0,' ')

c     If it's a new file write out the card giving the visibility
c     scaling parameter.  Make sure that this is the last card in the
c     header.
      if (mpSign(DatBase3(1,lu)).eq.0) then
        write(line,'(a,1pe18.11)')'AIPS WTSCAL = ',WtScal(lu)
        call fitwrhdh(lu,'HISTORY',line)
        call fithdfin(lu)
      endif

c     Perform Data I/O.
      PpVisf = nRanFile(lu) + ncomplex(lu)*pols(lu)*freqs(lu)
      call mpCvtim(offset3,number-1)
      call mpMulmi(offset3,BypPix(lu)*PpVisf)
      call mpAddmm(offset3,DatBase3(1,lu))
      length = BypPix(lu) * count * PpVisf
      call hwrite3r(item(lu),visdat,offset3,length,iostat)
      if (iostat.ne.0) call bugno('f',iostat)

      end

c***********************************************************************

c* FuvClose -- Close a UV FITS file.
c& rjs
c: fits
c+
      subroutine fuvclose(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Close up a UV file. Good night.
c
c  Input:
c    lu         Handle of the UV FITS file.
c-----------------------------------------------------------------------
      call fitclose(lu)

      end

c***********************************************************************

      subroutine fuvrtrn1(lu,n,in,PpVisi,out,PpViso)

      integer lu,n,PpVisi,PpViso
      integer in(*)
      real out(*)
c-----------------------------------------------------------------------
c  Read and scale uv data read from a FITS file.  Input is INTEGER
c  values.  The output is a real array of scaled visibilities in the
c  canonical order (i.e. u,v,w,baseline,time).  No assumptions can be
c  made about ordering, etc, but these routines optimise as best they
c  can.
c
c  Inputs:
c    lu         File descriptor.
c    n          Number of visibilities to scale and transfer.
c    in         Integer array of unscaled visibilities.
c    PpVisi     Elements per visibility in the input array. The
c               size of IN will be PpVisi*n elements.
c    PpViso     Elements per visibility in the output array. The
c               size of OUT will be PpViso*n elements.
c
c  Outputs:
c    out        Real array of scaled visibilities.
c
c-----------------------------------------------------------------------
      integer i,iin,iout
      include 'fitsio.h'
c-----------------------------------------------------------------------
c     Get u, v, w, baseline and date out of the buffer.
      do i = 1, nRanProg(lu)
        if (indices2(i,lu).ne.0) then
          call fuvmltr2(n,scales1(i,lu),in(indices1(i,lu)),
     *                   scales2(i,lu),in(indices2(i,lu)),
     *                  zeros(i,lu),PpVisi,
     *                  out(i), PpViso)
        else if (indices1(i,lu).ne.0) then
          call fuvmltr1(n,scales1(i,lu),zeros(i,lu),
     *                in(indices1(i,lu)),PpVisi,
     *                out(i),       PpViso)
        else
          call fuvclr(n,out(i),PpViso)
        endif
      enddo

c     Transfer and scale the visibilities from the input to the output
c     buffer.  To improve vectorization, do it so that the inner loop is
c     the longer.
      iin  = nRanFile(lu) + 1
      iout = nRanProg(lu) + 1
      if (n.gt.pols(lu)*freqs(lu)) then
        do i = 1, pols(lu)*freqs(lu)
          if (ncomplex(lu).eq.3) then
            call fuvmltv1(n,in(iin),PpVisi,out(iout),PpViso,
     *                bscale(lu),bzero(lu),WtScal(lu),BlankVal(lu))
          else
            call fuvmltv2(n,in(iin),PpVisi,out(iout),PpViso,
     *                bscale(lu),bzero(lu),BlankVal(lu))
          endif
          iin = iin + ncomplex(lu)
          iout = iout + 3
        enddo
      else
        do i = 1, n
          if (ncomplex(lu).eq.3) then
            call fuvmltv1(pols(lu)*freqs(lu),in(iin),ncomplex(lu),
     *       out(iout),3,bscale(lu),bzero(lu),WtScal(lu),BlankVal(lu))
          else
            call fuvmltv2(pols(lu)*freqs(lu),in(iin),ncomplex(lu),
     *       out(iout),3,bscale(lu),bzero(lu),BlankVal(lu))
          endif
          iin = iin + PpVisi
          iout = iout + PpViso
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvrtrn2(lu,n,in,mask,PpVisi,out,PpViso)

      integer lu,n,PpVisi,PpViso
      real in(*),out(*)
      integer mask(*)
c-----------------------------------------------------------------------
c  Read and scale uv data read from a FITS file.  Input is REAL values.
c  The output is real array of scaled visibilities in the canonical
c  order (i.e. u,v,w,baseline,time).  No assumptions can be made about
c  ordering, etc, but these routines optimise as best they can.
c
c  Inputs:
c    lu         File descriptor.
c    n          Number of visibilities to scale and transfer.
c    in         Real array of unscaled visibilities.
c    mask       Blanking mask.
c    PpVisi     Elements per visibility in the input array. The
c               size of IN will be PpVisi*n elements.
c    PpViso     Elements per visibility in the output array. The
c               size of OUT will be PpViso*n elements.
c
c  Outputs:
c    out        Real array of scaled visibilities.
c
c-----------------------------------------------------------------------
      integer i,iin,iout
      include 'fitsio.h'
c-----------------------------------------------------------------------
c     Get u, v, w, baseline and date out of the buffer.
      do i = 1, nRanProg(lu)
        if (indices2(i,lu).ne.0) then
          call fuvmltr4(n,scales1(i,lu),in(indices1(i,lu)),
     *                   scales2(i,lu),in(indices2(i,lu)),
     *                  zeros(i,lu),PpVisi,
     *                  out(i), PpViso)
        else if (indices1(i,lu).ne.0) then
          call fuvmltr3(n,scales1(i,lu),zeros(i,lu),
     *                in(indices1(i,lu)),PpVisi,
     *                out(i),       PpViso)
        else
          call fuvclr(n,out(i),PpViso)
        endif
      enddo

c     Transfer and scale the visibilities from the input to the output
c     buffer.  To improve vectorization, do it so that the inner loop is
c     the longer.
      iin  = nRanFile(lu) + 1
      iout = nRanProg(lu) + 1
      if (n.gt.pols(lu)*freqs(lu)) then
        do i = 1, pols(lu)*freqs(lu)
          if (ncomplex(lu).eq.3) then
            call fuvmltv3(n,in(iin),mask(iin),PpVisi,out(iout),
     *        PpViso,bscale(lu),bzero(lu),WtScal(lu),BlankVal(lu))
          else
            call fuvmltv4(n,in(iin),mask(iin),PpVisi,out(iout),
     *        PpViso,bscale(lu),bzero(lu),BlankVal(lu))
          endif
          iin = iin + ncomplex(lu)
          iout = iout + 3
        enddo
      else
        do i = 1, n
          if (ncomplex(lu).eq.3) then
            call fuvmltv3(pols(lu)*freqs(lu),in(iin),mask(iin),
     *        ncomplex(lu),out(iout),3,bscale(lu),bzero(lu),
     *        WtScal(lu),BlankVal(lu))
          else
            call fuvmltv4(pols(lu)*freqs(lu),in(iin),mask(iin),
     *        ncomplex(lu),out(iout),3,bscale(lu),bzero(lu),
     *        BlankVal(lu))
          endif
          iin = iin + PpVisi
          iout = iout + PpViso
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvclr(n,out,inc)

      integer n,inc
      real out(*)
c-----------------------------------------------------------------------
c  Set an array to zero.
c
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1,n*inc,inc
        out(i) = 0
      enddo

      end

c***********************************************************************

      subroutine fuvmltv1(n,a,na,b,nb,bscale,bzero,wtscal,blank)

      integer n,na,nb,a(*),blank
      real b(*)
      real bscale,bzero,wtscal
c-----------------------------------------------------------------------
c  Scale the visibility data(3 elements,integer) to a real array.
c  Optimise the case where BZERO is 0.
c-----------------------------------------------------------------------
      integer j,k
      real wtbscale,wtbzero
c-----------------------------------------------------------------------
      k = 1
      if (bzero.ne.0) then
        wtbscale = wtscal * bscale
        wtbzero  = wtscal * bzero
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k) + bzero
          b(j+1) = bscale * a(k+1) + bzero
          b(j+2) = wtbscale * a(k+2) + wtbzero
          k = k + na
        enddo
      else
        wtbscale = wtscal * bscale
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k)
          b(j+1) = bscale * a(k+1)
          b(j+2) = wtbscale * a(k+2)
          k = k + na
        enddo
      endif
c
c  Set visibilities to zero if they were magic value blanked.
c  Because there is an associated weight, we assume that the
c  weight correctly reflects the flagged state.
c
      k = 1
      if (blank.ne.0) then
c# ivdep
        do j = 1,n*nb,nb
          if (a(k).eq.blank .or. a(k+1).eq.blank) then
            b(j) = 0
            b(j+1) = 0
          endif
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltv2(n,a,na,b,nb,bscale,bzero,blank)

      integer n,na,nb,a(*),blank
      real b(*)
      real bscale,bzero
c-----------------------------------------------------------------------
c  Scale the visibility data(2 elements,integer) to a real array.
c  Optimise the case where BZERO is 0 and where there is no blanking.
c-----------------------------------------------------------------------
      integer j,k
c-----------------------------------------------------------------------
      k = 1
      if (bzero.ne.0) then
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k) + bzero
          b(j+1) = bscale * a(k+1) + bzero
          b(j+2) = 1
          k = k + na
        enddo
      else
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k)
          b(j+1) = bscale * a(k+1)
          b(j+2) = 1
          k = k + na
        enddo
      endif
c
c  Apply blanking if needed.
c
      k = 1
      if (blank.ne.0) then
c# ivdep
        do j = 1,n*nb,nb
          if (a(k).eq.blank .or. a(k+1).eq.blank) then
            b(j)   = 0
            b(j+1) = 0
            b(j+2) = -1
          endif
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltv3(n,a,mask,na,b,nb,bscale,bzero,wtscal,blank)

      integer n,na,nb,blank
      real a(*),b(*)
      integer mask(*)
      real bscale,bzero,wtscal
c-----------------------------------------------------------------------
c  Scale the visibility data(3 elements,real) to a real array.
c  Optimise the case where BZERO is 0 and BSCALE is 1.
c-----------------------------------------------------------------------
      integer j,k
      real wtbscale,wtbzero,temp
c-----------------------------------------------------------------------
c  Apply blanking if needed.
c
      if (blank.ne.0) then
        k = 1
        temp = -bzero/bscale
c# ivdep
        do j = 1,n*nb,nb
          if (mask(k).eq.blank)   a(k)   = temp
          if (mask(k+1).eq.blank) a(k+1) = temp
          k = k + na
        enddo
      endif

      k = 1
      if (bzero.eq.0 .and. bscale.eq.1 .and. wtscal.eq.1) then
c# ivdep
        do j = 1,n*nb,nb
          b(j) = a(k)
          b(j+1) = a(k+1)
          b(j+2) = a(k+2)
          k = k + na
        enddo
      else if (bzero.ne.0) then
        wtbscale = wtscal * bscale
        wtbzero  = wtscal * bzero
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k) + bzero
          b(j+1) = bscale * a(k+1) + bzero
          b(j+2) = wtbscale * a(k+2) + wtbzero
          k = k + na
        enddo
      else
        wtbscale = wtscal * bscale
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k)
          b(j+1) = bscale * a(k+1)
          b(j+2) = wtbscale * a(k+2)
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltv4(n,a,mask,na,b,nb,bscale,bzero,blank)

      integer n,na,nb,blank
      real a(*),b(*)
      integer mask(*)
      real bscale,bzero
c-----------------------------------------------------------------------
c  Scale the visibility data(2 elements,real) to a real array.
c  Optimise the case where BZERO is 0 and BSCALE is 1.
c-----------------------------------------------------------------------
      integer j,k
      real temp
c-----------------------------------------------------------------------
c  Apply blanking if needed.
c
      if (blank.ne.0) then
        k = 1
        temp = -bzero/bscale
c# ivdep
        do j = 1,n*nb,nb
          if (mask(k).eq.blank .or. mask(k+1).eq.blank) then
            a(k) = temp
            a(k+1) = temp
            b(j+2) = -1
          else
            b(j+2) = 1
          endif
          k = k + na
        enddo
      else
c# ivdep
        do j = 1,n*nb,nb
          b(j+2) = 1
        enddo
      endif
c
c  Copy the data.
c
      k = 1
      if (bzero.eq.0 .and. bscale.eq.1) then
c# ivdep
        do j = 1,n*nb,nb
          b(j) = a(k)
          b(j+1) = a(k+1)
          k = k + na
        enddo
      else if (bzero.ne.0) then
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k) + bzero
          b(j+1) = bscale * a(k+1) + bzero
          k = k + na
        enddo
      else
c# ivdep
        do j = 1,n*nb,nb
          b(j) = bscale * a(k)
          b(j+1) = bscale * a(k+1)
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltr1(n,bscale,bzero,a,na,b,nb)

      integer n,na,nb,a(*)
      real b(*)
      real bscale,bzero
c-----------------------------------------------------------------------
c  Scale a random parameter(single,integer) to a real.
c  Optimise this case when BZERO is zero.
c
c  Inputs:
c    n          Number of elements to scale.
c    bscale     Scale factor to multiply by.
c    bzero      Offset factor to add.
c    a          Integer array contining unscaled data.
c    na         Increment between elements in A to scale.
c    nb         Increment between elements in the output array B.
c
c  Outputs:
c     b         Real array containing scaled data.
c-----------------------------------------------------------------------
      integer j,k
c-----------------------------------------------------------------------
      k = 1
      if (bzero.ne.0) then
        do j = 1,n*nb,nb
          b(j) = bscale*a(k) + bzero
          k = k + na
        enddo
      else
        do j = 1,n*nb,nb
          b(j) = bscale*a(k)
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltr2(n,bscale1,a1,bscale2,a2,bzero,na,b,nb)

      integer n,na,nb,a1(*),a2(*)
      real b(*),bscale1,bscale2,bzero
c-----------------------------------------------------------------------
c  Scale a random parameter(double,integer) to a real.
c  Optimise this case when BZERO is zero. Also do the case where
c  BZERO is non-zero in double precision,to avoid rounding problems.
c-----------------------------------------------------------------------
      integer j,k
      double precision bs1,bs2,bz
c-----------------------------------------------------------------------
      k = 1
      if (bzero.ne.0) then
        bs1 = bscale1
        bs2 = bscale2
        bz = bzero
        do j = 1,n*nb,nb
          b(j) = bs1*a1(k) + bs2*a2(k) + bz
          k = k + na
        enddo
      else
        do j = 1,n*nb,nb
          b(j) = bscale1*a1(k) + bscale2*a2(k)
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltr3(n,bscale,bzero,a,na,b,nb)

      integer n,na,nb
      real a(*)
      real b(*)
      real bscale,bzero
c-----------------------------------------------------------------------
c  Scale a random parameter(single,float) to a real.
c  Optimise this case when BZERO is zero.
c-----------------------------------------------------------------------
      integer j,k
c-----------------------------------------------------------------------
      k = 1
      if (bscale.eq.1 .and. bzero.eq.0) then
        do j = 1,n*nb,nb
          b(j) = a(k)
          k = k + na
        enddo
      else if (bzero.ne.0) then
        do j = 1,n*nb,nb
          b(j) = bscale*a(k) + bzero
          k = k + na
        enddo
      else
        do j = 1,n*nb,nb
          b(j) = bscale*a(k)
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvmltr4(n,bscale1,a1,bscale2,a2,bzero,na,b,nb)

      integer n,na,nb
      real a1(*),a2(*)
      real b(*),bscale1,bscale2,bzero
c-----------------------------------------------------------------------
c  Scale a random parameter(double,float) to a real.
c  Optimise this case when BZERO is zero. Also do the case where BZERO
c  is non-zero in double precision, to avoid rounding problems.
c-----------------------------------------------------------------------
      integer j,k
      double precision bs1,bs2,bz
c-----------------------------------------------------------------------
      k = 1
      if (bzero.ne.0) then
        bs1 = bscale1
        bs2 = bscale2
        bz  = bzero
        do j = 1,n*nb,nb
          b(j) = bs1*a1(k) + bs2*a2(k) + bz
          k = k + na
        enddo
      else
        do j = 1,n*nb,nb
          b(j) = bscale1*a1(k) + bscale2*a2(k)
          k = k + na
        enddo
      endif

      end

c***********************************************************************

      subroutine fuvGrand(lu,indx,rparam)

      integer lu,indx
      double precision rparam
c-----------------------------------------------------------------------
c  Get the value of the first value of a particular random parameter
c  in the dataset.
c
c  Input:
c    lu         Handle of the FITS dataset.
c    indx       Index of the random parameter in fitsio's internal
c               tables.
c  Output:
c    rparam     The value of the first occurrence of the random
c               parameter.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer iostat
c-----------------------------------------------------------------------
      rparam = zeros(indx,lu)
      if (indices1(indx,lu).eq.0) return

      if (nRanFile(lu).gt.maxsize)
     *  call bug('f','Cannot fit the random parameters into buffer')
      if (float(lu)) then
        call hread3r(item(lu),rarray,Datbase3(1,lu),4*nRanFile(lu),
     *                                                      iostat)
        if (iostat.ne.0) then
          call bug('w','Error reading FITS file')
          call bugno('f',iostat)
        endif
        rparam = rparam +
     *    dble(scales1(indx,lu))*rarray(indices1(indx,lu))
        if (indices2(indx,lu).ne.0) rparam = rparam +
     *    dble(scales2(indx,lu))*rarray(indices2(indx,lu))
      else
        if (BypPix(lu).eq.2) then
          call hread3j(item(lu),array,Datbase3(1,lu),2*nRanFile(lu),
     *                                                        iostat)
        else
          call hread3i(item(lu),array,Datbase3(1,lu),4*nRanFile(lu),
     *                                                        iostat)
        endif
        if (iostat.ne.0) then
          call bug('w','Error reading FITS file')
          call bugno('f',iostat)
        endif
        rparam = rparam +
     *    dble(scales1(indx,lu))*array(indices1(indx,lu))
        if (indices2(indx,lu).ne.0) rparam = rparam +
     *    dble(scales2(indx,lu))*array(indices2(indx,lu))
      endif

      end

c***********************************************************************

      subroutine fuvFreq(lu,freq)

      integer lu
      double precision freq
c-----------------------------------------------------------------------
c  Get the reference frequency from the header.
c-----------------------------------------------------------------------
      integer   iax, naxis
      character cax*2, ctype*16

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
      call fitrdhdi(lu, 'NAXIS', naxis, 0)
      if (naxis.le.2) call bug('f','Invalid uv FITS header')

      freq = 0d0
      do iax = 1, naxis
        cax = itoaf(iax)
        call fitrdhda(lu, 'CTYPE'//cax, ctype, ' ')
        if (ctype.eq.'FREQ') then
          call fitrdhdd(lu, 'CRVAL'//cax, freq, 0d0)
          goto 10
        endif
      enddo

 10   if (freq.le.0d0) call bug('f','Unable to determine frequency')

      end

c***********************************************************************

      subroutine fuvwt(lu,factor)

      integer lu
      real factor
c-----------------------------------------------------------------------
c  Determine the weight scaling factor. AIPS is pretty cludgly here. The
c  scale factor for the visibility weights in included as a HISTORY
c  comment. Often there may be multiple copies of the HISTORY card which
c  gives the scale factor. This routine always returns the first card
c  found (I am not sure if this is correct).
c
c  Inputs:
c    lu         File descriptor.
c
c  Outputs:
c    factor     The weight scaling factor.
c-----------------------------------------------------------------------
      integer lcard
      parameter (lcard=80)
      double precision dval
      integer k,k0,l
      logical found
      character card*(lcard)
c-----------------------------------------------------------------------
      dval = 1
      call fitsrch(lu,'HISTORY',found)
      if (.not.found) return
      call fitcdio(lu,card)
c
c  Process all cards.
c
      do while (card(1:4).ne.'END ')
        if (card(1:8).eq.'HISTORY ') then
          k0 = 9
          k = index(card(k0:lcard),'AIPS')
          k0 = k0 + k - 1
          if (k.gt.0) k = index(card(k0:lcard),'WTSCAL')
          k0 = k0 + k - 1
          if (k.gt.0) k = index(card(k0:lcard),'=')
          k0 = k0 + k - 1
          if (k.gt.0) then
            k = k0 + 1
            do while (card(k:k).eq.' ')
              k = k + 1
            enddo
            l = k
            do while (card(l:l).ne.' ' .and. card(l:l).ne.'/')
              l = l + 1
            enddo
            l = l - 1
            if (l.ge.k) call atodf(card(k:l),dval,found)
          endif
        endif
        call fitcdio(lu,card)
      enddo

      factor = dval

      end

c***********************************************************************

      subroutine fuvget(lu,naxis,ncmplx,icmplx,npol,ipol,nfreq,ifreq,
     *        nif,iif)

      integer lu,naxis,npol,nfreq,ipol,ifreq,icmplx,ncmplx,iif,nif
c-----------------------------------------------------------------------
c  Search through the "regular" dimensions of a uv file, looking for
c  the frequency and polarization axis. If other axes, with lengths
c  other than 1, are found, raise an error.
c
c  Input:
c    lu         Handle of the input file.
c    naxis      Number of regular dimensions.
c  Output:
c    ncmplx     Number of elements along the "complex" axis.
c    npol       Number of elements along the polarization axis.
c    nfreq      Number of elements along the frequency axis.
c    nif        Number of elements along the if axis.
c    icmplx     Index of the complex axis.
c    ipol       Index of the polarization axis.
c    ifreq      Index of the frequency axis.
c    iif        Index of the IF frequency axis.
c-----------------------------------------------------------------------
      integer   iax, n
      character cax*2, string*16

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
      ncmplx = 0
      npol   = 1
      nfreq  = 1
      nif    = 1
      icmplx = 0
      ipol   = 0
      ifreq  = 0
      iif    = 0

      do iax = 2, naxis
        cax = itoaf(iax)
        call fitrdhdi(lu, 'NAXIS'//cax, n, 1)
        call fitrdhda(lu, 'CTYPE'//cax, string, ' ')
        if (n.le.1) then
          continue
        else if (string.eq.'FREQ') then
          ifreq = iax
          nfreq = n
        else if (string.eq.'IF') then
          nif = n
          iif = iax
        else if (string.eq.'STOKES') then
          ipol = iax
          npol = n
        else if (string.eq.'COMPLEX') then
          icmplx = iax
          ncmplx = n
        else
          call bug('f','Cannot deal with uv axis of type '//string)
        endif
      enddo

      if (icmplx.eq.0) call bug('f','COMPLEX axis missing')

      end

c***********************************************************************

      subroutine fuvtranr(Data,Tmp,n1,n2,n3,n4,ppvis,nvis)

      integer n1,n2,n3,n4,ppvis,nvis
      real data(ppvis*nvis),tmp(ppvis*nvis)
c-----------------------------------------------------------------------
      integer ii,io,i,j,k,l,m
c-----------------------------------------------------------------------
      if (n1*n2*n3*n4.gt.ppvis)
     *  call bug('f','Something is screwy in fuvtranr')

      ii = 1 - n1*n2*n3*n4 + ppvis
      do m = 1, nvis
        io = 1
        do l = 1, n4
          do k = 1, n2
            do j = 1, n3
              do i = 1, n1
                Tmp(io) = Data(ii)
                io = io + 1
                ii = ii + 1
              enddo
              ii = ii - n1 + n1*n2
            enddo
            ii = ii - n1*n2*n3 + n1
          enddo
          ii = ii - n1*n2 + n1*n2*n3
        enddo
        ii = ii - n1*n2*n3*n4

        do i = 1, n1*n2*n3*n4
          data(ii) = tmp(i)
          ii = ii + 1
        enddo
        ii = ii - n1*n2*n3*n4 + ppvis
      enddo

      end

c***********************************************************************

      subroutine fuvtrani(Data,Tmp,n1,n2,n3,n4,ppvis,nvis)

      integer n1,n2,n3,n4,ppvis,nvis
      integer data(ppvis*nvis),tmp(ppvis*nvis)
c-----------------------------------------------------------------------
      integer ii,io,i,j,k,l,m
c-----------------------------------------------------------------------
      if (n1*n2*n3*n4.gt.ppvis)
     *  call bug('f','Something is screwy in fuvtrani')

      ii = 1 - n1*n2*n3*n4 + ppvis
      do m = 1, nvis
        io = 1
        do l = 1, n4
          do k = 1, n2
            do j = 1, n3
              do i = 1, n1
                Tmp(io) = Data(ii)
                io = io + 1
                ii = ii + 1
              enddo
              ii = ii - n1 + n1*n2
            enddo
            ii = ii - n1*n2*n3 + n1
          enddo
          ii = ii - n1*n2 + n1*n2*n3
        enddo
        ii = ii - n1*n2*n3*n4

        do i = 1, n1*n2*n3*n4
          data(ii) = tmp(i)
          ii = ii + 1
        enddo
        ii = ii - n1*n2*n3*n4 + ppvis
      enddo

      end

c***********************************************************************

c* FitRdhdi -- Read an integer value from a FITS file header.
c& rjs
c: fits
c+
      subroutine fitrdhdi(lu,key,out,default)

      integer lu
      character key*(*)
      integer out,default
c  ---------------------------------------------------------------------
c  Read an integer valued FITS card.
c
c  Input:
c    lu         File descriptor.
c    key        Keyword to be read from header.
c    default    Default value if the keyword is not found in the header.
c
c  Output:
c    out        Value of the keyword (this will be the default if the
c               keyword was not found).
c-----------------------------------------------------------------------
      double precision val
c-----------------------------------------------------------------------
      call fitrdhdd(lu,key,val,dble(default))
      out = nint(val)

      end

c***********************************************************************

c* FitRdhdr -- Read a real value from a FITS file header.
c& rjs
c: fits
c+
      subroutine fitrdhdr(lu,key,out,default)

      integer lu
      character key*(*)
      real out,default
c  ---------------------------------------------------------------------
c  Read a real valued FITS card.
c
c  Input:
c    lu         File descriptor.
c    key        Keyword to be read from header.
c    default    Default value if the keyword is not found in the header.
c
c  Output:
c    out        Value of the keyword (this will be the default if the
c               keyword was not found).
c-----------------------------------------------------------------------
      double precision val
c-----------------------------------------------------------------------
      call fitrdhdd(lu,key,val,dble(default))
      out = val

      end

c***********************************************************************

c* FitRdhdd -- Read a double precision value from a FITS file header.
c& rjs
c: fits
c+
      subroutine fitrdhdd(lu,key,out,default)

      integer lu
      character key*(*)
      double precision out,default
c  ---------------------------------------------------------------------
c  Search for a double precision keyword. If found,decode it, otherwise
c  use the default.
c
c  Input:
c    lu         File descriptor.
c    key        Keyword to be read from header.
c    default    Default value if the keyword is not found in the header.
c
c  Output:
c    out        Value of the keyword (this will be the default if the
c               keyword was not found).
c-----------------------------------------------------------------------
      character card*80
      logical found,ok
      integer i,j
c-----------------------------------------------------------------------
      call fitsrch(lu,key,found)
      if (found) then
        call fitcdio(lu,card)
        i = index(card,'=') + 1
        do while (card(i:i).eq.' ')
          i = i + 1
        enddo
        j = i - 1
        do while (card(j+1:j+1).ne.' ' .and. card(j+1:j+1).ne.'/')
          j = j + 1
        enddo
        ok = .false.
        if (j.ge.i) call atodf(card(i:j),out,ok)
        if (.not.ok)
     *    call bug('f','Conversion error in decoding FITS card')
      else
        out = default
      endif

      end

c***********************************************************************

c* FitRdhda -- Read a character value from a FITS file header.
c& rjs
c: fits
c+
      subroutine fitrdhda(lu,key,out,default)

      integer lu
      character key*(*)
      character out*(*),default*(*)
c  ---------------------------------------------------------------------
c  Read a character valued FITS card.
c
c  Input:
c    lu         File descriptor.
c    key        Keyword to be read from header.
c    default    Default value if the keyword is not found in the header.
c
c  Output:
c    out        Value of the keyword (this will be the default if the
c               keyword was not found).
c-----------------------------------------------------------------------
      character card*80
      logical found
      integer i,j
c-----------------------------------------------------------------------
      call fitsrch(lu,key,found)
      if (found) then
        call fitcdio(lu,card)
        i = index(card,'=') + 1
        do while (card(i:i).ne.'''' .and. i.lt.80)
          i = i + 1
        enddo
        i = i + 1
        if (i.ge.80) then
           call bug('w','keyword w/o quotes')
           out = default
           return
        endif
        j = i
        do while (card(j:j).ne.'''' .and. j.lt.80)
          j = j + 1
        enddo
        if (j.ge.80) then
           call bug('w','keyword w/o end quote')
           out = default
           return
        endif
        out = card(i:j-1)
      else
        out = default
      endif

      end

c***********************************************************************

c* FitRdhdl -- Read a logical value from a FITS file header.
c& rjs
c: fits
c+
      subroutine fitrdhdl(lu,key,out,default)

      integer lu
      character key*(*)
      logical out,default
c  ---------------------------------------------------------------------
c  Read a logical valued FITS card.
c
c  Input:
c    lu         File descriptor.
c    key        Keyword to be read from header.
c    default    Default value if the keyword is not found in the header.
c
c  Output:
c    out        Value of the keyword (this will be the default if the
c               keyword was not found).
c-----------------------------------------------------------------------
      character card*80
      logical found
      integer i
c-----------------------------------------------------------------------
      call fitsrch(lu,key,found)
      if (found) then
        call fitcdio(lu,card)
        i = index(card,'=') + 1
        do while (card(i:i).eq.' ')
          i = i + 1
        enddo
        if (card(i:i).eq.'T') then
          out = .true.
        else if (card(i:i).eq.'F') then
          out = .false.
        else
          call bug('w','Error decoding logical keyword')
          out = default
        endif
      else
        out = default
      endif

      end

c***********************************************************************

c* FitWrhdh -- Write a string to a FITS file header.
c& rjs
c: fits
c+
      subroutine fitwrhdh(lu,key,value)

      integer lu
      character key*(*),value*(*)
c  ---------------------------------------------------------------------
c  Write a comment or history card.
c
c  Input:
c    lu         File descriptor.
c    key        FITS keyword (usually HISTORY).
c    value      The comment to write.
c-----------------------------------------------------------------------
      character card*80
c-----------------------------------------------------------------------
      card(1:8) = key
      card(9:80) = value
      call fitcdio(lu,card)

      end

c***********************************************************************

c* FitWrhda -- Write a string to a FITS file header.
c& rjs
c: fits
c+
      subroutine fitwrhda(lu,key,value)

      integer lu
      character key*(*)
      character value*(*)
c  ---------------------------------------------------------------------
c  Write an ascii valued fits card.
c
c  Input:
c    lu         File descriptor.
c    key        FITS keyword.
c    value      Value of the keyword.
c-----------------------------------------------------------------------
      character card*80
      integer length

      external len1
      integer  len1
c-----------------------------------------------------------------------
      length = min(len1(value),65)
      card(1:8) = key
      card(9:80) = '= '''//value(1:length)
      length = max(length,8) + 12
      card(length:length+3) = '''  /'
      call fitcdio(lu,card)

      end

c***********************************************************************

c* FitWrhdl -- Write a logical value to a FITS file header.
c& rjs
c: fits
c+
      subroutine fitwrhdl(lu,key,value)

      integer lu
      character key*(*)
      logical value
c  ---------------------------------------------------------------------
c  Write a logical valued fits cards.
c
c  Input:
c    lu         File descriptor.
c    key        FITS keyword.
c    value      Value of the keyword.
c-----------------------------------------------------------------------
      character card*80,keyword*8,c*1
c-----------------------------------------------------------------------
      keyword = key
      c = 'F'
      if (value) c='T'
      card = keyword//'=                    '//c//'  /'
      call fitcdio(lu,card)

      end

c***********************************************************************

c* FitWrhdi -- Write an integer value to a FITS file header.
c& rjs
c: fits
c+
      subroutine fitwrhdi(lu,key,value)

      integer lu
      character key*(*)
      integer value
c  ---------------------------------------------------------------------
c  Write an integer valued fits card.
c
c  Input:
c    lu         File descriptor.
c    key        FITS keyword.
c    value      Value of the keyword.
c-----------------------------------------------------------------------
      character card*80,keyword*8
c-----------------------------------------------------------------------
      keyword = key
      write(card,1000) keyword,value
 1000 format(a,'=     ',i16,'  /')
      call fitcdio(lu,card)

      end

c***********************************************************************

c* FitWrhdr -- Write a real value to a FITS file header.
c& rjs
c: fits
c+
      subroutine fitwrhdr(lu,key,value)

      integer lu
      character key*(*)
      real value
c  ---------------------------------------------------------------------
c  Write a real valued FITS card out.
c
c  Input:
c    lu         File descriptor.
c    key        FITS keyword.
c    value      Value of the keyword.
c-----------------------------------------------------------------------
      character card*80,keyword*8
c-----------------------------------------------------------------------
      keyword = key
      write(card,1000) keyword,value
 1000 format(a,'=   ',1pe18.11,'  /')
      call fitcdio(lu,card)

      end

c***********************************************************************

c* FitWrhdd -- Write a double precision keyword to a FITS file header.
c& rjs
c: fits
c+
      subroutine fitwrhdd(lu,key,value)

      integer lu
      character key*(*)
      double precision value
c  ---------------------------------------------------------------------
c  Write a real valued FITS card out.
c
c  Input:
c    lu         File descriptor.
c    key        FITS keyword.
c    value      Value of the keyword.
c-----------------------------------------------------------------------
      character card*80,keyword*8
c-----------------------------------------------------------------------
      keyword = key
      write(card,1000) keyword,value
 1000 format(a,'=   ',1pe18.11,'  /')
      call fitcdio(lu,card)

      end

c***********************************************************************

      subroutine fitopen(lu,name,status)

      integer lu
      character name*(*),status*(*)
c-----------------------------------------------------------------------
c  Open a fits file
c
c  Input:
c    Name       Name of file to be opened or created.
c    status     'old' or 'new'.
c
c  Output:
c    lu         File handle.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer iostat,i,bitpix,zero3(3)
      logical ok
      logical first

      external fithdini
      logical  fithdini

      save first
      data first/.true./
c-----------------------------------------------------------------------
c  Initialise the current block in the FITS header cache.
c
      if (first) then
        first = .false.
        curlu = -1
        curcard = 0
        do i = 1, maxopen
          opened(i) = .false.
        enddo
      endif
c
c  Open the file.
c
      lu = 0
      do i = 1, maxopen
        if (.not.opened(i)) lu = i
      enddo
      if (lu.eq.0) call bug('f','No fitsio handles left!')
      opened(lu) = .true.
      if (status.eq.'new') then
        new(lu) = .true.
        call haccess(0,item(lu),name,'write',iostat)
      else if (status.eq.'old') then
        new(lu) = .false.
        call haccess(0,item(lu),name,'read',iostat)
      else
        call bug('f','Unrecognised STATUS in FITOPEN')
      endif
      if (iostat.ne.0) call bugno('f',iostat)
c
c  For a new file, write the SIMPLE keyword. For an old file, initialise
c  the header.
c
      ExtNo(lu) = 0
      nExtOff(lu) = 0
      call mpCvtim(HdOff3(1,lu),-1)
      call mpCvtim(zero3,0)
      ok = fithdini(lu,zero3)
      if (.not.ok) call bug('f','Input is not a FITS file')
      if (new(lu)) then
        call fitwrhdl(lu,'SIMPLE',.true.)
        BlankVal(lu) = 0
      else
        call fitrdhdi(lu,'BITPIX',bitpix,0)
        if (bitpix.gt.0) then
          call fitrdhdi(lu,'BLANK',BlankVal(lu),0)
        else
          BlankVal(lu) = -1
        endif
      endif

      end

c***********************************************************************

      subroutine fitsize(lu,size3)

      integer lu,size3(3)
c-----------------------------------------------------------------------
c  Remember the size of the data region.
c-----------------------------------------------------------------------
      include 'fitsio.h'
c-----------------------------------------------------------------------
      call mpSet(DatSize3(1,lu),size3)

      end

c***********************************************************************

      logical function fithdini(lu,off3)

      integer lu,off3(3)
c-----------------------------------------------------------------------
c  Initialise the header of an old file.
c
c  Move to the start of a FITS header block. Determine the size of the
c  header, and the ensuing data. The header blocks offset is given by
c  "off". If "off" is negative, this moves to the next header block.
c
c  Input:
c    lu         The handle of the FITS file.
c    off        Offset of the header block.
c-----------------------------------------------------------------------
      integer offset3(3),totsize3(3),size3(3),bigint(3)
      integer bitpix,gcount,pcount,naxis
      integer iostat,i,axis,rem
      character string*8
      logical found
      include 'fitsio.h'

      external  itoaf, mpCmp, mpSign
      integer   mpCmp, mpSign
      character itoaf*2
c-----------------------------------------------------------------------
c  Determine the offset to the next header block. If this is already
c  the one we are at, just return.
c
      if (mpSign(off3).lt.0) then
        call mpSet(offset3,DatSize3(1,lu))
        call mpAddmi(offset3,2879)
        call mpDivmi(offset3,2880,rem)
        call mpMulmi(offset3,2880)
        call mpAddmm(offset3,DatOff3(1,lu))
c         ... offset = DatOff(lu) + 2880*((DatSize(lu) + 2879)/2880)
      else
        call mpSet(offset3,off3)
c         ... offset = off
      endif
      fithdini = .true.
      if (mpCmp(offset3,HdOff3(1,lu)).eq.0) return
c       ... if(offset.eq.HdOff(lu))return
c
c  If its a new file, then set things to a null state.
c
      if (new(lu)) then
        ncards(lu)  = 0
        call mpSet(HdOff3(1,lu),offset3)
c         ... HdOff(lu)   = offset
        call mpCvtim(HdSize3(1,lu),0)
c         ... HdSize(lu)  = 0
        call mpCvtim(DatSize3(1,lu),0)
c         ... DatSize(lu) = 0
        call mpCvtim(DatOff3(1,lu),0)
c         ... DatOff(lu)  = 0
        if (mpSign(offset3).eq.0) call mpCvtim(DatBase3(1,lu),0)
c         ... if(offset.eq.0) DatBase(lu) = 0
c
c  If its an old header, we have to work out a fair few things.
c
      else
        fithdini = .false.
        call hsize3(totsize3,item(lu))
        if (mpCmp(offset3,totsize3).ge.0) return
c         ... if(offset.ge.totsize) return
c
c  Check if it looks OK.
c
        call hread3b(item(lu),string,offset3,len(string),iostat)
        if (iostat.ne.0) return
        if ((mpSign(offset3).eq.0 .and. string.ne.'SIMPLE') .or.
     *    (mpSign(offset3).ne.0 .and. string.ne.'XTENSION')) return
c
c  If cards for this file are in the card cache, invalidate it.
c
        if (curlu.eq.lu) curlu = -1
c
c  It looks OK. Calculate the number of cards in the header.
c
        call mpSet(HdOff3(1,lu),offset3)
c         ... HdOff(lu) = offset
        call mpSet(HdSize3(1,lu),totsize3)
        call mpSubmm(HdSize3(1,lu),offset3)
c
c  If the size is really big, trim it back
c        
        call mpCvtim(bigint,2**30)
        if(mpCmp(HdSize3(1,lu),bigint).gt.0) 
     *    call mpSet(HdSize3(1,lu),bigint)
        call mpDivmi(HdSize3(1,lu),2880,rem)
        call mpMulmi(HdSize3(1,lu),2880)
c         ... HdSize(lu) = 2880 * ( (totsize - offset) / 2880 )
        ncards(lu) = 0
        call fitsrch(lu,'END',found)
        if (.not.found) call bug('f','Did not find end of FITS header!')
        call mpCvtim(HdSize3(1,lu),80*(ncards(lu)+1))
c         ... HdSize(lu) = 80*(ncards(lu)+1)
        call mpSet(DatOff3(1,lu),HdSize3(1,lu))
        call mpAddmi(DatOff3(1,lu),2879)
        call mpDivmi(DatOff3(1,lu),2880,rem)
        call mpMulmi(DatOff3(1,lu),2880)
        call mpAddmm(DatOff3(1,lu),HdOff3(1,lu))
c         ... DatOff(lu) = HdOff(lu) + 2880*((HdSize(lu) + 2879)/2880)
        if (mpSign(HdOff3(1,lu)).eq.0)
     *                call mpSet(DatBase3(1,lu),DatOff3(1,lu))
c         if(HdOff(lu).eq.0) DatBase(lu) = DatOff(lu)
c
c  Determine the size of the data region.
c
        call fitrdhdi(lu,'BITPIX',bitpix,0)
        call fitrdhdi(lu,'GCOUNT',gcount,1)
        call fitrdhdi(lu,'PCOUNT',pcount,0)
        call fitrdhdi(lu,'NAXIS',naxis,0)

        if (gcount.le.0 .or. pcount.lt.0 .or. naxis.lt.0 .or.
     *    mod(bitpix,8).ne.0 .or. bitpix.eq.0) call bug('f',
     *    'Bad values in fundamental parameter in FITS file')

        if (naxis.eq.0) then
          call mpCvtim(size3,0)
        else
          call mpCvtim(size3,1)
          do i = 1, naxis
            call fitrdhdi(lu,'NAXIS'//itoaf(i),axis,1)
            if (axis.lt.0) call bug('f',
     *         'Bad value in fundamental parameter in FITS file')
            if (i.eq.1) axis = max(axis,1)
            call mpMulmi(size3,axis)
          enddo
        endif

        ncards(lu) = 0
        call mpAddmi(size3,pcount)
        call mpMulmi(size3,gcount)
        call mpMulmi(size3, abs(bitpix)/8)
        call mpSet(DatSize3(1,lu),size3)
c         DatSize(lu) = gcount*abs(bitpix)/8 * (pcount + size)
        call mpSubmm(totsize3,DatOff3(1,lu))
        if (mpCmp(totsize3,DatSize3(1,lu)).lt.0)
     *    call bug('f','Serious inconsistency in file size')
      endif
c
c  All said and done.
c
      fithdini = .true.

      end

c***********************************************************************

      subroutine fithdfin(lu)

      integer lu
c-----------------------------------------------------------------------
c  Finish up the header for a new file.
c
c  Input:
c    lu         File descriptor of an old file.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer l,ltot,iostat,ltotd,rem
      character card*80
      integer k3(3),ktot3(3)

c  External.
      integer mpSign,mpCvtmi
c-----------------------------------------------------------------------
c  Check!
c
      if (.not.new(lu)) call bug('f','Called FITHDFIN for old file')
c
c  Write the END keyword.
c
      card = 'END'
      call fitcdio(lu,card)
c
c  Fix up some pointers.
c
      call mpSet(k3,HdOff3(1,lu))
      call mpAddmi(k3,80*ncards(lu))
c       ... k = 80 * ncards(lu) + HdOff3(lu)
      call mpSet(ktot3,k3)
      call mpAddmi(ktot3,2879)
      call mpDivmi(ktot3,2880,rem)
      call mpMulmi(ktot3,2880)
c       ... ktot = ( ( k - 1 ) / 2880 + 1 ) * 2880
      call mpSet(HdSize3(1,lu),k3)
      call mpSubmm(HdSize3(1,lu),HdOff3(1,lu))
c       ... HdSize(lu) = k - HdOff(lu)
      call mpSet(DatOff3(1,lu),ktot3)
c       ... DatOff(lu) = ktot
      if (mpSign(HdOff3(1,lu)).eq.0)
     *    call mpSet(DatBase3(1,lu),DatOff3(1,lu))
c       ... if(HdOff(lu).eq.0)DatBase(lu) = DatOff(lu)
c
c  Blank fill the end of the header.
c
      call mpSubmm(ktot3,k3)
      ltot = mpCvtmi(ktot3)
      if (ltot.gt.0) then
        ltotd = min(ltot,80*maxcards)
        carray(1:ltotd) = ' '
        do while (ltot.gt.0)
          ltotd = min(ltot,80*maxcards)
          call hwrite3b(item(lu),carray,k3,ltotd,iostat)
          if (iostat.ne.0) call bugno('f',iostat)
          ltot = ltot - ltotd
          call mpAddmi(k3,ltotd)
        enddo
        curlu = -1
      endif
c
c  Zero fill the end of the data area.
c
      call mpSet(k3,DatOff3(1,lu))
      call mpAddmm(k3,DatSize3(1,lu))
c       ... k =  DatOff(lu) + DatSize(lu)
      call mpSet(ktot3,k3)
      call mpAddmi(ktot3,2879)
      call mpDivmi(ktot3,2880,rem)
      call mpMulmi(ktot3,2880)
c       ... ktot = ( ( k - 1 ) / 2880 + 1 ) * 2880
      call mpSubmm(ktot3,k3)
      ltot = mpCvtmi(ktot3)
      if (ltot.ne.2*(ltot/2))
     *  call bug('f','Odd number of bytes in the data section')

      ltot = ltot/2
      do l = 1,min(ltot,maxsize)
        array(l) = 0
      enddo

      do while (ltot.gt.0)
        ltotd = min(ltot,maxsize)
        call hwrite3j(item(lu),array,k3,2*ltotd,iostat)
        if (iostat.ne.0) call bugno('f',iostat)
        ltot = ltot - ltotd
        call mpAddmi(k3,2*ltotd)
      enddo

      end

c***********************************************************************

      subroutine fitclose(lu)

      integer lu
c-----------------------------------------------------------------------
c  Close up a FITS file.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer iostat
c-----------------------------------------------------------------------
c  Mark the file as closed.
c
      opened(lu) = .false.
c
c  If cards for this file are in the card cache, invalidate it.
c
      if (curlu.eq.lu) curlu = -1
c
c  Then simply close up shop.
c
      call hdaccess(item(lu),iostat)
      if (iostat.ne.0) call bugno('f',iostat)

      end

c***********************************************************************

c* FitCdio -- Sequential read or write of a FITS header.
c& rjs
c: fits
c+
      subroutine fitcdio(lu,value)

      integer lu
      character value*(*)
c  ---------------------------------------------------------------------
c  This reads or writes a card at the current card pointer. For an old
c  file, the current pointer is the last card accessed using one of
c  the FITRDHD routines, or the FITSRCH routine. For a new file, the
c  current pointer is always the current end of the header.
c
c  Inputs:
c    lu         File descriptor.
c
c  Input or Output:
c    value      This is the card at the current pointer. If it is a new
c               file, this is an input. If it is an old file, this is an
c               output.
c-----------------------------------------------------------------------
      integer iostat,i
      include 'fitsio.h'
      integer k3(3)

      external mpSign
      integer  mpSign
c-----------------------------------------------------------------------
      if (mpSign(DatOff3(1,lu)).ne.0 .and. new(lu))
     *        call bug('f','Cards written after i/o started')
c
c  Check if the thing we are interested in is in the cache (almost
c  certainly it will be).
c
      call mpSet(k3,HdOff3(1,lu))
      call mpAddmi(k3,80*ncards(lu))
      if (.not.new(lu) .and. lu.eq.curlu .and.
     *  ncards(lu).ge.curcard .and. ncards(lu).lt.curcard+maxcards) then
        i = 80*(ncards(lu)-curcard)
        value = carray(i+1:i+80)
        iostat = 0
c
c  Either a card cache miss, or a write operation. Do it the normal
c  way.
c
      else if (new(lu)) then
        call hwrite3b(item(lu),value,k3,80,iostat)
      else
        call hread3b(item(lu),value,k3,80,iostat)
      endif
      if (iostat.ne.0) call bugno('f',iostat)

      ncards(lu) = ncards(lu) + 1

      end

c***********************************************************************

c* FitSrch -- Search for a keyword in the header of an old FITS file.
c& rjs
c: fits
c+
      subroutine fitsrch(lu,key,found)

      integer lu
      character key*(*)
      logical found
c  ---------------------------------------------------------------------
c  This searches for a keyword in the file header, and leaves the
c  card pointer pointing at the card. If it is not found
c  the pointer points at the END card, and FOUND is returned .false.
c
c  Inputs:
c    lu         File descriptor.
c    key        Keyword to search for.
c
c  Outputs:
c    found      This will be true if the card was found.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer i,cards,iostat,k,ktot,rem
      integer itemp3(3),k3(3)

      external mpCvtmi
      integer  mpCvtmi
c-----------------------------------------------------------------------
c  Check.
c
      if (new(lu)) call bug('f','Searches on new headers not allowed')
c
c  Search for it.
c
      found = .false.
      ncards(lu) = 0
      k = 0
      call mpSet(itemp3,HdSize3(1,lu))
      call mpDivmi(itemp3,80,rem)
      ktot = mpCvtmi(itemp3)
c       ... ktot = HdSize(lu)/80

      do while (k.lt.ktot .and. .not.found)
        cards = min(ktot-k,maxcards)
        if (lu.ne.curlu .or. k.ne.curcard) then
          call mpSet(k3,HdOff3(1,lu))
          call mpAddmi(k3,80*k)
          call hread3b(item(lu),carray,k3,80*cards,iostat)
          if (iostat.ne.0) call bugno('f',iostat)
          curlu = lu
          curcard = k
        endif
c
c  Search thru the cards read in.
c
        i = 0
        do while (.not.found .and. i.lt.80*cards)
          found = carray(i+1:i+8).eq.key
          if (.not.found) i = i + 80
        enddo
        ncards(lu) = ncards(lu) + i/80
        k = k + cards
      enddo

      end

c***********************************************************************

c* ftabLoc -- Locate a table of a particular kind, in a FITS file.
c& rjs
c: fits
c+
      subroutine ftabLoc(lu,name,found)

      integer lu
      character name*(*)
      logical found
c  ---------------------------------------------------------------------
c  This scans a FITS file, looking for a table of a particular kind.
c  If there are multiple versions of a particular table, the first
c  table will be located.
c
c  Input:
c    lu         Handle of the input FITS file.
c    name       Name of the table. This is the value of the EXTNAME
c               FITS keyword. Normally it will be something like:
c                 'AIPS SU ' -- AIPS source table.
c                 'AIPS FQ ' -- AIPS frequency table.
c                 'AIPS CC ' -- AIPS clean component table.
c               A special case is name = ' ', which repositions
c               the fitsio routines to access the main header/data.
c  Output:
c    found      True if the table was sucessfully found.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer zero3(3)

      external fithdini
      logical  fithdini
c-----------------------------------------------------------------------
c  "Rewind" to the main header/data, and then find the next table of
c  interest.
c
      ExtNo(lu) = 0
      if (name.eq.' ') then
        call mpCvtim(zero3,0)
        found = fithdini(lu,zero3)
        if (.not.found) call bug('f','Error reading FITS header')
      else
        call ftabNxt(lu,name,found)
      endif

      end

c***********************************************************************

c* ftabNxt -- Locate the next FITS table of a particular kind.
c& rjs
c: fits
c+
      subroutine ftabNxt(lu,name,found)

      integer lu
      character name*(*)
      logical found
c  ---------------------------------------------------------------------
c  Scan a FITS file from the current position looking for a table of a
c  particular kind.
c
c  Input:
c    lu         Handle of the input FITS file.
c    name       Name of the table. This is the value of the EXTNAME
c               FITS keyword. Normally it will be something like:
c                 'AIPS SU ' -- AIPS source table.
c                 'AIPS FQ ' -- AIPS frequency table.
c                 'AIPS CC ' -- AIPS clean component table.
c               A special case is name = ' ', which just loads
c               the next table.
c  Output:
c    found      True if the table was sucessfully found.
c-----------------------------------------------------------------------
c     Skip to the table.
      call ftabSkip(lu,name,found)

c     If found, load the info about the table.
      if (found) call ftabLoad(lu,found)

      end

c***********************************************************************

c* ftabSkip -- Skip to the next FITS table of a particular kind.
c& rjs
c: fits
c+
      subroutine ftabSkip(lu,name,found)

      integer lu
      character name*(*)
      logical found
c  ---------------------------------------------------------------------
c  Scan a FITS file from the current position looking for a table of a
c  particular kind.
c
c  Input:
c    lu         Handle of the input FITS file.
c    name       Name of the table. This is the value of the EXTNAME
c               FITS keyword. Normally it will be something like:
c                 'AIPS SU ' -- AIPS source table.
c                 'AIPS FQ ' -- AIPS frequency table.
c                 'AIPS CC ' -- AIPS clean component table.
c               A special case is name = ' ', which just loads
c               the next table.
c  Output:
c    found      True if the table was sucessfully found.
c-----------------------------------------------------------------------
      integer offset3(3),mone3(3)
      integer indx
      character ename*16
      include 'fitsio.h'

      external fithdini
      logical  fithdini
c-----------------------------------------------------------------------
c  See whether the desired table is in the index table.
c
      indx = ExtNo(lu)
      found = .false.
      do while (indx.lt.nExtOff(lu) .and. .not.found)
        indx = indx + 1
        found = name.eq.' ' .or. name.eq.ExtName(indx,lu)
      enddo
c
c  If we found it, initialise this header.
c
      if (found) then
        if (.not.fithdini(lu,ExtOff3(1,indx,lu)))
     *    call bug('f','Error reading FITS header')
        ExtNo(lu) = indx
c
c  If we failed to find anything in the index, keep on scanning forward.
c  Add anything more we find to the index, if we have space.
c
c  What is the offset of the last table that we know about.
c
      else
        if (indx.eq.0) then
          call mpCvtim(offset3,0)
        else if (indx.le.nExtOff(lu)) then
          call mpSet(offset3,ExtOff3(1,indx,lu))
        else if (indx.eq.ExtNo(lu)) then
          call mpSet(offset3,HdOff3(1,lu))
        else
          call bug('f','Something screwy, in ftabNxt')
        endif
c
c  Move to the last table, and loop until we have found the right table.
c
        if (.not.fithdini(lu,offset3))
     *    call bug('f','Error reading FITS header')
        call mpCvtim(mone3,-1)
        do while (.not.found)
          if (.not.fithdini(lu,mone3)) return
          indx = indx + 1
          call fitrdhda(lu,'EXTNAME',ename,' ')
          ExtNo(lu) = indx
          if (indx.le.MAXIDX) then
            nExtOff(lu) = nExtOff(lu) + 1
            call mpSet(ExtOff3(1,indx,lu),HdOff3(1,lu))
            ExtName(indx,lu) = ename
          endif
          found = name.eq.ename .or. name.eq.' '
        enddo
      endif

      end

c***********************************************************************

      subroutine ftabLoad(lu,ok)

      integer lu
      logical ok
c-----------------------------------------------------------------------
c  Load the info about the current table.
c
c  Input:
c    lu         Handle of the input FITS file.
c  Output:
c    ok         True if all seems OK.
c-----------------------------------------------------------------------
      character string*16,num*3
      integer offset,i,j,Form,Cnt,ncol
      include 'fitsio.h'

      external  itoaf
      character itoaf*3
c-----------------------------------------------------------------------
      ok = .false.
      call fitrdhda(lu,'XTENSION',string,' ')
      if (string.ne.'A3DTABLE' .and. string.ne.'3DTABLE' .and.
     *   string.ne.'BINTABLE') return
      call fitrdhdi(lu,'NAXIS1',width(lu),0)
      call fitrdhdi(lu,'NAXIS2',rows(lu),0)
      call fitrdhdi(lu,'TFIELDS',ncol,0)

      if (width(lu).le.0 .or. rows(lu).le.0 .or. ncol.le.0) return

      offset = 0
      j = 0
      do i = 1, ncol
        num = itoaf(i)
        call fitrdhda(lu,'TFORM'//num,string,' ')
        call ftabForm(string,Form,Cnt)
        if (Cnt.gt.0) then
          j = j + 1
          if (j.gt.MAXCOL) return
          call fitrdhda(lu,'TTYPE'//num,ColType(j,lu),' ')
          call fitrdhda(lu,'TUNIT'//num,ColUnits(j,lu),' ')
          ColForm(j,lu) = Form
          ColCnt(j,lu) = Cnt
          ColOff(j,lu) = offset
          offset = offset + (ColCnt(j,lu)+7)/8
        endif
      enddo

      if (j.le.0) return
      cols(lu) = j

c     Consistency check.
      if (offset.gt.width(lu))
     *  call bug('f','Table width inconsistency')

      ok = .true.

      end

c***********************************************************************

      subroutine ftabForm(string,ColForm,ColCnt)

      character string*(*)
      integer ColForm,ColCnt
c-----------------------------------------------------------------------
c  Decode the FITS way of specifying a type and count into a "form"
c  and byte count.
c
c  Input:
c    string     A string in the form of <number><character>, e.g. "1J".
c  Output:
c    ColForm    The form type.
c    ColCnt     The form count, in bits.
c-----------------------------------------------------------------------
      integer i
      character c*1
      logical more

      integer ftabSize
c-----------------------------------------------------------------------
      ColCnt = 0
      i = 1
      more = .true.
      do while (i.le.len(string) .and. more)
        if (string(i:i).ge.'0' .and. string(i:i).le.'9') then
          ColCnt = 10*ColCnt + ichar(string(i:i)) - ichar('0')
          i = i + 1
        else
          more = .false.
        endif
      enddo
      if (i.eq.1) then
        ColCnt = 1
      else if (i.gt.len(string) .or. ColCnt.lt.0) then
        call bug('f','Bad FORM string in FITS table description')
      endif

      c = string(i:i)
      ColForm = index('IJAEDXLCMP',c)
      if (ColForm.eq.0) call bug('f',
     *  'Bad FORM string in FITS table description')

      ColCnt = ColCnt*ftabSize(ColForm)

      end

c***********************************************************************

c* ftabInfo -- Return information about a value in the current table.
c& rjs
c: fits
c+
      subroutine ftabInfo(lu,name,type,units,nrow,nval)

      integer lu,nrow,nval
      character name*(*),type*1,units*(*)
c  ---------------------------------------------------------------------
c  Determine information about a parameter, which is hoped to be
c  in this table.
c
c  Input:
c    lu         Handle of the input FITS file.
c    name       Name of the parameter.
c  Output:
c    type       Data type -- either A, I, R or D.
c    units      Units of the parameter.
c    nrows      Number of rows (will be the same for all parameters
c               from the same table).
c    nval       Number of values in each row.
c-----------------------------------------------------------------------
      integer i

      character string*10
      include 'fitsio.h'

      external ftabColn, ftabSize
      integer  ftabColn, ftabSize

      data string/'IIARDXLCMP'/
c-----------------------------------------------------------------------

      i = ftabColn(lu,name)
      if (i.eq.0) then
c       Not matched, return indicating that it was not found.
        type = ' '
        units = ' '
        nrow = 0
        nval = 0

      else
c       Found a match, return info about it.
        type = string(ColForm(i,lu):ColForm(i,lu))
        units = ColUnits(i,lu)
        nrow = rows(lu)
        nval = ColCnt(i,lu)/ftabSize(ColForm(i,lu))
      endif

      end

c***********************************************************************

c* ftabGetI -- Return integer valued data from a FITS table.
c& rjs
c: fits
c+
      subroutine ftabGetI(lu,name,irow,data)

      integer lu,data(*),irow
      character name*(*)
c  ---------------------------------------------------------------------
c  Get integer data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c  Output:
c    data       The data values.
c-----------------------------------------------------------------------
      integer i,j,iostat,size,idx,ifirst,ilast
      character umsg*64
      integer offset3(3)
      include 'fitsio.h'

      external ftabColn, ftabSize
      integer  ftabColn, ftabSize
c-----------------------------------------------------------------------
c  Find this parameter in the table.
c
      i = ftabColn(lu,name)
      if (i.le.0) then
        umsg = 'FITS table does not have the parameter: '//name
        call bug('f',umsg)
      endif
c
c  Is this a short or long integer?
c
      if (ColForm(i,lu).ne.FormI .and. ColForm(i,lu).ne.FormJ) then
        umsg = 'Cannot convert FITS table parameter '//
     *        name//' to integer'
        call bug('f',umsg)
      endif
c
c  Does row exist?
c
      if (irow.gt.rows(lu)) then
        umsg = 'Requested row does not exist'
        call bug('f',umsg)
      endif
c
      size = ftabSize(ColForm(i,lu))
c
c  All is OK. So just read the data.
c
      idx = 1
      call mpSet(offset3,DatOff3(1,lu))
      call mpAddmi(offset3,ColOff(i,lu))
c       ... offset = DatOff(lu) + ColOff(i,lu)
      if (irow.lt.1) then
        ifirst = 1
        ilast = rows(lu)
      else
        ifirst = irow
        ilast = irow
        call mpAddmi(offset3,(irow-1)*width(lu))
c         ... offset = offset + (irow-1)*width(lu)
      endif
      do j = ifirst, ilast
        if (ColForm(i,lu).eq.FormJ) then
          call hread3j(item(lu),data(idx),offset3,ColCnt(i,lu)/8,
     *                                                        iostat)
        else if (ColForm(i,lu).eq.FormI) then
          call hread3i(item(lu),data(idx),offset3,ColCnt(i,lu)/8,
     *                                                        iostat)
        endif
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + ColCnt(i,lu)/size
        call mpAddmi(offset3,width(lu))
      enddo

      end

c***********************************************************************

c* ftabGetR -- Get real data from the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabGetR(lu,name,irow,data)

      integer lu, irow
      real data(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get real data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return
c    irow       Row number to return (0=all)
c  Output:
c    data       The data values.
c-----------------------------------------------------------------------
      integer i,j,iostat,size,idx,ifirst,ilast,ncol
      character umsg*64
      integer offset3(3)
      include 'fitsio.h'

      external ftabColn, ftabSize
      integer  ftabColn, ftabSize
c-----------------------------------------------------------------------
c  Find this parameter in the table.
c
      i = ftabColn(lu,name)
      if (i.le.0) then
        umsg = 'FITS table does not have the parameter: '//name
        call bug('f',umsg)
      endif
c
c  Is this a real?
c
      if (ColForm(i,lu).ne.FormE) then
        umsg = 'Cannot convert FITS table parameter '//
     *        name//' to real'
        call bug('f',umsg)
      endif
c
c  Does row exist?
c
      if (irow.gt.rows(lu)) then
        umsg = 'Requested row does not exist'
        call bug('f',umsg)
      endif

      size = ftabSize(ColForm(i,lu))
      ncol = ColCnt(i,lu)/size
      if (ncol.gt.MAXSIZE) call bug('f','Too many column values')
c
c  All is OK. So just read the data.
c
      idx = 1
      call mpSet(offset3,DatOff3(1,lu))
      call mpAddmi(offset3,ColOff(i,lu))
c       ... offset = DatOff(lu) + ColOff(i,lu)
      if (irow.lt.1) then
        ifirst = 1
        ilast = rows(lu)
      else
        ifirst = irow
        ilast = irow
        call mpAddmi(offset3,(irow-1)*width(lu))
c          ... offset = offset + (irow-1)*width(lu)
      endif
      do j = ifirst, ilast
        call hread3r(item(lu),data(idx),offset3,ColCnt(i,lu)/8,
     *                                                    iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif

        call hread3i(item(lu),array,offset3,ColCnt(i,lu)/8,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        call fnanflag(data(idx),array,ncol)

        idx = idx + ncol
        call mpAddmi(offset3,width(lu))
      enddo

      end

c***********************************************************************

      subroutine fnanflag(data,flag,n)

      integer n,flag(n)
      real data(n)
c-----------------------------------------------------------------------
c  Set a real value to zero if the value in flag represents the bit
c  pattern of a NaN value.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        if (
     *     (2139095040.le.flag(i) .and. flag(i).le.2147483647) .or.
     *     (-8388608 .le.flag(i) .and. flag(i).le.-1)
     *    ) data(i) = 0
      enddo

      end

c***********************************************************************

c* ftabGetC -- Get complex data from the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabGetC(lu,name,irow,data)

      integer lu, irow
      complex data(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get real data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return
c    irow       Row number to return (0=all)
c  Output:
c    data       The data values.
c-----------------------------------------------------------------------
      integer i,j,iostat,size,idx,ifirst,ilast
      character umsg*64
      integer offset3(3)
      include 'fitsio.h'

      external ftabColn, ftabSize
      integer  ftabColn, ftabSize
c-----------------------------------------------------------------------
c  Find this parameter in the table.
c
      i = ftabColn(lu,name)
      if (i.le.0) then
        umsg = 'FITS table does not have the parameter: '//name
        call bug('f',umsg)
      endif
c
c  Is this complex-valued?
c
      if (ColForm(i,lu).ne.FormC) then
        umsg = 'Cannot convert FITS table parameter '//
     *        name//' to complex'
        call bug('f',umsg)
      endif
c
c  Does row exist?
c
      if (irow.gt.rows(lu)) call bug('f','Requested row does not exist')

      size = ftabSize(ColForm(i,lu))
c
c  All it OK. So just read the data.
c
      idx = 1
      call mpSet(offset3,DatOff3(1,lu))
      call mpAddmi(offset3,ColOff(i,lu))
c       ... offset = DatOff(lu) + ColOff(i,lu)
      if (irow.lt.1) then
        ifirst = 1
        ilast = rows(lu)
      else
        ifirst = irow
        ilast = irow
        call mpAddmi(offset3,(irow-1)*width(lu))
c         ... offset = offset + (irow-1)*width(lu)
      endif
      do j = ifirst, ilast
        call hread3r(item(lu),data(idx),offset3,ColCnt(i,lu)/8,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + ColCnt(i,lu)/size
        call mpAddmi(offset3,width(lu))
      enddo

      end

c***********************************************************************

c* ftabGetD -- Get double precision data from the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabGetD(lu,name,irow,data)

      integer lu, irow
      double precision data(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get double precision data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c  Output:
c    data       The data values.
c-----------------------------------------------------------------------
      integer i,j,iostat,size,idx,ifirst,ilast
      character umsg*64
      integer offset3(3)
      include 'fitsio.h'

      external ftabColn, ftabSize
      integer  ftabColn, ftabSize
c-----------------------------------------------------------------------
c  Find this parameter in the table.
c
      i = ftabColn(lu,name)
      if (i.le.0) then
        umsg = 'FITS table does not have the parameter: '//name
        call bug('f',umsg)
      endif
c
c  Is this a double precision?
c
      if (ColForm(i,lu).ne.FormD) then
        umsg = 'Cannot convert FITS table parameter '//
     *        name//' to double precision'
        call bug('f',umsg)
      endif
c
c  Does row exist?
c
      if (irow.gt.rows(lu)) then
        umsg = 'Requested row does not exist'
        call bug('f',umsg)
      endif

      size = ftabSize(ColForm(i,lu))
c
c  All it OK. So just read the data.
c
      idx = 1
      call mpSet(offset3,DatOff3(1,lu))
      call mpAddmi(offset3,ColOff(i,lu))
c       ... offset = DatOff(lu) + ColOff(i,lu)
      if (irow.lt.1) then
        ifirst = 1
        ilast = rows(lu)
      else
        ifirst = irow
        ilast = irow
        call mpAddmi(offset3,(irow-1)*width(lu))
c       ... offset = offset + (irow-1)*width(lu)
      endif
      do j = ifirst, ilast
        call hread3d(item(lu),data(idx),offset3,ColCnt(i,lu)/8,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + ColCnt(i,lu)/size
        call mpAddmi(offset3,width(lu))
      enddo

      end

c***********************************************************************

c* ftabGeta -- Get ascii data from the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabGeta(lu,name,irow,data)

      integer lu, irow
      character data(*)*(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get ascii data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c  Output:
c    data       The data values.
c-----------------------------------------------------------------------
      integer i,j,iostat,length,ifirst,ilast
      character umsg*64
      integer offset3(3)
      include 'fitsio.h'

      external ftabColn
      integer  ftabColn
c-----------------------------------------------------------------------
c  Find this parameter in the table.
c
      i = ftabColn(lu,name)
      if (i.le.0) then
        umsg = 'FITS table does not have the parameter: '//name
        call bug('f',umsg)
      endif
c
c  Is this a character?
c
      if (ColForm(i,lu).ne.FormA) then
        umsg = 'Cannot convert FITS table parameter '//
     *        name//' to character'
        call bug('f',umsg)
      endif
c
c  Determine the length to read each time.
c
      length = min(len(data(1)),ColCnt(i,lu)/8)
c
c  Does row exist?
c
      if (irow.gt.rows(lu)) then
        umsg = 'Requested row does not exist'
        call bug('f',umsg)
      endif
c
c  All it OK. So just read the data.
c
      call mpSet(offset3,DatOff3(1,lu))
      call mpAddmi(offset3,ColOff(i,lu))
c       ... offset = DatOff(lu) + ColOff(i,lu)
      if (irow.lt.1) then
        ifirst = 1
        ilast = rows(lu)
      else
        ifirst = irow
        ilast = irow
        call mpAddmi(offset3,(irow-1)*width(lu))
      endif
      do j = ifirst, ilast
        if (length.lt.len(data(j))) data(j-ifirst+1) = ' '
        call hread3b(item(lu),data(j-ifirst+1),offset3,length,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        call mpAddmi(offset3,width(lu))
      enddo

      end

c***********************************************************************

c* ftabColn -- Determine the column number of a particular table entry.
c& rjs
c: fits
c+
      integer function ftabColn(lu,name)

      integer lu
      character name*(*)
c  ---------------------------------------------------------------------
c  Determine the column number of a particular table entry.
c
c  Input:
c    lu         FITS file handle.
c    name       Name of the parameter.
c  Output:
c    ftabColn   Column number.
c-----------------------------------------------------------------------
      integer i,l
      logical more
      include 'fitsio.h'

      external len1
      integer  len1
c-----------------------------------------------------------------------
c     Search until we find this one.
      l = min(len(ColType(1,lu)), max(8,len1(name)))
      i = 0
      more = .true.
      do while (i.lt.cols(lu) .and. more)
        i = i + 1
        more = name.ne.ColType(i,lu)(1:l)
      enddo

      if (more) i=0
      ftabColn = i

      end

c***********************************************************************

      integer function ftabSize(Form)

      integer Form
c-----------------------------------------------------------------------
c  Determine the size, in bytes, of an element of data, given its
c  format.
c
c  Input:
c    Form       One of FormI,FormJ,... etc
c  Output:
c    ftabSize   Size of one of the elements, in bits.
c-----------------------------------------------------------------------
      include 'fitsio.h'

      integer FormSize(NForms)
      save FormSize
      data FormSize(FormJ)/16/
      data FormSize(FormI)/32/
      data FormSize(FormA)/ 8/
      data FormSize(FormE)/32/
      data FormSize(FormD)/64/
      data FormSize(FormX)/ 1/
      data FormSize(FormL)/ 8/
      data FormSize(FormC)/64/
      data FormSize(FormM)/128/
      data FormSize(FormP)/ 64/
c-----------------------------------------------------------------------
      ftabSize = FormSize(Form)

      end

c***********************************************************************

c* ftabdini -- Start definition of new output table.
c& rjs
c: fits
c+
      subroutine ftabdini(lu,ename)

      integer lu
      character ename*(*)
c  ---------------------------------------------------------------------
c  Start the definition of an output FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    ename      Name of the extension table.
c    nrows      Total number of rows in the table.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer indx
      integer mone3(3)

      external fithdini
      logical  fithdini
c-----------------------------------------------------------------------
      if (.not.new(lu))
     *  call bug('f','Cannot add a table to an old file')
      call mpCvtim(mone3,-1)
      if (.not.fithdini(lu,mone3))
     *  call bug('f','Something is very screwy in ftabdini')
      nExtOff(lu) = 1
      ExtNo(lu) = 1
      indx = ExtNo(lu)

      call mpSet(ExtOff3(1,indx,lu),HdOff3(1,lu))
      ExtName(indx,lu) = ename
      rows(lu) = 0
      width(lu) = 0
      cols(lu) = 0

      end

c***********************************************************************

c* ftabdfin -- Finish definition of a new output table.
c& rjs
c: fits
c+
      subroutine ftabdfin(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Finish the definition of a new output FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer j,l,indx,size
      character string*8,num*8,types*10
      integer size3(3)

      external  ftabsize, itoaf, len1
      integer   ftabsize, len1
      character itoaf*8

      data types/'IJAEDXLCMP'/
c-----------------------------------------------------------------------
      if (.not.new(lu))
     *  call bug('f','Cannot add a table to an old file')
      if (cols(lu).le.0)
     *  call bug('f','Invalid number of columns, in ftabdfin')
      indx = ExtNo(lu)
      call mpCvtim(size3,width(lu))
      call mpMulmi(size3,rows(lu))
      call fitsize(lu,size3)
      call fitwrhda(lu,'XTENSION','BINTABLE')
      call fitwrhdi(lu,'BITPIX',8)
      call fitwrhdi(lu,'NAXIS',2)
      call fitwrhdi(lu,'NAXIS1',width(lu))
      call fitwrhdi(lu,'NAXIS2',rows(lu))
      call fitwrhdi(lu,'PCOUNT',0)
      call fitwrhdi(lu,'GCOUNT',1)
      call fitwrhdi(lu,'TFIELDS',cols(lu))

      do j = 1, cols(lu)
        num = itoaf(j)
        call fitwrhda(lu,'TTYPE'//num,ColType(j,lu))
        size = ftabSize(ColForm(j,lu))
        string = itoaf(ColCnt(j,lu)/size)
        l = len1(string)
        string(l+1:) = types(ColForm(j,lu):ColForm(j,lu))
        call fitwrhda(lu,'TFORM'//num,string)
        call fitwrhda(lu,'TUNIT'//num,ColUnits(j,lu))
      enddo
      call fitwrhda(lu,'EXTNAME',ExtName(indx,lu))
      call fitwrhdi(lu,'EXTVER', 1)

      end

c***********************************************************************

c* ftabdef -- Define a column in the new output table.
c& rjs
c: fits
c+
      subroutine ftabdef(lu,name,type,units,nrow,nval)

      integer lu
      character name*(*),type*1,units*(*)
      integer nrow,nval
c  ---------------------------------------------------------------------
c  Define a column in a new output FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the column.
c    type       Datatype of the column.
c    units      Units of the column.
c    nrow       Number of rows.
c    nval       Number of values.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer size

      external ftabsize
      integer  ftabsize
c-----------------------------------------------------------------------
      if (.not.new(lu))
     *  call bug('f','Cannot add a table to an old file')

      if (rows(lu).eq.0) then
        rows(lu) = nrow
        if (nrow.le.0)
     *    call bug('f','Invalid number of rows in output table')
      else if (rows(lu).ne.nrow) then
        call bug('f','The number of rows in a table must be constant')
      endif
      cols(lu) = cols(lu) + 1
      if (cols(lu).gt.MAXCOL)
     *  call bug('f','Too many columns in output table')
      ColType(cols(lu),lu) = name
      ColUnits(cols(lu),lu) = units
      ColForm(cols(lu),lu) = index('JIARDXLCMP',type)
      if (ColForm(cols(lu),lu).eq.0)
     *  call bug('f','Invalid data type in output table')
      size = ftabSize(ColForm(cols(lu),lu))
      ColOff(Cols(lu),lu) = width(lu)
      ColCnt(Cols(lu),lu) = nval*size
      if (nval.lt.0)
     *  call bug('f','Invalid number of elements in output table')
      width(lu) = width(lu) + (size*nval+7)/8

      end

c***********************************************************************

c* ftabputr -- Put real data into the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabputr(lu,name,irow,data)

      integer lu, irow
      real data(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get ascii data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c    data       The data values.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer type,ifirst,ilast,length,wide,inc,idx,iostat,i
      integer offset3(3)
c-----------------------------------------------------------------------
      call ftabput(lu,name,irow,type,ifirst,ilast,inc,offset3,length,
     *                                                          wide)

      if (type.ne.FormE)
     *  call bug('f','Incompatible data type in ftabputr')

      idx = 1
      do i = ifirst, ilast
        call hwrite3r(item(lu),data(idx),offset3,length,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + inc
        call mpAddmi(offset3,wide)
      enddo

      end

c***********************************************************************

c* ftabputd -- Put double precision data into the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabputd(lu,name,irow,data)

      integer lu, irow
      double precision data(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get ascii data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c    data       The data values.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer type,ifirst,ilast,length,wide,inc,idx,iostat,i
      integer offset3(3)
c-----------------------------------------------------------------------
      call ftabput(lu,name,irow,type,ifirst,ilast,inc,offset3,length,
     *                                                          wide)

      if (type.ne.FormD)
     *  call bug('f','Incompatible data type in ftabputd')

      idx = 1
      do i = ifirst, ilast
        call hwrite3d(item(lu),data(idx),offset3,length,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + inc
        call mpAddmi(offset3,wide)
      enddo

      end

c***********************************************************************

c* ftabputi -- Put integer data into the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabputi(lu,name,irow,data)

      integer lu, irow
      integer data(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get ascii data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c    data       The data values.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer type,ifirst,ilast,length,wide,inc,idx,iostat,i
      integer offset3(3)
c-----------------------------------------------------------------------
      call ftabput(lu,name,irow,type,ifirst,ilast,inc,offset3,length,
     *                                                          wide)

      if (type.ne.FormJ .and. type.ne.FormI)
     *  call bug('f','Incompatible data type in ftabputi')

      idx = 1
      do i = ifirst, ilast
        if (type.eq.FormJ) then
          call hwrite3j(item(lu),data(idx),offset3,length,iostat)
        else
          call hwrite3i(item(lu),data(idx),offset3,length,iostat)
        endif
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + inc
        call mpAddmi(offset3,wide)
      enddo

      end

c***********************************************************************

c* ftabputa -- Put ascii data into the current FITS table.
c& rjs
c: fits
c+
      subroutine ftabputa(lu,name,irow,data)

      integer lu, irow
      character data(*)*(*)
      character name*(*)
c  ---------------------------------------------------------------------
c  Get ascii data from the current FITS table.
c
c  Input:
c    lu         Handle of the FITS file.
c    name       Name of the parameter to return.
c    irow       Row number to return (0=all)
c    data       The data values.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer type,ifirst,ilast,length,wide,inc,idx,iostat,i
      integer offset3(3)
c-----------------------------------------------------------------------
      call ftabput(lu,name,irow,type,ifirst,ilast,inc,offset3,length,
     *                                                          wide)

      if (type.ne.FormA)
     *  call bug('f','Incompatible data type in ftabputa')
      if (len(data(1)).ne.inc)
     *  call bug('f','Incompatible data length in ftabputa')

      idx = 1
      do i = ifirst, ilast
        call hwrite3b(item(lu),data(idx),offset3,length,iostat)
        if (iostat.ne.0) then
          call bug('w','I/O error while reading FITS table')
          call bugno('f',iostat)
        endif
        idx = idx + 1
        call mpAddmi(offset3,wide)
      enddo

      end

c***********************************************************************

      subroutine ftabput(lu,name,irow,type,ifirst,ilast,inc,
     *                                        offset3,length,wide)

      integer lu,irow,type,ifirst,ilast,inc,length,wide
      integer offset3(3)
      character name*(*)
c-----------------------------------------------------------------------
c  Stuff common to the ftabput routines.
c-----------------------------------------------------------------------
      include 'fitsio.h'
      integer i,size
      character umsg*64

      external ftabcoln, ftabsize, mpSign
      integer  ftabcoln, ftabsize, mpSign
c-----------------------------------------------------------------------
c  Check that it is the right sort of operation for this file.
c
      if (.not.new(lu)) call bug('f','Cannot write old FITS file')
c
c  If its a new file, and this is the first call to perform data i/o on
c  it (not header i/o), handle the header properly.
c
      if (mpSign(DatOff3(1,lu)).eq.0) call fithdfin(lu)
c
c  Find this parameter in the table.
c
      i = ftabColn(lu,name)
      if (i.le.0) then
        umsg = 'FITS table does not have the parameter: '//name
        call bug('f',umsg)
      endif

      type = ColForm(i,lu)
c
c  Does row exist?
c
      if (irow.gt.rows(lu)) then
        umsg = 'Requested row does not exist'
        call bug('f',umsg)
      endif

      size = ftabSize(ColForm(i,lu))
c
c  All it OK. So just read the data.
c
      call mpSet(offset3,DatOff3(1,lu))
      call mpAddmi(offset3,ColOff(i,lu))
c       ... offset = DatOff(lu) + ColOff(i,lu)
      if (irow.lt.1) then
        ifirst = 1
        ilast = rows(lu)
      else
        ifirst = irow
        ilast = irow
        call mpAddmi(offset3,(irow-1)*width(lu))
      endif
      inc = ColCnt(i,lu)/size
      length = ColCnt(i,lu)/8
      wide = width(lu)

      end

c***********************************************************************

      subroutine fitdate(lu,keyw,jday)

      character keyw*(*)
      integer lu
      double precision jday
c-----------------------------------------------------------------------
c  Decode a FITS date keyword. Before doing so, check that the date is
c  valid.
c-----------------------------------------------------------------------
      character string*64

      external fitcdate
      logical  fitcdate
c-----------------------------------------------------------------------
      call fitrdhda(lu,keyw,string,' ')
      if (fitcdate(string)) then
        call dayjul(string,jday)
      else
        if (string.ne.' ')
     *    call bug('w','Failed to decode date string: '//string)
        jday = 0
      endif

      end

c***********************************************************************

      logical function fitcdate(string)

      character string*(*)
c-----------------------------------------------------------------------
      integer k1,k2,ndigit,nloop
      logical ok

      external len1
      integer  len1
c-----------------------------------------------------------------------
      k1 = 1
      k2 = len1(string)
      call spanchar(string,k1,k2,' ')

      call fitsnum(string,k1,k2,ndigit)
      if ((ndigit.eq.1 .or. ndigit.eq.2) .and. k1.lt.k2) then
        ok = string(k1:k1).eq.'/'
        if (ok) then
          k1 = k1 + 1
          call fitsnum(string,k1,k2,ndigit)
          ok = k1.lt.k2 .and. (ndigit.eq.1 .or. ndigit.eq.2)
          if (ok) ok = string(k1:k1).eq.'/'
          if (ok) then
            k1 = k1 + 1
            call fitsnum(string,k1,k2,ndigit)
            ok = k1.gt.k2 .and. ndigit.eq.2
          endif
        endif
      else if (ndigit.eq.4 .and. k1.lt.k2) then
        ok = string(k1:k1).eq.'-'
        if (ok) then
          k1 = k1 + 1
          call fitsnum(string,k1,k2,ndigit)
          ok = k1.lt.k2 .and. (ndigit.eq.1 .or. ndigit.eq.2)
          if (ok) ok = string(k1:k1).eq.'-'
          if (ok) then
            k1 = k1 + 1
            call fitsnum(string,k1,k2,ndigit)
            ok = ndigit.eq.1 .or. ndigit.eq.2
            if (ok .and. k1.lt.k2) then
              ok = string(k1:k1).eq.'t' .or. string(k1:k1).eq.'T'
              nloop = 3
              do while (ok .and. nloop.gt.0)
                k1 = k1 + 1
                ok = k1.le.k2
                if (ok) then
                  call fitsnum(string,k1,k2,ndigit)
                  ok = ndigit.eq.1 .or. ndigit.eq.2
                  if (k1.gt.k2) then
                    nloop = 0
                  else
                    nloop = nloop - 1
                    if (nloop.eq.0 .and. k1.le.k2) then
                      ok = string(k1:k1).eq.'.'
                      k1 = k1 + 1
                      call fitsnum(string,k1,k2,ndigit)
                      ok = k1.gt.k2
                    else
                      ok = string(k1:k1).eq.':' .and. nloop.gt.0
                    endif
                  endif
                endif
              enddo
            else if (k1.eq.k2) then
              ok = .false.
            endif
          endif
        endif
      else
        ok = .false.
      endif

      fitcdate = ok

      end

c***********************************************************************

      subroutine fitsnum(string,k1,k2,ndigit)

      character string*(*)
      integer k1,k2,ndigit
c-----------------------------------------------------------------------
      logical more
c-----------------------------------------------------------------------
      more = .true.
      ndigit = 0
      do while (k1.le.k2 .and. more)
        if (string(k1:k1).ge.'0' .and. string(k1:k1).le.'9') then
          ndigit = ndigit + 1
          k1 = k1 + 1
        else
          more = .false.
        endif
      enddo

      end

c***********************************************************************
c* fBasAnt - determine antennas from baseline number.
c& jm
c: FITS i/o
c+
      subroutine fbasant(bl, ant1, ant2, config)

      integer ant1, ant2,config
      real bl
c  ---------------------------------------------------------------------
c  fBasAnt is a Miriad routine that returns the antenna numbers that are
c  required to produce the input baseline number.
c
c  This uses an extension of the FITS convention to handle antenna
c  numbers up to 2047.  The relationship between the baseline and the
c  antenna numbers is defined as either
c    baseline = (Ant1 * 256) + Ant2.  (when ant1,ant2 < 256)
c  or
c    baseline = (Ant1 * 2048) + Ant2 + 65536. (otherwise)
c
c  See also: basant
c
c  Input:
c    bl       The baseline number.
c  Output:
c    ant1     The first antenna number.
c    ant2     The second antenna number.
c    config   Configuration number.
c-----------------------------------------------------------------------
      integer mant
c-----------------------------------------------------------------------
      ant2 = int(bl + 0.01)
      config = nint(100*(bl-ant2)) + 1
      if (ant2.gt.65536) then
        ant2 = ant2 - 65536
        mant = 2048
      else
        mant = 256
      endif
      ant1 = ant2 / mant
      ant2 = ant2 - (ant1 * mant)

      if (max(ant1,ant2).ge.mant) then
        ant1 = 0
        ant2 = 0
        config = 0
      endif

      end

c***********************************************************************

      subroutine fantbas(i1,i2,config,bl)

      integer i1,i2,config
      real bl
c-----------------------------------------------------------------------
c  Determine the baseline number of a pair of antennas.
c
c  See also: antbas
c-----------------------------------------------------------------------
      if (max(i1,i2).gt.255) then
        bl = 2048*i1 + i2 + 65536 + 0.01*(config-1)
      else
        bl =  256*i1 + i2 + 0.01*(config-1)
      endif

      end
