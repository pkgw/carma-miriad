      program avmaths

c= avmaths - Operate on cube with averaged plane from cube
c& nebk
c: analysis
c+
c       AVMATHS averages designated planes from a cube, and then
c       performs some mathematical operation on the cube with the
c       averaged plane.  Subtraction, optical depth, replacement,
c       and multiplication operations have been coded.  Undefined
c       output pixels are blanked.
c
c@ in
c       The input image. Wild card expansion is supported. No default.
c@ out
c       The output image. No default.
c@ region
c       Specify the channels to average with a REGION=IMAGE
c       command such as
c         region=image(1,5),image(120,128)
c       This would average channels 1:5 and 120:128 from the
c       cube.  No other region commands are accepted for example
c       spatial sub-regions will be ignored.
c@ options
c       Task enrichment options.  Minimum match is active.
c
c       "subtract"  OUT(i,j,k) = IN(i,j,k) - AV(i,j)
c       "odepth"    OUT(i,j,k) = ln(AV(i,j) / IN(i,j,k))
c       "replace"   OUT(i,j)   = AV(i,j)
c       "multiply"  OUT(i,j,k) = IN(i,j,k) * AV(i,j)
c
c       "noreduce" causes the output image to be of the same dimensions
c           as the input image when REPLACEMENT is invoked.  By default,
c           the REPLACED output image is reduced to two dimensions as
c           there is probably no point to replicating one plane N times.
c           In this case, the output third axis descriptors reflect the
c           size of the bounding box of the selected region on the third
c           axis
c
c       Pixels are blanked if the input pixel is blanked, the averaged
c       channel pixel is blanked, or the output is undefined.
c
c$Id$
c--
c  History:
c    nebk 26jul90 Original version.
c    rjs  25oct90 Merged source file and the documentation.
c    nebk 20jan91 Deal correctly with blanks
c    nebk 20jan91 Merge ODEPTH and AVSUB into AVMATHS
c    nebk  5mar91 Change itoa to itoaf
c    nebk 12mar91 Change KEYA to KEYF for input file
c    mchw 26mar91 Fixed a bug in plane counter "pinc"
c                 Added pbfwhm and bunit header items.
c    nebk 27jan92 Moved location of copying of bunit code.
c    rjs  10mar92 Increased size of maxruns.
c    nebk 22jun92 Add OPTIONS=REPLACE,NOREDUCE, adapt to MEMALLOC
c                 and add BTYPE
c    mjs  12mar93 Use maxnax.h rather than setting maxnax=5
c    nebk 03jun94 CLarify use of region keyword
c    nebk 28jun94 Add multiply option
c    nebk 04jan96 With options=replace, make third axis descriptors
c                 reflect size of the selected region on the third axis
c    nebk 06may97 Comment out the "INcluding plane ..." message
c    rjs  02jul97 cellscal change.
c    rjs  23jul97 added pbtype.
c    rjs  02apr98 Increase maxruns.
c    mchw 08sep14 Replace MAXPLANE with MAXCHAN
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer MAXBOXES, MAXRUNS
      parameter (MAXBOXES = 1024, MAXRUNS = 9*MAXDIM)

      logical   domul, dood, dored, dorepl, dosub, flags(MAXDIM), more
      integer   avpnt, blc(MAXNAX), boxes(MAXBOXES), i, iend(MAXCHAN),
     *          isnext, istart(MAXCHAN), k, lin, lout, naxis, nplanes,
     *          npnt, nruns, nsect, planes(MAXCHAN), runs(3,MAXRUNS),
     *          size(MAXNAX), size3, trc(MAXNAX), xblc, xtrc, yblc, ytrc
      real      buffer(MAXBUF), rline(MAXDIM)
      double precision cdelt3, zav
      character aline*72, in*80, out*80, str*1, version*80

      character itoaf*1, versan*80
      external  itoaf, versan

      common buffer
c-----------------------------------------------------------------------
      version = versan('avmaths',
     *                 '$Revision$',
     *                 '$Date$')
c
c  Get the input parameters.
c
      call keyini
      call keyf ('in', in, ' ')
      call keya ('out', out, ' ')
      if (in.eq.' ' .or. out.eq.' ')
     *    call bug ('f', 'You must specify the input and output files')
      call boxinput ('region', in, boxes, MAXBOXES)
      call getopt (dosub, dood, dorepl, dored, domul)
      call keyfin
c
c  Open the input image and pass some information to the box routines
c
      call xyopen (lin, in, 'old', MAXNAX, size)
c
c Allocate memory for images
c
      call memalloc (avpnt, size(1)*size(2), 'r')
      call memalloc (npnt,  size(1)*size(2), 'r')
c
c Deal partly with region
c
      call rdhdi (lin, 'naxis', naxis, 0)
      if (naxis.lt.3) call bug ('f', 'Image only has 2 dimensions')
      call boxmask (lin, boxes, MAXBOXES)
      call boxset (boxes, MAXNAX, size,' ')
c
c  Find region of image which contains all channels to average
c  and then work out which planes of those specified are not
c  completely blank (pointless to include those).
c
      call boxinfo (boxes, MAXNAX, blc, trc)
      nplanes = 0
      do k = blc(3), trc(3)
        call boxruns (1, k, ' ', boxes, runs, MAXRUNS,
     *                nruns, xblc, xtrc, yblc, ytrc)

        if (nruns.ne.0) then
           nplanes = nplanes + 1
           if (nplanes.gt.MAXCHAN)
     *       call bug ('f', 'Too many channels to average')
           planes(nplanes) = k
        endif
      enddo
c
c  Create the output image
c
      size3 = size(3)
      if (dored) then
        do i = 3, MAXNAX
          if (size(i).gt.1) then
            str = itoaf(i)
            call output ('Reducing axis '//str//' size to 1')
            size(i) = 1
          endif
        enddo
      endif
c
c Open output image and copy header keywords.
c
      call xyopen (lout, out, 'new', naxis, size)
      call headcopy (lin, lout, 0, naxis, 0, 0)
c
c  Write the history.
c
      call hisopen (lout,'append')
      call hiswrite (lout,'AVMATHS: Miriad '//version)
      call hisinput (lout,'AVMATHS')

      call listcom (nplanes, planes, istart, iend, nsect)
      isnext = 1
      more = .true.
      do while (more)
        call txtplane (nsect, istart, iend, aline, isnext, more)
        call hiswrite (lout, aline)
      enddo
      call hisclose (lout)
c
c  Initialize arrays
c
      do i = 1, size(1)*size(2)
        buffer(avpnt+i-1) = 0.0
        buffer(npnt+i-1)  = 0.0
      enddo
c
c Average the selected planes and hold in memory
c
      call average (size(1), size(2), lin, nplanes, planes, rline,
     *              flags, buffer(avpnt), buffer(npnt), zav)
c
c  Now compute and write out the output image
c
      if (dood) then
         call od (lin, lout, size(1), size(2), size3, rline, flags,
     *            buffer(avpnt), buffer(npnt))
         call wrbtype (lin, 'optical_depth')
      else if (dosub) then
         call sub (lin, lout, size(1), size(2), size3, rline, flags,
     *             buffer(avpnt), buffer(npnt))
         call hdcopy (lin, lout, 'bunit')
      else if (domul) then
         call mul (lin, lout, size(1), size(2), size3, rline, flags,
     *             buffer(avpnt), buffer(npnt))
         call hdcopy (lin, lout, 'bunit')
      else if (dorepl) then
         call replace (lout, size(1), size(2), size(3), rline,
     *                 flags, buffer(avpnt), buffer(npnt))
         call hdcopy (lin, lout, 'bunit')
c
c Fix up header if reduced
c
         if (dored) then
           call rdhdd (lin, 'cdelt3', cdelt3, 0.d0)
           cdelt3 = cdelt3 * nplanes

           call wrhdd (lout, 'cdelt3', cdelt3)
           call wrhdd (lout, 'crpix3', 1.0d0)
           call wrhdd (lout, 'crval3', zav)
         endif
      endif
c
c  Close up
c
      call xyclose (lin)
      call xyclose (lout)

      end


      subroutine getopt (dosub, dood, dorepl, dored, domul)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     dosub    Subtract average from cube
c     dood     Make optical depth
c     dorepl   Replace,ment
c     dored    Reduce to two dimensions
c     domul    Multiply
c
c-----------------------------------------------------------------------
      logical dosub, dood, dorepl, dored, domul
cc
      integer maxopt
      parameter (maxopt = 5)

      character opshuns(maxopt)*8
      logical present(maxopt), found
      integer i

      data opshuns /'subtract', 'replace', 'odepth', 'multiply',
     *              'noreduce'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
      found = .false.
      do i = 1, 4
        if (present(i) .and. found) then
          call bug ('f', 'You should specify one option only')
        else if (present(i)) then
          found = .true.
        endif
      enddo
      if (.not.found) call bug ('f', 'No options specified')

      dosub    = present(1)
      dorepl   = present(2)
      dood     = present(3)
      domul    = present(4)
      dored    = .not.present(5)
      if (.not.dorepl) dored = .false.

      end


        subroutine listcom (nplanes, planes, istart, iend, nsect)
c----------------------------------------------------------------------
c       Compact a list of planes such as: 1,2,3,4,7,10,12,13,14 to
c       a two lists giving the start and end planes of each section
c       for ease of formatting in history
c
c    Input:
c      nplanes   size of planes
c      planes    list of planes
c    Output:
c      istart    lists of start and end planes for each section
c      iend           e.g. if planes=1,2,3,5,7,8,9
c                        istart=1,5,7
c                        iend  =3,5,9
c      nsect     number of sections, for above example, nsect=3
c-------------------------------------------------------------------
        integer nplanes, planes(nplanes), istart(nplanes),
     *  iend(nplanes), nsect
cc
        integer i
c-------------------------------------------------------------------
        nsect = 1

        do i = 1, nplanes
          if (i.eq.1) then
            istart(nsect) = planes(1)
          else if (planes(i).ne.(planes(i-1)+1)) then
            iend(nsect) = planes(i-1)
            nsect = nsect + 1
            istart(nsect) = planes(i)
          else
            if (i.eq.nplanes) iend(nsect) = planes(i)
          endif
        enddo

      end


        subroutine txtplane (nsect, istart, iend, string, isnext, more)
c----------------------------------------------------------------------
c       Write the compacted plane list into a text string
c
c   Input:
c     nsect      number of sections in LIST
c     istart     list of start planes for each section
c     iend       list of end planes for each section
c   Output:
c     string     output string
c   Input/Output:
c     isnext     The number of the next section to start the next line
c     more       Do it again please if true
c----------------------------------------------------------------------
        integer nsect, istart(nsect), iend(nsect), isnext
        character string*(*)
        logical more
cc
        integer strlen, l1, l2, ipt, i
        character*4 ch1, ch2
c------------------------------------------------------------------
        string = 'AVMATHS: averaged channels = '
        ipt = 29
        strlen = len(string)
        do i = isnext, nsect
          if (istart(i).eq.iend(i)) then
            call itochar (istart(i), ch1, l1)
            if (ipt+l1.lt.strlen) then
              string(ipt:ipt+l1) = ch1(1:l1)
            else
              more = .true.
              isnext = i
              goto 999
            endif
            ipt = ipt + l1 + 2
          else
            call itochar (istart(i), ch1, l1)
            call itochar (iend(i), ch2, l2)
            if (ipt+l1+l2.lt.strlen) then
              string(ipt:ipt+l1+l2) = ch1(1:l1)//':'//ch2(1:l2)
              ipt = ipt + l1 + l2 + 2
            else
              more = .true.
              isnext = i
              goto 999
            endif
          endif
        enddo
        more = .false.

 999    return
        end


        subroutine itochar (ival, string, ilen)
c-----------------------------------------------------------------------
c       Encode an integer into a character string.
c
c     Input:
c        ival      i      Integer to encode
c     Output:
c        string    c(*)   String in which integer is placed.
c        ilen      i      Length of encoded part of string
c-----------------------------------------------------------------------
        integer ival, ilen
        character*(*) string
cc
        character itoaf*20, temp*20
        integer len1
c-----------------------------------------------------------------------
        temp = itoaf(ival)
        ilen = len1(temp)
        string = temp(1:ilen)

        end


      subroutine average (nx, ny, lin, nplanes, planes, rline, flags,
     *                    aver, norm, zav)
c----------------------------------------------------------------------
c     Average unblanked pixels from selected planes
c
c   Input:
c     nx,ny    X and Y size of image
c     lin      Handle for input image
c     nplanes  Number of planes to average
c     planes   List of planes to average
c     rline    Scratch array to read a line of data into
c     flags    Scratch array to read a line of blanking mask into
c  Output:
c     aver     The averaged image
c     norm     The normalization image.  If 0, then there is no good
c              point for the corresponding averaged image (i.e. all
c              the planes were blanked at that pixel) so the output
c              should be blanked at that pixel
c     zav      The average value of the third axis from the selected
c              planes
c-----------------------------------------------------------------------
      integer lin, nplanes, planes(nplanes), nx, ny
      double precision zav
      real aver(nx,ny), norm(nx,ny), rline(*)
      logical flags(*)
cc
      double precision crpix3, crval3, cdelt3
      integer i, j, k
      character aline*72
c-----------------------------------------------------------------------
c
c Get axis descriptors
c
      call rdhdd (lin, 'cdelt3', cdelt3, 0.d0)
      call rdhdd (lin, 'crpix3', crpix3, 0.d0)
      call rdhdd (lin, 'crval3', crval3, 0.d0)
c
c Accumulate desired planes
c
      zav = 0.0d0
      do k = 1, nplanes
        call xysetpl (lin, 1, planes(k))
        write (aline, 20) planes(k)
20      format (' Including plane ', i4, ' in average')
c        call output(aline)
        zav = zav + (dble(planes(k))-crpix3)*cdelt3+crval3

        do j = 1, ny
           call xyread  (lin, j, rline)
           call xyflgrd (lin, j, flags)

           do i = 1, nx
              if (flags(i)) then
                 aver(i,j) = aver(i,j) + rline(i)
                 norm(i,j) = norm(i,j) + 1.0
              endif
           enddo
        enddo
      enddo
      zav = zav / dble(nplanes)
      call output (' ')
c
c Now normalize averaged image
c
      do j = 1, ny
        do i = 1, nx
          if (norm(i,j).gt.0.0) aver(i,j) = aver(i,j) / norm(i,j)
        enddo
      enddo

      end


      subroutine mul (lin, lout, nx, ny, nz, rline, flags, aver, norm)
c-----------------------------------------------------------------------
c     Compute multiplied image and write it out.
c
c  Input:
c     lin      Handle for input image
c     lout     Handle for output image
c     nx,y,z   Image dimensions
c     rline    Scratch array to read a line of data into
c     flags    Scratch array to read a line of blanking mask into
c     aver     The averaged image
c     norm     The normalization image.  If 0, then there is no good
c              point for the corresponding averaged image (i.e. all
c              the planes were blanked at that pixel) so the output
c              should be blanked at that pixel
c
c-----------------------------------------------------------------------
      integer lin, lout, nx, ny, nz
      real rline(*), aver(nx,ny), norm(nx,ny)
      logical flags(*)
cc
      integer pinc, i, j, k
      character aline*72
c-----------------------------------------------------------------------
      pinc = nz / 10 + 1
      do k = 1, nz
        if (mod(k,pinc).eq.1) then
           write (aline, 10) k
10         format ('Begin computation of plane ', i4)
           call output (aline)
        endif

        call xysetpl (lin,  1, k)
        call xysetpl (lout, 1, k)

        do j = 1, ny
          call xyread (lin, j, rline)
          call xyflgrd (lin, j, flags)

          do i = 1, nx
            if (flags(i) .and. norm(i,j).gt.0.0) then
               rline(i) = rline(i) * aver(i,j)
            else
               rline(i) = 0.0
               flags(i) = .false.
            endif
          enddo

          call xywrite (lout, j, rline)
          call xyflgwr (lout, j, flags)
        enddo
      enddo

      end


      subroutine od (lin, lout, nx, ny, nz, rline, flags, aver, norm)
c-----------------------------------------------------------------------
c     Compute optical depth image and write it out.
c
c  Input:
c     lin      Handle for input image
c     lout     Handle for output image
c     nx,y,z   Image dimensions
c     rline    Scratch array to read a line of data into
c     flags    Scratch array to read a line of blanking mask into
c     aver     The averaged image
c     norm     The normalization image.  If 0, then there is no good
c              point for the corresponding averaged image (i.e. all
c              the planes were blanked at that pixel) so the output
c              should be blanked at that pixel
c
c-----------------------------------------------------------------------
      integer lin, lout, nx, ny, nz
      real rline(*), aver(nx,ny), norm(nx,ny)
      logical flags(*)
cc
      real temp
      integer pinc, i, j, k
      character aline*72
c-----------------------------------------------------------------------
      pinc = nz / 10 + 1
      do k = 1, nz
        if (mod(k,pinc).eq.1) then
           write (aline, 10) k
10         format ('Begin computation of plane ', i4)
           call output (aline)
        endif

        call xysetpl (lin,  1, k)
        call xysetpl (lout, 1, k)

        do j = 1, ny
          call xyread (lin, j, rline)
          call xyflgrd (lin, j, flags)

          do i = 1, nx
            temp = -1.0
            if (rline(i).ne.0.0) temp = aver(i,j) / rline(i)
            if (flags(i) .and. norm(i,j).gt.0.0 .and. temp.gt.0.0) then
               rline(i) = log(temp)
            else
               rline(i) = 1.0e10
               flags(i) = .false.
            endif
          enddo

          call xywrite (lout, j, rline)
          call xyflgwr (lout, j, flags)
        enddo
      enddo

      end


      subroutine sub (lin, lout, nx, ny, nz, rline, flags, aver, norm)
c-----------------------------------------------------------------------
c     Compute subtracted image and write it out.
c
c  Input:
c     lin      Handle for input image
c     lout     Handle for output image
c     nx,y,z   Image dimensions
c     rline    Scratch array to read a line of data into
c     flags    Scratch array to read a line of blanking mask into
c     aver     The averaged image
c     norm     The normalization image.  If 0, then there is no good
c              point for the corresponding averaged image (i.e. all
c              the planes were blanked at that pixel) so the output
c              should be blanked at that pixel
c
c-----------------------------------------------------------------------
      integer lin, lout, nx, ny, nz
      real rline(*), aver(nx,ny), norm(nx,ny)
      logical flags(*)
cc
      integer pinc, i, j, k
      character aline*72
c-----------------------------------------------------------------------
      pinc = nz / 10 + 1
      do k = 1, nz
        if (mod(k,pinc).eq.1) then
           write (aline, 10) k
10         format ('Begin computation of plane ', i4)
           call output (aline)
        endif

        call xysetpl (lin,  1, k)
        call xysetpl (lout, 1, k)

        do j = 1, ny
          call xyread (lin, j, rline)
          call xyflgrd (lin, j, flags)

          do i = 1, nx
            if (flags(i) .and. norm(i,j).gt.0.0) then
               rline(i) = rline(i) - aver(i,j)
            else
               rline(i) = 0.0
               flags(i) = .false.
            endif
          enddo

          call xywrite (lout, j, rline)
          call xyflgwr (lout, j, flags)
        enddo
      enddo

      end


      subroutine replace (lout, nx, ny, nz, rline, flags, aver, norm)
c-----------------------------------------------------------------------
c     Compute replaced image and write it out.
c
c  Input:
c     lout     Handle for output image
c     nx,y,z   Image dimensions
c     rline    Scratch array for a line of data
c     flags    Scratch array for a line of flags
c     aver     The averaged image
c     norm     The normalization image.  If 0, then there is no good
c              point for the corresponding averaged image (i.e. all
c              the planes were blanked at that pixel) so the output
c              should be blanked at that pixel
c
c-----------------------------------------------------------------------
      integer lout, nx, ny, nz
      real rline(*), aver(nx,ny), norm(nx,ny)
      logical flags(*)
cc
      integer pinc, i, j, k
      character aline*72
c-----------------------------------------------------------------------
      pinc = nz / 10 + 1
      do k = 1, nz
        if (mod(k,pinc).eq.1) then
           write (aline, 10) k
10         format ('Begin computation of plane ', i4)
           call output (aline)
        endif
        call xysetpl (lout, 1, k)

        do j = 1, ny
          do i = 1, nx
            if (norm(i,j).gt.0.0) then
               rline(i) = aver(i,j)
               flags(i) = .true.
            else
               rline(i) = 0.0
               flags(i) = .false.
            endif
          enddo

          call xywrite (lout, j, rline)
          call xyflgwr (lout, j, flags)
        enddo
      enddo

      end
