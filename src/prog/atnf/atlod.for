c***********************************************************************
        program atlod
c
c= atlod - Convert an RPFITS file into Miriad uv format.
c& rjs
c: data transfer
c+
c       ATLOD is a MIRIAD task, which converts a uv data-set from the
c       RPFITS format to Miriad format.
c@ in
c       Name of the input RPFITS files.  Several names can be given --
c       wildcard expansion is supported.  If a single name is given it
c       can be a raw tape device name (e.g. /dev/nrst0 in UNIX)
c       containing several files.  In this case, see the NFILES keyword
c       below.  There is no default.
c@ out
c       Name of the output Miriad uv data-set.  No default.
c@ ifsel
c       IF number(s) to select.  Default is all IFs.  For example,
c       if you observed with 5 GHz (frequency 1) and 8 GHz (frequency 2)
c       simultaneously, IF 1 would be the 5 GHz data and IF 2 would
c       be the 8 GHz data.  This now also lets you select zoom bands
c       using values greater than 2.  The freq 1 zoom bands come before 
c       the freq 2 ones.
c@ restfreq
c       The rest frequency, in GHz, for line observations.  By default,
c       the value in the RPFITS file is used.  Giving a value for the
c       "restfreq" parameter overrides the RPFITS file value.  If you do
c       set this parameter, you MUST give the same number of values as
c       the number of IFs written out.  A value of 0 is used for a
c       continuum observation.  For example, if you have two IFs, the
c       first of which is HI, and the second is continuum, use
c           restfreq=1.420405752,0
c@ options
c       This gives extra processing options.  Several can be given,
c       separated by comas.
c         'birdie'  For CABB data:
c                   CABB generates self-interference in a number of
c                   channels across the spectrum due to 640 MHz clock
c                   harmonics.  These birdies are fixed in channel
c                   number for each CABB configuration.  The birdie
c                   option currently knows about the 2048x1MHz continuum
c                   mode and flags the affected channels, 100 band edge
c                   channels on each side, and, at 20 and 13cm, the
c                   unusable parts of the spectrum. 
c
c                   For pre-CABB data:
c                   ATCA self-interference can corrupt channels at
c                   integral multiples of 128 MHz.  The birdie option
c                   flags these channels.  Additionally, in continuum
c                   (33 channels/128MHz) mode, the birdie option dicards
c                   every second channel, plus some edge channels.  The
c                   channels discarded are those most likely affected by
c                   the self-interference.  Discarding these channels
c                   does not have a sensitivity penalty, because the
c                   effective channel bandwidth is twice the channel
c                   separation.
c         'reweight' For pre-CABB data: re-weight the lag spectrum to 
c                   eliminate the "Gibbs" phenomena in continuum data
c                   (33 ch/128 MHz); ignored for all other data.
c         'compress' Write output data in compressed format.
c         'noauto'  Discard autocorrelation data.  The default is to
c                   copy the autocorrelation data.
c         'nocross' Discard cross-correlation data.  The default is to
c                   copy the cross correlation data.
c         'relax'   Do not flag visibilities based on SYSCAL
c                   information.  The default is to flag visibilities if
c                   they have not been preceded by a valid SYSCAL
c                   record, or if the the values in the SYSCAL record
c                   look bad.  SYSCAL values are checked for sampler
c                   statistics being within 3% of 17.3%, or 0.5% of
c                   50.0%, that the XY phase is within 10 degrees of its
c                   running median, and that the XY amplitudes are
c                   within 1 Jy or 10% of its running median.  The tests
c                   for xy phase and amplitude are skipped for 3mm data
c                   (as there is no noise calibration signal).
c         'mmrelax' This option is ignored, it is only present for
c                   historical reasons.
c         'unflag'  Save any data that is flagged and also keep it's 
c                   syscal info.  By default ATLOD discards most data 
c                   that is flagged.
c         'opcorr'  Correct for atmospheric opacity.  This option is
c                   possible for data measured after October 2003.
c                   Because of the way system temperature is measured at
c                   3mm (an "above atmosphere" measurement), it is not
c                   appropriate for 3mm data.  This option is silently
c                   ignored for 3mm data.  Generally it is only relevant
c                   for 7mm and 12mm observations.  It does no harm (and
c                   negligible good) for longer wavelengths.
c         'samcorr' Correct the pre-Dec93 data for incorrect sampler
c                   statistics.  Since December 1993, sampler
c                   corrections are performed online.  This option is
c                   silently ignored for data measured after December
c                   1993.
c         'xycorr'  Apply the on-line measurements of the XY phase.
c                   This option is silently ignored for 3mm data
c                   measured before October 2007.
c         'hanning' Hanning smooth spectra and drop every other channel
c                   This option is ignored for 128-MHz, 33-channel data.
c         'bary'    Use the barycentre as the velocity rest frame.  The
c                   default is to use the LSR frame.
c         'noif'    Do not map the simultaneous IFs to the IF axis.
c                   By default ATLOD attempts to map the simultaneous
c                   frequencies to the IF axis.  This will not be
c                   possible if there are a different number of
c                   polarisations in the different IFs.
c         'nopflag' If at least one polarisation of a set of 2 or 4
c                   polarimetric spectra are bad, ATLOD normally flags
c                   all of the polarisations.  Option nopflag changes
c                   this so that only the nominally bad spectrum is
c                   flagged.
c         'hires'   Treat bin data as measurements in the high time
c                   resolution mode.  The output dataset contains no
c                   bins, but instead appears as data measured with
c                   small cycle times.
c         'pmps'    Undo `poor man's phase switching'.  This is an
c                   obscure option that you should not generally use.
c         'single'  Assume input is a single dish RPFITS file (from
c                   Parkes or Mopra).  This is usually used together
c                   with option 'relax'.
c         'caldata' Save visibilities associated with certain system
c                   calibrations.  Currently this consists of reference
c                   pointing calibration and "paddle" measurements.
c         'nocacal' Flag data that atlod suspects is taken during a
c                   CACAL scan.  There is potential for error in atlod
c                   determining which data are and are not part of a
c                   cacal scan.  Use this with caution.
c         'nopol'   Discard data that is not "parallel hand" Stokes
c                   type.
c         'rfiflag' Flag channels at frequencies that are known to be
c                   bad.  This uses the file rfiflag.txt in the current
c                   directory or the default version in MIRCAT.  The
c                   file should contain 2 frequencies per line, the
c                   lower and upper end of the rfi in MHz.  Precede
c                   comments with a '#'.
c          'notsys' Undo the online Tsys correction. Useful if RFI in
c                   the tvchannel range has caused the corrections
c                   to be very noisy. The resulting data will be in raw
c                   counts scaled down by 10^6.
c          'nopack' Don't pack the two CABB autocorrelation bins into
c                   one bin. The autocorrelation bin 1 and 2 will  
c                   contain noise cal OFF and ON data.
c                   The default is to repack the info into bin 1:
c                   XX and YY will contain GTP and SDO data in the real
c                   and imaginary part; XY will contain the OFF data and
c                   YX will contain the ON-OFF data (for XY). This will
c                   allow recalculation of the Tsys and xyphase later.
c@ nfiles
c       This gives one or two numbers, being the number of files to
c       skip, followed by the number of files to process.  This is only
c       useful when the input is a tape device containing multiple
c       files.  The default is 0,1 (i.e. skip none, process 1 file).
c
c       NOTE: Using this feature to skip many files on a tape is VERY
c       inefficient.  It is far faster to skip using operating system
c       commands.  When doing this, however, you should be aware is that
c       every RPFITS files consists of 3 tape files.  Thus you will want
c       to skip three times as many tape files as RPFITS files.  For
c       example, in UNIX, to skip 10 RPFITS files, use
c                   mt -f /dev/nrst0 fsf 30
c@ nscans
c       This gives one or two numbers, being the number of scans to
c       skip, followed by the number of scans to process.  NOTE: This
c       applies to all files read.  The default is to skip none and
c       process all scans.
c@ nopcorr
c       This gives the number of frequencies to use per IF for opacity
c       correction. The default (2) is to use linear interpolation 
c       across the spectrum. Maximum value is 32.
c@ edge
c       Specify the percentage of edge channels the birdie option will
c       flag out. The default is 9.8 which will flag about 100 channels 
c       from the bottom and top of a 2049 channel spectrum. This 
c       parameter is only used if the birdie option is specified.
c       For concatenated spectra the width of a single zoom is used.
c       Note that noise and artefacts go up quickly towards the band 
c       edge so making this much smaller will not gain you much.
c
c$Id$
c--
c
c  Program Structure:
c    Miriad atlod can be divided into three rough levels. The high level
c    gets input parameters, opens the output, and tells the appropriate
c    subroutines what to do.
c
c    The RP layer is the layer which interacts with RPFITSIN and the
c    RPFITS common.  Its "dispatch" routine (RPDISP) waits for an
c    integration to start, and then calls the Poke routines in the
c    order:
c       Poke1st (start an integration)
c       PokeMisc,PokeAnt,PokeIF,PokeSrc (if these have changed)
c       PokeSC (syscal info), PokeData (corr data).
c       PokeFlsh (finish the integration)
c    Prior to calling the Poke routines, RPDISP discards unneeded
c    data and maps the selected frequencies notional IFs.
c    The RP layer is in charge of data selection.
c
c    The Poke layer buffers up information about the current
c    integration, and flushes it to the output file when all is ready.
c    The Poke layer is in charge of any massaging that needs to be done
c    to the data (e.g. hanning smoothing, sampler correction).
c
c  History:
c    rjs  12feb91 Original version.
c    rjs  20feb91 Eliminated my confusion between "files" and "scans".
c                 Precessed J2000 RA,DEC to get obsra,obsdec.
c    nebk 06mar91 Add XYPHASE array
c    nebk 12may91 Broke it, to give rjs the shits.
c    rjs  16may91 Fixed it, to spite nebk.
c    rjs  17may91 Fixed the headache of the sign of the XY phase and the
c                 if_invert switch.
c    rjs  19may91 noapply option. Calculates and prints out average XY
c                 phase.
c    rjs  12jun91 Make the default restfreq the value of if_freq.
c    rjs  13jun91 Subtracted (rather than adding) 45 deg to chi.
c    rjs  17jun91 Fiddled with XYPHASE and CHI some more.
c    rjs  21jul91 Write out xsampler, ysampler, xyamp variables.
c    rjs  13dec91 Deleted obstype processing (now done in uvio).
c    rjs  22jul92 Implemented nfiles processing.
c    rjs  28aug92 Doc changges only.
c    nebk 07sep92 Incredibly stupid mistake.  Only Mr. S could do it.
c                 VIS and WEIGHT were not dimensioned big enough.
c                 Also include MIRCONST.H in ATLOD.H for pi
c    rjs  11sep92 Add number of scans to skip.
c    rjs  14sep92 Better messages.  Write antenna coordinates and LST,
c                 as well as jiggery pokery with antenna numbers and XY
c                 phase.
c    rjs  18sep92 Write out X and Y Tsys separately.
c    rjs  21sep92 Get XYPHASE right (I think!).
c    rjs  28oct92 Sign of XY phase was wrong for 2nd IF in 2 IF system
c                 when the 2 IFs had different sideband indicators.
c    nebk 02feb93 Deal with all RPFITSIN error conditions explicitly
c                 and look for next scan if I/O error occurs
c    rjs  16mar93 Better (?) AsciiCpy.
c    rjs  29mar93 Change value of jyperk.
c    rjs  27oct93 Change treatment of xy phases. Add sampler correction.
c    nebk 28oct93 Add keywords ifsel,restfreq and options=hanning
c    rjs  17nov93 Rewritten.
c    rjs  25nov93 Eliminate spurious messages after the start of a scan.
c    rjs  13dec93 Sign convention of V change.
c    rjs  27jan94 Fix bug which mislabelled polarisations when
c                 options=noif.  Minor formating improvements.
c                 Robustness to i/o errors.
c    rjs  14mar94 INTBASE change.
c    rjs   8apr94 Readd disabling of sampler correction after 11Dec93.
c                 Where did this mod disappear to?
c    rjs  29aug94 w axis changes.
c    rjs   2sep94 Read multiple files.
c    rjs  21sep94 Change sign convention for XY and YX. Discard detached
c                 antennas.
c    rjs   3nov94 Eliminate spurious error message.
c    rjs  28nov94 Be more strict about what sampler stats are OK.
c    rjs  13jan95 Friday 13th! Add pulsar bin no as uv variable.
c    rjs  25jan95 Write the fudged source name (rather than just
c                 discarding it!!).
c    nebk 18feb95 Write out a correct fudged source name rather than
c                 a totally scrambled one.  Mr. S must be on drugs
c                 again.
c    nebk 03mar95 Add options=birdie and record file names in history.
c    rjs  14mar95 Discard channels for options=birdie.
c    rjs  27mar95 Options=reweight.
c    rjs  26apr95 Make options=unflag always behave as advertised.
c    rjs  20sep95 Option=birdie flags the birdie channel in spectral
c                 line mode.
c    rjs  27sep95 Flagging stats. Will Mr K still whinge? Probably!
c    rjs   6nov95 xycorr option.
c    rjs  29may96 Write out nbin variable.
c    rjs  02jul96 Changes to honour the proper motion parameters.
c    rjs  16jul96 Flagged all polarisation if any bad, and added nopflag
c                 option.
c    rjs  17jul96 Flag if there are glitches in the XY amplitude.
c    rjs  22nov96 Reset median-based flaggers after a scan change.
c    rjs  05dec96 Write pointing centre RA and DEC if needed.
c    rjs  03feb97 Better error messages, and make it more robust to
c                 multibeam datasets.  NOTE: To change it so that
c                 multibeam datasets can be read:
c                 * Set ATANT to 15 in atlod.h
c                 * Comment out checks for invalid antennas in antchk
c                 * Get rid of skip when jstat.eq.5 in RPDISP.
c    rjs  22sep97 Replace call to fdatejul with dayjul.
c    rjs  07jan98 Better printing of source names.
c    rjs  06apr98 Increase the max size of an integration.
c    rjs  07may98 Change in handling of jstat.eq.5 return value.
c    rjs  14may98 Handle higher time resolution.
c    rjs  04oct98 Extra check for validity of a record.
c    rjs  12nov98 options=hires now supports high time resolution bin
c                 mode.
c    rjs  31aug99 Check for bad RPFITS value for sdf.
c    rjs  11jun00 Include pmps switch.  More robust to bad number of
c                 channels etc. in RPFITS file.  Increase buffer space.
c    dpr  10apr01 Add cluge for correlator UT day rollover bug.
c    dpr  11apr01 ATANT=8 in atlog.h
c    rjs  22may02 Added options=mmrelax
c    rjs  25may02 Generate "tcorr" variable to keep track of whether
c                 Tsys scaling has been performed or not.
c    rjs  16jul03 Do not flip sign of XY and YX correlations for 12mm
c                 package.
c    rjs  19jul03 Removed option=mmrelax, and made this automatic!
c    rjs  06dec03 Write out met data, axisrms, axismax data.
c                 options=opcorr
c    rjs  30dec03 Doc change only.
c    rjs  15feb04 Save input RPFITS name and calcode.
c    rjs  10jun04 Handle change in xy correlation at 12mm.
c    rjs  11sep04 Add rain guage and seeing monitor data to output.
c    rjs  23oct04 Fix pntra and pntdec bug - always write them out.
c    rjs  16jun05 Detect CACAL data. Better flagging stats messages.
c    rjs  17sep05 Correct date for sampler correction code.
c    rjs  15oct05 Check for buffer overflows. Increase buffer size.
c                 Better flagging statistics.
c    rjs  02jan06 Save reference pointing information.  8MHz debirdie
c                 algorithm.  Added nopol option.
c    rjs  14jan06 Be relaxed about missing met data scans when applying
c                 opacity correction.
c    mhw  19oct07 Cope with new 3mm receiver on ca02 which has xyphase
c    mhw  18dec07 Changes to read simulated CABB files with up to
c                 16 IFs; avoid checking sampler stats for CABB files
c    mhw  17jan08 Add options=rfiflag: flagging based on file with rfi
c                 ranges.
c    rjs  06nov08 Corrected CA02 xyphase handling and some minor
c                 tidying.
c    mhw  09jan09 Add flagging of NaNs in CABB spectra
c    mhw  03jun09 Fix syscal handling for CABB gtp, sdo and caljy values
c    mhw  14sep09 Make sure birdie is ignored for CABB data
c    mhw  29sep09 Actually, make it do something useful instead, 
c                 integrate with rfiflag option
c    mhw  28mar10 Use IF number instead of IF chain for ifsel
c    mhw  13apr10 Record ifchain in file, allow multiple values for
c                 ifsel
c    mhw  28jun10 Add options nopack, notsys, process auto corr bins
c    mhw  22jul10 Fix ischan calculation.
c    mhw  19oct10 Update 20/13 band - now 1-3 GHz
c    mhw  26oct10 Fix birdie flagging in 20/13 cm band
c    mhw  11nov10 Record scan direction for otfmos scans
c    mhw  23nov10 Fix for CABB 33 channel (64MHz) mode
c    mhw  07feb11 Add edge keyword to control birdie/edge flagging
c    mhw  29may12 Try to make opcorr more accurate for wide bands
c    mhw  15jun12 Fix index errors in opcor change
c    mhw  12sep12 Drop edge channels for 16cm data with birdie option
c    mhw  07dec12 Fix 29may12 opcor code again - how did it ever work?
c    mhw  29jan13 Fix nscans skip and read code - RPEOF call hangs
c
c $Id$
c-----------------------------------------------------------------------

        integer MAXFILES,MAXTIMES,MAXSIM
        parameter(MAXFILES=128,MAXTIMES=32,MAXSIM=34)
c
        character in(MAXFILES)*128,line*64,out*64,t1*18,t2*18,version*80
        integer tno,ntimes
        integer ifile,ifsel(MAXSIM),nsel,nfreq,iostat,nfiles,nopcorr,i
        double precision rfreq(2),times(2,MAXTIMES)
        logical doauto,docross,docomp,dosam,relax,unflag,dohann
        logical dobary,doif,birdie,dowt,dopmps,doxyp,doop,dopack,dotsys
        logical polflag,hires,sing,docaldat,nocacal,nopol,rfiflag
        integer fileskip,fileproc,scanskip,scanproc
        real edge
c
c  Externals.
c
        character itoaf*8, rperr*32, versan*80
c-----------------------------------------------------------------------
      version = versan ('atlod',
     :                  '$Revision$',
     :                  '$Date$')
c
c  Get the input parameters.
c
        call keyini
        call mkeyf('in',in,MAXFILES,nfiles)
        if(nfiles.eq.0)
     *    call bug('f','Input name must be given')
        call keya('out',out,' ')
        if(out.eq.' ')
     *    call bug('f','Output name must be given')
        call mkeyi('ifsel',ifsel,MAXSIM,nsel)
        if (nsel.eq.0) then
          nsel=1
          ifsel(1)=0
        endif
        call mkeyd('restfreq',rfreq,2,nfreq)
        call getopt(doauto,docross,docaldat,docomp,dosam,doxyp,doop,
     *    relax,sing,unflag,dohann,birdie,dobary,doif,dowt,dopmps,
     *    nopol,polflag,hires,nocacal,rfiflag,dopack,dotsys)
        call keyi('nfiles',fileskip,0)
        call keyi('nfiles',fileproc,nfiles-fileskip)
        if(nfiles.gt.1.and.fileproc+fileskip.gt.nfiles)
     *    fileproc = nfiles - fileskip
        if(fileskip.lt.0.or.fileproc.lt.1)
     *    call bug('f','Invalid NFILES parameter')
        call keyi('nscans',scanskip,0)
        call keyi('nscans',scanproc,0)
        if(scanskip.lt.0.or.scanproc.lt.0)
     *    call bug('f','Invalid NSCANS parameter')
        call keyi('nopcorr',nopcorr,2)
        nopcorr=min(32,max(2,nopcorr))
        call keyr('edge',edge,9.8)
        call keyfin
c
        call cacalIni
        call rfiIni(rfiflag)
c
c  Open the output and initialise it.
c
        call uvopen(tno,out,'new')
        if(.not.docomp)call uvset(tno,'corr','r',0,0.,0.,0.)
        call uvset(tno,'preamble','uvw/time/baseline',0,0.,0.,0.)
        call Fixed(tno,dobary)
c
c  Do some history processing.
c
        call hisopen(tno,'write')
        call hiswrite(tno,'ATLOD: Miriad '//version)
        call hisinput(tno,'ATLOD')
c
c  Process a number of files.
c
        ifile = 0
        iostat = 0
        dowhile(ifile.lt.fileskip+fileproc.and.iostat.eq.0)
          ifile = ifile + 1
          if(ifile.le.fileskip)then
            if(nfiles.eq.1)then
              call output('Skipping file '//itoaf(ifile))
              call RPSkip(in(1),iostat)
            else
              call output('Ignoring file '//in(ifile))
              iostat = 0
            endif
            if(iostat.ne.0)call bug('f','Error skipping RPFITS file')
          else
            call PokeIni(tno,dosam,doxyp,doop,dohann,birdie,edge,
     *          dowt,dopmps,dobary,doif,hires,dopack,dotsys,nopcorr)
            if(nfiles.eq.1)then
              i = 1
            else
              i = ifile
            endif
            if(i.ne.ifile)then
              call liner('Processing file '//itoaf(ifile))
            else
              call liner('Processing file '//in(ifile))
            endif
            call RPDisp(in(i),scanskip,scanproc,doauto,docross,
     *          docaldat,relax,sing,unflag,nopol,polflag,dotsys,
     *          ifsel,nsel,rfreq,nfreq,iostat)
          endif
        enddo
c
c  Close up shop.
c
        if(iostat.ne.0)then
          line = 'RPFITS i/o error: '//rperr(iostat)
          call bug('w',line)
          call bug('w','Prematurely finishing because of errors')
          call hiswrite(tno,'ATLOD: '//line)
          call hiswrite(tno,
     *           'ATLOD: Prematurely finishing because of errors')
        endif
        call uvflush(tno)
c
c  Report on what appear to be runs on cacal.
c
        call cacalFin(times,MAXTIMES,ntimes)
        call liner(' ')
        if(ntimes.eq.0)call liner('No CACAL data was detected')
        do i=1,ntimes
          call julday(times(1,i),'H',t1)
          call julday(times(2,i),'H',t2)
          line = 'Probable CACAL data from '//t1//' to '//t2
          call liner(line)
        enddo
c
c  All said and done.
c
        call hisclose(tno)
        call uvclose(tno)
c
        if(nocacal.and.ntimes.gt.0)then
          call output('Flagging probable CACAL data ...')
          call uvopen(tno,out,'old')
          call caflag(tno,times,ntimes)
          call uvclose(tno)
        endif
c
        end
c***********************************************************************
        subroutine caflag(tno,times,ntimes)
c
        integer tno,ntimes
        double precision times(2,ntimes)
c-----------------------------------------------------------------------
        include 'maxdim.h'
        complex data(MAXCHAN)
        logical flags(MAXCHAN),doflag
        double precision preamble(4),time
        integer nchan,i
c
        call uvrewind(tno)
        call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
        dowhile(nchan.gt.0)
          doflag = .false.
          time = preamble(3)
          do i=1,ntimes
            doflag = doflag.or.
     *               (times(1,i).le.time.and.time.lt.times(2,i))
          enddo
          if(doflag)then
            do i=1,nchan
              flags(i) = .false.
            enddo
            call uvflgwr(tno,flags)
          endif
          call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
        enddo
c
        end
c***********************************************************************
        subroutine GetOpt(doauto,docross,docaldat,docomp,dosam,doxyp,
     *    doop,relax,sing,
     *    unflag,dohann,birdie,dobary,doif,dowt,dopmps,
     *    nopol,polflag,hires,nocacal,rfiflag,dopack,dotsys)
c
        logical doauto,docross,dosam,relax,unflag,dohann,dobary,doop
        logical docomp,doif,birdie,dowt,dopmps,doxyp,polflag,hires,sing
        logical docaldat,nocacal,nopol,rfiflag,dopack,dotsys
c
c  Get the user options.
c
c  Output:
c    doauto     Set if the user want autocorrelation data.
c    docross    Set if the user wants cross-correlation data.
c    docaldat   Do not flag calibration data.
c    docomp     Write compressed data.
c    dosam      Correct for sampler statistics.
c    doxyp      Correct the data with the measured xy phase.
c    doop       Correct for opacity.
c    dohann     Hanning smooth spectra
c    birdie     Discard bad channels in continuum mode.
c    doif       Map the simultaneous frequencies to the IF axis.
c    relax
c    unflag
c    dobary     Compute barycentric radial velocities.
c    birdie
c    dowt       Reweight the lag spectrum.
c    dopmps     Undo "poor man's phase switching"
c    nopol      Select only parallel-hand polarisations.
c    polflag    Flag all polarisations if any are bad.
c    hires      Convert bin-mode to high time resolution data.
c    sing       Single dish mode.
c    rfiflag    Flag known rfi sources
c    dopack     Pack bin 1 and 2 into 1, discard bin 2
c    dotsys     Use online tsys correction
c-----------------------------------------------------------------------
        integer nopt
        parameter(nopt=24)
        character opts(nopt)*8
        logical present(nopt)
        data opts/'noauto  ','nocross ','compress','relax   ',
     *            'unflag  ','samcorr ','hanning ','bary    ',
     *            'noif    ','birdie  ','reweight','xycorr  ',
     *            'opcorr  ','nopflag ','hires   ','pmps    ',
     *            'mmrelax ','single  ','caldata ','nocacal ',
     *            'nopol   ','rfiflag ','nopack  ','notsys  '/
        call options('options',opts,present,nopt)
        doauto = .not.present(1)
        docross = .not.present(2)
        docomp  = present(3)
        relax   = present(4)
        unflag  = present(5)
        dosam   = present(6)
        dohann  = present(7)
        dobary  = present(8)
        doif    = .not.present(9)
        birdie  = present(10)
        dowt    = present(11)
        doxyp   = present(12)
        doop    = present(13)
        polflag = .not.present(14)
        hires   = present(15)
        dopmps  = present(16)
c
c  Option mmrelax is now ignored (set automatically!).
c       mmrelax = present(16)
        sing    = present(18)
        docaldat= present(19)
        nocacal = present(20)
        nopol   = present(21)
        rfiflag = present(22)
        dopack  = .not.present(23).and.doauto
        dotsys  = .not.present(24)     
c
        if((dosam.or.doxyp.or.doop).and.relax)call bug('f',
     *    'You cannot use options samcorr, xycorr or opcorr with relax')
        end
c***********************************************************************
        subroutine Fixed(tno,dobary)
c
        integer tno
        logical dobary
c
c  This updates variables that never change.
c
c  Input:
c    tno        Handle of the output uv data-set.
c    dobary     Velocity restframe is the barycentre.
c-----------------------------------------------------------------------
        double precision latitude,longitud,dtemp
        real chioff
        integer mount
        logical ok
c
        call uvputvrr(tno,'epoch',2000.,1)
        call uvputvrr(tno,'vsource',0.,1)
        call obspar('ATCA','latitude',latitude,ok)
        if(ok)call obspar('ATCA','longitude',longitud,ok)
        if(ok)call obspar('ATCA','evector',dtemp,ok)
        if(ok)chioff = dtemp
        if(ok)call obspar('ATCA','mount',dtemp,ok)
        if(ok)mount = dtemp
        if(.not.ok)then
          call bug('w','Unable to determine telescope lat/long')
        else
          call uvputvrd(tno,'latitud',latitude,1)
          call uvputvrd(tno,'longitu',longitud,1)
          call uvputvrr(tno,'evector',chioff,1)
          call uvputvri(tno,'mount',mount,1)
        endif
c
        if(dobary)then
          call uvputvra(tno,'veltype','VELO-HEL')
        else
          call uvputvra(tno,'veltype','VELO-LSR')
        endif
        end
c***********************************************************************
c***********************************************************************
        subroutine PokeIni(tno1,dosam1,doxyp1,doop1,
     *          dohann1,birdie1,edge1,dowt1,dopmps1,dobary1,doif1,
     *          hires1,dopack1,dotsys1,nopcorr1)
c
        integer tno1,nopcorr1
        logical dosam1,doxyp1,dohann1,doif1,dobary1,birdie1,dowt1
        logical dopmps1,hires1,doop1,dopack1,dotsys1
        real    edge1
c
c  Initialise the Poke routines.
c-----------------------------------------------------------------------
        include 'atlod.h'
        integer bl,p,iif,bin
        logical ok
c
        sname  = ' '
        tno    = tno1
        dosam  = dosam1
        doxyp  = doxyp1
        opcorr = doop1
        dohann = dohann1
        doif   = doif1
        dobary = dobary1
        birdie = birdie1
        dowt   = dowt1
        dopmps = dopmps1
        hires  = hires1
        dopack = dopack1
        dotsys = dotsys1
        nopcorr= nopcorr1
        edgepc = edge1
c
        if(dowt)call LagWt(wts,2*ATCONT-2,0.04)
c
        newsc = .false.
        newfreq = .false.
        nants = 0
        nifs = 0
        nused = 0
        do iif=1,ATIF
          nstoke(iif) = 0
          nfreq(iif) = 0
        enddo
c
c  Reset the counters, etc.
c
        do iif=1,ATIF
          nbin(iif) = 0
        enddo
        inttim = 0
        do bin=1,ATBIN
          do bl=1,ATBASE
            do p=1,ATPOL
              do iif=1,ATIF
                pnt(iif,p,bl,bin) = 0
              enddo
            enddo
          enddo
        enddo
c
c  Nominal met data values.
c
        stemp = 300
        spress = 1013*97.5
        shumid = 0.3
c
c  Determine some constants for later use.
c
        call obspar('ATCA','latitude', lat, ok)
        if(.not.ok)call bug('f','Could not get ATCA latitude')
        call obspar('ATCA','longitude',long,ok)
        if(.not.ok)call bug('f','Could not get ATCA longitude')
c
        end
c***********************************************************************
        subroutine PokeInfo(scanno,time)
c
        double precision time
        integer scanno
c
c  Give some information about the current scan that has just
c  started.
c-----------------------------------------------------------------------
        character date*32,line*64
        integer length
c
        character itoaf*4
        integer len1
c
        call julday(time,'H',date)
        line = 'Scan '//itoaf(scanno)
        length = len1(line)
        line(length+1:) = ' started at '//date
        call output(line)
        end
c***********************************************************************
        subroutine PokeStat(nrec,fgbad,fgoffsrc,fginvant,fgsysc,fgsam,
     *                                                          fgcal)
c
        integer nrec,fgbad,fgoffsrc,fginvant,fgsysc,fgsam,fgcal
c
c  Report statistics on this file.
c-----------------------------------------------------------------------
c
c  Externals.
c
        character itoaf*8,pcent*6
c
        call output('---------------------------------------')
        call liner(
     *          'Total number of spectra selected: '//itoaf(nrec+fgbad))
        if(fgbad.gt.0)call liner(
     *          'Number of invalid data records: '//itoaf(fgbad))
        call output(' ')
        if(fgoffsrc+fginvant+fgsysc+fgsam+fgcal.gt.0)then
          call liner('Summary of spectra flagged')
          call liner('Flagging Reason            Fraction')
          call liner('---------------            --------')
          if(fgoffsrc.gt.0)call liner(
     *          'Antenna off-source/off-line '//pcent(fgoffsrc,nrec))
          if(fginvant.gt.0)call liner(
     *          'Antenna disabled            '//pcent(fginvant,nrec))
          if(fgsysc.gt.0)call liner(
     *          'Missing SYSCAL record       '//pcent(fgsysc,nrec))
          if(fgsam.gt.0)call liner(
     *          'Bad SYSCAL values           '//pcent(fgsam,nrec))
          if(fgcal.gt.0)call liner(
     *          'Discarded cal data          '//pcent(fgcal,nrec))
        else
          call liner('There was no flagged data')
        endif
        call liner('---------------------------------------')
c
        end
c***********************************************************************
        subroutine liner(string)
c
        character string*(*)
c
c-----------------------------------------------------------------------
        character line*72
        include 'atlod.h'
c
        call output(string)
        line = 'ATLOD:    '//string
        call hiswrite(tno,line)
        end
c***********************************************************************
        subroutine liners(string)
c
        character string*(*)
c
c-----------------------------------------------------------------------
        character line*72
        include 'atlod.h'
c
        line = 'ATLOD:     '//string
        call hiswrite(tno,line)
        end
c***********************************************************************
        subroutine sortcds(ncard,card,sctype,refpnt,nants,okref)
c
        integer ncard,nants
        logical okref
        real refpnt(2,nants)
        character card(ncard)*(*),sctype*(*)
c
c  Write out selected cards to the history file.
c
c-----------------------------------------------------------------------
        integer i,cacalval
        logical ok
c
        okref = .false.
        do i=1,nants
          refpnt(1,i) = 0
          refpnt(2,i) = 0
        enddo
c
        do i=1,ncard
c         if(card(i)(1:8).eq.'OBSLOG' .or.card(i)(1:8).eq.'ATTEN'   .or.
c     *     card(i)(1:8).eq.'SUBREFL' .or.card(i)(1:8).eq.'CORR_CFG'.or.
c     *     card(i)(1:8).eq.'SCANTYPE'.or.card(i)(1:8).eq.'CACALCNT'.or.
c     *     card(i)(1:8).eq.'POINTCOR'.or.card(i)(1:8).eq.'POINTINF'
c     *                                                         )then
          if(card(i)(1:8).eq.'OBSLOG')then
            call liners(card(i)(20:))
          else if(card(i)(1:8).eq.'POINTCOR')then
            call crackpnt(card(i),refpnt,nants,ok)
            if(.not.ok)then
              call bug('w','Error decoding reference pointing card')
            else
              okref = .true.
            endif
          else if(card(i)(1:8).eq.'CACALCNT')then
            call cardvali(card(i),cacalval)
            call cacalCnt(cacalval)
          else if(card(i)(1:8).eq.'SCANTYPE')then
            sctype = card(i)(11:)
            call lcase(sctype)
          endif
        enddo
c
        end
c***********************************************************************
        subroutine crackpnt(line,refpnt,nants,ok)
c
        character line*(*)
        integer nants
        real refpnt(2,nants)
        logical ok
c
c-----------------------------------------------------------------------
        integer k1,k2,iant,length
        character token*8
c
c  Externals.
c
        integer len1
c
        ok = .false.
        k1 =10
        k2 = len1(line)
c
        call getfield(line,k1,k2,token,length)
        if(token(1:3).ne.'CA0')return
        iant = ichar(token(4:4)) - ichar('0')
        if(iant.le.0.or.iant.gt.nants)return
        call getfield(line,k1,k2,token,length)
        if(token.ne.'Az=')return
        call getfield(line,k1,k2,token,length)
        if(length.eq.0)return
        call atorf(token,refpnt(1,iant),ok)
        if(.not.ok)return
        call getfield(line,k1,k2,token,length)
        if(token.ne.'El=')return
        call getfield(line,k1,k2,token,length)
        if(length.eq.0)return
        call atorf(token,refpnt(2,iant),ok)
        if(.not.ok)return
        ok = .true.
        end
c***********************************************************************
        subroutine cardvali(card,ival)
c
        integer ival
        character card*(*)
c-----------------------------------------------------------------------
        integer i,j
        double precision out
        logical ok
c
        i = index(card,'=') + 1
        dowhile(card(i:i).eq.' '.and.i.le.len(card))
          i = i + 1
        enddo
        j = i - 1
        do while(card(j+1:j+1).ne.' '.and.card(j+1:j+1).ne.'/')
          j = j + 1
        enddo
        ok = .false.
        if(j.ge.i)call atodf(card(i:j),out,ok)
        if(.not.ok)
     *      call bug('f','Conversion error in decoding FITS card')
        ival = nint(out)
c
        end
c***********************************************************************
        subroutine PokeName(in)
c
        character in*(*)
c
c-----------------------------------------------------------------------
        include 'atlod.h'
        character c*1
        integer i1,i2,i
c
        integer len1
c
        i1 = 1
        i2 = len1(in)
        do i=1,i2
          c = in(i:i)
          if(index('/[]:',c).ne.0)i1 = i + 1
        enddo
        if(i1.gt.i2)i1 = 1
        call uvputvra(tno,'name',in(i1:i2))
        end
c***********************************************************************
        subroutine Poke1st(time1,nifs1,nants1,cabb1)
c
        double precision time1
        integer nifs1,nants1
        logical cabb1
c
c  Set some fundamental parameters just before we start dumping other
c  things.
c
c-----------------------------------------------------------------------
        include 'atlod.h'
        time = time1
        nifs = nifs1
        nants = nants1
        cabb = cabb1
        if(nifs.le.0.or.nifs.gt.ATIF.or.nants.le.0.or.nants.gt.ATANT)
     *    call bug('f','Invalid nants or nifs in Poke1st')
        call uvputvri(tno,'nants',nants,1)
c
        end
c***********************************************************************
        subroutine PokeIF(iif,nfreq1,bw,freq,ref,rfreq,nstok,cstok,ifc)
c
        integer iif,nfreq1,nstok,ifc
        character cstok(nstok)*(*)
        double precision freq,bw,ref,rfreq
c
c  Save the frequency/IF information.
c
c-----------------------------------------------------------------------
        include 'atlod.h'
        integer p,t
c
c  Externals.
c
        integer PolsP2C
c
        if(iif.gt.nifs)call bug('f','Invalid IF in PokeIF')
        if(nstok.gt.ATPOL)call bug('f',
     *          'Invalid number of polarisation parameters in PokeIF')
c
        nfreq(iif) = nfreq1
        if(nfreq(iif).gt.1)then
          sdf(iif) = 1e-9*bw / (nfreq(iif) - 1)
        else
          sdf(iif) = 1e-9*abs(bw)
        endif
        if(abs(sdf(iif)).eq.0)
     *    call bug('w','Channel width in RPFITS file is 0')
        sfreq(iif) = 1e-9*freq - (ref-1)*sdf(iif)
        edge(iif) = 0
        bchan(iif) = 0
c
c  If we are working in "birdie" mode, compute the channel with the
c  128MHz LO signal in it for pre-CABB data
c
        if(.not.cabb.and.birdie.and.nfreq(iif).eq.33)then
          call birdchan(sfreq(iif),sdf(iif),nfreq(iif),t)
c
          edge(iif) = 3 + mod(t,2)
          sfreq(iif) = sfreq(iif) + edge(iif)*sdf(iif)
          sdf(iif) = 2*sdf(iif)
          nfreq(iif) = (nfreq(iif)-2*edge(iif)+1)/2
        endif
c
        if(dohann.and.nfreq(iif).gt.33)then
          edge(iif) = 1
          sfreq(iif) = sfreq(iif) + sdf(iif)
          sdf(iif) = 2*sdf(iif)
          nfreq(iif) = (nfreq(iif)-1)/2
        endif
c
c  If birdie mode, flag out the birdie channel.
c
        if(.not.cabb.and.birdie)then
          call birdchan(sfreq(iif),sdf(iif),nfreq(iif),bchan(iif))
        else
          bchan(iif) = 0
        endif
c
        nstoke(iif) = nstok
        restfreq(iif) = 1e-9 * rfreq
        ifchain(iif) = ifc
c
        do p=1,nstoke(iif)
          polcode(iif,p) = PolsP2C(cstok(p))
        enddo
c
        newfreq = .true.
c
        end
c***********************************************************************
        subroutine birdchan(sfreq,sdf,nfreq,chan)
c
        integer nfreq,chan
        double precision sfreq,sdf
c-----------------------------------------------------------------------
        double precision flo
c
        if(abs(sdf).eq.0)call bug('f',
     *    'Cannot use options=birdie when channel width is unknown')
        flo = sfreq + 0.5*(nfreq-1)*sdf
        flo = 0.128d0 * nint(flo/0.128d0)
        chan = nint((flo - sfreq)/sdf) + 1
        if(chan.le.0) chan = chan + nint(0.128d0/abs(sdf))
        end
c***********************************************************************
        subroutine pokemet(mdata1,mcount1)
c
        integer mcount1
        real mdata1(mcount1)
c
c-----------------------------------------------------------------------
        include 'atlod.h'
        integer i
c
        do i=1,mcount1
          mdata(i) = mdata1(i)
        enddo
        mcount = mcount1
c
        end
c***********************************************************************
        subroutine pokeref(refpnt1,nant1)
c
        integer nant1
        real refpnt1(2,nant1)
c-----------------------------------------------------------------------
        include 'atlod.h'
        integer i
c
        do i=1,nant1
          refpnt(1,i) = refpnt1(1,i)
          refpnt(2,i) = refpnt1(2,i)
        enddo
        refnant = nant1
        end
c***********************************************************************
        subroutine PokeSC(ant,iif,chi1,tcorr1,
     *          xtsys1,ytsys1,xyphase1,xyamp1,xsamp,ysamp,
     *          xgtp1,ygtp1,xsdo1,ysdo1,xcaljy1,ycaljy1,
     *          pntrms,pntmax)
c
        integer ant,iif,tcorr1
        real chi1,xtsys1,ytsys1,xyphase1,xyamp1
        real xgtp1,ygtp1,xsdo1,ysdo1,xcaljy1,ycaljy1
        real xsamp(3),ysamp(3),pntrms,pntmax
c
c  Save the SYSCAL group info.
c-----------------------------------------------------------------------
        include 'atlod.h'
        if(ant.gt.nants.or.iif.gt.nifs)call bug('f',
     *                          'Invalid Ant or IF in PokeSC')
c
        tcorr = tcorr1
        chi = chi1
        axisrms(ant)=pntrms
        axismax(ant)=pntmax
        xtsys(iif,ant) = xtsys1
        ytsys(iif,ant) = ytsys1
        xyphase(iif,ant) = xyphase1
        xyamp(iif,ant) = xyamp1
c
c CABB
c
        if (cabb) then
          xgtp(iif,ant) = xgtp1
          ygtp(iif,ant) = ygtp1
          xsdo(iif,ant) = xsdo1
          ysdo(iif,ant) = ysdo1
          xcaljy(iif,ant) = xcaljy1
          ycaljy(iif,ant) = ycaljy1
        else
          xsampler(1,iif,ant) = xsamp(1)
          xsampler(2,iif,ant) = xsamp(2)
          xsampler(3,iif,ant) = xsamp(3)
c
          ysampler(1,iif,ant) = ysamp(1)
          ysampler(2,iif,ant) = ysamp(2)
          ysampler(3,iif,ant) = ysamp(3)
        endif
c
        newsc = .true.
c
        end
c***********************************************************************
        subroutine PokeSrc(srcnam,ra1,dec1,obsra1,obsdec1,
     *                                  pntra1,pntdec1,calcode1)
c
        character srcnam*(*)
        double precision ra1,dec1,obsra1,obsdec1,pntra1,pntdec1
        character calcode1*(*)
c
c  Flush out source information.
c-----------------------------------------------------------------------
        include 'atlod.h'
        double precision r1,d1,pntra,pntdec
        character line*80,sdash*80,calcode*16
        integer length,l
c
c  Externals.
c
        integer len1
        double precision Epo2Jul
c
c  Save ra and dec.
c
        ra = ra1
        dec = dec1
        pntra = pntra1
        pntdec = pntdec1
c
c  Some (all?) RPFITS files fail to give the apparent RA and DEC of
c  the source. Compute this if necessarry.
c
        if((obsra1.eq.0.and.abs(ra).gt.0.01).or.
     *     (obsdec1.eq.0.and.abs(dec).gt.0.01))then
          call precess(Epo2Jul(2000.d0,'J'),ra,dec,time,obsra,obsdec)
          call Nutate(time,obsra,obsdec,r1,d1)
          call Aberrate(time,r1,d1,obsra,obsdec)
        else
          obsra = obsra1
          obsdec = obsdec1
        endif
c
c  Fiddle the source name to be all lower case, and eliminate
c  any special characters or spaces.
c
        sdash = srcnam
        length = min(len1(srcnam),len(sdash))
        if(length.gt.0)call lcase(sdash(1:length))
        do l=1,length
          if((sdash(l:l).ge.'a'.and.sdash(l:l).le.'z').or.
     *       (sdash(l:l).ge.'0'.and.sdash(l:l).le.'9').or.
     *        sdash(l:l).eq.'+'.or. sdash(l:l).eq.'-'.or.
     *        sdash(l:l).eq.'.')then
            continue
          else
            sdash(l:l) = '_'
          endif
        enddo
c
c  Give a message about a new source.
c
        if(srcnam.ne.sname.and.length.gt.0)then
          line = 'Source: '//sdash(1:length)
          call output(line)
          sname = srcnam
        endif
c
        if(length.gt.0)call uvputvra(tno,'source',sdash(1:length))
        call uvputvrd(tno,'ra',ra,1)
        call uvputvrd(tno,'dec',dec,1)
        call uvputvrd(tno,'pntra',pntra,1)
        call uvputvrd(tno,'pntdec',pntdec,1)
        call uvputvrd(tno,'obsra',obsra,1)
        call uvputvrd(tno,'obsdec',obsdec,1)
c
        calcode = calcode1
        length = len1(calcode)
        if(length.eq.0)then
          calcode = 'none'
          length = len1(calcode)
        endif
        call uvputvra(tno,'calcode',calcode(1:length))
c
        newpnt = .true.
        end
c***********************************************************************
        subroutine PokeAnt(n,x,y,z,sing)
c
        integer n
        double precision x(n),y(n),z(n)
        logical sing
c
c  Set antenna coordinates.
c
c-----------------------------------------------------------------------
        include 'atlod.h'
        include 'mirconst.h'
        double precision r,z0,cost,sint,temp,antpos(3*ATANT)
        integer i
        logical more
c
c  Check the number of antennas.
c
        if(n.ne.nants)call bug('f',
     *                  'Inconsistent no. antennas, in PokeAnt')
c
c  Convert them to the Miriad system: y is local East, z is parallel to
c  pole.  Units are nanosecs.
c
        i = 1
        more = .true.
        dowhile(more)
          r = sqrt(x(i)*x(i) + y(i)*y(i))
          more = r.eq.0
          if(more)i = i + 1
          more = more.and.i.le.nants
        enddo
        if(i.gt.nants)then
          if(.not.sing)
     *      call bug('w','Antenna table is identically 0!!')
          cost = 1
          sint = 0
          z0 = 0
        else
          cost = x(i) / r
          sint = y(i) / r
          z0 = z(i)
        endif
c
        do i=1,nants
          temp = x(i)*cost + y(i)*sint - r
          antpos(i)         = (1d9/DCMKS) * temp
          temp = -x(i)*sint + y(i)*cost
          antpos(i+nants)   = (1d9/DCMKS) * temp
          antpos(i+2*nants) = (1d9/DCMKS) * (z(i)-z0)
        enddo
        call uvputvrd(tno,'antpos',antpos,3*nants)
c
        end
c***********************************************************************
        subroutine PokeData(u1,v1,w1,baseln,iif,bin,vis,nfreq1,nstoke1,
     *          flag1,inttime1,docon,doxyflip,xymode)
c
        integer nfreq1,nstoke1,iif,baseln,bin,xymode
        real u1,v1,w1,inttime1
        logical flag1(nstoke1),docon,doxyflip
        complex vis(nfreq1*nstoke1)
c
c  Buffer up the data. Perform sampler correction and hanning smoothing
c  if needed.
c
c  Inputs:
c    xymode     If 0, then do not do any xy phase correction.
c               If negative, do xy phase correction on all antennas.
c               If positive, do xy phase correction for that antenna
c               alone.
c-----------------------------------------------------------------------
        integer PolXX,PolYY,PolXY,PolYX
        parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
        include 'atlod.h'
        include 'mirconst.h'
        integer ipnt,i1,i2,bl,p,pol,iXX,iXY,iYX,iYY,i
        logical doconj,doneg
        real rscr(2*ATCONT-2),gtp,sdo
        complex cscr(ATCONT)
c
        if(iif.gt.nifs)call bug('f',
     *          'Incorrect IF number')
        if(nstoke1.ne.nstoke(iif))call bug('f',
     *          'Inconsistent number of polarisations')
        i2 = baseln / 256
        i1 = mod(baseln,256)
        doconj = docon
        if(i1.gt.i2)then
          doconj = .not.doconj
          bl = ((i1-1)*i1)/2 + i2
          u(bl) = -u1 / (1e-9*CMKS)
          v(bl) = -v1 / (1e-9*CMKS)
          w(bl) = -w1 / (1e-9*CMKS)
          dosw(bl) = .false.
        else
          bl = ((i2-1)*i2)/2 + i1
          u(bl) = u1 / (1e-9*CMKS)
          v(bl) = v1 / (1e-9*CMKS)
          w(bl) = w1 / (1e-9*CMKS)
          dosw(bl) = .true.
        endif
c
c  Save the integration time.
c
        if(inttime1.gt.0)then
          inttime(bl) = inttime1
        else
          inttime(bl) = 10
        endif
        inttim = max(inttim,inttime(bl))
c
c  Remove the phase switching.
c
        if(dopmps)
     *    call deswitch(vis,nstoke(iif),nfreq1,time,inttime(bl),i1,i2)
c
c  Reweight the data, if needed.
c
        if(dowt.and.nfreq1.eq.ATCONT)
     *    call Reweight(vis,cscr,rscr,nstoke(iif),nfreq1,wts)
c
c  Do the 8 MHz de-birdie'ing operation.
c
        if(birdie.and.
     *     abs(abs(nfreq(iif)*sdf(iif))-0.008).lt.0.0001)then
          call do8(vis,nstoke(iif),nfreq(iif),sfreq(iif),sdf(iif))
        endif
c
c  Allocate buffer slots for each polarisation. Save the flags, Copy the
c  data to the output. Do sampler corrections.
c
        if(bin.gt.ATBIN)call bug('f','Invalid pulsar bin number')
        do p=1,nstoke(iif)
          ipnt = nused + 1
          pnt(iif,p,bl,bin) = ipnt
          flag(iif,p,bl,bin) = flag1(p)
          nbin(iif) = max(nbin(iif),bin)
          nused = nused + nfreq(iif)
          if(nused.gt.ATDATA)call bug('f','Buffer overflow in PokeData')
          doneg = (polcode(iif,p).eq.PolXY.or.
     *             polcode(iif,p).eq.PolYX).and.doxyflip
          call DatCpy(nstoke(iif),nfreq(iif),nfreq1,
     *          dohann.and.nfreq1.gt.33,
     *          birdie.and.nfreq1.eq.33.and..not.cabb,
     *          edge(iif),doconj,doneg,vis(p),data(ipnt))
          if(dosam)call SamCorr(nfreq(iif),data(ipnt),polcode(iif,p),
     *          i2,i1,iif,time,xsampler,ysampler,ATIF,ATANT)
c
c  Do XY phase correction if needed.
c
          if(doxyp.and.xymode.ne.0)then
            if(dosw(bl))then
              pol = polcode(iif,p)
              if(pol.eq.PolXY)then
                pol = PolYX
              else if(pol.eq.PolYX)then
                pol = PolXY
              endif
              call XypCorr(nfreq(iif),data(ipnt),pol,
     *          i1,i2,iif,xyphase,ATIF,ATANT,xymode)
            else
              call XYpCorr(nfreq(iif),data(ipnt),polcode(iif,p),
     *          i2,i1,iif,xyphase,ATIF,ATANT,xymode)
            endif
          endif
          if (polcode(iif,p).eq.polXX) iXX=p
          if (polcode(iif,p).eq.polXY) iXY=p
          if (polcode(iif,p).eq.polYX) iYX=p
          if (polcode(iif,p).eq.polYY) iYY=p
        enddo
        
c
c  Undo online Tsys correction/calibration if needed
c        
        if (.not.dotsys.and.xsdo(iif,i1).gt.0.and.xsdo(iif,i2).gt.0.and.
     *       ysdo(iif,i1).gt.0.and.ysdo(iif,i2).gt.0.and.
     *       xcaljy(iif,i1).gt.0.and.xcaljy(iif,i2).gt.0.and.
     *       ycaljy(iif,i1).gt.0.and.ycaljy(iif,i2).gt.0) then
          do i=1,nfreq(iif)
            data(pnt(iif,iXX,bl,bin)+i-1)=data(pnt(iif,iXX,bl,bin)+i-1)*
     *         sqrt(xsdo(iif,i1)*xsdo(iif,i2)/
     *              xcaljy(iif,i1)/xcaljy(iif,i2))/1.e6
            data(pnt(iif,iXY,bl,bin)+i-1)=data(pnt(iif,iXY,bl,bin)+i-1)*
     *         sqrt(xsdo(iif,i1)*ysdo(iif,i2)/
     *              xcaljy(iif,i1)/ycaljy(iif,i2))/1.e6
            data(pnt(iif,iYX,bl,bin)+i-1)=data(pnt(iif,iYX,bl,bin)+i-1)*
     *         sqrt(ysdo(iif,i1)*xsdo(iif,i2)/
     *              ycaljy(iif,i1)/xcaljy(iif,i2))/1.e6
            data(pnt(iif,iYY,bl,bin)+i-1)=data(pnt(iif,iYY,bl,bin)+i-1)*
     *         sqrt(ysdo(iif,i1)*ysdo(iif,i2)/
     *              ycaljy(iif,i1)/ycaljy(iif,i2))/1.e6
          enddo
        endif
c
c       Process auto correlation bins and produce sdo, gtp and 
c       xyphase spectra for the continuum bands. The first IF contains
c       the first cont. band, the 2nd cont band comes after the zooms
c       for band 1.
c        
        if(dopack.and.bin.eq.2.and.i1.eq.i2) then
          do i=1,nfreq(iif)
            sdo=real(data(pnt(iif,iXX,bl,2)+i-1) -
     *               data(pnt(iif,iXX,bl,1)+i-1))
            gtp=real(data(pnt(iif,iXX,bl,2)+i-1) +
     *               data(pnt(iif,iXX,bl,1)+i-1))/2
            data(pnt(iif,iXX,bl,1)+i-1)=cmplx(gtp,sdo)
            sdo=real(data(pnt(iif,iYY,bl,2)+i-1) -
     *               data(pnt(iif,iYY,bl,1)+i-1))
            gtp=real(data(pnt(iif,iYY,bl,2)+i-1) +
     *               data(pnt(iif,iYY,bl,1)+i-1))/2
            data(pnt(iif,iYY,bl,1)+i-1)=cmplx(gtp,sdo)
            data(pnt(iif,iYX,bl,1)+i-1)=data(pnt(iif,iXY,bl,2)+i-1)-
     *       data(pnt(iif,iXY,bl,1)+i-1)
          enddo
        endif
c
        end
c***********************************************************************
        subroutine do8(vis,npol,nfreq,sfreq,sdf)
c
        integer nfreq,npol
        complex vis(npol,nfreq)
        double precision sfreq,sdf
c-----------------------------------------------------------------------
        include 'maxdim.h'
        double precision flo
        integer chan,i,j,n
        complex cbuff(MAXDIM+1),ctmp
        real    rbuff(2*MAXDIM)
c
        if(abs(sdf).eq.0)call bug('f',
     *    'Cannot use options=birdie when channel width is unknown')
        flo = sfreq + 0.5*(nfreq-1)*sdf
        flo = 0.128d0 * nint(flo/0.128d0)
        chan = nint((flo - sfreq)/sdf) + 1
        if(chan.le.0) chan = chan + nint(0.128d0/abs(sdf))
        if(chan.le.nfreq)then
          if(nfreq.gt.MAXDIM+1)
     *      call bug('f','Buffer overflow when debirding 8MHz')

          n = 2*nfreq
          if (mod(nfreq,2).eq.1) n = n - 2

          do j=1,npol
            do i=1,nfreq
              cbuff(i) = vis(j,i)
            enddo
            ctmp = cbuff(chan)
            cbuff(chan) = 0
            call fftcr(cbuff,rbuff,-1,n)
            rbuff(nfreq) = 0
            call fftrc(rbuff,cbuff,1,n)
            cbuff(chan) = n*ctmp
            do i=1,nfreq
              vis(j,i) = cbuff(i)/n
            enddo
          enddo
        endif
c
        end
c***********************************************************************
        subroutine deswitch(vis,npol,nchan,time,inttime,i1,i2)
c
        integer npol,nchan,i1,i2
        complex vis(nchan,npol)
        double precision time
        real inttime
c
c  Correct for on-line phase switching.
c
c-----------------------------------------------------------------------
        integer i,j,istate
        real s
        double precision rinttime
c
        integer states(8,6)
        data states/
     *  +1,-1,+1,-1,+1,-1,+1,-1,
     *  +1,+1,-1,-1,+1,+1,-1,-1,
     *  +1,-1,-1,+1,+1,-1,-1,+1,
     *  +1,+1,+1,+1,-1,-1,-1,-1,
     *  +1,+1,-1,-1,-1,-1,+1,+1,
     *  +1,+1,+1,+1,+1,+1,+1,+1/
c
        rinttime = 5.d0 * nint(inttime/5.0) / 86400.d0
        istate = mod(nint(mod(time,1.0d0)/rinttime),8) + 1
c
        s = 1
        if(states(istate,i1).ne.states(istate,i2)) s = -1
c
        do j=1,npol
          do i=1,nchan
            vis(i,j) = s * vis(i,j)
          enddo
        enddo
c
        end
c***********************************************************************
        subroutine XypCorr(nfreq,vis,pol,i1,i2,iif,xyphase,ATIF,ATANT,
     *                     xymode)
c
        integer nfreq,pol,i1,i2,iif,ATIF,ATANT,xymode
        complex vis(nfreq)
        real xyphase(ATIF,ATANT)
c
c  Correct the data with the measured XY phase.
c
c-----------------------------------------------------------------------
        integer PolXX,PolYY,PolXY,PolYX
        parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
        integer i
        complex fac
        real theta,theta1,theta2
c
        theta1 = 0
        if(xymode.eq.i1.or.xymode.lt.0)theta1 = xyphase(iif,i1)
        theta2 = 0
        if(xymode.eq.i2.or.xymode.lt.0)theta2 = xyphase(iif,i2)
c
        theta = 0
        if(pol.eq.PolYY)then
          theta = theta1 - theta2
        else if(pol.eq.PolXY)then
          theta =        - theta2
        else if(pol.eq.PolYX)then
          theta = theta1
        endif
c
        if(theta.ne.0)then
          fac = cmplx(cos(theta),-sin(theta))
          do i=1,nfreq
            vis(i) = fac*vis(i)
          enddo
        endif
c
        end
c***********************************************************************
        subroutine Reweight(vis,cscr,rscr,npol,nchan,wts)
c
        integer npol,nchan
        complex vis(npol,nchan),cscr(nchan)
        real wts(2*nchan-2),rscr(2*nchan-2)
c
c  Reweight the lag spectrum.
c-----------------------------------------------------------------------
        integer i,p
c
        do p=1,npol
          do i=1,nchan
            cscr(i) = vis(p,i)
          enddo
          call fftcr(cscr,rscr,-1,2*nchan-2)
          do i=1,2*nchan-2
            rscr(i) = rscr(i)*wts(i)/real(2*nchan-2)
          enddo
          call fftrc(rscr,cscr, 1,2*nchan-2)
          do i=1,nchan
            vis(p,i) = cscr(i)
          enddo
        enddo
c
        end
c***********************************************************************
        subroutine PokeMisc(telescop,observer,version,sctype)
c
        character telescop*(*),observer*(*),version*(*),sctype*(*)
c
c  Set various miscellaneous parameters.
c-----------------------------------------------------------------------
        include 'atlod.h'
        character atemp*32
        integer length
c
        call AsciiCpy(telescop,atemp,length)
        if(length.gt.0)then
          if (atemp(1:length).eq.'ATCABB') then
            call uvputvra(tno,'telescop',atemp(1:length-2))
            call uvputvra(tno,'instrume',atemp(3:length))
          else
            call uvputvra(tno,'telescop',atemp(1:length))
            call uvputvra(tno,'instrume',atemp(1:length))
          endif
        endif
c
        call AsciiCpy(observer,atemp,length)
        if(length.gt.0)call uvputvra(tno,'observer',atemp(1:length))
c
        call AsciiCpy(version,atemp,length)
        if(length.gt.0)call uvputvra(tno,'version',atemp(1:length))
c
        call AsciiCpy(sctype,atemp,length)
        if(length.gt.0)call uvputvra(tno,'sctype',atemp(1:length))
c
        end
c***********************************************************************
        subroutine PokeFlsh
c
c
c  Flush out a saved integration.
c-----------------------------------------------------------------------
        include 'atlod.h'
        include 'mirconst.h'
        integer NDATA, NDIV
        parameter(NDATA=MAXCHAN*MAXWIN, NDIV=32)
        integer i,i1,i2,iif,p,bl,nchan,npol,ipnt,ischan(ATIF)
        integer tbinhi,tbin,binhi,binlo,bin,k
        complex vis(NDATA)
        logical flags(NDATA),doopcorr,wband
        double precision preamble(5),vel,lst,tdash,az,el
        real buf(3*ATANT*ATIF),fac(NDIV*ATIF),freq0(NDIV*ATIF)
        real jyperk,tfac,Tb(NDIV*ATIF)
c
c  Externals.
c
        double precision eqeq
        real getjpk
c
        if(nused.eq.0)return
c
c  Check that we can do what is asked.
c

c
c  In the case where we've packed bin 2 into bin 1 (for tsys and xyphase
c  calculations later) we discard bin 2 here.
c
        do iif=1,nifs
          if (nbin(iif).eq.2.and.dopack) nbin(iif)=1
        enddo 

        if(newfreq)then
          if(doif)then
            do iif=2,nifs
              if(nbin(iif).ne.nbin(1))    call bug('f',
     *          'Number of bins differ between IFs. '//
     *          'Use options=noif.')
              if(nstoke(iif).ne.nstoke(1))call bug('f',
     *          'Number of polarisations differ between IFs. '//
     *          'Use options=noif.')
              do p=1,nstoke(iif)
                if(polcode(iif,p).ne.polcode(1,p))call bug('f',
     *          'Polarisation types differ between IFs. '//
     *          'Use options=noif.')
              enddo
            enddo
          else if(hires)then
            do iif=2,nifs
              if(nbin(iif).ne.nbin(1))    call bug('f',
     *          'Number of bins in different IFs must '//
     *                             'agree for options=hires')
            enddo
          endif
        endif
c
c  Compute apparent LST.
c
        if(hires)then
          tdash  = time - 0.5*inttim*(nbin(1)-1)/86400.0d0
          tbinhi = nbin(1)
        else
          tdash  = time
          tbinhi = 1
        endif
c
c  Write out the reference pointing data.
c
        if(refnant.gt.0)then
          call uvputvrr(tno,'refpnt',refpnt,2*refnant)
        endif
c
c  Write out the met data.
c
        if(mcount.ge.5)then
          call uvputvrr(tno,'airtemp',mdata(1),1)
          call uvputvrr(tno,'pressmb',0.975*mdata(2),1)
          call uvputvrr(tno,'relhumid',mdata(3),1)
          call uvputvrr(tno,'wind',mdata(4),1)
          call uvputvrr(tno,'winddir',mdata(5),1)
        endif
        if(mcount.ge.7)then
c         call uvputvrr(tno,'metflag',mdata(6),1)
          call uvputvrr(tno,'rain',mdata(7),1)
        endif
        if(mcount.ge.9)then
c         call uvputvrr(tno,'smonphas',mdata(8),1)
          call uvputvrr(tno,'smonrms', mdata(9),1)
        endif
c
        do tbin=1,tbinhi
          call jullst(tdash,long,lst)
          lst = lst + eqeq(tdash)
          call uvputvrd(tno,'lst',lst,1)
c
c  Compute the az and el of the telescopes.c
c
          call azel(obsra,obsdec,lst,lat,az,el)
          call uvputvrd(tno,'antaz',az*180.d0/DPI,1)
          call uvputvrd(tno,'antel',el*180.d0/DPI,1)
c
c  Get ready to apply opacity correction.
c
          jyperk = getjpk(real(sfreq(1)))

          k=0
          do iif=1,nifs
            do i=1,nopcorr
              k=k+1
              freq0(k) = sfreq(iif)*1e9 + 
     *         (nfreq(iif)-1)/(nopcorr-1.0)*(i-1)*sdf(iif)*1e9
            enddo
          enddo
          wband = freq0(1).gt.75e9
          doopcorr = .false.
          if(opcorr.and..not.wband)then
            if(mcount.lt.3)then
              call bug('w','No met data to compute opacity correction')
            else
c
c  Convert to standard units. Temperature is converted from Celsius
c  to Kelvin. Pressure is converted from hPa at sea level to Pa at
c  Narrabri's altitude (a factor of 97.5 rather than 100). Relative
c  humidity is converted from percent to a fraction.
c
              stemp = mdata(1) + 273.15
              spress = 97.5*mdata(2)
              shumid = 0.01*mdata(3)
            endif
            call opacGet(nopcorr*nifs,freq0,real(el),stemp,spress,
     *                   shumid,fac,Tb)
            doopcorr = .true.
            tfac = 1
            k=0
            do iif=1,nifs
              do i=1,nopcorr
                k=k+1
                fac(k) = 1/fac(k)
                tfac = tfac * fac(k)
              enddo
            enddo
            jyperk = jyperk * tfac**(1.0/real(nopcorr*nifs))
          else
            k=0
            do iif=1,nifs
              do i=1,nopcorr
                k=k+1
                fac(k) = 1
              enddo
            enddo
          endif
c
c  Compute radial velocity of the observatory.
c
          call VelRad(.not.dobary,tdash,obsra,obsdec,ra,dec,lst,lat,vel)
          call uvputvrr(tno,'veldop',real(vel),1)
          call uvputvrr(tno,'jyperk',jyperk,1)

c
c         CABB data: mark packed on/off data in auto correlations
          if (dopack) call uvputvri(tno,'packauto',1,1)
c
c  Handle the case that we are writing the multiple IFs out as multiple
c  records.
c
          if(.not.doif.and.nifs.gt.1)then
            k=1
            do iif=1,nifs
              call uvputvri(tno,'nspect',1,1)
              call uvputvri(tno,'npol',  nstoke(iif),1)
              call uvputvri(tno,'nschan',nfreq(iif),1)
              call uvputvri(tno,'ischan',1,1)
              call uvputvrd(tno,'sfreq', sfreq(iif),1)
              call uvputvrd(tno,'sdf',   sdf(iif),  1)
              call uvputvrd(tno,'restfreq',restfreq(iif),1)
              call uvputvri(tno,'ifchain',ifchain(iif),1)
              
              if(newsc)call ScOut(tno,chi,tcorr,
     *            xtsys,ytsys,xyphase,xyamp,
     *            xsampler,ysampler,xgtp,ygtp,xsdo,ysdo,xcaljy,ycaljy,
     *            axisrms,axismax,cabb,
     *            ATIF,ATANT,nants,iif,iif,buf)
              if(hires)then
                binlo = tbin
                binhi = tbin
              else
                binlo = 1
                binhi = nbin(iif)
              endif
              do bin=binlo,binhi
                if(.not.hires)call uvputvri(tno,'nbin',nbin(iif),1)
                bl = 0
                do i2=1,nants
                  do i1=1,i2
                    bl = bl + 1
                    preamble(1) = u(bl)
                    preamble(2) = v(bl)
                    preamble(3) = w(bl)
                    preamble(4) = tdash
                    preamble(5) = 256*i1 + i2
                    do p=1,nstoke(iif)
                      ipnt = pnt(iif,p,bl,bin)
                      if(ipnt.gt.0)then
                        call PolPut(tno,polcode(iif,p),dosw(bl))
                        call GetFlag(flag(iif,p,bl,bin),nfreq(iif),
     *                                            bchan(iif),flags)
                        call FlagNaN(data(ipnt),flags,nfreq(iif))
                        call rfiFlag(flags,NDATA,1,nfreq(iif),
     *                         sfreq(iif),sdf(iif),birdie,edgepc,tdash)
                        if(.not.hires)call uvputvri(tno,'bin',bin,1)
                        call uvputvrr(tno,'inttime',inttime(bl),1)
                        if(doopcorr)
     *                    call opapply(data(ipnt),nfreq(iif),sfreq(iif),
     *                       sdf(iif),fac(k),
     *                       freq0(k),nopcorr)
                        call uvwrite(tno,preamble,data(ipnt),flags,
     *                                                  nfreq(iif))

                      endif
                    enddo
                  enddo
                enddo
              enddo
              k=k+nopcorr
            enddo
c
c  Handle the case were we are writing the multiple IFs out as a single
c  record.
c
          else
            if(newfreq.and.tbin.eq.1)then
              ischan(1) = 1
              do iif=2,nifs
                ischan(iif) = ischan(iif-1) + nfreq(iif-1)
              enddo
              call uvputvri(tno,'nspect',nifs,1)
              call uvputvri(tno,'ischan',ischan,nifs)
              call uvputvri(tno,'nschan',nfreq,nifs)
              call uvputvrd(tno,'sfreq', sfreq,nifs)
              call uvputvrd(tno,'sdf',   sdf,nifs)
              call uvputvrd(tno,'restfreq',restfreq,nifs)
              call uvputvri(tno,'ifchain',ifchain,nifs)
              if(.not.hires)call uvputvri(tno,'nbin',nbin(1),1)
            endif
            if(newsc.and.tbin.eq.1)call ScOut(tno,chi,tcorr,
     *          xtsys,ytsys,xyphase,xyamp,
     *          xsampler,ysampler,xgtp,ygtp,xsdo,ysdo,xcaljy,ycaljy,
     *          axisrms,axismax,cabb,ATIF,ATANT,nants,1,nifs,buf)
            if(hires)then
              binlo = tbin
              binhi = tbin
            else
              binlo = 1
              binhi = nbin(1)
            endif
            do bin=binlo,binhi
              bl = 0
              do i2=1,nants
                do i1=1,i2
                  bl = bl + 1
                  preamble(1) = u(bl)
                  preamble(2) = v(bl)
                  preamble(3) = w(bl)
                  preamble(4) = tdash
                  preamble(5) = 256*i1 + i2
                  call CntStok(npol,pnt(1,1,bl,bin),nifs,nstoke(1),ATIF)
                  if(npol.gt.0)then
                    call uvputvri(tno,'npol',npol,1)
                    do p=1,nstoke(1)
                      call GetDat(data,nused,pnt(1,p,bl,bin),
     *                  flag(1,p,bl,bin),nfreq,sfreq,sdf,ATIF,
     *                  fac,freq0,bchan,nifs,vis,flags,NDATA,nchan,
     *                  nopcorr)
                      if(nchan.gt.0)then
                        call rfiFlag(flags,NDATA,nifs,nfreq,sfreq,
     *                             sdf,birdie,edgepc,tdash)
                        if(.not.hires)call uvputvri(tno,'bin',bin,1)
                        call uvputvri(tno,'pol',polcode(1,p),1)
                        call uvputvrr(tno,'inttime',inttime(bl),1)
                        call uvwrite(tno,preamble,vis,flags,nchan)
                      endif
                    enddo
                  endif
                enddo
              enddo
            enddo
          endif
c
          tdash = tdash + inttim/86400.0d0
        enddo
c
c  Reset the counters, etc.
c
        inttim = 0
        do bl=1,ATBASE
          do p=1,ATPOL
            do iif=1,nifs
              do bin=1,nbin(iif)
                pnt(iif,p,bl,bin) = 0
              enddo
            enddo
          enddo
        enddo
c
        do iif=1,nifs
          nbin(iif) = 0
        enddo
c
        mcount = 0
        refnant = 0
        nused = 0
        newsc   = .false.
        newfreq = .false.
        newpnt  = .false.
        end
c      
c***********************************************************************

        subroutine flagnan(data,flags,nchan)
c
        integer nchan
        logical flags(nchan)
        complex data(nchan)
c-----------------------------------------------------------------------
        integer i,cnt
        complex tmp
        integer nantest(2)
        equivalence(tmp,nantest(1))
c
        cnt=0
        do i=1,nchan
          tmp=data(i)
c
c         Flag all data in the 'quiet' NaN range
c
          if ((nantest(1).ge.-8388608 .and. nantest(1).le.-1) .or.
     *     (nantest(1).ge.2139095040.and.nantest(1).le.2147483647)) then
            flags(i)=.false.
            data(i)=0.
            cnt=cnt+1

c
c         Also flag all data that is exactly zero
c
          else if (data(i).eq.0) then
            flags(i)=.false.
            cnt=cnt+1
          endif
        enddo
c
        end
c***********************************************************************
        subroutine opapply(data,nchan,sfreq,sdf,fac,freq,nop)
c
        integer nchan,nop
        double precision sfreq,sdf
        real fac(nop),freq(nop)
        complex data(nchan)
c-----------------------------------------------------------------------
        integer i,i1,i2
        real f,df
c
c       do piecewise linear interpolation across spectrum
c
        if (nchan.gt.1) then
          do i=1,nchan
            i1 = 1+(i-1)/(nchan/(nop-1.0))
            i2 = i1+1
            f = (sfreq + (i-1)*sdf)*1d9
            df = freq(i2)-freq(i1)
            data(i) = (fac(i1)*(freq(i2)-f)/df+
     *                 fac(i2)*(f-freq(i1))/df)
     *                 * data(i)
          enddo
        else
          data(1)=data(1)*fac(1)
        endif
c
        end
c***********************************************************************
        subroutine PolPut(tno,pol,dosw)
c
        integer tno,pol
        logical dosw
c
c  Write out the polarisation flag. If "dosw" is true, switch
c  PolXY to PolYX and visa versa.
c
c-----------------------------------------------------------------------
        integer PolXX,PolYY,PolXY,PolYX
        parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
        if(dosw)then
          if(pol.eq.PolXY)then
            call uvputvri(tno,'pol',PolYX,1)
          else if(pol.eq.PolYX)then
            call uvputvri(tno,'pol',PolXY,1)
          else
            call uvputvri(tno,'pol',pol,1)
          endif
        else
          call uvputvri(tno,'pol',pol,1)
        endif
c
        end
c***********************************************************************
c***********************************************************************
        subroutine VelRad(dolsr,time,raapp,decapp,raepo,decepo,
     *    lst,lat,vel)
c
        logical dolsr
        double precision time,raapp,decapp,raepo,decepo
        double precision lst,lat,vel
c
c  Compute the radial velocity of the observatory, in the direction of
c  a source, with respect to either LSR or the barycentre.
c
c  Input:
c    dolsr      If true, compute LSR velocity. Otherwise barycentric.
c    time       Time of interest (Julian date).
c    raapp,decapp Apparent RA and DEC (radians).
c    raepo,decepo RA and DEC at the J2000 epoch (radians).
c    lat        Observatory geodetic latitude (radians).
c    lst        Local sideral time (radians).
c  Output:
c    vel        Radial velocity.
c-----------------------------------------------------------------------
        double precision lmn2000(3),lmnapp(3)
        double precision velsite(3),posearth(3),velearth(3),velsun(3)
        integer i
c
c  Compute barycentric velocity.
c
        call sph2lmn(raapp,decapp,lmnapp)
        call vsite(lat,lst,velsite)
        call vearth(time,posearth,velearth)
        vel = 0
        do i=1,3
          vel = vel - (velsite(i) + velearth(i))*lmnapp(i)
        enddo
c
c  To compute LSR velocity, we need the source position in J2000
c  coordinates.  Vsun returns the Suns LSR velocity in the J2000 frame.
c  Add this contribution to the velocity we already have.
c
        if(dolsr)then
          call sph2lmn(raepo,decepo,lmn2000)
          call vsun(velsun)
          do i=1,3
            vel = vel + lmn2000(i)*velsun(i)
          enddo
        endif
c
        end
c***********************************************************************
        subroutine DatCpy(nstoke,nfreq,nfreq1,dohann,skip,edge,
     *                                  doconj,doneg,in,out)
c
        integer nstoke,nfreq,nfreq1,edge
        logical dohann,doconj,doneg,skip
        complex in(nstoke,nfreq1),out(nfreq)
c
c  Copy the data to an output buffer, conjugating and going
c  Hanning smoothing (if necessary) as we go.
c-----------------------------------------------------------------------
        integer i,id
c
        if(dohann)then
          if(nfreq.ne.(nfreq1-2*edge+1)/2)
     *          call bug('f','Incorrect dim info, in DatCpy')
          id = edge + 1
          do i=1,nfreq
            out(i) = 0.25*(in(1,id-1)+in(1,id+1)) + 0.5*in(1,id)
            id = id + 2
          enddo
c
        else if(skip)then
          if(nfreq.ne.(nfreq1-2*edge+1)/2)
     *          call bug('f','Incorrect dim info, in DatCpy')
          id = edge + 1
          do i=1,nfreq
            out(i) = in(1,id)
            id = id + 2
          enddo
c
        else
          if(nfreq.ne.nfreq1-2*edge)
     *          call bug('f','Incorrect dim info, in DatCpy')
          id = edge + 1
          do i=1,nfreq
            out(i) = in(1,id)
            id = id + 1
          enddo
        endif
c
c  Do we need to negate?
c
        if(doneg)then
          do i=1,nfreq
            out(i) = -out(i)
          enddo
        endif
c
c  Do we need to conjugate?
c
        if(doconj)then
          do i=1,nfreq
            out(i) = conjg(out(i))
          enddo
        endif
c
        end
c***********************************************************************
        subroutine GetFlag(flag,n,bchan,flags)
c
        integer n,bchan
        logical flag,flags(n)
c
c  Set the flags.
c-----------------------------------------------------------------------
        integer i
c
        do i=1,n
          flags(i) = flag
        enddo
        if(bchan.ge.1.and.bchan.le.n)flags(bchan) = .false.
c
        end
c***********************************************************************
        subroutine ScOut(tno,chi,tcorr,xtsys,ytsys,xyphase,xyamp,
     *          xsampler,ysampler,xgtp,ygtp,xsdo,ysdo,xcaljy,ycaljy,
     *          axisrms,axismax,cabb,
     *          ATIF,ATANT,nants,if1,if2,buf)
c
        integer tno,ATIF,ATANT,nants,if1,if2,tcorr
        real chi,xtsys(ATIF,ATANT),ytsys(ATIF,ATANT)
        real xyphase(ATIF,ATANT),xyamp(ATIF,ATANT)
        real xsampler(3,ATIF,ATANT),ysampler(3,ATIF,ATANT)
        real xgtp(ATIF,ATANT),ygtp(ATIF,ATANT)
        real xsdo(ATIF,ATANT),ysdo(ATIF,ATANT)
        real xcaljy(ATIF,ATANT),ycaljy(ATIF,ATANT)
        real buf(3*ATIF*ATANT),axisrms(ATANT),axismax(ATANT)
        logical cabb
c
c  Write out a syscal record into the appropriate Miriad variables.
c
c  Input:
c    n          IF number to write out. If this is zero, all IFs are
c               written out.
c  Scratch:
c    buf        Used to buffer the data before writing.
c-----------------------------------------------------------------------
        call uvputvrr(tno,'chi',chi,1)
        call uvputvri(tno,'tcorr',tcorr,1)
        call Sca(tno,'axisrms', axisrms, nants,buf)
        call Sca(tno,'axismax', axismax, nants,buf)
        call Sco(tno,'xtsys',   xtsys,   1,ATIF,ATANT,if1,if2,nants,buf)
        call Sco(tno,'ytsys',   ytsys,   1,ATIF,ATANT,if1,if2,nants,buf)
        call Sct(tno,'systemp',xtsys,ytsys,ATIF,ATANT,if1,if2,nants,buf)
        call Sco(tno,'xyphase', xyphase, 1,ATIF,ATANT,if1,if2,nants,buf)
        call Sco(tno,'xyamp',   xyamp,   1,ATIF,ATANT,if1,if2,nants,buf)
        if (cabb) then
          call Sco(tno,'xgtp',  xgtp,  1,ATIF,ATANT,if1,if2,nants,buf)
          call Sco(tno,'ygtp',  ygtp,  1,ATIF,ATANT,if1,if2,nants,buf)
          call Sco(tno,'xsdo',  xsdo,  1,ATIF,ATANT,if1,if2,nants,buf)
          call Sco(tno,'ysdo',  ysdo,  1,ATIF,ATANT,if1,if2,nants,buf)
          call Sco(tno,'xcaljy',xcaljy,1,ATIF,ATANT,if1,if2,nants,buf)
          call Sco(tno,'ycaljy',ycaljy,1,ATIF,ATANT,if1,if2,nants,buf)
        else
          call Sco(tno,'xsampler',xsampler,3,ATIF,ATANT,
     *             if1,if2,nants,buf)
          call Sco(tno,'ysampler',ysampler,3,ATIF,ATANT,
     *             if1,if2,nants,buf)
        endif
        end
c***********************************************************************
        subroutine Sca(tno,var,axis,nants,buf)
c
        integer tno,nants
        character var*(*)
        real axis(nants),buf(2*nants)
c
c-----------------------------------------------------------------------
        integer i,j
c
        j = 1
        do i=1,nants
          buf(j) = axis(i)
          buf(j+1) = axis(i)
          j = j + 2
        enddo
c
        call uvputvrr(tno,var,buf,2*nants)
c
        end
c***********************************************************************
        subroutine Sct(tno,var,xtsys,ytsys,ATIF,ATANT,if1,if2,nants,buf)
c
        integer tno,ATIF,ATANT,if1,if2,nants
        character var*(*)
        real buf(ATIF*ATANT),xtsys(ATIF,ATANT),ytsys(ATIF,ATANT)
c
c  Write out the SYSTEMP variable, which we fudge to be the geometric
c  mean of the xtsys and ytsys variables.
c-----------------------------------------------------------------------
        integer ant,iif,cnt
c
        cnt = 0
        do iif=if1,if2
          do ant=1,nants
            cnt = cnt + 1
            buf(cnt) = sqrt(xtsys(iif,ant)*ytsys(iif,ant))
          enddo
        enddo
c
        call uvputvrr(tno,var,buf,cnt)
c
        end
c***********************************************************************
        subroutine Sco(tno,var,dat,ndim,ATIF,ATANT,if1,if2,nants,buf)
c
        integer ATIF,ATANT,if1,if2,nants,tno,ndim
        character var*(*)
        real dat(ndim,ATIF,ATANT),buf(ndim*ATIF*ATANT)
c
c  Write out a syscal variable.
c-----------------------------------------------------------------------
        integer ant,iif,cnt,n
c
        cnt = 0
        do iif=if1,if2
          do ant=1,nants
            do n=1,ndim
              cnt = cnt + 1
              buf(cnt) = dat(n,iif,ant)
            enddo
          enddo
        enddo
c
        call uvputvrr(tno,var,buf,cnt)
c
        end
c***********************************************************************
        subroutine CntStok(npol,pnt,nifs,nstoke,ATIF)
c
        integer npol,nifs,nstoke,ATIF,pnt(ATIF,nstoke)
c
c  Determine the number of valid Stokes records in this record.
c-----------------------------------------------------------------------
        logical valid
        integer p,iif
c
        npol = 0
        do p=1,nstoke
          valid = .false.
          do iif=1,nifs
            valid = valid.or.pnt(iif,p).gt.0
          enddo
          if(valid)npol = npol + 1
        enddo
c
        end
c***********************************************************************
        subroutine GetDat(data,nvis,pnt,flag,nfreq,sfreq,sdf,ATIF,
     *                    fac,freq0,bchan,nifs,vis,flags,ndata,nchan,
     *                    nopcorr)
c
        integer nvis,nifs,pnt(nifs),nfreq(nifs),bchan(nifs),nchan
        integer ndata,ATIF,nopcorr
        logical flag(nifs),flags(ndata)
        double precision sfreq(ATIF),sdf(ATIF)
        complex vis(ndata),data(nvis)
        real fac(nopcorr,nifs),freq0(nopcorr,nifs)
c
c  Construct a visibility record constructed from multiple IFs.
c-----------------------------------------------------------------------
        integer n,ipnt,i,nchand
c
        nchan = 0
        nchand = 0
        do n=1,nifs
          if(nchand+nfreq(n).gt.ndata)
     *                  call bug('f','Buffer overflow in GetDat')
          ipnt = pnt(n)
          if(ipnt.gt.0)then
            if(nchan.lt.nchand)then
              do i=nchan+1,nchand
                flags(i) = .false.
                vis(i) = 0
              enddo
              nchan = nchand
            endif
c
            if (nfreq(n).gt.1) then
              do i=nchan+1,nchan+nfreq(n)
                vis(i) = data(ipnt)
                flags(i) = flag(n)
                ipnt = ipnt + 1
              enddo
              call opapply(vis(nchan+1),nfreq(n),sfreq(n),sdf(n),
     *            fac(1,n),freq0(1,n),nopcorr)         
            else
              vis(nchan+1)=fac(1,n)*data(ipnt)
              flags(nchan+1)=flag(n)
              ipnt=ipnt+1
            endif
            if(bchan(n).ge.1.and.bchan(n).le.nfreq(n))
     *                  flags(nchan+bchan(n)) = .false.
            nchan = nchan + nfreq(n)
          endif
          nchand = nchand + nfreq(n)
        enddo
c
        if(nchan.lt.nchand.and.nchan.gt.0)then
          do i=nchan+1,nchand
            vis(i) = 0
            flags(i) = .false.
          enddo
          nchan = nchand
        endif

        call flagnan(vis,flags,nchan)
c
        end
c***********************************************************************
        subroutine AsciiCpy(in,out,length)
c
        character in*(*),out*(*)
        integer length
c
c-----------------------------------------------------------------------
        integer i
c
c  Externals.
c
        integer len1
c
        length = 0
        out = ' '
        do i=1,len(in)
          if(length.lt.len(out).and.in(i:i).ge.' ')then
            length = length + 1
            out(length:length) = in(i:i)
          endif
        enddo
c
        length = len1(out)
c
        end
c***********************************************************************
        subroutine SamCorr(nfreq,vis,pol,i1,i2,iif,time,
     *          xsampler,ysampler,ATIF,ATANT)
c
        integer i1,i2,iif,nfreq,pol,ATIF,ATANT
        real xsampler(3,ATIF,ATANT),ysampler(3,ATIF,ATANT)
        double precision time
        complex vis(nfreq)
c
c  Correct the data for the incorrect sampler statistics.
c
c-----------------------------------------------------------------------
        double precision J20Jun91,J21Aug93,J11Dec93
        parameter(J20Jun91=2448427.5d0,J21Aug93=2449220.5d0)
        parameter(J11Dec93=2449332.5d0)
        integer PolXX,PolYY,PolXY,PolYX
        parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
        real fac, ssexp
        integer i
c
c  Externals.
c
        real twobit_gain_adjust
c
        if(time.gt.J11Dec93)return
        ssexp = 17.3
        if(time.gt.J20Jun91.and.time.le.J21Aug93)ssexp=17.1
c
        if(pol.eq.PolXX)then
          fac = twobit_gain_adjust(ssexp,xsampler(1,iif,i1),
     *                                   xsampler(2,iif,i1),
     *                                   xsampler(3,iif,i1),
     *                                   xsampler(1,iif,i2),
     *                                   xsampler(2,iif,i2),
     *                                   xsampler(3,iif,i2))
        else if(pol.eq.PolYY)then
          fac = twobit_gain_adjust(ssexp,ysampler(1,iif,i1),
     *                                   ysampler(2,iif,i1),
     *                                   ysampler(3,iif,i1),
     *                                   ysampler(1,iif,i2),
     *                                   ysampler(2,iif,i2),
     *                                   ysampler(3,iif,i2))
        else if(pol.eq.PolXY)then
          fac = twobit_gain_adjust(ssexp,xsampler(1,iif,i1),
     *                                   xsampler(2,iif,i1),
     *                                   xsampler(3,iif,i1),
     *                                   ysampler(1,iif,i2),
     *                                   ysampler(2,iif,i2),
     *                                   ysampler(3,iif,i2))
        else if(pol.eq.PolYX)then
          fac = twobit_gain_adjust(ssexp,ysampler(1,iif,i1),
     *                                   ysampler(2,iif,i1),
     *                                   ysampler(3,iif,i1),
     *                                   xsampler(1,iif,i2),
     *                                   xsampler(2,iif,i2),
     *                                   xsampler(3,iif,i2))
        else
          fac = 1
        endif
c
        do i=1,nfreq
          vis(i) = fac * vis(i)
        enddo
c
        end
c***********************************************************************
c***********************************************************************
        subroutine RPSkip(in,iostat)
c
        character in*(*)
        integer iostat
c
c  Skip an RPFITS file.
c-----------------------------------------------------------------------
        call RPOpen(in,iostat)
        if(iostat.eq.0)call RPEOF(iostat)
        if(iostat.eq.0)call RPClose(iostat)
        end
c***********************************************************************
        subroutine RPDisp(in,scanskip,scanproc,doauto,docross,docaldat,
     *    relax,sing,unflag,nopol,polflag,dotsys,ifsel,nsel,
     *    userfreq,nuser,iostat)
c
        character in*(*)
        integer scanskip,scanproc,nsel,ifsel(nsel),nuser,iostat
        double precision userfreq(*)
        logical doauto,docross,relax,unflag,polflag,sing,docaldat,nopol,
     *          dotsys
c
c  Process an RPFITS file. Dispatch information to the
c  relevant Poke routine. Then eventually flush it out with PokeFlsh.
c
c  Inputs:
c    scanskip   Scans to skip.
c    scanproc   Number of scans to process. If 0, process all scans.
c    doauto     Save autocorrelation data.
c    docross    Save crosscorrelation data.
c    docaldat   Do not flag calibration data.
c    relax      Save data even if the SYSCAL record is bad.
c    sing
c    nopol      Select only the parallel-hand polarisations.
c    polflag    Flag all polarisations if any are bad.
c    unflag     Save data even though it may appear flagged.
c    dotsys     Use online Tsys calibration
c    ifsel      IFs to select. 0 means select all IFs.
c    userfreq   User-given rest frequency to override the value in
c               the RPFITS file.
c    nuser      Number of user-specificed rest frequencies.
c-----------------------------------------------------------------------
        include 'maxdim.h'
        include 'mirconst.h'
        integer MAXPOL,MAXSIM,MAXXYP,NDATA
        parameter(MAXPOL=4,MAXSIM=34,MAXXYP=5,NDATA=MAXCHAN*MAXPOL)
        double precision J01Jul04,J18Oct07
        parameter(J01Jul04=2453187.5d0,J18Oct07=2454390.5d0)
        include 'rpfits.inc'
        integer scanno,i1,i2,baseln,i,id,j,xymode,prev,next
        logical NewScan,NewSrc,NewFreq,NewTime,Accum,ok,badbit
        logical flags(MAXPOL),corrfud,kband,qband,wband,flipper,cabb
        integer jstat,flag,bin,ifno,srcno,simno,Ssrcno,Ssimno
        integer If2Sim(MAX_IF),nifs(MAX_IF),Sim2If(MAXSIM,MAX_IF)
        integer Sif(MAX_IF)
        real ut,utprev,utprevsc,u,v,w,weight(MAXCHAN*MAXPOL)
        complex vis(NDATA)
        double precision reftime,ra0,dec0,pntra,pntdec
        character calcode*16,sctype*16,sname*16
c
c  The following has to agree with the first dimension of if_cstok in
c  rpfits.inc.
c
        integer MPOL
        parameter(MPOL=4)
        integer nstoke(MAX_IF)
        character cstoke(MPOL,MAX_IF)*4
        logical pselect(MPOL,MAX_IF)
c
c  Information on flagging.
c
        integer nrec,nspec,fgbad,fgoffsrc,fginvant,fgsysc,fgsam,fgcal
c
c  Variables to track the sysc records.
c
        logical scinit(MAX_IF,ANT_MAX),scbuf(MAX_IF,ANT_MAX)
        logical xflag(MAX_IF,ANT_MAX),yflag(MAX_IF,ANT_MAX)
        integer ptag(MAXXYP,MAX_IF,ANT_MAX)
        integer atag(MAXXYP,MAX_IF,ANT_MAX)
        integer nxyp(MAX_IF,ANT_MAX),tcorr
        real xyp(MAXXYP,MAX_IF,ANT_MAX),xya(MAXXYP,MAX_IF,ANT_MAX)
        real xyphase(MAX_IF,ANT_MAX),xyamp(MAX_IF,ANT_MAX)
        real pntrms(ANT_MAX),pntmax(ANT_MAX)
        real xsamp(3,MAX_IF,ANT_MAX),ysamp(3,MAX_IF,ANT_MAX)
        real xtsys(MAX_IF,ANT_MAX),ytsys(MAX_IF,ANT_MAX)
        real xgtp(MAX_IF,ANT_MAX),ygtp(MAX_IF,ANT_MAX)
        real xsdo(MAX_IF,ANT_MAX),ysdo(MAX_IF,ANT_MAX)
        real xcaljy(MAX_IF,ANT_MAX),ycaljy(MAX_IF,ANT_MAX)
        real chi,tint,mdata(9)
c
c  Variables for reference pointing.
c
        logical okref
        real refpnt(2,ANT_MAX)
c
        logical antvalid(ANT_MAX)
        integer mcount
        double precision jday0,time,tprev
c
c  Open the RPFITS file.
c
        call RPOpen(in,iostat)
        if(iostat.ne.0)return
c
c  Initialise.
c
        call PokeName(in)
        call AtFlush(mcount,scinit,tcorr,scbuf,xflag,yflag,
     *                                          MAX_IF,ANT_MAX)
c
c  Read the header.
c
        ncard = -1
        jstat = -1
        call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
        if(jstat.ne.0)call bug('w','Error reading 1st RPFITS header')
        if(jstat.ne.0)return
c       if(card(1)(25:30).ne.'RPFITS')
c     *               call bug('f','Input file is not in RPFITS format')
        sctype = 'unknown'
        okref = .false.
        if(ncard.gt.0)call sortcds(ncard,card,sctype,
     *                                  refpnt,ANT_MAX,okref)
c
c  Initialise.
c
        do j=1,ANT_MAX
          do i=1,MAX_IF
            nxyp(i,j) = 0
          enddo
        enddo
c
        kband = .false.
        qband = .false.
        wband = .false.
        cabb = instrument(1:6).eq.'ATCABB'
        if (cabb) call liner('CABB data detected')
        dotsys = dotsys.or..not.cabb
c
c  Initialise flagging information.
c
        nrec  = 0
        fgbad = 0
        fgoffsrc = 0
        fginvant = 0
        fgsysc   = 0
        fgsam    = 0
        fgcal    = 0
c
        utprev   = -1
        utprevsc = -1
        Accum = .false.
        NewScan = .true.
        corrfud=.false.
        Ssrcno = 0
        Ssimno = 0
        scanno = 1
        tprev = 0
c
c  Loop the loop getting data.
c
        jstat = 0
        badbit = .false.
        dowhile(jstat.eq.0)
          call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
          if(jstat.ne.5)badbit = .false.
c
c  Handle header encountered.  Note that the next header
c  read will be the same one we just encountered, not the
c  next one
c
          if(jstat.eq.1)then
            ncard = -1
            jstat = -1
            call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
            if(ncard.gt.0)
     *          call sortcds(ncard,card,sctype,refpnt,ANT_MAX,okref)
            NewScan = .true.
            scanno = scanno + 1
c
c  Flush the number of buffered samples.
c
            do j=1,ANT_MAX
              do i=1,MAX_IF
                nxyp(i,j) = 0
              enddo
            enddo
c
c  Handle end-of-scan
c
          else if(jstat.eq.2)then
            jstat = 0
c
c  Handle FG table encountered (ignore it)
c
          else if(jstat.eq.4)then
            jstat = 0
c
c  Handle some i/o error. First time tolerate it, but if it happens
c  immediately again, skip to the next header.
c
          else if(jstat.eq.5)then
            if(badbit)then
              call bug('w',
     *          'I/O error occurred with jstat=5. Look for next header')
              jstat = -1
              call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
              NewScan = .true.
              scanno = scanno + 1
            else
              badbit = .true.
              jstat = 0
            endif
c
c  Other errors, including EOF.
c
          else if(jstat.ne.0)then
            continue
c
c  Check if we have run out of records of interest. If so, skip to the
c  end of the file and pretend we have hit EOF.
c  Note: commented out because it fails - RPEOF call hangs indefinitely
c  Replaced by code 2 blocks down.
c
c          else if(scanproc.gt.0.and.scanno.gt.scanskip+scanproc)then
c            call RPEOF(jstat)
c            if(jstat.eq.0)jstat = 3
c
c  Handle a SYSCAL record. If it appears to belong to this integration,
c  send it through to the Poke routines right away. Otherwise, end the
c  integration and buffer up the SYSCAL record for later delivery.
c
          else if(baseln.eq.-1)then
            NewTime = abs(sc_ut-utprevsc).gt.0.04
            if(NewScan.or.an_found.or.NewTime)then
              call AtFlush(mcount,scinit,tcorr,
     *                  scbuf,xflag,yflag,MAX_IF,ANT_MAX)
              Accum = .false.
              utprevsc = sc_ut
            endif
            call SetSC(scinit,scbuf,MAX_IF,ANT_MAX,sc_q,sc_if,sc_ant,
     *          sc_cal,if_invert,polflag,unflag,
     *          xyphase,xyamp,xtsys,ytsys,xsamp,ysamp,
     *          xgtp,ygtp,xsdo,ysdo,xcaljy,ycaljy,
     *          chi,tcorr,pntrms,pntmax,
     *          nxyp,xyp,ptag,xya,atag,MAXXYP,xflag,yflag,wband,cabb,
     *          mdata,mcount,qband)
c
c  Data record. Check whether we want to accept it.
c  If OK, and we have a new scan, calculate the new scan info.
c
          else if(ifno.lt.1.or.ifno.gt.n_if.or.srcno.lt.1)then
            fgbad = fgbad + 1
          else
            ok = scanno.gt.scanskip
            if (scanproc.gt.0) ok=ok.and.scanno.le.scanskip+scanproc
            if(ok.and.NewScan)then
              call dayjul(datobs,jday0)
              time = ut / (3600.d0*24.d0) + jday0
c
c  Now we cludge a correlator fix:
c  It can happen that the datobs is written just before the
c  change of a ut day, but the ut is written after the change of
c  ut day. What the correlator should do is write the ut as ut+8640,
c  but as of 10-04-01 it didn't.
c  So, in this case, jday0 should be incremented here.
c  We assume no-one is going to average more than 30 sec...
c
              if ((ut .lt. 30) .and. (86400*(time-tprev).lt.-1)) then
                jday0=jday0+1
                time = ut / (3600.d0*24.d0) + jday0
                corrfud=.true.
                call bug('w',
     *          'Assuming first scan of integration')
                call bug('w',
     *          'started before UT day rollover')
              endif
              call SimMap(if_num,n_if,if_simul,if_chain,ifsel,nsel,
     *            If2Sim,nifs,Sim2If,Sif,MAXSIM)
              call ChkAnt(x,y,z,antvalid,nant,sing)
              call PolMap(nopol,MPOL,n_if,if_nstok,if_cstok,
     *          nstoke,cstoke,pselect)
            endif
c
c  Determine whether to flush the buffers.
c
            simno = If2Sim(ifno)
            NewFreq = simno.gt.0.and.simno.ne.Ssimno
            NewTime = abs(ut-utprev).gt.0.04
            NewSrc = srcno.ne.Ssrcno
            if(Accum.and.(NewScan.or.an_found.or.NewSrc.or.NewFreq.or.
     *                                                  NewTime))then
              call AtFlush(mcount,scinit,tcorr,
     *          scbuf,xflag,yflag,MAX_IF,ANT_MAX)
              Accum = .false.
            endif
c
            i1 = baseln/256
            i2 = mod(baseln,256)
c
            if(ok) ok = (i1.eq.i2.and.doauto).or.
     *                  (i1.ne.i2.and.docross)
            if(ok)then
              ok = ifno.ge.1.and.ifno.le.n_if.and.
     *             min(i1,i2).ge.1.and.max(i1,i2).le.nant.and.
     *             bin.ge.0
              if(.not.ok)fgbad = fgbad + 1
            endif
            if(ok)ok = If2Sim(ifno).gt.0
            if(ok) then
              nspec = nstoke(ifno)
              nrec = nrec + nspec
              if((sctype.eq.'point'.or.sctype.eq.'paddle').and.
     *                  .not.docaldat)then
                flag = 1
                fgcal = fgcal + nspec
              else if(flag.ne.0)then
                fgoffsrc = fgoffsrc + nspec
              else if(.not.(antvalid(i1).and.antvalid(i2)))then
                flag = 1
                fginvant = fginvant + nspec
c NOTE -need options=relax to get past this for cabb data
              else if(.not.((scinit(ifno,i1).and.scinit(ifno,i2)).or.
     *          relax.or.cabb))then
                flag = 1
                fgsysc = fgsysc + nspec
              endif
              if(ok) ok = flag.eq.0.or.unflag
            endif
c
c  If we are going to accept it, see if we need to flush the buffers.
c
            if(ok)then
c
c  Initialise the Poke routines with new info as required.
c
              if(.not.Accum)then
                time = ut / (3600.d0*24.d0) + jday0
                call Poke1st(time,nifs(simno),nant,cabb)
                if(NewScan)call PokeMisc(instrument,rp_observer,
     *                                          version,sctype)
                if(an_found)call PokeAnt(nant,x,y,z,sing)
                if(NewScan.or.NewFreq)then
                  kband = .false.
                  qband = .false.
                  wband = .false.
                  do i=1,nifs(simno)
                    id = Sim2If(i,simno)
                    if(nuser.ge.i)rfreq = 1e9*userfreq(i)
                    kband = kband.or.
     *                     (if_freq(id).gt.13e9.and.if_freq(id).lt.28e9)
                    qband = qband.or.
     *                     (if_freq(id).gt.30e9.and.if_freq(id).lt.50e9)
                    wband = wband.or.if_freq(id).gt.75e9
                    call PokeIF(i,if_nfreq(id),if_invert(id)*if_bw(id),
     *                  if_freq(id),if_ref(id),rfreq,
     *                  nstoke(id),cstoke(1,id),if_chain(id))
                  enddo
                endif
                calcode = su_cal(srcno)
                if(su_name(srcno).eq.'1934-638')calcode = 'c'
                if(NewScan)then
                  call cacalTyp(calcode,time)
                  call PokeInfo(scanno,time)
                endif
                pntra = su_pra(srcno)
                pntdec = su_pdec(srcno)
                if(pntra.eq.0.or.pntdec.eq.0)then
                  pntra = su_ra(srcno)
                  pntdec = su_dec(srcno)
                endif
c
c  For sctype=otfmos the antenna is scanning: use the pointing centre to
c   indicate the scanning direction
c                
                if (sctype.eq.'otfmos'.and.
     *              su_name(srcno)(1:1).ne.'@') then
                  next = srcno + 1
                  prev = srcno - 1
                  if (next.gt.n_su) next = 1
                  if (prev.lt.1) prev = n_su
                  sname = su_name(next)
                  if (sname.ne.' ') call lcase(sname)
                  if (index(sname,'turn').le.0) then
                    pntra = su_pra(next)
                    pntdec= su_pdec(next)
                  else
                    pntra = su_pra(prev)
                    pntdec = su_pdec(prev)
                  endif
                    
                endif
                
                if(abs(pm_ra)+abs(pm_dec).gt.0)then
                  reftime = pm_epoch + 2 400 000.5d0
                  ra0  = 2*DPI/(24d0*3600d0*365.25d0) * pm_ra
                  ra0  = su_ra(srcno)  + ra0* (time-reftime)
                  dec0 = 2*DPI/(360.d0*3600d0*365.25d0) * pm_dec
                  dec0 = su_dec(srcno) + dec0*(time-reftime)
                  call PokeSrc(su_name(srcno),ra0,dec0,0.d0,0.d0,
     *                  pntra,pntdec,calcode)
                else if(NewScan.or.NewSrc)then
c               if(NewScan.or.NewSrc)then
                  call PokeSrc(su_name(srcno),
     *            su_ra(srcno),su_dec(srcno),
     *            su_rad(srcno),su_decd(srcno),pntra,pntdec,calcode)
                endif
              endif
c
c  Flush out the met data and reference pointing if needed.
c
              if(okref)then
                call PokeRef(refpnt,nant)
                okref = .false.
              endif
              if(mcount.gt.0)then
                call PokeMet(mdata,mcount)
                mcount = 0
              endif
c
c  Flush out any buffered SYSCAL records.
c
              if(scbuf(ifno,i1))call PokeSC(i1,Sif(ifno),chi,tcorr,
     *          xtsys(ifno,i1),ytsys(ifno,i1),
     *          xyphase(ifno,i1),xyamp(ifno,i1),
     *          xsamp(1,ifno,i1),ysamp(1,ifno,i1),
     *          xgtp(ifno,i1),ygtp(ifno,i1),
     *          xsdo(ifno,i1),ysdo(ifno,i1),
     *          xcaljy(ifno,i1),ycaljy(ifno,i1),
     *          pntrms(i1),pntmax(i1))
              if(scbuf(ifno,i2))call PokeSC(i2,Sif(ifno),chi,tcorr,
     *          xtsys(ifno,i2),ytsys(ifno,i2),
     *          xyphase(ifno,i2),xyamp(ifno,i2),
     *          xsamp(1,ifno,i2),ysamp(1,ifno,i2),
     *          xgtp(ifno,i2),ygtp(ifno,i2),
     *          xsdo(ifno,i2),ysdo(ifno,i2),
     *          xcaljy(ifno,i2),ycaljy(ifno,i2),
     *          pntrms(i2),pntmax(i2))
c
              scbuf(ifno,i1) = .false.
              scbuf(ifno,i2) = .false.
c
c  Determine the flags for each polarisation based on the sampler
c  statistics if the samplers have been initialised.
c
              call GetFg(nstoke(ifno),cstoke(1,ifno),flag,
     *          xflag(ifno,i1).or.relax.or.cabb,
     *          yflag(ifno,i1).or.relax.or.cabb,
     *          xflag(ifno,i2).or.relax.or.cabb,
     *          yflag(ifno,i2).or.relax.or.cabb,
     *          flags,fgsam)
c
c  Send the data record to the Poke routines.
c
              if(bin.eq.0) bin = 1
c             tint = 0
              tint = intbase
              if(tint.eq.0)tint = intime
              if(tint.eq.0)tint = 15.0
              flipper = .not.kband.or.time.gt.J01Jul04
c
c  xymode sets the way xy phase correction is potentially done.  If
c  negative, then correctionis done on all data.  If positive, then just
c  that antenna is corrected. If 0, then no xy phase correction is
c  applied.  As of 18OCT07, CA02 has valid xyphase at 3mm, so use
c  xymode==2 to indicate this.
c
              xymode = -1
              if(wband)then
                if(time.gt.J18Oct07.and.instrument(1:4).eq.'ATCA')then
                  xymode = 2
                else
                  xymode = 0
                endif
              endif
c
c  It is pretty late to be checking for a buffer overflow, but
c  RPFITSIN's interface does not guard against it at all!  So at least
c  we are checking!
c
              if(if_nfreq(ifno)*if_nstok(ifno).gt.NDATA)
     *          call bug('f','Buffer overflow within rpfitsin')
c
              if(nstoke(ifno).ne.if_nstok(ifno))
     *          call vissy(vis,if_nfreq(ifno),if_nstok(ifno),
     *            pselect(1,ifno))
c
              call PokeData(u,v,w,baseln,Sif(ifno),bin,
     *          vis,if_nfreq(ifno),nstoke(ifno),flags,
     *          tint,if_invert(ifno).lt.0,flipper,xymode)
c
c  Reinitialise things.
c
              if (86400*(time-tprev).lt.-1) then
                if (.not. corrfud) then
                  call bug('w',
     *             'Data are out of time order')
                else
                  corrfud = .false.
                endif
              endif
              tprev = time
              utprev = ut
              Accum = .true.
              Ssrcno = srcno
              Ssimno = simno
              NewScan = .false.
              an_found = .false.
            endif
          endif
        enddo
c
c  Flush out anything remaining.
c
        if(Accum.and.jstat.eq.3)call PokeFlsh
c
c  Give summary about flagging.
c
        if(version.le.' ')version='unknown'
        call liner('RPFITS file version is '//version)
        call PokeStat(nrec,fgbad,fgoffsrc,fginvant,fgsysc,fgsam,fgcal)
c
c  We are done. Close up, and return the error code.
c
        call RPClose(iostat)
        if(iostat.eq.0.and.jstat.ne.3)iostat = jstat
c
        end
c***********************************************************************
        subroutine RPEOF(jstat)
c
        integer jstat
c
c  Skip to the EOF.
c
c-----------------------------------------------------------------------
        integer flag,baseln,bin,ifno,srcno
        real ut,u,v,w,weight
        complex vis
c
        character rperr*32
c
        jstat = 2
        call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
        if(jstat.eq.3)jstat = 0
        if(jstat.ne.0)call bug('w',
     *          'Error while skipping: '//rperr(jstat))
        end
c***********************************************************************
        character*(*) function RPErr(jstat)
c
        integer jstat
c
c  Translate an RPFITSIN jstat value into something a bit more
c  meaningful.
c-----------------------------------------------------------------------
        character itoaf*8
c
        integer NMESS
        parameter(NMESS=7)
        character mess(NMESS)*32
        data mess/'Operation unsuccessful          ',
     *            'Operation successful            ',
     *            'Encountered header while reading',
     *            'Probably OK ... End of scan     ',
     *            'Encountered end-of-file         ',
     *            'Encountered FG table            ',
     *            'Illegal parameter encountered   '/
c
        if(jstat.ge.-1.and.jstat.le.5)then
          rperr = mess(jstat+2)
        else
          rperr = 'RPFITS error: jstat='//itoaf(jstat)
        endif
c
        end
c***********************************************************************
        subroutine RPClose(jstat)
c
        integer jstat
c-----------------------------------------------------------------------
        integer flag,baseln,bin,ifno,srcno
        real ut,u,v,w,weight
        complex vis
c
        character rperr*32
c
        jstat = 1
        call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
        if(jstat.ne.0)call bug('w',
     *          'Error closing file: '//rperr(jstat))
        end
c***********************************************************************
        subroutine RPOpen(in,jstat)
c
        character in*(*)
        integer jstat
c
c  Open the RPFITS file.
c-----------------------------------------------------------------------
        include 'rpfits.inc'
c
        integer flag,baseln,bin,ifno,srcno
        real ut,u,v,w,weight
        complex vis
c
c  External.
c
        character rperr*32
c
        file = in
c
        jstat = -3
        an_found = .false.
        call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *                                          bin,ifno,srcno)
        if(jstat.ne.0) call bug('w',
     *      'Error opening RPFITS file: '//rperr(jstat))
        if(jstat.ne.0)return
        end
c***********************************************************************
        subroutine PolMap(nopol,mpol,nif,nstin,cstin,nstout,cstout,
     *                                                          pselect)
c
        integer nif,mpol
        integer nstin(nif),nstout(nif)
        logical pselect(mpol,nif),nopol
        character cstin(mpol,nif)*(*),cstout(mpol,nif)*(*)
c
c  Weed out non-parallel-hand polarisations from the list of Stokes
c  parameters.
c
c  Input:
c    nif
c    nstin
c    cstin
c  Output:
c    nstout
c    cstout
c    pselect
c-----------------------------------------------------------------------
        integer i,j
        character token*4
c
        if(nopol)then
          do j=1,nif
            nstout(j) = 0
            do i=1,nstin(j)
              token = cstin(i,j)
              call ucase(token)
              if(token.eq.'XX'.or.token.eq.'YY'.or.
     *           token.eq.'RR'.or.token.eq.'LL'.or.token.eq.'I')then
                nstout(j) = nstout(j) + 1
                cstout(nstout(j),j) = cstin(i,j)
                pselect(i,j) = .true.
              else
                pselect(i,j) = .false.
              endif
            enddo
          enddo
        else
          do j=1,nif
            nstout(j) = nstin(j)
            do i=1,nstin(j)
              cstout(i,j) = cstin(i,j)
              pselect(i,j) = .true.
            enddo
          enddo
        endif
c
        end
c***********************************************************************
        subroutine vissy(vis,nchan,nstin,pselect)
c
        integer nchan,nstin
        complex vis(nstin*nchan)
        logical pselect(nstin)
c-----------------------------------------------------------------------
        integer ki,ko,i,j
c
        ki = 0
        ko = 0
        do j=1,nchan
          do i=1,nstin
            ki = ki + 1
            if(pselect(i))then
              ko = ko + 1
              vis(ko) = vis(ki)
            endif
          enddo
        enddo
c
        end
c***********************************************************************
        character*(*) function pcent(frac,total)
c
        integer frac,total
c-----------------------------------------------------------------------
        character val*5
        real x
        x = real(100*frac)/real(total)
        if(x.gt.9.99)then
          write(val,'(f5.1)')x
        else
          write(val,'(f5.2,a)')x
        endif
        pcent = val//'%'
        end
c***********************************************************************
        subroutine ChkAnt(x,y,z,antvalid,nant,sing)
c
        integer nant
        double precision x(nant),y(nant),z(nant)
        logical antvalid(nant),sing
c
c  Check for a valid antenna position.
c-----------------------------------------------------------------------
        integer i
c
        do i=1,nant
          antvalid(i) = (abs(x(i)) + abs(y(i)) + abs(z(i)).gt.0).or.
     *                  sing
        enddo
c
        end
c***********************************************************************
        subroutine GetFg(nstok,cstok,flag,xflag1,yflag1,xflag2,yflag2,
     *          flags,fgsam)
c
        integer nstok,flag,fgsam
        character cstok(nstok)*(*)
        logical flags(nstok),xflag1,yflag1,xflag2,yflag2
c
c  Flag a polarisation either if "flag" indicates that the entire record
c  is bad, or if the syscal-based flags are bad.
c-----------------------------------------------------------------------
        integer p

c

        if(flag.ne.0)then
          do p=1,nstok
            flags(p) = .false.
          enddo
        else
          do p=1,nstok
            if(cstok(p).eq.'XX')then
              flags(p) = xflag1.and.xflag2
            else if(cstok(p).eq.'YY')then
              flags(p) = yflag1.and.yflag2
            else if(cstok(p).eq.'XY')then
              flags(p) = xflag1.and.yflag2
            else if(cstok(p).eq.'YX')then
              flags(p) = yflag1.and.xflag2
            else
              call bug('f','Unrecognised polarisation type, in GetFg')
            endif
            if(.not.flags(p))fgsam = fgsam + 1
          enddo
        endif
c
        end
c***********************************************************************
        subroutine syscflag(polflag,xsamp,ysamp,xyphase,xyamp,
     *          nxyp,maxxyp,xyp,ptag,xya,atag,xflag,yflag,mmrelax,cabb)
c
        real xsamp(3),ysamp(3),xyphase,xyamp
        integer nxyp,maxxyp,ptag(maxxyp),atag(maxxyp)
        real xyp(maxxyp),xya(maxxyp)
        logical polflag,xflag,yflag,mmrelax,cabb
c
c  Determine data flags based on the values of syscal statistics.
c
c  The data will be flagged bad if:
c    * The sampler stats deviate by 3% from 17.3%, or 0.5% from 50.0%
c    * There is a 10 degree change in the xyphase relative to the median
c      of the "nxyp" values.
c    * There is a 1 Jy or 10% change in the xyamp relative to the median
c      of the "nxyp" values.
c
c  Input:
c    xsamp      The x sampler statistics (percent).
c    ysamp      The y sampler statistics (percent).
c    xyphase    The online xyphase measurement (radians).
c    xyamp      XY amplitude, in (pseudo)Jy.
c    maxxyp     The max number of xy phase
c  Input/Output:
c    nxyp       Number of buffered xyphase measurements.
c    tag        Tags for xyphase measurements.  The oldest xyphase
c               measurement has the smallest tag value.
c    xyp        Buffered xyphase measurements. These are always
c               sorted into ascending order. In radians.
c    xya        Buffered xyamp measurements. These are always
c               sorted into ascending order. In (pseudo)Jy.
c
c  Output:
c    xflag      Flag for the X channel.
c    yflag      Flag for the y channel.
c-----------------------------------------------------------------------
        include 'mirconst.h'
        real mxyp,mxya
        integer ntemp
c
        ntemp = nxyp
        call MedMerge(nxyp,maxxyp,xyphase,xyp,ptag,mxyp)
        call MedMerge(ntemp,maxxyp,xyamp,xya,atag,mxya)
c
c  Flag both x and y as bad if there is a glitch in the xy phase.
c  Otherwise flag according to the goodness of the sampler stats.
c

        xflag=.true.
        yflag=.true.
        if((abs(xyamp-mxya).gt.max(1.0,0.1*mxya).or.
     *     abs(xyphase-mxyp).gt.10.*pi/180.).and..not.mmrelax)then
          xflag = .false.
          yflag = .false.
        else if(.not.cabb) then
          xflag = abs(xsamp(2)-50.0).lt.0.5 .and.
     *            abs(xsamp(1)-17.3).lt.3.0 .and.
     *            abs(xsamp(3)-17.3).lt.3.0
          yflag = abs(ysamp(2)-50.0).lt.0.5 .and.
     *            abs(ysamp(1)-17.3).lt.3.0 .and.
     *            abs(ysamp(3)-17.3).lt.3.0
          if(polflag)then
            xflag = xflag.and.yflag
            yflag = xflag
          endif
        endif
c
        end
c***********************************************************************
        subroutine MedMerge(nxyp,maxxyp,xyphase,xyp,tag,mxyp)
c
        integer nxyp,maxxyp,tag(maxxyp)
        real xyphase,xyp(maxxyp),mxyp
c-----------------------------------------------------------------------
        integer tmin,tmax,i,nxyp2
        logical more
c
c  Find the xyphase with the biggest and smallest tags.
c
        if(nxyp.gt.0)then
          tmax = 1
          tmin = 1
          do i=2,nxyp
            if(tag(i).gt.tag(tmax))tmax = i
            if(tag(i).lt.tag(tmin))tmin = i
          enddo
          tmax = tag(tmax) + 1
        else
          tmax = 1
        endif
c
c  If the buffer is full, discard the xyphase with the minimum tag,
c  by squeezing it out.
c
        if(nxyp.eq.maxxyp)then
          do i=tmin+1,nxyp
            xyp(i-1) = xyp(i)
            tag(i-1) = tag(i)
          enddo
        else
          nxyp = nxyp + 1
        endif
c
c  Merge in the new xyphase.
c
        i = nxyp
        more = i.gt.1
        dowhile(more)
          more = xyp(i-1).gt.xyphase
          if(more)then
            xyp(i) = xyp(i-1)
            tag(i) = tag(i-1)
            i = i - 1
            more = i.gt.1
          endif
        enddo
        xyp(i) = xyphase
        tag(i) = tmax
c
c  Determine the median xyphase
c
        nxyp2 = nxyp/2
        if(2*nxyp2.ne.nxyp)then
          mxyp = xyp(nxyp2+1)
        else
          mxyp = 0.5*(xyp(nxyp2)+xyp(nxyp2+1))
        endif
c
        end
c***********************************************************************
        real function getjpk(freq)
c
        real freq
c-----------------------------------------------------------------------
        if(freq.lt.15)then
          getjpk = 13
        else if(freq.lt.30)then
          getjpk = 13
        else
          getjpk = 13
        endif
c
        end
c***********************************************************************
        subroutine SetSC(scinit,scbuf,MAXIF,MAXANT,nq,nif,nant,
     *          syscal,invert,polflag,unflag,
     *          xyphase,xyamp,xtsys,ytsys,xsamp,ysamp,
     *          xgtp,ygtp,xsdo,ysdo,xcaljy,ycaljy,
     *          chi,tcorr,pntrms,pntmax,nxyp,xyp,ptag,xya,atag,MAXXYP,
     *          xflag,yflag,mmrelax,cabb,mdata,mcount,qband)
c
        integer MAXIF,MAXANT,MAXXYP,nq,nif,nant,invert(MAXIF)
        integer tcorr,mcount
        real syscal(nq,nif,nant),mdata(9)
        logical polflag,unflag
        logical scinit(MAXIF,MAXANT),scbuf(MAXIF,MAXANT)
        real xyphase(MAXIF,MAXANT),xyamp(MAXIF,MAXANT)
        real xtsys(MAXIF,MAXANT),ytsys(MAXIF,MAXANT)
        real xsamp(3,MAXIF,MAXANT),ysamp(3,MAXIF,MAXANT)
        real xgtp(MAXIF, MAXANT), ygtp(MAXIF, MAXANT)
        real xsdo(MAXIF, MAXANT), ysdo(MAXIF, MAXANT)
        real xcaljy(MAXIF, MAXANT), ycaljy(MAXIF, MAXANT)
        real pntrms(MAXANT),pntmax(MAXANT)
        real chi
        real xyp(MAXXYP,MAXIF,MAXANT),xya(MAXXYP,MAXIF,MAXANT)
        integer ptag(MAXXYP,MAXIF,MAXANT)
        integer atag(MAXXYP,MAXIF,MAXANT)
        integer nxyp(MAXIF,MAXANT)
        logical xflag(MAXIF,MAXANT),yflag(MAXIF,MAXANT),mmrelax,cabb
        logical qband
c
c  Copy across SYSCAL records. Do any necessary fiddles on the way.
c-----------------------------------------------------------------------
        include 'mirconst.h'
        integer j,k,ij,ik
        logical done,ok
c
        done = .false.
        do k=1,nant
          do j=1,nif
            ik = nint(syscal(1,j,k))
            ij = nint(syscal(2,j,k))
            ok = ij.gt.0.and.ik.gt.0.and.ij.le.maxif.and.ik.le.maxant
            if(ok.and.nq.ge.13) ok = syscal(13,j,k).eq.0.or.unflag
            if(ok)then
              scinit(ij,ik) = .true.
              scbuf(ij,ik)  = .true.
              xyphase(ij,ik) = invert(ij)*syscal(3,j,k)
              xyamp(ij,ik) = 0
              if(nq.ge.14)xyamp(ij,ik) = syscal(14,j,k)
              pntrms(ik) = 0
              if(nq.ge.16)pntrms(ik) = syscal(16,j,k)
              pntmax(ik) = 0
              if(nq.ge.15)pntmax(ik) = syscal(15,j,k)
              if(syscal(4,j,k).gt.0)tcorr = tcorr + 1
              if(syscal(5,j,k).gt.0)tcorr = tcorr + 1
              xtsys(ij,ik) = 0.1* syscal(4,j,k) * syscal(4,j,k)
              ytsys(ij,ik) = 0.1* syscal(5,j,k) * syscal(5,j,k)
              if (.not.cabb) then
                xsamp(1,ij,ik) = syscal(6,j,k)
                xsamp(2,ij,ik) = syscal(7,j,k)
                xsamp(3,ij,ik) = syscal(8,j,k)
                ysamp(1,ij,ik) = syscal(9,j,k)
                ysamp(2,ij,ik) = syscal(10,j,k)
                ysamp(3,ij,ik) = syscal(11,j,k)
              else
                xgtp(ij,ik)    = syscal(6,j,k)
                xsdo(ij,ik)    = syscal(7,j,k)
                xcaljy(ij,ik)  = syscal(8,j,k)
                ygtp(ij,ik)    = syscal(9,j,k)
                ysdo(ij,ik)    = syscal(10,j,k)
                ycaljy(ij,ik)  = syscal(11,j,k)
              endif
              call syscflag(polflag,xsamp(1,ij,ik),ysamp(1,ij,ik),
     *          xyphase(ij,ik),xyamp(ij,ik),nxyp(ij,ik),maxxyp,
     *          xyp(1,ij,ik),ptag(1,ij,ik),xya(1,ij,ik),atag(1,ij,ik),
     *          xflag(ij,ik),yflag(ij,ik),mmrelax,cabb)
              if(.not.done.and.syscal(12,j,k).ne.0)then
                chi = pi/180 * syscal(12,j,k) + pi/4
                if (qband) chi = chi + pi/2
                done = .true.
              endif
c
c  Save met data. The array consists of
c    1 Temperature
c    2 Pressure
c    3 Humidity
c    4 Wind speed
c    5 Wind direction
c    6 weather flag (1=no weather data)
c    7 rain gauge
c    8 seemon phase
c    9 seemon rms
c    10 seemon flag (1=no seemon data)
c    11 Jy per K (IF 1) - not stored at present 
c    12 Jy per K (IF 2) - needs a rethink of jyperk usage
            else if(ik.eq.0)then
              mcount = 7
              mdata(1) = syscal(2,j,k)
              mdata(2) = syscal(3,j,k)
              mdata(3) = syscal(4,j,k)
              mdata(4) = syscal(5,j,k)
              mdata(5) = syscal(6,j,k)
              mdata(6) = syscal(7,j,k)
              mdata(7) = syscal(8,j,k)
              mdata(8) = syscal(9,j,k)
              mdata(9) = syscal(10,j,k)
              if(syscal(11,j,k).eq.0)mcount=9
            endif
          enddo
        enddo
c
        end
c***********************************************************************
        subroutine AtFlush(mcount,scinit,tcorr,scbuf,xflag,yflag,
     *                                                  MAXIF,MAXANT)
c
        integer MAXIF,MAXANT
        integer tcorr,mcount
        logical scinit(MAXIF,MAXANT),scbuf(MAXIF,MAXANT)
        logical xflag(MAXIF,MAXANT),yflag(MAXIF,MAXANT)
c
c-----------------------------------------------------------------------
        integer i,j
c
        mcount = 0
        do j=1,MAXANT
          do i=1,MAXIF
            scinit(i,j) = .false.
            scbuf(i,j)  = .false.
            xflag(i,j)  = .false.
            yflag(i,j)  = .false.
          enddo
        enddo
        tcorr = 0
c
        call PokeFlsh
c
        end
c***********************************************************************
        subroutine SimMap(ifnum,nif,ifsimul,ifchain,ifsel,nsel,
     *            If2Sim,nifs,Sim2If,Sif,MAXSIM)
c
        integer nif,ifnum(nif),ifsimul(nif),ifchain(nif),MAXSIM
        integer nsel,ifsel(nsel)
        integer If2Sim(nif),nifs(nif),Sim2IF(MAXSIM,nif),Sif(nif)
c
c  Using the RPFITS IF table, determine a map between RPFITS "ifno",
c  to a simultaneous group number. Then determine a map between the
c  simultaneous group number and the RPFITS "ifno" number.
c
c  What the &%^$&^%&^ is the RPFITS entry "IF_NUM" used for? Is it
c  an extra level of indirection in the the IF table or what? Avoid
c  attempting to understand this (no one else does). Just make sure
c  that IF_NUM(i).eq.i, which means that IF_NUM must be redundant and
c  irrelevant.
c
c  Input:
c    nif        Total number of entries in the RPFITS IF table.
c    ifnum      RPFITS IF_NUM column. Just check that IF_NUM(i)==i.
c    ifsimul,ifchain RPFITS columns.
c    ifsel      IF axes to select (user specified).
c    MAXSIM     Maximum number of simultaneous frequencies.
c  Output:
c    If2Sim     Map from ifno to "simultaneous group number".
c    Sim2If     Map from "simultaneous group number" to "ifno".  There
c               can be up to MAXSIM entries per "sim. group no.".
c    nifs       Number of simultaneous IFs in each sim. group.
c    Sif        Maps from RPFITS ifno to the position on the Miriad IF
c               axis.
c-----------------------------------------------------------------------
        integer i,j,k,nsimgrp,s
        logical more,ok
c
        do i=1,nif
          if(ifnum(i).ne.i)call bug('f',
     *          'IF_NUM(i).ne.i ... I do not understand')
        enddo
c
c  Assign a simultaneous IF to each of them.
c
        nsimgrp = 0
        do i=1,nif
          If2Sim(i) = 0
c
c  Has this IF been selected?
c          
          ok = ifsel(1).eq.0
          do k=1,nsel
            ok = ok.or.(ifsel(k).eq.i)
          enddo
          if(ok) then
            do j=1,i-1
              if(ifsimul(i).eq.ifsimul(j).and.If2Sim(j).gt.0)
     *          If2Sim(i) = If2Sim(j)
            enddo
            if(If2Sim(i).eq.0)then
              nsimgrp = nsimgrp + 1
              If2Sim(i) = nsimgrp
              nifs(nsimgrp) = 0
            endif
          endif
        enddo
c
c  Map from simultaneous group number to ifno.
c
        do i=1,nif
          s = If2Sim(i)
          if(s.gt.0)then
            nifs(s) = nifs(s) + 1
            Sim2If(nifs(s),s) = i
          endif
        enddo
c
c  Sort the Sim2If index so that the ifno with smaller IF_CHAIN come
c  first.
c
        do i=1,nsimgrp
          more = .true.
          dowhile(more)
            more = .false.
            do j=2,nifs(i)
              if(ifchain(Sim2If(j,i)).lt.ifchain(Sim2If(j-1,i)))then
                s = Sim2If(j,i)
                Sim2If(j,i) = Sim2If(j-1,i)
                Sim2If(j-1,i) = s
                more = .true.
              endif
            enddo
          enddo
        enddo
c
c  Determine the map from the RPFITS ifno variable to the position on
c  the Miriad IF axis.
c
        do i=1,nif
          Sif(i) = 0
        enddo
c
        do i=1,nsimgrp
          do j=1,nifs(i)
            Sif(Sim2If(j,i)) = j
          enddo
        enddo
c
        end
c***********************************************************************
        subroutine cacalIni
c
c-----------------------------------------------------------------------
        integer MAXTIMES
        parameter(MAXTIMES=32)
        integer bcal,scal,ntimes
        double precision stime,times(2,MAXTIMES)
        common/cacalcom/times,stime,ntimes,scal,bcal
c
        scal = -1
        bcal = -1
        stime = 0
        ntimes = 0
        end
c***********************************************************************
        subroutine cacalCnt(count)
c
        integer count
c-----------------------------------------------------------------------
        integer MAXTIMES
        parameter(MAXTIMES=32)
        integer bcal,scal,ntimes
        double precision stime,times(2,MAXTIMES)
        common/cacalcom/times,stime,ntimes,scal,bcal
c
        bcal = count
        end
c***********************************************************************
        subroutine cacalTyp(calcode,time)
c
        character calcode*(*)
        double precision time
c-----------------------------------------------------------------------
        integer MAXTIMES
        parameter(MAXTIMES=32)
        integer bcal,scal,ntimes
        double precision stime,times(2,MAXTIMES)
        common/cacalcom/times,stime,ntimes,scal,bcal
c
        if(bcal.ge.0)then
          if(scal.ge.0..and.bcal.gt.scal.and.
     *       time-stime.gt.0.and.time-stime.le.0.5/24.0)then
            ntimes = ntimes + 1
            if(ntimes.gt.MAXTIMES)
     *          call bug('f','Buffer overflow in cacalTyp')
            times(1,ntimes) = stime
            times(2,ntimes) = time
          endif
          if(index(calcode,'c').ne.0.or.index(calcode,'C').ne.0)then
            scal = bcal
            stime = time
          else
            scal = -1
          endif
          bcal = -1
        endif
c
        end
c***********************************************************************
        subroutine cacalFin(time1,MAXTIME1,ntime1)
c
        integer MAXTIME1,ntime1
        double precision time1(2,MAXTIME1)
c-----------------------------------------------------------------------
        integer MAXTIMES
        parameter(MAXTIMES=32)
        integer bcal,scal,ntimes
        double precision stime,times(2,MAXTIMES)
        common/cacalcom/times,stime,ntimes,scal,bcal
c
        integer i
c
        if(ntimes.gt.MAXTIME1)
     *    call bug('f','Buffer overflow in cacalFin')
        do i=1,ntimes
          time1(1,i) = times(1,i)
          time1(2,i) = times(2,i)
        enddo
        ntime1 = ntimes
c
        end
c***********************************************************************
        subroutine rfiIni(rfiflag)
c
        logical rfiflag
c-----------------------------------------------------------------------
        double precision f1,f2
        character*80 filename,string,stcat
        integer lu,iostat,l
        integer MAXRFI, nrfi
        parameter(MAXRFI=99)
        double precision rfifreq(2,MAXRFI)
        common/rficom/rfifreq,nrfi
        nrfi=0
        if (.not.rfiflag) return
c
c  Read rfiflag.txt file from current directory or $MIRCAT
c
        filename='./rfiflag.txt'
        call txtopen(lu,filename,'old',iostat)
        if (iostat.ne.0) then
          call getenv('MIRCAT',filename)
          filename = stcat(filename,'/rfiflag.txt')
          call txtopen(lu,filename,'old',iostat)
        endif
        if (iostat.ne.0) then
          call bug('w',
     *      'File rfiflag.txt not found in current dir or $MIRCAT')
          nrfi=0
        else
          call txtread(lu,string,l,iostat)
          do while (iostat.eq.0)
            read(string,*,iostat=iostat) f1,f2
            if (iostat.eq.0) then
              if (nrfi.lt.MAXRFI) then
                nrfi=nrfi+1
                rfifreq(1,nrfi)=f1/1000
                rfifreq(2,nrfi)=f2/1000
                call txtread(lu,string,l,iostat)
              else
                call bug('w','Too many freq ranges in rfiflag.txt')
                iostat=1
              endif
            else 
              call txtread(lu,string,l,iostat)
            endif
          enddo
          call txtclose(lu)
        endif
        if (nrfi.gt.0) then
          write(string,'(A,I3,A)') 'Flagging channels in ',nrfi,
     *     ' frequency ranges'
          call output(string)
        endif

        end

c***********************************************************************
        subroutine rfiFlag(flags,NDATA,nifs,nfreq,sfreq,sdf,birdie,
     *                     edge,time)
c
c-----------------------------------------------------------------------
        integer NDATA,nifs,nfreq(nifs)
        logical flags(NDATA),birdie
        double precision sfreq(nifs),sdf(nifs),time
        real edge
c
        double precision c1,c2,tmp,cfreq,J17AUG10
        integer MAXRFI, NBIRDIE1, NBIRDIE2, nrfi,ch1,ch2,i,j,k,offset
        parameter(MAXRFI=99,NBIRDIE1=11,NBIRDIE2=3,J17AUG10=2455425.5)
        double precision rfifreq(2,MAXRFI)
        common/rficom/rfifreq,nrfi
c
c  CABB 1MHz continuum mode birdies (2049*1 MHz)
c        
        integer b1(NBIRDIE1)
        data b1/640,256,768,1408,1280,1920,1792,1176,156,128,1152/
c
c  CABB 64 MHz continuum mode birdies (33*64 MHz)
c        
        integer b2(NBIRDIE2)
        data b2/8,16,24/
c        
        if (nrfi.gt.0) then
          offset=1
          do i=1,nifs
c
c  Flag rfi in continuum bands
c
            if (abs(sdf(i)).gt.0.5e-3) then
              do j=1,nrfi
                c1=(rfifreq(1,j)-sfreq(i))/sdf(i)
                c2=(rfifreq(2,j)-sfreq(i))/sdf(i)
                if (c1.gt.c2) then
                  tmp=c1
                  c1=c2
                  c2=tmp
                endif
                ch1 = min(nfreq(i), nint(max(0.0d0,c1)))
                ch2 = max(-1, min(nfreq(i)-1, nint(c2)))
                do k=ch1,ch2
                  flags(offset+k)=.false.
                enddo
              enddo
            endif
            offset=offset+nfreq(i)
          enddo
        endif
c
c  Flag birdies and edge channels
c        
        if (birdie) then
          offset=1
          do i=1,nifs
            if (nfreq(i).eq.2049.and.
     *          abs(abs(sdf(i))-0.001).lt.1.e-4) then
c          
c             CABB Mode 2048*1MHz
c
              do j=1,NBIRDIE1
                flags(offset+b1(j))=.false.
              enddo
              ch1=2049*edge/200
              ch2=2049*(1.0-edge/200)
c
c             20cm band range 1131-1875, 13cm band range 1975-2675
c             new 16cm band: 1050-3150
c
              cfreq=sfreq(i)+(nfreq(i)/2)*sdf(i)
              if (cfreq.gt.1.d0.and.cfreq.lt.3.0d0) then
                if (time.lt.J17AUG10) then
                  if (cfreq.lt.2.d0) then
                    c1=(1.131-sfreq(i))/sdf(i)
                    c2=(1.875-sfreq(i))/sdf(i)
                  else
                    c1=(1.975-sfreq(i))/sdf(i)
                    c2=(2.675-sfreq(i))/sdf(i)
                  endif
                else
                  c1=(1.05-sfreq(i))/sdf(i)
                  c2=(3.15-sfreq(i))/sdf(i)
                endif
                ch1=max(ch1,min(nint(c1),nint(c2)))
                ch2=min(ch2,max(nint(c1),nint(c2)))
              endif
              do j=0,ch1
                flags(offset+j)=.false.
              enddo
              do j=ch2,nfreq(i)-1
                  flags(offset+j)=.false.
              enddo
            else if (nfreq(i).eq.33.and.
     *          abs(abs(sdf(i))-0.064).lt.1.e-4) then
c          
c             CABB Mode 32*64MHz
c
              do j=1,NBIRDIE2
                flags(offset+b2(j))=.false.
              enddo
              ch1=33*edge/200
              ch2=33*(1.0-edge/200)
     
            else if (nfreq(i).ge.2049.and.
     *          abs(sdf(i)).lt.4.e-5) then
c
c             CABB zoom mode 2049 channels or more (blended zooms)
c  
              ch1=2049*edge/200
              ch2=nfreq(i)-2049*edge/200
              do j=0,ch1
                flags(offset+j)=.false.
              enddo
              do j=ch2,nfreq(i)-1
                  flags(offset+j)=.false.
              enddo                      
            endif
            offset=offset+nfreq(i)
          enddo
        endif
        end

c***********************************************************************
c
c  The following code was contributed by WEW via NEBK.
c
c-----------------------------------------------------------------------
      REAL FUNCTION TWOBIT_GAIN_ADJUST(SSEXP, N1, Z1, P1, N2, Z2, P2)
C-----------------------------------------------------------------------
C
C      Finds gain correction factor to be applied to data whose
C      gain has been calculated on the assumption that the
C      sampler statistics were either set on :
C
C      17.1, 50.0, 17.1 percent, for SSEXP = 17.1
C                  --OR--
C      17.3, 50.0, 17.3 percent, for SSEXP = 17.3
C
C      whereas they were actually n1, z1, p1 and n2, z2, p2 percent
C      on inputs 1,2.  ( n=neg, z=zero, p=pos )
C
C      The data should be multiplied by twobit_gain_adjust
C      to obtain the corrected data.
C
C-----------------------------------------------------------------------

C PARAMETERS
      REAL      N1, Z1, P1, N2, Z2, P2, SSEXP

C EXTERNAL FUNCTIONS
      REAL      GAIN_PARAM, TWOBIT_GAIN_R0

C LOCAL VARIABLES
      REAL      QN1, QZ1, QP1, QN2, QZ2, QP2
      REAL      A

C BEGIN

      QN1 = GAIN_PARAM( N1 )
      QZ1 = GAIN_PARAM( Z1 )
      QP1 = GAIN_PARAM( P1 )

      QN2 = GAIN_PARAM( N2 )
      QZ2 = GAIN_PARAM( Z2 )
      QP2 = GAIN_PARAM( P2 )

C In the following, a is the gain that was applied on line.
C It is calculated from
C  a = average correlator count / digital correlator gain at
C      zero correlation

      IF (SSEXP.GT.17.2) THEN
C          5.444705 = 6.19 / 1.1368844
         A = 5.444705
      ELSE IF (SSEXP.GT.17.0) THEN
C          5.392175663 = 6.13 / 1.1368324
         A = 5.392175663
      END IF

C
      TWOBIT_GAIN_ADJUST = A /
     *     TWOBIT_GAIN_R0( QN1, QZ1, QP1, QN2, QZ2, QP2 )
      END
c***********************************************************************
      REAL FUNCTION GAIN_PARAM( LEVEL_PERCENT )
C-----------------------------------------------------------------------
C
C      Gets "gain parameter" - i.e. parameter useful for calculating
C      gain of 2-bit digital correlator for uncorrelated inputs
C      where one of the sampler statistics is level_percent.
C
C-----------------------------------------------------------------------

C PARAMETERS
      REAL      LEVEL_PERCENT

C EXTERNAL FUNCTIONS
      REAL      GAUSS_LEVEL

C LOCAL VARIABLES
      REAL      X

C BEGIN

C Find level ( RMS = 1.0 ) appropriate to this statistic
      X = GAUSS_LEVEL( LEVEL_PERCENT / 100.0 )

      GAIN_PARAM = EXP( -X * X / 2.0 )

      END
c***********************************************************************
      REAL FUNCTION GAUSS_LEVEL( FRACTION_ABOVE_LEVEL )
C-----------------------------------------------------------------------
C
C      Assuming Gaussian statistics, estimates the level given
C       the probability of being above this level, i.e.
C       the fraction of samples above this level.
C
C      Ref.      Approximation formulae - max. error 4.5E-04
C
C-----------------------------------------------------------------------

C PARAMETERS
      REAL            FRACTION_ABOVE_LEVEL

C CONSTANTS
      REAL            C0, C1, C2, D1, D2, D3

      PARAMETER      ( C0 = 2.515517 )
      PARAMETER      ( C1 = 0.802853 )
      PARAMETER      ( C2 = 0.010328 )
      PARAMETER      ( D1 = 1.432788 )
      PARAMETER      ( D2 = 0.189269 )
      PARAMETER      ( D3 = 0.001308 )

C LOCAL VARIABLES
      REAL            P, T, TT, TTT, A, B
      LOGICAL            INVERT

C BEGIN

C Algorithm works for ( 0.0 < p <= 0.5 )  hence
      IF (FRACTION_ABOVE_LEVEL .GT. 0.5 ) THEN
         P = 1.0 - FRACTION_ABOVE_LEVEL
         INVERT = .TRUE.
      ELSE
         P = FRACTION_ABOVE_LEVEL
         INVERT = .FALSE.
      END IF

      IF( P .LT. 1.0E-10 ) P = 1.0E-10

      T = SQRT( LOG( 1.0 / ( P * P ) ) )
      TT = T * T
      TTT = T * TT

      A = C0 + ( C1 * T ) + ( C2 * TT )
      B = 1.0 + ( D1 * T ) + ( D2 * TT ) + ( D3 * TTT )
      GAUSS_LEVEL = T - ( A / B )

      IF( INVERT ) GAUSS_LEVEL = -GAUSS_LEVEL

      RETURN

      END



c***********************************************************************
      REAL FUNCTION TWOBIT_GAIN_R0( QN1, QZ1, QP1, QN2, QZ2, QP2 )
C-----------------------------------------------------------------------
C
C      Finds gain of digital correlator for uncorrelated inputs 1,2
C      where the "gain paramaters" are ( qn1, qz1, qp1 ) and
C      ( qn2, qz2, qp2 ).  n=neg, z=zero, p=pos
C
C-----------------------------------------------------------------------

C PARAMETERS
      REAL      QN1, QZ1, QP1, QN2, QZ2, QP2

C EXTERNAL FUNCTIONS
      REAL      G_QUAD

C BEGIN

      TWOBIT_GAIN_R0 = 0.159154943 *
     *            ( G_QUAD( QZ1, QP1, QZ2, QP2 ) +
     *                G_QUAD( QZ1, QN1, QZ2, QP2 ) +
     *                  G_QUAD( QZ1, QP1, QZ2, QN2 ) +
     *                    G_QUAD( QZ1, QN1, QZ2, QN2 ) )
      END
c***********************************************************************
      REAL FUNCTION G_QUAD( QZ1, QP1, QZ2, QP2 )
      REAL QZ1, QP1, QZ2, QP2
      G_QUAD = 9 * QP1 * QP2 +
     *         3 * ( QZ1 * QP2 + QP1 * QZ2 ) + QZ1 * QZ2
      END
