*=======================================================================
*
* WCSLIB 4.18 - an implementation of the FITS WCS standard.
* Copyright (C) 1995-2013, Mark Calabretta
*
* This file is part of WCSLIB.
*
* WCSLIB is free software: you can redistribute it and/or modify it
* under the terms of the GNU Lesser General Public License as published
* by the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* WCSLIB is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
* License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with WCSLIB.  If not, see http://www.gnu.org/licenses.
*
* Direct correspondence concerning WCSLIB to mark@calabretta.id.au
*
* Author: Mark Calabretta, Australia Telescope National Facility, CSIRO.
* http://www.atnf.csiro.au/people/Mark.Calabretta
* $Id$
*=======================================================================

      PROGRAM TWCSFIX
*-----------------------------------------------------------------------
*
* TWCSFIX tests the translation routines for non-standard WCS keyvalues,
* the WCSFIX suite, and the spectral coordinate translation routine
* WCSPTR.
*
*-----------------------------------------------------------------------
      DOUBLE PRECISION TOL
      PARAMETER (TOL = 1D-10)

*     CUNITia is set to arcsec below as an additional test.
      DOUBLE PRECISION DEC, RA
      PARAMETER (RA  = 265.62209470900D0 * 3600D0)
      PARAMETER (DEC = -28.98849996030D0 * 3600D0)

*     Number of axes.
      INTEGER   N
      PARAMETER (N = 3)

      INTEGER   I, J, NAXIS
      DOUBLE PRECISION CDELT(N), CRPIX(N), CRVAL(N), PC(N,N), RESTFRQ,
     :          RESTWAV
      CHARACTER CTYPE(N)*72, CUNIT(N)*72, DATEOBS*72

      COMMON /HEADER/ CRPIX, PC, CDELT, CRVAL, RESTFRQ, RESTWAV, NAXIS
      COMMON /HEADCH/ CTYPE, CUNIT, DATEOBS

*     On some systems, such as Sun Sparc, the struct MUST be aligned
*     on a double precision boundary, done here using an equivalence.
*     Failure to do this may result in mysterious "bus errors".
      INCLUDE 'wcs.inc'
      INCLUDE 'wcsfix.inc'
      INTEGER   STAT(WCSFIX_NWCS), STATUS
      CHARACTER CTYPES*8
      INTEGER   WCS(WCSLEN)
      DOUBLE PRECISION DUMMY
      EQUIVALENCE (WCS,DUMMY)

      DATA NAXIS   /N/
      DATA (CRPIX(J), J=1,N)
     :             /90D0,   90D0,   1D0/
      DATA ((PC(I,J),J=1,N),I=1,N)
     :             /1D0, 0D0, 0D0,
     :              0D0, 1D0, 0D0,
     :              0D0, 0D0, 1D0/
      DATA (CDELT(I), I=1,N)
     :             /-1D0, 1D0, 19.68717093222D0/
      DATA (CUNIT(I), I=1,N)
     :             /'ARCSEC', 'ARCSEC', 'KM/SEC'/
      DATA (CTYPE(I), I=1,N)
     :             /'RA---NCP', 'DEC--NCP', 'FELO-HEL'/
      DATA (CRVAL(I), I=1,N)
     :             /RA, DEC, 5569.27104D0/
      DATA RESTFRQ /1.42040575D9/
      DATA RESTWAV /0D0/

*     N.B. non-standard, corresponding to MJD 35884.04861111
      DATA DATEOBS /'1957/02/15 01:10:00'/
*-----------------------------------------------------------------------
      WRITE (*, 10)
 10   FORMAT ('Testing WCSLIB translator for non-standard usage ',
     :        '(twcsfix.f)',/,
     :        '-------------------------------------------------',
     :        '-----------',/)

*     This routine simulates the actions of a FITS header parser.
      STATUS = WCSPUT (WCS, WCS_FLAG, -1, 0, 0)
      CALL PARSER (WCS)

*     Fix non-standard WCS keyvalues.
      STATUS = WCSFIX (7, 0, WCS, STAT)
      WRITE (*, 20) (STAT(I), I=1,WCSFIX_NWCS)
 20   FORMAT ('WCSFIX status returns: (',I2,5(',',I2),')',/)

      IF (STATUS.NE.0) THEN
        WRITE (*, 30) STATUS
 30     FORMAT ('WCSFIX ERROR',I2,'.')
        GO TO 999
      END IF

*     Extract information from the FITS header.
      STATUS = WCSSET (WCS)
      IF (STATUS.NE.0) THEN
        WRITE (*, 40) STATUS
 40     FORMAT (/,'WCSSET ERROR',I2,'.')
      END IF

      CALL FLUSH(6)
      STATUS = WCSPRT (WCS)
      WRITE (*, 50)
 50   FORMAT (/,'------------------------------------',
     :          '------------------------------------')

*     Should now have a 'VOPT-F2W' axis, translate it to frequency.
      CTYPES = 'FREQ-???'
      I = -1
      STATUS = WCSSPTR (WCS, I, CTYPES)
      IF (STATUS.NE.0) THEN
        WRITE (*, 60) STATUS
 60     FORMAT ('WCSPTR ERROR',I2,'.')
        GO TO 999
      END IF

      STATUS = WCSSET (WCS)
      IF (STATUS.NE.0) THEN
        WRITE (*, 70) STATUS
 70     FORMAT ('WCSSET ERROR',I2,'.')
        GO TO 999
      END IF

      CALL FLUSH(6)
      STATUS = WCSPRT (WCS)

      STATUS = WCSFREE (WCS)


 999  CONTINUE
      END

*-----------------------------------------------------------------------
      SUBROUTINE PARSER (WCS)
*-----------------------------------------------------------------------
* In practice a parser would read the FITS header until it encountered
* the NAXIS keyword which must occur near the start, before any of the
* WCS keywords.  It would then use WCSINI to allocate memory for arrays
* in the WCSPRM "data structure" and set default values.
*
* In this simulation the header keyvalues are set in the main program in
* variables passed in COMMON.
*-----------------------------------------------------------------------
*     Number of axes.
      INTEGER   N
      PARAMETER (N = 3)

      INTEGER   I, J, NAXIS, STATUS, WCS(*)
      DOUBLE PRECISION CDELT(N), CRPIX(N), CRVAL(N),
     :          PC(N,N), RESTFRQ, RESTWAV
      CHARACTER CTYPE(N)*72, CUNIT(N)*72, DATEOBS*72

      COMMON /HEADER/ CRPIX, PC, CDELT, CRVAL, RESTFRQ, RESTWAV, NAXIS
      COMMON /HEADCH/ CTYPE, CUNIT, DATEOBS

      INCLUDE 'wcsunits.inc'
      INCLUDE 'wcs.inc'
*-----------------------------------------------------------------------
      STATUS = WCSNPV (2)
      STATUS = WCSINI (NAXIS, WCS)

      DO 20 I = 1, NAXIS
        STATUS = WCSPUT (WCS, WCS_CRPIX, CRPIX(I), I, 0)

        DO 10 J = 1, NAXIS
          STATUS = WCSPUT (WCS, WCS_PC, PC(I,J), I, J)
 10     CONTINUE

        STATUS = WCSPUT (WCS, WCS_CDELT, CDELT(I), I, 0)
        STATUS = WCSPUT (WCS, WCS_CUNIT, CUNIT(I), I, 0)
        STATUS = WCSPUT (WCS, WCS_CTYPE, CTYPE(I), I, 0)
        STATUS = WCSPUT (WCS, WCS_CRVAL, CRVAL(I), I, 0)
 20   CONTINUE

      STATUS = WCSPUT (WCS, WCS_RESTFRQ, RESTFRQ, 0, 0)
      STATUS = WCSPUT (WCS, WCS_RESTWAV, RESTWAV, 0, 0)

      STATUS = WCSPUT (WCS, WCS_NPV, 1, 0, 0)
      STATUS = WCSPUT (WCS, WCS_PV, -1D0, -1, -1)

      STATUS = WCSPUT (WCS, WCS_DATEOBS, DATEOBS, 0, 0)

      RETURN
      END
