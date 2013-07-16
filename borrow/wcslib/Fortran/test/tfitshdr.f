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

      PROGRAM TFITSHDR
*-----------------------------------------------------------------------
*
* TFITSHDR tests FITSHDR, the FITS parser for image headers, by reading
* a test header and printing the resulting fitskey structs.
*
* Input comes from file 'pih.fits'.
*
* WCSHDR is called first to extract all WCS-related keyrecords from the
* input header before passing it on to FITSHDR.
*
* KEYS, which is meant to hold a pointer, is declared as an array of
* length 2 to accomodate 64-bit machines for which sizeof(void *) =
* 2*sizeof(int).
*-----------------------------------------------------------------------
      LOGICAL   GOTEND
      INTEGER   CTRL, I, IERR, IVAL(8), J, K, KEYNO, KEYS(2), KEYTYP,
     :          KTYP, NC, NKEYRC, NKEYID, NREJECT, NWCS, RELAX, STATUS,
     :          ULEN, WCSP
      DOUBLE PRECISION FVAL(2)
      CHARACTER KEYREC*80, CVAL*72, HEADER*288001, KEYWRD*12, INFILE*9,
     :          TEXT*84

      INCLUDE 'wcshdr.inc'
      INCLUDE 'fitshdr.inc'
      INTEGER KEYIDS(KEYIDLEN,8)

      DATA INFILE /'pih.fits'/
*-----------------------------------------------------------------------
      WRITE (*, 10)
 10   FORMAT ('Testing FITS image header parser (tfitshdr.f)',/,
     :        '---------------------------------------------',/)

*     Open the FITS WCS test header for formatted, direct I/O.
      OPEN (UNIT=1, FILE=INFILE, FORM='FORMATTED', ACCESS='DIRECT',
     :      RECL=80, IOSTAT=IERR)
      IF (IERR.NE.0) THEN
        WRITE (*, 20) IERR, INFILE
 20     FORMAT ('ERROR',I3,' opening ',A)
        GO TO 999
      END IF

*     Read in the FITS header, excluding COMMENT and HISTORY keyrecords.
      K = 1
      NKEYRC = 0
      GOTEND = .FALSE.
      DO 50 J = 0, 100
        DO 40 I = 1, 36
          READ (1, '(A80)', REC=36*J+I, IOSTAT=IERR) KEYREC
          IF (IERR.NE.0) THEN
            WRITE (*, 30) IERR
 30         FORMAT ('ERROR',I3,' reading header.')
            GO TO 999
          END IF

          HEADER(K:) = KEYREC
          K = K + 80
          NKEYRC = NKEYRC + 1

          IF (KEYREC(:8).EQ.'END     ') THEN
*           An END keyrecord was read, read the rest of the block.
            GOTEND = .TRUE.
          END IF
 40     CONTINUE

        IF (GOTEND) GO TO 60
 50   CONTINUE

 60   CLOSE (UNIT=1)

      HEADER(K:K) = CHAR (0)
      WRITE (*, 70) NKEYRC
 70   FORMAT ('Found',I4,' header keyrecords.')


*     Cull all recognized, syntactically valid WCS keyrecords from the
*     header.
      RELAX = WCSHDR_all
      CTRL = -1
*     WCSPIH will allocate memory for NWCS intialized WCSPRM structs.
      IERR = WCSPIH (HEADER, NKEYRC, RELAX, CTRL, NREJECT, NWCS, WCSP)
      IF (IERR.NE.0) THEN
        WRITE (*, 80) IERR
 80     FORMAT ('WCSPIH ERROR',I2,'.')
        GO TO 999
      END IF

*     Number remaining.
      DO 90 I = 1, 288001, 80
        IF (HEADER(I:I).EQ.CHAR(0)) GO TO 100
 90   CONTINUE

 100  NKEYRC = I / 80


*     Specific keywords to be located or culled.
      IERR = KEYIDPUT (KEYIDS, 0, KEYID_NAME, 'SIMPLE  ')
      IERR = KEYIDPUT (KEYIDS, 1, KEYID_NAME, 'BITPIX  ')
      IERR = KEYIDPUT (KEYIDS, 2, KEYID_NAME, 'NAXIS   ')
      IERR = KEYIDPUT (KEYIDS, 3, KEYID_NAME, 'COMMENT ')
      IERR = KEYIDPUT (KEYIDS, 4, KEYID_NAME, 'HISTORY ')
      IERR = KEYIDPUT (KEYIDS, 5, KEYID_NAME, '        ')
      IERR = KEYIDPUT (KEYIDS, 6, KEYID_NAME, 'END     ')
      NKEYID = 7

      IF (NKEYID.GT.0) THEN
        WRITE (*, '(/,A)')
     :    'The following keyrecords will not be listed:'
        DO 120 I = 0, NKEYID-1
          IERR = KEYIDGET (KEYIDS, I, KEYID_NAME, TEXT)
          WRITE (*, 110) TEXT
 110      FORMAT ('  "',A8,'"')
 120    CONTINUE
      END IF


*     Parse the header.
      IERR = FITSHDR (HEADER, NKEYRC, NKEYID, KEYIDS, NREJECT, KEYS)
      IF (IERR.NE.0) THEN
        WRITE (*, 130) IERR
 130    FORMAT ('FITSKEY ERROR',I2)
      END IF

*     Report the results.
      WRITE (*, 140) NKEYRC, NREJECT
 140  FORMAT(/,I3,' header keyrecords parsed by FITSHDR,',I3,
     :       ' rejected:',/)
      DO 200 I = 0, NKEYRC-1
*       Skip syntactically valid keyrecords that were indexed.
        IERR = KEYGET (KEYS, I, KEY_KEYNO, KEYNO, NC)
        IERR = KEYGET (KEYS, I, KEY_STATUS, STATUS, NC)
        IF (KEYNO.LT.0 .AND. STATUS.EQ.0) GO TO 200

*       Basic keyrecord info.
        IERR = KEYGET (KEYS, I, KEY_KEYWORD, KEYWRD, NC)
        IERR = KEYGET (KEYS, I, KEY_TYPE, KEYTYP, NC)
        WRITE (*, '(I4,I5,2X,A,I3,$)') KEYNO, STATUS, KEYWRD(:8),
     :    KEYTYP

*       Format the keyvalue for output.
        KTYP = MOD(ABS(KEYTYP),10)
        IF (KTYP.EQ.1) THEN
*         Logical.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, IVAL, NC)
          IF (IVAL(1).EQ.0) THEN
            TEXT = 'F'
          ELSE
            TEXT = 'T'
          END IF

        ELSE IF (KTYP.EQ.2) THEN
*         32-bit signed integer.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, IVAL, NC)
          WRITE (TEXT, '(I11)') IVAL(1)

        ELSE IF (KTYP.EQ.3) THEN
*         64-bit signed integer.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, IVAL, NC)
          IF (IVAL(3).NE.0) THEN
            WRITE (TEXT, '(SP,I11,SS,2I9.9)') IVAL(3), ABS(IVAL(2)),
     :             ABS(IVAL(1))
          ELSE
            WRITE (TEXT, '(SP,I11,SS,I9.9)') IVAL(2), ABS(IVAL(1))
          END IF

        ELSE IF (KTYP.EQ.4) THEN
*         Very long integer.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, IVAL, NC)
          K = 0
          DO 150 J = 8, 2, -1
            IF (IVAL(J).NE.0) THEN
              K = J
              GO TO 160
            END IF
 150      CONTINUE

 160      WRITE (TEXT, '(SP,I11)') IVAL(K)
          NC = 12
          DO 170 J = K-1, 1, -1
            WRITE (TEXT(NC:), '(I9.9)') ABS(IVAL(J))
            NC = NC + 9
 170      CONTINUE

        ELSE IF (KTYP.EQ.5) THEN
*         Float.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, FVAL, NC)
          WRITE (TEXT, '(SP,1PE13.6)') FVAL(1)

        ELSE IF (KTYP.EQ.6) THEN
*         Int complex.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, FVAL, NC)
          WRITE (TEXT, '(2I11)') NINT(FVAL(1)), NINT(FVAL(2))

        ELSE IF (KTYP.EQ.7) THEN
*         Float complex.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, FVAL, NC)
          WRITE (TEXT, '(SP,1P,E13.6,2X,E13.6)') FVAL

        ELSE IF (KTYP.EQ.8) THEN
*         String.
          IERR = KEYGET (KEYS, I, KEY_KEYVALUE, CVAL, NC)
          TEXT = '"' // CVAL(:NC) // '"'

        ELSE
*         No value.
          TEXT = ''
        END IF

*       Account for Fortran's abysmal formatting control.
        IF (ABS(KEYTYP).EQ.2 .OR.
     :      ABS(KEYTYP).EQ.3 .OR.
     :      ABS(KEYTYP).EQ.4 .OR.
     :      ABS(KEYTYP).EQ.6) THEN
*         Squeeze out leading blanks.
          DO 180 J = 1, 84
            IF (TEXT(J:J).NE.' ') THEN
              TEXT = TEXT(J:)
              GO TO 190
            END IF
 180      CONTINUE
        END IF

 190    NC = LNBLNK(TEXT)
        IF (KEYTYP.GT.0) THEN
*         Keyvalue successfully extracted.
          WRITE (*, '(2X,A,$)') TEXT(:NC)
        ELSE IF (KEYTYP.LT.0) THEN
*         Syntax error of some type while extracting the keyvalue.
          WRITE (*, '(2X,A,$)') '(' // TEXT(:NC) // ')'
        END IF

*       Units?
        IERR = KEYGET (KEYS, I, KEY_ULEN, ULEN, NC)
        IERR = KEYGET (KEYS, I, KEY_COMMENT, TEXT, NC)
        IF (ULEN.GT.0) THEN
          WRITE (*, '(X,A,$)') TEXT(2:ULEN-2)
        END IF

*       Comment text or reject keyrecord.
        WRITE (*, '(/,A)') TEXT(:NC)
 200  CONTINUE


*     Print indexes.
      WRITE (*, '(//,A)') 'Indexes of selected keywords:'
      DO 210 I = 0, NKEYID-1
        IERR = KEYIDGET (KEYIDS, I, KEYID_NAME, TEXT)
        IERR = KEYIDGET (KEYIDS, I, KEYID_COUNT, NC)
        IERR = KEYIDGET (KEYIDS, I, KEYID_IDX, IVAL)
        WRITE (*, '(A8,3I5,$)') TEXT, NC, IVAL(1), IVAL(2)

*       Print logical (SIMPLE) and integer (BITPIX, NAXIS) values.
        IF (NC.GT.0) THEN
          IERR = KEYGET (KEYS, IVAL(1), KEY_TYPE, KEYTYP, NC)
          WRITE (*, '(I4,$)') KEYTYP

          IF (KEYTYP.EQ.1) THEN
            IERR = KEYGET (KEYS, I, KEY_KEYVALUE, IVAL, NC)
            IF (IVAL(1).EQ.0) THEN
              WRITE (*, '(4X,A,$)') 'F'
            ELSE
              WRITE (*, '(4X,A,$)') 'T'
            END IF
          ELSE IF (KEYTYP.EQ.2) THEN
            IERR = KEYGET (KEYS, I, KEY_KEYVALUE, IVAL, NC)
            WRITE (*, '(I5,$)') IVAL(1)
          END IF
        END IF
        WRITE (*, '()')
 210  CONTINUE

      IERR = FREEKEYS(KEYS)

 999  CONTINUE
      END
