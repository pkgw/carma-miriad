/*============================================================================

  WCSLIB 4.18 - an implementation of the FITS WCS standard.
  Copyright (C) 1995-2013, Mark Calabretta

  This file is part of WCSLIB.

  WCSLIB is free software: you can redistribute it and/or modify it under the
  terms of the GNU Lesser General Public License as published by the Free
  Software Foundation, either version 3 of the License, or (at your option)
  any later version.

  WCSLIB is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
  more details.

  You should have received a copy of the GNU Lesser General Public License
  along with WCSLIB.  If not, see http://www.gnu.org/licenses.

  Direct correspondence concerning WCSLIB to mark@calabretta.id.au

  Author: Mark Calabretta, Australia Telescope National Facility, CSIRO.
  http://www.atnf.csiro.au/people/Mark.Calabretta
  $Id$
*===========================================================================*/

#include <stdio.h>
#include <string.h>

#include <wcserr.h>
#include <wcsutil.h>
#include <spc.h>

/* Fortran name mangling. */
#include <wcsconfig_f77.h>
#define spcini_  F77_FUNC(spcini,  SPCINI)
#define spcput_  F77_FUNC(spcput,  SPCPUT)
#define spcget_  F77_FUNC(spcget,  SPCGET)
#define spcfree_ F77_FUNC(spcfree, SPCFREE)
#define spcprt_  F77_FUNC(spcprt,  SPCPRT)
#define spcset_  F77_FUNC(spcset,  SPCSET)
#define spcx2s_  F77_FUNC(spcx2s,  SPCX2S)
#define spcs2x_  F77_FUNC(spcs2x,  SPCS2X)
#define spctype_ F77_FUNC(spctype, SPCTYPE)
#define spcspxe_ F77_FUNC(spcspxe, SPCSPXE)
#define spcxpse_ F77_FUNC(spcxpse, SPCXPSE)
#define spctrne_ F77_FUNC(spctrne, SPCTRNE)
#define spcaips_ F77_FUNC(spcaips, SPCAIPS)

#define spcptc_  F77_FUNC(spcptc,  SPCPTC)
#define spcptd_  F77_FUNC(spcptd,  SPCPTD)
#define spcpti_  F77_FUNC(spcpti,  SPCPTI)
#define spcgtc_  F77_FUNC(spcgtc,  SPCGTC)
#define spcgtd_  F77_FUNC(spcgtd,  SPCGTD)
#define spcgti_  F77_FUNC(spcgti,  SPCGTI)

/* Deprecated. */
#define spctyp_  F77_FUNC(spctyp,  SPCTYP)
#define spcspx_  F77_FUNC(spcspx,  SPCSPX)
#define spcxps_  F77_FUNC(spcxps,  SPCXPS)
#define spctrn_  F77_FUNC(spctrn,  SPCTRN)

#define SPC_FLAG    100
#define SPC_TYPE    101
#define SPC_CODE    102
#define SPC_CRVAL   103
#define SPC_RESTFRQ 104
#define SPC_RESTWAV 105
#define SPC_PV      106

#define SPC_W       200
#define SPC_ISGRISM 201
#define SPC_ERR     202

/*--------------------------------------------------------------------------*/

int spcini_(int *spc)

{
  return spcini((struct spcprm *)spc);
}

/*--------------------------------------------------------------------------*/

int spcput_(int *spc, const int *what, const void *value, const int *m)

{
  const char *cvalp;
  const int  *ivalp;
  const double *dvalp;
  struct spcprm *spcp;

  /* Cast pointers. */
  spcp  = (struct spcprm *)spc;
  cvalp = (const char *)value;
  ivalp = (const int *)value;
  dvalp = (const double *)value;

  spcp->flag = 0;

  switch (*what) {
  case SPC_FLAG:
    spcp->flag = *ivalp;
    break;
  case SPC_TYPE:
    strncpy(spcp->type, cvalp, 4);
    spcp->type[4] = '\0';
    break;
  case SPC_CODE:
    strncpy(spcp->code, cvalp, 3);
    spcp->code[3] = '\0';
    break;
  case SPC_CRVAL:
    spcp->crval = *dvalp;
    break;
  case SPC_RESTFRQ:
    spcp->restfrq = *dvalp;
    break;
  case SPC_RESTWAV:
    spcp->restwav = *dvalp;
    break;
  case SPC_PV:
    spcp->pv[*m] = *dvalp;
    break;
  default:
    return 1;
  }

  return 0;
}

int spcptc_(int *spc, const int *what, const char *value, const int *m)
{
  return spcput_(spc, what, value, m);
}

int spcptd_(int *spc, const int *what, const double *value, const int *m)
{
  return spcput_(spc, what, value, m);
}

int spcpti_(int *spc, const int *what, const int *value, const int *m)
{
  return spcput_(spc, what, value, m);
}

/*--------------------------------------------------------------------------*/

int spcget_(const int *spc, const int *what, void *value)

{
  int  k, m;
  char *cvalp;
  int  *ivalp;
  double *dvalp;
  const int *ispcp;
  const struct spcprm *spcp;

  /* Cast pointers. */
  spcp  = (const struct spcprm *)spc;
  cvalp = (char *)value;
  ivalp = (int *)value;
  dvalp = (double *)value;

  switch (*what) {
  case SPC_FLAG:
    *ivalp = spcp->flag;
    break;
  case SPC_TYPE:
    strncpy(cvalp, spcp->type, 4);
    break;
  case SPC_CODE:
    strncpy(cvalp, spcp->code, 3);
    break;
  case SPC_CRVAL:
    *dvalp = spcp->crval;
    break;
  case SPC_RESTFRQ:
    *dvalp = spcp->restfrq;
    break;
  case SPC_RESTWAV:
    *dvalp = spcp->restwav;
    break;
  case SPC_PV:
    for (m = 0; m < 7; m++) {
      *(dvalp++) = spcp->pv[m];
    }
    break;
  case SPC_W:
    for (m = 0; m < 6; m++) {
      *(dvalp++) = spcp->w[m];
    }
    break;
  case SPC_ISGRISM:
    *ivalp = spcp->isGrism;
    break;
  case SPC_ERR:
    /* Copy the contents of the wcserr struct. */
    if (spcp->err) {
      ispcp = (int *)(spcp->err);
      for (k = 0; k < ERRLEN; k++) {
        *(ivalp++) = *(ispcp++);
      }
    } else {
      for (k = 0; k < ERRLEN; k++) {
        *(ivalp++) = 0;
      }
    }
    break;
  default:
    return 1;
  }

  return 0;
}

int spcgtc_(const int *spc, const int *what, char *value)
{
  return spcget_(spc, what, value);
}

int spcgtd_(const int *spc, const int *what, double *value)
{
  return spcget_(spc, what, value);
}

int spcgti_(const int *spc, const int *what, int *value)
{
  return spcget_(spc, what, value);
}

/*--------------------------------------------------------------------------*/

int spcfree_(int *spc)

{
  return spcfree((struct spcprm *)spc);
}

/*--------------------------------------------------------------------------*/

int spcprt_(int *spc)

{
  /* This may or may not force the Fortran I/O buffers to be flushed.  If
   * not, try CALL FLUSH(6) before calling SPCPRT in the Fortran code. */
  fflush(NULL);

  return spcprt((struct spcprm *)spc);
}

/*--------------------------------------------------------------------------*/

int spcset_(int *spc)

{
  return spcset((struct spcprm *)spc);
}

/*--------------------------------------------------------------------------*/

int spcx2s_(
  int *spc,
  const int *nx,
  const int *sspec,
  const int *sx,
  const double x[],
  double spec[],
  int stat[])

{
  return spcx2s((struct spcprm *)spc, *nx, *sx, *sspec, x, spec, stat);
}

/*--------------------------------------------------------------------------*/

int spcs2x_(
  int *spc,
  const int *nspec,
  const int *sspec,
  const int *sx,
  const double spec[],
  double x[],
  int stat[])

{
  return spcs2x((struct spcprm *)spc, *nspec, *sspec, *sx, spec, x, stat);
}

/*--------------------------------------------------------------------------*/

int spctype_(
  const char ctypei[8],
  char stype[4],
  char scode[3],
  char sname[21],
  char units[7],
  char ptype[1],
  char xtype[1],
  int *restreq,
  iptr err)

{
  char ctypei_[9], scode_[4], sname_[22], stype_[5], units_[8];
  int status;

  strncpy(ctypei_, ctypei, 8);
  ctypei_[8] = '\0';

  status = spctype(ctypei_, stype_, scode_, sname_, units_, ptype, xtype,
                   restreq, (struct wcserr **)err);

  wcsutil_blank_fill( 5, stype_);
  wcsutil_blank_fill( 4, scode_);
  wcsutil_blank_fill(22, sname_);
  wcsutil_blank_fill( 8, units_);

  strncpy(stype, stype_, 4);
  strncpy(scode, scode_, 3);
  strncpy(sname, sname_, 21);
  strncpy(units, units_, 7);

  return status;
}

/* : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : :  */

int spctyp_(
  const char ctypei[8],
  char stype[4],
  char scode[3],
  char sname[21],
  char units[7],
  char ptype[1],
  char xtype[1],
  int *restreq)

{
  return spctype_(ctypei, stype, scode, sname, units, ptype, xtype, restreq,
                  0x0);
}

/*--------------------------------------------------------------------------*/

int spcspxe_(
  const char ctypeS[8],
  const double *crvalS,
  const double *restfrq,
  const double *restwav,
  char ptype[1],
  char xtype[1],
  int *restreq,
  double *crvalX,
  double *dXdS,
  iptr err)

{
  char ctypeS_[9];
  strncpy(ctypeS_, ctypeS, 8);
  ctypeS_[8] = '\0';

  return spcspxe(ctypeS_, *crvalS, *restfrq, *restwav, ptype, xtype, restreq,
                 crvalX, dXdS, (struct wcserr **)err);
}

/* : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : :  */

int spcspx_(
  const char ctypeS[8],
  const double *crvalS,
  const double *restfrq,
  const double *restwav,
  char ptype[1],
  char xtype[1],
  int *restreq,
  double *crvalX,
  double *dXdS)

{
  return spcspxe_(ctypeS, crvalS, restfrq, restwav, ptype, xtype, restreq,
                  crvalX, dXdS, 0x0);
}

/*--------------------------------------------------------------------------*/

int spcxpse_(
  const char ctypeS[8],
  const double *crvalX,
  const double *restfrq,
  const double *restwav,
  char ptype[1],
  char xtype[1],
  int *restreq,
  double *crvalS,
  double *dSdX,
  iptr err)

{
  char ctypeS_[9];
  strncpy(ctypeS_, ctypeS, 8);
  ctypeS_[8] = '\0';

  return spcxpse(ctypeS_, *crvalX, *restfrq, *restwav, ptype, xtype, restreq,
                 crvalS, dSdX, (struct wcserr **)err);
}

/* : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : :  */

int spcxps_(
  const char ctypeS[8],
  const double *crvalX,
  const double *restfrq,
  const double *restwav,
  char ptype[1],
  char xtype[1],
  int *restreq,
  double *crvalS,
  double *dSdX)

{
  return spcxpse_(ctypeS, crvalX, restfrq, restwav, ptype, xtype, restreq,
                  crvalS, dSdX, 0x0);
}

/*--------------------------------------------------------------------------*/

int spctrne_(
  const char ctypeS1[8],
  const double *crvalS1,
  const double *cdeltS1,
  const double *restfrq,
  const double *restwav,
  char   ctypeS2[8],
  double *crvalS2,
  double *cdeltS2,
  iptr err)

{
  int status;
  char ctypeS1_[9], ctypeS2_[9];

  strncpy(ctypeS1_, ctypeS1, 8);
  strncpy(ctypeS2_, ctypeS2, 8);
  ctypeS1_[8] = '\0';
  ctypeS2_[8] = '\0';

  status = spctrne(ctypeS1_, *crvalS1, *cdeltS1, *restfrq, *restwav,
                   ctypeS2_,  crvalS2,  cdeltS2, (struct wcserr **)err);

  strncpy(ctypeS2, ctypeS2_, 8);
  wcsutil_blank_fill(8, ctypeS2);

  return status;
}

/* : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : :  */

int spctrn_(
  const char ctypeS1[8],
  const double *crvalS1,
  const double *cdeltS1,
  const double *restfrq,
  const double *restwav,
  char   ctypeS2[8],
  double *crvalS2,
  double *cdeltS2)

{
  return spctrne_(ctypeS1, crvalS1, cdeltS1, restfrq, restwav, ctypeS2,
                  crvalS2, cdeltS2, 0x0);
}

/*--------------------------------------------------------------------------*/

int spcaips_(
  const char ctypeA[8],
  int *velref,
  char ctype[8],
  char specsys[8])

{
  int status;
  char ctypeA_[9], ctype_[9], specsys_[9];

  strncpy(ctypeA_, ctypeA, 8);
  ctypeA_[8] = '\0';

  status = spcaips(ctypeA_, *velref, ctype_, specsys_);

  wcsutil_blank_fill(9, ctype_);
  strncpy(ctype, ctype_, 8);

  wcsutil_blank_fill(9, specsys_);
  strncpy(specsys, specsys_, 8);

  return status;
}
