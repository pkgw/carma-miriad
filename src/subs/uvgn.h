c
c Variables to handle normal gains and delays associated with a file:
c
c  t1,t2	Pointers into the solution table.
c  nsols	Number of gain solutions in the gains file.
c  ngains	Total number of gains = (ntau + nfeeds) * nants
c  nfeeds	Number of feeds in each gain solution.
c  nants	Number of antennae in each gain solution.
c  ntau		Number of delay terms.
c  gitem	The item of the gains file.
c  solno	Gives the solution number of the solutions in memory.
c  timetab	Gives the time of the gain solutions in memory.
c  gains	The portion of the gain solutions in memory.
c  gflag	Flags of whether the corresponding gain is good.
c  nbpsols      Number of bandpass solutions in the bandpass file
c  bpsolno      Gives the solution number of the bp solutions in memory.
c  bptimes      Gives the time of the bandpass solutions in memory.
c------------------------------------------------------------------------
c  MCHW's baseline/channel number based bandpass correction.
c
c  docgains	Apply channel gains to channel data.
c  dowgains	Apply wideband gains to wideband data.
c  nwbase	The number of baselines for complex wideband gains.
c  ncbase	The number of baselines for complex channel gains.
c  nwgains	The number of complex wideband gains.
c  ncgains	The number of complex channel gains.
c  wgains	The complex wideband gains.
c  cgains	The complex channel gains.
c------------------------------------------------------------------------
c  RJS's antenna/frequency based bandpass correction.
c
c  vwide	UV variable handle for wfreq,wwidth.
c  vline	UV variable handle for sfreq,sdf,nschan.
c  dopass
c  aver
c  
c
	include 'maxdim.h'
	integer MAXTAB,MAXFEEDS,MAXGAINS,MAXSPECT,MAXSOLN
	parameter(MAXTAB=8096,MAXFEEDS=2,MAXSPECT=MAXWIN)
	parameter(MAXGAINS=3*MAXANT, MAXSOLN=1024)
	integer t1,t2,nsols,nants,nfeeds,ntau,ngains,gitem,solno(MAXTAB)
        integer nbpsols,bpsolno,nfbin
        integer b(2,MAXCHAN)
        real fac(MAXCHAN)
	double precision timetab(0:MAXTAB),dtime,bptimes(MAXSOLN)
        double precision freq(MAXFBIN)
	complex gains(MAXGAINS,MAXTAB,0:MAXFBIN)
	logical gflag(MAXGAINS,MAXTAB,0:MAXFBIN),dogains,dotau
c
	integer ncgains,ncbase,nwgains,nwbase
	logical docgains,dowgains
	ptrdiff pCgains,pWgains
c
	logical dopass,aver,first
	integer tno,vwide,vline,nchan,nspect,nschan(MAXSPECT)
	double precision sfreq(MAXSPECT),sdf(MAXSPECT),freq0
	ptrdiff pFlags(2),pDat(2),pTab,pFreq(2)
        integer nDat(2),nTab,nFreq(2)
c
c
c  The common blocks.
c
	common/UvGnA/timetab,dtime,bptimes,gains,t1,t2,nsols,nants,
     *	  nfeeds,ngains,ntau,nbpsols,gitem,solno,bpsolno,
     *    gflag,dogains,dotau,freq,nfbin,b,fac
c
	common/UvGnB/ncgains,nwgains,ncbase,nwbase,pCgains,pWgains,
     *		docgains,dowgains
c
	common/uvGnC/sfreq,sdf,freq0,pFlags,pDat,pTab,pFreq,
     *    nDat,nTab,nFreq,tno,vwide,vline,nchan,nspect,
     *	  nschan,dopass,aver,first

