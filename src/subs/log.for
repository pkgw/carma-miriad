c************************************************************************
c  LogOpen, LogWrite, LogClose:
c  A series of routines to simplify writting to either a log file or
c  the users terminal.
c
c  History:
c    rjs  16oct89  Original version.
c    pjt  11dec89  Experimental '/printer' version (see also log.h)
c    bpw  25may90  Add 'Q' flag and LogWrit
c    mjs  10mar91  "system" call becomes "ishell" call on Cray
c    rjs  10jan96  Changes to appease g77 on linux.
c    rjs  03feb97  General improvements.
c    pjt  26apr98  check if logopen was ever called
c    pjt   1may98  more standard FORTRAN, using LogNOpen (courtesy RJS)
c    rjs   7may98  Correction to change in logclose.
c    pjt  25jun98  standard decl order error (g77)
c    rjs  10feb02  Added loginput routine.
c    rjs  04sep12  Handle flag for output to STDOUT correctly
c************************************************************************
c* LogOpen -- Initialise the log file routines.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogOpen(name,flags)
c
	implicit none
	character name*(*),flags*(*)
c
c  This initialises the log file routines, and opens an output log file.
c
c  Inputs:
c    name	Name of the file to open. If this is blank, then output
c		is directed to the users terminal. If the name is '/printer'
c               at closing (LogClose) the file is sent to printer
c    flags	A parameter giving additional options to the routine. This
c		consists of a character string, with each character signifying
c		some particular option. Possible values are:
c		 ' '    Just write every to output device on every call
c			to logwrite.
c		 'q'	Query. If the output is a terminal, after every
c			22 lines, the user is queried if he/she wants to
c			continue. See also documentation of LogWrite and
c			LogWrit.			
c--
c------------------------------------------------------------------------
	include 'log.h'
	integer iostat,nopen
c
c  Externals.
c
	integer lognopen
c
	nopen = lognopen(1)
	if(nopen.ne.1) call bug('f','LogOpen: had been opened before')
	nlines = 0
        printer = .false.
	dostdout = .false.
        query = .false.
	domore = .true.
        if(name.eq.'/printer') then
          printer = .true. 
          call filedel('printer',iostat)
	  call txtopen(lu,'printer','new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	else if(name.ne.' ')then
	  call txtopen(lu,name,'new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	else
	  dostdout = .true.
	  query  = index(flags,'q').ne.0
	endif
	end
c************************************************************************
c* LogWrit -- Write a line to the log file.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogWrit(line)
c
	character line*(*)
c
c  THIS ROUTINE IS DEPRECATED
c
c  This writes a line to the log file or the users terminal.
c  If LogOpen was called with option 'q', LogWrit stops the
c  task if the user specifies 'quit' as answer.
c  See also LogWrite.
c
c  Input:
c    line	Line to write.
c--
c------------------------------------------------------------------------
	include 'log.h'
	logical more
	call logwrite(line,more)
	end
c************************************************************************
c* LogWrite -- Write a line to the log file.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogWrite(line,more)
c
	implicit none
	character line*(*)
	logical more
c
c  This writes a line to the log file or the users terminal.
c  If LogOpen was called with option 'q' then LogWrite will
c  return a .FALSE. value in more when the user specifies 'quit'.
c  The applications program then has to take care of stopping.
c
c  Input:
c    line	Line to write.
c  Output:
c    more	Set to false if the user has had enough.
c--
c------------------------------------------------------------------------
	include 'log.h'
	character ans*1
	integer length,iostat,nopen
c
c  Externals.
c
	integer lognopen
c
	nopen = lognopen(0)
	if(nopen.ne.1)call bug('f','LogWrite: LogOpen never called')
	if(domore)then
	  if(.not.dostdout)then
	    call txtwrite(lu,line,len(line),iostat)
	    if(iostat.ne.0) call bugno('f',iostat)
	  else
	    if(query.and.nlines.eq.maxlines)then
	      call prompt(ans,length,
     *		'Hit RETURN to continue, q to quit: ')
	      nlines = 0
	      domore = length.eq.0.or.(ans.ne.'q'.and.ans.ne.'Q')
	    endif
	    nlines = nlines + 1
	    if(domore) call output(line)
	  endif
	endif
	more = domore
	end
c************************************************************************
c* LogClose -- Finish up with the log file.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogClose
c
	implicit none
c
c  This completes access to the log file or terminal, and closes is it up.
c  In case the logfile was '/printer', it sends that file to a printer
c--
c------------------------------------------------------------------------
	include 'log.h'
	integer nopen
c
	character lpr
#ifdef vms
	parameter(lpr='print')
#else
	parameter(lpr='lpr')
#endif
c
c  Externals.
c
	integer lognopen
c
	nopen = lognopen(-1)
	if(nopen.ne.0)call bug('f','LogClose: LogOpen never called')
	if(.not.dostdout)call txtclose(lu)
	if(printer)then
	  call output('Queuing output to printer')
	  call command(lpr//' printer')
	endif
c
	end
c************************************************************************
c* LogNOpen -- Helper function for logging usage in LogOpen
c& rjs
c: text-i/o,log-file
c+
        integer function lognopen(val)
c
	implicit none
        integer val
c
c   This functions aids the logging usage of LogOpen/LogWrite/LogClose,
c   since variables cannot appear in both a DATA and COMMON.

c--
c------------------------------------------------------------------------
        integer nopen
        save nopen
        data nopen/0/
c
        nopen = nopen + val
        lognopen = nopen
        end
c************************************************************************
	subroutine loginput(name)
c
	implicit none
	character name*(*)
c
c  Log the inputs used in a log file. Comments are preceded by the # character.
c
c------------------------------------------------------------------------
	integer narg,i,length,lu,iostat
	logical dofile,more
	character line*256,file*256
	double precision julian
c
c  Externals.
c
	integer iargc,len1
c
	call logWrite('#',more)
	narg = iargc()
	if(name.ne.' ')then
	  line = '# Output generated by task '//name
	  call logwrite(line,more)
	  call TodayJul(julian)
	  call JulDay(julian, 'T', file)
	  line = '# Executed on: '//file(1:len1(file))
	  call logWrite(line,more)
	endif
	line = '# Command line inputs follow:'
	call logWrite(line,more)
c
	line(1:6) = '#'
	dofile = .false.
	do i=1,narg
	  if(dofile)then
	    call getarg(i,file)
	    call txtopen(lu,file,'old',iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error opening input parameter file')
	      call bugno('w',iostat)
	    else
	      call txtread(lu,line(7:),length,iostat)
	      dowhile(iostat.eq.0)
		length = min(len(line),length + 6)
		call logWrite(line(1:length),more)
		call txtread(lu,line(7:),length,iostat)
	      enddo
	      call txtclose(lu)
	    endif
	    dofile = .false.
	  else
	    call getarg(i,line(7:))
	    if(line(7:).eq.'-f')then
	      dofile = .true.
	    else
	      call logWrite(line,more)
	    endif
	  endif
	enddo
	call logWrite('#',more)
	end
