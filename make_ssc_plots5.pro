; Add journal command write to a file
; Use in error message mailed to developers
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
; journal, '/home/rumba/cdaweb/tmp/ssc_journal'
; With move to waltz, switch to use tmp2 directory
; journal, '/home/cdaweb/tmp2/ssc_journal'
; RCJ 02/08/2016  Changed for ssc on aster:
; journal, '/bigdisk/sscweb_tmp/ssc_journal'
; RCJ 04/23/2019  Changed for ssc on tara:
; journal, '/rwssd/sscweb_tmp/ssc_journal'
 journal, '/rwssd/sscweb_tmp/ssc_journal_'+pid
ON_ERROR, 1
; s = execute("restore, '/home/rumba/cdaweb/lib/spdflib.new'")
; s = execute("restore, '/home/rumba/cdaweb/lib/spdflib.dat5'")
;TJK 4/27/99 s = execute("restore, '/home/rumba/cdaweb/lib/spdflib51.dat'")
;restore the debug version that contains more of the jhuapl routines
;for now - can change this to spdflib5.dat after spectrograms are up to
;snuff and that lib is made operational
;s = execute("restore, '/home/rumba/cdaweb/lib/spdflib5.dat'")
 s = execute("restore, '/home/cdaweb/lib/spdflib.dat'")
if s ne 1  then journal
if s ne 1  then  exit
print, version()
print, 'from within make_ssc_plots...'
print, 'IDL_PATH is: ',getenv('IDL_PATH')
print, 'IDL_DIR is: ',getenv('IDL_DIR')
print, cdfnames

s = ssc_plot(cdfnames,PID,OUTDIR)

!p.noerase=0
!p.multi=0
!p.background=0
!p.position=0
!p.region=0

;to gif files.
print, 'Status of ssc_plot = ',s
print, 'Make_ssc_plots finished'
 journal
cmd="file_copy, '/rwssd/sscweb_tmp/ssc_journal_" +pid+ "', '/rwssd/sscweb_tmp/ssc_journal', /force, /overwrite"
s=execute(cmd)
exit
