; Add journal command write to a file
; Use in error message mailed to developers
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
; journal, '/home/cdaweb/tmp/ssc_journal_dev'
; journal, '/bigdisk/sscweb_tmp/ssc_journal_dev'
; journal, '/rwssd/sscweb_tmp/ssc_journal_dev'
journal, '/rwssd/sscweb_tmp/ssc_journal_dev_'+pid
ON_ERROR, 1
; s = execute("restore, '/home/rumba/cdaweb/lib/spdflib5.dat'") ; this was ops
s = execute("restore, '/home/cdaweb/dev/lib/spdflib.dat'") ; this is dev
if s ne 1  then journal
if s ne 1  then  exit
print, version()
print, 'from within LATEST make_ssc_plots5_dev ...'
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
; RCJ 17Mar2020  Print statements below are just to show some separation in the idlrpc.out file
print,' '
print,'************************************************'
print,' '
help, /files
journal
cmd="file_copy, '/rwssd/sscweb_tmp/ssc_journal_dev_" +pid+ "', '/rwssd/sscweb_tmp/ssc_journal_dev', /force, /overwrite"
s=execute(cmd)
exit
