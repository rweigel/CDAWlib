; RCJ 02/08/2016  Copied from make_ssc_plots5_dev_mambo, then edited.
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
 journal, '/bigdisk/sscweb_tmp/ssc_journal_dev'
ON_ERROR, 1
; s = execute("restore, '/home/rumba/cdaweb/lib/spdflib5.dat'") ; this was ops
 s = execute("restore, '/home/cdaweb/dev/lib/spdflib.dat'") ; this is dev
if s ne 1  then journal
if s ne 1  then  exit
print, version()
print, 'from within LATEST make_ssc_plots5_dev_aster on aster...'
print, 'IDL_DLM_PATH is: ',getenv('IDL_DLM_PATH')
print, 'IDL_PATH is: ',getenv('IDL_PATH')
print, 'IDL_DIR is: ',getenv('IDL_DIR')
print, ' cdfnames = ',cdfnames

s = ssc_plot(cdfnames,PID,OUTDIR)

!p.noerase=0
!p.multi=0
!p.background=0
!p.position=0
!p.region=0

;to gif files.
print, 'Status of ssc_plot = ',s
print, 'Make_ssc_plots finished'
help, /files
 journal
exit
