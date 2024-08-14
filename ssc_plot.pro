;$Author: rcjohns1 $
;$Date: 2020/03/17 16:42:04 $
;$Header: /home/cdaweb/dev/control/RCS/ssc_plot.pro,v 1.53 2020/03/17 16:42:04 rcjohns1 Exp rcjohns1 $
;$Locker: rcjohns1 $
;$Revision: 1.53 $
;
;+
; NAME: ssc_plot.pro
;
; PURPOSE: Calls read_myCDF and plotmaster for SSCWEB plots
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------

FUNCTION ssc_plot, cdfnames, PID, OUTDIR

; Establish error handler 
  catch, error_status
  if(error_status ne 0) then begin
   print, 'STATUS= SSCWEB PLOT failed.'
   print, 'ERROR=Error number: ',error_status,' in ssc_plot.'
   print, 'ERROR=Error Message: ', !ERR_STRING
;   printf, 1, 'STATUS= SSCWEB PLOT failed.'
;   printf, 1, 'ERROR=Error number: ',error_status,' in ssc_plot.'
;   printf, 1, 'ERROR=Error Message: ', !ERR_STRING
    close, 1
   return, -1
  endif

nbufs=n_elements(cdfnames)
;print, nbufs
print,'In ssc_plot.pro.  ',systime()
for i=0, nbufs-1 do begin 
 print, cdfnames[i] 
 vnames = ' ' 
 buf = read_myCDF(vnames, cdfnames[i], /ALL, /NODATASTRUCT)

 w=execute('buf'+strtrim(string(i),2)+'=buf')
  if w ne 1 then begin
      print,' Error in EXECUTE function'
      print, 'ssc_plot: A plotting error has occurred'
   return, -1
  endif

endfor
 
if n_elements(OUTDIR) eq 0 then OUTDIR = '/tmp/'
if n_elements(PID) eq 0 then PID = 1L
print, 'outdir = ',outdir
print,'pid = ', pid

if(nbufs eq 1) then s1 = plotmaster(buf0, /AUTO, PID=PID, /PS, /SSCWEB, $
                                 OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 2) then s1 = plotmaster(buf0,buf1, /AUTO, PID=PID, /PS, /SSCWEB, $
                                 OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)         
if(nbufs eq 3) then s1 = plotmaster(buf0,buf1,buf2, /AUTO, PID=PID, /PS, $
                          /SSCWEB,OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)         
if(nbufs eq 4) then s1 = plotmaster(buf0,buf1,buf2,buf3, /AUTO, PID=PID, /PS, $
                         /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)         
if(nbufs eq 5) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4, /AUTO, PID=PID, $
                    /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG) 
if(nbufs eq 6) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5, /AUTO, $
             PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG) 
if(nbufs eq 7) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6, /AUTO, $
             PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 8) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7, $
         /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 9) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7, buf8, $
         /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 10) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7, buf8,buf9, $
         /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 11) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10, $
         /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 12) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 13) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         buf12, /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 14) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         buf12, buf13, /AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 15) then s1 = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         buf12, buf13, buf14,/AUTO, PID=PID, /PS, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)


if(nbufs eq 1) then s = plotmaster(buf0, /AUTO, PID=PID, /GIF, /SSCWEB, $
                                 OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 2) then s = plotmaster(buf0,buf1, /AUTO, PID=PID, /GIF, /SSCWEB, $
                                 OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)         
if(nbufs eq 3) then s = plotmaster(buf0,buf1,buf2, /AUTO, PID=PID, /GIF, $
                          /SSCWEB,OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)         
if(nbufs eq 4) then s = plotmaster(buf0,buf1,buf2,buf3, /AUTO, PID=PID, /GIF, $
                         /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)         
if(nbufs eq 5) then s = plotmaster(buf0,buf1,buf2,buf3,buf4, /AUTO, PID=PID, $
                    /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG) 
if(nbufs eq 6) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5, /AUTO, $
             PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG) 
if(nbufs eq 7) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6, /AUTO, $
             PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 8) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7, $
         /AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 9) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7, buf8,$
         /AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 10) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7, buf8,buf9,$
         /AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 11) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10, $
         /AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 12) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         /AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 13) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         buf12, /AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 14) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         buf12,buf13,/AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)
if(nbufs eq 15) then s = plotmaster(buf0,buf1,buf2,buf3,buf4,buf5,buf6,buf7,buf8,buf9,buf10,buf11, $
         buf12,buf13,buf14,/AUTO, PID=PID, /GIF, /SSCWEB, OUTDIR=OUTDIR,/SMOOTH, /SLOW, /DEBUG)

;if(nbufs gt 12) then begin 
if(nbufs gt 15) then begin 
   reportf=strtrim(string(OUTDIR),2)+"idl_"+strtrim(string(PID),2)+".rep"
   openw,1,reportf 
   printf, 1, "STATUS=Select fewer than 15 satellites. "
   printf, 1, "ERROR=Number of bufs for plotmaster exceeds 15. NBUF=",nbufs 
   close, 1
   return, -1
endif
;print, '** s = ',s
;print, '** s1 = ',s1

 
if(s1 ne 0) then printf,1, "WARNING=PS plot failed "
if(s ne 0) then begin
   printf, 1, "STATUS=Plot failed  "
   printf, 1, "ERROR=Plotmaster called in SSC_PLOT failed " 
   close, 1
   return, -1
endif
   
   ;close, 1
   close,/all
return, s
end
