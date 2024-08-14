;$Author: johnson $
;$Date: 2012/05/01 21:47:44 $
;$Header: /home/cdaweb/dev/control/RCS/split_filename.pro,v 1.3 2012/05/01 21:47:44 johnson Exp johnson $
;$Locker: johnson $
;$Revision: 1.3 $
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;
PRO split_filename, instring, outpath, outfile
; split the instring into path and filename information
temp = break_mystring(instring,delimiter='/') ; assume UNIX
if n_elements(temp) eq 1 then begin ; no path information present
  outpath='' & outfile=temp[0] & return
endif else begin
  if temp[0] ne '' then outpath=temp[0] + '/' else outpath = ''
  for i=1,n_elements(temp)-2 do outpath = outpath + temp[i] + '/'
  outfile=temp[n_elements(temp)-1]
endelse
return
end


