;$Author: johnson $
;$Date: 2012/05/01 22:27:53 $
;$Header: /home/cdaweb/dev/control/RCS/handle_check.pro,v 1.4 2012/05/01 22:27:53 johnson Exp johnson $
;$Locker: johnson $
;$Revision: 1.4 $
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
FUNCTION handle_check, astruct

; Verify the type of the first parameter and retrieve the data
a = size(astruct)
if (a[n_elements(a)-2] ne 8) then begin
  print,'ERROR= 1st parameter is not a structure' & return,-1
endif else begin
  a = tagindex('DAT',tag_names(astruct))
  if (a[0] ne -1) then idat = astruct.DAT $
  else begin
    a = tagindex('HANDLE',tag_names(astruct))
    if (a[0] ne -1) then handle_value,astruct.HANDLE,idat $
    else begin
      print,'ERROR= 1st parameter does not have DAT or HANDLE tag' & return,-1
    endelse
  endelse
endelse

return, idat
end
