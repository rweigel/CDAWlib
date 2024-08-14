;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_cdf2user_var,a,d

if cmb_var_type(d) eq 'STRUCT' then begin
   ntags = n_tags(d)
   tnames = tag_names(d)
   for itag=0,ntags-1 do begin
       if d.(itag).var_type eq 'data' then begin
          ;if n_elements( cmb_dat(d.(itag)) ) eq 1 then goto, next ; 2018 1/9 SAB replaced with line below to handle vectors and tensors with only 1 time measurement
          if N_ELEMENTS( cmb_cdf_get_depend0(d,tagname0=tnames[itag]) ) le 1 then goto, next ; we won't bin 1 point, SAB. 
          if n_elements(cdfvar) eq 0 then cdfvar = tnames[itag] else cdfvar=[cdfvar, tnames[itag]] 
       endif
       next:
   endfor
   ; Make sure that we found at least one data varaible that has more than 1 data point.
   ; If we didn't we will return -1 as a signal that binnable varaibles are present. Ron Yurow
   if  N_ELEMENTS (cdfvar) ne 0 then $
       return, {cdfvar:cdfvar,uservar:cdfvar,nc:intarr(n_elements(cdfvar))+1} $ 
       else return, -1 
endif

na = n_elements(a)
cdfvar = strarr(na)
uservar = strarr(na)
nc = intarr(na)
for i=0,na -1 do begin
    b = strsplit(a[i],'=',/extract)
    cdfvar[i]=(strtrim(b[0],2))
    if n_elements(b) gt 1 then uservar[i] = b[1] else uservar[i] = strtrim(cdfvar[i],2)
    nc[i] = n_elements(cmb_var_name_components(uservar[i]))
endfor
return,{cdfvar:cdfvar,uservar:uservar,nc:nc}
end
