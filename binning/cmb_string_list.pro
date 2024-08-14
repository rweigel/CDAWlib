;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
pro cmb_string_list,a,n,more=more,lun=lun,format=format
if cmb_var_type(a) ne 'STRING' then return
if n_elements(n) eq 0 then n = n_elements(a)
for i=0,n-1 do print,i,' ',a[i]
if keyword_set(lun) then for i=0,n-1 do printf,lun,i,' ',a[i]
if keyword_set(more) then for i=0,n-1 do help,a[i]
end
