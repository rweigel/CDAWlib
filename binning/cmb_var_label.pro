;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
function cmb_var_label,var,other_depend,time_name
if other_depend[0] ne '' then a= cmb_str_flatten( other_depend,space=', ') else a=''
return, var + '[' + a + time_name + ']'
end
