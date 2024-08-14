;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
function cmb_valid_data_range,d,ip
; fillflag = cmb_valid_data_range(d,ip)
; return valid data ranges for variable ip
if cmb_tag_name_exists('validmax',d.(ip)) then validmax = d.(ip).validmax 
if cmb_tag_name_exists('validmin',d.(ip)) then validmin = d.(ip).validmin 
fillval = max( abs([validmax,validmin]*1.1))
print,'fillval not defined setting it to ',fillval
return, fillval
end
