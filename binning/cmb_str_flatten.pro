;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

;Caveat Emptor: this code was written by Scott Boardsen, Heliophysics Division, NASA/GSFC and UMBC/GEST.
function cmb_str_flatten,sa,space=space0,skipend=skipend
;convert string array sa into one string separated by spaces
n = n_elements(sa)
s=''
if n_elements(space0) eq 0 then space0 = ' '
space = replicate(space0,n)
if keyword_set(skipend) then space[n-1] = ''
for i=0,n-1 do s = s + sa[i] + space[i] 
return,s
end 
