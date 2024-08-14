;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_break_decision,av,ii,si0
;input
; av = cmb_cdf2user_var(vars)
; ii -index of cdf variabe in structure av 
; si0 = size(d.(ip).dat
;output
;0 don't break into components
;1 break into components
;-1 error, skip this variable
if av.nc[ii] eq 1 then return,0
nc = 1l & for i=0,si0.n_dimensions-2 do nc = nc*si0.dimensions[i] ;number of elements in matrix at a timestep
if nc eq av.nc[ii] then return,1
print,'Number of components in cdf variable '+ av.cdfvar[ii] + ' not equal to user specified variable components ' + av.uservar[ii]+ ', variable skipped.'
return,-1
end
