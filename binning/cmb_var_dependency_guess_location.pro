;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_var_dependency_guess_location,depend,a,z=z
; iv = cmb_var_dependency_guess_location( depend,a,z=z)
; if tag_names cdfmajor does not exit in the structure 'a' then guess where the dependency 'depend' in indexed.
print, '*** tag name: cdfmajor is not defined for cdf variable:' + a.varname + ' taking a guess at the index location of depency:' + depend + ' ***'
istat = cmb_tag_name_exists( depend,  z,i0)
adim = size(a.dat,/dimensions)
z0 = z.(i0)
if z0.n_dimensions eq 1 then iv = (where(z0.dimensions[0] eq adim))(0)
;note code has to be modified after this point
iv = (where(z0.dimensions[0] eq adim))(0)

return,iv
end
