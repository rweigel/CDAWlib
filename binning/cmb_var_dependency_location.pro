;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
function cmb_var_dependency_location,depend,a,z=z
;return index of depend for data a.dat
; index = cmb_var_dependency_location(depend,a)
;Note to self:the if attribute CDFMAJOR='ROW_MAJOR then the variabile dimensions are [depend_1,depend_2,depend_0] else [depend_2,depend_1,depend_0]
; z is from z = cmb_cdf_get_dependencies_dimensionandsize(d)
if cmb_tag_name_exists('depend_0',a) then if depend eq a.depend_0 then idepend=0
if cmb_tag_name_exists('depend_1',a) then if depend eq a.depend_1 then idepend=1 
if cmb_tag_name_exists('depend_2',a) then if depend eq a.depend_2 then idepend=2
if cmb_tag_name_exists('depend_3',a) then if depend eq a.depend_3 then idepend=3
ndim = n_elements(size(cmb_dat(a),/dimen))
if idepend eq 0 then return, ndim-1

if cmb_tag_name_exists( 'cdfmajor',  a) eq 0 then return, cmb_var_dependency_guess_location(depend,a,z=z)
cdfmajor = a.cdfmajor
if cdfmajor eq 'ROW_MAJOR' then index = ndim-1-idepend else index = idepend-1 
;help,index,cdfmajor,ndim,idepend, depend
return,index
end
