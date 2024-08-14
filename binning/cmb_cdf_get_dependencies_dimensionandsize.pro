;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
function cmb_cdf_get_dependencies_dimensionandsize,d
; z = cmb_cdf_get_dependencies_dimensionandsize(d)
;input
; d is a structure created by read_mycdf or spdfgetdata
;output
; structure whose tag names are the dependencies with ndimensions and sizes.

depends = cmb_cdf_get_dependencies(d)
tnames = tag_names(d)
for i=0,n_elements(depends)-1 do begin
    iv =where( strlowcase(depends[i]) eq strlowcase(tnames))
    si = size(cmb_dat(d.(iv)),/structure)
    if n_elements(z) eq 0 then z = create_struct(depends[i], si) else  z = create_struct(z,depends[i], si)  
endfor
return,z
end
