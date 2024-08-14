;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_cdf_get_dependencies, d, iv, not_zero=not_zero, writetotop=writetotop, smeta=smeta $
         , level=level,to_struct=to_struct, dp=dp, d0=d0
; depends = cmb_cdf_get_dependencies(d)
if n_elements(dp) eq 0 then dp='all'
if n_elements(iv) eq 0 then begin
   i0 = 0l
   i1 = n_tags(d)-1 
endif else begin
   i0 = iv
   i1 = iv
endelse
depend=''
for i=i0,i1 do begin
    a = d.(i)
    if keyword_set(not_zero) eq 0 then $
    if (dp eq 'all') or (dp eq '0') then if cmb_tag_name_exists('depend_0',a) then depend = cmb_add_element(depend,a.depend_0)
    if (dp eq 'all') or (dp eq '1') then if cmb_tag_name_exists('depend_1',a) then depend = cmb_add_element(depend,a.depend_1)
    if (dp eq 'all') or (dp eq '2') then if cmb_tag_name_exists('depend_2',a) then depend = cmb_add_element(depend,a.depend_2)
    if (dp eq 'all') or (dp eq '3') then if cmb_tag_name_exists('depend_3',a) then depend = cmb_add_element(depend,a.depend_3)
    if (dp eq 'all') or (dp eq '4') then if cmb_tag_name_exists('depend_4',a) then depend = cmb_add_element(depend,a.depend_4)
endfor
ii=where(depend ne '')
if ii[0] eq -1 then return,''
depend=depend[ii]

depend = cmb_unique_string(depend)

if keyword_set(writetotop) and depend[0] ne '' then begin

   for i=0,n_elements(depend)-1 do begin
       ip = where( strupcase(depend[i]) eq tag_names(d)) & ip=ip[0]
       vname = depend[i]
       valueout = cmb_dependency_collapse( cmb_dat(d.(ip)),istat )
       if istat eq 0 then begin
          print,'**********************************************************************'
          print,'WARNING DEPENDENCY:', vname, ' IS TIME DEPENDENT'
          print,'**********************************************************************'
       endif
       if cmb_var_type(d0) eq 'STRUCT' then begin
          d0 = create_struct( d0, vname+'_depend', valueout)
       endif else $
          if keyword_set(to_struct) then cmb_move_info_to_struct, to_struct, vname, valueout, level=level $
          else  DUMMY  = ROUTINE_NAMES(vname, valueout, STORE=level) ; store at calling level
          ameta =cmb_cdaw_meta( d.(ip))
          iexist =0
          if cmb_var_type(smeta) eq 'STRUCT' then begin ;check if already present
             ii=where( strupcase(vname) eq tag_names(smeta))
             if ii[0] ne -1 then iexist=1
          endif
       if iexist eq 0 then $
       if n_elements(smeta) eq 0 then smeta = create_struct(vname,ameta) else smeta = create_struct(smeta,vname,cmb_cdaw_meta(ameta))
       print,'Created dependency ' + vname
   endfor
endif
return,depend
end
