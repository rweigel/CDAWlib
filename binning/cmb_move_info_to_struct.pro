;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

pro cmb_move_info_to_struct, strname, vname, valueout, level=level
;move dependency or meta data to structure
;strname=name of structure

if n_elements(level) eq 0 then level=1
IF N_ELEMENTS(ROUTINE_NAMES(STRNAME, FETCH=LEVEL)) GT 0 THEN  s  = ROUTINE_NAMES(STRNAME, FETCH=LEVEL) ;retrieve structure from the calling level

;help,s, strname, time_name,t_out, vname, valueout,valueout_nbin, dt_sec

tnames= strlowcase(tag_names(s))  
ii=where(vname eq tnames)
if ii[0] ne -1 then begin
    i0 =ii[0]
    a = s.(i0)
    if cmb_var_type(a) eq 'STRUCT' then begin
       vtnames = tag_names(valueout)
       if cmb_tag_name_exists(vtnames[0],a) then begin
          print,'WARNING: variable: ' + vtnames[0] + ' already exists, modified name to '  +  vtnames[0]+'_1'
          valueout1 = create_struct( vtnames[0]+'_1', valueout.(0))
          a = create_struct(a,valueout1)
       endif else a = create_struct(a,valueout)
       b = create_struct(tnames[0], s.(0))
       for i=1,i0-1 do b = create_struct(b,tnames[i], s.(i))
       for i=i0+1,n_elements(tnames)-1 do b = create_struct(b,tnames[i], s.(i))
       b = create_struct(b,vname, a)
       s = b
       b=1
    endif else print,'WARNING: variable: ' + vname + ' not added to structure'
endif else begin
    if cmb_tag_name_exists(vname,s) then $
       print,'WARNING: variable: ' + vname + ' not added to structure, because it is already defined in structure' $
    else  s = create_struct(s, vname, valueout)
endelse
DUMMY  = ROUTINE_NAMES(strname, s, STORE=level)
;  DUMMY  = ROUTINE_NAMES(time_name, t_out, STORE=1) ; store at top level
;  DUMMY  = ROUTINE_NAMES(vname, valueout, STORE=1) ; store at top level
;  if dt_sec gt 0 then DUMMY  = ROUTINE_NAMES(vname+'_nbin', valueout_nbin, STORE=1) 
end
