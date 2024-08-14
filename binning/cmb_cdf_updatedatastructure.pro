; .compile cmb_cdf_updatedatastructure.pro
pro cmb_cdf_updatedatastructure,d,d0
; copy/overwrite data in d0 to d
;assume .handle instead of .dat
tnames = tag_names(d0)
tnames = tnames[0:n_elements(tnames)-2] 
cmb_cdf_add_global_attribute, d,'DELTA_PLUS_VAR',''
cmb_cdf_add_global_attribute, d,'DELTA_MINUS_VAR',''
for itag=0,n_elements(tnames)-1 do begin ; start at 1 to exclude epoch_bin
    if cmb_tag_name_exists( tnames[itag], d, i0) then begin
       ;if handle_info(d.(i0).handle) then handle_free, d.(i0).handle
       if cmb_tag_name_exists('handle',  d.(i0)) eq 0 then BEGIN
          print,'handle expected, stopping code'
       endif
       d.(i0).handle = handle_create(value = d0.(itag))
       d.(i0).DELTA_PLUS_VAR = tnames[itag] + $
              '_' + 'BIN_DELTA_PLUS_VAR'
       d.(i0).DELTA_MINUS_VAR = tnames[itag] + $ 
              '_' + 'BIN_DELTA_MINUS_VAR'
      ; help, d.(i0).varname, cmb_dat(d.(i0) )
       ;stop
    endif else begin 
       suffix=''
       if strpos(tnames[itag], 'EPOCH_BIN') ne -1 then goto, skip ;update epoch at end
       if strpos(tnames[itag], 'BIN_DELTA_PLUS_VAR') ne -1 then suffix='BIN_DELTA_PLUS_VAR'
       if strpos(tnames[itag], 'BIN_DELTA_MINUS_VAR') ne -1 then suffix='BIN_DELTA_MINUS_VAR'
       if strpos(tnames[itag], 'NBIN') ne -1 then suffix='NBIN'
       ;print,'B in cmb_cdf_updatedatastructure itag, suffix, tname: ', itag, suffix,tnames[itag]
       cmb_add_var2datastructure, d, d0, tnames[itag], suffix=suffix
       skip:
    ENDELSE
endfor
;cmb_update_depend2datastructure,d, d0 ;superceeded by cmb_modify_if_needed_depends_names

d = cmb_updatestructwith_epoch_bin(d,d0)  ;add epoch_nbin to structure

cmb_cdf_add_attribute,d, tnames, 'time_bin_width_sec', d0.aux.TIME_BIN_WIDTH_SEC
end

