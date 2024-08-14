pro cmb_spike_editor0,d,sigmul
;find where var_type is 'data'
tnames = tag_names(d)
for itag=0,n_elements(tnames)-1 do begin
    if d.(itag).var_type eq 'data' then begin
       print,'tag_name:', tnames[itag]
       a = cmb_dat(d.(itag))
       fillval=d.(itag).fillval
       VALIDMIN = 0
       VALIDMAX = 0
       if cmb_tag_name_exists('VALIDMIN',d.(itag)) and cmb_tag_name_exists('VALIDMAX',d.(itag)) then begin
          VALIDMIN = d.(itag).VALIDMIN
          VALIDMAX = d.(itag).VALIDMAX
       endif
       cmb_filter_time_series,a,fillval=fillval, validmax=validmax, validmin=validmin ; set values outside of valdimin/max to fillval
       a = cmb_collapse(a, ndimsorg = ndimsorg)
       
       ig = cmb_autobad(a,sigmul,fill=fillval, filter_by_fill=filter_by_fill)
       ib = where(ig eq 0)
       if ib[0] ne -1 then begin
          if cmb_tag_name_exists(d.(itag),'dat') then begin
             d.(itag).dat[ib] = fillval
          endif else begin
             a[ib] = fillval
             a = cmb_collapse(a, ndimsorg = ndimsorg,/revert)
             handle_value,/set,d.(itag).handle, a       
          endelse
       endif
    endif
endfor
end