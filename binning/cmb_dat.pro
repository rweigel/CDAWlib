function cmb_dat, d, ip
;values = cmb_dat( d, ip) or values = cmb_dat( d )
;return d.(ip).dat if tagname exists or  values of d.(ip).handel
if n_elements(ip) eq 0 then begin
  if cmb_tag_name_exists('dat', d) then return, d.dat
  if cmb_tag_name_exists('handle', d) then begin
    handle_value, d.handle, values
    return,values
  endif
endif else begin
  if cmb_tag_name_exists('dat', d.(ip)) then return, d.(ip).dat
  if cmb_tag_name_exists('handle', d.(ip)) then begin
    handle_value, d.(ip).handle, values
    return,values
  endif
endelse
return,0
end