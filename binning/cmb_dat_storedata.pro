pro cmb_dat_storedata, d,data, ip

if n_elements(ip) eq 0 then begin
  if cmb_tag_name_exists('dat', d) then begin
     d.dat=data
     return
  endif
  if cmb_tag_name_exists('handle', d) then begin
    ;handle_free, d.handle
    d.handle = handle_create(value = data)    
  endif
endif else begin
  if cmb_tag_name_exists('dat', d.(ip)) then begin
     d.(ip).dat = data
     return
  endif
  if cmb_tag_name_exists('handle', d.(ip)) then begin
    ;handle_free, d.(ip).handle
    d.(ip).handle = handle_create(value = data)    
  endif
endelse
end
