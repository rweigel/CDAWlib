
function cmb_cdf_extract_metadata, d, ip
; meta = cmb_cdf_extract_metadata( d, i1)
tnames = tag_names(d.(ip))
meta = create_struct( tnames[0],d.(ip).(0))
for itag=1,n_elements(tnames)-1 do $
    if tnames[itag] ne 'DAT' and tnames[itag] ne 'HANDLE' then meta = create_struct(meta, tnames[itag],d.(ip).(itag))
return,meta
end
