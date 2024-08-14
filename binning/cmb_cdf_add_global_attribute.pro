pro cmb_cdf_add_global_attribute, d, attribute,attvalue
ntags = n_tags(d)
tnames = tag_names(d)
help,d, attribute,attvalue
for itag=0,ntags-1 do begin
    a = d.(itag)
    ;stop
    if cmb_tag_name_exists(attribute,a,i0) then b=1 $
    else a= create_struct(a, attribute, attvalue)
    if itag eq 0 then d0 =create_struct( tnames[itag],a) $
    else d0 = create_struct(d0,tnames[itag],a)
    ;help, d0 & stop
endfor
d=d0
end

