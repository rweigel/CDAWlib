pro cmb_cdf_add_attribute,d, tagnames, attname, attvalue
ntags = n_tags(d)
tnames = tag_names(d)
for itag=0,ntags-1 do begin
    a = d.(itag)
    ip=where( strlowcase(tnames[itag]) eq  strlowcase(tagnames) )
    if ip[0] ne -1 then a= create_struct(a, attname, attvalue)
    if itag eq 0 then d0 =create_struct( tnames[itag],a) else d0 = create_struct(d0,tnames[itag],a)
endfor
d=d0
end