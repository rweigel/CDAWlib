;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_cdaw_meta, d
; d is cdaw data structure returned by read_mycdf
; extract meta data from cdaw cdf

list =['VARNAME', 'LOGICAL_FILE_ID','LOGICAL_SOURCE', 'DESCRIPTOR', 'DATA_VERSION', $
       'TIME_RESOLUTION', 'FIELDNAM','FILLVAL','VALIDMIN','VALIDMAX','UNITS', 'DEPEND_0', 'DEPEND_1','DEPEND_2','DEPEND_3', 'DICT_KEY', 'CATDESC', 'VAR_NOTES','CDFTYPE','CDFRECVARY','CDFMAJOR','VAR_TYPE']

n=n_elements(list)
meta = create_struct(list[0],'')
for i=1, n-1 do begin
    if list[i] eq 'FILLVAL' then vartype=-1e-31 else vartype=''
    if list[i] eq 'VALIDMIN' then vartype=-1e-31 else vartype=''
    if list[i] eq 'VALIDMAX' then vartype=-1e-31 else vartype=''
    meta = create_struct(meta, list[i],vartype)
endfor

if n_elements(d) ne 0 then begin
    vnames = tag_names(d)
    for i=0, n-1 do begin
	ii=where(list[i] eq vnames) & ii=ii[0]
	if ii[0] ne -1 then meta.(i) = (d.(ii))(0)
    endfor
endif

return,meta
end
