
pro cmb_add_var2datastructure,d, d0, varname, suffix = suffix
; add  varname + '_NBIN' to data structure d
if varname eq 'AUX' then return
if n_elements(suffix) eq 0 then suffix= 'NBIN'
iexist = cmb_tag_name_exists(varname,d0, i0)
;slen = strlen(varname)-  strlen(suffix) -1 ; RonYurow 2017/11/28 following 2 lines
slen = strlen (varname)
if  strlen(suffix) gt 0 then slen = slen-  strlen(suffix) -1 
varnamedependent = strmid( varname, 0, slen)
iexist = cmb_tag_name_exists(varnamedependent,d, i1)
;help, i0,i1, varname, varnamedependent
;if i0 lt 0 or i1 lt 0 then message,'error, stopping code'
if i0 lt 0 or i1 lt 0 then return ;added by SAB 2017/11/28
if i0 le 0 then return ;i0=0 is depend_0, i0=-1 not found so return without doing anything
meta = cmb_cdf_extract_metadata( d, i1)
if cmb_tag_name_exists('VARNAME',meta) then meta.varname = varname
if cmb_tag_name_exists('UNITS',meta) then meta.UNITS = 'CNTS'
if cmb_tag_name_exists('VALIDMIN',meta) then meta.VALIDMIN = 0
; Check to make sure we are not setting the validmax to the fillval.  This can
; happen if every value is set to fillval ( possibly because the calculation
; of error bars was attempted using time interval with two or fewer data points ) 
; Added by Ron Yurow (August 22, 2018)
; if cmb_tag_name_exists('VALIDMAX',meta) then meta.VALIDMAX = max(d0.(i0))
IF  cmb_tag_name_exists ('VALIDMAX',meta) THEN BEGIN
    sink = WHERE (d0.(i0) ne meta.FILLVAL, cnt) 
    IF cnt eq 0 THEN meta.VALIDMAX = 1.0 ELSE  meta.VALIDMAX = MAX (d0.(i0))
ENDIF
; Added by Ron Yurow to limit plots only to the original data set.  Other variables
; added during binning should have the 'VAR_TYPE' attribute set to 'support data'.
if cmb_tag_name_exists('VAR_TYPE',meta) then meta.VAR_TYPE = 'support_data'
if cmb_tag_name_exists('catdesc',meta) then begin
	case suffix OF
	  'NBIN':meta.catdesc = 'Time Binning of ' +  meta.FIELDNAM
	else: begin
	   meta.catdesc = 'uncertainty of mean of ' +  meta.FIELDNAM
	   if cmb_tag_name_exists('DELTA_PLUS_VAR',meta) then meta.DELTA_PLUS_VAR =''
	   if cmb_tag_name_exists('DELTA_MINUS_VAR',meta) then meta.DELTA_MINUS_VAR=''
	   end
	ENDCASE
endif
if cmb_tag_name_exists('FIELDNAM',meta) then meta.FIELDNAM = meta.FIELDNAM + '_' + suffix

if suffix eq 'NBIN' then begin
	if cmb_tag_name_exists('LABLAXIS',meta) then $
	  if meta.LABLAXIS[0] ne '' then meta.LABLAXIS = '# of '+ meta.LABLAXIS

	if cmb_tag_name_exists('LABL_PTR_1',meta) then $
	  if meta.LABL_PTR_1[0] ne '' then meta.LABL_PTR_1 = '# of '+ meta.LABL_PTR_1

	if cmb_tag_name_exists('LABL_PTR_2',meta) then $
	  if meta.LABL_PTR_1[0] ne '' then meta.LABL_PTR_2 = '# of '+ meta.LABL_PTR_2
  
	if cmb_tag_name_exists('LABL_PTR_3',meta) then $
	  if meta.LABL_PTR_1[0] ne '' then meta.LABL_PTR_3 = '# of '+ meta.LABL_PTR_3
endif

handle = handle_create(value = d0.(i0))
a = create_struct(meta, 'HANDLE',handle_create(value = d0.(i0)))
d = create_struct(d,varname, a)
end

