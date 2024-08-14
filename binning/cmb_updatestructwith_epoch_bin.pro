
function cmb_updatestructwith_epoch_bin,d,s
; d0 = cmb_updatestructwith_epoch_bin(d,s)
 a = cmb_cdf_get_depend0(d, varsthathavedepend0=vars, check_allow_bin=1)
 d0=d
 a0 = d.(a[0].index)

; overwrite unbinned variables with their binned values
for ivar=0,n_elements(vars)-1 do $
if cmb_tag_name_exists(vars[ivar], d0, i0) then begin 
  if d0.(i0).depend_0 ne '' and d0.(i0).var_type eq 'data' then begin      
      if cmb_tag_name_exists('allow_bin', d0.(i0)) then BEGIN
         if d0.(i0).allow_bin ne 'FALSE' then d0.(i0).depend_0='Epoch_bin'
      endif else d0.(i0).depend_0='Epoch_bin'
  endif
endif
add_epoch_bin_delta = 0 ; set to 1 to add 'epoch_bin_delta'; SAB 2018/10/29
; modified/added SAB 2018/09/24 to fix attributes of Epoch_bin
a0.varname = 'Epoch_bin'
; Altered the following line to ensure that the FIELDNAM tag exists
; before setting it.  If it does not, then it is created.  Ron Yurow (Oct 2, 2018)
; a0.fieldnam = 'Epoch_bin'
if cmb_tag_name_exists('FIELDNAM',a0) then a0.fieldnam = 'Epoch_bin' $
   else a0 = CREATE_STRUCT (a0, 'FIELDNAM', 'Epoch_bin')
; Altered the following line to ensure that the CATDESC tag exists
; before setting it.  If it does not, then it is created.  Ron Yurow (Oct 2, 2018)
; a0.catdesc = 'Time base for time binned measurements, the time is the center time of each bin.'
catdesc = 'Time base for time binned measurements, the time is the center time of each bin.'
if cmb_tag_name_exists('CATDESC',a0) then a0.catdesc = catdesc $
   else a0 = CREATE_STRUCT (a0, 'CATDESC', catdesc)

if add_epoch_bin_delta then deltavar = 'epoch_bin_delta' else deltavar = ''  ; SAB 2018/10/29
if cmb_tag_name_exists('DELTA_MINUS_VAR',a0) then a0.DELTA_MINUS_VAR=deltavar  $
   else a0 = CREATE_STRUCT(a0,'DELTA_MINUS_VAR',deltavar)
if cmb_tag_name_exists('DELTA_PLUS_VAR',a0) then a0.DELTA_PLUS_VAR=deltavar $
   else a0 = CREATE_STRUCT(a0,'DELTA_PLUS_VAR',deltavar)
; define epoch_bin_delta

a0.handle = handle_create(value = s.epoch_bin)
; Altered the following line to ensure that the VAR_NOTES tag exists
; before setting it.  If it does not, then it is created.  Ron Yurow (Oct 2, 2018)
; a0.var_notes = 'times of binned data'
if cmb_tag_name_exists('VAR_NOTES',a0) then a0.var_notes = 'times of binned data' $
   else a0 = CREATE_STRUCT (a0, 'VAR_NOTES', 'times of binned data')
; This section fails for single data point.
; Specifically, the variable dtmsec requires two time points to be calculated, but with only a 
; Single data point, only one is available.
; Ron Yurow (Jan 13, 2021)
;epoch_bin = cmb_dat(a0) & dtmsec = (epoch_bin[1]-epoch_bin[0])/2
;if cmb_var_type(dtmsec) eq 'LONG64' then dtmsec = dtmsec/1d6  ; SAB 2018/10 29 added to insure dt is in msec not nsec.

epoch_bin = cmb_dat(a0) 

; This section is dead code.  However since this the only place where the dtmsec variable is needed,
; The code that calculates it will be put here as well.
; Ron Yurow (Jan 13, 2021)
if add_epoch_bin_delta then begin ; SAB 2018/10/29
   ; The following statement causes an error when binning returns only a single value. 
   ; Currently this section is dead code.  If it ever gets used, then this statement will need to 
   ; be refactored.
   dtmsec = (epoch_bin[1]-epoch_bin[0])/2
   if cmb_var_type(dtmsec) eq 'LONG64' then dtmsec = dtmsec/1d6  ; SAB 2018/10 29 added to insure dt is in msec not nsec.
	a1 = a0
	a1.varname = 'epoch_bin_delta'
	a1.fieldnam = 'epoch_bin_delta'
	a1.catdesc = 'Half width of time bin interval in ms'
	a1.DELTA_PLUS_VAR=''
	a1.DELTA_MINUS_VAR=''
	a1.handle = handle_create(value = dtmsec )
	a1.var_notes = 'time width of binned data'
	d0 = create_struct('Epoch_bin',a0,'epoch_bin_delta',a1 , d0) ; SAB 2018/10/29
endif else d0 = create_struct('Epoch_bin',a0, d0) ; SAB 2018/10/29
; end of changes SAB 2018/9/24 

; SAB added lines below to get the dependicies correct
tnames =tag_names(d0)
ii=where( strpos(tnames,'_BIN_DELTA') ne -1 or strpos(tnames,'_NBIN') ne -1)
for i = 0l, n_elements(ii)-1 do d0.(ii[i]).depend_0 = 'Epoch_bin'
ii=where(  strpos(tnames,'_NBIN') ne -1)
for i = 0l, n_elements(ii)-1 do d0.(ii[i]).DELTA_MINUS_VAR = ''
for i = 0l, n_elements(ii)-1 do d0.(ii[i]).DELTA_PLUS_VAR = ''
return,d0
end

