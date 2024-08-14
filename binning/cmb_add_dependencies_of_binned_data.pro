pro cmb_add_dependencies_of_binned_data,d,d0
; add dependencies of binned variables to 'd'
; rename dependencies of binned variables by adding '_bin' to values of depend_1 etc.
help, d, d0
t0 = TAG_NAMES(d0)
iaux = strpos( t0, 'AUX') & iaux = where(iaux ne -1) & iaux =iaux[0]
dummy = cmb_tag_name_exists('AUX', d0, iaux)
ip = strpos( t0, '_DEPEND') & ip = where( ip ne -1)
if ip[0] eq -1 then begin
   print,'no binned data variables returning'
   return
endif

; insert dependencies of binned data into d
i=0
vdpend = t0[ip]
for i = 0l, n_elements(ip)-1 do begin
    i0 = ip[i]
    var = strmid(t0[i0] ,0, strpos(t0[i0], '_DEPEND'))
    vdpend[i] = var
    dummy = cmb_tag_name_exists(var,d, i1)
    a = d.(i1)
    a.varname = a.varname + '_bin'
    a.fieldnam = a.varname + '_bin'
    a.handle = handle_create(value = d0.(i0) )
    if cmb_tag_name_exists('delta_minus_var', a) then a.delta_minus_var = ''
    if cmb_tag_name_exists('delta_plus_var', a) then a.delta_plus_var = ''
    ; Make sure any created variable has VAR_TYPE set to 'support_data'
    ; Ron Yurow
    a.var_type = 'support_data'
    d = CREATE_STRUCT(d, a.varname, a)
endfor

; modify dependencies _1 _2 etc. of binned variables by adding  '_bin'
ip = LINDGEN( iaux)
FOR i = 0l, n_elements(ip)-1 do begin
    dummy = cmb_tag_name_exists(t0[ip[i]],d, i1)
    if cmb_tag_name_exists('DEPEND_1', d.(i1)) then $
       if d.(i1).depend_1 ne '' then d.(i1).depend_1 = d.(i1).depend_1 + '_bin'
    if cmb_tag_name_exists('depend_2', d.(i1)) then $
       if d.(i1).depend_2 ne '' then d.(i1).depend_2 = d.(i1).depend_2 + '_bin'
    if cmb_tag_name_exists('depend_3', d.(i1)) then $
       if d.(i1).depend_3 ne '' then d.(i1).depend_3 = d.(i1).depend_2 + '_bin'
    print,d.(i1).varname, ' depend_1: ', d.(i1).depend_1
ENDFOR

end
