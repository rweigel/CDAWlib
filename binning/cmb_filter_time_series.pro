
pro cmb_filter_time_series,x,fillvalin=fillvalin, validmax=validmax, validmin=validmin
; x is the variable to be binned
; set values of x outside of validmin and validmax to fillval, if validmax > validmin
; check if abs(x) very close to abs(fillval), if too close set those 'x' to fillval
if keyword_set(validmin) eq 0 or  keyword_set(validmax) eq 0 then begin
   validmin = 0
   validmax = 0
endif
validmin = min(validmin)
validmax = max(validmax)
fillval = fillvalin
;fillval = cmb_check_fillval( fillvalin, iredefinefillval, validmax=validmax, validmin=validmin) ; adds + 1 to fillval, if fillval in an integer <0 and abs(fillval) = fillval ; NOTE commented out, SAB 06/19/2017

vartype = cmb_var_type(x)
if vartype eq 'DOUBLE' or  vartype eq 'FLOAT' or vartype eq 'COMPLEX' or vartype eq 'DCOMPLEX' then BEGIN
   ifill = reform(  where( (abs(x) ge 0.99*abs(fillvalin)) and (abs(x) le  1.01*abs(fillvalin))) or x eq fillvalin  )
endif else ifill = reform( x eq fillvalin)



;ifill = reform(  where( (abs(x) ge 0.99*abs(fillvalin))) or x eq fillvalin  )

;help,x, fillvalin,fillval, validmax, validmin,ifill
if ifill[0] ne -1 then x[ifill] = fillval ;in
if validmax le validmin then return
	i=where( x lt validmin or x gt validmax)
	if i[0] ne -1 then x[i]=fillval ;in
	fillvalin = fillval ; SAB 6/19/2017, to communicate the change in fillval down the pipeline
end