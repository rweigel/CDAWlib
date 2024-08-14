;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
pro cmb_ical,yr,doy,month,dom,eom=eom,idoy = idoy ;set idoy=1 to compute day of year
;eom - set to compute the doy  -> to the end of the month
;written and modified 9/03/2007 by Scott Boardsen UMBC/GEST at GSFC
if( n_params() eq 0 ) then begin
	print,'positional param: yr,doy,month,dom
	print,'keyword: idoy = 0-doy to month and dom, 1-month dom to doy'
	return
endif
if( n_elements(idoy) eq 0 ) then idoy = 0
days=[31,28,31,30,31,30,31,31,30,31,30,31]
yr0=yr
if yr0 lt 100 and yr0 gt 50 then yr0=1900+yr0
if yr0 le 50 then yr0=2000+yr0

if( ((yr0-1900) mod 4) eq 0)then begin
	days(1) = 29 ;leap year
;        print,'year:',yr0,' is a leap year'
endif else days(1) = 28

if keyword_set(eom) then begin
   idoy = 1
   dom = days(month-1)
endif

if(idoy eq 1)then begin
	doy =dom
	for i= 0,month-2 do doy = doy + days(i)
	return
endif else begin
	dom = doy
	for month = 1,12 do begin
		if( dom le days(month-1)) then goto, jump
		dom = dom - days(month-1)
	endfor
endelse
print,' error in date conversion'
jump:return
end	
