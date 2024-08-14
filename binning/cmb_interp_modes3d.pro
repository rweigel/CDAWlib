;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

pro cmb_interp_modes3d,datatemp,xo,xd,ixd,fillval
;cmb_interp_modes3d,datatemp,index,xd,xo,fillval
;datatemp[*,*,timestep] - data values of this minor mode are interpolated to that of the major mode.
;xd-values of the dependeny of the dominant mode, i.e. energy steps
;xo-values of this dependency of this minor mode. i.e. energy steps with different values than xd
;note this routine is called once for each minor mode of this depency.
;idx is the array index of this dependency, i.e. if idx=0 the datatemp[xo,*,timestep]

datamin = min( datatemp[where(datatemp ne fillval)])
ifill=where(datatemp eq fillval)
if ifill[0] ne -1 then datatemp[ifill]=datamin
si = size(datatemp,/str)
n0 = si.dimensions[0]
n1 = si.dimensions[1]
nt = si.dimensions[2]
if ixd eq 0 then begin
   if n_elements(xd) ne n0 then stop
    for it=0,nt-1 do begin
    for i1 =0,n1-1 do begin
	datatemp[*,i1,it]=interpol( datatemp[*,i1,it],xo,xd)
    endfor
    endfor
endif else begin
   if n_elements(xd) ne n1 then stop
    for it=0,nt-1 do begin
    for i0 =0,n0-1 do begin
	datatemp[i0,*,it]=interpol( datatemp[i0,*,it],xo,xd)
    endfor
    endfor
endelse
if ifill[0] ne -1 then datatemp[ifill]=fillval
end
