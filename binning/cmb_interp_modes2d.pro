;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

pro cmb_interp_modes2d,datatemp,xo,xd,fillval
;cmb_interp_modes2d,datatemp,xd,xo,fillval
;datatemp[*,timestep] - data values of this minor mode are interpolated to that of the major mode.
;xd-values of the dependeny of the dominant mode, i.e. energy steps
;xo-values of this dependency of this minor mode.
datamin = min( datatemp[where(datatemp ne fillval)])
ifill=where(datatemp eq fillval)
if ifill[0] ne -1 then datatemp[ifill]=datamin
si = size(datatemp,/str)
n0 = si.dimensions[0]
nt = si.dimensions[1]
for it=0,nt-1 do begin
     datatemp[*,it]=interpol( datatemp[*,it],xo,xd)
endfor
if ifill[0] ne -1 then datatemp[ifill]=fillval

end
