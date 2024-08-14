;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_dependency_collapse,y,istat
;+
; yu = cmb_dependency_collapse(y,istat)
;INPUTS:
;y is a dependency and if y is 2d and the first dimension doesn't vary with time remove the time  dimension.
;y[ dependency at time t, time t]
;
;KEYWORDS:
;    istat ; 1 is dependency is not time varying, 0 if dependency is time varying.
;OUTPUTS:
;if istat eq 1 then reform(y[ dependency at time t, time t]) if istat eq 0 then y[ dependency at time t, time t]
;
; MODIFICATION HISTORY:
;   SAB added 'min( finite(y[i,*]))' because NaN is not equal to NaN 2016-02-12
si0 = size(y,/structure)
istat=1
if si0.n_dimensions le 1 or si0.n_dimensions gt 2 then return,y
nd0 = si0.dimensions[0:si0.n_dimensions-1]
nt = nd0[si0.n_dimensions-1] ;time steps
y0 = reform(y[*,0])
for i=0,nd0[0]-1 do begin
    ;print,'i,min,max:',i,min(y[i,*]), max(y[i,*])
    if min(y[i,*]) ne max(y[i,*]) and min( finite(y[i,*])) then begin ;SAB 2016-02-12
       istat=0
       return,y
    endif
endfor
return,y0
end