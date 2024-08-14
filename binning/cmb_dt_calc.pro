
function cmb_dt_calc,t,noadd=noadd
if n_params() eq 0 then begin
   print,'dt_calc,variable t not defined returning'
endif

n=n_elements(t)
if n le 1 then return,0
i=lindgen(n-1)
dt = t[i+1]-t[i]
if keyword_set(noadd) then begin
   if n_elements(dt) eq 1 then dt=dt[0]
   return, dt
endif
;dt = [ dt, median(dt)]
dt = [ dt, dt[n-2]]
return,dt
end
