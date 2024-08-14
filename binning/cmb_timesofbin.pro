

function cmb_timesofbin,t0,tf,dt
; tbin = cmb_timesofbin(t0,tf,dt)
if dt le 0 then return, 0
ntb = ceil( (tf-t0)/double(dt)) ; # of time series nbins
tout = t0 + lindgen(ntb)*dt 
;print, 't0-tout[0]:',t0- tout[0]
;print, 'tf-tout[ntb-1]',tf-tout[ntb-1]
;print, 'tf-tout[ntb-1]/double(dt)',(tf-tout[ntb-1])/double(dt)
tout= tout + dt/2 ; center times of binned data 
return,tout
end