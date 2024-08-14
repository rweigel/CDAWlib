
;Caveat Emptor: this code was written by Scott Boardsen, Heliophysics Division, NASA/GSFC and UMBC/GEST.

function cmb_data_in_range,x,xr, branchcut=xcut,outsiderange=outsiderange
;input 
;x[n],xr[2]
;return indices of x the are in range given by xr

if keyword_set(xcut) then $
if xr[0] gt xr[1] then begin ;assume that the x is a angle and a branch cut exists between xr[0] and xr[1]
   ii0=where( x ge xr[0])
   ii1=where( x lt xr[1])
   ii= [ii0,ii1]
   ii = ii[sort(ii)]
   if ii[0] eq -1 then ii = ii[1:*]
   return,ii
endif

ii=where( x ge min(xr) and x le max(xr) )

if keyword_set( outsiderange ) then ii=where( x lt min(xr) or x gt max(xr) )

return,ii
end
