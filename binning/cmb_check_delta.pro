; cmb_check_delta  compare user defined delta against binning defined delta per bin, save the largest.
function cmb_find_delta,tname, sdeltavarname, d,d0
; istat = cmb_find_delta( sdeltavarname, d,i0, aux)
;help,tname, sdeltavarname, d,d0
istat = cmb_tag_name_exists(tname, d, i0)
if istat eq 0 then return,0 ;SAB 04/04/2018
istat = cmb_tag_name_exists(tname + '_BIN_' + sdeltavarname, d0, i9)
if istat eq 0 then return,0 ;SAB 04/04/2018
ep = d0.epoch_bin
dt = ep[1]-ep[0]
deltabin = d0.(i9)
a = d.(i0)
istat = cmb_tag_name_exists(sdeltavarname, a, i1)
if istat ne 1 then return,0
if a.(i1) ne '' then  begin
 istat = cmb_tag_name_exists(a.(i1), d, i2)
 delta = cmb_dat(d.(i2))
 fillval = d.(i2).fillval
 istat = cmb_tag_name_exists('depend_0', a, i3)
 istat = cmb_tag_name_exists(a.(i3), d, i4)
 epdelta = cmb_dat(d.(i4) )
endif else return,0

nb = n_elements(ep)
for ib=0l,nb-1 do begin
    ii = cmb_data_in_range( epdelta, ep[ib] + [0,dt] )
    if ii[0] ne -1 then begin
    k =where( delta[ii] ne fillval)
    if k[0] ne -1 then begin
      deltamax = max( delta[ii[k]] )
      ;print, deltamax, deltabin[ib]
      deltabin[ib] =  deltabin[ib] > deltamax 
    endif
    endif
endfor

return,1
end


pro cmb_check_delta, d, d0
;PURPOSE
; cmb_check_delta  compare user defined delta against binning defined delta per bin, save the largest.
;find tag names for var_type 'data'.
tnames = tag_names(d0)
i= strpos(tnames,'_BIN_DELTA')
k=where(i ne -1)
if k[0] ne -1 then tnames[k] = ''
i= strpos(tnames,'_BIN')
k=where(i ne -1)
if k[0] ne -1 then tnames[k] = ''
i= strpos(tnames,'AUX')
k=where(i ne -1)
if k[0] ne -1 then tnames[k] = ''
i= strpos(tnames,'_NBIN')
k=where(i ne -1)
if k[0] ne -1 then tnames[k] = ''
tnames = tnames[where(tnames ne '')]
cmb_string_list, tnames
iv=0
for iv=0, n_elements(tnames)-1 do begin  
   istat = cmb_find_delta( tnames[iv], 'DELTA_PLUS_VAR', d, d0)
   istat = cmb_find_delta( tnames[iv],'DELTA_MINUS_VAR', d, d0)
endfor

end
