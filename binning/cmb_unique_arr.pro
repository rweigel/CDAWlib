;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_unique_arr,a,cnts=cnts,isame=isame
;given and n by m array a(n,m)
;return a list of sorted unique m column vectors in a b(l,m)
;cnts-no. of identical copies of each unique vector
si = size(a)

;add SAB 6/19/2017
; Ron Yurow pointed out that this routine fails if a contains some NAN values
; fix for NAN
i = where( finite(a) eq 0 )
if i[0] ne -1 then a[i] = -1d31
; end of fix
n = si[1]
m = si[2]
b = a*0 ;array which will hold unique vectors
id = lonarr(n)
;help,id
isame = LONARR(n)-1
ii = where( isame eq -1)
;help,isame,ii
icount = 0l
while (ii[0] ne -1) do begin
   bn =reform(a[ii[0],*])
   nii = n_elements(ii)
   for i=0,nii-1 do begin
       dd = max( abs(bn-reform(a[ii[i],*]) ) )
       if dd eq 0 then isame[ii[i]] = icount
   endfor
   b[icount,*] = bn
   icount = icount + 1
   ii = where( isame eq -1)
endwhile
print,'no. of unique vectors:',icount
cnts = lonarr(icount)
for i=0,icount-1 do cnts[i] = n_elements(where( isame eq i))
print,'no. of unique vectors:',icount,' no. of hits:',cnts
b = b[0:icount-1,*]
return,b
end

