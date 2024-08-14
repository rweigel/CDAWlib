;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_unique_string,s,nis=nis,make_unique=make_unique

n = n_elements(s)
nis = intarr(n)

for i=0l, n-1 do begin
    a = s[i]
    j=where( a eq s)
    nj = n_elements(j)
    if nj gt 1 then begin
      if keyword_set(make_unique) then begin
         s[j[0:nj-1]]=  s[j[0:nj-1]] + strtrim(string(lindgen(nj)),2)        
      endif else begin
         s[j[1:nj-1]]= 'dup787'
      endelse
       ;print,s[j[0:nj-1]]
       ;stop
    endif
endfor
i =where( s ne 'dup787')
s0 = s[i]
return,s0
end
