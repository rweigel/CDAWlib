function cmb_collapse,a,revert=revert,ndimsorg = nd1
;input a[n1,n2,..,nt] or a[n,nt] where nt is number of time steps
; n = n1*n2*..
; if input dimension of a is 2 or less return 'a' as is.
;ouput
; a[n,nt] or
; a[n1,n2,..,nt] if keyword revert is set

if keyword_set(revert) then return, reform(a,nd1,/overwrite)
si = size(a,/str)
nd1 = si.dimensions[0:si.n_dimensions-1]
if si.n_dimensions le 2 then return, a
nt = si.dimensions[si.n_dimensions-1]
na = 1l & for i=0,si.n_dimensions-2 do na = na*si.dimensions[i] ;number of elements in matrix at a timestep
nd0 = [na,nt]
return, reform(a,nd0,/overwrite)
end
