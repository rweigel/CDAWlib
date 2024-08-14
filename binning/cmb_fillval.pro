function cmb_fillval,x,fillval, ieq=ieq
; ii = cmb_fillval(x,fillval, ieq=ieq)
; if ieq is set and ne 0 then return indices that are indices of fillval
; else return indices that are not fillval
if keyword_set(ieq) then BEGIN
	case finite(fillval) OF
	0:i = where( finite(x) eq 0)
	1:i = where( x eq fillval)
	endcase
endif else begin
	case finite(fillval) OF
	0:i = where( finite(x) eq 1)
	1:i = where( x ne fillval)
	endcase
ENDELSE
return,i
end
