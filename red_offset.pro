;function to determine the appropriate offset for the red value (next value
;under white, which is at the very top) on the current color scale/pallet 
; - this depends on which device is being used and the difference between 
;the scale min and max (diff)
;TJK June 25, 2004
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
function red_offset, GIF=GIF, PNG=PNG, diff

color_table_size=!d.n_colors ;default of device 


if keyword_set(GIF) or keyword_set(PNG) then color_table_size = !d.table_size

roffset = 1  ; default
if (diff gt 0) then roffset = diff/(color_table_size)
;if keyword_set(GIF) then deviceclose
return, roffset
end
