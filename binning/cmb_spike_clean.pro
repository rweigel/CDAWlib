function cmb_spike_clean, d, SIGMUL=sigmul
;+
; EXAMPLE USAGE:
;   status= cmb_skip_clean ( d, sigmul=sigmul)
;
; NAME:
;   cmb_spike_clan
;            
; PURPOSE:   
; This procedure is wrapper function for cmb_spike_editor0 to allow it to be easily called 
; from within CDAWeb
;
; CATEGORY:
; Data binning.
;
; CALLING SEQUENCE:                                   
;  status= cmb_skip_clean ( d, sigmul=sigmul )
;                                                     
; INPUTS:                                             
; d = data structure returned by read_mycdf.pro
;
; Keyword Inputs:
;   sigmul: if set and sigmul >= 1, sigmul is the multiplicative factor of standard 
;   deviation for rejection of data: 
;           5 (default)
;           4 (less aggressive)
;           6 (more aggressive)
;   Note if sigmul < 1 or not defined filtering is skipped.
;
; OUTPUTS:
;   1 is successful
;   0 if not.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   Unknown.
;
; RESTRICTIONS:
;   Unknown.
;
; FUNCTION:
;
; MODIFICATION HISTORY:
;   Based on code developed by Aaron Roberts and Scott Boardsen at GSFC.
;-

if cmb_var_type(d) ne 'STRUCT' then return, 0

if n_elements(sigmul) eq 0 then return, 0

; Check for errors trapped in read_myCDF
d_tags = tag_names(d)
rflag = WHERE (d_tags eq 'DATASET')
if (rflag[0] ne -1) then return, 0

if sigmul ge 1. then begin
   
   tnames = tag_names(d)

   for itag=0,n_elements(tnames)-1 do begin

       ; Change conditional from enclosing an if branch to simply triggering the next
       ; iteration of the loop.
       ; Ron Yurow (Dec 17, 2021)
       ; if d.(itag).var_type eq 'data' then begin
       if  d.(itag).var_type ne 'data' then continue 

       ; Add check string variable types.
       ; Not sure how we got away without it for so long.
       ; Ron Yurow (Dec 17, 2021)
       sink = where ('CDFTYPE' eq tag_names (d.(itag)), cnt)
       if  cnt gt 0 && d.(itag).cdftype eq 'CDF_CHAR' then continue
          
       print,'tag_name:', tnames[itag]

       a = cmb_dat(d.(itag))
       fillval=d.(itag).fillval
       VALIDMIN = 0
       VALIDMAX = 0

       if  cmb_tag_name_exists('VALIDMIN', d) and cmb_tag_name_exists('VALIDMAX', d) then begin
           VALIDMIN = d.(itag).VALIDMIN
           VALIDMAX = d.(itag).VALIDMAX
       endif

       ; set values outside of valdimin/max to fillval
       cmb_filter_time_series,a,fillval=fillval, validmax=validmax, validmin=validmin 

       IF  ISA (a, /SCALAR) THEN a = [a]

       a = cmb_collapse(a, ndimsorg = ndimsorg)
      
       ig = cmb_autobad (a,sigmul, fillval=fillval, /filter_by_fill)

       ib = where(ig eq 0)

       if  ib[0] ne -1 then begin

           if  cmb_tag_name_exists('dat', d) then begin

               d.(itag).dat[ib] = fillval

           endif else begin

               a[ib] = fillval
               a = cmb_collapse(a, ndimsorg = ndimsorg,/revert)
               handle_value,/set,d.(itag).handle, a 
   
           endelse

       endif

       ; endif

   endfor

endif

return, 1

end
