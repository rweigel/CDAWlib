;$Author: ryurow $
;$Date: 2016/10/20 15:31:27 $
;$Header: /home/cdaweb/dev/control/RCS/apply_esa_qflag.pro,v 1.4 2016/10/20 15:31:27 ryurow Exp ryurow $
;$Locker: ryurow $
;$Revision: 1.4 $
;Function: Apply_esa_qflag
;Purpose: To use the quality variable to "filter out bad themis l2 esa 
;data points"
;Author: Tami Kovalick, Perot Systems, September 2008
;
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;
function apply_esa_qflag, astruct, orig_names, index=index

;Input: astruct: the structure, created by read_myCDF that should
;		 contain at least one Virtual variable.
;	orig_names: the list of varibles that exist in the structure.
;	index: the virtual variable (index number) for which this function
;		is being called to compute.  If this isn't defined, then
;		the function will find the 1st virtual variable.

;this code assumes that the Component_0 is the original variable, 
;Component_1 should be the filter/quality variable.

;astruct will contain all of the variables and metadata necessary
;to filter out the bad flux values (based on the filter variables values -
;a value >= 0 (bad). 

atags = tag_names(astruct) ;get the variable names.
vv_tagnames=strarr(1)
vv_tagindx = vv_names(astruct,names=vv_tagnames) ;find the virtual vars

if keyword_set(index) then begin
  index = index
endif else begin ;get the 1st vv

  index = vv_tagindx[0]
  if (vv_tagindx[0] lt 0) then return, -1

endelse

;print, 'In Apply_esa_qflag'
;print, 'original variables ',orig_names

c_0 = astruct.(index).COMPONENT_0 ;1st component var (real flux var)

if (c_0 ne '') then begin ;this should be the real data
  var_idx = tagindex(c_0, atags)
  itags = tag_names(astruct.(var_idx)) ;tags for the real data.

  d = tagindex('DAT',itags)
    if (d[0] ne -1) then  esa_data = astruct.(var_idx).DAT $
    else begin
      d = tagindex('HANDLE',itags)
      handle_value, astruct.(var_idx).HANDLE, esa_data
    endelse
  fill_val = astruct.(var_idx).fillval

endif else print, 'ESA variable not found'


;9/2/2008 - TJK - check for just one record, if found make it (1,*,*) and
;           continue on.  Otherwise it gets thrown out.
;if (data_size(0) eq 2) then begin 
;  dims = size(esa_data, /dimensions)
;  tmp_esa = make_array(1,dims(0), dims(1))
;  tmp_esa(0,*,*) = esa_data
;  esa_data = tmp_esa
;  data_size = size(esa_data)
;endif


c_0 = astruct.(index).COMPONENT_1 ; should be the quality variable

found_data_quality = 0

if (c_0 ne '') then begin ;
  var_idx = tagindex(c_0, atags)
  itags = tag_names(astruct.(var_idx)) ;tags for the real data.

  d = tagindex('DAT',itags)
; Rewrote the following to actually check if the data and or handle is
; valid or if it just a null value set by read_myCDF but no 'real' data
; is available.
; Ron Yurow (Oct 19, 2016) 
;    if (d[0] ne -1) then quality_data = astruct.(var_idx).DAT $
;    else begin
;      d = tagindex('HANDLE',itags)
;     handle_value, astruct.(var_idx).HANDLE, quality_data
;    endelse
  if (d[0] ne -1) then begin

     if  (astruct.(var_idx).DAT [0] ne 0) then begin
         quality_data = astruct.(var_idx).DAT
         found_data_quality = 1
     endif 

  endif else begin

     handle = astruct.(var_idx).HANDLE
    
     if  (handle ne 0) then begin
         handle_value, astruct.(var_idx).HANDLE, quality_data
         found_data_quality = 1
     endif   

  endelse


  ; Also check to see if the data quality variable has a fill value.  For some
  ; reason this is a possibility.
  found_qflag_fill = 0 

  a = tagindex ('FILLVAL', tag_names (astruct.(var_idx)))
  IF (a[0] ne -1) THEN BEGIN 
     b = SIZE (astruct.(var_idx).FILLVAL, /N_ELEMENTS)
     IF (b ne 0) THEN BEGIN
        qflag_fill_val = astruct.(var_idx).fillval 
        found_qflag_fill = 1
     ENDIF 
  ENDIF

; If we couldn't read the data quality variable for some reason, then we need
; to do more then print out a message.  Instead we will create the data quality
; variable ourselves with every element set to good data.
; Ron Yurow (19 Oct 2016)  
; endif else print, 'Quality variable not found'
endif

; The data quality flag values were not available for some reason.  So we 
; will just make some up!  All data is now good data.
if  ~found_data_quality then begin

    fmt = '("STATUS=Variable name: ", A, ' + $ 
          '" is not available so the data quality flag has not been applied")'
   
    error_str = STRING (FORMAT = fmt, astruct.(var_idx).VARNAME) 
  
    PRINT, error_str

    dim = SIZE (esa_data, /DIMENSIONS)

    quality_data = MAKE_ARRAY (DIMENSION=dim, VALUE=0)

; We have valid data quality info.  But we still need to account for the 
; possibility that some values may be set to fillval ?!?
endif

; Check if the quality flag had a fill value.  If there is one, then records 
; where the quality flag is set to FILL will automatically be assumed to 
; contain valid data. 
if found_qflag_fill then begin

     filli = where (quality_data eq qflag_fill_val, fillcnt)
     if fillcnt then quality_data [filli] = 0
     
endif

;temp = where(quality_data(0,*) gt 0, badcnt)
temp = where(quality_data gt 0, badcnt)
if (badcnt ge 1) then begin
   print, 'found ',badcnt, ' bad ESA data points'
   dims = size(esa_data, /n_dimensions)
;   print, 'size of esa_data = ',dims
;help, quality_data
;help, esa_data
;help, temp
   if (dims eq 1) then esa_data[temp] = fill_val
   if (dims eq 2) then esa_data[*,temp] = fill_val
   if (dims eq 3) then esa_data[*,*,temp] = fill_val
endif



;now, need to fill the virtual variable data structure with this new data array
;and "turn off" the original variable.

temp = handle_create(value=esa_data)

astruct.(index).HANDLE = temp

esa_data = 1B
quality_data = 1B

; Check astruct and reset variables not in orignal variable list to metadata,
; so that variables that weren't requested won't be plotted/listed.

   status = check_myvartype(astruct, orig_names)

return, astruct

;endif else return, -1 ;if there's no esa data return -1

end





