;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
pro cmb_interp_modes1d, data, ii

    ; Data points that correspond to minor sub-modes can be described as 'gaps' in
    ; in the data.  This routine will will perform a first order interpolation of 
    ; values inside these gaps based on the neighbors to either side.

    ; Set up some variables.
    ndata = n_elements (data)
    nii   = n_elements (ii)
    iprev = intarr (nii)
    inext = intarr (nii)
    first = -1      ; index of first major mode record
    last = -1       ; index of last major mode record
    drag = -1
    push = -1
    first_bad = ii [0] eq 0         ; true if first record is for a minor mode
    last_bad = ii [-1] eq ndata-1   ; true if last record is for a minor mode                    

    ; Create the iprev vector.  For each minor mode record, there is a corresponding 
    ; element in the iprev vector.  We will set each element of the iprev vector to the 
    ; nearest index prior to the current one that corresponds to a the major mode record.
    ptr = 0
    for ind = 0, ndata - 1 do begin
        ; ind refers to a minor mode record.
        if  (ind eq ii [ptr]) then begin
            iprev [ptr] = drag
            ptr++
            if  (ptr eq nii) then break
        ; ind refers to a major mode record.
        endif else begin
            drag = ind
            if  first lt 0 then first = ind
        endelse
    endfor

    ; Create the inext vector.  For each minor mode record, there is a corresponding 
    ; element in the inext vector.  We will set each element of the inext vector to the 
    ; nearest index after to the current one that corresponds to a the major mode record.
    ptr = n_elements (ii) - 1
    for ind = ndata-1, 0, -1 do begin
        ; ind refers to a minor mode record.
        if  (ind eq ii [ptr]) then begin
            inext [ptr] = push
            ptr--
            if  (ptr eq -1) then break
        ; ind refers to a major mode record.
        endif else begin
            push = ind
            if  last lt 0 then last = ind
        endelse
    endfor

    ; If the first record (and possibly following records) were minor mode records, then
    ; they will not refer to a valid major mode record at this point (they will be set
    ; -1).  Search on this value and replace it with the index of the first valid major
    ; mode record.
    if  first_bad then begin
        replace = where (iprev eq -1, cnt)
        if  cnt gt 0 then iprev [replace] = first
    endif

    ; If the first record (and possibly following records) were minor mode records, then
    ; they will not refer to a valid major mode record at this point (they will be set
    ; -1).  Search on this value and replace it with the index of the first valid major
    ; mode record.
    if  last_bad then begin
        replace = where (inext eq -1, cnt)
        if  cnt gt 0 then inext [replace] = first
    endif

    ; do the interpolation.
    data [ii] = (data [inext] + data [iprev]) / 2.0

    return

end

pro cmb_cdf_check_if_dependencies_are_time_varying,d, iv $
   ,set_depend_to_dominant_mode=set_depend_to_dominant_mode, multiple_modes=multiple_modes, $
   prepass=prepass, mode=mode
; cmb_cdf_check_if_dependencies_are_time_varying,d, iv
;Note if the attribute CDFMAJOR='ROW_MAJOR then the variabile dimensions are [depend_1,depend_2,depend_0] else [depend_2,depend_1,depend_0]

; multiple_modes = 0 ;interpolate minor modes to major mode
;                  1 set minor modes to fillval, default is 1

; prepass =        0 Operate normally
;                  1 Only check for multidimensional depend, and if available, then return
;                    the array minor mode indices in the mode keyword then stop.

; mode =           (prepass) return the array of minor mode indices.
;                  (normal) array of major mode to be used when setting to dominant mode
depend=''
a = d.(iv)
zdepends = cmb_cdf_get_dependencies_dimensionandsize(d) ; not used, diagnositic check
if keyword_set(not_zero) eq 0 then if cmb_tag_name_exists('depend_0',a) then depend = cmb_add_element(depend,a.depend_0)
;if keyword_set(multiple_modes) eq 0 then multiple_modes=1 ; set minor modes to fillval
if n_elements(multiple_modes) eq 0 then multiple_modes=1 ; set minor modes to fillval
if cmb_tag_name_exists('depend_1',a) then depend = cmb_add_element(depend,a.depend_1)
if cmb_tag_name_exists('depend_2',a) then depend = cmb_add_element(depend,a.depend_2)
if cmb_tag_name_exists('depend_3',a) then depend = cmb_add_element(depend,a.depend_3)
if cmb_tag_name_exists('depend_4',a) then depend = cmb_add_element(depend,a.depend_4)

; Update keywords based on the preset keyword
; Added for filtering minor modes from 1D data.
; Ron Yurow (Nov 3, 2021)
if  n_elements (prepass) eq 0 then prepass = 0
if  keyword_set (prepass) then prepass = 1
if  n_elements (mode) gt 0 then reformat = 1 else reformat = 0 
if  keyword_set (prepass)  then set_depend_to_dominant_mode = 0
if  keyword_set (reformat) then set_depend_to_dominant_mode = 0

ii=where(depend ne '')
if ii[0] eq -1 then return
depend=depend[ii]
depend = cmb_unique_string(depend) 
ndimdata = n_elements(size(cmb_dat(a),/dimen))
data = cmb_dat(a)

; Set the reformat flag.
; reformat will be set to true when actually reformating data based on the mode keyword
; for now we are limiting reformats to variables with only a single depend 
; Ron Yurow (Nov 3, 2021)
if  reformat && n_elements (depend) ne 1 then reformat = 0

for i=0,n_elements(depend)-1 do begin

       ; Check for the reformat flag.  This section only needs to be done only if we are not
       ; reformating data (normal processing or prepass)
       ; Ron Yurow (Nov 3, 2021)
       if  (~reformat) then begin
            ip = where( strupcase(depend[i]) eq tag_names(d)) & ip=ip[0]
            vname = depend[i]
            x = cmb_dat(d.(ip))
            ndim =  n_elements(size(x,/dimen) )
            if ndim eq 1 then goto,next 
            if ndim gt 2 then begin
               print,'routine cmb_cdf_check_dependencies_are_time_varying assumes that the dependency dimensions are < 3'
               print,'vname:', vname
               ; Force an error to transfer control to a global error handler.
               MESSAGE, "DEPEND variable not vector.", /NOPRINT
            endif
            xu = cmb_unique_arr(transpose(x),cnts=cnts,isame=isame)
            xu = transpose(xu)
       endif

       ; Only continue checking the dependent variable if we are reformatting or
       ; if at least one minor mode was found.  NOte that for reformatting, we are not
       ; doing anything with the dependent variable since we already have the array of 
       ; minor mode records.
       ; Ron Yurow (Nov 3, 2021)
       if  reformat || n_elements(cnts) gt 1 then begin
           ; Find all the minor sub-modes.
           ; We only need to do this section if we are not reformatting data.
           ; Ron Yurow (Nov 3, 2021)
           if  (~reformat) then begin
               cntsmax = max(cnts,imax)
               j = where(isame eq imax) & j=j[0]
               ;x_dominant_mode = xu[*,j]
               xd = x[*,j] 
               ixd = cmb_var_dependency_location(vname,a,z=zdepends) ;index of this dependency in the data array
               if keyword_set(set_depend_to_dominant_mode) then goto, next1
               print,'**********************************************************************' 
               print,'WARNING DEPENDENCY:', vname, ' IS TIME DEPENDENT'
               ;help,x,xu
               print,'occurrence of unique vectors of ' + vname + ':',cnts 
               help,multiple_modes, ndimdata 
               if multiple_modes  or ndimdata gt 2 then print,'setting data of minor modes to fillval:'  $
                  else print,'interpolating minor modes'
           endif

           ; Use n_passes to determine the number of iterations for the next loop, instead of cnts.
           ; Normally n_passes will be be set to cnts (total number of modes found), howwever when 
           ; Reformatting we will set it 1, since only a single pass through the loop is necessary.
           ; Ron Yurow (Nov 3, 2021)
           if  reformat then n_passes = 1 else n_passes = cnts  
           ; for iu=0,n_elements(cnts)-1 do begin  ; --- go through just once for reformat
           for iu=0,n_elements(n_passes)-1 do begin

               ; Rewrite this test to short-circuit if format is true
               ; Ron Yurow (Nov 3, 2021)
               ; if cnts[iu] ne cntsmax then begin
               if reformat || cnts[iu] ne cntsmax then begin

                  ; If we are reformating data, then the indexes of the minor mode records is
                  ; available from the keyword mode.  Otherwise we have to calculate it now.
                  ; Ron Yurow (Nov 3, 2021)
                  if  (reformat) then begin
                     ii=mode 
                     multiple_modes = 1  ; interpolation doesn't work with fillvals
                  endif else begin
                     ii=where(isame eq iu)
                     xo = x[*,ii[0]]
                     ;help,ii,cnts[iu]
                     if n_elements(ii) ne cnts[iu] then begin
                        print,'error: cnts ne cnts:',cnts[iu], n_elements(ii)
                        stop
                     endif
                  endelse 

                  ; If the prepass flag was set, then we now have indices of all the minor
                  ; mode records, so were done.
                  ; Ron Yurow  (Nov 3, 2021)
                  if  (prepass) then begin
                     mode = ii
                     return
                  endif

                  case ndimdata of
                  ; Added case for scalar data, however this should only be used for reformating
                  ; based on indexes of minor mode records which had previously been determined.
                  ; Ron Yurow (Nov 3, 2021)
                  1:begin 
                       if  (multiple_modes eq 0) then begin
                           cmb_interp_modes1d, data, ii
                       endif else begin
                           data[ii] = a.fillval
                       endelse
                     end
                  2:begin
                     ;xo = x[*,ii[0]]
                     datatemp = data[*,ii]
                     ;ik=where(datatemp eq a.fillval)
                     case multiple_modes of
                        0:begin
                           ;for ik=0,n_elements(ii)-1 do datatemp[*,ik]=interpol( datatemp[*,ik],xo,xd)
                           cmb_interp_modes2d,datatemp,xo,xd,a.fillval
                           data[*,ii] = datatemp
                           end
                        1:data[*,ii] = a.fillval
                     endcase
                     end
                  3:begin
                     case multiple_modes of
                        0:begin
                           datatemp = data[*,*,ii]
                           cmb_interp_modes3d,datatemp,xo,xd,ixd,a.fillval
                           data[*,*,ii] = datatemp
                           end 
                        1:data[*,*,ii] = a.fillval
                     endcase
                     end
                  4:begin
                     data[*,*,*,ii] = a.fillval
                     ;stop
                     end
                  else: begin
                     print,'Dimension of Data greater than 4:', size(data,/dimen)
                     stop
                     endelse
                  endcase   
              endif       
            endfor
          print,'**********************************************************************'
          if  reformat then break

           next1: ii=where( isame ne imax)
          xumax =xu[*,imax]
          for ic=0, n_elements(xumax)-1 do x[ic,ii] = xumax[ic]
          ;if keyword_set(set_depend_to_dominant_mode) then d.(ip).dat=x
          if keyword_set(set_depend_to_dominant_mode) then  cmb_dat_storedata, d,x, ip
          if keyword_set(set_depend_to_dominant_mode) then  d.(ip).depend_0 = ''
          ;if keyword_set(set_depend_to_dominant_mode) then stop

       endif
       next: ;check next dependency
endfor
;d.(iv).dat = data
cmb_dat_storedata, d,data, iv
end

