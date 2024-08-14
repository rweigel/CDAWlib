;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_cdf_get_depend0,d, varsthathavedepend0=vars,tagname0=tagname0 $
                              , check_allow_bin= check_allow_bin
; a = cmb_cdf_get_depend0(d)
; epoch = cmb_cdf_get_depend0(d,tagname0=tagname0)
;Purpose scan structure for all depend_0
; return structure of unique depend_0 and their CDFTYPEcmb_add_element(depend0
; depend0 = cmb_cdf_get_depend0(d)

tnames= tag_names(d)

if keyword_set(tagname0) then begin ;return depend_0 values for tagname0
  istat = cmb_tag_name_exists(tagname0, d, i0)
  if istat eq 0 then begin
      print,'tag name doesnot exists:', tagname0
      return,0
  endif
  a = d.(i0)
  if cmb_tag_name_exists('depend_0',a,i1) then begin
     ; Extra condition to check for empty strings.  
     ; When a standard varaible attribute doesn't exist, read_myCDF will add a tag
     ; for it to the varaible structure, but leave that tag pointing to an empty string...
     ; Added by Ron Yurow (Feb 7, 2018)
     IF  STRLEN (STRCOMPRESS (a.(i1), /REMOVE_ALL )) gt 0 THEN BEGIN
         ip = where( strlowcase(a.(i1)) eq strlowcase(tnames)) & ip=ip[0] 
         return, cmb_dat(d.(ip))
     ENDIF
  endif
  print,tagname0, ' has no depend_0'
  return,0
endif

i0 = 0l
i1 = n_tags(d)-1
c=''
for itag=i0,i1 do begin
    a = d.(itag)
     if cmb_tag_name_exists('depend_0',a) then begin
        if n_elements(vars) eq 0 then vars = tnames[itag] else vars = [vars, tnames[itag]]
        ;pprint, tnames[itag],' ',a.depend_0
        c0 = strupcase(a.depend_0)
        if strlen(c0) gt 3 then begin
           i=where( c0 eq c)
           ;print,c0,' ',c,' ',i
           if i[0] eq -1 then c = [c,c0] 
        endif
     endif        
endfor
c=c[1:*]
cmb_string_list,c
s = replicate({name:'',DELTA_MINUS_VAR:'',DELTA_PLUS_VAR:'', cdftype:'',index:0,dtmin:0d0,dtmed:0d0, n:0l, allow_bin:1},n_elements(c))
for ic = 0,n_elements(c)-1 do begin
    i=where( c[ic] eq tnames) & i=i[0]
    s[ic].name = tnames[i]
    s[ic].cdftype = d.(i).cdftype
    if cmb_tag_name_exists('DELTA_MINUS_VAR',d) then s[ic].DELTA_MINUS_VAR = d.(i).DELTA_MINUS_VAR
    if cmb_tag_name_exists('DELTA_PLUS_VAR',d) then s[ic].DELTA_PLUS_VAR = d.(i).DELTA_PLUS_VAR
    s[ic].index = i
    s[ic].n = n_elements( cmb_dat(d.(i)) )
     if s[ic].n gt 1 then begin
        t= cmb_dat(d.(i))
        t= t[sort(t)]
        t = cmb_epoch_modify(t)
        t = cmb_dt_calc(t)
        s[ic].dtmin = min( t)/1d3
        s[ic].dtmed = median( t )/1d3
     endif
endfor

;help, check_allow_bin
if KEYWORD_SET( check_allow_bin ) then begin ;SAB 4/6/2016
   for ip=0l,N_TAGS(d)-1 do begin
       if cmb_tag_name_exists('depend_0', d.(ip)) then BEGIN
          if cmb_tag_name_exists('ALLOW_BIN',d.(ip)) then begin 
              if d.(ip).allow_bin eq 'FALSE' then BEGIN
                idummy = cmb_tag_name_exists(d.(ip).depend_0,s,i0)
                i0 = where( strlowcase(d.(ip).depend_0) eq strlowcase(s.name))
                s[i0].allow_bin = 0
                print, s[i0].name, ' setting allow_bin to 0'
              endif
           endif
           ;help, ip
           ;stop
       endif 
   endfor
endif

help,s
;stop
return,s
end
