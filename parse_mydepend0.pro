FUNCTION parse_mydepend0, a
;---------------------------------------------------------
; PURPOSE: If more than 1 depend_0 variable defined, separate variables
;          in the input structure.  Build a new structure for each
;          depend_0 and it's associated variables and metadata.
;
; AUTHOR:  R. Baldwin  HSTX
; 
; HISTORY:
;          1.0   R.Baldwin  1/97
;          12/13/2006 - TJK moved this code from w/in
;                       LIST_mystruct.pro to this file so that it can
;                       be called by a function in read_myCDF
;
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;
; Check input structure
chsz=size(a)
if(chsz[n_elements(chsz)-2] ne 8) then begin
   print, 'ERROR=Structure not defined in parse_mydepend0'
   return, -1
endif 
; Compile # and names of variables in structure 
namest=tag_names(a)
ns_tags=n_tags(a)
depend0=strarr(ns_tags)
depend1=strarr(ns_tags)

;for k=0, ns_tags-1 do depend0[k]=a.(k).depend_0
;RCJ 06/09/2004. If data is 'ignore_data' then don't bother w/ its depend_0.
for k=0, ns_tags-1 do begin
   if (strlowcase(a.(k).var_type) ne 'ignore_data' and $
   strlowcase(a.(k).var_type) ne 'additional_data') $
   then depend0[k]=a.(k).depend_0
endfor

depend0=depend0[uniq(depend0,sort(depend0))]
dc=where(depend0 ne '',dcn)
depend0=depend0[dc]
dc=where(depend0 ne ' ',dcn)
depend0=depend0[dc]

; Build mega-structure
if(dcn le 1) then begin
   ret_str=create_struct('num',1,depend0[0],a)
endif else begin
   for k=0, dcn-1 do begin
      astr='a'+strtrim(k,2) 
      comm_x0 = astr+'=create_struct(depend0[k],a.'+depend0[k]+')'
      x0=execute(comm_x0)
   endfor ; end k
   ret_str=create_struct('num',dcn)

   ; Find depend_0
   for k=0, ns_tags-1 do begin
      if(a.(k).depend_0 ne ' ') then begin
         for i=0, dcn-1 do begin
            astr='a'+strtrim(i,2)
            if(a.(k).depend_0 eq depend0[i]) then begin
	       comm='nms=tag_names(a'+strtrim(i,2)+')'
	       q=execute(comm)
	       q=where(nms eq strupcase(a.(k).varname))  ; see if var already in struct
               if q[0] eq -1 then begin
                  data=create_struct(namest[k],a.(k))
                  comm_x2=astr+'=create_struct('+astr+',data)'
                  x2=execute(comm_x2)
                  ;
	          ; check to see if there is a depend_1 attribute before trying
	          ; to use it - added on 09/20/2000 by TJK.
                  q=where(tag_names(a.(k)) eq 'DEPEND_1')
                  if q[0] ne -1 then begin
		     depend1=a.(k).depend_1   
		     ; RCJ 05/16/2013 If depend_1 exists, does alt_cdaweb_depend1 exist?
		     ;  If so, it will overwrite depend_1 
                     qq=where(tag_names(a.(k)) eq 'ALT_CDAWEB_DEPEND_1')
		     if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_1 ne '') then depend1=a.(k).alt_cdaweb_depend_1
                     ;if(a.(k).depend_1 ne '') then begin
                     if(depend1 ne '') then begin
                        comm_z1='depend_1=a.'+a.(k).depend_1
			if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_1 ne '') then comm_z1='depend_1=a.'+a.(k).alt_cdaweb_depend_1 
                        z1=execute(comm_z1) 
			q=where(nms eq strupcase(a.(k).depend_1)) ; see if var already in struct
	                if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_1 ne '') then q=where(nms eq strupcase(a.(k).alt_cdaweb_depend_1)) 
			if q[0] eq -1 then begin 
			   meta=create_struct(a.(k).depend_1,depend_1)
                           if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_1 ne '') then meta=create_struct(a.(k).alt_cdaweb_depend_1,depend_1) 
                           comm_x3=astr+'=create_struct('+astr+',meta)'
                           x3=execute(comm_x3)
			   ;if (qq[0] ne -1) then depdep1=alt_cdaweb_depend_1.depend_1 else depdep1=depend_1.depend_1
                           depdep1=depend_1.depend_1
                           ;if(depend_1.depend_1 ne '') then begin
                           if(depdep1[0] ne '') then begin
                             ;if (qq[0] ne -1) then comm_z2='depend_n=a.'+alt_cdaweb_depend_1.depend_1 else comm_z2='depend_n=a.'+depend_1.depend_1
                             comm_z2='depend_n=a.'+depend_1.depend_1
                             z2=execute(comm_z2) 
                             ;if (qq[0] ne -1) then meta_n=create_struct(alt_cdaweb_depend_1.depend_1,depend_n) else meta_n=create_struct(depend_1.depend_1,depend_n)
                             meta_n=create_struct(depend_1.depend_1,depend_n)
                             comm_x4=astr+'=create_struct('+astr+',meta_n)'
                             x4=execute(comm_x4)
                          endif 
			endif  ; if already in struct    
                     endif
		  endif
                  ;
                  ; RCJ 12/99 Added the following piece of code, to look
                  ; for depend_2 too.
                  ;
                  q=where(tag_names(a.(k)) eq 'DEPEND_2')
                  if q[0] ne -1 then begin
		     depend2=a.(k).depend_2
		     ; RCJ 05/16/2013 If depend_2 exists, does alt_cdaweb_depend2 exist?
		     ;  If so, it will overwrite depend_2 
                     qq=where(tag_names(a.(k)) eq 'ALT_CDAWEB_DEPEND_2')
		     if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_2 ne '') then depend2=a.(k).alt_cdaweb_depend_2
                     ;if(a.(k).depend_2 ne '') then begin
                     if(depend2 ne '') then begin
		        comm_z1='depend_2=a.'+a.(k).depend_2
                        if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_2 ne '') then comm_z1='depend_2=a.'+a.(k).alt_cdaweb_depend_2 
                        z1=execute(comm_z1)
		        q=where(nms eq strupcase(a.(k).depend_2))  ; see if var already in struct
	                if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_2 ne '') then q=where(nms eq strupcase(a.(k).alt_cdaweb_depend_2)) 
			if q[0] eq -1 then begin 
			   meta=create_struct(a.(k).depend_2,depend_2)
                           if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_2 ne '') then meta=create_struct(a.(k).alt_cdaweb_depend_2,depend_2) 
                           comm_x3=astr+'=create_struct('+astr+',meta)'
                           x3=execute(comm_x3)
			   ;if (qq[0] ne -1) then depdep2=alt_cdaweb_depend_2.depend_2 else depdep2=depend_2.depend_2
                           depdep2=depend_2.depend_2
			   ;if(depend_2.depend_2 ne '') then begin
                           if(depdep2[0] ne '') then begin
                             ;if (qq[0] ne -1) then comm_z2='depend_n=a.'+alt_cdaweb_depend_2.depend_2 else comm_z2='depend_n=a.'+depend_2.depend_2
                             comm_z2='depend_n=a.'+depend_2.depend_2
			     z2=execute(comm_z2) 
                             ;if (qq[0] ne -1) then meta_n=create_struct(alt_cdaweb_depend_2.depend_2,depend_n) else meta_n=create_struct(depend_2.depend_2,depend_n)
                             meta_n=create_struct(depend_2.depend_2,depend_n)
			     comm_x4=astr+'=create_struct('+astr+',meta_n)'
                             x4=execute(comm_x4)
                          endif
			endif  ; if already in struct  
                     endif
                  endif     
                  ;
                  ;
                  q=where(tag_names(a.(k)) eq 'DEPEND_3')
                  if q[0] ne -1 then begin
		     depend3=a.(k).depend_3
		     ; RCJ 05/16/2013 If depend_3 exists, does alt_cdaweb_depend3 exist?
		     ;  If so, it will overwrite depend_3 
                     qq=where(tag_names(a.(k)) eq 'ALT_CDAWEB_DEPEND_3')
		     if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_3 ne '') then depend3=a.(k).alt_cdaweb_depend_3
                     ;if(a.(k).depend_3 ne '') then begin
                     if(depend3 ne '') then begin
		        comm_z1='depend_3=a.'+a.(k).depend_3
                        if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_3 ne '') then comm_z1='depend_3=a.'+a.(k).alt_cdaweb_depend_3 
                        z1=execute(comm_z1)
		        q=where(nms eq strupcase(a.(k).depend_3))  ; see if var already in struct
	                if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_3 ne '') then q=where(nms eq strupcase(a.(k).alt_cdaweb_depend_3)) 
			if q[0] eq -1 then begin 
			   meta=create_struct(a.(k).depend_3,depend_3)
                           if (qq[0] ne -1) then if (a.(k).alt_cdaweb_depend_3 ne '') then meta=create_struct(a.(k).alt_cdaweb_depend_3,depend_3) 
                           comm_x3=astr+'=create_struct('+astr+',meta)'
                           x3=execute(comm_x3)
			   ;if (qq[0] ne -1) then depdep3=alt_cdaweb_depend_3.depend_3 else depdep3=depend_3.depend_3
                           depdep3=depend_3.depend_3
			   ;if(depend_3.depend_3 ne '') then begin
                           if(depdep3[0] ne '') then begin
                             ;if (qq[0] ne -1) then comm_z3='depend_n=a.'+alt_cdaweb_depend_3.depend_3 else comm_z3='depend_n=a.'+depend_3.depend_3
                             comm_z3='depend_n=a.'+depend_3.depend_3
			     z3=execute(comm_z3) 
                             ;if (qq[0] ne -1) then meta_n=create_struct(alt_cdaweb_depend_3.depend_3,depend_n) else meta_n=create_struct(depend_3.depend_3,depend_n)
                             meta_n=create_struct(depend_3.depend_3,depend_n)
			     comm_x4=astr+'=create_struct('+astr+',meta_n)'
                             x4=execute(comm_x4)
                          endif
			endif  ; if already in struct  
                     endif
                  endif     
                  ;
               endif
	    endif  ; if var not already in struct   
         endfor   ; end i 
      endif
   endfor   ; end k
   ;
   for k=0, dcn-1 do begin
      astr='a'+strtrim(k,2) 
      dp0=strtrim(depend0[k],2)
      comm_x3='temp=create_struct(dp0,'+astr+')'
      x3=execute(comm_x3)
      ret_str=create_struct(ret_str,temp)
   endfor   ; end k
endelse

; Free Memory
spdf_delete, data
spdf_delete, meta
spdf_delete, astr
spdf_delete, temp 

return, ret_str 
end
