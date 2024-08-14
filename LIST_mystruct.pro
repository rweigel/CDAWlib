;+
; NAME:	remove_excl_pnt 
;
; PURPOSE: remove !N, !E, etc from string, so list labels look better
;           Initially written for RBSP data units. 
;
; INPUT: units - string 
;
; OUTPUT: units - cleaned up string
;
; Example: newunits = remove_excl_pnt('s!E-1!Ncm!E-2!Nster!E-1!NkeV!E-1!N')
;  
FUNCTION remove_excl_pnt, units
res=strsplit(units,'!',/extract)
units=''  ; reset 'units'
for i=0,n_elements(res)-1 do begin 
   res1=strmid(res[i],0,1)  ;  get first character of each part
   case 1 of
     (res1 eq 'N') or (res1 eq 'n'): begin
        len=strlen(res[i])
        units=units+''+strmid(res[i],1,len)
        end
     (res1 eq 'E') or (res1 eq 'e'): begin
	len=strlen(res[i])
	units=units+'^('+strmid(res[i],1,len)+')'
	end
     (res1 eq 'I') or (res1 eq 'i'): begin
	len=strlen(res[i])
	units=units+'_('+strmid(res[i],1,len)+')'
        end
     (res1 eq 'U') or (res1 eq 'u'): begin
	len=strlen(res[i])
	units=units+'^('+strmid(res[i],1,len)+')'
	end
     (res1 eq 'D') or (res1 eq 'd'): begin
	len=strlen(res[i])
	units=units+'_('+strmid(res[i],1,len)+')'
	end
     else: units=units+res[i]
   endcase
endfor
return, units
end
;-
;--------------------------------------------------------------------
;+
; NAME:	ffo_string - substitute for string()
;
; PURPOSE: allows override of free format I/O specifications 
;
; INPUT: format - a format specification, value - a value to be string'ed
;
; Examples: newstring = ffo_string( 'F10.2', 354.9985 )
;           newstring = ffo_string( struct.format, struct.dat )
;
; NOTE: this function wraps the format string in parenthesis
;
; original version - JWJ 08/08/2000
;
FUNCTION ffo_string, format, value

  ; First, if format is defined, just use it against the value
  ; and return the result
  if strlen( format ) gt 0 then begin
    return, string( FORMAT = '(' + format + ')', value )
  endif

  ; Here's the original reason this function was developed.
  ; If the format is not defined and the data type
  ; is FLOAT, use F13.6 instead of the IDL 5.2 free format 
  ; specifier of G13.6 which is causes us particular problems
  if size( value, /type ) eq 4 then begin
    return, string( FORMAT = '(F13.6)', value )
  endif

  ; At last, if no particular rules were met for overriding the
  ; format specifcation, use the free format I/O
  return, string( value )

end ; ffo_string

;----------------------------------------------------------------------------
; Function check_binning_status 
;
; Checks if the data set is the result of the time based binning software.
; Returns 1 if the data has been binned, 0 otherwise.

; Added by Ron Yurow (Nov 4, 2020)

FUNCTION check_binning_status, a

tags = TAG_NAMES (a)
n_var = N_TAGS (a)

s1_found = 0 ; True when any variable ends in '_BIN'
s2_found = 0 ; True when any variable ends in '_NBIN'

FOR i = 0, n_var - 1 DO BEGIN

    IF  STRPOS (tags [i], '_BIN', 4,  /REVERSE_OFFSET) ne -1 THEN s1_found = 1
    IF  STRPOS (tags [i], '_NBIN', 5,  /REVERSE_OFFSET) ne -1 THEN s2_found = 1

    IF  s1_found eq 1 && s2_found eq 1 THEN RETURN, 1

ENDFOR

RETURN, 0

END

;----------------------------------------------------------------------------
; Function compress_format_str

; Compress a format string so that it uses fewer format elemements by combining consecutive
; requests for white space into a single element.

; Added by Ron Yurow (Nov 19, 2020)

FUNCTION compress_format_str, bloat

str = bloat

m = 0
n = 0

WHILE (1) DO BEGIN

    ; Remove white space
    str = STRCOMPRESS (str, /REMOVE_ALL)

    ; Look for compressable format elements  
    match = STREGEX (str, '[0-9]+[Xx],[0-9]+[Xx]', LENGTH=len) 

    ; Check if no more compression is possible.
    IF len [0] eq -1 THEN RETURN, str

    ; Read the two repeat counts for the X format codes.
    READS, STRMID (str, match [0], len [0]), m, n, FORMAT = '(I0,"X,",I0,"X")'

    rewrite = BYTARR (len)

    rewrite [*] = 32 ; space character

    rewrite = STRING (rewrite)

    STRPUT, rewrite, STRING ( m + n, FORMAT='(I0,"X")')

    STRPUT, str, rewrite, match [0]

ENDWHILE

END

;----------------------------------------------------------------------------
;+                                                                            
; NAME: reform_strc.pro
;
; PURPOSE: Reforms the data array from a (1,N) to a (N).
;
; astrc    -  Input structure
;
FUNCTION reform_strc, astrc
istr=0
namest=tag_names(astrc)
ns_tags=n_tags(astrc)

for k=0, ns_tags-1 do begin
   tagname=namest[k]
   names=tag_names(astrc.(k))
   ntags=n_tags(astrc.(k))
   mc=where((names eq 'VAR_NOTES'),nc)
   for j=0, ntags-1 do begin
      if(names[j] eq 'DAT') then begin
         asize=size(astrc.(k).dat)
         if(asize[0] gt 0) then newdata=reform(astrc.(k).dat) else $
                              newdata=astrc.(k).dat
         tempa=create_struct('DAT',newdata)
         tempb=create_struct(tempb,tempa)
      endif else begin
         str_p=astrc.(k).(j)
         if(j eq 0) then begin
            tempb=create_struct(names[j],str_p)
         endif else begin
            tempa=create_struct(names[j],str_p)
            tempb=create_struct(tempb,tempa)
         endelse
      endelse
   endfor  ; end j
   ; Add VAR_NOTES to each variable that does not have this attribute
   if(mc[0] lt 0) then begin
      tempa=create_struct('VAR_NOTES','')
      tempb=create_struct(tempb,tempa)
   endif
   ; Add each variable to the overall structure
   if(istr eq 0) then begin
      temp2=create_struct(namest[k],tempb)
      b=create_struct(temp2)
   endif else begin
      temp2=create_struct(namest[k],tempb)
      b=create_struct(b,temp2)
   endelse
   istr=istr+1
endfor    ; end k

; Free Memory
spdf_delete, tempa
spdf_delete, tempb
spdf_delete, temp2

return, b
end
;
;12/13/2006 - TJK moved parse_mydepend0 out of this file to its own
;file (w/ same name so that it can be called by read_myCDF.pro
;

;----------------------------------------------------------------------------
;+
; NAME: reform_mystruc.pro
;
; PURPOSE: Reforms the data array from a (i,j,k) to a (i*j,k) and (i,j,k,l) to a (i*j*k,l)
;
; astrc    -  Input structure

FUNCTION reform_mystruct, astrc

istr=0
namest=tag_names(astrc)
ns_tags=n_tags(astrc)

for k=0, ns_tags-1 do begin
   sz=size(astrc.(k).dat)
   names=tag_names(astrc.(k))
   ntags=n_tags(astrc.(k))
   ;
   ;
   case sz[0] of
     3: begin
      tagname=namest[k]
      newsz=sz[1]*sz[2]
      newdata=reform(astrc.(k).dat,newsz,sz[3])
      ; RCJ 08/27/2015  Attach var_notes, don't just replace them.
      astrc.(k).var_notes='ListImage. ' + astrc.(k).var_notes
      for j=0, ntags-1 do begin
         if(names[j] eq 'DAT') then begin
            tempa=create_struct('DAT',newdata)
            tempb=create_struct(tempb,tempa)
         endif else begin
            tag_cmd='str_p=astrc.'+tagname+'.(j)' 
            if(not execute(tag_cmd)) then begin
               print, 'ERROR=Execute error setting tag_cmd'
               close,1
               return, -1
            endif
            if(j eq 0) then begin
               tempb=create_struct(names[j],str_p)
            endif else begin
               tempa=create_struct(names[j],str_p)
               tempb=create_struct(tempb,tempa)
            endelse
         endelse
      endfor   ; end j
      temp2=create_struct(namest[k],tempb)
      b=create_struct(b,temp2)
     end
     4: begin
      tagname=namest[k]
      newsz=sz[1]*sz[2]*sz[3]
      newdata=reform(astrc.(k).dat,newsz,sz[4])
      ; RCJ 08/27/2015  Attach var_notes, don't just replace them.
      astrc.(k).var_notes='ListImage3D. ' + astrc.(k).var_notes
      for j=0, ntags-1 do begin
         if(names[j] eq 'DAT') then begin
            tempa=create_struct('DAT',newdata)
            tempb=create_struct(tempb,tempa)
         endif else begin
            tag_cmd='str_p=astrc.'+tagname+'.(j)' 
            if(not execute(tag_cmd)) then begin
               print, 'ERROR=Execute error setting tag_cmd in reform_mystruct'
               close,1
               return, -1
            endif
            if(j eq 0) then begin
               tempb=create_struct(names[j],str_p)
            endif else begin
               tempa=create_struct(names[j],str_p)
               tempb=create_struct(tempb,tempa)
            endelse
         endelse
      endfor   ; end j
      temp2=create_struct(namest[k],tempb)
      b=create_struct(b,temp2)
      ;
     end
     else: begin
      if(istr eq 0) then begin
         b=create_struct(namest[k],astrc.(k))
      endif else begin
         temp=create_struct(namest[k],astrc.(k))
         b=create_struct(b,temp)
      endelse
     end
   endcase 
   istr=istr+1
endfor

; Free Memory
spdf_delete, tempa
spdf_delete, tempb
spdf_delete, temp

return, b
end

;----------------------------------------------------------------------------
;+
; NAME: ord_mystruc.pro
;
; PURPOSE: Reorders the given structure so that the dimension of the data 
;          variables is increasing w/ each entry. 
;
;   astrc  -  Input structure
;   vorder -  An array of the dimension of each variable in the structure
;
;  RCJ 04/24/2008 Before the structure is reordered,
;      look for vars w/ uncertainties associated w/ them, create
;      and index and reorder the structure according to this index. 
;      This will make var and uncertainty be listed side by side. 

FUNCTION ord_mystruct, astrc, vorder, is

vlen=n_elements(vorder)
vmax=max(vorder)
istr=0
names=tag_names(astrc)
;
; RCJ 04/24/2008
; Reorder names so that uncertainties go right next to their respective vars.
; Note: astrc is not being reordered!  only names!  So we also need 'order'
;
nnames=[names[0]]  ; Start w/ Epoch.
order=0            ; Position of Epoch in astrc
for i=1,n_elements(names)-1 do begin
   q=where(nnames eq names[i])
   if q[0] eq -1 then begin  ;  Avoid repeating vars already listed
      nnames=[nnames,names[i]]
      q=where(names eq names[i])
      order=[order,q[0]]
      qq=where(tag_names(astrc.(i)) eq 'DELTA_PLUS_VAR')
      qqq=where(tag_names(astrc.(i)) eq 'DELTA_MINUS_VAR')
      if qq[0] ne -1 and qqq[0] ne -1 then begin
         if astrc.(i).delta_plus_var ne '' then begin
            q=where(names eq strupcase(astrc.(i).delta_plus_var))
	    qq=where(nnames eq strupcase(astrc.(i).delta_plus_var))
	    ;if q[0] ne -1 then begin  ; if, for whatever reason, this var is not
	    if (q[0] ne -1 and qq[0] eq -1) then begin  ; if, for whatever reason, this var is not
	       ;  included in the input structure, then skip it; or if it's already
	       ; in nnames due to another var
               nnames=[nnames,strupcase(astrc.(i).delta_plus_var)]
               order=[order,q[0]]
	    endif
         endif
         if astrc.(i).delta_minus_var ne '' and $
            astrc.(i).delta_minus_var ne astrc.(i).delta_plus_var then begin
            q=where(names eq strupcase(astrc.(i).delta_minus_var))
	    qq=where(nnames eq strupcase(astrc.(i).delta_minus_var))
            ;if  q[0] ne -1 then begin  ; if, for whatever reason, this var is not
            if (q[0] ne -1 and qq[0] eq -1) then begin  ; if, for whatever reason, this var is not
	       ;  included in the input structure, then skip it; or if it's already
	       ; in nnames due to another var
	       nnames=[nnames,strupcase(astrc.(i).delta_minus_var)]
               order=[order,q[0]]
	    endif  
         endif
      endif
   endif      
endfor
;
;   Need to reorder vorder:
vorder=vorder[order]
; RCJ 07/10/2013  RBSP test revealed that 'names' should be ordered too:
names=names[order]
;
for k=is, vmax do begin
   for i=0, vlen-1  do begin
      if(vorder[i] eq k) then begin
         if(istr eq 0) then begin
            bnew=create_struct(names[i],astrc.(i))
         endif else begin
            temp=create_struct(names[i],astrc.(order[i]))
            bnew=create_struct(bnew,temp)
         endelse
         istr=istr+1
      endif
   endfor   ; end i
endfor   ; end k
;
; Free Memeory
spdf_delete, temp

return, bnew
end
;
;----------------------------------------------------------------------------
;
FUNCTION dependn_search,a,i,d
;
; INPUT: a - data structure
;	 i - index of variable for which we want the values of depend_n
;        d - which depend? 1? 2? 3?
; OUTPUT: array of depend_n values
;
; Establish error handler
catch, error_status
if(error_status ne 0) then begin
   print, 'STATUS= Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (dependn_search).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   close, 1
   return, -1
endif

depn_values=''
tmp_stuff=''
dependn=''    
;
case d of
   1:begin
      q=where(tag_names(a.(i)) eq 'DEPEND_1')
      if (q[0] ne -1) then dependn=a.(i).depend_1
      ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
      q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_1')
      if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_1 ne '') then dependn=a.(i).alt_cdaweb_depend_1
     end
   2:begin
      q=where(tag_names(a.(i)) eq 'DEPEND_2')
      if (q[0] ne -1) then dependn=a.(i).depend_2
      ; RCJ 05/16/2013  If alt_cdaweb_depend_2 exists, use it instead:
      q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_2')
      if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_2 ne '') then dependn=a.(i).alt_cdaweb_depend_2
     end 
   3:begin
      q=where(tag_names(a.(i)) eq 'DEPEND_3')
      if (q[0] ne -1) then dependn=a.(i).depend_3
      q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_3')
      if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_3 ne '') then dependn=a.(i).alt_cdaweb_depend_3
     end 
endcase
if (dependn[0] ne '') then s=execute('tmp_stuff= a.'+dependn+'.DAT')
if (dependn[0] ne '') then begin
   s=execute('thisformat = a.'+dependn+'.FORMAT')
   if s eq 0 then thisformat=''
endif   
if (dependn[0] ne '') then s=execute('dep_fill= a.'+dependn+'.FILLVAL')
if string(tmp_stuff[0]) ne '' then begin      
   size_tmp=size(tmp_stuff)
   case size_tmp[0] of
      1: begin
            if thisformat eq '' then depn_values=strtrim(tmp_stuff,2) else $
               depn_values=strtrim(string(tmp_stuff,format='('+thisformat+')'),2)
         end
      2: begin
            ; RCJ 12/01  As far as I know, there are 2 types of 2D depend matrices.
            ; Taking, for example angles and a 3x3 matrix:
            ; a=[[90,180,270],[90,180,270],[90,180,270]] is one possible arrangement, but
            ; a=[[90,90,90],[180,180,180],[270,270,270]] is also an arrangement I've seen.
            ; So this code is checking for these 2 types. Taking the first and second
            ; rows, the first is compared to the second using 'match'. If the elements
            ; do not match (second case, count=0) our depend array is column 0 of 
            ; the original matrix. If all elements match (first case, 
            ; count=n_elements(one of the rows) then our depend array is row 0 of
            ; the original matrix. For a case other than these 2 listing the data
            ; becomes a more difficult task. I would have to think about it....
            ;ts1=tmp_stuff[*,0]
            ;ts2=tmp_stuff[*,1]
	    ; Above is the old ts1 and ts2. Now I'm trying to avoid
	    ; comparing arrays that contain fillval (hopefully not *all* of 
	    ; them do!) :  RCJ 02/21/02
	    ; The "if j lt size_tmp(2)" tests are used in case there are fillvals in
	    ; all of the rows and we don't want to run out of rows to test. RCJ 09/25/02
            j=0L
	    test_ts1:
	    q=where(tmp_stuff[*,j] eq dep_fill)
	    if q[0] ne -1 then begin
	       j=j+1L
	       if j lt size_tmp[2]-1 then goto, test_ts1
	    endif   
            ts1=tmp_stuff[*,j]
	    if j lt size_tmp[2]-1 then j=j+1L
	    test_ts2:
	    q=where(tmp_stuff[*,j] eq dep_fill)
	    if ((q[0] ne -1) and (j lt size_tmp[2]-1)) then begin
	       j=j+1L
	       goto, test_ts2
	    endif
            ts2=tmp_stuff[*,j]
	    ;
	    ; Test one array against the other:
              ; RCJ 06/16/2008  Moved and commented out what we had here to end of function.
	      ;  If this test doesn't work we can bring it back.
	    ; tolerance of 5 units ok?  testing only 3 first elements ok? can do better algorithm.
	    tol=5
	      ; RCJ 06/23/2011  Comparing only 2 first elements because of [2,4300], var:HETCNOFlux,
              ;                 dataset: sta_lb_impact
	    if (((ts1[0] le ts2[0]+tol) or (ts1[0] ge ts2[0]-tol)) or $
	       ((ts1[1] le ts2[1]+tol) or (ts1[1] ge ts2[1]-tol))) then begin
	       ;((ts1[3] le ts2[3]+tol) or (ts1[3] ge ts2[3]-tol))) then begin
	       tmp_stuff=tmp_stuff[*,0]  
	    endif else tmp_stuff=tmp_stuff[*,0]
            if thisformat eq '' then depn_values="~"+strtrim(tmp_stuff,2) else $
            depn_values="~"+strtrim(string(tmp_stuff,format='('+thisformat+')'),2)
         end
   endcase      
endif 
;
return, depn_values
end


;-------------------------------------------------------------------------------------------------

FUNCTION label_search,a,sz,i,k,debug=debug
;
;  INPUT: a - data structure
;	  sz - dimension of a(i) variable. First element of output of size().
;	  i - index of variable for which we want the label
;	  k - index of array element for which we want the label
;
;  OUTPUT: element k of label array for variable a.(i)
;
catch, error_status
if(error_status ne 0) then begin
   print, 'STATUS= Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (label_search).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   close, 1
   return, -1
endif

label=''
len=size(a.(i).dat)
lent=size(a.(0).dat)
length=lent[lent[0]+2]

case sz of

  0: begin
   if(length eq 1 and len[0] eq 0 and len[1] gt 1) then begin
      if(a.(i).lablaxis ne '') then label=strupcase(a.(i).lablaxis) else begin 
                   lab=strupcase(a.(i).labl_ptr_1)
         label=lab[k]
      endelse
   endif else begin
      if(a.(i).lablaxis eq '') then label=strupcase(a.(i).fieldnam) else $
      label=strupcase(a.(i).lablaxis)
   endelse
  end

  1: begin
   if(length eq 1 and len[0] eq 1 and len[1] gt 1) then begin
      if(a.(i).lablaxis ne '') then label=strupcase(a.(i).lablaxis) else begin 
                   lab=strupcase(a.(i).labl_ptr_1)
         label=lab[k]
      endelse
   endif else begin
      if(a.(i).lablaxis eq '') then label=strupcase(a.(i).fieldnam) else $
      label=strupcase(a.(i).lablaxis)
   endelse
  end

  2: begin
   if((strcmp(a.(i).var_notes,'ListImage',9)) eq 1 or (strcmp(a.(i).var_notes,'ListImage3D',11)) eq 1 ) then begin     
      if(a.(i).lablaxis ne '') then label=a.(i).lablaxis else $
                   label=a.(i).fieldnam
   endif
   if(strcmp(a.(i).var_notes,'ListImage',9) eq 0) then begin
      ;assign lab to lablaxis initially
      if (tagindex('LABLAXIS',tag_names(a.(i))) ne -1) then begin
         lab=strupcase(a.(i).lablaxis)
      endif
      ;if labl_ptr_1 existing and isn't blank then use it instead
      if (tagindex('LABL_PTR_1',tag_names(a.(i))) ne -1) then begin
       if(a.(i).labl_ptr_1(0) ne '') then lab=strupcase(a.(i).labl_ptr_1) 
      endif
    
      if(lab[0] ne '') then begin
         if (n_elements(lab) gt 1) then label=lab[k] else label=lab[0]
      endif else begin
         ;
         depend1=strupcase(a.(i).depend_1)
         ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
         q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_1')
         if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_1 ne '') then depend1=a.(i).alt_cdaweb_depend_1
	 ;
         ; RTB added code 3/98
         temp_names=tag_names(a)
         z=tagindex(depend1,temp_names)
         if(z[0] ne -1) then begin
	    lab=a.(z[0]).labl_ptr_1
           if(lab[0] eq '') then begin
            lab=ffo_string(a.(z[0]).format, a.(z[0]).dat[k])
            label=strtrim(lab[0],2)
           endif else begin
            label=strtrim(lab[k],2)
           endelse
	 endif  ; z[0] ne -1
      endelse   
   if label eq '' then begin
     label= 'Label not found'
     if keyword_set(DEBUG) then print,'Warning:  Label not found for var ',a.(i).varname
   endif  
   endif
  
  end

  4: begin
      if(a.(i).lablaxis ne '') then label=a.(i).lablaxis else $
                   label=a.(i).fieldnam

      ;assign lab to lablaxis initially
      if (tagindex('LABLAXIS',tag_names(a.(i))) ne -1) then begin
         lab=strupcase(a.(i).lablaxis)
      endif
      ;if labl_ptr_1 existing and isn't blank then use it instead
      if (tagindex('LABL_PTR_1',tag_names(a.(i))) ne -1) then begin
       if(a.(i).labl_ptr_1(0) ne '') then lab=strupcase(a.(i).labl_ptr_1) 
      endif
  end

endcase
label=strjoin(strsplit(label,/extract),'_')
COMMON SHARE, csv1
if (csv1 GT 0) then label = STRJOIN(STRSPLIT(label,',',/EXTRACT),'_') ;CWG 04/06/18, added label change here to replace ',' in label values
return, label
end

;-------------------------------------------------------------------------------------------------

FUNCTION unit_search,a,sz,i,k

;  INPUT: a - data structure
;	  sz - dimension of a(i) variable. First element of output of size().
;	  i - index of variable for which we want the unit
;	  k - index of array element for which we want the unit
;
;  OUTPUT: element k of label array for variable a.(i)
;
; Establish error handler
catch, error_status
if(error_status ne 0) then begin
   print, 'STATUS= Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (unit_search).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   close, 1
   return, -1
endif

; RCJ 02/06/2003  Was going to call this var 'unit' but the name is already used: "printf, unit,.."
; so I'm calling it 'units':
units=''
len=size(a.(i).dat)
lent=size(a.(0).dat)
length=lent[lent[0]+2]

if(sz le 1) then begin  
   ; RCJ 17Nov2021 Applying the same test as for (sz eq 2):
   if(a.(i).units ne '') then begin
     unts=a.(i).units 
   endif else begin
      z=tagindex('unit_ptr',tag_names(a.(i))) 
      if z[0] ne -1 then unts=a.(i).unit_ptr else unts=''
   endelse   
   if (n_elements(unts) gt 1) then units=unts[k] else units=unts[0]
endif

if(sz eq 2) then begin
   if((strcmp(a.(i).var_notes,'ListImage',9)) eq 1 or (strcmp(a.(i).var_notes,'ListImage3D',11)) eq 1 ) then begin     
      if(a.(i).units ne '') then units=a.(i).units
   endif
   if((strcmp(a.(i).var_notes,'ListImage',9)) eq 0 and (strcmp(a.(i).var_notes,'ListImage3D',11)) eq 0 ) then begin      ;
      ;
      if(a.(i).units ne '') then begin
         unts=a.(i).units 
      endif else begin
         ; RCJ 02/25/2004 'units' or 'unit_ptr' are *required* var attributes according to the
         ; ISTP guidelines but some cdfs come w/ neither of them, so here's the same
         ; test as above:
         z=tagindex('unit_ptr',tag_names(a.(i))) 
         if z[0] ne -1 then unts=a.(i).unit_ptr else unts=''
      endelse   
      if (n_elements(unts) gt 1) then units=unts[k] else units=unts[0]
   endif
endif

if(sz eq 4) then begin

      if(a.(i).units ne '') then begin
         unts=a.(i).units 
      endif else begin
         ; RCJ 02/25/2004 'units' or 'unit_ptr' are *required* var attributes according to the
         ; ISTP guidelines but some cdfs come w/ neither of them, so here's the same
         ; test as above:
         z=tagindex('unit_ptr',tag_names(a.(i))) 
         if z[0] ne -1 then unts=a.(i).unit_ptr else unts=''
      endelse 
      units=unts
endif

if strcmp('dd-mm-yyyy',units,10,/fold_case) eq 0 then $  ;  RCJ added this line at Bob's request. see email from 8/22/2011
units=strjoin(strsplit(units,/extract),'_')
if strpos(units,'!') ne -1 then units=remove_excl_pnt(units)

return, units
end

;-----------------------------------------------------------------------------------
FUNCTION list_header, a, unit, ntags

; Establish error handler
catch, error_status
if(error_status ne 0) then begin
   print, 'STATUS= Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (list_header).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   close, 1
   return, -1 
endif
status=0

COMMON SHARE, csv1

if (csv1 LE 1) then begin
  printf, unit, format='("#",14x,"************************************")'
  printf, unit, format='("#",14x,"****  RECORD VARYING VARIABLES  ****")'
  printf, unit, format='("#",14x,"************************************")'
  printf, unit, format='("#",14x)'
endif else begin
  COMMON SHARE1, jsonstruct
  jsontag = 'VARIABLE INFO'
  jsonstruct = create_struct(jsontag, 'RECORD VARYING VARIABLES')
endelse
 
ii=0
for i=0L, ntags-5 do begin
   
   if (strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY')) then begin
      ii=ii+1
      if(n_elements(a.(i).catdesc) eq 0) then begin
         if (csv1 LE 1) then begin 
          printf,unit, format='("# ",i2,". ",a)', ii, a.(i).fieldnam
         endif else begin
          jsontag = a.(i).varname          
          dummystruct = create_struct(jsontag, a.(i).fieldnam)    
          jsonstruct = create_struct(jsonstruct, dummystruct)
         endelse
      endif else begin
         if(strlen(a.(i).catdesc) eq 0) then begin
            if (csv1 LE 1) then begin
              printf,unit, format='("# ",i2,". ",a)', ii, a.(i).fieldnam
            endif else begin
              jsontag = a.(i).varname
              dummystruct = create_struct(jsontag, a.(i).fieldnam)
              jsonstruct = create_struct(jsonstruct, dummystruct)
            endelse
         endif else begin
            if (csv1 LE 1) then begin
             printf,unit, format='("# ",i2,". ",a)', ii, a.(i).catdesc
           endif else begin
             jsontag = a.(i).varname
             dummystruct = create_struct(jsontag, a.(i).catdesc)
             jsonstruct = create_struct(jsonstruct, dummystruct)
           endelse            
         endelse
      endelse
      if (csv1 LE 1) then begin
        if ((a.(i).var_notes ne '') and (a.(i).var_notes ne ' ')) then printf,unit, format='("#       NOTES:  ",a)', a.(i).var_notes
      endif else begin
        if ((a.(i).var_notes ne '') and (a.(i).var_notes ne ' ')) then begin
          jsontag = a.(i).varname+'_NOTES'
          dummystruct = create_struct(jsontag, a.(i).var_notes)
          jsonstruct = create_struct(jsonstruct, dummystruct)
        endif
      endelse
      
   endif
   if((strcmp(a.(i).var_notes,'ListImage',9)) eq 1 or (strcmp(a.(i).var_notes,'ListImage3D',11)) eq 1 ) and $   
     ((strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY'))) then begin 
      depend1=a.(i).depend_1
      depend2=a.(i).depend_2
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1) then depend3=a.(i).depend_3
      ; RCJ 05/16/2013  If alt_cdaweb_depend_1 and 2 exist, use those instead:
      q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_1')
      if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_1 ne '') then depend1=a.(i).alt_cdaweb_depend_1 
      q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_2')
      if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_2 ne '') then depend2=a.(i).alt_cdaweb_depend_2 
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1) then begin
         q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_3')
         if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_3 ne '') then depend3=a.(i).alt_cdaweb_depend_3 
      endif
      ;
      temp_names=tag_names(a)
      z=tagindex(depend1,temp_names)
      ;
      ; RCJ 05/24/2012 If a.(z[0]).dat does not exist, the execute command
      ;   a few lines further down will fail and throw out an error.
      ;   Let's try to make dep?_cmd="" and same for frm?_cmd
      ;
      if(z[0] ne -1) then dep1_cmd='dep1=a.(z[0]).dat' else dep1_cmd='dep1=""'
      if(z[0] ne -1) then frm1_cmd='frm1=a.(z[0]).format' else frm1_cmd='frm1=""'
      if(not execute(dep1_cmd)) then print, "ERROR=Error setting dep1"
      if(not execute(frm1_cmd)) then print, "ERROR=Error setting frm1"
      ; RCJ 12/99 changed z -> zz, otherwise energy and angle will have 
      ;the same values when the commands are executed
      ;
      zz=tagindex(depend2,temp_names)
      if(zz[0] ne -1) then dep2_cmd='dep2=a.(zz[0]).dat' else dep2_cmd='dep2=""'
      if(zz[0] ne -1) then frm2_cmd='frm2=a.(zz[0]).format' else  frm2_cmd='frm2=""'
      if(not execute(dep2_cmd)) then print, "ERROR=Error setting dep2"
      if(not execute(frm2_cmd)) then print, "ERROR=Error setting frm2"
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1 ) then begin
         zz=tagindex(depend3,temp_names)
         if(zz[0] ne -1) then dep3_cmd='dep3=a.(zz[0]).dat' else dep3_cmd='dep3=""'
         if(zz[0] ne -1) then frm3_cmd='frm3=a.(zz[0]).format' else  frm3_cmd='frm3=""'
         if(not execute(dep3_cmd)) then print, "ERROR=Error setting dep3"
         if(not execute(frm3_cmd)) then print, "ERROR=Error setting frm3"
      endif
      len1=string(strlen(dep1[0])+1)
      len2=string(strlen(dep2[0])+1)
      sz1=size(dep1)
      sz2=size(dep2)
      ln1=strtrim(sz1[sz1[0]+2],2)
      ln2=strtrim(sz2[sz2[0]+2],2)
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1 ) then begin
         len3=string(strlen(dep3[0])+1)
         sz3=size(dep3)
         ln3=strtrim(sz3[sz3[0]+2],2)
      endif
      frm1=ln1+'a'+len1
      frm2=ln2+'a'+len2
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1 ) then frm3=ln3+'a'+len3
      form1='("# depend_1 is ",a,": [",'+frm1+',"]")'
      form2='("# depend_2 is ",a,": [",'+frm2+',"]")'
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1 ) then form3='("# depend_3 is ",a,": [",'+frm3+',"]")'
      if (csv1 LE 1) then begin        
        printf, unit, format=form1, depend1,dep1
        printf, unit, format=form2, depend2,dep2
      endif else begin
        jsontag = 'depend1_' + a.(i).depend_1
        dummystruct = create_struct(jsontag, dep1)
        ;TJK add check to see if this tag already exists
        match = where(strupcase(jsontag) eq tag_names(jsonstruct), nmatches)
        if (nmatches eq 0) then jsonstruct = create_struct(jsonstruct, dummystruct)
        jsontag = 'depend2_' + a.(i).depend_2
        dummystruct = create_struct(jsontag, dep2)
        ;TJK add check to see if this tag already exists
        match = where(strupcase(jsontag) eq tag_names(jsonstruct), nmatches)
        if (nmatches eq 0) then jsonstruct = create_struct(jsonstruct, dummystruct)
      endelse
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1) then begin
         ; RCJ 26Aug2021  Added this test, don't want to print out in all cases of csv
         if (csv1 LE 1) then printf, unit, format=form3, depend3,dep3
      endif	 
      ;TJK changed this upon Bob's request since its not an accurate statement...
      ;      printf, unit, format='("Format: [1st depend_1 x M depend_2s, 2nd depend_1 x M depend_2s, ... Nth depend_1 x M depend_2s]")'
      if (strcmp(a.(i).var_notes,'ListImage3D',11) eq 1 ) then begin
         if (csv1 LE 1) then printf, unit, format='("# Order of values for this variable in each row:  values at each depend_1 for the 1st depend_2, for the 1st depend_3; values at each depend_1 for the 1st depend_2, for the 2nd depend_3, ..., values at each depend_1 for the each depend_2, for the last depend_3")' 
      endif else begin
         if (csv1 LE 1) then printf, unit, format='("# Order of values for this variable in each row:  values at each depend_1 for the 1st depend_2, values at each depend_1 for the 2nd depend_2, ..., values at each depend_1 for the last depend_2")'
      endelse
      if (csv1 LE 1) then printf,unit,format='("#",14x)'
   endif
endfor   ; end i
if (csv1 LE 1) then printf,unit,format='("#",14x)'

return, status
end

;----------------------------------------------------------------------------------------------
FUNCTION ex_prt, unit, var, var2, slen, k 

;  This function is called when listing global attributes.
;  This function will break a long line at 75 char long.
;    If line is first of an attribute w/ many lines (k=0),
;    then the attr name is written, then it's value.   For subsequent lines only
;    the value is written.

; Establish error handler
catch, error_status
if(error_status ne 0) then begin
   print, 'STATUS = Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (ex_prt).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   close, 1
   return, -1 
endif

; special characters that should be removed prior to printing.
; Ron Yurow (May 10, 2022)
ch_remove = [7b, 8b, 9b, 10b, 11b, 12b, 13b, 27b]
; RCJ Dec2023: 7b: Bell, 8b: Backspace, 9b: tab/horizontal tab, 10b: newline/linefeed, 
;              11b: vertical tab, 12b: formfeed, 13b: carriage return, 27b: escape 

status=0
icnt=0
output='                                                                                                      '
flg=0
for i=0L, slen-1 do begin
   ch=strmid(var2,i,1)

   ; Check for a forbidden control character (IE. new line, form feed).  Convert these to spaces.
   ; Ron Yurow (May 10, 2022)
   if  ((where ((byte (ch)) [0] eq ch_remove)) [0] ne -1) THEN ch = ' '

   if(icnt lt 75) then begin
      strput,output,ch,icnt  
   endif else begin
      if(ch eq ' ') then begin
         if(k eq 0) then begin
            ;printf, unit, format='("#",5x,a,2x,a)', var, output
	    if flg eq 0 then begin
	       printf, unit, format='("#",5x,a,2x,a)', var, output
	       flg=1
	    endif else begin
	       printf, unit, format='("#",37x,a)', output
	    endelse    
         endif else begin
            printf, unit, format='("#",37x,a)', output 
         endelse
         icnt=0
         output='                                                                                                       '
      endif else strput,output,ch,icnt
   endelse
   icnt=icnt+1 
endfor
if(icnt gt 1) then printf, unit, format='("#",37x,a)', output 

return, status
end

;------------------------------------------------------------------------------------------------
;+
; NAME: wrt_hybd_strct.pro
;
; PURPOSE: Prints ascii file of RV or NRV variables
;
FUNCTION wrt_hybd_strct, a, unit, convar, maxrecs, depend0, mega_num, csv, nvar

; Establish error handler
catch, error_status
if(error_status ne 0) then begin
   ;if(error_status eq -96) then $ ; This is Keyword %s not allowed in call to: %s
   ;       print, 'STATUS= This amount of data cannot be listed, please request a shorter time range' 
   ;if(error_status eq -133) then $ ; This is Temporary variables are still checked out - cleaning up...
   ;       print, 'STATUS= Incompatible variable types. Select variables separately' 
   ;if(error_status eq -124) then $ ; This is Value of %s cannot be NaN
   ;       print, 'STATUS= Temporary memory error. Please try again.'
   ;if(error_status eq -350) then $ ;  This is Unable to obtain current Julian time
   if(error_status eq -360) then $ ;  format has too many elements (idl8.7: -340)
          print, 'STATUS= Please select fewer variables.' $
          else print, 'STATUS= Data cannot be listed'
   print, 'ERROR=Error number: ',error_status,' in listing (wrt_hybd_strct).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   return, -1 
endif
 
status=0
names=strupcase(tag_names(a))
ntags=n_tags(a)
blnk='# '

;print,'convar = ',convar
case convar of
   0 : begin
       ; Check MAXRECS
       if(n_elements(num_data) eq 0) then num_data=0 
       num_data=num_data+ntags
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs 
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          text2='# WARNING: Maxrecs exceeded in Global Attributes; No. Recs. = '
          printf, unit,text,maxrecs
          printf, unit, format='(a,i)',text1,dif_rec
          printf, unit, format='(a)',blnk
          status=1
       endif
       printf, unit, format='("#",14x,"************************************")'
       printf, unit, format='("#",14x,"*****    GLOBAL ATTRIBUTES    ******")'
       printf, unit, format='("#",14x,"************************************")'
       printf, unit, format='("#",14x)'       
       for i=0L, ntags-1 do begin
          ;  RCJ 03/18/2014  Space below is arbitrarily defined so each global attr name will fit in that
	  ;       space, and could be chopped if too long.
	  ;       Needs to be increased if we find longer global attr name. You might need to make changes to ex_prt too.
          var='                              '
          var1=strtrim(names[i],2)
          strput,var,var1,0
          tstsz=size(a.(i))
          if(tstsz[0] eq 0) then begin
             var2=strtrim(a.(i),2)
	     ; RCJ 03/18/2014  Clean var2 from carriage-returns, replace w/ blank space:
	     var2=strjoin(strsplit(var2,string(10B),/extract),' ')
             slen=strlen(var2)
             if(slen gt 80) then begin
                status=ex_prt(unit,var,var2,slen,0) 
             endif else begin
                printf, unit, format='("#",5x,a,2x,a)', var, var2
             endelse
          endif else begin
            if (names[i] eq 'CDAWEB_PARENTS' or names[i] eq 'PARENTS') then begin
	     for k=0L, tstsz[1]-1 do begin
                var2=strtrim(a.(i)[k])
                   if(k eq 0) then begin
                      printf, unit, format='("#",5x,a,2x,a)', var, var2
                   endif else begin
		      if k ne tstsz[1]-1 then begin
		        var3=strtrim(a.(i)[k+1])
                        printf, unit, format='("#",37x,a,2a,a)', var2,', ',var3
		        k=k+1
		      endif else begin
                        printf, unit, format='("#",37x,a)', var2
		      endelse	
                   endelse
	     endfor
	    endif else begin ; if not 'cdaweb_parents' 
             for k=0L, tstsz[1]-1 do begin
                var2=strtrim(a.(i)[k])
                slen=strlen(var2)
                if(slen gt 80) then begin
                   status=ex_prt(unit,var,var2,slen,k) 
                endif else begin
                   if(k eq 0) then begin
                      printf, unit, format='("#",5x,a,2x,a)', var, var2
                   endif else begin
                      printf, unit, format='("#",37x,a)', var2
                   endelse
                endelse
             endfor   ; end k
	    endelse
          endelse
       endfor   ; end i
       ;
       if(num_data gt maxrecs) then begin                                       
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          text2='# WARNING: Maxrecs exceeded in Global Attributes; No. Recs. = '
          printf, unit, format='(a)',blnk
          printf, unit, text,maxrecs
          printf, unit, format='(a,i)',text1,dif_rec
          status=1
       endif
       ;
       printf, unit, format='("#",14x)'   ;'(15x)'
       if mega_num gt 1 then printf, unit,'# **************************************************************************************'
       if mega_num gt 1 then printf, unit,'# *********    There is more than one Epoch for the variables selected    **************'
       if mega_num gt 1 then printf, unit,'# *********    Please scroll down                                         **************'
       if mega_num gt 1 then printf, unit,'# **************************************************************************************'
       if mega_num gt 1 then printf, unit, format='("#",14x)'    ;'(15x)'
   end  ; end case 0
   ;
   ; Record Varying Variables 
   ;
   1 : begin

       ; Check MAXRECS
       if(n_elements(num_data) eq 0) then num_data=0
       ; Put in appropriate record count
       len=size(a.(0).dat)
       length=len[len[0]+2]
       
       ; RCJ24
       if n_elements(a.(0).dat) eq 2 and (a.(0).dat[0] eq a.(0).dat[1]) then begin
         length=1
       endif	 
       
       num_data=length
       ; Check for maxrecs begin exceeded
       num_data=num_data+4
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          printf, unit, format='(a)',blnk
          status=1
          length=maxrecs
       endif
             
       status=list_header(a,unit,ntags)
       
       ; RCJ 05/12/2009   Append strings to 'labels' and 'units' instead of presetting the array sizes.
       ; Note that this first value is cut off the array after the array is populated. 
       labels=''
       units='' 
       ;
       inc=0
       for i=0L, ntags-5 do begin
          if (strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY')) then begin
	     labels=[labels,label_search(a,1,i,0,debug=debug)]
	     units=[units,unit_search(a,1,i,0)]
             ; if 'EPOCH' or 'EPOCH92' etc.
             if(names[i] eq depend0) then begin
                temp=create_struct(names[i],a.(i).dateph[0])
             endif else begin
                if(nvar eq 0) then begin
                   temp=create_struct(names[i],a.(i).dat[0]) 
                endif else begin
                   temp=create_struct(names[i],a.(i).dat[0:nvar]) 
                endelse
             endelse
             if(inc eq 0) then begin
                b=temp
             endif else begin
                b=create_struct(b,temp)
             endelse
	     inc=inc+1
	  endif   
       endfor   ; end i
       labels=labels[1:*]
       units=units[1:*]
       ; Free Memory
       spdf_delete, temp
       for j=0L, length-1 do begin
          inc=0
          for i=0L,ntags-5 do begin
             if (strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY')) then begin
                if(names[i] eq depend0) then begin
                   b.(inc)=a.(i).dateph[j]
		   inc=inc+1
                endif else begin
                   if(nvar eq 0) then begin
                      b.(inc)=a.(i).dat[j] 
                   endif else begin
                      b.(inc)=a.(i).dat[0:nvar]
                   endelse
		   inc=inc+1
                endelse
             endif		
          endfor   ; end i
                         
          ;Handle JSON file creation for csv=3
          COMMON SHARE, csv2
          if (csv2 EQ 3) AND (j EQ 0) then begin
            datsize = 0
            jdata_array = []
            for jindex1=0, (n_tags(a)-1) DO BEGIN
              if (ISA(a.(jindex1), /STRING) NE 1) then if (a.(jindex1).var_type NE 'additional_data') then begin
                if jdata_array EQ !NULL then jdata_array = [jindex1] else jdata_array = [jdata_array, jindex1] ;Variables to include in JSON listing
              endif
            endfor
            COMMON SHARE3, cjsonstruct
            COMMON SHARE4, nan_count
            datsize = size(jdata_array, /n_elements) ;Number of variables we want to include in JSON listing
            nan_count=0
            for jindex2=0, (datsize-1) DO BEGIN
              varattsreached = 'false'
              jtags = tag_names(a.(jindex2))
              counter = 0
              
              ;Check for NaN in data, then change values to ISTP Standard - CWG 07/09/2019
              if ISA(a.(jindex2).dat, /STRING) NE 1 then begin
                if (a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] NE !NULL) AND (nan_count EQ 0) then begin
                  nan_count=1      ;this sared variable will force code to add a section in the listng that will inform user that NaNs were changed for JSON
                endif

                ; Set a generic NAN replacer value using the case statement then use that replacer value for both
                ; data replacement (currently done in the case) and fillvalue substitution.
                ; Ron Yurow 
                ;typeCDF = a.(jindex2).cdftype
                ;CASE typeCDF OF
                ;  'CDF_REAL4': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_REAL8': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_FLOAT' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_DOUBLE' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_BYTE': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                ;  'CDF_INT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                ;  'CDF_INT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -32768
                ;  'CDF_INT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -2147483648
                ;  'CDF_INT8' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                ;  'CDF_UINT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 255
                ;  'CDF_UINT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 65535
                ;  'CDF_UINT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 4294967295
                ;  'CDF_EPOCH' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_EPOCH16' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_TIME_TT2000' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                ;  'CDF_CHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ; 'CDF_UCHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ;  ELSE : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ;ENDCASE

                ; NaN replacer
                nan_fillval_replace = 0

                typeCDF = a.(jindex2).cdftype
                CASE typeCDF OF
                  'CDF_REAL4': nan_fillval_replace = -1.0E+31
                  'CDF_REAL8': nan_fillval_replace = -1.0E+31
                  'CDF_FLOAT' : nan_fillval_replace = -1.0E+31
                  'CDF_DOUBLE' : nan_fillval_replace = -1.0E+31
                  'CDF_BYTE': nan_fillval_replace = -128
                  'CDF_INT1' : nan_fillval_replace = -128
                  'CDF_INT2' : nan_fillval_replace = -32768
                  'CDF_INT4' : nan_fillval_replace = -2147483648
                  'CDF_INT8' : nan_fillval_replace = -9223372036854775808
                  'CDF_UINT1' : nan_fillval_replace = 255
                  'CDF_UINT2' : nan_fillval_replace = 65535
                  'CDF_UINT4' : nan_fillval_replace = 4294967295
                  'CDF_EPOCH' : nan_fillval_replace = -1.0E+31
                  'CDF_EPOCH16' : nan_fillval_replace = -1.0E+31
                  'CDF_TIME_TT2000' : nan_fillval_replace = -9223372036854775808
                  'CDF_CHAR' : nan_fillval_replace = " "
                  'CDF_UCHAR' : nan_fillval_replace = " "
                  ELSE : nan_fillval_replace = " "
                ENDCASE

                a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = nan_fillval_replace

              endif


              for ii=0, (n_tags(a.(jindex2))-1) DO BEGIN

                if jtags[ii] EQ 'FIELDNAM' then varattsreached = 'true'

                if varattsreached EQ 'true' then begin 
		
                  ; Check to see if the FILLVAL attribute is not finite.  If it is not, then set it
                  ; to the NaN replacement value.
                  ; Ron Yurow
                  IF  jtags[ii] EQ 'FILLVAL' THEN BEGIN
                      IF  ~finite (a.(jindex2).(ii)) THEN a.(jindex2).(ii) = nan_fillval_replace
                  ENDIF
		
                  if ((a.(jindex2).cdftype EQ 'CDF_EPOCH16') || (a.(jindex2).cdftype EQ 'CDF_EPOCH') || (a.(jindex2).cdftype EQ 'CDF_TIME_TT2000')) then begin ;Convert epochs to iso-8601 for JSON files

                    if ((jtags[ii] EQ 'VALIDMIN') || (jtags[ii] EQ 'VALIDMAX') || (jtags[ii] EQ 'DAT')) then begin
		      
                      if (TYPENAME(a.(jindex2).(ii)) NE 'STRING') then begin
		      
                       epochIso=cdf_epoch_tojuldays(a.(jindex2).(ii), /string)+'Z'
		       
                      endif else epochIso=a.(jindex2).(ii)+'Z'
                      jvar = create_struct(jtags[ii], epochIso)
                    endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))

                  endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))
                  

                  if (counter EQ 0) then begin
                    varstruct = create_struct(jvar) 
                   endif else if (jtags[ii] NE 'DATEPH') then begin
                    varstruct = create_struct(varstruct, jvar)
                   endif

                  counter = counter + 1

                endif ; varattsreached

              endfor ; ii=0

              jdummystruct = create_struct(a.(jindex2).varname+'_', varstruct) 
              
              if (jindex2 EQ 0) then cjsonstruct = create_struct(jdummystruct) else cjsonstruct = create_struct(cjsonstruct, jdummystruct)
            endfor
          endif

          ;Handle CSV file creation
          if keyword_set(csv) AND (csv2 NE 3) then begin            
             labsize=n_elements(labels)
             for csvindex=0, (labsize-1) DO BEGIN
                if (j EQ 0) then begin
                   units[0] = 'yyyy-mm-ddThh:mm:ss.sssZ'
                   labunits = labels+'_'+units
                   tagname1 = 'tagg'+STRTRIM(csvindex,2)
                   if (csvindex LE (labsize-2)) then tmpvar1 = STRTRIM(labunits[csvindex],2)+',' else tmpvar1 = STRTRIM(labunits[csvindex],2)
                   tmpstruc1=create_struct(tagname1,tmpvar1)
                   if (csvindex EQ 0) then newlabs=create_struct(tmpstruc1) else newlabs=create_struct(newlabs,tmpstruc1)
                endif             
                tagname2 = 'tag'+STRTRIM(csvindex,2)
                if (csvindex LE (labsize-2)) then tmpvar2=STRTRIM(b.(csvindex),2)+',' else tmpvar2=STRTRIM(b.(csvindex),2)
                tmpstruc2= create_struct(tagname2, tmpvar2)
                if (csvindex EQ 0) then begin
                   epoch_iso = a.(0).dat[j]
                   epoch_iso=cdf_epoch_tojuldays(epoch_iso, /string)
                   epoch_iso=epoch_iso+'Z,'
                   tmpvar2=STRTRIM(epoch_iso,2)
                   tmpstruc2= create_struct(tagname2, tmpvar2)
                   newb = create_struct(tmpstruc2)
                endif else begin
                   newb = create_struct(newb, tmpstruc2)
                endelse
             endfor
             if (j EQ 0) then begin
                struclength= n_tags(newlabs)
                newform = '('+strtrim(struclength,2)+'(A))'                
                printf,unit,format=newform,newlabs
                cc = strlen(epoch_iso)
                ; Make sure that we don't write a format element 0 times, IDL
                ; does not like that.
                ; Ron Yurow (August 10, 2020)
                ; newform = '(A'+strtrim(cc,2)+','+strtrim(struclength-1,2)+'(A))'
                newform = struclength gt 1 ? '(A'+strtrim(cc,2)+','+strtrim(struclength-1,2)+'(A))' : '(A'+strtrim(cc,2)+')'
                
             endif 
             printf, unit, format=newform,newb
          endif else if (csv2 NE 3) then begin
             ;Handle the .txt print statements
             if (j EQ 0) then begin
               printf,unit,format=a.lform,labels
               printf,unit,format=a.uform,units 
             endif             
             printf,unit,format=a.dform,b
          endif

       endfor   ; end j   
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='The maximum number of records allowed to be listed is '
          text1='Your request has exceeded this maximum by '
          printf, unit, format='(a)',blnk
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          status=1                                  
       endif
       ; Free Memory
       spdf_delete, b
   end   ; end case 1
   ; 
   ; Non-Record Varying Variables 
   ;
   2 : begin
       ; Check MAXRECS
       if(n_elements(num_data) eq 0) then num_data=0
       ; Put in appropriate record count
       num_data=num_data+4    
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='The maximum number of records allowed to be listed is '
          text1='Your request has exceeded this maximum by '
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          printf, unit, format='(a)',blnk
          status=1                                  
          length=maxrecs
       endif
       ;
       printf, unit, format='("#",14x,"************************************")'
       printf, unit, format='("#",14x,"**  NON-RECORD VARYING VARIABLES  **")'
       printf, unit, format='("#",14x,"************************************")'
       printf, unit, format='("#",14x)'
   end   ; end case 2
   ;
   ; 2-D Record Varying Variables 
   ;
   3 : begin
       ;Put in a loop to determine the data sizes for each variable's data array
       ;just once instead of doing this a million times below.  We can't
       ;trust what's set in a.*.idlsize (at least for virtual variables)
       idlsizes = lonarr(ntags-4,10)
       for i = 0, ntags-5 do begin
           t_size = size(a.(i).dat)
           for j = 0, n_elements(t_size)-1 do begin
               idlsizes[i,j] = t_size[j]
           endfor
       endfor

       ; Check MAXRECS
       if(n_elements(num_data) eq 0) then num_data=0
       ; Put in appropriate record count
       ;Use the computed sizes stored in idlsizes above
       ;       len=size(a.(0).dat)
       len=idlsizes[0,*]
       length=len[len[0]+2]
       
       ; RCJ24
       if n_elements(a.(0).dat) eq 2 and (a.(0).dat[0] eq a.(0).dat[1]) then begin
         length=1
       endif	 
       
       ; Check for maxrecs begin exceeded                 
       num_data=length
       num_data=num_data+4
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          printf, unit, format='(a)',blnk
          status=1                                  
          length=maxrecs
       endif
       
       status=list_header(a,unit,ntags)
       
       num=nvar
       labels=strarr(num)
       units=strarr(num)
       dep1_values=''
       atags=tag_names(a)
       inc=0L 
       for i=0L, ntags-5 do begin
          if (strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY')) then begin
              ;TJK replace w/ computed size above to improve performance
              ;st_sz=size(a.(i).dat)
              st_sz=idlsizes[i,*]
             if(st_sz[0] le 1) then begin
                ; Include condition where only 1 time selected w/ num_var 
                ;  length vector
                ;if(st_sz[0] eq 1 and st_sz[1] gt 1 and length eq 1) then begin
                   ;num_var=st_sz[1]
		; RCJ24
                if(st_sz[0] eq 1 and st_sz[1] eq 2 and length eq 1 and a.(i).dat[0] eq a.(i).dat[1]) then begin
                   num_var=st_sz[1]-1
		   
                   for k=0L, num_var-1 do begin
                      labels[inc]=label_search(a,st_sz[0],i,k,debug=debug)
		      units[inc]=unit_search(a,st_sz[0],i,k)
		      unique = strtrim(string(inc), 2)
                      ;temp=create_struct(atags[i]+unique,a.(i).dat[k,0])
		      ; RCJ24
                      if(names[i] eq depend0) then begin
		         temp=create_struct(atags[i]+unique,a.(i).dateph[k,0])
		      endif else begin	 
                         temp=create_struct(atags[i]+unique,a.(i).dat[k,0])
		      endelse	 
                      if(inc eq 0) then begin
                         b=temp
                      endif else begin
                         b=create_struct(b,temp)
                      endelse
                      inc=inc+1
                   endfor   ; end k
                endif else begin
                   labels[inc]=label_search(a,st_sz[0],i,0,debug=debug)
		   units[inc]=unit_search(a,st_sz[0],i,0)
                   ; names[i] eq 'EPOCH' or 'EPOCH92' etc.
                   if(names[i] eq depend0) then begin
                      temp=create_struct(names[i],a.(i).dateph[0])
                   endif else begin
                      temp=create_struct(names[i],a.(i).dat[0])
                   endelse
                   if(inc eq 0) then begin
                      b=temp
                   endif else begin
                      b=create_struct(b,temp)
                   endelse
                   inc=inc+1
                endelse
             endif   ;  end st_sz[0] le 1
             ;
             if(st_sz[0] eq 2) then begin
                num_var=st_sz[1]
                   for k=0L, num_var-1 do begin
                      labels[inc]=label_search(a,st_sz[0],i,k,debug=debug)
		      units[inc]=unit_search(a,st_sz[0],i,k)
		      unique = strtrim(string(inc), 2)
                      temp=create_struct(atags[i]+unique,a.(i).dat[k,0])
                      if(inc eq 0) then begin
                         b=temp
                      endif else begin
                         b=create_struct(b,temp)
		      endelse
                      inc=inc+1
                   endfor
             endif   ; end if st_sz(0) eq 2
             dep1=dependn_search(a,i,1)
             if (dep1[0] ne '') then begin
                depend1=a.(i).depend_1
                ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
                q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_1')
                if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_1 ne '') then depend1=a.(i).alt_cdaweb_depend_1
                s=execute('dep1_units=a.'+strtrim(depend1,2)+'.units')
                dep1=['(@_'+dep1+'_'+dep1_units+')']
             endif   
             dep1_values=[dep1_values,dep1]
          endif   ; end a.(i).var_type
       endfor   ; end i
       ; Free Memory
       spdf_delete, temp
       ;
       ; listing depend_1 values if they exist. RCJ 04/01
       dep1_values=dep1_values[1:*]
       ;
       ;do this computation once, instead of for each record
       i_ntags = ntags-5

       for j=0L, length-1 do begin
          inc=0L
          for i=0L,i_ntags do begin
             if ((strlowcase(a.(i).var_type) eq 'data') or (strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY'))  then begin
                ; 'EPOCH' or 'EPOCH92'               
                if(names[i] eq depend0) then begin
                   b.(inc)=a.(i).dateph[j]
                   inc=inc+1
                endif else begin
                   ;TJK 8/24/2009 EXTREMEMLY poor performance 
                   ;st_sz=size(a.(i).dat)
                   ;instead, compute the sizes once above this big loop and reference 
                   ;the values here.  
                   st_sz = idlsizes[i,*]
                   if(st_sz[0] le 1) then begin
		     ;if(st_sz[0] eq 1 and st_sz[1] gt 1 and length eq 1) then begin
                         ;num_var=st_sz[1]
                     ; RCJ24
		     if(st_sz[0] eq 1 and st_sz[1] eq 2 and length eq 1 and a.(i).dat[0] eq a.(i).dat[1]) then begin
                         num_var=st_sz[1]-1
                         for k=0L,num_var-1 do begin
                            b.(inc)=a.(i).dat[k,j]
                            inc=inc+1
                         endfor
                      endif else begin
                         b.(inc)=a.(i).dat[j] 
                         inc=inc+1
                      endelse
                   endif   
                   if(st_sz[0] eq 2) then begin
                      num_var=st_sz[1]
                         for k=0L,num_var-1 do begin
                            b.(inc)=a.(i).dat[k,j]
                            inc=inc+1
                         endfor
                   endif
                endelse  ; end  (names(i) ne depend0)
             endif   ; end a.(i).var_type
          endfor   ; end i
           
           
           COMMON SHARE, csv2
           
           if (csv2 EQ 3) AND (j EQ 0) then begin ;Handle JSON file creation for csv=3
             datsize = 0
             jdata_array = []
             for jindex1=0, (n_tags(a)-1) DO BEGIN
               if (ISA(a.(jindex1), /STRING) NE 1) then if (a.(jindex1).var_type NE 'additional_data') then begin
                 if jdata_array EQ !NULL then jdata_array = [jindex1] else jdata_array = [jdata_array, jindex1] ;Variables to include in JSON listing
               endif
             endfor
             COMMON SHARE3, cjsonstruct
             COMMON SHARE4, nan_count
             datsize = size(jdata_array, /n_elements) ;Number of variables we want to include in JSON listing             
             nan_count=0
             
             for jindex2=0, (datsize-1) DO BEGIN
               varattsreached = 'false'
               jtags = tag_names(a.(jindex2))
               counter = 0
               ;Check for NaN in data, then change values to ISTP Standard - CWG 07/09/2019
               if ISA(a.(jindex2).dat, /STRING) NE 1 then begin
                 if (a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] NE !NULL) AND (nan_count EQ 0) then begin                     
                   nan_count=1      ;this shared variable will force code to add a section in the listng that will inform user that NaNs were changed for JSON
                 endif
                 
                 ; Set a generic NAN replacer value using the case statement then use that replacer value for both
                 ; data replacement (currently done in the case) and fillvalue substitution.
                 ; Ron Yurow (Dec 3, 2021)
                 ;typeCDF = a.(jindex2).cdftype
                 ;CASE typeCDF OF
                 ; 'CDF_REAL4': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                 ; 'CDF_REAL8': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                 ; 'CDF_FLOAT' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                 ; 'CDF_DOUBLE' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                 ; 'CDF_BYTE': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                 ; 'CDF_INT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                 ; 'CDF_INT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -32768
                 ; 'CDF_INT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -2147483648
                 ; 'CDF_INT8' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                 ; 'CDF_UINT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 255
                 ; 'CDF_UINT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 65535
                 ; 'CDF_UINT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 4294967295
                 ; 'CDF_EPOCH' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                 ; 'CDF_EPOCH16' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                 ; 'CDF_TIME_TT2000' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                 ; 'CDF_CHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                 ; 'CDF_UCHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                 ; ELSE : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                 ;ENDCASE
                 
                 ; NaN replacer
                 nan_fillval_replace = 0

                 typeCDF = a.(jindex2).cdftype
                 CASE typeCDF OF
                   'CDF_REAL4': nan_fillval_replace = -1.0E+31
                   'CDF_REAL8': nan_fillval_replace = -1.0E+31
                   'CDF_FLOAT' : nan_fillval_replace = -1.0E+31
                   'CDF_DOUBLE' : nan_fillval_replace = -1.0E+31
                   'CDF_BYTE': nan_fillval_replace = -128
                   'CDF_INT1' : nan_fillval_replace = -128
                   'CDF_INT2' : nan_fillval_replace = -32768
                   'CDF_INT4' : nan_fillval_replace = -2147483648
                   'CDF_INT8' : nan_fillval_replace = -9223372036854775808
                   'CDF_UINT1' : nan_fillval_replace = 255
                   'CDF_UINT2' : nan_fillval_replace = 65535
                   'CDF_UINT4' : nan_fillval_replace = 4294967295
                   'CDF_EPOCH' : nan_fillval_replace = -1.0E+31
                   'CDF_EPOCH16' : nan_fillval_replace = -1.0E+31
                   'CDF_TIME_TT2000' : nan_fillval_replace = -9223372036854775808
                   'CDF_CHAR' : nan_fillval_replace = " "
                   'CDF_UCHAR' : nan_fillval_replace = " "
                   ELSE : nan_fillval_replace = " "
                 ENDCASE

                 a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = nan_fillval_replace

               endif

               
               for ii=0, (n_tags(a.(jindex2))-1) DO BEGIN
                 if jtags[ii] EQ 'FIELDNAM' then varattsreached = 'true'
                 if varattsreached EQ 'true' then begin

                   ; Check to see if the FILLVAL attribute is not finite.  If it is not, then set it
                   ; to the NaN replacement value.
                   ; Ron Yurow (Dec 3, 2021)
                   IF  jtags[ii] EQ 'FILLVAL' THEN BEGIN
                       IF  ~finite (a.(jindex2).(ii)) THEN a.(jindex2).(ii) = nan_fillval_replace
                   ENDIF

                   if ((a.(jindex2).cdftype EQ 'CDF_EPOCH16') || (a.(jindex2).cdftype EQ 'CDF_EPOCH') || (a.(jindex2).cdftype EQ 'CDF_TIME_TT2000')) then begin ;Convert epochs to iso-8601 for JSON files
                     if ((jtags[ii] EQ 'VALIDMIN') || (jtags[ii] EQ 'VALIDMAX') || (jtags[ii] EQ 'FILLVAL') || (jtags[ii] EQ 'DAT')) then begin

                       if (TYPENAME(a.(jindex2).(ii)) NE 'STRING') then begin 
		      
                        epochIso=cdf_epoch_tojuldays(a.(jindex2).(ii), /string)+'Z'

                       endif else epochIso=a.(jindex2).(ii)+'Z'
                       jvar = create_struct(jtags[ii], epochIso)
                     endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))
                   endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))
                   
                   if (counter EQ 0) then begin
                    varstruct = create_struct(jvar) 
                   endif else if (jtags[ii] NE 'DATEPH') then begin
                    varstruct = create_struct(varstruct, jvar)
                   endif
                   counter = counter + 1
                 endif
               endfor
               jdummystruct = create_struct(a.(jindex2).varname+'_', varstruct)
               if (jindex2 EQ 0) then cjsonstruct = create_struct(jdummystruct) else cjsonstruct = create_struct(cjsonstruct, jdummystruct)
             endfor
           endif

                
           ;Handle CSV file creation                    
           if keyword_set(csv) AND (csv2 NE 3) then begin
             labsize=n_elements(labels)
             for csvindex=0, (labsize-1) DO BEGIN
                if (j EQ 0) then begin
                   dep1_size = n_elements(dep1_values)
                   units[0] = 'yyyy-mm-ddThh:mm:ss.sssZ'
                   if dep1_size EQ labsize then labunits = labels+'_'+dep1_values+'_'+units else labunits = labels+'_'+units
                   tagname1 = 'tagg'+STRTRIM(csvindex,2)
                   if (csvindex LE (labsize-2)) then tmpvar1 = STRTRIM(labunits[csvindex],2)+',' else tmpvar1 = STRTRIM(labunits[csvindex],2)
                   tmpstruc1=create_struct(tagname1,tmpvar1)
                   if (csvindex EQ 0) then newlabs=create_struct(tmpstruc1) else newlabs=create_struct(newlabs,tmpstruc1)
                endif
                tagname2 = 'tag'+STRTRIM(csvindex,2)   
                if (csvindex LE (labsize-2)) then tmpvar2=STRTRIM(b.(csvindex),2)+',' else tmpvar2=STRTRIM(b.(csvindex),2)
                tmpstruc2= create_struct(tagname2, tmpvar2)
                if (csvindex EQ 0) then begin
                   epoch_iso = a.(0).dat[j]
                   epoch_iso=cdf_epoch_tojuldays(epoch_iso, /string)
                   epoch_iso = epoch_iso+'Z,'
                   tmpvar2=STRTRIM(epoch_iso,2)
                   tmpstruc2= create_struct(tagname2, tmpvar2)
                   newb = create_struct(tmpstruc2)
                endif else begin         
                   newb = create_struct(newb, tmpstruc2)
                endelse
             endfor
             if (j EQ 0) then begin
                struclength= n_tags(newlabs)
                newform = '('+strtrim(struclength,2)+'(A))'
                printf, unit, format=newform,newlabs
                cc = strlen(epoch_iso)
                ; Make sure that we don't write a format element 0 times, IDL
                ; does not like that.
                ; Ron Yurow (August 10, 2020)
                ; newform = '(A'+strtrim(cc,2)+','+strtrim(struclength-1,2)+'(A))'
                newform = struclength gt 1 ? '(A'+strtrim(cc,2)+','+strtrim(struclength-1,2)+'(A))' : '(A'+strtrim(cc,2)+')'
             endif 
             printf, unit, format=newform,newb
             
           endif else if (csv2 NE 3) then begin ;Handle .txt files
               if (j EQ 0) then begin
                 printf,unit,format=a.lform,labels
                 q=where (dep1_values ne '')
                 if q[0] ne -1  then printf,unit,format=a.dpform,dep1_values
                 printf,unit,format=a.uform,units
               endif
               printf,unit,format=a.dform,b
           endif

       endfor   ; end j
       
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, format='(a)',blnk
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          status=1                                  
          length=maxrecs
       endif
       ; Free Memory
       spdf_delete, b
   end   ;   end case 3
   ;
   ; 3-D Record Varying Variables 
   ;
   4 : begin
       ; Check MAXRECS
       if(n_elements(num_data) eq 0) then num_data=0
       ; Put in appropriate record count
       len=size(a.(0).dat)
       length=len[len[0]+2]
       
       ; RCJ24
       if n_elements(a.(0).dat) eq 2 and (a.(0).dat[0] eq a.(0).dat[1]) then begin
         length=1
       endif	 
       
       ; Check for maxrecs begin exceeded                 
       num_data=length
       num_data=num_data+4
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          printf, unit, format='(a)',blnk
          ; printf, unit, ' '                                                     
          status=1                                  
          length=maxrecs
       endif
       ;
       printf, unit, format='("#",14x,"************************************")'
       printf, unit, format='("#",14x,"****  RECORD VARYING VARIABLES  ****")'
       printf, unit, format='("#",14x,"************************************")'
       printf, unit, format='("#",14x)'
       printf,unit, format='("# 1. ",a)', a.epoch.fieldnam
       printf,unit, format='("# 2. ",a)', a.index.catdesc
       printf,unit, format='("# 3. ",a)', a.qflag.catdesc
       printf,unit, format='("# 4. ",a)', a.position.fieldnam
       printf,unit, format='("# 5. ",a)', a.vel.fieldnam
       printf,unit,format='("#",14x)'
       ;
       num=7
       labels=strarr(num)
       units=strarr(num)
       inc=0
       ; Epoch
       eplabel='                       ' 
       strput,eplabel,a.epoch.fieldnam,0
       labels[inc]=eplabel
       units[inc]=a.epoch.units
       temp=create_struct('EPOCH',a.epoch.dateph[0])
       b=temp
       inc=inc+1
       ; Index     
       labels[inc]="Index" 
       units[inc]=''
       inc=inc+1
       ; Qflag
       labels[inc]=a.qflag.lablaxis
       units[inc]=a.qflag.units
       inc=inc+1
       ; Position     
       for k=0, 1 do begin
          if(k eq 0) then labels[inc]=" geo latitude"
          if(k eq 1) then labels[inc]="geo longitude"
          units[inc]=a.position.units
          inc=inc+1
       endfor
       for k=0, 1 do begin
          if(k eq 0) then labels[inc]=" geo east vel"
          if(k eq 1) then labels[inc]="geo north vel"
          units[inc]=a.vel.units
          inc=inc+1
       endfor
       ;
       farr=fltarr(180)
       in=0
       for l=0,29 do begin
          farr[in]=a.index.dat[0]
          in=in+1
          farr[in]=a.qflag.dat[l,0]
          in=in+1
          for k=0, 1 do begin
             farr[in]= a.position.dat[k,l,0]
             in=in+1
          endfor
          for k=0, 1 do begin
             farr[in]= a.vel.dat[k,l,0]
             in=in+1
          endfor
       endfor
       temp=create_struct('DATREC',farr)
       b=create_struct(b,temp)
       ;
       ; Free Memory
       spdf_delete, temp 
       ;
       for j=0L, length-1 do begin
          m=0
          b.epoch=a.epoch.dateph[j]
          for l=0,29 do begin
             b.datrec[m]=a.index.dat[l]
             m=m+1
             b.datrec[m]=a.qflag.dat[l,j]
             m=m+1
             for k=0,1 do begin
                b.datrec[m]=a.position.dat[k,l,j]
                m=m+1
             endfor
             for k=0,1 do begin
                b.datrec[m]=a.vel.dat[k,l,j]
                m=m+1
             endfor
          endfor   ; end l
                    
          ;Handle JSON file creation for csv=3
          COMMON SHARE, csv2
          if (csv2 EQ 3) AND (j EQ 0) then begin
            datsize = 0
            jdata_array = []
            for jindex1=0, (n_tags(a)-1) DO BEGIN
              if (ISA(a.(jindex1), /STRING) NE 1) then if (a.(jindex1).var_type NE 'additional_data') then begin
                if jdata_array EQ !NULL then jdata_array = [jindex1] else jdata_array = [jdata_array, jindex1] ;Variables to include in JSON listing
              endif
            endfor
            COMMON SHARE3, cjsonstruct
            COMMON SHARE4, nan_count
            datsize = size(jdata_array, /n_elements) ;Number of variables we want to include in JSON listing
            nan_count=0
            for jindex2=0, (datsize-1) DO BEGIN
              varattsreached = 'false'
              jtags = tag_names(a.(jindex2))
              counter = 0
              
              ;Check for NaN in data, then change values to ISTP Standard - CWG 07/09/2019
              if ISA(a.(jindex2).dat, /STRING) NE 1 then begin
                if (a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] NE !NULL) AND (nan_count EQ 0) then begin
                  nan_count=1      ;this sared variable will force code to add a section in the listng that will inform user that NaNs were changed for JSON
                endif

                ; Set a generic NAN replacer value using the case statement then use that replacer value for both
                ; data replacement (currently done in the case) and fillvalue substitution.
                ; Ron Yurow (Dec 3, 2021)
                ;typeCDF = a.(jindex2).cdftype
                ;CASE typeCDF OF
                ;  'CDF_REAL4': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_REAL8': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_FLOAT' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_DOUBLE' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_BYTE': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                ;  'CDF_INT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                ;  'CDF_INT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -32768
                ;  'CDF_INT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -2147483648
                ;  'CDF_INT8' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                ;  'CDF_UINT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 255
                ;  'CDF_UINT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 65535
                ;  'CDF_UINT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 4294967295
                ;  'CDF_EPOCH' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_EPOCH16' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_TIME_TT2000' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                ;  'CDF_CHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ; 'CDF_UCHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ;  ELSE : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ;ENDCASE

                ; NaN replacer
                nan_fillval_replace = 0

                typeCDF = a.(jindex2).cdftype
                CASE typeCDF OF
                  'CDF_REAL4': nan_fillval_replace = -1.0E+31
                  'CDF_REAL8': nan_fillval_replace = -1.0E+31
                  'CDF_FLOAT' : nan_fillval_replace = -1.0E+31
                  'CDF_DOUBLE' : nan_fillval_replace = -1.0E+31
                  'CDF_BYTE': nan_fillval_replace = -128
                  'CDF_INT1' : nan_fillval_replace = -128
                  'CDF_INT2' : nan_fillval_replace = -32768
                  'CDF_INT4' : nan_fillval_replace = -2147483648
                  'CDF_INT8' : nan_fillval_replace = -9223372036854775808
                  'CDF_UINT1' : nan_fillval_replace = 255
                  'CDF_UINT2' : nan_fillval_replace = 65535
                  'CDF_UINT4' : nan_fillval_replace = 4294967295
                  'CDF_EPOCH' : nan_fillval_replace = -1.0E+31
                  'CDF_EPOCH16' : nan_fillval_replace = -1.0E+31
                  'CDF_TIME_TT2000' : nan_fillval_replace = -9223372036854775808
                  'CDF_CHAR' : nan_fillval_replace = " "
                  'CDF_UCHAR' : nan_fillval_replace = " "
                  ELSE : nan_fillval_replace = " "
                ENDCASE

                a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = nan_fillval_replace

              endif             
              
              for ii=0, (n_tags(a.(jindex2))-1) DO BEGIN
                if jtags[ii] EQ 'FIELDNAM' then varattsreached = 'true'
                if varattsreached EQ 'true' then begin
                   ; Check to see if the FILLVAL attribute is not finite.  If it is not, then set it
                   ; to the NaN replacement value.
                   ; Ron Yurow (Dec 3, 2021)
                   IF  jtags[ii] EQ 'FILLVAL' THEN BEGIN
                       IF  ~finite (a.(jindex2).(ii)) THEN a.(jindex2).(ii) = nan_fillval_replace
                   ENDIF

                  if ((a.(jindex2).cdftype EQ 'CDF_EPOCH16') || (a.(jindex2).cdftype EQ 'CDF_EPOCH') || (a.(jindex2).cdftype EQ 'CDF_TIME_TT2000')) then begin ;Convert epochs to iso-8601 for JSON files
                    ;if ((jtags[ii] EQ 'VALIDMIN') || (jtags[ii] EQ 'VALIDMAX') || (jtags[ii] EQ 'FILLVAL') || (jtags[ii] EQ 'DAT')) then begin
                    if ((jtags[ii] EQ 'VALIDMIN') || (jtags[ii] EQ 'VALIDMAX') ||  (jtags[ii] EQ 'DAT')) then begin

                      if (TYPENAME(a.(jindex2).(ii)) NE 'STRING') then begin
                        epochIso=cdf_epoch_tojuldays(a.(jindex2).(ii), /string)+'Z'
                      endif else epochIso=a.(jindex2).(ii)+'Z'
                      jvar = create_struct(jtags[ii], epochIso)
                    endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))
                  endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))

           
                  if (counter EQ 0) then begin
                    varstruct = create_struct(jvar) 
                   endif else if (jtags[ii] NE 'DATEPH') then begin
                    varstruct = create_struct(varstruct, jvar)
                   endif
                  counter = counter + 1
                endif
              endfor
              jdummystruct = create_struct(a.(jindex2).varname+'_', varstruct)
              if (jindex2 EQ 0) then cjsonstruct = create_struct(jdummystruct) else cjsonstruct = create_struct(cjsonstruct, jdummystruct)
            endfor
          endif

           ;Handle CSV file creation 
           if keyword_set(csv) AND (csv2 NE 3) then begin
             labsize=n_elements(labels)
             for csvindex=0, (labsize-1) DO BEGIN
                if (j EQ 0) then begin
                   units[0] = 'yyyy-mm-ddThh:mm:ss.sssZ'
                   labunits = labels+'_'+units
                   tagname1 = 'tagg'+STRTRIM(csvindex,2)
                   if (csvindex LE (labsize-2)) then tmpvar1 = STRTRIM(labunits[csvindex],2)+',' else tmpvar1 = STRTRIM(labunits[csvindex],2)
                   tmpstruc1=create_struct(tagname1,tmpvar1)
                   if (csvindex EQ 0) then newlabs=create_struct(tmpstruc1) else newlabs=create_struct(newlabs,tmpstruc1)
                endif
                tagname2 = 'tag'+STRTRIM(csvindex,2)
                if (csvindex LE (labsize-2)) then tmpvar2=STRTRIM(b.(csvindex),2)+',' else tmpvar2=STRTRIM(b.(csvindex),2)
                tmpstruc2= create_struct(tagname2, tmpvar2)
                if (csvindex EQ 0) then begin
                   epoch_iso = a.(0).dat[j]
                   epoch_iso=cdf_epoch_tojuldays(epoch_iso, /string)
                   epoch_iso=epoch_iso+'Z,'
                   tmpvar2=STRTRIM(epoch_iso,2)
                   tmpstruc2= create_struct(tagname2, tmpvar2)
                   newb = create_struct(tmpstruc2)
                endif else begin
                   newb = create_struct(newb, tmpstruc2)
                endelse
             endfor
             if (j EQ 0) then begin
                struclength= n_tags(newlabs)
                newform = '('+strtrim(struclength,2)+'(A))'
                printf, unit, format=newform,newlabs
                cc = strlen(epoch_iso)
                ; Make sure that we don't write a format element 0 times, IDL
                ; does not like that.
                ; Ron Yurow (August 10, 2020)
                ; newform = '(A'+strtrim(cc+1,2)+','+strtrim(struclength-1,2)+'(A))'
                newform = struclength gt 1 ? '(A'+strtrim(cc+1,2)+','+strtrim(struclength-1,2)+'(A))' : '(A'+strtrim(cc+1,2)+')'
             endif 
             printf, unit, format=newform,newb
          endif else if (csv2 NE 3) then begin
             ;Handle .txt files
             if (j EQ 0) then begin
              printf,unit,format=a.lform,labels
              printf,unit,format=a.uform,units
             endif
             printf,unit,format=a.dform,b
          endif

       endfor   ; end j
       ;
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, format='(a)',blnk
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          status=1                                  
          length=maxrecs
       endif
       ; Free Memory
       spdf_delete, b
   end   ;  end case 4
   ;
   ; Image Data and 3D data (only difference is 3D data will have depend_3)
   ;
   5: begin
       ; Check MAXRECS
       if(n_elements(num_data) eq 0) then num_data=0
       ; Put in appropriate record count
       len=size(a.(0).dat)
       length=len[len[0]+2]
       
       ; RCJ24
       if n_elements(a.(0).dat) eq 2 and (a.(0).dat[0] eq a.(0).dat[1]) then begin
         length=1
       endif	 
       
       ; Check for maxrecs begin exceeded                 
       num_data=length
       num_data=num_data+4
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          printf, unit, format='(a)',blnk
          ; printf, unit, ' '                                                     
          status=1                                  
          length=maxrecs
       endif

       status=list_header(a,unit,ntags)
       num=nvar
       
       final_labels=''
       final_units=''
       final_dep1_values=''
       final_dep2_values=''
       final_dep3_values=''
       atags=tag_names(a)
       inc=0L
       for i=0L, ntags-5 do begin
          if(strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY')) then begin
             labels=''
             units=''
             dep1_values=''
             dep2_values=''
             dep3_values=''
             st_sz=size(a.(i).dat)
             if(st_sz[0] le 1) then begin
                ; get labels and units:
                labels=[labels,label_search(a,st_sz[0],i,0,debug=debug)]
                ;units=[units,a.(i).units]
                units=[units,unit_search(a,st_sz[0],i,0)]
                if(names[i] eq depend0) then begin
                   temp=create_struct(names[i],a.(i).dateph[0])
                endif else begin
                   temp=create_struct(names[i],a.(i).dat[0])
                endelse
                if(inc eq 0) then begin
                   b=temp
                endif else begin
                   b=create_struct(b,temp)
                endelse
                inc=inc+1L
             endif
             if(st_sz[0] eq 2) then begin
                ; get labels and units:
                num_var=st_sz[1]
                for k=0L, num_var-1 do begin
                   labels=[labels,label_search(a,st_sz[0],i,k,debug=debug)]
                   units=[units,unit_search(a,st_sz[0],i,k)]
		   unique = strtrim(string(inc), 2)
                   temp=create_struct(atags[i]+unique,a.(i).dat[k,0])
                   b=create_struct(b,temp)
                   inc=inc+1
                endfor
             endif   ; end st_sz(0) eq 2
             ; Free Memory
             spdf_delete, temp
             ;
             labels=labels[1:*]
             final_labels=[final_labels,labels]
             units=units[1:*]
             final_units=[final_units,units]
             ;
             ; create array of depend_1 values, if they exist, to also be listed
             ; RCJ 07/2013
             ; exist test is done in dependn_search, if does not exist
             ; return ''
             dep1=dependn_search(a,i,1)
             if (dep1[0] ne '') then begin
                depend1=a.(i).depend_1
                ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
                q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_1')
                if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_1 ne '') then depend1=a.(i).alt_cdaweb_depend_1 
                s=execute('dep1_units=a.'+strtrim(depend1,2)+'.units')
                dep1=['(@_'+dep1+'_'+dep1_units+')']
             endif 
             dep1_values=[dep1_values,dep1]
             ; create array of depend_2 and _3 values, if they exist, to also be listed
             ; RCJ 07/13
             ; exist test is done in dependn_search, if does not exist
             ; return ''
             dep2=dependn_search(a,i,2)
             if (dep2[0] ne '') then begin
                depend2=a.(i).depend_2
                ; RCJ 05/16/2013  If alt_cdaweb_depend_2 exists, use it instead:
                q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_2')
                if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_2 ne '') then depend2=a.(i).alt_cdaweb_depend_2 
                s=execute('dep2_units=a.'+strtrim(depend2,2)+'.units')
                dep2=['(@_'+dep2+'_'+dep2_units+')']
             endif 
             dep2_values=[dep2_values,dep2]
             dep3=dependn_search(a,i,3)
             if (dep3[0] ne '') then begin
                depend3=a.(i).depend_3
                q=where(tag_names(a.(i)) eq 'ALT_CDAWEB_DEPEND_3')
                if (q[0] ne -1) then if (a.(i).alt_cdaweb_depend_3 ne '') then depend3=a.(i).alt_cdaweb_depend_3 
                s=execute('dep3_units=a.'+strtrim(depend3,2)+'.units')
                dep3=['(@_'+dep3+'_'+dep3_units+')']
             endif 
             dep3_values=[dep3_values,dep3]
             ;
             ; listing depend_1 values if they exist. RCJ 06/01
             if (n_elements(dep1_values) gt 1) then begin
                tmp_dep1_values=dep1_values[1:*]
                while n_elements(dep1_values)-1 le n_elements(labels)-n_elements(tmp_dep1_values) do begin
                   dep1_values=[dep1_values,tmp_dep1_values]
                endwhile
                dep1_values=dep1_values[1:*]
                final_dep1_values=[final_dep1_values,dep1_values]
             endif    
             ; listing depend_2 values if they exist. RCJ 06/01
             if (n_elements(dep2_values) gt 1) then begin
                tmp_dep2_values=dep2_values[1:*]
                if n_elements(tmp_dep2_values) eq n_elements(labels) then begin
                   ; RCJ 07/01 If the initial depend_2 is 2D (now stretched into 1D)
                   ; we don't need to do what goes below:
                endif else begin
                   k=0
                   dep2_values=''
                   while n_elements(dep2_values)-1 le n_elements(labels)-n_elements(tmp_dep1_values) do begin
                      for kk=0L,n_elements(tmp_dep1_values)-1 do begin
                         dep2_values=[dep2_values,tmp_dep2_values[k]]
                      endfor   
                      k=k+1
                      if k ge n_elements(tmp_dep2_values) then k=0
                   endwhile
                endelse   
                if n_elements(dep2_values) gt 1 then dep2_values=dep2_values[1:*]
                final_dep2_values=[final_dep2_values,dep2_values]
             endif  
             ; listing depend_3 values if they exist. 
             if (n_elements(dep3_values) gt 1) then begin
                tmp_dep3_values=dep3_values[1:*]
                if n_elements(tmp_dep3_values) eq n_elements(labels) then begin
                endif else begin
                   k=0
                   dep3_values=''
                   while n_elements(dep3_values)-1 le (n_elements(labels)-(n_elements(tmp_dep2_values)*n_elements(tmp_dep1_values))) do begin
                      for kk=0L,(n_elements(tmp_dep2_values)*n_elements(tmp_dep1_values))-1 do begin
                         dep3_values=[dep3_values,tmp_dep3_values[k]]
                      endfor   
                      k=k+1
                      if k ge n_elements(tmp_dep3_values) then k=0
                   endwhile
                endelse 
                if n_elements(dep3_values) gt 1 then dep3_values=dep3_values[1:*]
                final_dep3_values=[final_dep3_values,dep3_values]
             endif  
             ;
             ;
          endif   ; end a.(i).var_type
          ;
       endfor   ; end i
       ;
       final_labels=final_labels[1:*]
       final_units=final_units[1:*]  ;  but cannot printf the units right now
                                     ;  If there are depend_1/_2 they come first.
       ; If there are labels with no corresponding dep1 values,
       ; then add spaces before the first element of the array.
       ; This works as long as the labels which *do not have* corresponding dep1 
       ; values come before the labels which *have* corresponding dep1 values. 
       ; If that condition is not true, the logic has to be reworked.  RCJ 07/01
       if n_elements(final_dep1_values) gt 1 then begin
          final_dep1_values=final_dep1_values[1:*]
          diff=n_elements(final_labels)-n_elements(final_dep1_values)
          for k=1L,diff do begin
             cmd='space=string("",format="('+strtrim(strlen(final_labels[k])+1,2)+'x,a)")'
             s=execute(cmd)
             final_dep1_values=[space,final_dep1_values]
          endfor
       endif   
       ; same for dep2 values:
       if n_elements(final_dep2_values) gt 1 then begin
          final_dep2_values=final_dep2_values[1:*]
          diff=n_elements(final_labels)-n_elements(final_dep2_values)
          for k=1L,diff do begin
             cmd='space=string("",format="('+strtrim(strlen(final_labels[k])+1,2)+'x,a)")'
             s=execute(cmd)
             final_dep2_values=[space,final_dep2_values]
          endfor
       endif  
       ; 
       ; same for dep3 values:
       if n_elements(final_dep3_values) gt 1 then begin
          final_dep3_values=final_dep3_values[1:*]
          diff=n_elements(final_labels)-n_elements(final_dep3_values)
          for k=1L,diff do begin
             cmd='space=string("",format="('+strtrim(strlen(final_labels[k])+1,2)+'x,a)")'
             s=execute(cmd)
             final_dep3_values=[space,final_dep3_values]
          endfor
       endif  

       for j=0L, length-1 do begin
          inc=0L
          for i=0L,ntags-5 do begin
             if (strlowcase(a.(i).var_type) eq 'data') or ((strlowcase(a.(i).var_type) eq 'support_data') and (a.(i).cdfrecvary ne 'NOVARY')) then begin
                ; if(names(i) eq 'EPOCH' or names(i) eq 'EPOCH92') then begin
                if(names[i] eq depend0) then begin
                   b.(inc)=a.(i).dateph[j]
                   inc=inc+1L
                endif else begin
                   st_sz=size(a.(i).dat)  
                   if(st_sz[0] eq 1) then begin
                      b.(inc)=a.(i).dat[j] 
                      inc=inc+1L
                   endif   
                   if(st_sz[0] eq 2) then begin
                      num_var=st_sz[1]
                      for k=0L,num_var-1 do begin
                         b.(inc)=a.(i).dat[k,j]
                         inc=inc+1L
                      endfor
                   endif
                endelse
             endif
          endfor   ; end i
                    
          ;Handle JSON file creation for csv=3
          COMMON SHARE, csv2
          if (csv2 EQ 3) AND (j EQ 0) then begin
            datsize = 0
            jdata_array = []
            for jindex1=0, (n_tags(a)-1) DO BEGIN
              if (ISA(a.(jindex1), /STRING) NE 1) then if (a.(jindex1).var_type NE 'additional_data') then begin
                if jdata_array EQ !NULL then jdata_array = [jindex1] else jdata_array = [jdata_array, jindex1] ;Variables to include in JSON listing
              endif
            endfor
            COMMON SHARE3, cjsonstruct
            COMMON SHARE4, nan_count
            datsize = size(jdata_array, /n_elements) ;Number of variables we want to include in JSON listing
            nan_count=0
            for jindex2=0, (datsize-1) DO BEGIN
              varattsreached = 'false'
              jtags = tag_names(a.(jindex2))
              counter = 0
              
              ;Check for NaN in data, then change values to ISTP Standard - CWG 07/09/2019
              if ISA(a.(jindex2).dat, /STRING) NE 1 then begin
                if (a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] NE !NULL) AND (nan_count EQ 0) then begin
                  nan_count=1      ;this sared variable will force code to add a section in the listng that will inform user that NaNs were changed for JSON
                endif

                ; Set a generic NAN replacer value using the case statement then use that replacer value for both
                ; data replacement (currently done in the case) and fillvalue substitution.
                ; Ron Yurow (Dec 3, 2021)
                ;typeCDF = a.(jindex2).cdftype
                ;CASE typeCDF OF
                ;  'CDF_REAL4': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_REAL8': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_FLOAT' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_DOUBLE' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_BYTE': a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                ;  'CDF_INT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -128
                ;  'CDF_INT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -32768
                ;  'CDF_INT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -2147483648
                ;  'CDF_INT8' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                ;  'CDF_UINT1' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 255
                ;  'CDF_UINT2' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 65535
                ;  'CDF_UINT4' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = 4294967295
                ;  'CDF_EPOCH' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_EPOCH16' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -1.0E+31
                ;  'CDF_TIME_TT2000' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = -9223372036854775808
                ;  'CDF_CHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ; 'CDF_UCHAR' : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ;  ELSE : a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = " "
                ;ENDCASE

                ; NaN replacer
                nan_fillval_replace = 0

                typeCDF = a.(jindex2).cdftype
                CASE typeCDF OF
                  'CDF_REAL4': nan_fillval_replace = -1.0E+31
                  'CDF_REAL8': nan_fillval_replace = -1.0E+31
                  'CDF_FLOAT' : nan_fillval_replace = -1.0E+31
                  'CDF_DOUBLE' : nan_fillval_replace = -1.0E+31
                  'CDF_BYTE': nan_fillval_replace = -128
                  'CDF_INT1' : nan_fillval_replace = -128
                  'CDF_INT2' : nan_fillval_replace = -32768
                  'CDF_INT4' : nan_fillval_replace = -2147483648
                  'CDF_INT8' : nan_fillval_replace = -9223372036854775808
                  'CDF_UINT1' : nan_fillval_replace = 255
                  'CDF_UINT2' : nan_fillval_replace = 65535
                  'CDF_UINT4' : nan_fillval_replace = 4294967295
                  'CDF_EPOCH' : nan_fillval_replace = -1.0E+31
                  'CDF_EPOCH16' : nan_fillval_replace = -1.0E+31
                  'CDF_TIME_TT2000' : nan_fillval_replace = -9223372036854775808
                  'CDF_CHAR' : nan_fillval_replace = " "
                  'CDF_UCHAR' : nan_fillval_replace = " "
                  ELSE : nan_fillval_replace = " "
                ENDCASE

                a.(jindex2).dat[where(~finite(a.(jindex2).dat), /null)] = nan_fillval_replace

              endif              
              
              for ii=0, (n_tags(a.(jindex2))-1) DO BEGIN
                if jtags[ii] EQ 'FIELDNAM' then varattsreached = 'true'
                if varattsreached EQ 'true' then begin
                   ; Check to see if the FILLVAL attribute is not finite.  If it is not, then set it
                   ; to the NaN replacement value.
                   ; Ron Yurow (Dec 3, 2021)
                   IF  jtags[ii] EQ 'FILLVAL' THEN BEGIN
                      IF  ~finite (a.(jindex2).(ii)) THEN a.(jindex2).(ii) = nan_fillval_replace
                   ENDIF

                  if ((a.(jindex2).cdftype EQ 'CDF_EPOCH16') || (a.(jindex2).cdftype EQ 'CDF_EPOCH') || (a.(jindex2).cdftype EQ 'CDF_TIME_TT2000')) then begin ;Convert epochs to iso-8601 for JSON files
                    ;if ((jtags[ii] EQ 'VALIDMIN') || (jtags[ii] EQ 'VALIDMAX') || (jtags[ii] EQ 'FILLVAL') || (jtags[ii] EQ 'DAT')) then begin
                    if ((jtags[ii] EQ 'VALIDMIN') || (jtags[ii] EQ 'VALIDMAX') || (jtags[ii] EQ 'DAT')) then begin
		      
                      if (TYPENAME(a.(jindex2).(ii)) NE 'STRING') then begin
                        epochIso=cdf_epoch_tojuldays(a.(jindex2).(ii), /string)+'Z'
                      endif else epochIso=a.(jindex2).(ii)+'Z'
                      jvar = create_struct(jtags[ii], epochIso)
                    endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))
                  endif else jvar = create_struct(jtags[ii], a.(jindex2).(ii))
                  

                  if (counter EQ 0) then begin
                    varstruct = create_struct(jvar) 
                   endif else if (jtags[ii] NE 'DATEPH') then begin
                    varstruct = create_struct(varstruct, jvar)
                   endif
                               
                  counter = counter + 1
                endif
              endfor
              jdummystruct = create_struct(a.(jindex2).varname+'_', varstruct)
              if (jindex2 EQ 0) then cjsonstruct = create_struct(jdummystruct) else cjsonstruct = create_struct(cjsonstruct, jdummystruct)
            endfor
          endif
                 
          ;Handle CSV file creation
          if keyword_set(csv) AND (csv2 NE 3) then begin
             labsize=n_elements(final_labels)
             for csvindex=0, (labsize-1) DO BEGIN
                if (j EQ 0) then begin
                   final_units[0] = 'yyyy-mm-ddThh:mm:ss.sssZ'
                   dep1_size = n_elements(final_dep1_values)
                   dep2_size = n_elements(final_dep2_values)
                   dep3_size = n_elements(final_dep3_values)
                   if (dep1_size EQ dep2_size && dep2_size EQ dep3_size && dep3_size EQ labsize) then labunits = final_labels+'_'+final_dep1_values+'_'+final_dep2_values+'_'+final_dep3_values+'_'+final_units else labunits = final_labels+'_'+final_units
                   tagname1 = 'tagg'+STRTRIM(csvindex,2)
                   if (csvindex LE (labsize-2)) then tmpvar1 = STRTRIM(labunits[csvindex],2)+',' else tmpvar1 = STRTRIM(labunits[csvindex],2)
                   tmpstruc1=create_struct(tagname1,tmpvar1)
                   if (csvindex EQ 0) then newlabs=create_struct(tmpstruc1) else newlabs=create_struct(newlabs,tmpstruc1)         
                endif              
                tagname2 = 'tag'+STRTRIM(csvindex,2)
                if (csvindex LE (labsize-2)) then tmpvar2=STRTRIM(b.(csvindex),2)+',' else tmpvar2=STRTRIM(b.(csvindex),2)
                tmpstruc2= create_struct(tagname2, tmpvar2)
                if (csvindex EQ 0) then begin    
                   epoch_iso = a.(0).dat[j]
                   epoch_iso=cdf_epoch_tojuldays(epoch_iso, /string)
                   epoch_iso=epoch_iso+'Z,'
                   tmpvar2=STRTRIM(epoch_iso,2)
                   tmpstruc2= create_struct(tagname2, tmpvar2)
                   newb = create_struct(tmpstruc2)
                endif else begin
                   newb = create_struct(newb, tmpstruc2)
                endelse
             endfor
             if (j EQ 0) then begin
                struclength= n_tags(newlabs)
                newform = '('+STRTRIM(struclength,2)+'(A))'
                printf, unit, format=newform,newlabs
                cc = strlen(epoch_iso)
                ; Make sure that we don't write a format element 0 times, IDL
                ; does not like that.
                ; Ron Yurow (August 10, 2020)
                ; newform = '(A'+strtrim(cc,2)+','+strtrim(struclength-1,2)+'(A))'
                newform = struclength gt 1 ? '(A'+strtrim(cc,2)+','+strtrim(struclength-1,2)+'(A))' : '(A'+strtrim(cc,2)+')'
             endif 
             printf, unit, format=newform,newb
          endif else if (csv2 NE 3) then begin
             ;Handle .txt files
             if (j EQ 0) then begin
               printf,unit,format=a.lform,final_labels
               q=where (final_dep1_values ne '')
               if q[0] ne -1 then printf,unit,format=a.dpform,final_dep1_values
               q=where (final_dep2_values ne '')
               if q[0] ne -1 then printf,unit,format=a.dpform,final_dep2_values
               q=where (final_dep3_values ne '')
               if q[0] ne -1 then printf,unit,format=a.dpform,final_dep3_values
               printf,unit,format=a.uform,final_units
             endif            
             printf,unit,format=a.dform,b
          endif

       endfor   ; end j
       ; 
       if(num_data gt maxrecs) then begin
          dif_rec=num_data-maxrecs
          text='# The maximum number of records allowed to be listed is '
          text1='# Your request has exceeded this maximum by '
          printf, unit, format='(a)',blnk
          printf, unit,text,maxrecs
          printf, unit, format='(a,i6)',text1,dif_rec
          status=1                                  
          length=maxrecs
       endif
       ; Free Memory
       spdf_delete, b   
   end   ; end case 5
   ;
   ;
   else : begin
          print, 'STATUS= A listing of these data cannot be generated. '
          print, "ERROR=Error: Invalid control variable; convar= ",convar
          close,1
          return, -1
   end 
endcase   ; end case convar

return, status 
  
end

;----------------------------------------------------------------------------
;+
; NAME:  form_bld.pro
;
; PURPOSE: Builds format statements 
;
; shft - 0= left justified field; 1= right justified field
;
FUNCTION form_bld, col_sz, label, units, dat_len, dep_col_sz, depend1_labels, $
   dep2_col_sz, depend2_labels, dep3_col_sz, depend3_labels,form, shft
 
; Use column size to build label, unit and data format statements
;
maxlength=max(strlen(depend1_labels)) > max(strlen(depend2_labels))  > max(strlen(depend3_labels)) > strlen(label) 
mintab=fix(dep_col_sz-max(strlen(depend1_labels))) < fix(dep2_col_sz-max(strlen(depend2_labels))) < fix(dep3_col_sz-max(strlen(depend3_labels)))<fix(col_sz-strlen(label))
;
; depend1 and depend2 use the same format (depv) :
; depend1, depend2 and depend3 use the same format (depv) :
ltab=strtrim(mintab,2)
lfld=strtrim(maxlength,2)

if(shft eq 0) then begin
   if(ltab ne '0') then depv='A'+lfld+','+ltab+'X,1X,' else depv='A'+lfld+',1X,'
endif else begin
   if(ltab ne '0') then depv=ltab+'X,A'+lfld+',1X,' else depv='A'+lfld+',1X,'
endelse
;
if(shft eq 0) then begin
   if(ltab ne '0') then labv='A'+lfld+','+ltab+'X,1X,' else labv='A'+lfld+',1X,'
endif else begin
   if(ltab ne '0') then labv=ltab+'X,A'+lfld+',1X,' else labv='A'+lfld+',1X,'
endelse
;
col_sz=maxlength > col_sz
utab=strtrim(fix(col_sz-strlen(units)),2)
ufld=strtrim(strlen(units),2)
if(shft eq 0) then begin
   if(utab ne '0') then untv='A'+ufld+','+utab+'X,1X,' else untv='A'+ufld+',1X,'
endif else begin
   if(utab ne '0') then untv=utab+'X,A'+ufld+',1X,' else untv='A'+ufld+',1X,'
endelse
;
dtab=strtrim(fix(col_sz-dat_len),2)
if(dtab ne '0') then datv=dtab+'X,'+form+',1X,' $
     else datv=form+',1X,'
sform=create_struct('labv',labv,'untv',untv,'datv',datv,'depv',depv)

return, sform
end

;----------------------------------------------------------------------------
;+ 
; NAME:  data_len.pro
;
; PURPOSE: Determines the length of the data field given FORMAT, FILLVAL 
;
;

FUNCTION data_len,format,fillval,debug=debug
                  
; Set input values if undefined 
;
status=0
;if(n_elements(format) eq 0) then form='null' else form=strmid(format,0,1)
;if(strlen(format) eq 0) then form='null' else begin
; RCJ 11/23/05   It has to be G format or fillvals will be ****          
if(strlen(format) eq 0) then format='G13.6' 

; RCJ 18Nov2019 This logic will remove the exponent width part of the format, if present.
;               For example: G10.2E3 -> G10.2     
;               According to IDL help, IDL ignores this portion of the format
format1=strmid(format,0,strpos(format,'E',/reverse_search))
if format1 ne '' then begin
   if keyword_set(DEBUG) then print,'In LIST_mystruct. Changing data format from ',format,' to ',format1
   format=format1
endif

itrip=0
nc=0
new_form='        '
nvar=''
ivar=0
for i=0L, strlen(format)-1 do begin   
   ch=strupcase(strmid(format,i,1))
   ; Added format code 'D' Ron Yurow (8 Nov 2016)
   if(ch ne '(') and (ch ne 'A') and (ch ne 'F') and (ch ne 'P') and (ch ne 'D') and $
      (ch ne 'I') and (ch ne 'Z') and (ch ne 'G') and (ch ne 'E') and (ch ne 'B') then begin
      if(ivar eq 0) then nvar=nvar+ch
   endif
   ; Added format code 'D' Ron Yurow (8 Nov 2016)
   if(ch eq 'A') or (ch eq 'F') or (ch eq 'I') or (ch eq 'Z') or (ch eq 'G') or (ch eq 'B') $
      or (ch eq 'E') or (ch eq 'D') then begin
      form=ch
      itrip=1
      ivar=1
   endif 
   if(ch eq 'P') then ch=''
   if(ch eq ',') or (ch eq ')') then itrip=0
   if(itrip eq 1) then begin
      strput,new_form,ch,nc
      nc=nc+1
   endif
endfor   ; end i
format=strtrim(new_form,2)
formlen=strlen(format)-1
;endelse
;
case form of
   'null' : begin
            status=-1
            return, status
   end
   ; RCJ 11/23/05  We are setting formats F,E,G to G13.6 (or wider)
   ;   to accomodate possible fillvals in the data
   'B' : begin
         ; RCJ 04/29/2014.  The value 30 below might have to be adjusted.
         dat_len = 30 > strmid(format,1,formlen)
         dat_len = strmid(dat_len,6,formlen > 4)
   end
   'Z' : begin
         dat_len = 13.6 > strmid(format,1,formlen)
         dat_len = strmid(dat_len,6,formlen > 4)
   end
   'F' : begin
         dat_len = 13.6 > strmid(format,1,formlen)
         ;dat_len = strmid(dat_len,6,4)
	 ; RCJ 10/29/2007  Generalizing line above. Same for E and G below.
         dat_len = strmid(dat_len,6,formlen > 4)
   end
   ; Added little used 'D' format type.  Same as 'F' Ron Yurow (8 Nov 2016)
   'D' : begin
         dat_len = 13.6 > strmid(format,1,formlen)
         dat_len = strmid(dat_len,6,formlen > 4)
   end
   'E' : begin
         dat_len = 13.6 > strmid(format,1,formlen)
         ;dat_len = strmid(dat_len,6,4)
         dat_len = strmid(dat_len,6,formlen > 4)
   end
   'G' : begin
         dat_len = 13.6 > strmid(format,1,formlen)
         ;dat_len = strmid(dat_len,6,4)
         dat_len = strmid(dat_len,6,formlen > 4)
   end
   'I' : begin
         ;  Program caused arithmetic error: Floating illegal operand; where?
         if(n_elements(fillval) eq 0) then dat_len=strmid(format,1,formlen) else $
            dat_len=strlen(strtrim(string(fix(fillval)),2)) > strmid(format,1,3)
   end
   'A' : begin
         ;if(n_elements(fillval) eq 0) then dat_len=strmid(format,1,formlen) else $
         ;   dat_len=strlen(strtrim(fillval,2)) > strmid(format,1,3)       
         if(n_elements(fillval) eq 0) then dat_len=strmid(format,1,formlen) $
	 else begin
	    if size(fillval,/tname) eq 'DCOMPLEX' then $
	    ; RCJ 11/2006  This is the case of epoch for themis data
            dat_len=strlen(strtrim(real_part(fillval),2)) > strmid(format,1,3) else $
            dat_len=strlen(strtrim(fillval,2)) > strmid(format,1,3) 
	 endelse   
   end
   else : begin
          dat_len=0
   end
endcase
; RCJ 11/23/05  It has to be G format or fillvals will be ****:
if(form eq 'F') or (form eq 'E') then form='G' 
if(nvar ne '') then begin
   format=nvar+form+strtrim(dat_len,2)
   dat_len=fix(nvar)*fix(dat_len)
   nvar=fix(nvar)-1
endif else begin
   format=form+strtrim(dat_len,2)
   dat_len=fix(dat_len)
   nvar=0
endelse
frm_st=create_struct('status',status, 'form',format, 'dat_len',dat_len, $
                      'nvar',nvar) 
return, frm_st

end

;---------------------------------------------------------------------------------

function ep_conv, b, depd0, HANDLE=handle, sec_of_year=sec_of_year
; 
catch, error_status
if(error_status ne 0) then begin
   ;if(error_status eq -78) then $ 
   ;   print, 'STATUS=Available memory exceeded. Re-select time interval.'
   ;if(error_status eq -1039) then $ 
   ;   print, 'STATUS=Bad Epoch data type. Select longer time interval. '
   print, 'STATUS= Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (ep_conv).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   return,-1
   ;stop
endif
;
tagnames=tag_names(b)
v1=tagindex(depd0,tagnames)
if(n_elements(handle) eq 0) then handle=0

if(handle eq 0) then begin
   dat=b.(v1[0]).dat
   datsz=size(dat)
   if(datsz[0] gt 0) then dat=reform(dat) 
endif else begin 
   tmp=b.(v1[0]).HANDLE
   handle_value, tmp, dat
   datsz=size(dat)
   if(datsz[0] gt 0) then dat=reform(dat) 
endelse
len=size(dat)
;TJK 10/1/2009 - put in code to check for Epoch 16 values (dcomplex)
;if found, then print the extra time fields (micro, nano and pico)

;epoch_type = size(dat,/type)
epoch_type = b.(v1[0]).cdftype
case epoch_type of
  ;9: begin  ; complex
 'CDF_EPOCH16': begin  ; complex
   ep16=1
   tt2k=0
   if keyword_set(sec_of_year) then b.(v1[0]).units="Year____Secs-of-year" else $  ;  e.g. 2001 4585746.000  <-  microsec precision
      b.(v1[0]).units="dd-mm-yyyy hh:mm:ss.mil.mic.nan.pic"
 end
  ;14: begin  ;  long64
 'CDF_TIME_TT2000' : begin  ;  string
   ep16=0
   tt2k=1
   if keyword_set(sec_of_year) then b.(v1[0]).units="Year____Secs-of-year" else $  ;  e.g. 2001 4585746.000  <-  microsec precision
      b.(v1[0]).units="dd-mm-yyyy hh:mm:ss.mil.mic" 
 end
 else: begin
    ep16=0
    tt2k=0
   if keyword_set(sec_of_year) then b.(v1[0]).units="Year____Secs-of-year" else $  ;  e.g. 2001 4585746.000  <-  microsec precision
       b.(v1[0]).units="dd-mm-yyyy hh:mm:ss.ms"
 end
endcase

length=long(len[len[0]+2])
dat_eph=strarr(length)

for k=0L, length-1 do begin
   if keyword_set(sec_of_year) then begin
     if (ep16) then begin
      CDF_EPOCH16,dat[k], yr, mo, dy, hr, mn, sc, milli, micro, nano, pico, /break 
     endif   
     if (tt2k) then begin
      if (dat[k] eq b.(v1[0]).fillval) then begin
         yr=long(9999) & mo=long(12) & dy=long(31) & hr=long(23) & mn=long(59) & sc=long(59.99999999)
	 milli=long(0) & micro = long(0) & nano= long(0)
      endif else begin 
         CDF_TT2000,dat[k], yr, mo, dy, hr, mn, sc, milli, micro, nano, /break
         yr=long(yr) & mo=long(mo) & dy=long(dy) & hr=long(hr) & mn=long(mn) & sc=long(sc)
      endelse
     endif else begin   
       CDF_EPOCH,dat[k], yr, mo, dy, hr, mn, sc, milli, /break
       micro=(dat[k]-floor(dat[k],/l64))*1.e3
       nano=(micro-floor(micro,/l64))*1.e3
     endelse
     
     ical,yr,doy,mo,dy,/idoy
     doy=float(doy-1)  ;  if day=1 have to start from beginning of day, ie, not a whole day has passed at 00:05 of day 1, don't you agree?
     yrsec=double(sc)+double(mn)*60.+double(hr)*3600.+double(doy)*24.*3600.
     yrsec=yrsec+double(milli)/1.e3+double(micro)/1.e6

   endif else begin

     if (ep16) then begin
      CDF_EPOCH16,dat[k], yr, mo, dy, hr, mn, sc, milli, micro, nano, pico, /break 
     endif   
     if (tt2k) then begin
      if (dat[k] eq b.(v1[0]).fillval) then begin
         yr=long(9999) & mo=long(12) & dy=long(31) & hr=long(23) & mn=long(59) & sc=long(59.99999999)
	 milli=long(0) & micro = long(0) & nano= long(0)
      endif else begin 
         CDF_TT2000,dat[k], yr, mo, dy, hr, mn, sc, milli, micro, nano, /break
         yr=long(yr) & mo=long(mo) & dy=long(dy) & hr=long(hr) & mn=long(mn) & sc=long(sc)
      endelse	 
     endif else begin   
       CDF_EPOCH,dat[k], yr, mo, dy, hr, mn, sc, milli, /break
       micro=(dat[k]-floor(dat[k],/l64))*1.e3
       nano=(micro-floor(micro,/l64))*1.e3
     endelse 
   endelse 

   if(dy lt 10) then dy= '0'+strtrim(dy,2) else dy=strtrim(dy,2)
   if(mo lt 10) then mo= '0'+strtrim(mo,2) else mo=strtrim(mo,2)
   if(hr lt 10) then hr= '0'+strtrim(hr,2) else hr=strtrim(hr,2)
   if(mn lt 10) then mn= '0'+strtrim(mn,2) else mn=strtrim(mn,2)
   if(sc lt 10) then sc= '0'+strtrim(sc,2) else sc=strtrim(sc,2)
   milli=strmid(strtrim(float(milli)/1000.,2),2,3)
   yr=strtrim(yr,2)
   
   if keyword_set(sec_of_year) then begin
     yrsec_str=string(yrsec,format='(f15.6)')
     dat_eph[k]=yr+' '+yrsec_str
   endif else begin
     if (ep16) then begin
       micro=strmid(strtrim(float(micro)/1000.,2),2,3)
       nano=strmid(strtrim(float(nano)/1000.,2),2,3)
       pico=strmid(strtrim(float(pico)/1000.,2),2,3)
       dat_eph[k]=dy+'-'+mo+'-'+yr+' '+hr+':'+mn+':'+sc+'.'+milli+'.'+micro+'.'+nano+'.'+pico   
     endif    
     if (tt2k) then begin
       micro=strmid(strtrim(float(micro)/1000.,2),2,3)
       nano=strmid(strtrim(float(nano)/1000.,2),2,3)
       dat_eph[k]=dy+'-'+mo+'-'+yr+' '+hr+':'+mn+':'+sc+'.'+milli+'.'+micro+'.'+nano
     endif else begin
       dat_eph[k]=dy+'-'+mo+'-'+yr+' '+hr+':'+mn+':'+sc+'.'+milli
     endelse
   endelse  
endfor
eptmp=create_struct('DATEPH',dat_eph)
return, eptmp
;
end 

;+ 
; NAME:  LIST_mystruct.pro
;
; PURPOSE:  Generates a list output for CDAWweb
;
; CALLING SEQUENCE:
;
; FUNCTION LIST_mystruct, a,NOGATT=nogatt,NOVATT=novatt,NORV=norv,$
;                         NONRV=nonrv,NO2DRV=no2drv,FILENAME=filename,CSV=csv,$
;                         TSTART=TSTART,TSTOP=TSTOP,MAXRECS=maxrecs
;  
; VARIABLES:
;
; Input:
;
;  a        - an IDL structure
; 
; Keyword Parameters:
;
;  nogatt   - Global attributes output: =0 (print), =1 (no print)
;  novatt   - Variable attributes output: =0 (print), =1 (no print)
;  norv     - Record varying output: =0 (print), =1 (no print) 
;  nonrv    - Non record varying output: =0 (print), =1 (no print)
;  no2drv   - 2D record varying output: =0 (print), =1 (no print)
;  filename - Output filename 
;  maxrecs  - Maximum record output
;  csv      - =0 (regular .txt file), =1 (.csv data+header output), =2 (.csv data and .json header output), =3 (.json data+header output)
;
; REQUIRED PROCEDURES:
;
; HISTORY
;
; Initial version: 
;
;         1.0  R. Baldwin  HSTX           2/9/96
;
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;
FUNCTION LIST_mystruct, a,NOGATT=nogatt,NOVATT=novatt,NORV=norv,$
                        NONRV=nonrv,NO2DRV=no2drv,FILENAME=filename,CSV=csv,$
                        TSTART=TSTART,TSTOP=TSTOP, START_msec=start_msec, STOP_msec=stop_msec,$
			MAXRECS=maxrecs, SEC_OF_YEAR=sec_of_year, $
                        REPORT=REPORT,STATUS=STATUS,DEBUG=DEBUG
;
; Set input values if undefined 
;
;TJK 5/18/00 - Modified to allow 100,000 records to be listed - greatly
;increased from 30,000 - might have to back this off some should we start
;to see performance impacts.
;


compile_opt idl2

COMMON SHARE, csv1 ;Added to assist in applying proper labels to .txt or .csv files - CWG 04/12/2018
; Define all the other common blocks here as well.  Variables will be available globally.
; Ron Yurow (Jan 4, 2022)
COMMON SHARE1, jsonstruct
COMMON SHARE3, jsondata
COMMON SHARE4, nanExists
 
if (keyword_set(CSV)) then csv1 = csv else csv1 = 0

status=0
reportflag = 0
if(n_elements(nogatt) eq 0) then nogatt=0
if(n_elements(novatt) eq 0) then novatt=0
if(n_elements(norv) eq 0) then norv=1
if(n_elements(nonrv) eq 0) then nonrv=1
if(n_elements(no2drv) eq 0) then no2drv=1
if(n_elements(no3drv) eq 0) then no3drv=1
if(n_elements(no4drv) eq 0) then no4drv=1
if(n_elements(noimg) eq 0) then noimg=1
if(n_elements(filename) eq 0) then filename='cdaweb_listing.asc' 
;if(n_elements(maxrecs) eq 0) then maxrecs=150000 ;TJK changed from 30000
if(n_elements(maxrecs) eq 0) then maxrecs=15000000 ;TJK changed from 150000 for testing on new machine
;if(n_elements(maxrecs) eq 0) then maxrecs=100000 ;TJK changed from 30000
; REPORT and reportflag no longer used;
if(n_elements(REPORT) eq 0) then report=''
; statusflag not implemented yet!
if(n_elements(STATUS) eq 0) then statusflag=1L else statusflag=0L 
if(n_elements(DEBUG) eq 0) then debugflag=1L else debugflag=0L 

; Establish error handler
catch, error_status
if(error_status ne 0) then begin
   print, 'STATUS= Data cannot be listed.'
   print, 'ERROR=Error number: ',error_status,' in listing (LIST_mystruct).'
   print, 'ERROR=Error Message: ', !ERR_STRING
   ;if(error_status eq -98) then begin
   ;   if reportflag then printf, 1, 'STATUS=Data space too large. Cannot currently list these data.'
   ;   print, 'STATUS=Data space too large. Cannot currently list these data.'
   ;endif else begin
   ;   if reportflag then printf, 1, 'STATUS= Data cannot be listed. '
   ;   print, 'STATUS=Data cannot be listed. '
   ;endelse
   close,1
   return, -1 
endif
; Open report file
if(REPORT) then begin
   openw, 1, REPORT, error=err
   if(err ne 0) then begin
      print, "ERROR=",!ERR_STRING 
      close, 1 & return, -1
   endif
   reportFlag = 1
endif
if(keyword_set(DEBUG)) then print, 'Opening output file=', filename
;
; Open output file
;

;CWG - Added to be user-proof, not allowing them to give bad filenames that would throw errors
; RCJ 06Jan2023  Not sure what these errors could be but if the user
;     set a filename s/he will expect to see that filename
if not keyword_set(filename) then begin
  if csv1 LT 1 then extension='.txt' else if ((csv1 EQ 1) || (csv1 EQ 2)) then extension='.csv' else if csv1 GE 3 then extension='.json' 
  pos = strpos(filename, '.')
  filename = filename.Remove(pos)+extension
endif
   
openw, unit, filename, /get_lun,error=err,width=1000

if(n_elements(a) eq 0) then begin
   if reportflag then printf, 1, 'STATUS= Data cannot be listed.'
   print, 'STATUS=Data cannot be listed.'
   print, 'ERROR=Error: Undefined structure' 
   close, 1
   return, -1 
endif

; Add Code to trap a=-1 bad structures  RTB
str_tst=size(a)
if(str_tst[str_tst[0]+1] ne 8) then begin
   v_data='DATASET=UNDEFINED'
   v_err='ERROR=Input is not a stucture.'
   v_stat='STATUS=Cannot list this data'
   ; a=crate_struct('DATASET',v_data,'ERROR',v_err,'STATUS',v_stat)
   print, v_data
   print, v_err
   print, v_stat
   return, 0
endif else begin
   ; Test for errors trapped in read_myCDF
   atags=tag_names(a)
   rflag=tagindex('DATASET',atags)
   if(rflag[0] ne -1) then begin
      print, a.DATASET
      print, a.STATUS
      return, 0
   endif
   ;
   ; RCJ 06/09/2004. Testing number of columns. Large images
   ; yield more than 32767. columns and IDL will not print
   ; that much (and it will take a looong time to process). 
   ; This test had to be done here, before 'LISTING=' because parse.ph
   ; will register a 'system error' if there is a filename but nothing
   ; is listed.
   for i=0,n_elements(atags)-1 do begin
      if strupcase(a.(i).var_type) eq 'DATA' or $
         strupcase(a.(i).var_type) eq 'SUPPORT_DATA' then begin
	 aatags=tag_names(a.(i))
         q=where(aatags eq 'HANDLE')
	 if q[0] ne -1 then handle_value,a.(i).handle,testarr $
	    else testarr=a.(i).dat
	 sz=size(testarr)
	 if sz[0] eq 3 then begin
	    if sz[1]*sz[2] gt 32767. then begin
	       a.(i).var_type = 'ignore_data'
	       print,'STATUS=3D array too big. Will not list.'
	    endif  
	 endif   	
      endif	
   endfor
   ;
   ;
   ; There's nothing to list unless at least one of the variables is var_type=data 
   ; reuse variable rflag:    RCJ 09/16/02
   rflag=''
   for i=0,n_elements(atags)-1 do rflag=[rflag,a.(i).var_type]
   q=where(strupcase(rflag) eq 'DATA')
   if q[0] eq -1 then return,0 
endelse  

; Write DATASET=
data_set=''
;2/25/2016 - TJK - no longer want to use logical_file_id here at all
;if(data_set eq '') then begin 
   ; RCJ 03/13/2003 Some datasets are larger than 9 characters.
   ; Using strsplit to separate the string at the '_' and rejoin the dataset name only.
   ;data_set=strmid(a.(0).LOGICAL_FILE_ID,0,9)
   ; RCJ 06/27/2013 Logical_file_id is a required attribute but it may not be there
   ;    so I added this test:
;2/25/2016 - TJK - no longer want to use logical_file_id here at all
;   s=tagindex('LOGICAL_FILE_ID',tag_names(a.(0)))
;   if s[0] ne -1 then begin
;      s=strsplit(a.(0).LOGICAL_FILE_ID,'_',/extract)
;      if n_elements(s) gt 1 then begin  ; only continue if '_' exists in logical_file_id
;         data_set=s[0]+'_'
;         for i=1,n_elements(s)-4 do begin
;            data_set=data_set+s[i]+'_'
;         endfor	 
;         data_set=strupcase(data_set+s[n_elements(s)-3]) 
;         ;data_set=strupcase(data_set[0]+'_'+data_set[1]+'_'+data_set[2])
;      endif
;   endif
;endif
s=tagindex('LOGICAL_SOURCE',tag_names(a.(0)))
if s[0] ne -1 then begin
  if(data_set eq '') then begin
   data_set=strtrim(a.(0).LOGICAL_SOURCE,2)
   data_set=strupcase(data_set)
  endif
endif  

if(data_set eq '') then data_set='UNDEFINED'

if reportflag then printf, 1, 'DATASET=',data_set
print, 'DATASET=',data_set
; Write file name to REPORT file
if reportflag then printf, 1, 'LISTING=',filename
split=strsplit(filename,'/',/extract)
loutdir='/'
for t=0L,n_elements(split)-2 do loutdir=loutdir+split[t]+'/'


print, 'LIST_OUTDIR=',loutdir
fmt='(a10,a'+strtrim(strlen(split[t]),2)+')'
print, 'LONG_LIST=',split[t], format=fmt

;
; Reform dat arrays w/in structure.
;
if(keyword_set(DEBUG)) then print, 'Reform arrays w/in structure.'
a=reform_strc(a)
;
; Separate variables by their depend_0; build mega-structure
;
mega=parse_mydepend0(a)
depends=tag_names(mega)

; JBS - below forloop was to make sure time series are setup correctly. 
b=tag_names(a)
for i=1, mega.num do begin
    d0=depends[i]
    v1 = tagindex(d0, b)
    vtype = a.(v1).VAR_TYPE
    if(vtype ne 'data' and vtype ne 'support_data') then begin
        print, "STATUS= ", d0, " cannot have a var_type of ", a.(v1).var_type, ". Change needed in master to support_data for time variables."
        return, -1
    endif
endfor 
mega_json = LIST ()

; We can't rely on the mega_loop interator variable being 1 for opening files
; since interations may be aborted due to there being no listable variables.
; So lets use this variable instead.  This will only be cleared when we get to
; the end of the loop.
; Ron Yurow  (Jan 4, 2022)
first_iteration = 1 

for mega_loop=1, mega.num do begin
   a=mega.(mega_loop)

   ; Set jsonstruct and jsondata to NULL so that we don't try to process data
   ; from a previous loop iteration.
   ; Ron Yurow (Jan 4, 2022)
   jsonstruct = !NULL
   jsondata = !NULL

   depend0=depends[mega_loop]
   if(depend0 eq ' ') then continue
   ;
   ; Determine Global and variable attribute structures for listing
   ;
   ns_tags=n_tags(a)
   namest=strupcase(tag_names(a))
   ; Determine location of Epoch variable
   if(keyword_set(DEBUG)) then print, 'Find DEPEND_0'
   incep=-1
   incep=where(namest eq depend0,w)
   ; Remove any nasty variables
   v1=tagindex(depend0,namest)
   if(v1[0] ne -1) then begin
      station=a.(v1[0]).source_name 
      station=strmid(station,0,4)
      v1=tagindex('delay_time',namest)
      if (((station eq "ISIS")or(station eq "ALOU")) and (v1[0] ne -1)) then a.DELAY_TIME.var_type="metadata"
   endif else begin
      print, 'ERROR= Tag name not found'
      return, -1
   endelse

   ; No record varying attribute found
   incep=incep[0]
   if(incep eq -1) then begin
      if reportflag then printf, 1, 'STATUS= Data cannot be listed. '
      print, 'STATUS= Data cannot be listed. '
      print, 'ERROR=Error: No record varying data selected' 
      close, 1
      return, -1 
   endif

   ; RCJ 21Jun2024 If record varying IS found but it's only one element and it's fillval:
   handle_value,a.(incep[0]).handle,val
   if (n_elements(val) eq 1 and val[0] eq a.(incep[0]).fillval) then begin
     mega.num=mega.num-1
     if (csv1 LE 1) then  begin
        warning='#  WARNING: No epoch record for this time period for at least one variable. '
        printf, unit, format='(a)',warning
     endif	
     if mega.num gt 0 then continue
   endif   

   ; Check if the data set was binned.  We will need this later.
   ; Ron Yurow (Nov 4, 2020)
   binning_flag = check_binning_status (a) 

   ; Add loop to create a list of other dependent variables besides DEPEND_0.  We need to do
   ; this because there may be variables specified as DELTA_PLUS_VAR, etc. that need to be 
   ; part of the listing.
   ; Ron Yurow  (May 13, 2019)

   ; List of attributes that may indicate a dependent variable.
   ; Note:  Do not change this order.  It necessary for ascessing if the dependent varaible
   ; is a DEPEND_X or something else.  (Needed for NRV conversion of binned variables)
   depend_attrib = ['DEPEND_1', 'DEPEND_2', 'DEPEND_3', 'DELTA_MINUS_VAR', 'DELTA_PLUS_VAR']

   ; Set the max level of depend variables that we can work with. Currently this is 3.
   ; Ron Yurow (Nov 4, 2020)
   max_depend_dim = 3

   n_depend_attrib = N_ELEMENTS (depend_attrib)

   ; List of variables to allow to be listed, even though they have no DEPEND_0 and are not
   ; NRV. 
   allowed_list  = ['']

   ; Create a list of pointers into the array of dependency attributes.  As we find each 
   ; depenedent variable, we will add a pointer to the attribute it is pointed to by.
   ; This should sync with the allowed_list array.
   ; Ron Yurow (Nov 4, 020)
   depend_lst_ptr = [""]

   ; Flag to short circuit checking of the allowed list, in case its empty.
   allowed_list_empty = 1

   ; Loop through all variables.  If any variable has an attribute in depend_atrib[], then add
   ; variable the attribute points to the array allowed_list[].  Yes, this may result in mutilple
   ; copies of the same variable name, but we sort that out later. 
   FOR varcheck = 0, N_ELEMENTS (namest) - 1 DO BEGIN
       attrib_names = TAG_NAMES (a.(varcheck))
       FOR n_attrib = 0, n_depend_attrib - 1 DO BEGIN
           pos = WHERE (attrib_names eq depend_attrib [n_attrib], found)
           IF  found gt 0 && STRLEN (a.(varcheck).(pos)) gt 0 THEN BEGIN
               allowed_list = [allowed_list, a.(varcheck).(pos)]
               depend_lst_ptr = [depend_lst_ptr, n_attrib]
           ENDIF
       ENDFOR 
   ENDFOR
   
   ; Trim, sort and remove duplicates if we found anything at all.
   IF  N_ELEMENTS (allowed_list) gt 1 THEN BEGIN

       allowed_list = allowed_list [1:*]
       ; Also do the list pointers into the dependency list 
       ; Ron Yurow (Nov 19, 2020)
       ; allowed_list = allowed_list [UNIQ (allowed_list, SORT (allowed_list))]
       depend_lst_ptr = depend_lst_ptr [1:*]

       sort_order = UNIQ (allowed_list, SORT (allowed_list))
       depend_lst_ptr = depend_lst_ptr [sort_order]
       allowed_list   = allowed_list [sort_order]
       
       allowed_list_empty = 0
   ENDIF
   
   ; Convert any dependendent variables that were reduced by the time based binning software
   ; to NRV.  Do this by changing the CDFRECVARY attribute to NOVARY.
   ; Ron Yurow (Nov 4, 2020)
   IF  binning_flag && ~allowed_list_empty THEN BEGIN
       ; Loop should itterate to end of depened_lst_ptr array.
       ; Ron Yurow (Nov 19, 2020)
       ; FOR depend = 0, max_depend_dim - 1 DO BEGIN
       FOR depend = 0, N_ELEMENTS (depend_lst_ptr) - 1 DO BEGIN
           IF depend_lst_ptr [depend] lt max_depend_dim THEN BEGIN

              depend_var_ind = WHERE (STRUPCASE (allowed_list [depend]) eq namest, cnt) 

              IF cnt eq 1 THEN  a.(depend_var_ind).CDFRECVARY = 'NOVARY'

           ENDIF
       ENDFOR 
   ENDIF 
   
   ; Check for variables that do not have a DEPEND_0 (not time variant) and is not NRV, then we
   ; can not print the data for this variable, since it is impossible to assign records to time
   ; stamps.
   ; Ron Yurow   (May 3, 2019)
   ; Loop through all the variables in the mega.
   FOR varcheck = 0, N_ELEMENTS (namest) - 1 DO BEGIN

       ; If the variable is the DEPEND_0, then don't worry about it.
       IF  varcheck eq incep THEN CONTINUE

       ; Check if there is a valid allowed_list.  If there is, then check if the variable
       ; is on the list.  If it is, then no need to check any further.
       ; Ron Yurow (May 13, 2019)
       IF  ~ allowed_list_empty THEN BEGIN
           sink = WHERE (a.(varcheck).varname eq allowed_list, exception)

           ; IF  exception ne 0 THEN CONTINUE 
           IF  exception ne 0 THEN BEGIN
               CONTINUE
           ENDIF
       ENDIF

       ; Check if the variable is NRV.  If it is, then don't worry about it either.      
       IF  STRUPCASE (a.(varcheck).cdfrecvary) eq 'NOVARY' THEN BEGIN
           ; Seems to be that NRVs can have var_types other then 'support_data' so lets
           ; just eliminate this requirement for now.
           ; Ron Yurow  (May 6, 2019)
           ; IF STRLOWCASE (a.(varcheck).var_type) eq 'support_data' THEN CONTINUE
           CONTINUE
       ENDIF 

       ; Check if the variable has a DEPEND_0.  If it doesn't, then change the VAR_TYPE to
       ; 'ignore_data' so it won't be ploted.
       IF  STRUPCASE (a.(varcheck).depend_0) ne STRUPCASE (depend0) THEN BEGIN
           a.(varcheck).var_type = 'ignore_data'
           ;  Check if the variable has the same name as another variable, only with the extension
           ; '_bin'  These are probably epochs left over from the binning software, so no need to 
           ; issue a warning when one occurs.
           ; Ron Yurow (Dec 3, 2021)
           sink = WHERE (namest [varcheck] + "_BIN" eq namest, cnt)
           IF  cnt eq 0 THEN BEGIN
               if(keyword_set(DEBUG)) then PRINT, 'STATUS= Variable ' + a.(varcheck).varname + ' is not NRV and does not have a ' + $
                      'DEPEND_0 attribute.  Setting VAR_TYPE to "ignore_data" to prevent listing.'
           ENDIF

       ENDIF
       
   ENDFOR 

   ; Check again to make sure that we have at least one variable with
   ; with type data.
   ; Ron Yurow (August 10, 2020)
   datafound = 0   
   FOR tagno = 0, N_TAGS (a) - 1 DO BEGIN

       IF  STRUPCASE (a.(tagno).VAR_TYPE) eq "DATA" THEN BEGIN
           datafound = 1
           BREAK
       ENDIF

   ENDFOR

   IF  ~datafound THEN CONTINUE

   ; Create new structure w/ data (w/o handle) and determine size of Data array
   ; Epoch or depend_0 will be the first variable processed

   if(keyword_set(DEBUG)) then print, 'Converting handles; Compute size;',$
         ' Build new structure.'
   names=tag_names(a.(incep))
   ntags=n_tags(a.(incep))
   ; Check to see if HANDLE a tag name
   wh=where(names eq 'HANDLE',whn)
   if(whn) then begin
      handle_value, a.(incep).HANDLE, dat
      datsz=size(dat)
      if(datsz[0] gt 0) then dat=reform(dat) 
      ; Convert Epoch info. to string
      ;  'EPOCH' or 'EPOCH92' etc.
      if(namest[incep] eq depend0) then begin
         eptmp=ep_conv(a,depend0,/handle,sec_of_year=sec_of_year)
	 
	 ;RCJ24   Trying to fix for the case of one time record. IDL does not like
	 ;       dimensions of size 1 so tricking it by duplicating the data.
	 ;       At the time of actually writing it to the file we ignore the second record.
	 if n_elements(dat) eq 1 then begin
	   datorig=dat
	   dat=[dat,dat]
	 endif
	 ;
         temp=create_struct('DAT',dat)
         temp1=create_struct(a.(incep),temp)
         temp2=create_struct(temp1,eptmp)
         b=create_struct(namest[incep],temp2)
      endif else begin
	 ;RCJ24
	 if n_elements(dat) eq 1 then begin
	   datorig=dat
	   dat=[dat,dat]
	 endif
	 ;
         temp=create_struct('DAT',dat)
         temp1=create_struct(a.(incep),temp)
         b=create_struct(namest[incep],temp1)
      endelse
      ;
   endif else begin
      ; Convert Epoch info. to string
      ; 'EPOCH' or 'EPOCH92' etc.
      if(namest[incep] eq depend0) then begin
         eptmp=ep_conv(a,depend0,sec_of_year=sec_of_year) 
         tmp=create_struct(a.(incep),eptmp)
         b=create_struct(namest[incep],tmp)
      endif else begin 
         b=create_struct(namest[incep],a.(incep))
      endelse
   endelse
   
   vorder=intarr(ns_tags)
   vorder[*]=-1
   vorder_data=intarr(ns_tags)
   for k=0, ns_tags-1 do begin
      if(k ne incep) then begin
         names=tag_names(a.(k))
         ntags=n_tags(a.(k))
         whc=where(names eq 'HANDLE',whn)
         if(whn) then begin
            handle_value, a.(k).HANDLE, dat
	    ;
	    ; RCJ24 ; for one time record, if 2d or 3d data associated to it,
	    ;         stretch into one long array, duplicate it and reform again.
	    if a.(k).var_type eq 'data' and n_elements(datorig) eq 1 then begin
	     datb=reform(dat,n_elements(dat),1)
	     datbb=datb#REFORM(intarr(2)+1,1,2)
	     sz=size(dat)
	     case sz[0] of
	      0: begin
	       dat=reform(datbb,sz[2],2)
	       end
	      1: begin
	       dat=reform(datbb,sz[1],sz[0],2)
	       end 
	      2: begin
	       dat=reform(datbb,sz[1],sz[2],2)
	       end
	      3: begin
	       dat=reform(datbb,sz[1],sz[2],sz[3],2)
	       end
	      else: 
	     endcase 
            endif
	    ;
            datsz=size(dat)
            if(datsz[0] gt 0) then dat=reform(dat)
            temp=create_struct('DAT',dat)
            temp1=create_struct(a.(k),temp)
            temp2=create_struct(namest[k],temp1)
            b=create_struct(b,temp2)
         endif else begin
            temp=create_struct(namest[k],a.(k))
            b=create_struct(b,temp)
         endelse
      endif
      st_sz=size(b.(k).DAT)
      vorder[k]=st_sz[0]
      if strupcase(b.(k).var_type) eq 'DATA' then vorder_data[k]=st_sz[0]
   endfor   ; end k
   ; Free Memory
   spdf_delete, a
   spdf_delete, temp
   spdf_delete, temp1
   spdf_delete, temp2
   spdf_delete, tmp
   ;
   if(keyword_set(DEBUG)) then print, 'Determine type of listing.'
   ; Determine type of listing
   ; plist=max(vorder)
   plist=max(vorder_data)
   ;TJK change to 1st variable since not all CDF's have a time variable called
   ;"EPOCH"    if(plist eq 3) then station=strmid(b.epoch.source_name,0,4)
   if(plist eq 3) then station=strmid(b.(0).source_name,0,4)
   ;
   if(plist eq 0) then begin
      ;norv=1 & no2drv=1 & nonrv=1 & no3drv=1 & noimg=1
      ; RCJ24
      norv=0 & no2drv=1 & nonrv=1 & no3drv=1 & noimg=1
   endif
   if(plist eq 1) then begin
      norv=0 & no2drv=1 & nonrv=1 & no3drv=1 & noimg=1 
   endif
   if(plist eq 2) then begin
      norv=1 & no2drv=0 & nonrv=1 & no3drv=1 & noimg=1 
   endif
   if(plist eq 3) then begin
      if(station ne "DARN") then begin
         norv=1 & no2drv=1 & nonrv=1 & no3drv=1 & noimg=0
      endif else begin
         norv=1 & no2drv=1 & nonrv=1 & no3drv=0 & noimg=1
      endelse
   endif
   if(plist eq 4) then begin ; e.g.: Array[72, 16, 5, 3514]
       norv=1 & no2drv=1 & nonrv=1 & no3drv=1 & no4drv=0 & noimg=1
   endif
   if(plist gt 4) then begin 
      if reportflag then printf, 1, 'STATUS= Data of 4D or less can be listed. Re-select variables'
      print, 'STATUS= Data of 3D or less can be listed. Re-select variables'
      close, 1 
      return, -1 
   endif
   ; Reorder structue
   b_tagnames=tag_names(b)
   v1=tagindex(depend0,b_tagnames)
   if(v1[0] eq -1) then begin
      print, 'ERROR= No tag found for DEPEND0'
      return, -1
   endif
   epsz=size(b.(v1[0]).dat)
   if(epsz[0] eq 0) then b=ord_mystruct(b,vorder,0) else $
                       b=ord_mystruct(b,vorder,1)

   ; Reform Image 3D data arrays
   if(noimg eq 0 and keyword_set(DEBUG)) then print, 'Reform 3D Image arrays.'
   if(noimg eq 0) then  b=reform_mystruct(b)
   
   ; Reform Image 4D data arrays
   ; This will make the 4D data into 2D, ie, one long line for each time element.
   ; Visually, we are going to stretch the 3D data cube into one looooong line of numbers.
   if(no4drv eq 0 and keyword_set(DEBUG)) then print, 'Reform 4D Image arrays.'
   if(no4drv eq 0) then  b=reform_mystruct(b)
   
   ; Set/Convert tstart and tstop 
   if(keyword_set(DEBUG)) then print, 'Set/Convert tstart and tstop.'
   tmpoch=b.(v1[0]).dat
   leng=n_elements(tmpoch)
   if((n_elements(TSTART) eq 0) or (n_elements(TSTOP) eq 0)) then begin
      if(leng gt 1) then begin
         TSTART=tmpoch[0]
         TSTOP=tmpoch[leng-1]
      endif else begin
         tmp=tmpoch
         tmpoch=fltarr(1)
         tmpoch[0]=tmp
         TSTART=tmpoch[0]
         TSTOP=tmpoch[0]
      endelse
   endif

   ; Set time constraints
   start_time = 0.0D0 ; initialize
   stop_time = 0.0D0 ; initialize
   if keyword_set(TSTART) then begin ; determine datatype and process if needed
      b1 = size(TSTART) & c1 = n_elements(b1)
      case (b1[c1-2]) of
         5: start_time=tstart  ; double float
	 9: start_time=tstart  ; dcomplex
	 7: begin  ; string
	    case (size(tmpoch[0],/type)) of
	       '14': begin
	                start_time=encode_cdfepoch(tstart,/tt2000,msec=start_msec)
		     end
	       else: begin
	          if keyword_set(start_msec) then $
		  start_time=encode_cdfepoch(tstart,msec=start_msec) else $
	          start_time=encode_cdfepoch(tstart)
	       end  
	    endcase
            end
	 else: begin
	   print,'ERROR=TSTART parameter must be STRING, DOUBLE or DCOMPLEX' & close, 1
           return,-1 
	   end
      endcase
   endif

   if keyword_set(TSTOP) then begin ; determine datatype and process if needed
      b1 = size(TSTOP) & c1 = n_elements(b1)
      case (b1[c1-2]) of
         5: stop_time=tstop  ; double float
	 9: stop_time=tstop  ; dcomplex
	 7: begin   ;string
	    case (size(tmpoch[0],/type)) of
	       '14': begin
	                stop_time=encode_cdfepoch(tstop,/tt2000,msec=stop_msec)
		     end
	       else: begin
	          if keyword_set(stop_msec) then $
		  stop_time=encode_cdfepoch(tstop,msec=stop_msec) else $
	          stop_time=encode_cdfepoch(tstop)
	       end  
	    endcase
            end
	 else: begin
	   print,'ERROR=TSTOP parameter must be STRING, DOUBLE or DCOMPLEX' & close, 1
           return,-1 
	   end
      endcase
   endif

   ; Restrict long listings giving user some information to gage their
   ; work

   time_dif=stop_time-start_time
   time_dif=time_dif/(86400.*1000.)
   tnum=n_elements(tmpoch)-1
   ; Try a more accurate method for determining time interval
   ;dif_ep=tmpoch(1)-tmpoch[0]
   if(tnum gt 100) then begin
      idcs=findgen(99) 
      idct=findgen(99)+1
      difs=tmpoch[idct]-tmpoch[idcs] 
      mnval=min(difs)
      mxval=max(difs)
      if(mnval ne mxval) then begin
         dif_ep=moment(difs)
      endif else begin
         ;dif_ep=fltarr(1)
	 ; Input to cdf_epoch has to be double:
         dif_ep=dblarr(1)
         dif_ep[0]=difs[0]
      endelse
      if (size(tmpoch[0],/type) eq 14) then $
        cdf_epoch,dif_ep[0]*1.D0,y1,mo1,d1,h1,m1,s1,mi1,/break,/tointeger else $
        cdf_epoch,dif_ep[0],y1,mo1,d1,h1,m1,s1,mi1,/break
      deltime=strtrim(h1,2)+':'+strtrim(m1,2)+':'+strtrim(s1,2)+'.'+strtrim(mi1,2)
   endif
   ;TJK 8/30/2004 - add code to determine the number of columns of data being requested - 
   ;and kick out on that, in addition to the number of records (below).

   ;TJK 9/21/2004 - find the depend_1 and depend_2 variables and then look at their sizes
   ; in order to determine the actual number of columns of data requested

   for vars = 0, n_tags(b)-1 do begin
      dep1=dependn_search(b,vars,1)
      if (dep1[0] ne '') then begin
	depend1 = b.(vars).depend_1
        ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
        q=where(tag_names(b.(vars)) eq 'ALT_CDAWEB_DEPEND_1')
        if (q[0] ne -1) then if (b.(vars).alt_cdaweb_depend_1 ne '') then depend1=b.(vars).alt_cdaweb_depend_1 
	if (n_elements(dep1_values) eq 0) then dep1_values = depend1 else $
	dep1_values=[dep1_values,depend1]
      endif
      dep2=dependn_search(b,vars,2)
      if (dep2[0] ne '') then begin
	depend2 = b.(vars).depend_2
        ; RCJ 05/16/2013  If alt_cdaweb_depend_2 exists, use it instead:
        q=where(tag_names(b.(vars)) eq 'ALT_CDAWEB_DEPEND_2')
        if (q[0] ne -1) then if (b.(vars).alt_cdaweb_depend_2 ne '') then depend2=b.(vars).alt_cdaweb_depend_2 
	if (n_elements(dep2_values) eq 0) then dep2_values = depend2 else $
	dep2_values=[dep2_values,depend2]
      endif
      dep3=dependn_search(b,vars,3)
      if (dep3[0] ne '') then begin
	depend3 = b.(vars).depend_3
        q=where(tag_names(b.(vars)) eq 'ALT_CDAWEB_DEPEND_3')
        if (q[0] ne -1) then if (b.(vars).alt_cdaweb_depend_3 ne '') then depend3=b.(vars).alt_cdaweb_depend_3 
	if (n_elements(dep3_values) eq 0) then dep3_values = depend3 else $
	dep3_values=[dep3_values,depend3]
      endif
   endfor

   ;sort and uniq the arrays to get rid of duplicates
   if (n_elements(dep1_values) gt 0) then begin
     dep1_idx = uniq(dep1_values,sort(dep1_values))
     dep1_values = dep1_values[dep1_idx]
   endif
   if (n_elements(dep2_values) gt 0) then begin
     dep2_idx = uniq(dep2_values,sort(dep2_values))  
     dep2_values = dep2_values[dep2_idx]
     ;     print, 'DEBUG - dep1_values = ',dep1_values, 'dep2_values = ',dep2_values
   endif
   if (n_elements(dep3_values) gt 0) then begin
     dep3_idx = uniq(dep3_values,sort(dep3_values))
     dep3_values = dep3_values[dep3_idx]
   endif
   cols = 0 & d1cols = 0 & d2cols = 0 & d3cols = 0
   b_tagnames=tag_names(b)
   ;Find the number of records
   idx = tagindex(depend0,b_tagnames)
   var_size = size(b.(idx).dat)
   n_recs = var_size[1]

   for vars = 0, n_elements(dep1_values)-1 do begin
	var_idx = tagindex(dep1_values[vars],b_tagnames)
        if (var_idx ge 0) then begin 
	  var_size = size(b.(var_idx).dat)
	  if (strupcase(b.(var_idx).var_type) eq 'SUPPORT_DATA') then begin
	    if (var_size[0] eq 1) then d1cols = d1cols + var_size[1] ; support data like rows and 
							       ; columns for an image variable
            if (var_size[0] eq 2) then d1cols = d1cols+ var_size[1] ;spectrogram type vars.
	  endif 
	endif
   endfor

   for vars = 0, n_elements(dep2_values)-1 do begin
	var_idx = tagindex(dep2_values[vars],b_tagnames)
        if (var_idx ge 0) then begin 
	  var_size = size(b.(var_idx).dat)
	  if (strupcase(b.(var_idx).var_type) eq 'SUPPORT_DATA') then begin
	    if (var_size[0] eq 1) then d2cols = d2cols + var_size[1] ; support data like rows and 
							       ; columns for an image variable
            if (var_size[0] eq 2) then d2cols = d2cols+ var_size[1] ;spectrogram type vars.

	  endif
	endif
   endfor
   for vars = 0, n_elements(dep3_values)-1 do begin
	var_idx = tagindex(dep3_values[vars],b_tagnames)
        if (var_idx ge 0) then begin 
	  var_size = size(b.(var_idx).dat)
	  if (strupcase(b.(var_idx).var_type) eq 'SUPPORT_DATA') then begin
	    if (var_size[0] eq 1) then d3cols = d3cols + var_size[1] ; support data like rows and 
							       ; columns for an image variable
            if (var_size[0] eq 2) then d3cols = d3cols+ var_size[1] ;spectrogram type vars.

	  endif
	endif
   endfor
   cols = d1cols + d2cols + d3cols

   if (cols eq 0) then begin ;have to look at the data variables to decide # cols
     ;     print, 'TJK DEBUG - No depend1 or depend2 values, so looking at the data vars to determine cols'
     for vars = 0, n_elements(b_tagnames)-1 do begin
       if (strupcase(b.(vars).var_type) eq 'DATA') then begin
	 var_size = size(b.(vars).dat)
         if (var_size[0] eq 1) then cols = cols+ 1 ;regular DATA variable - scalar
         if (var_size[0] eq 2 and var_size[1] lt 10) then cols = cols+ var_size[1] ;vectors
       endif
     endfor
   endif
   
   ;TJK 9/2/2004 - change the logic to check for a large number of columns AND a large number of records
   ;print a different message, depending on whichever is too large for listing to handle
   stars = '#******************'
      
   ;  RCJ 02Mar2021.  maxrecs=2000000000 is set in plot.ph to allow developers to create longer lists
   if maxrecs ne 2000000000 then begin
    if((n_recs gt 15000000 and cols gt 6) or n_recs gt 16000000) then begin ;larger than this takes 30+ min. to generate.
     status = '# WARNING: You have requested '+strtrim(string(n_recs),2)+' records of data, the limit is 15,000,000, please reduce the time range and resubmit.'
      printf, unit, stars 
      printf, unit, format='(a)',status
      printf, unit, stars
      ; Continue to get listing of Global Attributes
      nogatt=0 & norv=1  & nonrv=1 & no2drv=1 & no3drv=1 & no4drv=1 & noimg=1
      ;print, 'STATUS= ',status
      ;return, 0
    endif
   endif

   ;  RCJ  01/14/2013  Removed these conditions to see if/how we can stretch them.
   ;      if((cols gt 100 and n_recs gt 10000) or $
   ;	(d1cols gt 40 and d2cols gt 40))then begin
   ;	  status = '# WARNING: Cannot list this type of data, at least one of the variables that you have selected requires too many columns.'
   ; 	 printf, unit, stars
   ;         printf, unit, format='(a)',status
   ;	 printf, unit, stars
   ;        ; Continue to get listing of Global Attributes
   ;         nogatt=0 & norv=1  & nonrv=1 & no2drv=1 & no3drv=1 & noimg=1
   ;         print, 'STATUS= ',status
   ;         return, 0
   ;      endif

   ; Determine indices of epoch.dat that are within the tstart and tstop
   ;tind=where((b.epoch.dat ge start_time) and (b.epoch.dat le stop_time),w)
   ; RCJ 11/2006  Cannot do 'where' when complex numbers are introduced.
   ;    Have to use cdf_epoch_compare.
   ;tind=where((tmpoch ge start_time) and (tmpoch le stop_time),w)
   if (size(tmpoch,/tname) eq 'DCOMPLEX')then begin
      tind = lonarr(n_elements(tmpoch))
      for i = 0L, n_elements(tmpoch)-1 do begin
         tind[i] = ((cdf_epoch_compare(stop_time, tmpoch[i]) ge 0) and $
                   (cdf_epoch_compare(tmpoch[i], start_time) ge 0))
                    ;cdf_epoch_compare returns 0 for equal
                    ;value and 1 for greater than
      endfor
      tind = where(tind eq 1,w)
   endif else begin
      ;original code for regular tmpoch value
      tind=where((tmpoch ge start_time) and (tmpoch le stop_time),w)
   endelse   
   if(tind[0] eq -1) then begin
      ; Continue to get listing of Global Attributes
      ; RCJ 26Aug2021  Added this test, don't want to print out in all cases of csv
      if (csv1 LE 1) then  begin
        warning='#  WARNING: No Data Selected for this Time Period for at least one variable. '
        printf, unit, format='(a)',warning
      endif	
      nogatt=0 & norv=1  & nonrv=1 & no2drv=1 & no3drv=1 & no4drv=1 & noimg=1
      c=b
   endif
   ;
   ; temporary fix for 1 time choosen for a N dim arrays
   ;if(w eq 1) then begin
   if(w eq 0) then begin
      ; RCJ 26Aug2021  Added this test, don't want to print out in all cases of csv
      if (csv1 LE 1) then  begin
        warning='#  WARNING: Increase time period selected for listing. '
        printf, unit, '# ******************************************************* '    
        printf, unit, format='(a)',warning
        printf, unit, '# ******************************************************* '    
        printf, unit, '# '
      endif	  
      w=0
      nogatt=0 & norv=1  & nonrv=1 & no2drv=1 & no3drv=1 & no4drv=1 & noimg=1
      c=b
   endif
   itrip=0
   irv=0
   inrv=0
   lab_for='('
   unt_for='('
   dat_for='('
   dep_for='('
   ns_tags=n_tags(b)
   namest=tag_names(b)
   ;
   ; Apply time constraints to data structure
   if keyword_set(DEBUG) then print,'Apply time constraints to data structure.'
   if(w gt 0) then begin
      for i=0, ns_tags-1 do begin
         ntags=n_tags(b.(i))
         names=tag_names(b.(i))
         st_sz=size(b.(i).dat)
         ; if(namest(i) eq "EPOCH" or namest(i) eq "EPOCH92") then begin
         if(namest[i] eq depend0) then begin
            temp_dat=b.(i).dateph[tind]
            temp_dat1=b.(i).dat[tind]
            tmp=create_struct('DATEPH',temp_dat)
            tmp1=create_struct('DAT',temp_dat1)
            for l=0, ntags-1 do begin
               if((names[l] eq "DAT") or (names[l] eq "DATEPH")) then begin
                  if(names[l] eq "DAT") then tmpt=create_struct(tmpt,tmp1)
                  if(names[l] eq "DATEPH") then tmpt=create_struct(tmpt,tmp)
               endif else begin        
                  if(l eq 0) then tmpt=create_struct(names[l],b.(i).(l)) else begin $
                      tmpt1=create_struct(names[l],b.(i).(l))
                      tmpt=create_struct(tmpt,tmpt1)
                  endelse
               endelse
            endfor
            ctmp=create_struct(namest[i],tmpt)
            if(i eq 0) then c=ctmp else c=create_struct(c,ctmp)
         endif else begin
            if(strlowcase(b.(i).var_type) eq 'data') then begin
               if(st_sz[0] eq 1) then temp_dat1=b.(i).dat[tind]
               if(st_sz[0] eq 2) then temp_dat1=b.(i).dat[*,tind]
               if(st_sz[0] eq 3) then temp_dat1=b.(i).dat[*,*,tind]
               if(st_sz[0] eq 4) then temp_dat1=b.(i).dat[*,*,*,tind]
            endif else begin
               temp_dat1=b.(i).dat
            endelse
            tmp1=create_struct('DAT',temp_dat1)
            for l=0, ntags-1 do begin
               if(names[l] ne "DAT") then begin
                  if(l eq 0) then tmpt=create_struct(names[l],b.(i).(l)) else begin $
                     tmpt1=create_struct(names[l],b.(i).(l))
                     tmpt=create_struct(tmpt,tmpt1)
                  endelse 
               endif else begin
                  tmpt=create_struct(tmpt,tmp1)
               endelse
            endfor   ; end l
            ctmp=create_struct(namest[i],tmpt)
            if(i eq 0) then c=ctmp else c=create_struct(c,ctmp)
         endelse   ; end if (namest[i] ne depend0)
      endfor   ; end i
   endif   ; end if (w gt 0)
   ; Free Memory
   spdf_delete, ctmp
   spdf_delete, tmp1
   spdf_delete, tmpt
   spdf_delete, tmpt1
   spdf_delete, tmp
   spdf_delete, b
   ;
   nvar=0
   for i=0,ns_tags-1 do begin
      j=0
      ntags=n_tags(c.(i))    
      names=tag_names(c.(i))
      shft=1
      ; 'EPOCH' or 'EPOCH92' etc.
      if(namest[i] eq depend0) then shft=0
      ;if(c.(i).var_notes eq 'ListImage') then shft=0
       ; Build Global Structure
      if keyword_set(DEBUG) then print,'Build Global Structure.'
      while (itrip eq 0) and (j lt ntags) do begin
         j=j+1
         nc=j
         if(names[j] eq 'FIELDNAM') then begin
            itrip=1
         endif else begin
            pair=create_struct(names[j],c.(i).(j))
            if(j eq 1) then begin
               glbatt=create_struct(pair)
            endif else begin
               glbatt=create_struct(glbatt,pair)
            endelse
         endelse
      endwhile
      ; End Global Structure 
      ; Determine Format type and data width 
      if keyword_set(DEBUG) then print,'Determine Format type and data width.'
      form='' ; Default field 
      ; 'EPOCH' or 'EPOCH92' etc.
       
      if(namest[i] eq depend0) then begin
         if keyword_set(sec_of_year) then begin
	    ; RCJ 03/11/2014  When sec_of_year is set we need to redefine the fillval 
	    ;  which is, up to this point, based on a yyyy-mm-dd hh:mm:ss.msec format.
	    format='A20' 
	    c.(i).fillval='9999        0.000000'
	 endif else format='A23' 
      endif else format=c.(i).format
      ;TJK 10/1/2009 - need to allow for the more epoch fields w/ epoch16
      if (c.(i).cdftype eq 'CDF_EPOCH16') then begin
         if keyword_set(sec_of_year) then begin
	    format='A20' 
	    c.(i).fillval='9999        0.000000'
	 endif else format='A35' 
      endif
      if (c.(i).cdftype eq 'CDF_TIME_TT2000') then begin
         if keyword_set(sec_of_year) then begin
	    format='A20'
	    c.(i).fillval='9999        0.000000' 
	 endif else format='A27' 
      endif

      ;if(c.(i).var_type eq 'data') or $
      if(strlowcase(c.(i).var_type) eq 'data') or $
         ; the line below will allow support_data that is not a depend_1 or 2 variable to be listed.
	 ; I'm assuming depend_1 or 2 variables do not have depend_0=Epoch
	 ; RCJ 10/28/2002
         ;((c.(i).var_type eq 'support_data') and (strupcase(c.(i).depend_0) eq depend0)) or $
         ;      (strupcase(c.(i).VARNAME) eq depend0) then begin 
	 ; RCJ 11/12/2003  Sometimes depend_1 or 2 do have depend_0=Epoch (see
	 ; i8_h0_gme var Proton_DIntn2.   This seems to be a better test:
         ((strlowcase(c.(i).var_type) eq 'support_data') and $
	 (c.(i).cdfrecvary ne 'NOVARY')) then begin 

         ; RCJ 08/15/2013  Remove '%' if present in 'format'.
	 ;    We might run into different, more complex cases in the future. Will deal with
	 ;    them as they come up. This case is from bar118_1a_2_l2_ephm dataset.
	 format=strsplit(format,'%',/extract)
         frm_st=data_len(format[0],c.(i).fillval,debug=debug)
	 
         status=frm_st.status
         if( status ne 0) then begin
            if reportflag then printf, 1, 'STATUS= Data cannot be listed. '
            print, 'STATUS= Data cannot be listed. '
            print, 'ERROR=Error: In function data_len'
            close, 1
            return, -1 
         endif

         dat_len=frm_st.dat_len
         form=frm_st.form 	 
      endif

      ; Set record varying formats
      if(form ne '') then begin
      ;print,'^^^ ',nogatt,norv,nonrv,no2drv,no3drv,no4drv,noimg
         if(norv eq 0) then begin
            label=label_search(c,1,i,0,debug=debug)
            units=unit_search(c,1,i,0)
            col_sz=strlen(label) > strlen(units) > dat_len
            ; the second set of "col_sz, label" input will be used to define the format
            ; dep_for. This input changes when we have variables w/ depend_1 attribute.
            ; RCJ 04/01  
            ;sform=form_bld(col_sz, label, c.(i).units, dat_len, col_sz, label, col_sz, label,form, shft)
            sform=form_bld(col_sz, label, units, dat_len, col_sz, label, col_sz, label,col_sz, label,form, shft)
            lab_for=lab_for + sform.labv
            unt_for=unt_for + sform.untv
            dat_for=dat_for + sform.datv
            dep_for=dep_for + sform.depv
         endif

         ; Determine formats for 2D-RV
         if(no2drv eq 0) then begin
            if keyword_set(DEBUG) then print,'Determine formats for 2D-RV.'

            if(strlowcase(c.(i).var_type) eq 'data') or $
                 ;(c.(i).var_type eq 'support_data') then begin
		 ; RCJ 11/12/2003 Same kind of test as above. Look for 11/12/2003.
                 ((strlowcase(c.(i).var_type) eq 'support_data') and $
		  (c.(i).cdfrecvary ne 'NOVARY')) then begin
               st_sz=size(c.(i).dat)
               ; Compute depend_0 or Epoch
               if(st_sz[0] le 1) then begin
                  label=label_search(c,1,i,0,debug=debug)
		  units=unit_search(c,1,i,0)
                  num_var=1
                  if(w eq 1 and st_sz[1] gt 1 and st_sz[0] eq 1) then num_var=st_sz[1] 
                  for k=0, num_var-1 do begin
                     col_sz=strlen(label) > strlen(units) > dat_len
                     ; the second set of "col_sz, label" input will be used to define the format
                     ; dep_for. This input changes when we have variables w/ depend_1 attribute.
                     ; RCJ 04/01  
                     ;sform=form_bld(col_sz, label, c.(i).units, dat_len, col_sz,label,col_sz,label,form, shft)
                     sform=form_bld(col_sz, label, units, dat_len, col_sz,label,col_sz,label,col_sz,label,form, shft)
                     lab_for=lab_for + sform.labv
                     unt_for=unt_for + sform.untv
                     dat_for=dat_for + sform.datv
                     dep_for=dep_for + sform.depv
                     nvar=nvar+1
                  endfor
               endif
               ; Compute all other 2D variables
               if(st_sz[0] eq 2) then begin
                  ;
                  ; Set labels 
                  ;
                  num_var=st_sz[1]
                        depend1_labels=dependn_search(c,i,1) ; st_sz(0)=2
                        if (depend1_labels[0] ne '') then begin
                              depend1=c.(i).depend_1
                              ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
                              q=where(tag_names(c.(i)) eq 'ALT_CDAWEB_DEPEND_1')
                              if (q[0] ne -1) then if (c.(i).alt_cdaweb_depend_1 ne '') then depend1=c.(i).alt_cdaweb_depend_1
                              s=execute('dep1_units=c.'+strtrim(depend1,2)+'.units')
                              depend1_labels=['(@_'+depend1_labels+'_'+dep1_units+')']
                        endif
                        depend2_labels=dependn_search(c,i,2) ; st_sz(0)=2
                        if (depend2_labels[0] ne '') then begin
                              depend2=c.(i).depend_2
                              ; RCJ 05/16/2013  If alt_cdaweb_depend_2 exists, use it instead:
                              q=where(tag_names(c.(i)) eq 'ALT_CDAWEB_DEPEND_2')
                              if (q[0] ne -1) then if (c.(i).alt_cdaweb_depend_2 ne '') then depend2=c.(i).alt_cdaweb_depend_2 
                              s=execute('dep2_units=c.'+strtrim(depend2,2)+'.units')
                              depend2_labels=['(@_'+depend2_labels+'_'+dep2_units+')']
                        endif
                        ;  RCJ 13Apr2021  Label should be first set by depend_0 (st_sz[0] le 1)
			for kk=0L,st_sz[1]-1 do begin
                           label0=label_search(c,st_sz[0],i,kk,debug=debug)
                           if strlen(label0) gt strlen(label) then label=label0
                           units0=unit_search(c,st_sz[0],i,kk)
                           if strlen(units0) gt strlen(units) then units=units0
			endfor 
			; label: this is only the longest of the labels, not a specific label.  
                        dep_col_sz=max(strlen(depend1_labels)) > max(strlen(depend2_labels)) >strlen(units) > dat_len
                        col_sz = strlen(label) > strlen(units) > dat_len
                        sform=form_bld(col_sz, label, units, dat_len, dep_col_sz, depend1_labels,dep_col_sz, depend2_labels,dep_col_sz, depend2_labels,form, shft)
                        ; Modify format 
                        labv=strtrim(num_var,2)+'('+sform.labv+' ' 
                        untv=strtrim(num_var,2)+'('+sform.untv+' '
                        datv=strtrim(num_var,2)+'('+sform.datv+' '
                        depv=strtrim(num_var,2)+'('+sform.depv+' ' 
                        lend=strlen(labv)-2
                        uend=strlen(untv)-2
                        dend=strlen(datv)-2
                        dpend=max(strlen(depv))-2
                        strput,labv,'),',lend
                        strput,untv,'),',uend
                        strput,datv,'),',dend
                        strput,depv,'),',dpend
                        lab_for=lab_for + labv
                        unt_for=unt_for + untv
                        dat_for=dat_for + datv
                        dep_for=dep_for + depv
                        nvar=nvar+num_var
               endif   ; end  if (z[0] ne -1)

            endif   ; end if (c.(i).var_type eq 'data')
         endif ; format condition
      endif   ; end if (form ne '')
      ;
      ; Determine formats for 3D-RV images
      if ((noimg eq 0) or (no4drv eq 0)) then begin
         if keyword_set(DEBUG) then print,'Determine formats for 3D-RV or 4D-RV images.'
         if(i gt 0) then $
             if(strlowcase(c.(i).var_type) eq 'support_data') then c.(i).var_type="metadata"
         if (strlowcase(c.(i).var_type) eq 'data') or ((strlowcase(c.(i).var_type) eq 'support_data') and (c.(i).cdfrecvary ne 'NOVARY')) then begin
            st_sz=size(c.(i).dat)
            ; Compute depend_0 or Epoch 
            if(st_sz[0] le 1) then begin
               nvar=nvar+1
               label=label_search(c,st_sz[0],i,0,debug=debug)
               units=unit_search(c,st_sz[0],i,0)
               col_sz=strlen(label) > strlen(units) > dat_len
               ; the second set of "col_sz, label" input will be used to define the format
               ; dep_for. This input changes when we have variables w/ depend_1 attribute.
               ; RCJ 04/01  
               sform=form_bld(col_sz, label, units, dat_len, col_sz, label, col_sz, label,col_sz, label,form, shft)
               lab_for=lab_for + sform.labv
               unt_for=unt_for + sform.untv
               dat_for=dat_for + sform.datv
               dep_for=dep_for + sform.depv
            endif
            ; Compute all other 2D variables
            if(st_sz[0] eq 2) then begin
                  ; Determine labels
                  depend1_labels=dependn_search(c,i,1) ; st_sz(0)=2
                  if (depend1_labels[0] ne '') then begin
                     depend1=c.(i).depend_1
                     ; RCJ 05/16/2013  If alt_cdaweb_depend_1 exists, use it instead:
                     q=where(tag_names(c.(i)) eq 'ALT_CDAWEB_DEPEND_1')
                     if (q[0] ne -1) then if (c.(i).alt_cdaweb_depend_1 ne '') then depend1=c.(i).alt_cdaweb_depend_1
                     s=execute('dep1_units=c.'+strtrim(depend1,2)+'.units')
                     depend1_labels=['(@_'+depend1_labels+'_'+dep1_units+')']
                  endif   
                  depend2_labels=dependn_search(c,i,2) ; st_sz[0]=2
                  if (depend2_labels[0] ne '') then begin
                     depend2=c.(i).depend_2
                     ; RCJ 05/16/2013  If alt_cdaweb_depend_2 exists, use it instead:
                     q=where(tag_names(c.(i)) eq 'ALT_CDAWEB_DEPEND_2')
                     if (q[0] ne -1) then if (c.(i).alt_cdaweb_depend_2 ne '') then depend2=c.(i).alt_cdaweb_depend_2 
                     s=execute('dep2_units=c.'+strtrim(depend2,2)+'.units')
                     depend2_labels=['(@_'+depend2_labels+'_'+dep2_units+')']
                  endif   
		  ;if this is 2D image there will be no depend3. 
                     depend3_labels=dependn_search(c,i,3) ; st_sz[0]=3
                     if (depend3_labels[0] ne '') then begin
                       depend3=c.(i).depend_3
                       q=where(tag_names(c.(i)) eq 'ALT_CDAWEB_DEPEND_3')
                       if (q[0] ne -1) then if (c.(i).alt_cdaweb_depend_3 ne '') then depend3=c.(i).alt_cdaweb_depend_3 
                       s=execute('dep3_units=c.'+strtrim(depend3,2)+'.units')
                       depend3_labels=['(@_'+depend3_labels+'_'+dep3_units+')']
                     endif 
                  label=''
		  units=''
                  for kk=0L,st_sz[1]-1 do begin
                     label0=label_search(c,st_sz[0],i,kk,debug=debug)
                     if strlen(label0) gt strlen(label) then label=label0
                     units0=unit_search(c,st_sz[0],i,kk)
                     if strlen(units0) gt strlen(units) then units=units0
                  endfor 
		  dep_col_sz=max(strlen(depend1_labels)) > max(strlen(depend2_labels))  > max(strlen(depend3_labels)) >strlen(units) > dat_len
                  col_sz = strlen(label) > strlen(units) > dat_len
                  sform=form_bld(col_sz, label, units, dat_len,dep_col_sz,depend1_labels,dep_col_sz,depend2_labels,dep_col_sz,depend3_labels,form, shft)
                  ;
               	  sform.labv=strmid(sform.labv,0,strlen(sform.labv)-1) ; remove comma
                  lab_for=lab_for + strtrim((st_sz[1]),2)+'('+sform.labv+'),'
                  ;
                  sform.untv=strmid(sform.untv,0,strlen(sform.untv)-1) ; remove comma
                  unt_for=unt_for + strtrim((st_sz[1]),2)+'('+sform.untv+'),'
                  ;
                  sform.depv=strmid(sform.depv,0,strlen(sform.depv)-1) ; remove comma
                  dep_for=dep_for + strtrim((st_sz[1]),2)+'('+sform.depv+'),'
                  ;
                  sform.datv=strmid(sform.datv,0,strlen(sform.datv)-1) ; remove comma
                  dat_for=dat_for + strtrim((st_sz[1]),2)+'('+sform.datv+'),'
                  nvar=nvar+1
            endif 
	    nvar=st_sz[1]

         endif   ; end if (c.(i).var_type eq 'data' or ...)
      endif   ; end if (noimg eq 0) 
   endfor   ; end i
   ; 
   ; Add ending parenthesis of formats
   lend=strlen(lab_for)-1
   uend=strlen(unt_for)-1
   dend=strlen(dat_for)-1
   dpend=max(strlen(dep_for))-1
   strput,lab_for,')',lend
   strput,unt_for,')',uend
   strput,dat_for,')',dend
   strput,dep_for,')',dpend
   ; 
   if(no3drv eq 0) then begin
      lab_for='(a23,1x,A5,1x,a9,2(1x,a13),2(1x,a13))'
      unt_for='(a23,1x,a5,1x,a9,2(7x,a7),2(11x,a3))'
      dat_for='(a23,4x,i2,7x,i3,2(1x,g13.5),2(1x,g13.5),/,29(27x,i2,7x,i3,2(1x,g13.5),2(1x,g13.5),/))'
   endif
   ;print,'dep_for = ', dep_for
   ;print,'lab_for = ', lab_for
   ;print,'unt_for = ', unt_for
   ;print,'dat_for = ', dat_for
   
   ; Compress unt_for and dat_for format strings using the compress_format_str function.
   ; Ron Yurow (Nov 19, 2020)
   unt_for = compress_format_str (unt_for)
   dat_for = compress_format_str (dat_for)

   if(norv eq 0) or (no2drv eq 0) or (no3drv eq 0) or (no4drv eq 0) or (noimg eq 0) then $ 
          rvars=create_struct(c,'LFORM',lab_for,'UFORM',unt_for,'DFORM',dat_for,'DPFORM',dep_for)
   ; 
   ; Write out structure listing
   ; Turn off glbatt for mulitple depend_0's
   ; Use the variable first_iteration to determine whether to do this section.
   ; Ron Yurow  (Jan 4, 2022)
   ; if(mega_loop gt 1) then begin
   if(~first_iteration) then begin
      ;  Do not do this for JSON or CSV output.   
      ;  Ron Yurow (Sep 21, 2021)
      if (csv1 LE 1) then  begin
         nogatt=1
         printf, unit, format='("#  ")' 
      endif 
   endif
   ; Free Memory   
   spdf_delete, c
   if keyword_set(DEBUG) then print,'Write out variables.'
   if csv1 GE 2 then nogatt = 1 ;CWG 06/24/2019 - Added so that listing headers won't be created, or duplicated when json files are made  
   if(nogatt eq 0) then  status=wrt_hybd_strct(glbatt,unit,0,maxrecs,depend0,mega.num,csv,nvar)
   if(norv   eq 0) then  status=wrt_hybd_strct(rvars,unit,1,maxrecs,depend0,mega.num,csv,nvar)  
   if(nonrv  eq 0) then  status=wrt_hybd_strct(nrvars,unit,2,maxrecs,depend0,mega.num,csv,nvar)
   if(no2drv eq 0) then  status=wrt_hybd_strct(rvars,unit,3,maxrecs,depend0,mega.num,csv,nvar)
   if(no3drv eq 0) then  status=wrt_hybd_strct(rvars,unit,4,maxrecs,depend0,mega.num,csv,nvar)
   if(noimg  eq 0) then  status=wrt_hybd_strct(rvars,unit,5,maxrecs,depend0,mega.num,csv,nvar)
   ; RCJ 07/19/2013  After no4drv data has been reformed, the call below is just like the 
   ;     call above, for 'noimg'.
   if(no4drv eq 0) then  status=wrt_hybd_strct(rvars,unit,5,maxrecs,depend0,mega.num,csv,nvar)
   ; Free Memory  
   spdf_delete, rvars


;endfor ; end mega_loop
; RCJ 26Aug2021.  Moved the code below inside the mega_loop.
;      If listing had 2 different epochs only the second one
;      and its associated deta, was printed.

 time_string=systime()
 if (csv1 EQ 2) then begin
  ; Moved statement to the top of the main procedure so that COMMON block variables will
  ; defined at startup and be available globally.
  ; Ron Yurow  (Jan 4, 2022)  
  ; COMMON SHARE1, jsonstruct
  
  ; RCJ 21Jun2024  Added this condition around the filename. If user
  ;      provided a filename then that's what it should be.
  if not keyword_set(filename) then begin
    jsonfilename = filename + ' '
    strput, jsonfilename, '.json', strpos(jsonfilename, '.csv')  
  endif else jsonfilename=filename
  
  ; Use the variable first_iteration to determine whether to do this section.
  ; Ron Yurow  (Jan 4, 2022)
  ; if mega_loop eq 1 then openw, unit1, jsonfilename, /get_lun
  if first_iteration then openw, unit1, jsonfilename, /get_lun
  if (jsonstruct EQ !NULL) then begin
    jsontag = 'STATUS'
    ;jsonstruct = create_struct(jsontag, 'No data found for specified time range')
    jsonstruct = create_struct(jsontag, 'No epoch record or data selected for this time period for at least one variable.')
    json = call_function('json_serialize',jsonstruct)
    ;print, "STATUS = No data found for specified time range"
    print, "STATUS = No epoch record or data selected for this time period for at least one variable"
  endif else begin
    newglbatt = create_struct(glbatt, jsonstruct)
    json = call_function('json_serialize',newglbatt)
  endelse  
  printf, unit1, format='("",a)',json
  split1=strsplit(jsonfilename,'/',/extract)
  print, 'LIST_OUTDIR=',loutdir
  fmt = '(a10,a'+strtrim(strlen(split1[t]),2)+')'
  print, 'LONG_LIST=',split1[t],format=fmt
 endif else if csv1 EQ 3 then begin
  ; Moved statement to the top of the main procedure so that COMMON block variables will
  ; defined at startup and be available globally.
  ; Ron Yurow  (Jan 4, 2021)  
  ; COMMON SHARE1, jsonstruct
  ; COMMON SHARE3, jsondata
  ; COMMON SHARE4, nanExists
  
  ;file_delete, filename
  ; Use the variable first_iteration to determine whether to do this section.
  ; Ron Yurow  (Jan 4, 2022)
  ; if mega_loop eq 1 then openw, unit, filename, /get_lun
  if first_iteration then openw, unit, filename, /get_lun
  if (jsonstruct EQ !NULL) then begin    
    jsontag = 'STATUS'
    ;jsonstruct = create_struct(jsontag, 'No data found for specified time range')
    jsonstruct = create_struct(jsontag, 'No epoch record or data selected for this time period for at least one variable')
    ; Don't write out JSON now, instead add it to a list for later serialization.
    ; Ron Yurow (Sep 21, 2021)
    ; json = call_function('json_serialize',jsonstruct)
    mega_json.Add, jsonstruct, /NO_COPY
    ;print, 'STATUS = No data found for specified time range'
    print, 'STATUS = No epoch record or data selected for this time period for at least one variable'
  endif else begin
    if (nanExists>0) then begin ;make new glbatt with msg indicating nan values were changed
      tagArr=tag_names(glbatt)
      for i=0, n_elements(tagArr)-1 do begin
        if (tagArr[i] NE 'TEXT') then begin
          if (i EQ 0) then begin
            nanGlbAtt=create_struct(tagArr[0], glbatt.(0))
          endif else begin
            nanGlbAtt=create_struct(nanGlbAtt, tagArr[i], glbatt.(i))
          endelse
        endif else begin
          oldText=glbatt.text
          nanText=strarr(1)
          nanText[0]='CDAWEB NOTE: NaN values not allowed in JSON.  All such fillvals were changed from NaN to the appropriate ISTP Standard value.'
          newText=[oldText, nanText]
          nanGlbAtt=create_struct(nanGlbAtt, 'TEXT', newText)
        endelse
      endfor
      glbatt = nanGlbAtt
    endif
    newglbatt = create_struct(glbatt, jsonstruct)
    newglbatt = create_struct(newglbatt, jsondata)
    ; Don't write out JSON now, instead add it to a list for later serialization.
    ; Ron Yurow (Sep 21, 2021)
    ; json = call_function('json_serialize',newglbatt)
    mega_json.Add, newglbatt, /NO_COPY
  endelse
  
  ; printf, unit, format='(a)',json
 endif else begin
  ; RCJ 21Jun2024  Changed this line from 'eq' to 'ge' because
  ; of other change I made above. Look for '21Jun2024'
  ;if mega_loop eq mega.num then begin
  if mega_loop ge mega.num then begin
    printf, unit, format='("#  ")'
    printf, unit, format='("# Key Parameter and Survey data (labels K0,K1,K2) are preliminary browse data.")'
    printf, unit, format='("# Generated by CDAWeb on: ",a)',time_string
  endif
 endelse

 ; Clear the first_iteration variable.
 ; Ron YUrow  (Jan 4, 2022)
 first_iteration = 0

endfor ; end mega_loop

; Create the JSON if JSON is requested.
; Ron Yurow (Sep 21, 2021)
if  (csv1 EQ 3) then begin
    ; Instead of calling json_serialize, create a CDFJSON object, which contains code
    ; specifically designed for serialize CDF structures into JSON and call the method
    ; directly.
    ; Ron Yurow  (March 3, 2022)
    ;json = call_function('json_serialize',mega_json)
    CDFJSON__define

    obj = OBJ_NEW ("CDFJSON")
    json = obj->Serialize (mega_json)
    OBJ_DESTROY, obj
    printf, unit, format='(a)',json

endif

close,/all
end

