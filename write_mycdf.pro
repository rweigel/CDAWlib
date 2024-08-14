;----------------------------------------------------------------------------------
; RCJ 06/03/2014  Function to calculate size of variable in bytes.  Purpose is to calculate
;                  var size before deciding to compress it.
FUNCTION calculate_varsz, d
  types=['BYTE','INT','LONG','FLOAT','DOUBLE','COMPLEX','STRING','STRUCT', $
         'DCOMPLEX','POINTER','OBJREF','UINT','ULONG','LONG64','ULONG64']
  bytes=[1,2,4,4,8,8,0,0,16,4,0,2,4,8,8] ; bytes for each type above. From IDL help on IDL_DATA_Types
  typ=size(d,/tname)
  nele=size(d,/n_elem)
  q=where(types eq typ)
  if q[0] ne -1 then varsz=nele*bytes[q[0]] else varsz=0
return,varsz
end
;----------------------------------------------------------------------------------
FUNCTION compress_var, d, vname, varstruct, id, nrv, this_zvariable
         if tagindex('CDFTYPE', tag_names(varstruct)) ne -1 then begin
	   ;print,'vname, cdftype = ',vname,'  ',varstruct.cdftype,nrv
	   if ((varstruct.CDFTYPE ne 'CDF_TIME_TT2000') and  (varstruct.CDFTYPE ne 'CDF_EPOCH16') $
	   and  (varstruct.CDFTYPE ne 'CDF_EPOCH')) then begin ; don't want to compress epoch var
	     if (nrv eq 1) then begin ; if nrv with size > 1K then compress, otherwise it's not worth it
	       varsz=calculate_varsz(d)
	       if (varsz gt 1000) then $
	       cdf_compression,id, variable=vname, zvariable=this_zvariable,set_var_compression=5,$ ; 5=gzip
	               set_var_gzip_level=6
	     endif else begin
	       cdf_compression,id, variable=vname, zvariable=this_zvariable,set_var_compression=5,$ ; 5=gzip
	               set_var_gzip_level=6
	      
	     endelse
	   endif ; if not an epoch  	       
         endif else begin ; varstruct.cdftype doesn't exist, could still be compressed, test nrv:
	     if (nrv eq 1) then begin ; if nrv with size > 1K then compress, otherwise it's not worth it
	       varsz=calculate_varsz(d)
	       if (varsz gt 1000) then $
	       cdf_compression,id, variable=vname, zvariable=this_zvariable,set_var_compression=5,$ ; 5=gzip
	               set_var_gzip_level=6
	     endif else begin
	       cdf_compression,id, variable=vname, zvariable=this_zvariable,set_var_compression=5,$ ; 5=gzip
	               set_var_gzip_level=6
	      
	     endelse
	 endelse		       
return,d
end
;----------------------------------------------------------------------------------
; IDL always stores structure tags in uppercase.  The ISTP/IACG CDF
; Guidelines show that most required global attributes are not in
; uppercase.  This function performs a case-check on input attribute
; names, and returns the proper case according to the guidelines.
; Unrecognized attribute names are returned without change.
FUNCTION ISTP_gattr_casecheck, a

case strupcase(a) of
   'ADID_REF': a = 'ADID_ref'
   'CDAWEB_PARENTS': ;do nothing
   'DOI': ; do nothing, keep it as is
   'HTTP_LINK': ; do nothing, keep it as is
   'LINK_TEXT': ; do nothing, keep it as is
   'LINK_TITLE': ; do nothing, keep it as is
   'MODS': ; do nothing, keep it as is
   'NSSDC_ID': ; do nothing, keep it as is
   'PARENTS': ;do nothing
   'PI_AFFILIATION': a = 'PI_affiliation'
   'PI_NAME': a = 'PI_name'
   'SPASE_DATASETRESOURCEID': a = 'spase_DatasetResourceID'
   'SPDF_MASTER_NOTES': a = 'SPDF_master_notes'
   'TEXT': ; do nothing, keep it as is
   'TITLE': ; do nothing, keep it as is
   else: begin
      ; only first letter upper case, others lower case
      a=strupcase(strmid(a,0,1))+strlowcase(strmid(a,1,strlen(a)-1))
   end
endcase
;case a of
;  'PROJECT'                    : a = 'Project'
;  'DISCIPLINE'                 : a = 'Discipline'
;  'SOURCE_NAME'                : a = 'Source_name'
;  'DESCRIPTOR'                 : a = 'Descriptor'
;  'DATA_TYPE'                  : a = 'Data_type'
;  'DATA_VERSION'               : a = 'Data_version'
;  'ADID_REF'                   : a = 'ADID_ref'
;  'LOGICAL_FILE_ID'            : a = 'Logical_file_id'
;  'LOGICAL_SOURCE'             : a = 'Logical_source'
;  'LOGICAL_SOURCE_DESCRIPTION' : a = 'Logical_source_description'
;  'PI_NAME'                    : a = 'PI_name'
;  'PI_AFFILIATION'             : a = 'PI_affiliation'
;  'MISSION_GROUP'              : a = 'Mission_group'
;  'INSTRUMENT_TYPE'            : a = 'Instrument_type'
;  'TEXT'            	       : a = 'Text'
;  else                          : b = 0 ; do nothing
;endcase
return,a
end
;----------------------------------------------------------------------------------
FUNCTION parse_mytime,str,tt2000=tt2000,epoch16=epoch16
;
case 1 of
 keyword_set(tt2000): begin
  str1=strsplit(str,' ',/extract)
  str2=strsplit(str1[0],'/',/extract)
  str3=strsplit(str1[1],'.',/extract)
  str3=strsplit(str3[0],':',/extract)
  end

 keyword_set(epoch16): begin
  str1=strsplit(str,' ',/extract)
  str2=strsplit(str1[0],'/',/extract)
  str3=strsplit(str1[1],'.',/extract)
  str3=strsplit(str3[0],':',/extract)

  end
 else: begin 
  str1=strsplit(str,' ',/extract)
  str2=strsplit(str1[0],'/',/extract)
  str3=strsplit(str1[1],':',/extract)
  end
endcase

s=[str2,str3]
return,s
;
end

;

;----------------------------------------------------------------------------------

; Determine name for a cdf file given the contents of the data structure
; and the ISTP/IACG filenaming conventions.
; Added binning keyword.  Ron Yurow (Oct 23, 2018)
FUNCTION autoname_mycdf, a, longtime=longtime, bothtimes=bothtimes,  $
                            uppercase=uppercase, lowercase=lowercase, binned=binned

; Determine the variable that contains the timing information
atags = tag_names(a)  
tvar = -1
found = 0
found_tt2000=0
found_epoch16=0
found_epoch=0
for i=0,n_elements(atags)-1 do begin
  w = where(tag_names(a.(i)) eq 'CDFTYPE')
  ;if (w[0] ne -1) then if (a.(i).CDFTYPE eq 'CDF_EPOCH') then tvar = i
  ;if (w[0] ne -1 and found eq 0) then begin  ; Is this the best way to test this?
  ; RCJ 02/15/2008 Looking for 'novary' epochs will eliminate the epoch0's (see themis data) 
  if (w[0] ne -1 and a.(i).cdfrecvary ne 'NOVARY' and found eq 0) then begin  ; Is this the best way to test this?
     ; RCJ 16Mar2023  Added more logic below, to now just set the flags but check the epoch array
     ;                and see if it's not make of only fillvals.  If all epochs are fillvals
     ;                cdf should not be generated.
     case a.(i).CDFTYPE of
     'CDF_TIME_TT2000': begin
	ti=where(tag_names(a.(i)) eq 'FILLVAL')
	if (ti[0] ne -1 and size(a.(i).fillval,/type) ne 7) then begin
         if tagindex('HANDLE',tag_names(a.(i))) ne -1 then handle_value,a.(i).HANDLE,t else t = a.(i).DAT
	 q=where(round(t,/L64) ne round(a.(i).fillval,/L64))
	 if q[0] ne -1 then begin
           tvar = i
           found=1
	   found_tt2000=1
	 endif 
	endif else begin
           tvar = i
           found=1
	   found_tt2000=1
	endelse 
	end
     'CDF_EPOCH16': begin
	ti=where(tag_names(a.(i)) eq 'FILLVAL')
	if (ti[0] ne -1 and size(a.(i).fillval,/type) ne 7) then begin
         if tagindex('HANDLE',tag_names(a.(i))) ne -1 then handle_value,a.(i).HANDLE,t else t = a.(i).DAT
	 q=where(round(t,/L64) ne round(a.(i).fillval,/L64))
	 if q[0] ne -1 then begin
           tvar = i
           found=1
	   found_epoch16=1
	 endif 
	endif else begin
           tvar = i
           found=1
	   found_epoch16=1
	endelse 
	end
     'CDF_EPOCH': begin
	ti=where(tag_names(a.(i)) eq 'FILLVAL')
	if (ti[0] ne -1 and size(a.(i).fillval,/type) ne 7) then begin
         if tagindex('HANDLE',tag_names(a.(i))) ne -1 then handle_value,a.(i).HANDLE,t else t = a.(i).DAT
	 q=where(round(t,/L64) ne round(a.(i).fillval,/L64))
	 if q[0] ne -1 then begin
           tvar = i
	   found=1
	   found_epoch=1
	 endif 
	endif else begin
           tvar = i
           found=1
	   found_epoch=1
	endelse 
	end
     else:	
     endcase
   endif  	
endfor
; Now that the 'tvar' is found, Determine the start and stop time of the data
if (tvar ne -1) then begin
  d = get_mydata(a,tvar) 
  ;w = where(d gt 0.0D0,wc)
  ; had to add exception for tt2000 because those times *can* be negative:
  if (found_tt2000 eq 1) then w=(indgen(n_elements(d)))  else w = where(d gt 0.0D0,wc)
  ;if (wc le 0) then begin
  ;if w[0] eq -1 then begin
  if (w[0] eq -1 and i eq n_elements(atags)-1) then begin
    stime = '00000000' & ptime = '00000000'
    if keyword_set(LONGTIME) then begin
      ;stime = stime + '00' & ptime = ptime + '00'
      stime = stime + '000000' & ptime = ptime + '000000'
    endif
  endif else begin
    ;s = parse_mytime(decode_cdfepoch(d[w[0]]))
    s = parse_mytime(decode_cdfepoch(d[w[0]],tt2000=found_tt2000, epoch16=found_epoch16),tt2000=found_tt2000,epoch16=found_epoch16)
    stime = s[0] + s[1] + s[2]
    ; RCJ 02/06/2003  Added min (s[4]) to longtime:
    ;if keyword_set(LONGTIME) then stime = stime + s[3] + s[4]+ s[5]
    ; RCJ 01/14/2013  Parse s[5] to remove decimal point:
    if keyword_set(LONGTIME) then begin
       ss=strsplit(s[5],'.',/extract)
       stime = stime + s[3] + s[4]+ ss[0]
    endif   
    ;s = parse_mytime(decode_cdfepoch(d[w[n_elements(w)-1]]))
    s = parse_mytime(decode_cdfepoch(d[w[n_elements(w)-1]],tt2000=found_tt2000, epoch16=found_epoch16),tt2000=found_tt2000,epoch16=found_epoch16)
    ptime = s[0] + s[1] + s[2]
    ; RCJ 02/06/2003  Added min (s[4]) to longtime:
    ;if keyword_set(LONGTIME) then ptime = ptime + s[3] + s[4]+ s[5]
    ; RCJ 01/14/2013  Parse s[5] to remove decimal point:
    if keyword_set(LONGTIME) then begin
       ss=strsplit(s[5],'.',/extract)
       ptime = ptime + s[3] + s[4]+ ss[0]
    endif   
  endelse
  d = 0 ; free the data space
endif else begin
  print,'ERROR>autoname_mycdf: Type CDF_EPOCH, CDF_TIME_TT2000 or CDF_EPOCH16 not found or all fillval.' 
  return,-1
endelse

; Determine the Logical source for using metadata from the structure
atags = tag_names(a.(0)) ; get names of the epoch attributes
w = where(atags eq 'LOGICAL_SOURCE')
if (w[0] ne -1) then begin 
  ; RCJ 02/06/2003 Bob suggested K0 -> K0s (s=subset of original cdf), H0 -> HOs, etc
  s=strsplit(a.(0).(w[0]),'_',/extract)
  ;lsource=s[0]+'_'+s[1]+'s_'+s[2]
  ; Determine how to annotate the file name to indicate that it contains a composite of
  ; of the original CDF(s).  Binned data is indicated by appending a the text '_binned_'
  ; while other data sets are indicated by using the text 's_'
  ; Ron Yurow (Oct 23, 2018)
  IF  KEYWORD_SET (BINNED) THEN anno_text = '_binned_' ELSE anno_text = 's_'
  lsource=s[0]+'_'+s[1]+anno_text
  ; lsource=s[0]+'_'+s[1]+'s_'
  for i=2,n_elements(s)-2 do begin
     lsource=lsource+s[i]+'_'
  endfor
  lsource=lsource+s[n_elements(s)-1]
endif else begin ; construct lsource from other info
  s = '$' & t = '$' & d = '$'
  w = where(atags eq 'SOURCE_NAME')
  if (w[0] ne -1) then begin
    s=strsplit(a.(0).(w[0]),'>',/extract)
    ;s = strmid(s[0],0,2)
    s = s[0]
  endif
  w = where(atags eq 'DATA_TYPE')
  if (w[0] ne -1) then t=strsplit(a.(0).(w[0]),'>',/extract)
  w = where(atags eq 'DESCRIPTOR')
  if (w[0] ne -1) then d=strsplit(a.(0).(w[0]),'>',/extract)
  ; RCJ 02/06/2003 Bob suggested K0 -> K0s (s=subset of original cdf), H0 -> HOs, etc
  ; Determine how to annotate the file name to indicate that it contains a composite of
  ; of the original CDF(s).  Binned data is indicated by appending a the text '_binned_'
  ; while other data sets are indicated by using the text 's_'
  ; Ron Yurow (Oct 23, 2018)
  IF  KEYWORD_SET (BINNED) THEN anno_text = '_binned_' ELSE anno_text = 's_'
  lsource = s[0] + '_' + t[0] + anno_text + d[0]
;  lsource = s[0] + '_' + t[0] + 's_' + d[0]
endelse

; Determine the version of the cdf file
; RCJ 02/06/2003 Bob does not want version number.
;v = '01' & w = where(atags eq 'DATA_VERSION',wc)
;if (wc gt 0) then v = a.(0).(w[0])
;if strlen(v) lt 2 then v = '0' + v

; create the filename
fname = lsource + '_' + stime
if keyword_set(BOTHTIMES) then fname = fname + '_' + ptime
;fname = fname + '_v' + v

; create the cdf filename by adding the cdf suffix
; RCJ 03/16/2003  It's useless to return the suffix in uppercase because
; cdf_create only creates cdfs w/ suffix '.cdf' and it will change your input
; if you try to call it w/ '.CDF'
if keyword_set(LOWERCASE) then fname = strlowcase(fname) + '.cdf' else $
   fname = strupcase(fname) + '.cdf'

;
return,fname
;
end

;----------------------------------------------------------------------------------
;; RCJ 02/06/2003 There's a separate fnc with the same name in cdfx.pro
FUNCTION compare_vars, a, b
sa = size(a) & nsa = n_elements(sa)
sb = size(b) & nsb = n_elements(sb)
if (nsa ne nsb) then return,0
for i=0,nsa-1 do if (sa[i] ne sb[i]) then return,0
case sa[0] of
  0    : if (a ne b) then return,0
  1    : begin 
         for i=0,sa[1]-1 do begin
	    if (a[i] ne b[i]) then return,0
	 endfor
	 end   
  2    : begin
         for i=0,sa[1]-1 do begin
           for j=0,sa[2]-1 do if (a[i,j] ne b[i,j]) then return,0
         endfor
         end
  else : print,'WARNING>cannot yet compare vars with > 2 dimensions!'
endcase
return,1
end
;----------------------------------------------------------------------------------

; Determine all information about the variable in the varstruct parameter,
; which is required in order to create the variable in a CDF file
FUNCTION create_myCDF_variable,id,varstruct,novirtual=novirtual,cdf27=cdf27, $
                               no_compress=no_compress,DEBUG=DEBUG
vid = -1
vname = varstruct.VARNAME ; Determine the name of the variable

CATCH, Error_status
if Error_status ne 0 then begin
  ;help,!ERROR_STATE
  case !error_state.name of
    'IDL_M_CDF_ERROR': begin
       catch, /cancel ; if error here an infinite loop starts. /cancel prevents this.
       ; print,strmessage(997) gives you "CDF file error" which is pretty vague. 
       if ((strpos(!ERROR_STATE.MSG,'BAD_DATA_TYPE') ne -1) and keyword_set(cdf27)) then begin
         print,'STATUS=CDF was not generated.'
	 print,'ERROR=CDF2.7 file containing EPOCH_16, INT8 or TT2000 variable(s) cannot be generated.'
	 message,/reset
       endif	
    end
    'IDL_M_HANDLE_BADID': begin
       ;  This is a case when handle id = 0 bc there's no data
       print,'STATUS=','No data was found. Please increase time range.'
       print,'ERROR=',!error_state.msg
    end   
    else: print,'ERROR=',!ERROR_STATE.MSG
  endcase  
  return,-1
endif

; Determine IDL sizing information about the data
ti = tagindex('HANDLE',tag_names(varstruct))
if ti ne -1 then handle_value,varstruct.HANDLE,d else d = varstruct.DAT

; This is great. But if novirtual is set and this is a virtual var then d will be =0
if keyword_set(novirtual) then begin
   ti = tagindex('VIRTUAL',tag_names(varstruct))
   if ti ne -1 then begin
      if strtrim(varstruct.(ti),2) ne '' then d=0B
   endif   
endif

c = size(d)
nc = n_elements(c)

; Determine if this variable is RV or NRV
nrv = 0L & rv = 0L ; initialize
ti = tagindex('VAR_TYPE',tag_names(varstruct))
if (ti ne -1) then begin ; var_type is present
  ;if (strupcase(varstruct.VAR_TYPE) eq 'METADATA') then nrv=1L else rv=1L
  ; RCJ 03/05/2003 Going to use cdfrecvary instead if var_type to determine nrv or rv:
  if (strupcase(varstruct.cdfrecvary) eq 'NOVARY') then nrv=1L else rv=1L
endif else rv=1L ; assume RV

; Determine the dimensionality and the data type based on record variance
if (rv eq 1L) then begin
  ; RCJ 10/22/2003 Changed these cases based on data tests.
  case c[0] of
    0   : begin
            ;print,'ERROR>size of data cannot be 0! - write_mycdf rv internal error'
            dims = 0L & dvar=[0]
          end
    1   : begin
            ;if strupcase(varstruct.CDFTYPE) eq 'CDF_EPOCH' then begin 
            if ((strupcase(varstruct.CDFTYPE) eq 'CDF_EPOCH') or $
	        (strupcase(varstruct.CDFTYPE) eq 'CDF_TIME_TT2000')) then begin 
                dims = 0L
	        dvar=[0]
	     endif else begin
	        dims = 0L  ;c[1]  
	        dvar=[1]  
 	     endelse
	  end
          ; RCJ Below was the original:
          ;1   : begin & dims = 0L & dvar=[0] & end
    2   : begin
          pos=strpos(strupcase(tag_names(varstruct)),'DISPLAY_TYPE')
          if pos[0] ne -1 then begin
             if strpos(strupcase(varstruct.display_type),'TIME_SERIES') ne -1 or $
	        strpos(strupcase(varstruct.display_type),'STACK_PLOT') ne -1  or $  
	        varstruct.display_type eq '' or  varstruct.display_type eq ' ' or $  
	        strpos(strupcase(varstruct.display_type),'SPECTROGRAM') ne -1 then begin    
                dims = c[1] & dvar=[1]
             endif else begin
	        dims = [c[1],c[2]] & dvar=[1,1]
	     endelse
	  endif else begin
	     dims = c[1] & dvar=[1]
	  endelse    
          end   
         ; RCJ Below was the original:
         ;2   : begin &  dims = c[1] & dvar=[1] & end
    3   : begin & dims = [c[1],c[2]] & dvar=[1,1] & end
         ; RCJ Below was the original:
         ;3   : begin & dims = [c[1],c[2],c[3]] & dvar=[1,1,1] & end
    4   : begin & dims = [c[1],c[2],c[3]] & dvar=[1,1,1] & end
    else: print,'WARNING>cannot write cdfs with vars with > 3 dimensions!'
  endcase
endif
if (nrv eq 1L) then begin
  case c[0] of
    0   : begin
            ;print,'ERROR>size of data cannot be 0! - write_mycdf nrv internal error'
            dims = 0L & dvar=[0]
          end
    1   : begin & dims = c[1] & dvar=[1] & end
    2   : begin & dims = [c[1],c[2]] & dvar=[1,1] & end
    3   : begin & dims = [c[1],c[2],c[3]] & dvar=[1,1,1] & end
    else: print,'WARNING>cannot write cdfs with vars with > 3 dimensions!'
  endcase
endif

; Determine the type of the CDF variable 
;dtype = lonarr(15) & nelems=1L ; initialize
dtype = lonarr(17) & nelems=1L ; initialize

;RCJ 06/01/2009  Added this first portion of if test.
;  Testing the case based on 'size' alone wasn't enough.
;  Some datasets have their own datatype as described in cdftype.

if rv eq 1 then begin  ; reasonable to say if rv=1 then cdftype exists ?
   case strupcase(varstruct.CDFTYPE) of
        'CDF_BYTE': dtype[0]=1
        'BYTE': dtype[0]=1
	;
	'CDF_CHAR':  begin
           dtype[1]=1                                                                                                     
           nelems = max(strlen(d)) ;TJK need to determine the max length in order for the cdfput to work correctly
           end
	'STRING':  begin
           dtype[1]=1                                                                                                     
           nelems = max(strlen(d)) ;TJK need to determine the max length in order for the cdfput to work correctly
           end
	'CHAR':  begin
           dtype[1]=1                                                                                                     
           nelems = max(strlen(d)) 
           end
	;
	'CDF_DOUBLE': dtype[2]=1
        'CDF_EPOCH': dtype[3]=1
	'CDF_INT1': dtype[5]=1
        ;
	'CDF_INT2': dtype[6]=1
        'UINT': dtype[6]=1
	;
	'CDF_INT4': dtype[7]=1
	'INT': dtype[7]=1
	'SHORT': dtype[7]=1
	;
	;'CDF_REAL4': dtype[8]=1  ; idl8.1 help states that cdf_float = cdf_real4 but not that cdf_real4 is not valid.    
	'CDF_REAL4': dtype[4]=1        
	'CDF_FLOAT': dtype[4]=1
	'FLOAT': dtype[4]=1        
	;
	'CDF_REAL8': dtype[9]=1
	'DOUBLE': dtype[9]=1
	;
	'CDF_UCHAR': begin
           dtype[10]=1
           nelems = max(strlen(d)) ;TJK need to determine the max length in order for the cdfput to work correctly
           end 
	;
	'CDF_UINT1': dtype[11]=1
	'UBYTE': dtype[11]=1
        ;
	'CDF_UINT2': dtype[12]=1
	;
	'CDF_UINT4' : dtype[13]=1
	'ULONG': dtype[13]=1
	;
	; RCJ 07/30/09  cdf_long_epoch is now cdf_epoch16.  NOTE: in cdf_varcreate
	;  cdf_long_epoch still exists, so cdf_epoch16 would not to be confused w/ cdf_epoch.
	'CDF_EPOCH16': dtype[14]=1
	'CDF_LONG_EPOCH': dtype[14]=1
	; RCJ 04/02/2012  Added tt200 and int8 types:
	'CDF_TIME_TT2000': dtype[15]=1
	;
	'CDF_INT8': dtype[16]=1
	'LONG64': dtype[16]=1
        else: print,'ERROR>Did not find type in case statement (function: create_myCDF_variable)'
   endcase  	
endif else begin
   case c[nc-2] of
     ; RCJ 09/01/06  These codes are based on idl's 'size' function 
     0   : print,'ERROR>Undefined data type'
     1   : dtype[0] = 1L ; cdf_byte
     2   : dtype[6] = 1L ; cdf_int2
     3   : dtype[7] = 1L ; cdf_int4 or long
     4   : dtype[4] = 1L ; cdf_real4 or cdf_float or float
     5   : begin ; determine if it is real8 or epoch
          ; determine if a CDFTYPE tag is present, if not then assume real8
          if tagindex('CDFTYPE',tag_names(varstruct)) eq -1 then dtype[9]=1L $
          else begin
            if varstruct.CDFTYPE eq 'CDF_EPOCH' then dtype[3] = 1L $
            else dtype[9] = 1L ; cdf_real8, double
          endelse
        end
     6   : print,'WARNING>CDF does not have complex_float type'
     7   : begin ; cdf_char
          dtype[1] = 1L
          ;nelems = strlen(d[0])
	  ; RCJ 08/13/2003 When saving labels of different lengths the line
	  ; above cuts off labels longer than the first element of these labels.
	  ; The line below works better:
          nelems = max(strlen(d))
        end
     8   : print,'WARNING>CDF does not have structure type'
     9   : begin ; double-precision complex
          dtype[14] = 1L
	end
     10  : print,'WARNING>CDF does not have pointer type'
     ; RCJ 10/22/2003 Added a few more types.
     ; RCJ 09/01/06  Fixing/Adding types based on idl6.3
     11  : print,'WARNING>CDF does not have object reference type'
     12  : dtype[12] = 1L ; cdf_uint2, uint
     13  : dtype[13] = 1L ; cdf_uint4, ulong
     14  : dtype[16] = 1L ; cdf_int8, long64
     15  : print,'WARNING>CDF does not have ulong64 type'
     else: print,'ERROR>Unknown IDL data type'
   endcase
endelse

; Create the variable
if keyword_set(DEBUG) then begin
  print,'creating the variable:',vname
  print,'rv=  ',rv,  ' nrv=  ',nrv,' nelems=',nelems
  print,'dims=',dims,' dvary=',dvar
endif

if (dims[0] eq 0) then begin
  this_zvariable=1 ; to be used in call to cdf_compression
  vid = cdf_varcreate(id,vname,/ZVARIABLE,NUMELEM=nelems,$
        CDF_BYTE=dtype[0],CDF_CHAR=dtype[1],CDF_DOUBLE=dtype[2],$
        CDF_EPOCH=dtype[3],CDF_FLOAT=dtype[4],CDF_INT1=dtype[5],$
        ;CDF_INT2=dtype[6],CDF_INT4=dtype[7],CDF_REAL4=dtype[8],$
	; RCJ 04/03/2012  Leaving cdf_real4=dtype[8] gives a:
	;  'CDF_VARCREATE: Too many types specified.'
        CDF_INT2=dtype[6],CDF_INT4=dtype[7],$
        CDF_REAL8=dtype[9],CDF_UCHAR=dtype[10],CDF_UINT1=dtype[11],$
        CDF_UINT2=dtype[12],CDF_UINT4=dtype[13],CDF_LONG_EPOCH=dtype[14],$
	CDF_TIME_TT2000=dtype[15], CDF_INT8=dtype[16],$
        REC_NOVARY=nrv,REC_VARY=rv)
endif else begin
  this_zvariable=0 ; to be used in call to cdf_compression
  vid = cdf_varcreate(id,vname,dvar,DIMENSIONS=dims,NUMELEM=nelems,$
        CDF_BYTE=dtype[0],CDF_CHAR=dtype[1],CDF_DOUBLE=dtype[2],$
        CDF_EPOCH=dtype[3],CDF_FLOAT=dtype[4],CDF_INT1=dtype[5],$
        ;CDF_INT2=dtype[6],CDF_INT4=dtype[7],CDF_REAL4=dtype[8],$
	; RCJ 04/03/2012  Leaving cdf_real4=dtype[8] gives a:
	;  'CDF_VARCREATE: Too many types specified.'
        CDF_INT2=dtype[6],CDF_INT4=dtype[7],$
        CDF_REAL8=dtype[9],CDF_UCHAR=dtype[10],CDF_UINT1=dtype[11],$
        CDF_UINT2=dtype[12],CDF_UINT4=dtype[13],CDF_LONG_EPOCH=dtype[14],$
	CDF_TIME_TT2000=dtype[15], CDF_INT8=dtype[16],$
        REC_NOVARY=nrv,REC_VARY=rv)
endelse

; write the data into the cdf with special handling for character data

case 1 of
;TJK 7/16/2013 removed the check for type 14 which is long64 because
;that's now the type of TT2000 and we just want to put the values into
;the output cdf.
;  ((c[nc-2] eq 7) or (c[nc-2] eq 14)): begin
   ((c[nc-2] eq 7)): begin
     ;if ((c[0] eq 0)AND(d[0] ne '')) then begin
     ; RCJ 06Jan2023  Removed the second condition bc it caused error: if d[0]=''
     ;       the d[j] part of the 'else' statement doesn't exist for j>0
     if (c[0] eq 0) then begin
      if not keyword_set (no_compress) then d=compress_var(d, vname, varstruct, id, nrv, this_zvariable)
        cdf_varput,id,vname,d
     endif else begin ; data is a string array
       ; pad all elements to same length and concatenate into single buffer
       maxlength = max(strlen(d))  
       buffer = ''
       for j=0,c[1]-1 do begin
         if strlen(d[j]) eq maxlength then buffer = [buffer , d[j]]  $
         else begin
           pad=' '
           for g=strlen(d[j]),(maxlength)-2 do pad = pad + ' '
          buffer = [buffer , d[j] + pad] 
         endelse
       endfor
       buffer=buffer[1:*]
      if not keyword_set (no_compress) then d=compress_var(d, vname, varstruct, id, nrv, this_zvariable)
       cdf_varput,id,vname,buffer,COUNT=[c[1]]
     endelse   
   end
   else: begin
      if not keyword_set (no_compress) then d=compress_var(d, vname, varstruct, id, nrv, this_zvariable)
      cdf_varput,id,vname,d
   end
endcase

return,vid
end

;-------------------------------------------------------------------------
; NAME: WRITE_MYCDF
; PURPOSE:
;       To input up to 35 idl structures of the type returned by read_mycdf,
;       and to output each as a CDF file.
; CALLING SEQUENCE:
;       status = write_mycdf(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,.....,a34)
; INPUTS:
;       instruct = input data structure
; KEYWORD PARAMETERS:
;       filename = the names of the cdf files being created (['file1.cdf','file2.cdf',...])
;       autoname = if set, then override the filename parameter by
;                  generating the name for the cdf file according to
;                  the ISTP filenaming conventions.  This will also
;                  cause the global attribute LOGICAL_FILE_ID to
;                  be set accordingly.
;       longtime = if set, is used in conjunction with the autoname
;                  keyword, but will cause a deviation from the ISTP
;                  filenaming conventions in that the timestamp in the
;                  filename will also include the starting hour of the data.
;       inputfiles = if set, this string array of cdf files used to
;                    generate the new cdf. They will be placed in the
;                    global attribute 'PARENTS'. This keyword will
;                    overwrite the global attr of the input cdfs.
;       bothtimes = if set, is used in conjunction with the autoname and
;                   longtime keywords, will cause a deviation from the ISTP
;                   filenaming conventions in that the timestamp in the
;                   filename will include both start and stop times.
;       uppercase = if set, is used in conjunction with the autoname and
;                   longtime keywords such that the automatically deter-
;                   mined filename will be in all uppercase.
;       lowercase = if set, is used in conjunction with the autoname and
;                   longtime keywords such that the automatically deter-
;                   mined filename will be in all lowercase.
;       outdir    = if set, is used in conjunction with the autoname keywords
;                   to create the file in the specified directory.
;       cdf27_comp = 0/1  Create a cdf that's cdf2.7 backward compatible
;                   so it can be read by versions of idl previous to 6.3.
;                   The default is cdf3.0 when using idl6.3
;	novirtual = 0/1  If set the virtual vars will have only one data element:0 ,
;		    the their attributes FUNC, COMPONENT_0 and  VIRTUAL will
;		    remain unaltered.
;       no_compress = 0/1  Do not compress by variable. If=0, will compress all vars except cdftype=cdf_epoch, cdf_epoch16 or cdf_time_tt2000.
;                         Also, if=0,  will not compress if var is non-record variant and its size is less 
;                         than 1K, just not worth the effort. 
;
; OUTPUTS:
;       status = integer indicating success (0) or failure (-1)
; EXAMPLE:
;       a = read_mycdf('','file1.cdf',/all) ; read all vars from file1.cdf
;       s = write_mycdf(a,filename='file2.cdf')      ; create same file named file2.cdf
;       s = write_mycdf(a0,a1,a2,/autoname)    ; create filename based on contents
;                                           ;  of structures 'a0,a1,a2'.
; AUTHOR:
;       Richard J. Burley, NASA/GSFC/Code 632,  June, 1998
;       burley@nssdca.gsfc.nasa.gov    (301)286-2864
; MODIFICATION HISTORY:
;   	Rita C Johnson, 01/06/2003. We want to be able to input more than 1 structure
;   	    	and come out with just as many cdfs.
;   	    	Also added a few print lines at the end of the program so it can
;   	    	be integrated into the CDAWeb system.
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;
; This package of routines complements the read_myCDF package.  read_myCDF
; reads one or more cdf's, and returns all data and metadata as a single
; structure.  write_myCDF will do just the opposite, given a similar structure,
; it will write a cdf.

FUNCTION write_myCDF, a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,$
 		      a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,$
                      a30,a31,a32,a33,a34,$
                      filename=filename, AUTONAME=AUTONAME,  LONGTIME=LONGTIME,  $
		              inputfiles=inputfiles, $
                                   BOTHTIMES=BOTHTIMES,OUTDIR=OUTDIR,      $
                                   UPPERCASE=UPPERCASE,LOWERCASE=LOWERCASE,$
                                   CDF27_COMP=CDF27_COMP, NOVIRTUAL=NOVIRTUAL, $
				   NO_COMPRESS=NO_COMPRESS, APPEND_TEXT=APPEND_TEXT, BINNED=BINNED, $
				   DEBUG=DEBUG

compile_opt idl2

; Verify that number of parameters is acceptable
if ((n_params() le 0)OR(n_params() gt 35)) then begin
  print, 'STATUS= No data selected for plotting'
  print,'ERROR=Number of parameters must be from 1 to 35' & return,-1
endif

if keyword_set(filename) then begin
   if (n_elements(filename) ne n_params()) then begin
      print,'ERROR=Enter a string array with one filename for each input structure' 
      return,-1
   endif
endif else autoname=1

; RCJ 06/02/2014  Set !quiet=1 so we don't see informational msgs coming from cdf_close
origq=!quiet
!quiet=1

files=''
datasets=''
if keyword_set(filename) then cdfnames=filename
ttime = systime(1)

for k=0,n_params()-1 do begin ; process each structure parameter

 w = execute('a=a'+strtrim(string(k),2))
 if size(a,/type) eq 8 then begin  ;  8 is type 'structure' 
   already_created='' ;  to be used before one of the calls to create_myCDF_variable
   new_order=1000  ;  dummy # to start array. use to eliminate redundant var names but keep the same order

   ; RCJ 05/05/2003 Temporary test to exclude isis, rpi. These are not producing good cdfs
   ; RCJ 06/04/2003 I think I fixed the case for isis data. Will test in dev.
   ; RCJ 07/31/2003 RPI seems fixed too. I added some logic a little bit further
   ;     down that includes the ignore_data vars if they are depends or components
   ;
   ;b = tag_names(a.(0)) & w = where(b eq 'MISSION_GROUP')
   ;if w[0] ne -1 then begin
      ;case strupcase(a.(0).MISSION_GROUP) of
      ;'ISIS': begin
      ;   print, 'STATUS=Currently cannot write cdf for ISIS data'
      ;   print,'ERROR=Cannot write cdf for ISIS data'
      ;   return,-1
      ;end 
      ;'IMAGE': begin
      ;   if strpos(strupcase(a.(0).DESCRIPTOR),'RPI') ne -1 then begin
      ;     print, 'STATUS=Currently cannot write cdf for RPI data'
      ;	    print,'ERROR=Cannot write cdf for RPI data'
      ;     return,-1
      ; 	 endif   
      ;end
      ;else:
      ;endcase
   ;endif

   ; Non longer going to check for plottable data before processing structure.
   ; So no need keep track of which variables have plottable data.
   ; Ron Yurow (Nov 10, 2022)
   ;
   ;  RCJ 06Mar2023 Reusing this var, 'ok', but now as a flag array to check
   ;        if there is any good epoch (depend_0) data, whatever the var_type may be.
   ;        With bad epochs we get bad filenames, eg: *_99991231235959_99991231235959.cdf
   ;        or *_00000000000000_00000000000000.cdf
   ok=0
   ;  RCJ 08/26/2013   Added this test for case when structure does not
   ;        have any data, but only dataset, error and status. This is the
   ;        output structure of read_myCDF when an there is error. 
   rflag=tagindex('DATASET',tag_names(a))
   if(rflag[0] ne -1) then begin
      print, a.DATASET
      print, a.STATUS
      return, 0
   endif
   ;
   for j=0,n_elements(tag_names(a))-1 do begin
      ; Don't bother running evaluate_varstruct.  Don't care if variable is plottable.
      ; Ron Yurow (Nov 10, 2022)
      ;b = evaluate_varstruct(a.(j))
      
      ; RCJ 17May2024  If var name changed in read_myCDF for 'bad character',
      ;               reinstate the original name here.
      q=where(tag_names(a.(j)) eq 'ORIGVNAME')
      if q[0] ne -1 then a.(j).varname = a.(j).ORIGVNAME


      ; RCJ 06Mar2023.  Not checking for plottable data but should check if data
      ;      is not all fillval.  Will check if there are depend_0's and if these
      ;      have good data.  Assuming here that user only interested in vars 
      ;      that have depend_0 attr
      ;
      ; print,'depend0 for this : ',a.(j).varname,'  ',a.(j).depend_0,'  *',a.(j).fillval,'*'
      ti=where(tag_names(a) eq strupcase(a.(j).depend_0))
      if ti[0] ne -1 then begin
         if tagindex('HANDLE',tag_names(a.(ti[0]))) ne -1 then handle_value,a.(ti[0]).HANDLE,tr else tr = a.(ti[0]).DAT
	 ; does fillval attr exist? assign a value if doesn't , just don't 
	 ;    get error.  reuse ti :
	 ti=where(tag_names(a.(j)) eq 'FILLVAL')
	 if (ti[0] ne -1 and size(a.(j).fillval,/type) ne 7) then begin
	    q=where(round(tr,/L64) ne round(a.(j).fillval,/L64))
	    if q[0] ne -1 then ok=[ok,1] else ok=[ok,0]
	 endif   
      endif else begin
         ok=[ok,0]
      endelse	        
      
      ;if b.ptype ne 0 then ok=[ok,1] else ok=[ok,0]
      ; RCJ 11/05/2013  At this point, evaluate the epoch in the structure and decide if a 
      ; cdf2.7 can be created. If not, revert to cdf3.5 or above
      if keyword_set(cdf27_comp) then begin  ; don't need to get into this if cdf27_comp is not set
         ; RCJ 20Dec2021  Check attributes too, not only variables!
         for tt=0,n_elements(tag_names((a).(j)))-1 do begin
	    if (size((a.(j).(tt)),/tname) eq 'LONG64') then begin ;  only 'long64' ?
	      print,'WARNING>Cannot generate a 2.7.2 CDF because variable attributes require version 3.5 or above, so generating a 3.5 or above CDF'
	      cdf27_comp=0
	    endif
         endfor
         if tagindex('CDFTYPE', tag_names(a.(j))) ne -1 then begin
            if (a.(j).CDFTYPE eq 'CDF_TIME_TT2000' or a.(j).CDFTYPE eq 'CDF_INT8' or a.(j).CDFTYPE eq 'CDF_EPOCH16') then begin
	      print,'WARNING>Cannot generate a 2.7.2 CDF because data variables require version 3.5 or above, so generating a version 3.5 or above CDF'
	      cdf27_comp=0
	    endif
         endif
      endif
   endfor
   ;
   ok=ok[1:*]
   
   ; Refactor the data structore to change varaibale names and attributes if
   ; binning is indicated by the binning keyword being set.
   IF  KEYWORD_SET (BINNED) THEN BEGIN

       ; Number of variables to examine
       ntags = N_TAGS (a)

       ; dataset is pointer array to STRUCTS for each variable.  
       dataset = PTRARR (ntags)
       ; array of variable names. convenient.
       varlist = [!NULL]

       ; Create the dataset pointer array, where each element is pointer to variables data STRUCT.
       ; Since variables may get new names, keeping them in a pointer array is better than a 
       ; structure of structures.
       FOR index = 0, ntags - 1 DO BEGIN

           dataset [index] = PTR_NEW (a.(index))
           varlist = [varlist, (*dataset[index]).VARNAME]

       ENDFOR
       
       ; Make sure thre is a variable with the name 'Epoch_bin'.  If this doesn't exist, then
       ; the user may have requested binned data, but no variable were actually binned. Checking
       ; this now may save some work.
       sink = WHERE (STRUPCASE (varlist) eq 'EPOCH_BIN', cnt)

       ; OK, go ahead and process the binned variables.
       IF  cnt gt 0 THEN BEGIN

           FOR index = 0, ntags - 1 DO BEGIN

               ; Attempt to classify all variables based on if they are the binned version of some
               ; variable, a count of bins, or delta_plus/delta_minus generated as part of the 
               ; binning process.

               ; First weed out any variable that is not binned or time dependent. 
               tagnames = TAG_NAMES (*dataset[index])

               ; Make sure we have a DEPEND_0 attribute.
               sink = WHERE (STRUPCASE (tagnames) eq 'DEPEND_0', cnt)

               IF  cnt eq 0 THEN CONTINUE

               ; Makes sure the DEPEND_0 attribute as value other than the empty string.
               IF  STRLEN ((*dataset[index]).DEPEND_0) eq 0 THEN CONTINUE

               ; Make sure we have the TIME_BIN_WIDTH_SEC attribute.  All binned variables 
               ; should have this attribute.
               sink = WHERE (STRUPCASE (tagnames) eq 'TIME_BIN_WIDTH_SEC', cnt)

               IF  cnt eq 0 THEN CONTINUE

               ; Check if the variable ends in the suffix _NBIN.  These variables are 
               ; created by the binning software and contain the numbe of values in in 
               ; single time interval.  If it is this type of variable, then make 
               ; appropiated changes to some of the attributes.
               nbin_suffix_pos = STRPOS ((*dataset[index]).VARNAME, 'NBIN')

               IF  nbin_suffix_pos ne -1 THEN BEGIN
                 
                   root = strmid ((*dataset[index]).VARNAME, 0, nbin_suffix_pos - 1)
        
                   (*dataset[index]).VARNAME  = root + '_N_TIMEAVG'
                   (*dataset[index]).CATDESC  = root + ': Number of values averaged.'

                   (*dataset[index]).VAR_TYPE = 'data' 

                   nbin_suffix_pos = STRPOS ((*dataset[index]).FIELDNAM, 'NBIN')

                   stem = strmid ((*dataset[index]).FIELDNAM, 0, nbin_suffix_pos - 1)
 
                   (*dataset[index]).FIELDNAM = stem + ': Number of values averaged.'

                   CONTINUE

               ENDIF

               ; Check if the variable ends in the suffix _PLUS_VAR.  These variables are 
               ; created by the binning software and contains a mean of the original values
               ; in each time period, expressed as a positive limit on error bar for the binned 
               ; value.  If it is this type of variable, then make appropiated changes to some
               ; of the attributes.
               nbin_suffix_pos = STRPOS ((*dataset[index]).VARNAME, 'BIN_DELTA_PLUS_VAR')

               IF  nbin_suffix_pos ne -1 THEN BEGIN

                   old_vname = (*dataset[index]).VARNAME
                 
                   root = strmid (old_vname, 0, nbin_suffix_pos - 1)

                   new_vname =  root + '_DELTA_PLUS_TIMEAVG'
        
                   (*dataset[index]).VARNAME  = new_vname
                   (*dataset[index]).CATDESC  = root + ': Uncertainty of mean time average.'

                   nbin_suffix_pos = STRPOS ((*dataset[index]).FIELDNAM, 'BIN_DELTA_PLUS_VAR')

                   stem = strmid ((*dataset[index]).FIELDNAM, 0, nbin_suffix_pos - 1)
 
                   (*dataset[index]).FIELDNAM = stem + ': Uncertainty in average.'

                   ; Change any DELTA_MINUS_VAR attribute that has the old variable name
                   ; to point to the new variable name.
                   FOR v_idx = 0, ntags - 1 DO BEGIN

                       referance = STRCOMPRESS ((*dataset[v_idx]).DELTA_PLUS_VAR, /REMOVE_ALL)

                       IF  STRCMP ( referance, old_vname) THEN $
                           (*dataset[v_idx]).DELTA_PLUS_VAR = new_vname

                   ENDFOR

                   CONTINUE

               ENDIF
                
               ; Check if the variable ends in the suffix _MINUS_VAR.  These variables are 
               ; created by the binning software and contains a mean of the original values
               ; in each time period, expressed as a negative limit on error bar for the binned 
               ; value.  If it is this type of variable, then make appropiated changes to some
               ; of the attributes.
               nbin_suffix_pos = STRPOS ((*dataset[index]).VARNAME, 'BIN_DELTA_MINUS_VAR')
               
               IF  nbin_suffix_pos ne -1 THEN BEGIN

                   old_vname = (*dataset[index]).VARNAME
                 
                   root = strmid (old_vname, 0, nbin_suffix_pos - 1)

                   new_vname =  root + '_DELTA_MINUS_TIMEAVG'
        
                   (*dataset[index]).VARNAME  = new_vname
                   (*dataset[index]).CATDESC  = root + ': Uncertainty of mean time average.'

                   nbin_suffix_pos = STRPOS ((*dataset[index]).FIELDNAM, 'BIN_DELTA_MINUS_VAR')

                   stem = strmid ((*dataset[index]).FIELDNAM, 0, nbin_suffix_pos - 1)
 
                   (*dataset[index]).FIELDNAM = stem + ': Uncertainty in average.'

                   ; Change any DELTA_MINUS_VAR attribute that has the old variable name
                   ; to point to the new variable name.
                   FOR v_idx = 0, ntags - 1 DO BEGIN

                       referance = STRCOMPRESS ((*dataset[v_idx]).DELTA_MINUS_VAR, /REMOVE_ALL)

                       IF  STRCMP ( referance, old_vname) THEN $
                           (*dataset[v_idx]).DELTA_MINUS_VAR = new_vname

                   ENDFOR

                   CONTINUE
                
               ENDIF

              ; This must a be a binned variable.  Make appropiated changes to some
              ; of the attributes. 
              (*dataset[index]).FIELDNAM = (*dataset[index]).FIELDNAM + ' (SPDF Time Averaged)' 
              (*dataset[index]).CATDESC  = (*dataset[index]).CATDESC  + ' (SPDF Time Averaged)'             
                      
           ENDFOR

           ; Convert the dataset array into a CDF data structure.  At the same, free the 
           ; pointers.
           tmp = CREATE_STRUCT ((*dataset[0]).VARNAME, *dataset[0])

           PTR_FREE, dataset[0]

           index = 1

           WHILE (index lt N_ELEMENTS (dataset)) DO BEGIN

               tmp = CREATE_STRUCT (tmp, (*dataset[index]).VARNAME, *dataset[index])

               PTR_FREE, dataset[index]

               index++
                
           ENDWHILE

           ; Replace the old data structure with the new one.
           a = tmp

       ENDIF

   ENDIF

   ; Removed check for at least one plotable variable.  this was keeping us from creating CDFs.
   ; with no-plot or listing only variables.
   ; NB also comment out line at ~1462 that ends ELSE branch of this check.
   ; Ron Yurow (Nov 10, 2022)
   ; RCJ 06Mar2023  Brought this check back again. Search for 'ok' for more info. 
    q=where(ok ne 0)   
    
   if q[0] eq -1 then begin
      print,'STATUS=No valid data for '+strupcase(a.(k).logical_source)+'. Please try another time range.' 
      ;print,'STATUS=No valid data for '+strupcase(a.(0).logical_source)+'. Please try another time range.' 
   endif else begin   
      ; Identify the global attributes.
      b = tag_names(a.(0)) 
      w = where(b eq 'FIELDNAM',wc) 
      gattrs=indgen(w[0]-1)+1
      ; Determine the order of the variables to be written to the CDF.
      b = tag_names(a) 
      nb = n_elements(b)
      c = intarr(nb)
      d = indgen(nb)
      ; RCJ 09/08/2003 Look for all depends and components. Even if the vars are 'ignore_data' we will
      ; want them.
      ; RCJ 09/17/2003 Also look at the display_type attribute. For example, in the case of
      ; RPI data there were variables needed for a plasmagram (labels) whose names were
      ; only found in the display_type attribute. 
      needed_vars=''
      for i=0,nb-1 do begin
         if (tagindex('DISPLAY_TYPE',tag_names(a.(i)))) ne -1 then begin        
	    out=parse_display_type(a.(i).display_type)
	    if strtrim(out[0],2) ne '-1' then needed_vars=[needed_vars, out]
	 endif
	 ;for ii=0,3 do begin
	 for ii=0,7 do begin  ;  twins1_l1_imager  has depend_7
	 ;print,tagindex('DEPEND_0',tag_names(a.(i)))
         ;if (tagindex('DEPEND_0',tag_names(a.(i)))) ne -1 then needed_vars=[needed_vars,a.(i).depend_0]
	 comm='if (tagindex("DEPEND_'+strtrim(ii,2)+ $
	    '",tag_names(a.(i)))) ne -1 then needed_vars=[needed_vars,a.(i).depend_' + $
	    strtrim(ii,2)+']'
	 s=execute(comm)   
	 endfor
	 for ii=0,14 do begin
	 ;if (tagindex('COMPONENT_0',tag_names(a.(i)))) ne -1 then needed_vars=[needed_vars,a.(i).component_0]
	 comm='if (tagindex("COMPONENT_'+strtrim(ii,2)+ $
	    '",tag_names(a.(i)))) ne -1 then needed_vars=[needed_vars,a.(i).component_' + $
	    strtrim(ii,2)+']'
	 s=execute(comm)
	 endfor
	 ; RCJ 10/22/2003 Delta plus and minus vars have to be added at this point too.
	 if (tagindex('DELTA_MINUS_VAR',tag_names(a.(i)))) ne -1 then $
	    needed_vars=[needed_vars,a.(i).delta_minus_var]
 	 if (tagindex('DELTA_PLUS_VAR',tag_names(a.(i)))) ne -1 then $
	    needed_vars=[needed_vars,a.(i).delta_plus_var]
     endfor
      if (needed_vars[0] ne '') then needed_vars=needed_vars[1:*]  
      needed_vars=needed_vars[uniq(needed_vars,sort(needed_vars))] 
      ;print,'needed_vars = ',needed_vars
      for i=0,nb-1 do begin
         ;if a.(i).CDFTYPE eq 'CDF_EPOCH' then c(i) = 2 $ ; time variable
         ;else if strupcase(a.(i).VAR_TYPE) eq 'DATA' then c(i) = 1 ; RV variable
         if strupcase(a.(i).VAR_TYPE) eq 'SUPPORT_DATA' then c[i] = 1 ; RV variable
         if a.(i).CDFTYPE eq 'CDF_EPOCH' then c[i] = 2 ; time variable
         if strupcase(a.(i).VAR_TYPE) eq 'DATA' then c[i] = 1 ; RV variable
         ;if strupcase(a.(i).VAR_TYPE) eq 'IGNORE_DATA' then c(i) = -1 ; RV variable
	 ; RCJ 04/03/2012  do not ignore this var. Should have it if request is /all 
	 ;  or code may break when handling vars related to this one.
         if strupcase(a.(i).VAR_TYPE) eq 'IGNORE_DATA' then c[i] = 1 ; RV variable
         pos_needed_vars=where(strupcase(needed_vars) eq strupcase(a.(i).varname))
         ;print,'varname: ',strupcase(a.(i).VARname),'  vartype: ',strupcase(a.(i).VAR_type)
	 ;print,'zeros: ',pos_needed_vars[0]
	 ;
	 ; RCJ 09/2003 If var_type is ignore_data *and* the variable is not a depend_0,1,2
	 ; or component_0,1,2 to any other variable, then don't include it in the cdf,
	 ; make c[i]=-1
         ;;if (strupcase(a.(i).VAR_TYPE) eq 'IGNORE_DATA') and $
	 ;;(pos_needed_vars[0] eq -1) $
	 ;;then c(i) = -1 ; RV variable
	 ; RCJ 09/10/2003 Now that we decided if this ignore_data var should
	 ; be added to the new cdf, let's make it support_data, basically
	 ; not to confuse SKTEditor.
	 ; RCJ 04/02/2012 SKTEditor can now handle ignore_data types. Commented out lines below.
        ; if (strupcase(a.(i).VAR_TYPE) eq 'IGNORE_DATA') then begin
	;    a.(i).var_type='support_data'
	;    print,'new var_type: ',a.(i).var_type,'  ',a.(i).varname
	; endif   
      endfor
      w2 = where(c eq 2,wc2) & if wc2 gt 0 then s=d[w2]
      w1 = where(c eq 1,wc1) & if wc1 gt 0 then s=[s,d[w1]]
      w0 = where(c eq 0,wc0) & if wc0 gt 0 then s=[s,d[w0]]
      order = s
      ; Determine the name of the new CDF if autonaming option is turned on
      if keyword_set(AUTONAME) then begin
        ; Added BINNED keyword to autoname_mycdf () function.
        ; Ron Yurow  (Oct 23, 2018)
        filename = autoname_mycdf(a,LONGTIME=LONGTIME,BOTHTIMES=BOTHTIMES,$
                            UPPERCASE=UPPERCASE,LOWERCASE=LOWERCASE,BINNED=BINNED)
        s = size(filename) & i = n_elements(s)
        if (s[i-2] ne 7) then begin
	   print,'ERROR: In autoname'
	   ;return,-1 ; fatal error in autoname
	   ;  RCJ 23May2023  Mod: return -> continue. 
	   ;    Motivation: multiple themis s/c; if one does not have valid data and error
	   ;    was generated but we want it to continue to the next s/c
	   continue
	endif   
      endif else begin
         filename=cdfnames[k]
      endelse
      
      ; Create the new CDF
      ;
      if keyword_set(OUTDIR) then begin
         case (strupcase(!version.os_family)) of 
                'UNIX': begin
		; RCJ 12/11/2006  If buf1,buf2,buf3,...  /'s can accumulate
		   if strmid(outdir,strlen(outdir)-1L,1) ne '/' $
		      then outdir=outdir+'/'
		 end  
                'MACOS': begin
		   if strmid(outdir,strlen(outdir)-1L,1) ne '/' $
		      then outdir=outdir+'/'; osX ?
		 end  
                'WINDOWS': begin
		   if strmid(outdir,strlen(outdir)-1L,1) ne '\' $
		      then outdir=outdir+'\' 
		 end  
                else: print, 'Warning! Unknown OS. ' 
         endcase
         res=findfile(outdir+filename)
      endif else res=findfile(filename)
      if res[0] ne '' then begin	 
         case (strupcase(!version.os_family)) of 
            'UNIX': begin
	       if keyword_set(outdir) then spawn,'rm -f '+outdir+filename $
	       else spawn,'rm -f '+filename
	     end  
            'MACOS': begin
	       if keyword_set(outdir) then spawn,'rm -f '+outdir+filename $
	       else spawn,'rm -f '+filename
	     end  
            'WINDOWS': begin
	       if keyword_set(outdir) then spawn,'del '+outdir+filename $
	       else spawn,'del '+filename
	       ; DOS window will flash on screen!!
	       ; RCJ 09/2006  From IDL help: "Issuing a SPAWN command when 
	       ; IDL's current working directory is set to a UNC path 
	       ; will cause Windows to generate an error"
	       ; UNC=Universal/Uniform Naming Convention
	       ; and it looks like this:  \\server\volume\directory\file
	     end  
            else: begin
	       if keyword_set(outdir) then print, 'Unknown OS. Could not remove already existing ',outdir+filename $
               else print, 'Unknown OS. Could not remove already existing ',filename 
            end
         endcase
      endif	 
      if keyword_set(DEBUG) then begin
         print,'Now creating the CDF ',filename 
         if keyword_set(outdir) then print, 'in ',outdir
      endif	
      
      ; RCJ 04/03/2012 Warning!!!  Setting cdf_set_cdf27_backward_compatible to yes
      ;  will set an environment variable in your system!!! Subsequent calls to
      ;  write_mycdf will imply that you want to generate v2.7 cdf, unless you
      ;  reset the env var to 'no' as we did here.
      if keyword_set (cdf27_comp) then cdf_set_cdf27_backward_compatible, /yes else $ 
            cdf_set_cdf27_backward_compatible, /no

      ; RCJ 05/28/2013  Augmented the call to cdf_create. For years we have been creating
      ;  cdfs w/ col_major but now we want to also be able to write in row_major, depending
      ;  on what major the parent cdfs were.  See, for example, rbsp/ect cdfs.
      ;  Any var in structure 'a' should contain cdfmajor, so pick 0 :
      maj = tagindex('CDFMAJOR', tag_names(a.(0)))
      if maj[0] eq -1 then maj='' else maj=a.(0).cdfmajor
      case maj of
         'ROW_MAJOR': begin
                    if keyword_set (outdir) then $
                    id = cdf_create(outdir+filename,/clobber,/NETWORK_ENCODING,/SINGLE_FILE,/ROW_MAJOR) $
                    else id = cdf_create(filename,/clobber,/NETWORK_ENCODING,/SINGLE_FILE,/ROW_MAJOR) 
		    end
	 else: begin  ; if it's not row then it can only be column, and it has been the default for years. 
                    if keyword_set (outdir) then $
                    id = cdf_create(outdir+filename,/clobber,/NETWORK_ENCODING,/SINGLE_FILE,/COL_MAJOR) $
                    else id = cdf_create(filename,/clobber,/NETWORK_ENCODING,/SINGLE_FILE,/COL_MAJOR)
		    end
      endcase
      ;if keyword_set (outdir) then $
      ;   id = cdf_create(outdir+filename,/clobber,/NETWORK_ENCODING,/SINGLE_FILE,/COL_MAJOR)$
      ;   else id = cdf_create(filename,/clobber,/NETWORK_ENCODING,/SINGLE_FILE,/COL_MAJOR)
      ;
      ; RCJ 04/28/2008  Adding call to md5checksum. For cdf3.1 or earlier this command seems to be ignored.
      ;cdf_set_md5checksum,id,/yes
      ;
      ; Write global attributes to the CDF
      if keyword_set(DEBUG) then print,'Writing global attributes to the CDF...'
      b = tag_names(a.(0)) ; get names of attributes
      for i=0,n_elements(gattrs)-1 do begin
        g = ISTP_gattr_casecheck(b[gattrs[i]])  ; perform ISTP-case checking
        ; RCJ 01/14/2016 Same thing I did on 12/04/2015 but now for global vars:
	; Added the 'if' statement below. 'cdfmajor' is created in read_mycdf.   
        ; We do not want it written to the new cdf or it could cause conflict if it's read by read_mycdf.
        ; It is not simply a case of making the attribute value = '', we don't want it written at all.
        if strupcase(g) ne 'CDFMAJOR' then begin
          ; RCJ 03/14/2003  Update some logical attributes:
	  case strupcase(g) of
	     'LOGICAL_FILE_ID': begin
	         fname=strsplit(filename,'.',/extract)
	         if strupcase(g) eq 'LOGICAL_FILE_ID' then a.(0).(gattrs[i])=fname[0]
	         ;lsrc=a.(0).(gattrs[i])
	         ;lsrc=strsplit(lsrc[0],'_',/extract)
	     end
	     'LOGICAL_SOURCE': begin
                lsrc=a.(0).(gattrs[i])
	        lsrc=strsplit(lsrc[0],'_',/extract)
	        case n_elements(lsrc) of
	           2:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'s_'+lsrc[1])  ; added for rbspa_ect-hope-sci-l2 case.(RCJ 10/21/2013)
	           3:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2])
	           4:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2]+'_'+lsrc[3])
                   5:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2]+'_'+lsrc[3]+'_'+lsrc[4])
                   6:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2]+'_'+lsrc[3]+'_'+lsrc[4]+'_'+lsrc[5])
                   7:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2]+'_'+lsrc[3]+'_'+lsrc[4]+'_'+lsrc[5]+'_'+lsrc[6])
                   8:  a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2]+'_'+lsrc[3]+'_'+lsrc[4]+'_'+lsrc[5]+'_'+lsrc[6]+'_'+lsrc[7])
		   else: begin
		      print,'WARNING>Truncating dataset name. Dataset name too long. Please inform software developers.
		      a.(0).(gattrs[i])=strupcase(lsrc[0]+'_'+lsrc[1]+'s_'+lsrc[2]+'_'+lsrc[3]+'_'+lsrc[4]+'_'+lsrc[5]+'_'+lsrc[6]+'_'+lsrc[7])
		      end
	        endcase
	     end  
         'LOGICAL_SOURCE_DESCRIPTION' or 'DATA_TYPE': begin

	        a.(0).(gattrs[i])='DERIVED FROM: '+a.(0).(gattrs[i])
            ; If the data is binned (Keyword BINNED set) and we are working with the 
            ; LOGICAL_SOURCE_DESCRIPTION attribute, then add an appropiate suffix.
            ; Ron Yurow (Oct 23, 2018)
            IF  STRUPCASE (g) eq 'LOGICAL_SOURCE_DESCRIPTION' THEN BEGIN 
                IF  KEYWORD_SET (BINNED) THEN BEGIN 
                    a.(0).(gattrs[i]) = a.(0).(gattrs[i]) + ' (SPDF Time Binned)'
                ENDIF
            ENDIF
            ; Add appropiate suffix for binning
            ; If the data is binned (Keyword BINNED set) and we are working with the 
            ; LOGICAL_SOURCE_DESCRIPTION attribute, then add an appropiate suffix.
            ; Ron Yurow (Oct 23, 2018)
            IF  STRUPCASE (g) eq 'DATA_TYPE' THEN BEGIN 
                IF  KEYWORD_SET (BINNED) THEN BEGIN 
                    a.(0).(gattrs[i]) = a.(0).(gattrs[i]) + '>(SPDF Time Binned)'
                ENDIF
            ENDIF
            end
	     'TEXT': begin
	        if strupcase(!version.os_family) eq 'WINDOWS' then begin
	         spawn,'date /T',d
	         spawn,'time /T',t
	         d=d+t  ; to get date *and* time
	        endif else spawn,'date',d
	        der='CDAWeb'
	        spawn,'printenv SCRIPT_NAME',dd
	        if strpos(dd[0],'cdawdev') ne -1 then der='CDAWeb dev'
	        if strpos(dd[0],'cdaweb') ne -1 then der='CDAWeb ops'
	        ;string(13B))  =  <CR>   and  string(10B)) = <LF> 
            ; The following section of code was rewritten so that lines added to
            ; the TEXT attribute will be added into separate entries instead of 
            ; being appended to the first entry.  In addition, if the 
            ; APPEND_ENTRY keyword is defined, the value of that keyword is added
            ; to the end of the TEXT attribute.
            ; Ron Yurow (June 21, 2018)
	        ; a.(0).(gattrs[i])= $
	         ;;der + ' interface derived data on '+ d +string(13B)+ $
	         ;;string(10B) + a.(0).(gattrs[i])
	         ;der + ' interface derived data on '+ d +'  '+ $
	         ; RCJ 10/22/2003 Added contacts:
	        ; der + ' interface derived data on '+ d +'. Contacts:  '+ $
	        ; 'Tami.J.Kovalick@nasa.gov, Rita.C.Johnson@nasa.gov. ' + $
		    ; a.(0).(gattrs[i])

            ; Get the data structure for the variable we will be updating the 
            ; TEXT attribute for.
            vmod = a.(0)

            ; Get the TEXT attribute as an array.
            text = vmod.(gattrs[i])

            ; Add some lines at the beginning to indicate this is a derived CDF.
            desc = [der + ' interface derived data on ' + d + '.',                  $
                    'Contacts: Tami.J.Kovalick@nasa.gov, Rita.C.Johnson@nasa.gov.', $
                    '' ]
            text = [desc, text]

            ; If the APPEND_TEXT keyword is set, then append its value to the end of 
            ; the TEXT array.
            IF  KEYWORD_SET (APPEND_TEXT) THEN text = [text, '', APPEND_TEXT]

            ; Array containing all of the structure field indices for each field after 
            ; and including the TEXT field.
            after  = INDGEN (N_TAGS (vmod) - gattrs[i]) + gattrs[i]
            ; Array containing all of the structure field indices for each field prior 
            ; to and including the TEXT field.
            before = INDGEN (gattrs[i] + 1)
            ; Recreate the variable structure using the new TEXT array.
            vnew = CREATE_STRUCT (CREATE_STRUCT (vmod, REMOVE=after),     $
                                  'TEXT', text,                           $
                                  CREATE_STRUCT (vmod, REMOVE=before))
            ; Get the tag name of the variable we just modified.  This may 
            ; different than varname.
            tag = (TAG_NAMES (a)) [0]
            ; Recreate the data block using the new variable.
            a = CREATE_STRUCT (tag, vnew, CREATE_STRUCT (a, REMOVE=0))
	     end   
	     else: ;do nothing, keep going
	  endcase  
	
          aid = cdf_attcreate(id,g,/GLOBAL_SCOPE) ; create the attribute
	

          ; Now put the proper value in the attribute
          s = size(a.(0).(gattrs[i]))  
	  ns = n_elements(s)  
	  c=''
          if (s[ns-2] eq 7) then begin ; special case for string handling
            if s[0] eq 0 then begin ; single string, not an array of strings
              c = a.(0).(gattrs[i])  
	      if c ne '' then cdf_attput,id,aid,0L,c
            endif else begin ; attribute value is an array of strings
              for j=0,s[1]-1 do begin
                c=a.(0).(gattrs[i])[j] 
	        ;if c[0] ne '' then cdf_attput,id,aid,j,c
	        if c[0] eq '' then c[0]=' '
	        cdf_attput,id,aid,j,c
              endfor
            endelse
          endif else cdf_attput,id,aid,0L,a.(0).(gattrs[i])
        endif; if not 'cdfmajor'
      endfor
      
      ; Now add another global var: 'PARENTS'.
      if keyword_set(inputfiles) then begin
	q=where(strupcase(b[gattrs]) eq 'PARENTS') ; where global attr eq 'parents'
	if q[0] eq -1 then aid = cdf_attcreate(id,'PARENTS',/GLOBAL_SCOPE) $ ; create the attribute
	   else aid = cdf_attnum(id,'PARENTS') ; get id of existing attribute
	for j=0,n_elements(inputfiles)-1 do begin
	   c=inputfiles[j]
	   if c[0] eq '' then c[0]=' ' else begin
	     parts=strsplit(c,'/',/extract)
	     c=parts[n_elements(parts)-1]
	   endelse
	   cdf_attput,id,aid,j,c
	endfor
      endif
      ; Create the variables
      for i=0,n_elements(order)-1 do begin
        b = order[i] 
	;vname = a.(b).VARNAME
	q=where(already_created eq a.(b).varname)
        if q[0] eq -1 then begin
	   vid1 = create_myCDF_variable(id, a.(b),novirtual=novirtual,cdf27=cdf27_comp, $
                               no_compress=no_compress,DEBUG=DEBUG)
	   ;if vid1[0] eq -1 then return, -1
	   if vid1[0] eq -1 then continue
           already_created=[already_created,a.(b).varname]
	   new_order=[new_order,b]
	endif   
      endfor ; create and write all variables

      order=new_order[1:*]

      ; Write the variable attributes to the CDF
      for i=0,n_elements(order)-1 do begin ; loop through every variable
      ;for i=0,n_elements(tag_names(a))-1 do begin ; loop through every variable
        ;vname = a.(i).VARNAME ; get the case sensitive variable name
        vname = a.(order[i]).VARNAME ; get the case sensitive variable name
        ;vtags = tag_names(a.(i)) ; get the attribute names
        ; RCJ 12/04/2015 Added the 'if' statement below. 'cdfmajor' is created in read_mycdf.   
        ; We do not want it written to the new cdf or it could cause conflict if it's read by read_mycdf.
        ; It is not simply a case of making the attribute value = '', we don't want it written at all.
        if strupcase(vname) ne 'CDFMAJOR' then begin
          vtags = tag_names(a.(order[i])) ; get the attribute names
          from = tagindex('FIELDNAM',vtags) ; fieldnam is the first vattr
          to   = tagindex('CDFTYPE' ,vtags) ; cdftype is the next non-vattr
          for j=from,to-1 do begin ; process each variable attribute
            ;if strupcase(vtags[j]) ne 'CDFMAJOR' then begin
           if (strupcase(vtags[j]) ne 'NO_DATA' and strupcase(vtags[j]) ne 'ALLOW_BIN') then begin
            ;print,'vtags[j] = ',vtags[j]
	    case vtags[j] of 
	        'FILLVAL': begin
	           if (size(a.(order[i]).(j),/type) eq 14) then a.(order[i]).(j) = decode_CDFEPOCH(a.(order[i]).(j), /tt2000)
	        end
	        ;'DELTA_MINUS_VAR': a.(order[i]).(j)=replace_bad_chars(a.(order[i]).(j),diff)
	        ;'DELTA_PLUS_VAR': a.(order[i]).(j)=replace_bad_chars(a.(order[i]).(j),diff)
	        ;'COMPONENT_0': a.(order[i]).(j)=''
	        'COMPONENT_0': if not keyword_set(novirtual) then a.(order[i]).(j)=''
	        'COMPONENT_1': if not keyword_set(novirtual) then a.(order[i]).(j)=''
                ;11/5/04 - TJK - had to change FUNCTION to FUNCT for IDL6.* compatibility
                ;	     'FUNCTION': a.(order[i]).(j)=''
	        'FUNCT': if not keyword_set(novirtual) then a.(order[i]).(j)=''
	        'VIRTUAL': if not keyword_set(novirtual) then a.(order[i]).(j)=''
	        else: ; do nothing
	    endcase     
            if i eq 0 then aid = cdf_attcreate(id,vtags[j],/VARIABLE_SCOPE) $
             else aid = cdf_attnum(id,vtags[j]) ; get id of existing attribute
            ;if i eq 0 then begin
                ;aid = cdf_attnum(id,vtags[j]) ; get id of existing attribute
                ;help,aid
                ;if aid eq -1 then $
                ;aid = cdf_attcreate(id,vtags[j],/VARIABLE_SCOPE) 
              ;endif   
            ; Special processing is required for ISTP-stype pointer attributes.
            ; If the current attribute is such an attribute, do not process it here.
            if (amI_ISTPptr(vtags[j]) ne 1) then begin
               ;s = size(a.(i).(j)) & ns = n_elements(s)
               s = size(a.(order[i]).(j)) 
	       ns = n_elements(s)
               ;if (s(ns-2) ne 7) then cdf_attput,id,aid,vname,a.(i).(j) $
               ;if (s(ns-2) ne 7) then cdf_attput,id,aid,vname,a.(order[i]).(j) $
               if (s[ns-2] ne 7) then begin
	         if not keyword_set(novirtual) then cdf_attput,id,aid,vname,a.(order[i]).(j)
               endif else begin ; special processing for character data
                 if s[0] eq 0 then begin
                   ;e = a.(i).(j)
                   e = a.(order[i]).(j)
	     	  if e ne '' then cdf_attput,id,aid,vname,e
                 endif else begin ; data is a string array
                   print,'WARNING: ',vtags[j],' vattr not written because of IDL bug!'
                 endelse
               endelse
            endif 
            ;endif; if not 'cdfmajor'
	   endif ; if 'no_data' or 'allow_bin'
          endfor  ; processed every var attribute
        endif; if not 'cdfmajor'
      endfor

      ; Perform special processing for ISTP pointer-type attributes.  When such an
      ; attribute is located, a new metadata variable may have to be created.  This
      ; depends on how the original cdf was read.  If the original cdf was read
      ; with the /all keyword, then all variables, including non-record-varying
      ; metadata were read into the structure.  If not, then those non-record-
      ; varying variables may have been lost, in which case new variables must
      ; be created.
      mvcount = 0L
      for i=0,n_elements(tag_names(a))-1 do begin ; loop through every variable
         vtags = tag_names(a.(i)) ; get the name of every attribute
         for j=0,n_elements(vtags)-1 do begin
	    q=where(a.(i).(j) ne '')
            if ((amI_ISTPptr(vtags[j]) eq 1)AND $
	       ;(a.(i).(j)(0) ne '') and $
	       ; RCJ 11/14/2003 Replaced line above w/ line below.
	       ; If a.(i).(j) is an array we have to check all of its elements
	       ; not only the first one.
	       (q[0] ne -1)) then begin   ; and $
	       ;(strupcase(a.(i).var_type) ne 'IGNORE_DATA')) then begin
                ;print,'special processing for istpptr for var '$
		;,a.(i).varname,' attr ', vtags[j]
               ; determine if any other variable in the structure has the same
               ; value as a.(i).(j) so that unneeded metavars are not created.
               pvar = -1 ; initialize flag
               ; print,'searching existing variables for correct value'
               for g=0,n_elements(tag_names(a))-1 do begin ; loop through every var
                  if compare_vars(a.(i).(j),get_mydata(a,g)) eq 1 then begin
                     pvar  = g ; variable with matching value already exists
                     ;vname = (tag_names(a))(g)
		     ; RCJ 10/08/2003 Line above causes problems. Vname is all
		     ; capitalized , masking the real var name which can be
		     ; a combination of capital and non-capital letters.
		     ; The line below gives us the real varname.
                     vname = a.(g).varname
                     ;print,'value found in the variable ',vname,' pvar=',pvar
                   endif
               endfor

               ; if no existing variable in the structure has the correct value,
               ; then determine if any metavar has already been created with that value.
               if pvar eq -1 then begin
                  ; print,'searching previously created metavars for correct value'
                  for g=0,mvcount-1 do begin
                     if compare_vars(a.(i).(j),a.(pv[g]).(pt[g])) eq 1 then begin
                        pvar  = g ; same attribute value exists
                        vname = pvn[g] ; get name of metavar which already exists
                        ;print,'value found in the variable ',vname,' pvar=',pvar
                     endif
                  endfor
               endif

               ; if pvar still equals -1, then no variable with a matching value exists
               ; in the original structure, or has been previously created as a metavar.
               ; In this case, a new metavar must now be created.
               if (pvar eq -1) then begin
                  ; determine the name for new variable
                  vname = 'metavar' + strtrim(string(mvcount),2)
                  ; print,'creating new metavar named ',vname
                  ; create a variable structure
                  va = create_struct('VARNAME',vname)
                  vb = create_struct('VAR_TYPE','metadata')
		  ; RCJ 10/06/2003 If a.(i).(j) is not array it cannot
		  ; be reformed
		  if n_elements(a.(i).(j)) eq 1 then $
		     vc = create_struct('DAT',a.(i).(j)) else $
                     vc = create_struct('DAT',reform(a.(i).(j)))
		  ; RCJ I added the line below because I look for this attr later on...
                  vd = create_struct('CDFRECVARY','novary')
                  v  = create_struct(va,vb) & v = create_struct(v,vc) 
		  v = create_struct(v,vd)
                  ; create the new variable in the CDF
                  vid = create_myCDF_variable(id, v,novirtual=novirtual,cdf27=cdf27_comp, $
                               no_compress=no_compress,debug=debug)
		  ;if vid[0] eq -1 then return,-1
		  if vid[0] eq -1 then continue
                  cdf_attput,id,'VAR_TYPE',vname,'metadata'
                  cdf_attput,id,'FIELDNAM',vname,vname
		  ; RCJ 10/22/2003 Vars need catdesc and format to pass skteditor test.
                  cdf_attput,id,'CATDESC',vname, 'Metadata for variable '+ $
		     a.(i).varname + ' and possibly other variables.'
                  cdf_attput,id,'FORMAT',vname,'a'+strtrim(strlen(v.dat[0]),2)
                  ; record the number of the new variable and attribute tag number
                  if mvcount eq 0 then begin
                     pv = i & pt = j & pvn = vname
                  endif else begin
                     pv = [pv,i] & pt = [pt,j] & pvn = [pvn,vname]
                  endelse
                  mvcount = mvcount + 1 ; increment metavariable count
               endif
               ; point to the correct variable from the istp-pointer type attribute
               ; print,'setting ',a.(i).VARNAME,' ',vtags[j],' to ',vname
               cdf_attput,id,vtags[j],a.(i).VARNAME,vname
            endif
         endfor
      endfor ; end i
      ; Close the new CDF
      cdf_close,id
      ;   RCJ 10/10/2012
      ; if there was an error in create_myCDF_variable - and we are concerned w/ EPOCH_16, INT8 and TT2000 -
      ; then epoch is not written to this cdf and it's useless to the user. We'll remove this cdf name
      ; from 'files' so its link is not displayed on the web page.
      if vid1[0] ne -1 then begin 
        files=[files,filename]
        datasets=[datasets,a.(0).logical_source]
      endif
   endelse ;-- closes out unused check for at least one plottable variable
 endif else begin ; if (size(a,/type) ne 8 ie, not a structure. -- from 675
   print,'WARNING: Input ',strtrim(k,2),' is not a structure'
 endelse
endfor ;---- ends loop begining at 672

print, 'write_mycdf took ',systime(1)-ttime, ' seconds to run'
; RCJ 12/30/2002
; the 'print' lines below are needed so parse.ph will be able to get this information from 
; the idl log file. You have to make sure each print line fits in one line of output.
; If the filename, for example, ends up in the next line parse.ph will read 'new_cdf' as empty.
if n_elements(files) gt 1 then begin 
   files=files[1:*]
   datasets=datasets[1:*]
   for k=0,n_elements(files)-1 do begin
      print, 'DATASET=',strupcase(datasets[k])
      ; RCJ  This next line is needed for the web interface
      if keyword_set(outdir) then print, 'CDF_OUTDIR=',outdir
      fmt='(a'+strtrim((7+strlen(strtrim(string(k),2))),2)+',1a,a'+strtrim(strlen(files[k]),2)+')'
      print, 'NEW_CDF'+strtrim(string(k),2),'=',files[k],format=fmt
   endfor
endif ;else return,-1
;
; RCJ 11/05/2013 Back to default if cdf27_comp was set to 'yes' (this may not be needed. remove if causes problems)
cdf_set_cdf27_backward_compatible, /no

; RCJ 06/02/2014  !Quiet back to original:
!quiet=origq

return,0
end

