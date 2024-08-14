;
; This utilities package includes functions used by read_myCDF.pro and read_mynetcdf.pro
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;---------------------------------------------------------------------------
 
function write_fill, vn_sdat, burley, tmp_str

;v_err='ERROR=Instrument off; fillval=dat'
v_stat='STATUS=Instrument off for variable '+vn_sdat+'.  Re-select variable or time. '
atags=tag_names(burley.(0))
b0 = tagindex('LOGICAL_SOURCE',atags)
b1 = tagindex('LOGICAL_FILE_ID',atags)
b2 = tagindex('Logical_file_id',atags)
if (b0[0] ne -1) then  psrce = strupcase(burley.(0).LOGICAL_SOURCE)
if (b1[0] ne -1) then $ 
   psrce = strupcase(strmid(burley.(0).LOGICAL_FILE_ID,0,9))
if (b2[0] ne -1) then $     
   psrce = strupcase(strmid(burley.(0).Logical_file_id,0,9))
v_data='DATASET='+psrce
; Reduce the number of reported errors to the developers RTB 1/97
;tmp_str=create_struct('DATASET',v_data,'ERROR',v_err,'STATUS',v_stat)
tmp_str=create_struct('DATASET',v_data,'STATUS',v_stat)
ikill=1

return, ikill
end

;------------------------------------------------------------------------------------

;This function checks to make sure a given variables 'varname' attribute 
;actually matches its structure members name.
;
function correct_varname, struct, varnames, index

;TJK 09/29/00 Put in a check to make the varname attribute value match
;its variables structure tag name - if it doesn't list_mystruct won't work...
;This is all necessary for the upgrade to IDL5.3
str_index = strtrim(string(index),2) ;convert to string
comm = execute('att_names = tag_names(struct.('+str_index+'))')
if (comm eq 1) then begin
   att_v = where(att_names eq 'VARNAME', att_cnt)
   if (att_cnt gt 0) then begin
      ;assign the variable name to the "VARNAME" attribute for this variable...
      ;assign_string = 'struct.'+varnames(index)+'.('+strtrim(string((att_v[0])),2)+')='''+varnames(index)+'''
      assign_string = 'struct.('+str_index+').('+strtrim(string((att_v[0])),2)+')='''+varnames[index]+'''
      ;print, 'assign_string = ',assign_string
      comm = execute(assign_string)
      if (comm ne 1) then print, 'execute failed for ',assign_string 
   endif
endif ;end TJK mod 09/29/00

return, struct
end

;------------------------------------------------------------------------------------

;uniq_array takes two string arrays (like lists of variable names)
;and doesn't modify the 1st, but adds the second to the 1st w/o adding
;any duplicates.
function unique_array, first, second
   x = where(second ne '',xcnt)
   if (xcnt gt 0) then begin
     second = second[x]
     u_s_index = uniq(second, sort(second))
     for s=0,n_elements(u_s_index)-1 do begin
        for u=0,n_elements(first)-1 do begin
          if (first[u] eq second[u_s_index[s]]) then  begin
             ;print, first[u], second[u_s_index[s]]
             second[u_s_index[s]] = ' '
          endif
        endfor
      endfor
      t = where(second ne ' ',tcnt)
      if (tcnt gt 0) then first = [first,second[t]] ;append sorted/uniqed
   endif
return, first
end
;+------------------------------------------------------------------------
; NAME: PARSE_DISPLAY_TYPE
; PURPOSE: 
;	Parse and examine the input string.  It should be the value of the
;	CDF attribute 'DISPLAY_TYPE'.  Return an array of variable names
;       that it 'points' to.
; CALLING SEQUENCE:
;	out = parse_display_type(instring)
; INPUTS:
;       instring = string, value of a CDF attribute called 'DISPLAY_TYPE'
; KEYWORD PARAMETERS:
; OUTPUTS:
;       out = string array, names of other variables required for display
; NOTES: This routine expects to find 'DISPLAY_TYPE' values looking like:
;        PLOT_TYPE>x=vname,y=vname ...
;        PLOT_TYPE>y=vname,z=vname(*,1),z=vname(*,2) ...
; AUTHOR:
;       Richard Burley, NASA/GSFC/Code 632.0, Feb 13, 1996
;       burley@nssdca.gsfc.nasa.gov    (301)286-2864
; MODIFICATION HISTORY:
;TJK modified 01/27/98 to not parse orbit display type here - the items
;specified for the orbit plot type aren't additional variables.
;TJK modified 09/25/2001 to not parse the "symsize" keyword because its
;value isn't a variable.
;
;-------------------------------------------------------------------------
FUNCTION parse_DISPLAY_TYPE, instring
num_vnames = 0L & spos = 0L & i=0L ; initialize
a = break_mySTRING(instring,DELIMITER='>') ; break string into components
if n_elements(a) le 1 then return,-1 ; no '>' following plot type
if a[1] eq '' then return,-1 ; no info past '>' to parse
if strlowcase(a[0]) eq 'orbit' then return,-1 ; dont want to parse orbit.

lastn=n_elements(a)-1  ; RTB added 3/98
b = break_mystring(a[lastn],delimiter=',') ; Parse the string into substrings
for i=0,n_elements(b)-1 do begin ; examine each substring
   s = strpos(b[i],'=') & p = strpos(b[i],'(') ; find '=' and '(' signs
   if s ne -1 then begin ; probably a properly formated DISPLAY_TYPE vattr
      if(strlowcase(strmid(b[i],0,s)) ne 'symsize') then begin
        s = s + 1 ; point to first character past the '=' sign
        if p ne -1 then vname = strmid(b[i],s,[p-s]) $ ; extract vname
           else vname = strmid(b[i],s,(strlen(b[i])-s))   ; extract vname
        if num_vnames eq 0 then begin
           variable_names = vname & num_vnames = num_vnames + 1
        endif else begin
           index = where(variable_names eq vname,wc)
           if wc eq 0 then begin
              variable_names = [variable_names,vname] ; add to existing list
              num_vnames = num_vnames + 1 ; keep count of number of names
           endif
        endelse
      endif ;if the left side of the equal sign isn't equal to "symsize"
   endif
endfor

if num_vnames eq 0 then return,-1 else return,variable_names
end


;Function correct_vnames(vnames)
;This function takes a list of variable names, checks to see if any of them
;are in the structure called "table" which has a mapping of the "real" variable names
;to those who've been "corrected" in order to run under IDL5.3.

Function correct_vnames, vnames
common global_table, table

if (n_elements(table) gt 0) then begin
   for i = 0, n_elements(table.equiv)-1 do begin
      c_index = where(vnames eq table.equiv[i], c_count)
      if (c_count ge 1) then vnames[c_index[0]] = table.varname[i]
   endfor
endif

return, vnames
end


;+------------------------------------------------------------------------
; NAME: follow_mydepends
; PURPOSE: 
;	Search the metadata anonymous structure for ISTP 'DEPEND' attributes.
;       If and when found, add the variable name that it points to to the
;       vnames array if it is not already present, and increase the size
;       of the dhids and mhids arrays.
; CALLING SEQUENCE:
;       follow_mydepends, metadata, vnames, dhids, mhids
; INPUTS:
;       metadata = anonymous structure holding attribute values
;       vnames   = string array of the names of variables already processed
;       vvarys   = string array of the record variance for each variable
;       dhids    = array of data handle id's
;       mhids    = array of metadata handle id's
;       dlstid   = array of handle id's. Each points to a list of dependent variables.
; KEYWORD PARAMETERS:
;       DEPEND0  = array of flags do indicate if the variables is referenced by
;                  the DEPEND_0 attribute of another variable.
; OUTPUTS:
;       dhids    = array of data handle id's
;       mhids    = array of metadata handle id's
;       dlstid   = array of handle id's. Each points to a list of dependent variables.
; AUTHOR:
;       Richard Burley, NASA/GSFC/Code 632.0, Feb 13, 1996
;       burley@nssdca.gsfc.nasa.gov    (301)286-2864
; MODIFICATION HISTORY:
;       Added parameter dlstid   (Ron Yurow 11/16/2018)
;       Moved the routine from the read_myCDF.pro file to 
;       the spdf_read_utilities.pro file (TJK 11/21/2018)
;       
;-------------------------------------------------------------------------
PRO follow_mydepends, metadata, vnames, vvarys, ctypes, dhids, mhids, dlstid, DEPEND0=dpnd0

tnames = tag_names(metadata)
for i=0,n_elements(tnames)-1 do begin
   ; Determine if the current tagname is a legal ISTP-style DISPLAY_TYPE vattr
   if (tnames[i] eq 'DISPLAY_TYPE') then begin
      dvnames = parse_DISPLAY_TYPE(metadata.(i)) & dvs = size(dvnames)
      if (dvs[n_elements(dvs)-2] eq 7) then begin ; variable names found
         for j=0,n_elements(dvnames)-1 do begin
	    dvnames[j] = correct_vnames(dvnames[j]) ;look for variable names that have
	    ;been corrected (no illegal characters)
	    ;replace them w/ their "real" names 
	    ;so that their associated data can be
	    ;retrieved from the cdf files.
            a = where(vnames eq dvnames[j],count) ; search vnames array so no duplicates.
            if ((dvnames[j] ne '')AND(count eq 0)) then begin
               ; add the display variable name all array parameters
               n = n_elements(vnames)
               newn = strarr(n+1) & newd = lonarr(n+1) & newm = lonarr(n+1)
               newv = strarr(n+1) & newr = lonarr(n+1) & newc = strarr(n+1)
               newn[0:[n-1]] = vnames[0:[n-1]]
               newv[0:[n-1]] = vvarys[0:[n-1]]
               newc[0:[n-1]] = ctypes[0:[n-1]]
               newd[0:[n-1]] = dhids[0:[n-1]]
               newm[0:[n-1]] = mhids[0:[n-1]]
               newn[n]=dvnames[j] & newv[n]='' & newc[n]='' & newd[n]=0 & newm[n]=0
               vnames=newn & vvarys=newv & ctypes=newc & dhids=newd & mhids=newm
               ; Add new 
               dlstid = [dlstid, 0] 
               ; Add a new element to the array of depend 0 flags.  We will do this
               ; even if this information was not requested.
               ; Ron Yurow (Sept 24, 2019)
               ; New elements are added with a value of -1 (no depend_0 attribute found)
               ; Ron Yurow (Nov 15, 2019)
               ; IF  N_ELEMENTS (dpnd0) gt 0 THEN dpnd0 = [dpnd0, 0]    
               IF  N_ELEMENTS (dpnd0) gt 0 THEN dpnd0 = [dpnd0, -1]    

            endif
         endfor
      endif
   endif

   ; Determine if the current tagname is a legal ISTP-style depend attribute
   ;TJK 8/11/2015 - do not include DEPEND_EPOCH0 which is a THEMIS specific attribute
   len = strlen(tnames[i]) & pos = strpos(tnames[i],'DEPEND_') & th_exclude = (tnames[i] ne 'DEPEND_EPOCH0')
   if ((len gt 7)AND(pos eq 0) AND th_exclude) then begin ; DEPEND found, check remainder
      ON_IOERROR, escape ; return false if non-digit found
      for j=0,(len-8) do begin ; check one character at a time
         r = strmid(tnames[i],(7+j),1) & READS,r,v,FORMAT='(I1)'
      endfor
      dvname = metadata.(i) ; depend attribute FOUND
      dvname = correct_vnames(dvname) ;look for variable names that have
      ;been corrected (no illegal characters)
      ;replace them w/ their "real" names 
      ;so that their associated data can be
      ;retrieved from the cdf files.
      
      ; TJK 9/8/03 remove this because we can't touch these attribute values because they
      ;they need to be the real variables name.
      ; RCJ 08/11/03 in case we have " " where a "" should be.
;      dvname = strtrim(dvname,2)
;debug      help, dvname

      a = where(vnames eq dvname,count) ;search vnames array to make sure
				      ;there are no duplicates
      if ((dvname ne '')AND(count eq 0)) then begin
         ; add the depend variable name to all array parameters
         n = n_elements(vnames)
         newn = strarr(n+1) & newd = lonarr(n+1) & newm = lonarr(n+1)
         newv = strarr(n+1) & newr = lonarr(n+1) & newc = strarr(n+1)
         newn[0:[n-1]] = vnames[0:[n-1]]
         newv[0:[n-1]] = vvarys[0:[n-1]]
         newc[0:[n-1]] = ctypes[0:[n-1]]
         newd[0:[n-1]] = dhids[0:[n-1]]
         newm[0:[n-1]] = mhids[0:[n-1]]
         newn[n] = dvname & newv[n]='' & newc[n]='' & newd[n]=0 & newm[n]=0
         vnames = newn & vvarys=newv & ctypes=newc & dhids=newd & mhids=newm
         ; Need to also add a new element onto the array of dependent variable 
         ; lists and array of depend 0 flags.
         ; Ron Yurow (Nov 26, 2019)
         dlstid = [dlstid, 0] 
         IF  N_ELEMENTS (dpnd0) gt 0 THEN dpnd0 = [dpnd0, -1]    
      endif

      ; Add the variable being processed to the list of dependent varaibles, but
      ; do this only if the string is not blank.
      ; Ron Yurow (ov 16, 2016)
      IF  STRLEN (STRCOMPRESS (dvname, /remove_all)) gt 0 THEN BEGIN
          IF  SIZE (depend, /TYPE) ne 0 THEN depend = [depend, dvname] ELSE depend = [dvname]           
      ENDIF

   endif
   escape: ; Current tag name is not a depend attribute
endfor
; Create a handle that points to the list of dependent variables for the current variable
; being processed and then add that handle to the array of dependent lists.
; Ron Yurow (Nov 19, 2018)
ind = WHERE (metadata.varname eq vnames, cnt) 
IF  cnt gt 0 && SIZE (depend, /TYPE) ne 0 THEN BEGIN
    dlstid [ind] = HANDLE_CREATE ()
    HANDLE_VALUE, dlstid [ind], depend, /SET  
ENDIF
; Flag any variables that are referenced by another variables DEPEND_0 attribute. 
; Ron Yurow (Sept 24, 2019)
; Rewritten to actually referance variable that is the target of the DEPEND_0 attribute
; instead of just setting a flag.
; Ron Yurow (Nov 15, 2019)
IF  N_ELEMENTS (dpnd0) gt 0 THEN BEGIN
    ; Check that the metadata has a DEPEND_0 variable defined.
    sink = WHERE (tnames eq 'DEPEND_0', dpnd_0_exist)
    ; Check that it is filled with variable name and not a zero length string.
    IF  dpnd_0_exist && STRLEN (metadata.depend_0) gt 0 THEN BEGIN
        ; Get the index into the vnames array for the variable whose metadata we
        ; are processing.
        indx = WHERE (metadata.varname eq vnames, found)
        indx = indx [0]
        ; Set the dpnd0 entry for the variable to the index in the vnames array of its
        ; depend_0 attribute.
        IF found gt 0 THEN BEGIN
           target = WHERE (metadata.depend_0 eq vnames, found_depend_0)
           IF found_depend_0 THEN dpnd0 [indx] = target
         ENDIF
    ENDIF
ENDIF
end

