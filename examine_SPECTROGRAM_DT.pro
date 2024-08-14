;$Author: tkovalic $
;$Date: 2020/04/01 20:05:38 $
;$Header: /home/cdaweb/dev/control/RCS/examine_SPECTROGRAM_DT.pro,v 1.30 2020/04/01 20:05:38 tkovalic Exp rcjohns1 $
;$Locker: rcjohns1 $
;$Revision: 1.30 $
;

; The following functions are used to Analyze/Parse the value of the DISPLAY_TYPE attribute.
; All of them require an error handler be estrablished by the top level calling program by 
; using the CATCH procedure, otherwise parse errors will be fatal, which is probably not what
; is desired.

; Basic token recognition function.  Takes a regular expression (regex) and tries to extract it from 
; the begining of the input string (food).   If found, it will return the token.  Otherwise will either
; throw an error (if the REQ keyword is set) or return an empty string.  The WS keyword ignores
; whitespace prior to the token.
; Ron Yurow (Feb 23, 2018)
FUNCTION dt_extract_regex, food, regex, REQ=req, MSG=msg, WS=ws

    IF KEYWORD_SET (ws) THEN regex = '^[ ]*' + regex ELSE regex = '^' + regex

    pos = STREGEX (food, regex, LENGTH=len)
 
    IF  len ne -1 THEN BEGIN

        r = STRCOMPRESS (STRMID (food, 0, len),  /REMOVE_ALL)
        food = STRMID (food, len)

        RETURN, r
 
    ENDIF

    req = KEYWORD_SET (req)

    IF ~KEYWORD_SET (msg) THEN msg = "Error parsing value of DISPLAY_TYPE string"

    IF req THEN MESSAGE, msg, /NOPREFIX, /NOPRINT, /NONAME

    RETURN, ''

END

; Wrapper for dt_extract_regex.  Used to extract a varaiable name.  Since variable names
; are always required, the REQ flag is set by default.
; Ron Yurow (Feb 23, 2018)
FUNCTION dt_extract_varname, food

    regex = '[a-zA-Z][a-zA-Z_0-9-]*'

    msg   = 'Error parsing variable name in DISPLAY_TYPE string'

    RETURN, dt_extract_regex (food, regex, /REQ, /WS, MSG=msg)

END

; Wrapper for dt_extract_regex.  Used to extract a single character.  Characters special
; to regular expressions are detected and a forward slash (escape) is prepended to them.
; Ron Yurow (Feb 23, 2018)
FUNCTION dt_extract_chr, chr, food, REQ=req

    special = '[]\().+?{}|^$'

    msg   = 'Unexpected character found in in DISPLAY_TYPE string.  Expected: ' + chr

    chr = STRMID (chr, 0, 1)
    IF STRPOS (special, chr) ge 0 THEN chr = '\' + chr

    RETURN, dt_extract_regex (food, chr, REQ=req, /WS, MSG=msg)

END

; Wrapper for dt_extract_regex.  Used to extract a single element of a channel specification
; (energy, pitch angle, etc).  Each element expected to be a single number or the '*'.  
; Ron Yurow (Feb 23, 2018)
FUNCTION dt_extract_single_arg, food

    msg   = "Unexpected character found in in DISPLAY_TYPE string.  Expected number or '*'"

    RETURN, dt_extract_regex (food, '[0-9]+|\*', /REQ, /WS, MSG=msg)

END

; Extracts a channel specification string from the beginning of the input string.  The channel
; specification consists of a string of comma separated numbers separated enclosed in parenthesis.
; One of the numbers may be replaced with an asterisk.  The channel specification is considered 
; optional, so the function will return the empty string if the first character of the input string
; is not '('.  Otherwise it throw an error if the channel specification contains unexpected 
; characaters.
; Ron Yurow (Feb 23, 2018)
FUNCTION dt_extract_arg_str, food

    IF  dt_extract_chr ('(', food)  THEN BEGIN
          
        str = '('          
        str += dt_extract_single_arg (food)
          
        WHILE  (dt_extract_chr (',', food)) DO BEGIN

           str += ','
           str += dt_extract_single_arg (food)
                           
        ENDWHILE

        str += dt_extract_chr (')', food, /REQ )

        RETURN, str

    ENDIF

    RETURN, ''
END

; Extracts a single term from the beginning of the input string.  Throws an error if the 
; term can not be reduced. 
; Ron Yurow (Feb 23, 2018)
FUNCTION dt_extract_term, food

     term = ''

     term += dt_extract_varname (food)
     term += dt_extract_chr ("=", food, /REQ)
     term += dt_extract_varname (food)
     term += dt_extract_arg_str (food)

     RETURN, term
   
END 


;TJK April 17, 1997
;Modified to also parse the DISPLAY_TYPE string for STACK_PLOT type
;as well as Spectrograms and Ionograms.
;TJK July 30, 1999
;Modified to parse DISPLAY_TYPE for TIME_SERIES (y=flux[1])
;
FUNCTION examine_SPECTROGRAM_DT, instring, thedata=thedata, data_fillval=data_fillval, $
           valid_minmax=valid_minmax, debug=debug

; Initialize local variables
xname='' & yname='' & zname='' & lptrn=1 & igram=0
npanels=0 & dvary=-1
elist=lonarr(1) 
zelist1=lonarr(1)
zelist2=lonarr(1)
zelist3=lonarr(1)
zelist4=lonarr(1)

; Verify that instring is for spectrogram plot and instructions exist
a = break_mystring(instring,delimiter='>')
;TJK change to check for anythign w/ ionogram in it, since the values
;actually specified in our masters are "TOPSIDE" or "BOTTOMSIDE_IONOGRAM"
;if (strupcase(a[0]) eq 'IONOGRAM') then igram=1 $
if (strpos(strupcase(a[0]),'IONOGRAM') ge 0) then igram=1 $
else if (strupcase(a[0]) eq 'STACK_PLOT') then igram=0 $
else if (strupcase(a[0]) eq 'PLASMAGRAM') then igram=0 $
else if (strupcase(a[0]) eq 'PLASMA_MOVIE') then igram=0 $
else if (strupcase(a[0]) eq 'TIME_SERIES') then igram=0 $
else if (strupcase(a[0]) eq 'TIME_TEXT') then igram=0 $
else if (strupcase(a[0]) ne 'SPECTROGRAM') then return,-1 ; neither
;TJK 02/14/2002 - chage this to return the initialized structure
; instead of -1 if (n_elements(a) eq 1) then return,-1 ; no instructions

num_elements = n_elements(a)
if (num_elements gt 1)then begin ;additional display type syntaxfound...

;Set variable to display_type element w/ additional display type
;syntax in it
extra_syntax = a[num_elements-1]
 
; Count the number of '=' to determine the number of instructions
;b=0 & for i=0,strlen(a[1])-1 do if (strmid(a[1],i,1) eq '=') then b=b+1
b=0 & for i=0,strlen(extra_syntax)-1 do if (strmid(extra_syntax,i,1) eq '=') then b=b+1

if (b ge 1) then begin
   ilist = strarr(b) 
endif else begin ;syntax like time_series>noauto found
;TJK 10/12/2007 - try returning zero instead of 1 since not all
;                 timeseries variables are just a single panel.  Zero
;                 should force the code in the calling routine to
;                 determine the number of panels based on the data size.
;   npanels=1 ;return,-1 ; no valid instructions
   npanels=0 ;return,-1 ; no valid instructions
   ;return the structure instead of -1
   ; initialize the structure which is output from this function
;TJK 12/7/2012 - add elist2 and elist3 so that we can support 4-d
;                data for use w/ spectrograms and plasmagrams(x-y
;                image plots).
   out = {x:xname,y:yname,z:zname,npanels:npanels,$
         dvary:dvary,elist:elist,zelist1:zelist1,zelist2:zelist2,zelist3:zelist3,zelist4:zelist4,lptrn:lptrn,igram:igram}
   return,out
endelse

; The following sectiion of code was removed replaced by the section below, 
; which implements a more robust parser than the original version.
; Ron Yurow (Feb 23, 2018)
; Dissect the input string into its separate instructions
;inum = 0 & next_target=',' ; initialize
;for i=0,strlen(extra_syntax)-1 do begin
;  c = strmid(extra_syntax,i,1) ; get next character in string
;  if (c eq next_target) then begin
;    if (next_target eq ',') then inum = inum + 1
;    if (next_target eq ')') then begin
;      ilist[inum] = ilist[inum] + c & next_target = ','
;    endif
;  endif else begin
;    ilist[inum] = ilist[inum] + c ; copy to instruction list
;    if (c eq '(') then next_target = ')'
;  endelse
;endfor

inum = 0
sink = ''

; Handle errors from parsing the DISPLAY_TYPE value.  Any error throws
; an exception which winds up here.
; Ron Yurow (Feb 23, 2018)
CATCH, err_status
IF err_status NE 0 THEN BEGIN
    PRINT, 'STATUS=', !ERROR_STATE.MSG
    CATCH, /CANCEL
    RETURN, -1
ENDIF

; Parse the display value of the DISPLAY_TYPE attribute, filling the ilist 
; array with individual components.  
 
; We make use of the the following functions to implement a top down parsers using the
; STREGEX function for tokenization:

; dt_extract_regex
; dt_extract_varname
; dt_extract_chr
; dt_extract_single_arg
; dt_extract_arg_str
; dt_extract_term
; Ron Yurow (Feb 23, 2018)

ilist[inum++] = dt_extract_term (extra_syntax)

;Rewriting ilist to handle z=(*,^x) logic. This way the ilist that
;gets referenced in plotting logic later will behave as expected.
if (STRPOS(extra_syntax, '*,^') NE -1) then begin ;handle z=(*,^x) logic, where x is the variable's 2nd dimension's size
  dimStart = STRPOS(extra_syntax, '*,^') + 3
  dimEnd = STRPOS(extra_syntax, ')')
  dimDiff = dimEnd-dimStart
  dimSizeStr = STRMID(extra_syntax, dimStart, dimDiff)
  dimSize = uint(dimSizeStr)
  dimStart = dimStart - 1
  zSubStr = STRMID(extra_syntax, 0, dimStart)
  extra_syntax = ""
  for i=1,dimSize do begin
    extra_syntax = extra_syntax + zSubStr + strtrim(string(i),2) + ')'
  endfor
  firstVal = ilist[0]
  ilist = strarr(dimSize+1)
  ilist[0] = firstVal
endif
if (STRPOS(extra_syntax, '(^') NE -1) then begin ;handle z=(^x,*) logic, where x is the variable's 1st dimension's size
  dimStart = STRPOS(extra_syntax, '(^') + 2
  dimEnd = STRPOS(extra_syntax, '*)') - 1
  dimDiff = dimEnd-dimStart
  dimSizeStr = STRMID(extra_syntax, dimStart, dimDiff)
  dimSize = uint(dimSizeStr)
  zSubStr1 = STRMID(extra_syntax, 0, dimStart-1)
  zSubStr2 = STRMID(extra_syntax, dimStart+dimDiff, dimEnd)
  extra_syntax = ""
  for i=1,dimSize do begin
    extra_syntax = extra_syntax + zSubStr1 + strtrim(string(i),2) + zSubStr2 
  endfor
  firstVal = ilist[0]
  ilist = strarr(dimSize+1)
  ilist[0] = firstVal
endif


WHILE (STRLEN (extra_syntax) gt 0) DO BEGIN

    sink = dt_extract_chr (",", extra_syntax, /REQ)
    ilist[inum++] = dt_extract_term (extra_syntax)

ENDWHILE

;print, 'DEBUG in examine_spectrogram_dt after dt_extact_chr',ilist

; Done parsing, so remove the error handler.
; Ron Yurow (Feb 23, 2018)
CATCH, /CANCEL

; Determine if the xaxis variable is present as an instruction
for inum=0,n_elements(ilist)-1 do begin
  b=strpos(ilist[inum],'x=') & c=strpos(ilist[inum],'X=') & if c gt b then b=c
  if (b ne -1) then begin ; extract the name of the x variable
    c = break_mystring(ilist[inum],delimiter='=') & xname = c[1]
  endif
endfor

; Determine if the yaxis variable is present as an instruction
; looking for an instruction like y=flux[1] or y=flux

for inum=0,n_elements(ilist)-1 do begin
  b=strpos(ilist[inum],'y=') 
  c=strpos(ilist[inum],'Y=') 
  if c gt b then b=c
  if (b ne -1) then begin ; extract the name of the y variable and elist
    c = break_mystring(ilist[inum],delimiter='=') 
    yname = c[1]
    d = break_mystring(c[1],delimiter='(')
    if (n_elements(d) eq 2) then begin
      yname = d[0] 
      npanels = npanels + 1
      c = strmid(d[1],0,(strlen(d[1])-1)) ; remove closing quote
      d = break_mystring(c,delimiter=',') ; split into components
       if (n_elements(d) eq 1) then begin  ; just look for the index #
	dvary=0 
	lptrn=1
        if (npanels eq 1) then elist[0] = long(d[0]) $
	else elist = [elist,long(d[0])] ;this extends the size of elist
      endif
    endif
  endif
endfor


; Determine if the zaxis variable is present as an instruction
Z_index = 0
for inum=0,n_elements(ilist)-1 do begin
  b=strpos(ilist[inum],'z=') 
  c=strpos(ilist[inum],'Z=') 
  if c gt b then b=c
  if (b ne -1) then begin ; extract the name of the z variable and elist
    c = break_mystring(ilist[inum],delimiter='=') & zname0 = c[1]
    d = break_mystring(c[1],delimiter='(')
    ;look for an occurrence of z=(*,1) or z=(1,1,*), etc.
    if (n_elements(d) eq 2) then begin

      zname = d[0] 
      ;npanels = npanels + 1
      c = strmid(d[1],0,(strlen(d[1])-1)) ; remove closing quote
      d = break_mystring(c,delimiter=',') ; split into components
      if (n_elements(d) eq 2) then begin  ; the form is *,n or n,*
        if (d[0] eq '*') then begin & dvary=0 & lptrn=2
          ;if npanels eq 1 then elist[0] = long(d[1]) $
          ;else elist = [elist,long(d[1])]
          if npanels eq 0 then begin
	     elist[0] = long(d[1]) 
	     npanels=npanels+1
          endif else begin
	     ; RCJ 03/15/2013  If data_fillval and valid_minmax are present
	     ; check data for each panel. If panel would be empty of good data do
	     ; not count this as a valid panel.  This will avoid blank spaces 
	     ; between the last plot and the credits for the plot and data.
	     if keyword_set(thedata) and keyword_set(data_fillval) then begin
		q=-1 & qq=-1 & qqq=-1
		;  RCJ 11/04/2016  Added lines below to crop array so program can move on an produce an output, otherwise we get a fatal error.
		if long(d[1]) gt n_elements(thedata[0,*,0]) then begin
                  d[1]=strtrim(n_elements(thedata[0,*,0]),2)
                   print,'WARNING= Array in master cdf is defined larger than the data cdf actually is for this time frame. Cropping array in examine_SPECTROGRAM_DT.'
	        endif
		;
	        q=where(thedata[*,long(d[1])-1,*] ne data_fillval)
		if q[0] ne -1 then begin
		  if keyword_set(valid_minmax) then begin
		     ;qq=where((thedata[*,long(d[1])-1,*])[q] le valid_minmax[0])  
                     ;qqq=where((thedata[*,long(d[1])-1,*])[q] ge valid_minmax[1])
		     ; RCJ 05Apr2017  Should be lt and gt validmin/max, min and max should be included
		     qq=where((thedata[*,long(d[1])-1,*])[q] lt valid_minmax[0])  
                     qqq=where((thedata[*,long(d[1])-1,*])[q] gt valid_minmax[1])
		     if qq[0] ne -1 then nqq=n_elements(qq) else nqq=0
		     if qqq[0] ne -1 then nqqq=n_elements(qqq) else nqqq=0
		     if ((nqq + nqqq) eq n_elements(q)) then q[0]=-1
		     if q[0] eq -1 then print,'WARNING= Data out of validmin/max for ',zname0
		     if keyword_set(debug) then begin
		        ; RCJ 03/15/2013  10% was determined by Bob.
		        if (nqq+nqqq) gt n_elements(thedata)*.1  $ ; more than 10% of data is out of valid min/max
		           then print,'WARNING= More than 10% of data points are outside validmin/max. ' + $
			            'It may be time to revise those values. (in examine_SPECTROGRAM_DT)'
		     endif		    
		  endif		
		endif else begin  ; all data is fillval
		   print,'WARNING= All data is fillval for ',zname0
		endelse
	        ;if q[0] ne -1 then elist = [elist,long(d[1])]
                ;if q[0] ne -1 then npanels = npanels + 1
		; RCJ 31Jan2017  We want all panels, even if they are going to be labeled 'no good data'
	        elist = [elist,long(d[1])]
                npanels = npanels + 1
	     endif else begin 
	        elist = [elist,long(d[1])]
                npanels = npanels + 1
	     endelse	
	  endelse 	  
        endif
        if (d[1] eq '*') then begin & dvary=1 & lptrn=1
          ;if npanels eq 1 then elist[0] = long(d[0]) $
          ;else elist = [elist,long(d[0])]
          if npanels eq 0 then begin 
	     elist[0] = long(d[0]) 
	     npanels=npanels+1
          endif else begin
	     if keyword_set(thedata) and keyword_set(data_fillval) then begin
		q=-1 & qq=-1 & qqq=-1
		;  RCJ 11/04/2016  Added lines below to crop array so program can move on an produce an output, otherwise we get a fatal error.
		if long(d[0]) gt n_elements(thedata[*,0,0]) then begin
                   d[0]=strtrim(n_elements(thedata[*,0,0]),2)
                   print,'WARNING= Array in master cdf is defined larger than the data cdf actually is for this time frame. Cropping array in examine_SPECTROGRAM_DT.'
	        endif
		;
	        q=where(thedata[long(d[0])-1,*,*] ne data_fillval)
		if q[0] ne -1 then begin
		  if keyword_set(valid_minmax) then begin
		     ;qq=where((thedata[long(d[0])-1,*,*])[q] le valid_minmax[0])  
                     ;qqq=where((thedata[long(d[0])-1,*,*])[q] ge valid_minmax[1])
		     ; RCJ 05Apr2017  Should be lt and gt validmin/max, min and max should be included
		     qq=where((thedata[long(d[0])-1,*,*])[q] lt valid_minmax[0])  
                     qqq=where((thedata[long(d[0])-1,*,*])[q] gt valid_minmax[1])
		     if qq[0] ne -1 then nqq=n_elements(qq) else nqq=0
		     if qqq[0] ne -1 then nqqq=n_elements(qqq) else nqqq=0
		     if ((n_elements(qq) + n_elements(qqq)) eq n_elements(q)) then q[0]=-1		
		     if q[0] eq -1 then print,'WARNING= Data out of validmin/max for ',zname0
		     if keyword_set(debug) then begin
		        if (nqq+nqqq) gt n_elements(thedata)*.1  $ ; more than 10% of data is out of valid min/max
		        then print,'WARNING= More than 10% of data points are outside validmin/max. ' + $
			            'It may be time to revise those values. (in examine_SPECTROGRAM_DT)'
		     endif
		  endif   		
		endif else begin  ; all data is fillval
		   print,'WARNING= All data is fillval for ',zname0
		endelse
	        ;if q[0] ne -1 then elist = [elist,long(d[0])]
                ;if q[0] ne -1 then npanels = npanels + 1
		; RCJ 31Jan2017  We want all panels, even if they are going to be labeled 'no good data'
	        elist = [elist,long(d[0])]
                npanels = npanels + 1
	     endif else begin 
	        elist = [elist,long(d[0])]
                npanels = npanels + 1
	     endelse	
	  endelse   
        endif
     endif
     ;TJK 12/6/2012 new section for more than 2-d data
      if (n_elements(d) gt 2) then begin  ; the form is n,n,* or *,n,n
        ;initialize the arrays to -1 which means take the whole array "*"

        if (Z_index eq 0) then begin
          if (inum eq 1) then adjust = 1 ;1st element is a "y=var"
          if (inum eq 0) then adjust = 2 ;all elements are "z=var"
          if (inum eq 2) then adjust = 2 ;elements are x=var, y=var,z=var(*,*,1) 
          n_dims = n_elements(ilist) - adjust
          zelist1 = make_array(n_dims,/long, value=-1) ;
          zelist2 = make_array(n_dims,/long, value=-1) ;
          zelist3 = make_array(n_dims,/long, value=-1) ;
          zelist4 = make_array(n_dims,/long, value=-1) ;

          dvary = make_array(n_elements(d),/long, value=0) ; these need to multi-dimensional for 3-d data
                                                  ; and are 0 for no and 1 for yes
          lptrn = lonarr(n_elements(d)) ; these need to multi-dimensional for 3-d data
        endif
        ; switch the logic, since we can't make assumptions about the other dimensions
        for dims = 0, n_elements(d)-1 do begin
         if (d[dims] eq '*') then begin 
          dvary[dims]=1
          if (dims eq 0) then zelist1[Z_index] = -2 ;-2 will mean the dimension varies
          if (dims eq 1) then zelist2[Z_index] = -2
          if (dims eq 2) then zelist3[Z_index] = -2
          if (dims eq 3) then zelist4[Z_index] = -2
	  ; RCJ 04/15/2013 added line below to update npanels:
	  npanels=npanels+1
         endif 
         if (d[dims] ne '*') then begin 
          lptrn[dims]=dims+1
          if (dims eq 0) then zelist1[Z_index] = long(d[dims])
          if (dims eq 1) then zelist2[Z_index] = long(d[dims])
          if (dims eq 2) then zelist3[Z_index] = long(d[dims])
          if (dims eq 3) then zelist4[Z_index] = long(d[dims])
         endif
        endfor
     endif 
      Z_index = Z_index+1
   endif
;TJK end new section greater than 2-d data

 endif

endfor

endif
;TJK 2/14/2002 regardless of what's found in the display_type, we'll 
;initialize the structure and return it.

; initialize the structure which is output from this function
out = {x:xname,y:yname,z:zname,npanels:npanels,$
       dvary:dvary,elist:elist,zelist1:zelist1,zelist2:zelist2,zelist3:zelist3,zelist4:zelist4,lptrn:lptrn,igram:igram}
return,out
end







