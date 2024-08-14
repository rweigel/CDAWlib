;$Author: johnson $
;$Date: 2019/02/07 17:44:15 $
;$Header: /home/cdaweb/dev/control/RCS/spdf_read_data.pro,v 1.2 2019/02/07 17:44:15 johnson Exp johnson $
;$Locker: johnson $
;$Revision: 1.2 $
;Purpose: spdf_read_data, depending on the type of data files being
;read for a given data set, will call read_myCDF for cdfs and
;read_mynetcdf for netcdfs.
;It will take the same set of arguments and keywords as
;read_mycdf/netcdf and will return the same structure of the form:
;
;       structure_name.variable_name.attribute_name.attribute_value
;
; From this structure, all data and metadata for the requested variables
; is easily accessed.
;
; Three additional 'attributes' will be included in the sub-structure for 
; each variable.  The first is the 'VARNAME' field.  Because IDL structure
; tags are always uppercase, and because CDF variable names are case sen-
; sitive, a case sensitive copy of the variable name is created.  The second
; 'attribute' to be added is the 'CDFTYPE' field.  This field will hold a
; string value holding the cdf data type.  The last 'attribute' to be
; artificially added will be either the 'DAT' field or, if the keyword
; NODATASTRUCT is set, the 'HANDLE' field.  The 'DAT' field will contain
; the actual data values read from the CDF's for the variable.  The 'HANDLE'
; field will hold a handle_id where the data will reside.
;
; INPUTS:
;       vnames = string, array of variable names or a single string of
;                names separated by a comma.  (ex. 'Epoch,Magfld,Bmax')
;       fnames = string, array of CDF or Netcdf filenames or a single string of
;                names separated by a comma.
; KEYWORD PARAMETERS:
;	ALL = 0: get data and metadata for requested variable(s) only.
;             1: get data and metadata for ALL variables in the CDFs.
;             2: get data and metadata for all var_type='data' variables.
;       NODATASTRUCT = If set, instead of returning the data for each variable
;                   in the 'DAT' attribute field, create a 'HANDLE' field
;                   and set it to the handle id of a data handle which
;                   holds the data for each variable.
;       NOQUIET = If set, do NOT set the !QUIET system variable before
;                 reading the cdf file(s).
;       DEBUG = If set, print out some progress information during reading.
;	TSTART = epoch starting value - YYYYMMDD etc. string.
;	TSTOP = epoch ending value - YYYYMMDD etc. string.
; OUTPUTS:
;       out = anonymous structure holding all data and metadata for the
;             requested variables (structure described above).
;
;             If an error occurs, that we know how
;             to deal w/, an alternate structure is returned, its structure
;	      is as follows: ('DATASET',d_set,'ERROR',v_err,'STATUS',v_stat)

Function spdf_read_data, vnames, fnames, ALL=ALL,NODATASTRUCT=NODATASTRUCT, $
NOQUIET=NOQUIET,DEBUG=DEBUG, TSTART=TSTART, TSTOP=TSTOP, $
START_MSEC=START_MSEC, STOP_MSEC=STOP_MSEC, START_USEC=START_USEC, $ 
STOP_USEC=STOP_USEC, START_NSEC=START_NSEC, STOP_NSEC=STOP_NSEC, $
START_PSEC=START_PSEC, STOP_PSEC=STOP_PSEC, NOVIRTUAL=NOVIRTUAL

compile_opt idl2
if (!version.release ge '8.0') then CDF_SET_VALIDATE, /no  ;turn off CDF validation

; establish exception handler to trap errors from all sources.

CATCH,error_status
if (error_status ne 0) then begin
   print,!ERR_string ," Trapped in spdf_read_data."; output description of error
   print,'Error Index=',error_status
   ;also need to check for -123 for IDL 5.02, -98 is for IDL 4.01b - TJK 1/23/98
   ;added check for -134 out of memory in IDL5.3
   ; added check for the string "unable to allocate memory", since IDL seems
   ; to change the error number associated w/ this w/ each release
   if((strpos(!ERR_string, "Unable to allocate memory") gt -1) or error_status eq -98 or error_status eq -123 or error_status eq -124 or error_status eq -134) then begin
      val_err="ERROR=Memory Exceeded; -98 or -123 or -124 or -134 or -151"
      val_stat="STATUS=Time range selected generates array which exceeds available system resources. Re-select a smaller time range."
      ;
      if(n_elements(mydata) ne 0) then begin
         atags=tag_names(mydata.(0))
         b0 = tagindex('LOGICAL_SOURCE',atags)
         b1 = tagindex('LOGICAL_FILE_ID',atags)
         b2 = tagindex('Logical_file_id',atags)
         if (b0[0] ne -1) then  psrce = strupcase(mydata.(0).LOGICAL_SOURCE)
         if (b1[0] ne -1) then $
            psrce = strupcase(strmid(mydata.(0).LOGICAL_FILE_ID,0,9))
         if (b2[0] ne -1) then $
            psrce = strupcase(strmid(mydata.(0).Logical_file_id,0,9))
         v_data='DATASET='+psrce
      endif else begin
         parts=str_sep(fnames[cx],'/')
         piece=strupcase(str_sep(parts[n_elements(parts)-1],'_'))
         tempnm= piece[0]+'_'+piece[1]+'_'+piece[2]
         val_data="DATASET="+tempnm
      endelse
      tmpstr=create_struct('DATASET',val_data,'ERROR',val_err,'STATUS',val_stat)
      return, tmpstr
   endif
endif
if keyword_set(DEBUG) then debug = 1 else debug=0

;Initialize flags
netcdf_files = 0
cdf_files = 0

num_files = n_elements(fnames)
if keyword_set(DEBUG) then print,'Number of Files to read=',num_files

quiet_flag = !quiet ; save current state of quiet flag
if not keyword_set(NOQUIET) then !quiet=1 ; turn off annoying cdf messages

if (num_files gt 0) then begin

  file_types = strpos(strupcase(fnames), '.CDF', /reverse_search)
  ;if not all file_types are gt 0 (cdf), then assume netcdfs
  found = where(file_types gt 0, fcnt)
  if fcnt eq num_files then cdf_files = 1 ;all cdfs (master and data)
  if fcnt ne num_files then netcdf_files = 1 ;master is cdf and others a netcdfs
 
  ; Validate fnames parameter, remove .cdf extensions if present
  s = size(fnames) & ns = n_elements(s)
  ; find the cdfs
  if (cdf_files) then begin
    if (s[ns-2] eq 7) then begin
      if (s[0] eq 0) then fnames = break_mySTRING(fnames,DELIMITER=',')
      for i=0,n_elements(fnames)-1 do begin
         j=strpos(fnames[i],'.cdf') & if (j eq -1) then j=strpos(fnames[i],'.CDF')
         if (j ne -1) then fnames[i] = strmid(fnames[i],0,j)
      endfor
     endif
  endif

  if (netcdf_files) then  begin ;netcdfs
    if (s[ns-2] eq 7) then begin
      if (s[0] eq 0) then fnames = break_mySTRING(fnames,DELIMITER=',')
      for i=0,n_elements(fnames)-1 do begin
         j=strpos(fnames[i],'.nc') & if (j eq -1) then j=strpos(fnames[i],'.NC')
      ;TJK don't remove the file extensions   if (j ne -1) then fnames[i] = strmid(fnames[i],0,j)
      endfor
    endif
  endif

endif else begin
   print,'ERROR=No CDFs or NetCdfs specified.', fnames
   return,-1
endelse

if (cdf_files) then begin ;passing everything through to read_myCDF
 if keyword_set(DEBUG) then print,'Calling read_myCDF, ',fnames

 buffer = read_myCDF(vnames, fnames, ALL=ALL,NODATASTRUCT=NODATASTRUCT, $
 NOQUIET=NOQUIET,DEBUG=DEBUG, TSTART=TSTART, TSTOP=TSTOP, $
 START_MSEC=START_MSEC, STOP_MSEC=STOP_MSEC, START_USEC=START_USEC, $ 
 STOP_USEC=STOP_USEC, START_NSEC=START_NSEC, STOP_NSEC=STOP_NSEC, $
 START_PSEC=START_PSEC, STOP_PSEC=STOP_PSEC, NOVIRTUAL=NOVIRTUAL)

endif 

if (netcdf_files) then begin  ; passing everything through to read_mynetcdf
 if keyword_set(DEBUG) then print,'Calling read_mynetcdf, ',fnames
 
 buffer = read_mynetcdf(vnames, fnames, ALL=ALL,NODATASTRUCT=NODATASTRUCT, $
 NOQUIET=NOQUIET,DEBUG=DEBUG, TSTART=TSTART, TSTOP=TSTOP, $
 START_MSEC=START_MSEC, STOP_MSEC=STOP_MSEC, START_USEC=START_USEC, $ 
 STOP_USEC=STOP_USEC, START_NSEC=START_NSEC, STOP_NSEC=STOP_NSEC, $
 START_PSEC=START_PSEC, STOP_PSEC=STOP_PSEC, NOVIRTUAL=NOVIRTUAL)

endif

return, buffer
end

