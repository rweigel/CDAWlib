;Notes - master cdfs are cdfs NOT netcdfs - they are CDFS.  
;
;$Author: tkovalic $
;$Date: 2022/10/24 19:15:53 $
;$Header: /home/cdaweb/dev/control/RCS/read_mynetcdf.pro,v 1.23 2022/10/24 19:15:53 tkovalic Exp $
;$Locker:  $
;$Revision: 1.23 $
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;+------------------------------------------------------------------------
; This package of IDL functions facilitates reading data and metadata from
; Net Common Data Format (NCDF) files.  
; While NCDF provides all the benefits
; of a portable, self-documenting scientific data format, reading them is
; not always a simple matter.  To make it simple, I have created this IDL
; package so that all of the data and metadata from multiple variables can 
; be read from multiple NCDF files ... in one single, simple command.  The 
; function is called 'READ_MYNETCDF' and it returns an anonymous structure of
; the form:
;
;       structure_name.variable_name.attribute_name.attribute_value
;
; From this structure, all data and metadata for the requested variables
; is easily accessed.
;
; AUTHOR:
;       Tami Kovalick, NASA/GSFC/Code 672.0, Spring 2017
; 
; NOTES:
;
; For starters, this is a copy of read_myCDF with all cdf read
; routines replaced by netcdf read routines.  Hopefully this will get 
; us most of the way there to having a good reader that produces the desired
; output structure.  Generic routines already in read_myCDF were
; removed from read_myNETCDF such as break_mystring, ami_istpptr,
; follow_mydepends, etc.
;
; Three additional 'attributes' will be included in the sub-structure for 
; each variable.  The first is the 'VARNAME' field.  Because IDL structure
; tags are always uppercase, and because NCDF variable names are case sen-
; sitive, a case sensitive copy of the variable name is created.  The second
; 'attribute' to be added is the 'NCDFTYPE' field.  This field will hold a
; string value holding the ncdf data type.  The last 'attribute' to be
; artificially added will be either the 'DAT' field or, if the keyword
; NODATASTRUCT is set, the 'HANDLE' field.  The 'DAT' field will contain
; the actual data values read from the NCDF's for the variable.  The 'HANDLE'
; field will hold a handle_id where the data will reside.
;
; This package will look for and utilize certain special attributes required
; by the International Solar Terrestrial Physics Key Parameters Generation
; Software Standards and Guidelines.  The existance of these attributes is
; not required for the operation of this software, but will enhance its
; usefullness, primarily by reading variables that will be needed for proper
; utilization of the data, even though you may not have asked for them 
; explicitly.
;
; This package was tested under IDL version 8.5.  This package was tested
; on NCDF's up to version 4 and master cdfs of versionm 3.6.4.
;
; NCDF variables defined as unsigned integers are, unfortunately, currently
; returned by the IDL NCDF_VARGET procedure as signed integers.  This can
; cause sign flips.  This software detects and corrects for this defect for
; data values.  However, it cannot detect and correct for this defect for
; attribute values because the IDL procedure NCDF_ATTINQ does not return the
; NCDF data type of the attribute.  These problems have been reported to
; RSI.
;
;
; Modifications: 
;
; 1996, NASA/Goddard Space Flight Center
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-------------------------------------------------------------------------
;Function ncdf_max_recsize (NCDFid)
;Call: size = ncdf_max_recsize(NCDFid)
;Purpose: Determine the maximum number of records in the file
;Requires NCDFid
;Returns max number of records
;TJK 9/26/2016
function ncdf_max_recsize, NCDFid

cinfo = ncdf_inquire(NCDFid)
help, cinfo
; Determining the max number of records in the file.
recs = -1L ;initialize  - not sure what this should be for netcdf
if (cinfo.ndims gt 0) then begin
  ncdf_diminq, NCDFid, cinfo.recdim[0], ulimited_recvarname, unlimited_recvarsize
  recs = unlimited_recvarsize ; max number of records
endif
return, recs
end
;---------------------------------------------------------------------------
;Function ncdf_var_recvary(NCDFid,varname)
;Purpose: find out whether the variable is record varying (the native
;netcdf routines do not return this information).
;Requires: NCDFid - integer id of already opened netcdf file
;          varmame - variable name 
;Returns: 'VARY' = true - variable does vary by the record (time)
;         'NOVARY' = false - variable does not vary by the record
;Call: vary = ncdf_var_recvary(NCDFid,varname)
;Author: Tami Kovalick, 9/26/2016
;
function ncdf_var_recvary,NCDFid,varname
vary = 'NOVARY' ;initialize to false
if (NCDFid gt 0) then begin
;   fileinfo = ncdf_inquire(NCDFid)
;   if (fileinfo.ndims gt 0) then begin ;fileinfo.recdim is the unlimited dimension
;The above 2 lines works for a netcdf with just one unlimited
;variable, replaced it with the following to work with more.
;   unlimited_ids = ncdf_unlimdimsinq(NCDFid, count=ucount)
;8/24/2018 - TJK change call to use call_function so that it this will
;            compile with older versions of IDL (Bernie needs 7.1)
   unlimited_ids = call_function('ncdf_unlimdimsinq',NCDFid, count=ucount)
   varinfo = ncdf_varinq(NCDFid, varname)
   if (varinfo.ndims gt 0 and ucount gt 0) then begin
      print, 'DEBUG varname= ',varname, varinfo.ndims, ' unlimited ids= ', unlimited_ids, 'var dim ids= ',varinfo.dim
      ;if fileinfo.recdim is one of the varinfo.dim's then this variable vary's by record
      for j=0,varinfo.ndims-1 do begin
        found = where(unlimited_ids eq varinfo.dim[j], nfound)
        if (nfound gt 0) then vary = 'VARY'
      endfor
   endif
endif
print, 'Variable ',varname,' does vary by record'
return, vary
end

;-------------------------------------------------------------------------
; NAME: NCDF_getvar_size, NCDFid, vinfo, start=start, count=count
; PURPOSE: 
;	based on the vinfo structure, initialize the starts recs and
;	count arrays which can then be used to read a variable's values
; CALLING SEQUENCE:
;       ncdf_getvar_size, NCDFid, vinfo, recs=recs, count=count
; INPUTS:
;       NCDFid value = id for the currently opened netcdf file.
;       vinfo structure (returned from a ncdf_varinq call)
; KEYWORD PARAMETERS:
;       start = this procedure will initialize the start array based on
;       the dimensions of the variable.
;       count = this procedure will initialize the count array  based on
;       the dimensions and size of the dimensions of the variable (to
;       read all records and all dimension).
; OUTPUTS:
;       out = integer, IDL type number
; AUTHOR:
; 
; MODIFICATION HISTORY:
; 
;-------------------------------------------------------------------------
pro ncdf_getvar_size, NCDFid, vinfo, start=start, count=count
start = make_array(vinfo.ndims, /long, value=0) ;0 based array
;set up the dimension sizes for the varget call
count = make_array(vinfo.ndims, /long) ; initialize
for i = 0, vinfo.ndims-1 do begin 
  ncdf_diminq, ncdfid, vinfo.dim[i],name,size 
  count[i]=size
endfor
end

;-------------------------------------------------------------------------
; NAME: NCDFTYPE_T0_MYIDLTYPE
; PURPOSE: 
;	Convert from NCDF type number to IDL type number
; CALLING SEQUENCE:
;       out = NCDFtype_to_myIDLtype(in)
; INPUTS:
;       in = integer, NCDF type number
; KEYWORD PARAMETERS:
; OUTPUTS:
;       out = integer, IDL type number
; AUTHOR:
; 
; MODIFICATION HISTORY:
; 
;-------------------------------------------------------------------------
FUNCTION NCDFtype_to_myIDLtype,ncdftype

case ncdftype of
   22L : idltype = 5 ; NCDF_REAL8
   45L : idltype = 5 ; NCDF_DOUBLE
   31L : idltype = 5 ; NCDF_EPOCH
   32L : idltype = 9 ; NCDF_EPOCH16
   21L : idltype = 4 ; NCDF_REAL4
   44L : idltype = 4 ; NCDF_FLOAT
   4L  : idltype = 3 ; NCDF_INT4
   14L : idltype = 3 ; NCDF_UINT4
   2L  : idltype = 2 ; NCDF_INT2
   12L : idltype = 2 ; NCDF_UINT2
   51L : idltype = 7 ; NCDF_CHAR
   52L : idltype = 1 ; NCDF_UCHAR
   1L  : idltype = 1 ; NCDF_INT1
   11L : idltype = 1 ; NCDF_UINT1
   41L : idltype = 1 ; NCDF_BYTE
   else: idltype = 0 ; undefined
endcase
return,idltype
end

;
;Name: ncdf_attexists, Cdfid, Attrname, Varname=varname
;Call: exists = ncdf_attexists(NCDFid, Attrname, Varname=varname)
;Purpose: Determine if a given attribute exists for the given variable or for
;         the file if the variable name is not provided.
;Requires NCDFid, Attrname amd Varmame if looking for a variable attribute
;Returns 0 (false) if attrname doesn't exist, 1 otherwise
;TJK 9/20/2016
;
function ncdf_attexists, NCDFid, Attrname, Varname=varname

File_info = NCDF_INQUIRE(NCDFid)
status = 0 ;false

if (not keyword_set(varname)) then begin ; look for global attribute
  if (file_info.ngatts gt 0 ) then begin
      for i = 0, file_info.ngatts-1 do begin
         attname = ncdf_attname(NCDFid, i, /GLOBAL)
         if (attname eq Attrname) then return, 1
      endfor
   endif
endif else begin ; look for variable attribute
   var_info = ncdf_varinq(NCDFid, varname)
   if (var_info.natts gt 0) then begin
      for i = 0, var_info.natts-1 do begin
         attname = ncdf_attname(NCDFid, varname, i)
         if (attname eq Attrname) then return, 1
      endfor
   endif
endelse
return, status
end

;
;+------------------------------------------------------------------------
; NAME: READ_MYVARIABLE_netcdf
; PURPOSE: 
;	Return the data for the requested variable.
; CALLING SEQUENCE:
;       out = read_myvariable_netcdf(vname, NCDFid, vary, dtype, recs)
; INPUTS:
;       vname = string, name of variable to be read from the NCDF
;       NCDFid = integer, id or already opened NCDF file.
; KEYWORD PARAMETERS:
;	START_REC = first record to read.
;	REC_COUNT = number of records to read.
; OUTPUTS:
;       out = all data from the NCDF for the variable being read
;       vary = True(1) or False(0) is variable record-varying
;       dtype= string, NCDF data type
;       recs = integer, number of data records
; AUTHOR:
;       Tami Kovalick, NASA/GSFC/Code 672
;
; MODIFICATION HISTORY:
;
;-------------------------------------------------------------------------
FUNCTION read_myvariable_netcdf, vname, NCDFid, vary, $
	 dtype, recs, START_REC=START_REC, REC_COUNT=REC_COUNT,set_column_major=set_column_major,DEBUG=DEBUG

;
; Get needed information about the ncdf and variable

;not available for ncdf NCDF_LIB_INFO, VERSION=V, RELEASE=R, COPYRIGHT=C, INCREMENT=I
;not available ncdf ncdfversion = string(V, R, I, FORMAT='(I0,".",I0,".",I0,A)')

cinfo = ncdf_inquire(NCDFid) ; inquire about the ncdf
vinfo = ncdf_varinq(NCDFid,vname) ; inquire about the variable
;ncdf_control,NCDFid,VAR=vname,GET_VAR_INFO=vinfo2 ; inquire more about the var
maxrecs = ncdf_max_recsize(NCDFid) ;for netcdfs
vary = ncdf_var_recvary(NCDFid,vname) ;for netcdfs
;print, 'DEBUG in read_myvariable_netcdf, vary being set vary = ',vary
;stop;
dtype = vinfo.DATATYPE

;no z vars in netcdfzflag = vinfo.is_zvar ; determine if r-variable or z-variable
if keyword_set(START_REC) then start_rec = START_REC else start_rec = 0L
rcount = maxrecs & if (vary eq 'NOVARY') then rcount=1L ;for netcdf

if keyword_set(REC_COUNT) then recs = REC_COUNT else recs = maxrecs

;The following to_column logic is never going to be true for netcdfs,
;so commented out
;if keyword_set(DEBUG) then begin
;  if keyword_set(set_column_major) then print, 'In read_myvariable_netcdf, to_column SET in varget' else print, 'In read_myvariable_;netcdf, to_column NOT set in varget' 
;endif

; Read the NCDF for the data for the requested variable
;not for netcdf if (zflag eq 1) then begin ; read the z-variable - N/A for netcdf
;   ncdf_control,NCDFid,VAR=vname,GET_VAR_INFO=zvinfo

   ; Add the condition that the variable is Record Variant before entering this branch.  According
   ; to the documentation, NRV variables should return a MAXREC = 0, however, at least in some cases,
   ; the structure returned in GET_VAR_INFOR when calling NCDF_CONTROL on a NRV variable has MAXREC
   ; set to -1.
   ; Ron Yurow (October 6, 2015)
;   if zvinfo.MAXREC eq -1  && vary eq "VARY" then begin  ; this means NO records have been written

   if maxrecs eq -1  && vary eq "VARY" then begin  ; this means NO records have been written
      if keyword_set(DEBUG) then print,'WARNING=',vname,' has ZERO records!'
;TJK 11/17/2006 - instead of reading 1 record when maxrec = -1 (which
;                 indicates that no records for the variable were
;                 written), return the fill value for this variable
;print, 'WARNING, no records for variable ',vname
;print, 'attempting to get fillval and return that'

      if (ncdf_attexists(NCDFid, 'FILLVAL', Varname=vname))then begin
         ncdf_attget,NCDFid,vname,'FILLVAL',wfill
	 if (size(wfill,/tname) eq 'DCOMPLEX') then return,real_part(wfill) else $
         ;TJK 5/4/2007 - change from "return,wfill" to check if size should be
         ; an array, if so the make the appropriate size array and fill w/ the variables fill value.
         ; dim is array of longs. The value of each element corresponds to the dimension of the variable.
	 ; This field is only included in the structure if the variable is a zVariable. (source: ncdf_varinq in idl help)
         ; So this 'case' refers to the number of elements of dim.
;
;***Left off here - look up vinfo.dim for cdf and netcdf - make sure
;                   they are the same thing
        case (size(vinfo.dim, /n_elements)) of
             0: return, wfill
             1: begin
                 ;if (vinfo.dim le 0) then return, wfill $
                 ;else return, make_array(vinfo.dim,value=wfill)
                 if (vinfo.dim le 0) then begin
		    return, wfill 
		 endif else begin 
		   ; RCJ 02/07/2012 
		   ; if var and its depend_0 have no records (vinfo2.maxrecs=0) OR
		   ; if var has no records but its depend_0 does, then we need
		   ; to fill in the var array w/ as many records as depend_0 has. 
		   ; Ran into this problem w/ thb_l2_mom_20090929_v01.ncdf 
                   ;if vinfo2.maxrecs eq 0 then begin
		   ; RCJ 03/23/2012   maxrecs could be -1 too, so changed to 'le'
                   ;TJK 4/11/2012, use maxrecs+1 instead of maxrecs
                   if (vinfo2.maxrecs le 0) then begin
		      return, make_array(vinfo.dim,value=wfill)
		   endif else begin 
 
;1/30/2014 TJK check the depend0 variable to see if its virtual, if so
;it will have a component_1 (THEMIS case) so the epoch values don't 
;exist yet... so need to get the depend_0's, component_1's data size 
;and compare w/ the current variables size - stored below in cinfo.maxrec
;3/18/2014 - call ncdf_attexists for depend_0 and component_0 instead
;of ncdf_attnum (this routine doesn't exist for ncdf - TJK 8/23/2018)

                      dnum = ncdf_attexists(NCDFid,'DEPEND_0',varname=vname) ;returns true or false
                      if ((dnum) and ncdf_attexists(NCDFid,'DEPEND_0',varname=vname))then begin
                         ncdf_attget,NCDFid,vname,'DEPEND_0', depend0 ;depend_0 of the data variable
                         if (depend0 ne ' ') then begin ;if depend_0 isn't blank get its component_0
                            cnum = ncdf_attexists(NCDFid,'COMPONENT_1',varname=depend0) ;returns true or false
                            if ((cnum) and ncdf_attexists(NCDFid,'COMPONENT_1',varname=depend0))then begin
                               ncdf_attget,NCDFid,depend0,'COMPONENT_1',component1 ;component_1 of the data variable
                               if (component1 ne ' ') then begin ; now get the data array sizes for the component_1 variable
                                  ncdf_control,NCDFid,VAR=component1,GET_VAR_INFO=cinfo
                                  if (cinfo.maxrec+1 gt 0) then make_records = cinfo.maxrec+1 else make_records = 1
                                  if keyword_set(DEBUG) then print,'WARNING, ',vname,' has no records but its component_1 ',component1,' does. Filling in array with ',make_records,' elements.'  
                                  return, make_array(vinfo.dim,make_records,value=wfill)
;                                 help, /struct, cinfo
                               endif 
                             endif else begin ; if component_1 isn't found, then use the maxrec for this variable
                                ;if (vinfo2.maxrec+1 gt 0) then make_records = vinfo2.maxrec+1 else make_records = 1
				; RCJ 10/09/2014  Recs seems to have the correct number of records to be read. 
                                ; if (recs gt 0) then make_records = recs else make_records = 1
                                ; It looks what is needed is maximum records of the depend_0 variable.  This is 
                                ; done in the following two lines.
                                ; Ron Yurow (Feb 12, 2016)
                                ncdf_control,NCDFid,VAR=depend0,GET_VAR_INFO=dinfo
                                if (dinfo.maxrec+1 gt 0) then make_records = dinfo.maxrec+1 else make_records = 1
                                if keyword_set(DEBUG) then print,'WARNING, ',vname,' has no records but its depend_0 ',depend0,' does. Filling in array with ',make_records,' elements.'  
                                return, make_array(vinfo.dim,make_records,value=wfill)
                             endelse
                         endif
                      endif else begin ; if depend_0 isn't found, then use the maxrec for this variable
                        ;if (vinfo2.maxrec+1 gt 0) then make_records = vinfo2.maxrec+1 else make_records = 1
			; RCJ 10/09/2014 Recs seems to have the correct number of records to be read.
                        if (recs gt 0) then make_records = recs else make_records = 1
                        if keyword_set(DEBUG) then print,'WARNING, ',vname,' has no records and no depend_0. Filling in array with ',make_records,' elements.'  
                        return, make_array(vinfo.dim,make_records,value=wfill)
                      endelse

                      ;1/30/2014 TJK vinfo2.maxrecs is the maximum record for all variables
                                ;in this ncdf.  So we don't want
                                ;to use that, see above for new logic
;		      if keyword_set(DEBUG) then print,'WARNING, ',vname,' has no records but its depend_0 does. Filling in array with ',vinfo2.maxrecs+1,' elements.'  
;		      return, make_array(vinfo.dim,vinfo2.maxrecs+1,value=wfill)

		   endelse  
		 endelse   
                end
             2: begin
                  tmp_a = make_array(dimension=vinfo.dim, value=wfill)
                  tmp_a = reform(temporary(tmp_a),vinfo.dim[0],vinfo.dim[1],1)
                  return, tmp_a
                end
             3: begin
                  tmp_a = make_array(dimension=vinfo.dim, value=wfill)
                  tmp_a = reform(temporary(tmp_a),vinfo.dim[0],vinfo.dim[1],vinfo.dim[2],1)
                  return, tmp_a
                end
             else: print, "STATUS = array size too large"

         endcase
  
      endif else begin ;if don't have a fill value, go ahead and read 1 rec.
;        if keyword_set(DEBUG) then vtime = systime(1)
;           cdf_varget,NCDFid,vname,dat,REC_COUNT=1 & return,dat
           ncdf_varget,NCDFid,vname,dat,offset=0,count=1 & return,dat
;        if keyword_set(DEBUG) then print, '1 Took ',systime(1)-vtime, ' seconds to do ncdf_varget for ',vname
      endelse
   
   endif else begin

;     if keyword_set(DEBUG) then vtime = systime(1)
;     cdf_varget,NCDFid,vname,dat,REC_START=start_rec,REC_COUNT=recs
;help, vinfo
;print, 'DEBUG reading ',vname, ' for offset ',start_rec,' recs ',recs
;print, 'DEBUG dimension ids ', vinfo.dim, '# dims ',vinfo.ndims
      ncdf_getvar_size, NCDFid, vinfo, start=startr, count=countr

      ; set up offset and count values based on the dimension sizes for each variable
      ; and then set the start record number based on the epoch we're looking for
      ; the last dimension is the record/time dimension so set the
      ; starting record and how many records to read (if we don't need all records in the file
      startr[vinfo.ndims-1] = start_rec
      countr[vinfo.ndims-1] = recs
      ;print, 'debug: start array',startr, ' number of records array',countr
      ncdf_varget,NCDFid,vname,dat,offset=startr,count=countr

;     if keyword_set(DEBUG) then print, '2 Took ',systime(1)-vtime, ' seconds to do ncdf_varget for ',vname

   endelse
   ;TJK - added the next two lines so that extraneous single dimensions
   ;will be taken out - this was already being done for r variables
   ;but wasn't for Z variables, so if we were loading a variable from
   ;both a z and r variable ncdf there would be a mismatch and the
   ;append of the two data arrays would not be successful.
   ds = size(dat) ; get size info to determine if dat is scalar or not
   if (ds[0] ne 0) then dat = reform(temporary(dat)) ; eliminate extraneous dims
;endif zflag



; Correct for fact that IDL retrieves character data as bytes

;if ((vinfo.DATATYPE eq 'NCDF_CHAR') or (vinfo.DATATYPE eq 'NCDF_UCHAR')) then begin ; IDL retrieves as bytes
if ((vinfo.DATATYPE eq 'CHAR') or (vinfo.DATATYPE eq 'UCHAR')) then begin ; IDL retrieves as bytes
   ds = size(dat) ; get dimensions of dat for single char special case
;   if (ds[0] gt 1) then begin 
   if (ds[0] ge 1) then begin ; 9/24/2021 change gt to ge, converting strings works for 1 element as well as more
      dat = string(dat)
      dat = reform(temporary(dat)); eliminate extraneous dims
   endif else begin 
     if ((ds[0] eq 1) and vinfo.dim eq 1) then begin 
         print, 'DEBUG converting one record of bytes '
        dat = string(dat)
     endif else begin  ; process each element of array
         print, 'DEBUG converting more than one record of bytes '
       d2=strarr(ds[1])
       for i=0,ds[1]-1 do d2[i]=string(dat[i])
       dat = d2
     endelse
  endelse

if keyword_set(DEBUG) then print, '*** Converted ',vinfo.datatype,' to STRING for variable ',vinfo.name
;check if this is a UTC time (GOLD netcdf files), if so convert it to TT2000 values for
;our use.

  if (ncdf_attexists(NCDFid, 'FIELDNAM', varname=vinfo.name)) then begin ;TJK add call to new routine
      ncdf_attget,NCDFid,vinfo.name,'FIELDNAM',fieldname
      if (ncdf_attexists(NCDFid, 'Mission_group')) then ncdf_attget,NCDFid,/global, 'Mission_group',mission_group

      ;for some GOLD data sets, we're given a time for each pixel, for CDAWeb we
      ;just need one value so use the minimum
      ;time for each record.  Have to exclude/ignore the '***' fill values.
      newdat = lon64arr(recs)

      if ((fieldname eq 'Universal Time' or fieldname eq 'Scan Start Time') and mission_group eq 'GOLD') then begin
;print, 'found a GOLD universal time variable'

         for t = 0, recs-1 do begin
            case (size(dat,/n_dimensions)) of
               0: temp = dat
               1: temp = dat[t]
               2: temp = dat[*,t]
               3: temp = dat[*,*,t] ; each record (where there are times for every pixel
            endcase
            ;eliminate the fill "*" values by 1st finding all values that have a "2" in
            ;them (like from the year 2018).
            good = where(strpos(temp, '2') eq 0, ngood) 

           ;determine the minimum start time for this record if there are many
           ;(GOLD images)

            if ngood gt 0 then newdat[t] = min(CDF_PARSE_TT2000(temp[good])) $ ;minimum value for this record
                          else newdat[t] = CDF_PARSE_TT2000('9999-12-31T23:59:59.999999999') ;fill for tt2000
            ;print, 'DEBUG, min time for record ',t,' is ',cdf_encode_tt2000(newdat[t])
         endfor

         temp = 0               ; clear out
         dat = newdat
         dtype='CDF_TIME_TT2000' ; set this here since we've changed the values from char to long64(tt2000)
      endif

   endif
endif

; Check for sign loss for ncdf unsigned integer data.  IDL (as of v4.0.1b)
; returns unsigned ncdf variables as signed IDL variables with the same
; number of bytes.  This could cause a sign flip.  Detect and Correct.
if (vinfo.DATATYPE eq 'CDF_UINT1' and strupcase(vary) eq 'NOVARY') then begin
; RCJ 03/09/2016  Doing this for novary vars only at the moment.  There are 'vary' types
; that are of type byte but they list and plot properly, so I don't want to mess with them.
; The problem here was that novary vars that go on the header of a listing were not showing (mms fpi data,
; that are supposed to show as an index array). In LIST_mystruct, these byte arrays are turned to strings
; and become ''.
      dat = uint(dat)  & dtype='CDF_INT1'
      print,'WARNING=Converting BYTE to CDF_INT1.'
endif
if vinfo.DATATYPE eq 'CDF_UINT2' then begin
   w = where(dat lt 0,wc) ; search for negative values
   if (wc gt 0) then begin ; convert to long
      dat = long(dat) & dat[w] = dat[w]+(2L^16) & dtype='CDF_INT4'
      print,'WARNING=Converting CDF_UINT2 to CDF_INT4 to avoid sign switch.'
   endif
endif
if vinfo.DATATYPE eq 'CDF_UINT4' then begin
   w = where(dat lt 0,wc) ; search for negative values
   if (wc gt 0) then begin ; convert to float
      dat = float(dat) & dat[w] = dat[w]+(2.0d0^32) & dtype='CDF_REAL'
      print,'WARNING=Converting CDF_UINT4 to CDF_REAL4 to avoid sign switch.'
   endif
endif

; If this variable is a record-varying variable, but this NCDF only happens
; to have one record, then we must add the extra dimension onto the end
; for proper appending to take place when other NCDF's are read
;if ((vinfo.RECVAR eq 'VARY')AND(rcount eq 1L)) then begin
if ((vary eq 'VARY')AND(rcount eq 1L)) then begin
   ; print,'WARNING=Reforming single-record variable' ;DEBUG
   ds = size(dat) ; get dimensions of dat
   case ds[0] of
      0    : rcount = 1L ; do nothing
      1    : dat = reform(temporary(dat),ds[1],1)
      2    : dat = reform(temporary(dat),ds[1],ds[2],1)
      3    : dat = reform(temporary(dat),ds[1],ds[2],ds[3],1)
      else : print,'ERROR=Cannot reform single-record variable with > 3 dims'
   endcase
endif
; Return the data read from the NCDF

return,dat
end

function ncdf_majority_check, NCDFid=NCDFid, buf=buf
; Netcdfs are only row major, so nothing to check.
; Return the value of set_column_major = false
; This is here as a stub to match the cdf routine (majority_check)


set_column_major = 0

return, set_column_major

end

;+------------------------------------------------------------------------
; NAME: READ_MYATTRIBUTE_netcdf
; PURPOSE: 
;	Return the value of the requested attribute for the requested variable.
; CALLING SEQUENCE:
;       out = read_myattribute_netcdf(vname,anum,NCDFid,isglobal=isglobal)
; INPUTS:
;       vname = string, name of variable whose attribute is being read
;       anum = integer, number of attribute being read
;       NCDFid = integer, id of already opened NCDF file.
; KEYWORD PARAMETERS:
;       isglobal = true (global attribute requested, otherwise variable)
; OUTPUTS:
;       out = anonymous structure holding both the name of the attribute
;             and the value of the attribute
; AUTHOR:
;       Tami Kovalick, NASA/GSFC/Code 672
;
; MODIFICATION HISTORY:
;-------------------------------------------------------------------------
FUNCTION read_myattribute_netcdf, vname, anum, NCDFid, isglobal=isglobal
common global_table, table

;Initialize some values
attval=''
if keyword_set(isglobal) then begin
   ascope = 'GLOBAL'
endif else begin ;variable scope
   isglobal = 0
   ascope = 'VARIABLE'
endelse

;get the attribute
if (isglobal) then begin
  aname = ncdf_attname(NCDFid, anum, global=isglobal)
  ncdf_attget, NCDFid, global=isglobal, aname, attval
  attinfo = ncdf_attinq(NCDFid, aname, global=isglobal)

endif else begin ;variable attribute
  aname = ncdf_attname(NCDFid, vname, anum)
  ncdf_attget, NCDFid, vname, aname, attval
  attinfo = ncdf_attinq(NCDFid, vname, aname)
endelse 

;cdf_attinq,NCDFid,anum,aname,ascope,maxe,maxze ; inquire about the attribute

aname = strtrim(aname,2) ; trim any possible leading or trailing blanks
;TJK 2/28/2002 - call replace_bad_chars to replace any "illegal" characters in
;the attribute name w/ a legal one.  This was necessary to go to IDL 5.3.

aname = replace_bad_chars(aname,repchar="_",found)
if (attinfo.datatype eq 'CHAR') then attval = string(attval)
;1/20/2021 TJK added test for MOD (found in ICON netcdfs) - illegal to
; use as a tag name
if (strcmp('MOD',strupcase(aname),strlen(aname)))then begin
   ;can't use it as a tag name in a structure, so ignore
   print, 'DEBUG found illegal attribute in file: ',aname,' changing name to ',aname,'_'
   aname = aname + '_'
endif
astruct=create_struct(aname,attval) ; initialize anonymous structure
;TJK modified this error catch to re-set the !error value since not finding
;all attributes is not a fatal error - w/o this SSWeb and the IDL server
;were getting stuck.
CATCH,error_status & if error_status ne 0 then begin !ERROR=0 & return,astruct & endif

;Netcdfs do not have multiple global attribute entries and do not have R and Z variables)
;So just check for Variable scope attributes and if they are "pointer"
;variables, follow the pointer to the value.

if (ascope eq 'VARIABLE') then begin
    if (amI_ISTPptr(aname) eq 1) then begin ; check for pointer-type attribute 
        attval = read_myvariable_netcdf(attval,NCDFid,vary,ctype,recs)

;Netcdf's are only row major, so I don't think we need to do this, go back
;to original call above.
;         if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
;            to_column = ncdf_majority_check(NCDFid=NCDFid)
;         attval = read_myvariable_netcdf(attval,NCDFid,vary,ctype,recs,set_column_major=to_column)
    endif

    asize = size(attval) & nea = n_elements(asize)
      ;TJK, 3/2/2000, restrict the strtrim calls below because some attributes
      ;values are actually variable names which may need to have any leading/trailing
      ;blanks in order to be found in the ncdf...  this is certainly the case for
      ;depend and component variable attributes...

    if ((asize[nea-2] eq 7) and NOT(amI_VAR(aname))) then begin
       if asize[0] eq 0 then attval = strtrim(attval,2) $
       else for i=0,asize[1]-1 do attval[i] = strtrim(attval[i],2)
    endif else begin
       if (amI_VAR(aname)) then begin
       ;replace "bad characters" w/ a "$"
         table_index = where(table.varname eq attval, tcount)
         ttable_index = where(table.equiv eq attval, ttcount)
         vcount = -1 ;initialize
         if (table_index[0] eq -1) and (ttable_index[0] eq -1)then begin ;add this variable to the table
            if keyword_set(debug) then print, 'found new attribute adding to table, ',attval
	    tfree = where(table.varname eq '',fcount)
	    if (fcount gt 0) then begin
	       table.varname[tfree[0]] = attval
	    endif else begin
	       print, '1, Number of variables exceeds the current size ' + $
	           'of the table structure, please increase it, current size is ...' 
	       help, table.varname
	       return, -1
	    endelse
            table_index = where(table.varname eq attval, vcount)
         endif 

         if (vcount ge 0) then begin
      	    attval = replace_bad_chars(attval, diff)
	    table.equiv[table_index[0]] = attval ;set equiv to either the
	    ;new changed name or the original
	    ;if it doesn't contain any bad chars..
         endif else begin
	    if (vcount eq -1) then begin ;already set in the table, assign attval to what's in equiv.
	       attval = table.equiv[table_index[0]]
	    endif
         endelse
      endif
   endelse
   astruct = create_struct(aname,attval)
endif

return,astruct
end ;read_myattribute_netcdf

;+------------------------------------------------------------------------
; NAME: READ_MYMETADATA_netcdf
; PURPOSE: 
;	To read all of the attribute values for the requested variable, and
;       to return this information as an anonymous structure.
; CALLING SEQUENCE:
;       metadata = read_mymetadata_netcdf(vname,NCDFid)
; INPUTS:
;       vname = string, name of variable whose metadata is being read
;       NCDFid = integer, id of already opened NCDF file
; KEYWORD PARAMETERS:
; OUTPUTS:
;       metadata = anonymous structure whose tags are the attribute names
;                  and whose fields are the corresponding attribute values.
; AUTHOR:
;       Tami Kovalick, NASA/GSFC/Code 672
;
; MODIFICATION HISTORY:
;
;-------------------------------------------------------------------------
FUNCTION read_mymetadata_netcdf, vname, NCDFid

;ncdf_inquire returns cinfo.ndims, .nvars, .ngatts (just global attributes), .recdim

cinfo = ncdf_inquire(NCDFid) ; inquire the ncdf to get # of global attributes

vinfo = ncdf_varinq(NCDFid,vname) ; get the # of attributes for this variable (this can vary from var to var)

ntotal_atts = cinfo.ngatts + vinfo.natts

; Create initial data structure to hold all of the metadata information
METADATA = create_struct('varname',vname)
; Extract all metadata information for all global attributes
nglobal=0
for anum=0,cinfo.ngatts-1 do begin
   astruct = 0 ; initialize astruct
   ; Get the name and value of the next attribute for vname
   astruct = read_myattribute_netcdf(vname,anum,NCDFid, /isglobal)
   new_attr = string(tag_names(astruct),/print) ;reform to take the value out of array
   existing_attrs = tag_names(metadata)
   dups = where(new_attr eq existing_attrs, nfound)
   if (nfound eq 0) then begin
     nglobal=[nglobal,1]
     METADATA = create_struct(temporary(METADATA),astruct)
   endif else print, 'read_mymetadata_netcdf: duplicate attribute skipped ', new_attr

endfor ; for each attribute

;add in the variable attribute information

for anum=0,vinfo.natts-1 do begin
   ; Get the name and value of the next attribute for vname
   astruct = read_myattribute_netcdf(vname,anum,NCDFid)
   new_attr = string(tag_names(astruct),/print) ;reform to take the value out of array
   existing_attrs = tag_names(metadata)
   dups = where(new_attr eq existing_attrs, nfound)
   if (nfound eq 0) then begin
     nglobal=[nglobal,0]
     METADATA = create_struct(temporary(METADATA),astruct)
   endif else print, 'read_mymetadata_netcdf: duplicate attribute skipped ', new_attr

endfor ; for each attribute

mtags = tag_names(METADATA)
maj=tagindex('NCDFMAJOR',mtags) 
if (maj[0] eq -1) then begin
;print, 'DEBUG adding the ncdfmajor structure element for ', metadata.varname
;  METADATA= create_struct(temporary(METADATA),'ncdfmajor',cinfo.majority) 
  METADATA= create_struct(temporary(METADATA),'ncdfmajor','ROW_MAJOR') ; netcdf is only row major 
  nglobal=[nglobal,1]
endif


;11/20/2007 TJK after all attributes are read and put in metadata
;structure, add one more attribute that's needed for appending the 
;data arrays from file to file (for THEMIS especially).  Add a 
;dim_sizes element.

;status = ncdf_varnum(NCDFid,vname) ;check to see if this named variable exists
;for netcdf, just set the status from the vinfo structure
if (vinfo.datatype ne '') then status = 1

; RCJ 01/31/2008 Need to check for the existence of dim_sizes
; In ncdfs generated by cdaweb, for example, they already exist in the structure
q=where(tag_names(metadata) eq 'DIM_SIZES')

if (status ne -1) and (q[0] eq -1) then begin
;this isn't a netcdf concept
;  ncdf_control, NCDFid, SET_ZMODE=2 ;set zmode so that we'll always get a .dim (default for R variables
  ;is a value for .dimvar (which is different than .dim)...
;already did this above  vinfo = ncdf_varinq(NCDFid,vname) ; get the dim_size info. on this var.
  METADATA = create_struct(temporary(METADATA),'DIM_SIZES',vinfo.dim)
  nglobal=[nglobal,0] ;add this as a variable attribute [0]
endif

;
; CDAWeb's listing and write_myncdf s/w rely on the fact that 'fieldnam' is 
; the first of the var attrs. Occasionally (see dataset po_k0_hyd) the data ncdf
; is such that 'fieldnam' is not the first of the var attrs. Rewritting the 
; structure and moving 'fieldnam' to the top seems to fix that problem.
; RCJ 11/05/03
;
tnames=tag_names(metadata)

q0=where(tnames eq 'FIELDNAM')
if q0[0] ne -1 then begin
   n0=where(nglobal eq 0, var_cnt) ; variable scope
   n1=where(nglobal eq 1, global_cnt) ; global scope
;print, 'number of global attrs ', global_cnt
;print, 'number of variable attrs ', var_cnt

;   if q0[0] ne n0[1] then begin ; if fieldnam is not the second 'variable scope' var.
             ; we do not compare q0[0] and n0[0] because n0[0] is 'varname's
	     ; position. 'Fieldnam' should be the next one, n0[1]

   reorder = where(n1 gt q0[0], globals_after_vars) ;TJK 1/14/2016 added to further determine when to reorder
   if (q0[0] ne n0[1] or globals_after_vars gt 0) then begin ; if fieldnam is not the second 'variable scope' var.
             ; we do not compare q0[0] and n0[0] because n0[0] is 'varname's
	     ; position. 'Fieldnam' should be the next one, n0[1]
             ;added test to see if there are global attributes AFTER Fieldnam, if so,
             ;need to reorder the attributes

      si=strtrim(n1[0],2)
      comm = "tmpstr=create_struct('varname',vname," ; first global attr
      if (global_cnt gt 0) then begin ;TJK 11/27 check if there are global attributes
        for ii=0,n_elements(n1)-1 do begin  ;do global attr first
           si=strtrim(n1[ii],2)
           ;print,si,' g ',tnames[si]
           comm = comm + "tnames["+si+"],metadata.("+si+"),"
        endfor 
      endif
      si=strtrim(q0[0],2)
      comm=comm + "'FIELDNAM',metadata.("+si+"),"
      for ii=0,n_elements(n0)-2 do begin  ;do variable attr now
         si=strtrim(n0[ii],2)
         ;print,si,' v ',tnames[si]
         if tnames[si] ne 'FIELDNAM' and tnames[si] ne 'VARNAME' then begin
            comm = comm + "tnames["+si+"],metadata.("+si+"),"
         endif 
      endfor
      si=strtrim(n0[n_elements(n0)-1],2)  ; last variable attr
      comm = comm + "tnames["+si+"],metadata.("+si+"))"
      s=execute(comm)
      metadata=tmpstr

   endif
endif
;
return,METADATA
end ; end read_mymetadata_netcdf
;
;Name: ncdf_max_numvar_attributes(NCDF_id)
;Purpose: 
;Determine the maximum number of variable attributes for the file
;(since each variable can have a different number of attributes)
;Keywords: varid - the routine will return the varid of the variable
;                  with the most attributes
;Author: Tami Kovalick 9/22/2016
;
function ncdf_max_numvar_attributes, NCDFid, varid=varid

cinfo = ncdf_inquire(NCDFid) ; inquire about the ncdf to get #attributes
; Create initial data structure to hold all of the metadata information
max_attrs = 0
varid = 0
if (cinfo.nvars gt 0 ) then begin
  for i = 0, cinfo.nvars-1 do begin
    vinfo = ncdf_varinq(NCDFid, i)
    if (vinfo.natts gt max_attrs) then begin
       max_attrs = vinfo.natts
       varid = i
    endif
  endfor
endif
;print, 'DEBUG max attributes for all variables',max_attrs
return, max_attrs
end

;+------------------------------------------------------------------------
; NAME: ncdf_getvar_attribute_names NCDFid
; PURPOSE: 
;	To return all of the attribute names for the requested variable, as
;	an array.
; CALLING SEQUENCE:
;       att_array = ncdf_getvar_attribute_names(vname,NCDFid, ALL=ALL)
; INPUTS:
;       NCDFid = integer, id of already opened NCDF file
; KEYWORD PARAMETERS:
;	ALL - all attributes are returned
;	      default is that just variable scoped attributes are returned
; OUTPUTS:
;       att_array = string array of attribute names
; AUTHOR:
;       Tami Kovalick
;
; MODIFICATION HISTORY:
;
;-------------------------------------------------------------------------
FUNCTION ncdf_getvar_attribute_names, NCDFid, ALL=ALL

cinfo = ncdf_inquire(NCDFid) ; inquire about the ncdf to get #attributes
; Create initial data structure to hold all of the metadata information

if (cinfo.nvars gt 0 ) then begin
   max_varattrs = ncdf_max_numvar_attributes(NCDFid, varid=varid)
endif

n_allatts = cinfo.ngatts + max_varattrs

; get the names of the attributes

;TJK 1/28/2003 change size because this won't work when data ncdfs don't have
;any global attributes att_array = make_array(cinfo.natts-1,/string, value="")
;TJK 3/21/2003 - added a check for when there are no attributes in the data
;ncdfs at all...

if (keyword_set(ALL)) then all = 1 else all = 0 ;add a keyword to get all attributes

if (n_allatts gt 0) then begin
  att_array = make_array(n_allatts,/string, value="")
  i = 0
; get globals 1st
  for anum=0,cinfo.ngatts-1 do begin
    att_array[i] = ncdf_attname(NCDFid,anum, /global) ; global attribute
    i = i + 1
  endfor
; get variable attributes next (for the varid w/ the most attributes)
  for anum=0,max_varattrs-1 do begin
    aname = ncdf_attname(NCDFid, varid, anum) ; get the attributes from the variable with the most attributes
    aname = strtrim(aname,2) ; trim any possible leading or trailing blanks
    ;call replace_bad_chars to replace any "illegal" characters in
    ;the attribute name w/ a legal one.  This was necessary for IDL 5.3.
    aname = replace_bad_chars(aname,repchar="_",found)
    att_array[i] = aname
    i = i + 1
   endfor

endif else att_array = make_array(1, /string, value="-1")
return,att_array
end

;+------------------------------------------------------------------------
; NAME: GET_NUMALLVARS_netcdf
; PURPOSE: 
; 	To return the total number of variables in the ncdf.
;
; CALLING SEQUENCE:
;       num_vars = get_numallvars_netcdf(CNAME=CNAME)
; INPUTS:
; KEYWORD PARAMETERS:
;	CNAME = string, name of a NCDF file to be opened and read
;	NCDFid = integer, id of an already opened NCDF file
; OUTPUTS:
;       num_vars = number of variables in the NCDF
; AUTHOR:
;       Tami Kovalick
;
; MODIFICATION HISTORY:
;
;-------------------------------------------------------------------------
FUNCTION get_numallvars_netcdf, CNAME=CNAME, NCDFid=NCDFid

; validate keyword combination and open ncdf if needed
if keyword_set(CNAME) AND keyword_set(NCDFid) then return,0 ; invalid
if keyword_set(CNAME) then NCDFindex = NCDF_OPEN(CNAME) ; open the ncdf
if keyword_set(NCDFid) then NCDFindex = NCDFid ; save the ncdf file number

; determine the number of variables 
cinfo = NCDF_INQUIRE(NCDFindex) ; inquire about number of variables
num_vars = cinfo.nvars 
if keyword_set(CNAME) then NCDF_close,NCDFindex ; close the ncdf
return, num_vars
end



;+------------------------------------------------------------------------
; NAME: GET_ALLVARNAMES_netcdf
; PURPOSE: 
; 	To return a string array containing the names of all of the
;	variables in the given NCDF file.
; CALLING SEQUENCE:
;       vnames = get_allvarnames_netcdf()
; INPUTS:
; KEYWORD PARAMETERS:
;	CNAME = string, name of a NCDF file to be opened and read
;	NCDFid = integer, id of an already opened NCDF file
;       VAR_TYPE = string, only return the names for variables who have an
;                  attribute called 'VAR_TYPE' and whose value matches the
;                  value given by this keyword.  (ex. VAR_TYPE='data')
; OUTPUTS:
;       vnames = string array of variable names
; AUTHOR:
;       Tami Kovalick, NASA/GSFC/Code 672
; MODIFICATION HISTORY:
;
;-------------------------------------------------------------------------
function get_allvarnames_netcdf, CNAME=CNAME, NCDFid=NCDFid, VAR_TYPE=VAR_TYPE
if keyword_set(CNAME) AND keyword_set(NCDFid) then return,0 ; invalid
if keyword_set(CNAME) then NCDFindex = NCDF_OPEN(CNAME) ; open the ncdf
if keyword_set(NCDFid) then NCDFindex = NCDFid ; save the ncdf file number

; determine the number of variables 

cinfo = NCDF_INQUIRE(NCDFindex) ; inquire about number of variables
numvars = cinfo.nvars 

;vnames=strarr(numvars)
vnames=''

; Set up an error handler
CATCH, Error_status
if Error_status ne 0 then begin
   if keyword_set(CNAME) then ncdf_close,NCDFindex
   print, "STATUS= Error reading NCDF. "
   print,!ERR_STRING, " get_allvarnames_netcdf" & return,-1
endif

; Get the name of every z variable
for j=0,cinfo.nvars-1 do begin
;   vinfo = NCDF_VARINQ(NCDFindex,j,/ZVARIABLE)
   vinfo = NCDF_VARINQ(NCDFindex,j)

; RCJ 11May2021

if keyword_set(VAR_TYPE) then begin ; only get VAR_TYPE='data'
  res=ncdf_varinq(NCDFindex,vinfo.name)
  for i=0,res.natts-1 do begin
   res=ncdf_attname(NCDFindex,vinfo.name,i)
    
   case res of
      'VAR_TYPE': begin
        ncdf_attget,NCDFindex,vinfo.name,'VAR_TYPE',attgot 
        if string(attgot) eq 'data' then vnames=[vnames,vinfo.name]   
      end
      'Var_Type': begin
        ncdf_attget,NCDFindex,vinfo.name,'Var_Type',attgot
        if string(attgot) eq 'data' then vnames=[vnames,vinfo.name]   
      end 
      else:  
   endcase 
  endfor
endif else begin
  vnames=[vnames,vinfo.name]
endelse 
endfor

if keyword_set(CNAME) then NCDF_CLOSE,NCDFindex
return,vnames[1:*]
end
 
;------------------------------------------------------------------------------------

function ncdf_find_var, NCDFid, variable
;Look in the current data ncdf and return the actual correct spelling 
;of this variable (called only when one doesn't exist).
;This can occur when the master has a variable like "Epoch" (which many 
;of the datasets data files have, but then for whatever reason, some of the
;data files have the epoch variable spelled as "EPOCH"... which in NCDF 
;land is not the same variable (variable names are case sensitive)!

cinfo = NCDF_INQUIRE(NCDFid) ; inquire about number of variables
numvars = cinfo.nvars
for j=0,numvars-1 do begin
;   vinfo = NCDF_VARINQ(NCDFid,j,/ZVARIABLE)
   vinfo = NCDF_VARINQ(NCDFid,j)
   caps = strupcase(strtrim(vinfo.name,2)); trim blanks and capitalize
   in_caps = strupcase(strtrim(variable,2)); trim blanks and capitalize
   match = where(caps eq in_caps,match_cnt)
   if (match_cnt gt 0) then begin
	print, variable,' match found = ',vinfo.name, ' returning...'
	return, vinfo.name
   endif
endfor

return, -1 ;no match found
end

;------------------------------------------------------------------------------------

function ncdf_find_epochvar, NCDFid
;Look in the current data ncdf and return the actual correct spelling 
;of this epoch variable (called only when one doesn't exist).
;This occurs when the master has depend0 = "Epoch" (which many of the datasets
;data files have, but then for whatever reason, a data file has
;the epoch variable spelled as "EPOCH"... which in NCDF land is not a match!


cinfo = NCDF_INQUIRE(NCDFid) ; inquire about number of variables
numvars = cinfo.nvars
for j=0,numvars-1 do begin
;print, 'in find_epochvar'
;   vinfo = NCDF_VARINQ(NCDFid,j,/ZVARIABLE)
   vinfo = NCDF_VARINQ(NCDFid,j)
   caps = strupcase(strtrim(vinfo.name,2)); trim blanks and capitalize
   match = where(caps eq 'EPOCH',match_cnt)
   if (match_cnt gt 0) then begin
	;print, 'epoch match found = ',vinfo.name, ' returning...'
	return, vinfo.name
   endif
endfor

return, -1 ;no match found
end
;----------------------------------------------------------

;Function merge_metadata_netcdf
;Merge the master (cdf) and the 1st data netcdf's attributes when some of the
;master's attribute values are intensionally left blank.
;This function was originally conceived to accommodate ACE's concerns
;about including the most appropriate metadata with our listings.
;But will likely be used for other datasets/projects.
;
;Written by Tami Kovalick, QSS, 4/8/2005
;
function merge_metadata_netcdf, cnames, base_struct, all=all
;
;Mods: 12/7/2005 by TJK
;Had to change how I dealt w/ multiple element attributes
;originally I just appended elements together so I didn't have to
;remake the variable structure.  But listing didn't like character
;strings that were so long.  So this routine basically re-builds the
;data structure for every variable just to include multi-element attributes.
;These changes were prompted by "blanking" out many of the global attributes
;in the cluster masters.

; Set up an error handler
CATCH, Error_status
if Error_status ne 0 then begin
   if keyword_set(CNAMES) then ncdf_close,data_NCDFid
   print,!ERR_STRING, " in merge_metadata_netcdf" 
   return, burley ;probably not a critical error, just return the buffer
endif

; Array of exempted attributes.  These attributes are added by read_myCDF on a variable
; by variable basis and thus may not be in the initial variable.
; Ron Yurow (Jan 4, 2019)
exception = ["NO_DATA", "ALLOW_BIN"]

status = 0
;do this merge if we have more than two cdfs specified and the 1st one
;is a master.  
; For netcdf data files, we will be merging a cdf master, if it exists
;and a netcdf data files metadata.
if ((n_elements(cnames) ge 2) and strpos(cnames[0],'00000000') ne -1) then begin  

      data_NCDFid = ncdf_open(cnames[1]) ;this is the data netcdf
      
      ; RCJ 01/14/2013   get_allvarnames needs to know the value of 'all'.
      ;data_vnames = get_allvarnames(NCDFid=data_NCDFid)
      if keyword_set(ALL) then begin
      ; Modified the following two lines so that they will use the MASTER to extract
      ; variable names from.  This is necessary in case the MASTER has the attribute 
      ; VAR_TYPE defined and the data CDFs do not.
      ; Ron Yurow (Sep 30, 2018)
      ;if all eq 1 then data_vnames = get_allvarnames(CDFid=data_CDFid)
      ;if all eq 2 then data_vnames = get_allvarnames(CDFid=data_CDFid,var_type='data')
      if all eq 1 then data_vnames = get_allvarnames(CNAME=cnames[0])
      if all eq 2 then data_vnames = get_allvarnames(CNAME=cnames[0],var_type='data')
      endif else data_vnames = get_allvarnames_netcdf(NCDFid=data_NCDFid)

      atmp = read_mymetadata_netcdf (data_vnames[0], data_NCDFid)
      dnames=tag_names(atmp)
      data_attr=where(dnames eq 'FIELDNAM') ; this is the break between global and variable attributes

      bnames=tag_names(base_struct.(0))
      base_attr=where(bnames eq 'FIELDNAM') ; this is the break between global and variable attributes

      tpnames=tag_names(base_struct) 
      cpy_struct = 0 ; initialize a variable that will contain our new structure filled below

      ;compare atmp values w/ base_struct

   if (base_attr[0] gt 0 and data_attr[0] gt 0) then begin ;we have global attributes to look at
      for vars = 0, n_tags(base_struct) - 1 do begin

         ; Initialize the array addlist array.  We will use this array to store additional variable 
         ; attributes that are created read_myCDF itself and need to be added to a variable. 
         ; Ron Yurow (Jan 4, 2019)
         addlist = [!NULL]

         ; Compare the list of attributes for the current variable being processed with that from the first
         ; variable in the structure.  Currently, variable attributes are taken from intial variable, so any 
         ; attribute that appears in a subsequent variable but not in it the initial will be deleted (not sure
         ; if this is inteneded behavior).  Special excptions will be made for attributes added by read_myCDF. 
         ; Ron Yurow (Jan 4, 2019)

         ; tnames is the array of the all the attributes from the variable being processed.
         tnames = TAG_NAMES (base_struct.(vars))

         ; Find any attributes that the current variable has, but not the initial (or visa versa)
         all_att_names = [bnames, tnames]
         all_att_names = all_att_names [SORT (all_att_names)]
         u = UNIQ (all_att_names)
         d = u [1:*] - u
         ; singletons are the indexes of attributes that only exist in one variable.
         ; cnt is the number of these attributes
         singletons = WHERE(d eq 1, cnt) + 1

         ; check if we have any of these of 'orphan' attributes
         IF cnt gt 0 THEN BEGIN 
            ; for each singleton, check if it is a member of the current variable being processed
            ; (and not the initial!!) and if it is one of the attributes that may be added by 
            ; read_myCDF.  If that is the case, then add it to the array extra attributes that 
            ; we need to add to the variable.
            FOR i = 0, cnt - 1 DO BEGIN
               target = all_att_names [u [singletons [i]]]
               sink = WHERE (target eq bnames, exist)
               
               IF (~ exist) THEN BEGIN            
                  sink = WHERE (target eq exception, accept)
                  IF accept ne 0 THEN  addlist = [addlist, target] 
               ENDIF
            ENDFOR
         ENDIF 
      
         for bnum = 0, n_elements(bnames)-1 do begin ;have to do all attributes now
            if (bnum ge base_attr) then begin
               ; attributes are variable_scope:
	       
               ; Ron Yurow (Jan 4, 2008)
               ; Rewrite the next couple of lines so that values for attributes and attribute names
               ; (from bnames) are matched by name and not just structure index (which may vary).
               ;1st in structure (highly unlikely)
               ;if (bnum eq 0) then new_struct = create_struct(bnames[bnum],base_struct.(vars).(bnum)) else $
               ;	 new_struct = create_struct(temporary(new_struct), bnames[bnum],base_struct.(vars).(bnum))
               pos = WHERE (bnames [bnum] eq tnames, found)
               IF found ne 0 THEN BEGIN 
	          if (bnum eq 0) then new_struct = create_struct(bnames[bnum], base_struct.(vars).(pos)) else $
		 	   new_struct = create_struct(temporary(new_struct), bnames[bnum], base_struct.(vars).(pos))
               ENDIF
            endif else begin
	       ; attributes are global_scope : 
	   
	       ; RCJ 03Mar2020 Commented this out. List can be quite long when listing...
	       ;                 Maybe make an informational variable for the cdf ?
	       ; RCJ 25Feb2020 Add global attr 'cdaweb_parents' to structure
	       ;if bnum eq base_attr-1 then begin ; add at the end of list of global attrs
	       ;   ncnames=['']
	       ;   for nc=0,n_elements(cnames)-1 do begin
	       ;      pts=strsplit(cnames[nc],'/',/extract)
	       ;      ncnames=[ncnames,pts[n_elements(pts)-1]]
	       ;   endfor
	       ;   ncnames=ncnames[1:*]
	       ;   new_struct = create_struct(temporary(new_struct), 'CDAWEB_PARENTS', ncnames)
               ;endif
			
               if (base_struct.(vars).(bnum)[0] ne '') then begin 
	          ;global attribute isn't blank in master...
	       
                  if (bnum eq 0) then new_struct = create_struct(bnames[bnum],base_struct.(vars).(bnum)) else $
                	new_struct = create_struct(temporary(new_struct), bnames[bnum],base_struct.(vars).(bnum))		
	       endif else begin 
	          ;attribute IS blank in master get value for data cdf
	       
	          ;**** have to match up the tag names so get the values in the right places.
	          s = where(dnames eq bnames[bnum], wc)
                  ;	print, 'DEBUG1 current master attribute ',bnames[bnum]
	          if (wc gt 0) then begin
                     ;	print, 'DEBUG2 found in data ',bnames[bnum]
		     if (atmp.(s[0])[0] ne '') then begin
                        ;		      print, 'DEBUG Setting missing attribute value ', bnames[bnum], ' = ',atmp.(s[0]), ' from value found in 1st data cdf'
                        if (n_tags(new_struct) eq 0) then begin
                           new_struct = create_struct(bnames[bnum],atmp.(s[0]))
                        endif else begin
                           new_struct = create_struct(temporary(new_struct), bnames[bnum],atmp.(s[0]))
                        endelse
		     endif ;value is good in data cdf
	          endif ; found an attribute match between data and master
               endelse ;attribute value is blank
	    endelse ;attribute is global scope
         endfor  ; end of bnum

         ; Ron Yurow (Jan 4, 2019)
         ; Append any additional attributes to variable structure that were previously identified.
         FOR new_attrib = 0, N_ELEMENTS (addlist) - 1 DO BEGIN
            pos = WHERE  (addlist [new_attrib] eq tnames, found)
            IF  found gt 0 THEN new_struct = CREATE_STRUCT (temporary(new_struct), addlist [new_attrib], base_struct.(vars).(pos)) 
         ENDFOR

         if (n_tags(cpy_struct) eq 0) then cpy_struct = create_struct(tpnames[vars],new_struct) else $
		cpy_struct = create_struct(temporary(cpy_struct),tpnames[vars],new_struct)
      endfor ; end of vars
      
      ; RCJ 12/13/05  wind 3dp data has no global or var attributes
      ; so data_attr(0) = -1 and we get here. We still want
      ; to return the structure so make cpy_struct=base_struct :
   endif else cpy_struct = temporary(base_struct)
   ncdf_close,data_NCDFid
endif else cpy_struct = temporary(base_struct)

return, cpy_struct
end

;+------------------------------------------------------------------------
; NAME: READ_MYNETCDF
; PURPOSE: 
;	Read all data and metadata for given variables, from given NCDF
;       files, and return all information in a single anonymous structure
;       of the form: 
;          structure_name.variable_name.attribute_name.attribute_value
;
; CALLING SEQUENCE:
;       out = read_mynetcdf(vnames,cnames)
; INPUTS:
;       vnames = string, array of variable names or a single string of
;                names separated by a comma.  (ex. 'Epoch,Magfld,Bmax')
;       cnames = string, array of NCDF filenames or a single string of
;                names separated by a comma.
; KEYWORD PARAMETERS:
;	ALL = 0: get data and metadata for requested variable(s) only.
;             1: get data and metadata for ALL variables in the NCDFs.
;             2: get data and metadata for all var_type='data' variables.
;       NODATASTRUCT = If set, instead of returning the data for each variable
;                   in the 'DAT' attribute field, create a 'HANDLE' field
;                   and set it to the handle id of a data handle which
;                   holds the data for each variable.
;       NOQUIET = If set, do NOT set the !QUIET system variable before
;                 reading the ncdf file(s).
;       DEBUG = If set, print out some progress information during reading.
;	TSTART = epoch starting value - YYYYMMDD etc. string.
;	TSTOP = epoch ending value - YYYYMMDD etc. string.
; OUTPUTS:
;       out = anonymous structure holding all data and metadata for the
;             requested variables. If an error occurs, that we know how
;             to deal w/, an alternate structure is returned, its structure
;	      is as follows: ('DATASET',d_set,'ERROR',v_err,'STATUS',v_stat)
;	      
; AUTHOR:
;       Tami Kovalick - ADNET - September 2016
; MODIFICATION HISTORY:
;	Tami Kovalick - this is based on read_myCDF, but should read
;                       netcdfs.
;
;-------------------------------------------------------------------------

FUNCTION read_mynetcdf, vnames, cnames, ALL=ALL,NODATASTRUCT=NODATASTRUCT, $
                                     NOQUIET=NOQUIET,DEBUG=DEBUG, $
				     TSTART=TSTART, TSTOP=TSTOP, $
START_MSEC=START_MSEC, STOP_MSEC=STOP_MSEC, START_USEC=START_USEC, $ 
STOP_USEC=STOP_USEC, START_NSEC=START_NSEC, STOP_NSEC=STOP_NSEC, $
START_PSEC=START_PSEC, STOP_PSEC=STOP_PSEC, NOVIRTUAL=NOVIRTUAL


compile_opt idl2
;not a Netcdf thing if (!version.release ge '8.0') then NCDF_SET_VALIDATE, /no  ;turn off NCDF validation

; establish exception handler to trap errors from all sources.

CATCH,error_status
if (error_status ne 0) then begin
   print,!ERR_string ," Trapped in read_mynetcdf."; output description of error
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
         parts=str_sep(cnames[cx],'/')
         piece=strupcase(str_sep(parts[n_elements(parts)-1],'_'))
         tempnm= piece[0]+'_'+piece[1]+'_'+piece[2]
         val_data="DATASET="+tempnm
      endelse
      tmpstr=create_struct('DATASET',val_data,'ERROR',val_err,'STATUS',val_stat)
      return, tmpstr
  endif  
  return,-1 ; return failure flag
endif ;catch

if keyword_set(DEBUG) then debug = 1 else debug=0

need_timeslice = 0L ;initialize

;This routine will be a little different than read_mycdf in
;that we're for the most part, going to read/expect netcdf
;files, but if there's a master cdf (w/ 00000000 in its name),
;that will be a .cdf file.  So the sections below that are for reading
;the master, will call the cdf routines.

orig_cnames = cnames ;save original data file names

; Validate cnames parameter, remove .ncdf extensions if present
s = size(cnames) & ns = n_elements(s)
if (s[ns-2] eq 7) then begin
   if (s[0] eq 0) then cnames = break_mySTRING(cnames,DELIMITER=',')
   for i=0,n_elements(cnames)-1 do begin
      j=strpos(cnames[i],'.ncdf') & if (j eq -1) then j=strpos(cnames[i],'.NCDF') & if (j eq -1) then j=strpos(cnames[i],'.cdf')
      if (j ne -1) then cnames[i] = strmid(cnames[i],0,j)
   endfor
endif else begin
   print,'ERROR=NCDF filenames must be given as strings.' & return,-1
endelse
if keyword_set(DEBUG) then begin
  print,'Number of NCDFs to read=',n_elements(cnames)
  print,'DEBUG files to read = ',cnames
endif

quiet_flag = !quiet ; save current state of quiet flag
if not keyword_set(NOQUIET) then begin
  !quiet=1 ; turn off annoying ncdf messages
   ncdf_control, 0, /noverbose ; turn off error messages
   ;print, 'Announcement of annoying NCDF messages = ', !quiet
endif

;TJK setup a structure called table to hold the variable name as they are in
;the ncdf and then an 'equivalent' name if the real variable name turned out to 
;contain "illegal characters", e.g. "!,@,#" etc..

common global_table, table
;TJK 03/15/2010 - check for the max number of variables between the
;                 master and a datancdf (if there is a
;                 master)... sometimes the master has fewer (like w/
;                 sta_l1_mag_rtn) which then causes a problem.
;Where there's a master (cdf) and data files (netcdf)
;print, 'DEBUG'
HELP, 'ncdf', /DLM
if ((n_elements(cnames) gt 1) and (strpos(cnames[0],'00000000') ne -1)) then begin
  print, 'calling get_numallvars_netcdf for ', cnames[1]
  num_vars = max([get_numallvars(CNAME=cnames[0]), get_numallvars_netcdf(CNAME=cnames[1])])
endif else begin
  print, 'calling get_numallvars_netcdf for ', cnames[0]
  num_vars = get_numallvars_netcdf(CNAME=cnames[0]) ;Where there are just data netcdfs
endelse
var_names = strarr(num_vars)
total_storage_time = 0L
;varname will contain the real ncdf variable name(s)
;equiv will contain the "fake" one(s)
table = create_struct('varname',var_names,'equiv',var_names)


; If the ALL keyword is set then get names of all data variables
; RCJ 11/21/2003  Added another option for 'all'. Now if all=0: read requested
;   var(s);  if all=1: read all vars;  if all=2: read all 'data' vars
if keyword_set(ALL) then begin
  if ((n_elements(cnames) gt 1) and (strpos(cnames[0],'00000000') ne -1)) then begin ;master is a cdf
   if all eq 1 then vnames = get_allvarnames(CNAME=cnames[0])
   if all eq 2 then vnames = get_allvarnames(CNAME=cnames[0],var_type='data')
  endif else begin
   if all eq 1 then vnames = get_allvarnames_netcdf(CNAME=cnames[0])
   if all eq 2 then vnames = get_allvarnames_netcdf(CNAME=cnames[0],var_type='data')
  endelse
endif
variables_read = make_array(n_elements(cnames),num_vars,/string, value="")
;variables_comp = make_array(n_elements(cnames),num_vars,/string, value="")


;make a copy of the vnames in the orig_names array for use w/
;virtual variables and/or alternate views. TJK 2/4/98
orig_names = vnames

; Validate vnames parameter.  May be a strarr or single string
s = size(vnames) & ns = n_elements(s)
if (s[ns-2] eq 7) then begin
   if (s[0] eq 0) then vnames = break_mySTRING(vnames,DELIMITER=',')
endif else begin
   print,'ERROR=variable names must be given as strings.' & return,-1
endelse

;TJK - 12/16/96
; added this section of code to check whether the requested variables
;are actually in the NCDFs (other than the master CDFs).  If not, take them
;out... and continue building the structure w/ the resultant variables.
 
rcount = n_elements(vnames)
num_virs = -1 ; keep counter of number of virtual variables found
virs_found = 0L ;TJK 11/28/2006 set a flag, so that if virtual variables are found in the master, 
                ;don't check in the data ncdf.
;create a structure to contain the virtual variable name and whether
;the variable was found in the master or in a data NCDF, set to
;0 or 1 respectively.
n_tmp = strarr(num_vars) ; initialize it
m_tmp = make_array(num_vars,/integer, value=-1)
vir_vars= create_struct('name',n_tmp,'flag',m_tmp)
; RCJ 09/19/05  We are going to use the isis_flag for alouette-2 data too.
isis_flag = 0
for cx=0,n_elements(cnames)-1 do begin
   if (rcount gt 0) then begin
      ; Open the NCDF and inquire about global information about file
;    print, 'DEBUG, getting global attrbitue info. for ',cnames[cx]
    if ((n_elements(cnames) gt 1) and (strpos(cnames[cx],'00000000') ne -1)) then begin ;master is a cdf
      CDFid = cdf_open(cnames[cx]) 
      cinfo = cdf_inquire(CDFid)
    endif else begin
      NCDFid = ncdf_open(cnames[cx]) 
      cinfo = ncdf_inquire(NCDFid)
    endelse
      if (debug) then print,'Verifying variables in ',cnames[cx]
      ; if there is a master, look there for virtual variables that may not
      ; be defined in the actual ncdfs...
      if (strpos(cnames[cx],'00000000') ne -1) then begin  ;a master
	 ;
	 if (debug) then print, 'checking ',cnames[cx], ' for Virtual variables';
         ;read the metadata for the variables requested from the master...	
         vdep_cnt=0;
         chkvv_dep=''

         ;  RCJ 08Nov2016  Added this for loop to look for vars not initially added to the structure atmp.
	 ;                 These are, for example, a comp0 of a VV which is the dep2 of a comp0 to the requested
	 ;                 var which, of course, is a VV.  These are too many levels past the requested var and 
	 ;                 not added to the vnames array by calls to add_mycomponents, add_mydepends or add_mydeltas
	 ;                 in the main for loop a few lines below.

         for nreq =0, n_elements(vnames)-1 do begin
	    atmp = read_myMETADATA (vnames[nreq], CDFid)
	    add_mycomponents,atmp,vnames
	    add_mydepends,atmp,vnames
	    add_mydeltas,atmp,vnames
         endfor
;
         for nreq =0, n_elements(vnames)-1 do begin
	    atmp = read_mymetadata(vnames[nreq], CDFid)
	    add_mydeltas,atmp,vnames
	    atags = tag_names (atmp)
            ;TJK 09/28/2001 add code to flag whether we're looking at an ISIS mission, if so set
            ; a flag that's used lower down.  We need to check here in the master instead of in
            ; the data ncdfs because lots of data ncdf's don't have a mission_group global attribute.
	    b0 = tagindex ('MISSION_GROUP', atags)
	    if (b0[0] ne -1) then begin
	 	if ((strupcase(atmp.mission_group[0]) eq 'ISIS') or $
	           (strupcase(atmp.mission_group[0]) eq 'ALOUETTE')) $
		then isis_flag = 1
	 	;if (strupcase(atmp.mission_group(0)) eq 'ISIS') then isis_flag = 1
            endif
;TJK 11/23/2005 add logic to look for virtual depend variables related
;to "regular" variables... for some reason we haven't needed this till
;now!
            b1 = tagindex ('DEPEND_0', atags)
            if (b1[0] ne -1 ) then begin
	       if (atmp.depend_0 ne '') then begin
	           num = where(chkvv_dep eq atmp.depend_0, cnt)
	           if (cnt eq 0) then begin
		      chkvv_dep=[chkvv_dep,atmp.depend_0]
                      vdep_cnt=vdep_cnt+1
                   endif
	       endif
	    endif
            b1 = tagindex ('DEPEND_1', atags)
            if (b1[0] ne -1 ) then begin
	        atmp_dep1=atmp.depend_1
		; RCJ 05/16/2013 ok, but if alt_cdaweb_depend_1 exists, use it instead:
	        q=tagindex ('ALT_CDAWEB_DEPEND_1', atags)
		if q[0] ne -1 then if (atmp.alt_cdaweb_depend_1 ne '') then atmp_dep1=atmp.alt_cdaweb_depend_1
                ;if (atmp.depend_1 ne '') then begin
                if (atmp_dep1 ne '') then begin
	            if q[0] ne -1 then num = where(chkvv_dep eq atmp.alt_cdaweb_depend_1, cnt) else num = where(chkvv_dep eq atmp.depend_1, cnt)
	            if (cnt eq 0) then begin
                        if q[0] ne -1 then chkvv_dep=[chkvv_dep,atmp.alt_cdaweb_depend_1] else chkvv_dep=[chkvv_dep,atmp.depend_1]
                        vdep_cnt=vdep_cnt+1
                    endif
                endif
            endif
            b1 = tagindex ('DEPEND_2', atags)
	    if (b1[0] ne -1 ) then begin
	        atmp_dep2=atmp.depend_2
		; RCJ 05/16/2013 ok, but if alt_cdaweb_depend_1 exists, use it instead:
	        q=tagindex ('ALT_CDAWEB_DEPEND_2', atags)
	        if q[0] ne -1 then if (atmp.alt_cdaweb_depend_2 ne '') then atmp_dep2=atmp.alt_cdaweb_depend_2 
		;if (atmp.depend_2 ne '') then begin
		if (atmp_dep2 ne '') then begin
	            if q[0] ne -1 then num = where(chkvv_dep eq atmp.alt_cdaweb_depend_2, cnt) else num = where(chkvv_dep eq atmp.depend_2, cnt) 
                    if (cnt eq 0) then begin
 	               if q[0] ne -1 then chkvv_dep=[chkvv_dep,atmp.alt_cdaweb_depend_2] else chkvv_dep=[chkvv_dep,atmp.depend_2]
		       vdep_cnt=vdep_cnt+1
                    endif
	        endif
            endif
            b1 = tagindex ('DEPEND_3', atags)
	    if (b1[0] ne -1 ) then begin
	        atmp_dep3=atmp.depend_3
;		help,atmp_dep3
	        q=tagindex ('ALT_CDAWEB_DEPEND_3', atags)
	        if q[0] ne -1 then if (atmp.alt_cdaweb_depend_3 ne '') then atmp_dep3=atmp.alt_cdaweb_depend_3 
		if (atmp_dep3 ne '') then begin
	            if q[0] ne -1 then num = where(chkvv_dep eq atmp.alt_cdaweb_depend_3, cnt) else num = where(chkvv_dep eq atmp.depend_3, cnt) 
                    if (cnt eq 0) then begin
 	               if q[0] ne -1 then chkvv_dep=[chkvv_dep,atmp.alt_cdaweb_depend_3] else chkvv_dep=[chkvv_dep,atmp.depend_3]
		       vdep_cnt=vdep_cnt+1
                    endif
	        endif
            endif

;TJK 4/17/2008 adding check for deltas here otherwise those vars
;get thrown out below (needed this for voyager coho datasets
            b1 = tagindex ('DELTA_PLUS_VAR', atags)
	    if (b1[0] ne -1 ) then begin
	        if (atmp.delta_plus_var ne '') then begin
	            num = where(chkvv_dep eq atmp.delta_plus_var, cnt)
                    if (cnt eq 0) then begin
                        chkvv_dep=[chkvv_dep,atmp.delta_plus_var]
		        vdep_cnt=vdep_cnt+1
	            endif
	         endif
            endif
            b1 = tagindex ('DELTA_MINUS_VAR', atags)
	    if (b1[0] ne -1 ) then begin
	        if (atmp.delta_minus_var ne '') then begin
	             num = where(chkvv_dep eq atmp.delta_minus_var, cnt)
                     if (cnt eq 0) then begin
		         chkvv_dep=[chkvv_dep,atmp.delta_minus_var]
		         vdep_cnt=vdep_cnt+1
	             endif
	        endif
	    endif
;end of 4/17/2008 added check for deltas
;TJK 2/13/2015 remove extra blank element in index 0; leaving it in
;causes problems below w/ identifying virtual depends for virtual variables.
         if(vdep_cnt gt 0) then begin
            not_blank = where(chkvv_dep ne '', blank_cnt)
            if blank_cnt gt 0 then chkvv_dep = chkvv_dep[not_blank]
         endif
         
;if (debug) then print, 'At bottom of new section = ',chkvv_dep

;TJK 11/23/2005 - end of new section, back to original section looking
;                 for virtual variables w/in virtual variables

	    b0 = tagindex ('VIRTUAL', atags)
            c0 = tagindex ('COMPONENT_0', atags) ;add check for component_0 value as well
            ;look through metadata and look for virtual variables...
            ; get components of the virtual variables and add them to the vnames  
            ; array...
;TJK 11/6/2009 add check for component_0 in order to determine if virtual
;variable definition is for real or not.
;               if (b0[0] ne -1 ) then begin
;                  if (strlowcase(atmp.VIRTUAL) eq 'true') then begin
            if ((b0[0] ne -1) and (c0[0] ne -1)) then begin
               if ((strlowcase(atmp.VIRTUAL) eq 'true') and (atmp.COMPONENT_0 ne '')) then begin
	          if (DEBUG) then print, 'Found a VV ',vnames[nreq],' in Master NCDF.'
	          num_virs = num_virs + 1
                  vir_vars.name[num_virs] = vnames[nreq]
                  vir_vars.flag[num_virs] = 0 ;indicate this var found in master
	          if (DEBUG) then begin
                      print, 'found a VV ', vnames[nreq], ' in Master NCDF.'
                      print, 'adding deltas and components next...'
                  endif

                  add_myDELTAS, atmp, vnames
	          add_myCOMPONENTS, atmp, vnames

                  ; Check VV's depends for other VV's and add to list
                  ;TJK 11/98 added logic to only add the variable if it doesn't
                  ;already exist in the chkvv_dep list.
                  b1 = tagindex ('DEPEND_0', atags)
	          if (b1[0] ne -1 ) then begin
	             if (atmp.depend_0 ne '') then begin
	                num = where(chkvv_dep eq atmp.depend_0, cnt)
	                if (cnt eq 0) then begin
		           ;chkvv_dep(vdep_cnt)=atmp.depend_0
		           chkvv_dep=[chkvv_dep,atmp.depend_0]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
	          b1 = tagindex ('DEPEND_1', atags)
	          if (b1[0] ne -1 ) then begin
	             atmp_dep1=atmp.depend_1
		     ; RCJ 05/16/2013 ok, but if alt_cdaweb_depend_1 exists, use it instead:
	             q=tagindex ('ALT_CDAWEB_DEPEND_1', atags)
		     if (q[0] ne -1) then if (atmp.alt_cdaweb_depend_1 ne '') then atmp_dep1=atmp.alt_cdaweb_depend_1 
	             ;if (atmp.depend_1 ne '') then begin
                     if (atmp_dep1 ne '') then begin
			if (q[0] ne -1) then num = where(chkvv_dep eq atmp.alt_cdaweb_depend_1, cnt) else num = where(chkvv_dep eq atmp.depend_1, cnt)
	                if (cnt eq 0) then begin ;if not in the list already, add it
		           ;chkvv_dep[vdep_cnt]=atmp.depend_1
                           ;below still gets the value of alt_cdaweb_depehnd_1, even if it is blank, which we don't want.
                                ;if q[0] ne -1 then
                                ;chkvv_dep=[chkvv_dep,atmp.alt_cdaweb_depend_1]
                                ;else
                                ;chkvv_dep=[chkvv_dep,atmp.depend_1]
		           chkvv_dep=[chkvv_dep,atmp_dep1]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
	          b1 = tagindex ('DEPEND_2', atags)
	          if (b1[0] ne -1 ) then begin
	             atmp_dep2=atmp.depend_2
		     ; RCJ 05/16/2013 ok, but if alt_cdaweb_depend_2 exists, use it instead:
	             q=tagindex ('ALT_CDAWEB_DEPEND_2', atags)
		     if (q[0] ne -1) then if (atmp.alt_cdaweb_depend_2 ne '') then atmp_dep2=atmp.alt_cdaweb_depend_2 
	             ;if (atmp.depend_2 ne '') then begin
                     if (atmp_dep2 ne '') then begin
	                if (q[0] ne -1) then num = where(chkvv_dep eq atmp.alt_cdaweb_depend_2, cnt) else num = where(chkvv_dep eq atmp.depend_2, cnt)
                        if (cnt eq 0) then begin
		           ;chkvv_dep(vdep_cnt)=atmp.depend_2
                           ;below still gets the value of alt_cdaweb_depehnd_2, even if it is blank, which we don't want.
		           ;if q[0] ne -1 then chkvv_dep=[chkvv_dep,atmp.alt_cdaweb_depend_2] else chkvv_dep=[chkvv_dep,atmp.depend_2]
		           chkvv_dep=[chkvv_dep,atmp_dep2]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
	          b1 = tagindex ('DEPEND_3', atags)
	          if (b1[0] ne -1 ) then begin
	             atmp_dep3=atmp.depend_3
	             q=tagindex ('ALT_CDAWEB_DEPEND_3', atags)
		     if (q[0] ne -1) then if (atmp.alt_cdaweb_depend_3 ne '') then atmp_dep3=atmp.alt_cdaweb_depend_3 
                     if (atmp_dep3 ne '') then begin
	                if (q[0] ne -1) then num = where(chkvv_dep eq atmp.alt_cdaweb_depend_3, cnt) else num = where(chkvv_dep eq atmp.depend_3, cnt)
                        if (cnt eq 0) then begin
		           chkvv_dep=[chkvv_dep,atmp_dep3]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
                  ;TJK - 1/29/2001 add a check to see whether the component 
                  ; variables are virtual
	          b1 = tagindex ('COMPONENT_0', atags)
	          if (b1[0] ne -1 ) then begin
	             if (atmp.component_0 ne '') then begin
	                num = where(chkvv_dep eq atmp.component_0, cnt)
                        if (cnt eq 0) then begin
		           ;chkvv_dep(vdep_cnt)=atmp.component_0
		           chkvv_dep=[chkvv_dep,atmp.component_0]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
                  ;TJK - 1/27/2009 add a check to see whether the component_1
                  ; variables are virtual
	          b1 = tagindex ('COMPONENT_1', atags)
	          if (b1[0] ne -1 ) then begin
	             if (atmp.component_1 ne '') then begin
	                num = where(chkvv_dep eq atmp.component_1, cnt)
                        if (cnt eq 0) then begin
		           ;chkvv_dep(vdep_cnt)=atmp.component_1
		           chkvv_dep=[chkvv_dep,atmp.component_1]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
                  ;TJK - 1/27/2009 add a check to see whether the component_2 
                  ; variables are virtual
	          b1 = tagindex ('COMPONENT_2', atags)
	          if (b1[0] ne -1 ) then begin
	             if (atmp.component_2 ne '') then begin
	                num = where(chkvv_dep eq atmp.component_2, cnt)
                        if (cnt eq 0) then begin
		           ;chkvv_dep(vdep_cnt)=atmp.component_2
		           chkvv_dep=[chkvv_dep,atmp.component_2]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
	          b1 = tagindex ('DELTA_PLUS_VAR', atags)
	          if (b1[0] ne -1 ) then begin
	             if (atmp.delta_plus_var ne '') then begin
	                num = where(chkvv_dep eq atmp.delta_plus_var, cnt)
                        if (cnt eq 0) then begin
		           chkvv_dep=[chkvv_dep,atmp.delta_plus_var]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
	          b1 = tagindex ('DELTA_MINUS_VAR', atags)
	          if (b1[0] ne -1 ) then begin
	             if (atmp.delta_minus_var ne '') then begin
	                num = where(chkvv_dep eq atmp.delta_minus_var, cnt)
                        if (cnt eq 0) then begin
		           chkvv_dep=[chkvv_dep,atmp.delta_minus_var]
		           vdep_cnt=vdep_cnt+1
	                endif
	             endif
	          endif
               endif ; if atmp.virtual eq true
            endif ; if b0[0] ne -1
         endfor
         ; Now check the depend var's of the VV for VV

         if(vdep_cnt gt 0) then begin
            ;cwc=where(chkvv_dep ne '',cwcn)
            ;chkvv_dep=chkvv_dep(cwc)
            ;dont need to do this, extra blank removed up above chkvv_dep=chkvv_dep[1:*]
            for nvvq =0, n_elements(chkvv_dep)-1 do begin
               atmp = read_mymetadata(chkvv_dep[nvvq], CDFid)
               atags = tag_names (atmp)
               add_myDELTAS, atmp, vnames ;TJK add this here because we have regular variables w/ delta
                                          ;not only virtual variables (3/20/2014)

               b0 = tagindex ('VIRTUAL', atags)
;TJK 11/6/2009 add check for component_0 in order to determine if virtual
;variable definition is for real or not.
;               if (b0[0] ne -1 ) then begin
;                  if (strlowcase(atmp.VIRTUAL) eq 'true') then begin

               c0 = tagindex ('COMPONENT_0', atags) ;add check for component_0 value as well
               if ((b0[0] ne -1) and (c0[0] ne -1)) then begin
                  if ((strlowcase(atmp.VIRTUAL) eq 'true') and (atmp.COMPONENT_0 ne '')) then begin

;TJK 11/28/2006 - need to check to see if this virtual variable is in
;                 the vnames array, if not add it.  This is the case
;                 for the THEMIS epoch variables since none of them
;                 are "real" variables".
                     v_index = where(vnames eq chkvv_dep[nvvq] , v_count)
                     if (v_count eq 0) then begin ;need to add to vnames
                       if (DEBUG) then print, 'Found a VV ',chkvv_dep[nvvq],' among Depends; adding to vnames.'
                       vnames = [vnames,chkvv_dep[nvvq]]
                     endif
;TJK 3/26/2009 add check for whether the variable name is in the
;vir_vars array
                     v_index = where(vir_vars.name eq chkvv_dep[nvvq] , v_count)
                     if (v_count eq 0) then begin ;need to add to vir_vars...
                       if (DEBUG) then print, 'Found a VV ',chkvv_dep[nvvq],' among Depends; adding to vir_vars.'
                       num_virs = num_virs + 1
                       vir_vars.name[num_virs] = chkvv_dep[nvvq]
                       vir_vars.flag[num_virs] = 0 ;indicate this var found in master
                       add_myDELTAS, atmp, vnames 
                       add_myCOMPONENTS, atmp, vnames 
                     endif
                  endif
               endif
           endfor 
        endif ; if (vdep_cnt gt 0)

      if keyword_set(DEBUG) then print, 'end of master ncdf section - should have depends and others at this point: ',vnames
      endif  ;endif a master

;TJK 1/2/2007 - still have to determine how many virtual variables
;               there are for use lower down so can't set this for
;               keyword_set(ALL)
;      if (num_virs ge 0 or keyword_set(ALL)) then virs_found = 1L
      if (num_virs ge 0) then virs_found = 1L
 
      ; Continue to process vnames array...  
      ;  Possibly the virtual variable is defined in a data ncdf, check for 
      ;  that...

      if (strpos(cnames[cx],'00000000') eq -1) then begin  ;not a master (data file - netcdf)
         ; RCJ 02/02/2005 Added lines below. If we are not using a master cdf
	 ; or if we are using a ncdf generated by write_myNCDF
	 ; then the deltas and components are added to vnames here:
	 if cx eq 0 then begin ; first ncdf only. This could
	    ; represent a problem but I'm trying to avoid cases
	    ; like the one described by TJK 8/27/2002 below.

	    for nreq =0, n_elements(vnames)-1 do begin
	       atmp = read_mymetadata_netcdf (vnames[nreq], NCDFid)
               add_mydeltas, atmp, vnames
               add_mycomponents, atmp, vnames
            endfor
	 endif  
         ;if this is not a master ncdf we want to check to make sure all of the
         ;variables that were requested (in vnames) actually exist in this ncdf.
         ;If not, do not ask for them... doing so causing problems...
         ; look for the requested variable in the whole list of vars in this ncdf
         all_ncdf_vars = get_allvarnames_netcdf(NCDFid = NCDFid)
         ;  Look to see if a virtual variable is defined in the ncdf file...

;	 if (debug) then print, 'checking the data ncdf for virtual variables '

	 att_names = ncdf_getvar_attribute_names (NCDFid, /ALL) ;added all keyword, default 
							 ;is the variable attributes
	;TJK if no attributes are found, a -1 is returned above - which should
	;kick out below.
        ;TJK 11/28/2006 - add check for virs_found, if
        ;already found in master, don't check below

	afound = where(att_names eq 'VIRTUAL', acnt)
;TJK 3/12/2010 - add code to get component_0 info so it can be checked
;                below - otherwise we can't read themis ncdfs (w/o
;                        masters) because they depend so heavily on
;                        virtual variables.
        bfound = where(att_names eq 'COMPONENT_0', bcnt)

	if (acnt eq 1 and not(virs_found)) then begin ; continue on w/ the checking otherwise get out
         for nvar = 0, (n_elements(all_ncdf_vars)-1)  do begin
	   ;TJK 8/27/2002 replaced call to read_myMETADATA_netcdf since we found at least
	   ; one case w/ c*_pp_whi where doing so severely hampered performance
	   ; because some attributes had many thousands of entries.
           ; atmp = read_myMETADATA_netcdf (all_ncdf_vars(nvar), NCDFid) 
	   ; Replaced w/ call to ncdf_getvar_attribute_names and where statement, then
	   ; only get into this for loop if the VIRTUAL attribute actually exists...
           ; Now, just get the  value for the VIRTUAL attribute, not all
	   ; attributes
	   
           atmp = read_myattribute_netcdf(all_ncdf_vars[nvar],afound[0],NCDFid)
           ;this section finds all virtual variables in the data ncdf
            atags = tag_names (atmp)
	    b0 = tagindex ('VIRTUAL', atags)
;TJK 3/12/2010 added the following because it was wrong before... atags only has 
;the virtual tag in it not all tags for the given variable (unlike way
;above)... so get the component_0 info. and store in btmp, btags and use t0 below

            btmp = read_myattribute_netcdf(all_ncdf_vars[nvar],bfound[0],NCDFid)
            btags = tag_names (btmp)
            t0 = tagindex ('COMPONENT_0', btags);add check for component_0 value as well


;TJK 11/6/2009 add check for component_0 in order to determine if virtual
;variable definition is for real or not.
;            if (b0[0] ne -1) then begin
;               if (strlowcase(atmp.VIRTUAL) eq 'true') then begin
;TJK 3/12/2010 - this check was wrong since atags only had "virtual"
;                in it - instead use t0 and btmp defined above
;            c0 = tagindex ('COMPONENT_0', atags) ;add check for component_0 value as well
;            if ((b0[0] ne -1) and (c0[0] ne -1)) then begin
;               if ((strlowcase(atmp.VIRTUAL) eq 'true') and (atmp.COMPONENT_0 ne '')) then begin

;print, 'DEBUG 3rd check for VIRTUAL and COMPONENT '
;print, ' tag indexes for virtual ',b0, 'for component ', t0
;
;TJK 3/12/2010 change to only check for virtuals in data ncdfs if there 
;isn't a master ncdf.  So if the data ncdf has some virtuals defined
;they will be found... so they better be correct!  If the data ncdfs
;virtual's aren't correct like in sta/b_l1_mag/b_rtn/sc, then use a
;master cdf w/ the virtuals turned off (set to false) and you'll be set.

          if (strpos(cnames[0],'00000000') eq -1) then begin ;data netcdf
              ;print, 'DEBUG 1st ncdf is not a master ', cnames[0]

            if ((b0[0] ne -1) and (t0[0] ne -1)) then begin
               if ((strlowcase(atmp.VIRTUAL) eq 'true') and (btmp.COMPONENT_0 ne '')) then begin

		   if (debug) then print, 'found a VIRTUAL tag for ',all_ncdf_vars[nvar]

	          ;check to see if the vir_var is already in the array,
	          ; if so don't add it to the array.
 	          if (num_virs ge 1) then begin
	             c = where(vir_vars.name eq all_ncdf_vars[nvar], cnt) 
	             ;compare this one with the vir_vars
	             if (cnt eq 0) then begin ;if this one isn't in vir_vars add it.
	                num_virs = num_virs+1
	                vir_vars.name[num_virs] = all_ncdf_vars[nvar]
                        vir_vars.flag[num_virs] = 1 ;indicate this var found in data ncdf
	                if (DEBUG) then print, 'Found a VV ',all_ncdf_vars[nvar],' in data NCDF'

                     endif
	          endif else begin
	             num_virs = num_virs+1
	             vir_vars.name[num_virs] = all_ncdf_vars[nvar]
                     vir_vars.flag[num_virs] = 1 ;indicate this var found in data ncdf
	             if (DEBUG) then print, 'Found a VV ',all_ncdf_vars[nvar],' in data NCDF'
                  endelse	

	       endif
             endif
           endif ; check for whether a master ncdf has been defined
         endfor

         ;  If virtual variables were found in the ncdf, see if they were requested...

         dnames='' & cmpnames=''
         for req_vars=0, (n_elements(vnames)-1) do begin
            ;TJK 11/29/2006 - add code to get depends and components 
            ;for requested vnames - add to the vnames array a little 
            ;lower down.

            dtmp = read_mymetadata_netcdf (vnames[req_vars], NCDFid)
            add_myDEPENDS, dtmp, dnames
            for delts = 0, n_elements(dnames)-1 do begin
              ctmp = read_mymetadata_netcdf (dnames[delts], NCDFid)
              add_myCOMPONENTS,ctmp, cmpnames
            endfor

            vncdf = where(vir_vars.name eq vnames[req_vars], v_cnt)
            ;virtual has been requested...

            if (vncdf[0] ne -1L)  then begin
	       ; found in data ncdf (vs. Master ncdf) so we need to add it
               if(vir_vars.flag[num_virs]) then begin
	          if (debug) then print, 'Reading metadata for VV, ',vnames[req_vars]
                  atmp = read_mymetadata_netcdf (vnames[req_vars], NCDFid)
	          if (debug) then print, 'Add DELTAs for VV, ',vnames[req_vars]
	          add_myDELTAS, atmp, vnames
	          if (debug) then print, 'Add components for VV, ',vnames[req_vars]
	          add_myCOMPONENTS, atmp, vnames
	       endif
           endif 
         endfor
         ;Concatenate depends and component variables to the vnames array
         ;Make sure not to add any blanks or variables that are already
         ;in vnames.

         ;TJK 6/23/2008 add code to add the unique list of depends and
         ;components, but don't want to use sort and uniq on the requested vnames
         ;array, because that messes up the ordering of the variables (we want to 
         ;plot them in the same order they're listed in the master ncdf.


         vnames = unique_array(vnames, dnames)
         cmpnames = unique_array(vnames, cmpnames)


	endif ;TJK added on 8/27/2002 to match further checking for the VIRTUAL
	      ;attribute.

         ;  Now check that all variables requested, and components of virtuals, are
         ;  found in the ncdf file...
         for req_vars=0, (n_elements(vnames)-1) do begin
;            if (DEBUG) then print, 'Checking to see if variables are actually in this data ncdf.'
            fncdf = where(all_ncdf_vars eq vnames[req_vars], found_cnt)  
            if (fncdf[0] eq -1L) then begin      ;didn't find requested variable.
               ;Make sure you don't blank out a virtual variable that has 
               ;been defined in a master...
               vncdf = where (vir_vars.name eq vnames[req_vars], v_cnt)
               ;TJK added code to check whether vnames[req_vars] is a variable that has been altered
               ;because its original name had "invalid" characters in it - if it was altered, do
               ;not throw it out... 10/31/2000 
              table_index = where(table.equiv eq vnames[req_vars], tcount)
               if (tcount gt 0) then begin
	          ;before adding the table.varname to vnames, make sure it isn't already there...
	          already_there = where(table.varname[table_index[0]] eq vnames, already_cnt)
	          if (already_cnt eq 0) then begin
	             vnames[req_vars] = table.varname[table_index[0]] 
	             ;another name needs to be used in order to get 
	             ;the data out of the ncdf
	          endif ;TJK removed this - this shouldn't be necessary 1/29/2001
	          ; else vnames(req_vars) = ' '     ; so blank it out
	       endif
               ;No, this is not a virtual variable
               ;TJK modified if (vncdf[0] eq -1L) then begin
	       if (vncdf[0] eq -1L and tcount eq 0) then begin
	          if (DEBUG) then print,'Variable ',vnames[req_vars],$
                     ' not found in this data ncdf - throwing out.'
                  vnames[req_vars] = ' '     ; so blank it out
	       endif
            endif
         endfor ;for each requested variable
         if (DEBUG) then print, 'made it through SETUP for DATA ncdfs'
      endif ; if not a master ncdf

      real_vars = where(vnames ne ' ', rcount);
      if (rcount gt 0) then vnames = vnames[real_vars]
;print, 'DEBUGclosing cdf or netcdf ',cnames[cx]
      if ((n_elements(cnames) gt 1) and (strpos(cnames[cx],'00000000') ne -1)) then begin ;master is a cdf
        cdf_close,CDFid ; this should be the master 
      endif else begin
        ncdf_close,NCDFid ; close the open netcdf data files
      endelse
   endif 
endfor ; for each cdf(master) and netcdfs

if keyword_set(DEBUG) then print, 'end of data ncdf section ',vnames

;end TJK modifications


dhids = lonarr(n_elements(vnames)) ; create array of handle ids for data
mhids = lonarr(n_elements(vnames)) ; create array of handle ids for metadata
vvarys = strarr(n_elements(vnames)) ; initialize variable variance array
ncdftyp = strarr(n_elements(vnames)) ; initialize ncdftype array
ncdffill = strarr(n_elements(vnames)) ; initialize ncdffill array - need to be able to override the fillval for gold time_utc
; Create an array of handle ids, each handle will point to an array of 
; dependant veriables.
; Ron Yurow  (Nov 19, 2018)
dlstid = lonarr (n_elements (vnames)) 

if (rcount gt 0) then begin ; check whether there are any variables to retrieve
   ; get the data and metadata for all variables from all ncdfs
   mastervary=''
   thereisamaster = 0 ; initialize

   for cx=0,n_elements(cnames)-1 do begin
      readingmastercdf = 0      ;false, true = 1
      ; Check whether we're reading a master cdf or a data netcdf
      ; Open the appropriate type of file and inquire about global information about file
      if ((n_elements(cnames) gt 1) and (strpos(cnames[cx],'00000000') ne -1)) then begin ;file is master cdf
        CDFid = cdf_open(cnames[cx]) & cinfo = cdf_inquire(CDFid)
        readingmastercdf = 1
        thereisamaster = 1
      endif else begin
        NCDFid = ncdf_open(cnames[cx]) & cinfo = ncdf_inquire(NCDFid)
      endelse
      if keyword_set(DEBUG) then print, 'Opening file ',cnames[cx]
      ; Determining the max number of records in the file which is
      ; different for cdf vs. netcdf
      start_rec = 0L ;initialize to read the whole ncdf.
      if (readingmastercdf) then begin
         cdf_control, CDFid, SET_ZMODE=2 
         cdf_control, CDFid,  VAR=0, /Zvariable, GET_VAR_INFO=vinfo2 ; inquire more about the var
         rec_count = vinfo2.maxrecs+1 ; initialize to read all records for cdf
      endif else begin
         rec_count = ncdf_max_recsize(NCDFid) ; initialize to read all records for netcdf
      endelse

;TJK 7/21/2006 initialize some arrays to hold the extra time resolution values
      msec = make_array(2, /integer, value=0)
      usec = make_array(2, /integer, value=0)
      nsec = make_array(2, /integer, value=0)
      psec = make_array(2, /integer, value=0)

      if (keyword_set(START_MSEC)) then msec[0] = START_MSEC 
      if (keyword_set(STOP_MSEC)) then msec[1] = STOP_MSEC 

      if (keyword_set(START_USEC)) then usec[0] = START_USEC 
      if (keyword_set(STOP_USEC)) then usec[1] = STOP_USEC 

      if (keyword_set(START_NSEC)) then nsec[0] = START_NSEC 
      if (keyword_set(STOP_NSEC)) then nsec[1] = STOP_NSEC 

      if (keyword_set(START_PSEC)) then psec[0] = START_PSEC 
      if (keyword_set(STOP_PSEC)) then psec[1] = STOP_PSEC 

      if (keyword_set(TSTART) and keyword_set(TSTOP))then begin 		
         ;convert the TSTART and TSTOP to double precision numbers.
	 ;Get the epoch variable data first, determine
	 ;which records fall within the TSTART and TSTOP range.
	 start_time = 0.0D0 ; initialize
         b = size(TSTART) & c = n_elements(b)
;TJK 7/20/2006 - original code.  New code gets epoch in original epoch
;                and in epoch16 so that either can be used lower down
;                in the code...

	 if (b[c-2] eq 5) then start_time = TSTART $ ; double float already
	 else if (b[c-2] eq 7) then begin
             start_time = encode_cdfepoch(TSTART, MSEC=msec[0]) ; string

             start_time16 = encode_cdfepoch(TSTART,/EPOCH16, MSEC=msec[0], $
                                            USEC=usec[0], NSEC=nsec[0], $
                                            PSEC=psec[0]) ;string
             start_timett = encode_cdfepoch(TSTART, /TT2000, MSEC=msec[0], $
                                            USEC=usec[0], NSEC=nsec[0], $
                                            PSEC=psec[0]) ;string
            endif
	 stop_time = 0.0D0 ; initialize
	 b = size(TSTOP) & c = n_elements(b)
	 if (b[c-2] eq 5) then stop_time = TSTOP $ ; double float already
	 else if (b[c-2] eq 7) then begin
             stop_time = encode_cdfepoch(TSTOP, MSEC=msec[1]) ; string
             stop_time16 = encode_cdfepoch(TSTOP,/EPOCH16, MSEC=msec[1], $
                                            USEC=usec[1], NSEC=nsec[1], $
                                            PSEC=psec[1]) ;string
             stop_timett = encode_cdfepoch(TSTOP, /TT2000, MSEC=msec[1], $
                                            USEC=usec[1], NSEC=nsec[1], $
                                            PSEC=psec[1]) ;string

             endif
         endif

      ;end TJK TSTART and TSTOP modifications.
      vnn=0
      vn_sdat=strarr(n_elements(vnames)+40)
      if (readingmastercdf) then all_ncdf_vars = get_allvarnames(CDFid = CDFid) else all_ncdf_vars = get_allvarnames_netcdf(NCDFid = NCDFid)
      ;get the list of vars in the current NCDF.
      ; Read all of the selected variables from the open NCDF
      vx = 0 & REPEAT begin
         ;TJK check to see if the current variable exists in this NCDF.
         ;if not go to the bottom of the repeat. 5/1/98
         found = where(all_ncdf_vars eq vnames[vx], found_cnt)
	 ;if the variable isn't found try to find it w/ a different spelling - the
	 ;case that we know exists is w/ geotail orbit files, most data files and the
	 ; master ncdf have the variable Epoch, some data ncdfs have it spelled EPOCH...
	 if (found_cnt eq 0L) then begin
            new_name = ncdf_find_var(NCDFid, vnames[vx]) ; return the actual 
						   ;correct spelling of the variable
	    if (strtrim(string(new_name),2) ne '-1') then begin
               print, 'replacing vnames ',vnames[vx], ' w/ ',new_name
	       vnames[vx] = new_name
	       found_cnt = 1L
	    endif
	 endif
         if (found_cnt gt 0L) then begin  ;did find requested variable.
            ;TJK added this next section so that the vvarys array is 
            ;actually set for all of the variables. 4/98
            ;TJK 10/5/2006 only initialize the vvarys and ncdftyp to novary and ' '
            ;if this cdf is a master. Otherwise vvs info were being wiped out.
            if (strpos(cnames[cx],'00000000') ne -1) then begin  ;a master
              vvarys[vx] = 'NOVARY'
              ncdftyp[vx] = ' '
            endif
            if (num_virs gt -1) then begin
               vv = where(vnames[vx] eq vir_vars.name, vv_cnt)
               ;if this var is not virtual or looking in a master NCDF
               if ((vv_cnt le 0) or (cx eq 0)) then begin 
                  if (readingmastercdf) then begin 
                     vinfo = cdf_varinq(CDFid,vnames[vx]) 
                     vvarys[vx] = vinfo.RECVAR
                  endif else begin
                     vinfo = ncdf_varinq(NCDFid,vnames[vx]); inquire about the variable
                     vvarys[vx] = ncdf_var_recvary(NCDFid,vnames[vx]); find out whether the variable is record varying
                  endelse
                  ncdftyp[vx] = vinfo.DATATYPE
               endif
            endif else begin
               if (readingmastercdf) then begin 
                  vinfo = cdf_varinq(CDFid,vnames[vx]) 
                  vvarys[vx] = vinfo.RECVAR
               endif else begin
                  vinfo = ncdf_varinq(NCDFid,vnames[vx]); inquire about the variable
                  vvarys[vx] = ncdf_var_recvary(NCDFid,vnames[vx]); find out whether the variable is record varying
               endelse 
               ncdftyp[vx] = vinfo.DATATYPE
            endelse

            ;end of TJK mods. 4/98



            ; Read the data for the variable unless it is known to be non varying
            if (vvarys[vx] ne 'NOVARY') then begin 
               ; Determine rec_count from depend_0 of current variable; check start & stop times RTB 9/30/97
               if (keyword_set(TSTART) and keyword_set(TSTOP))then begin 		
                  ; Need to find depend_0 for the current variable
                  ;TJK - 12/20/96 added the use of the start_rec and rec_count keywords.
                  if (cx eq 0) then begin ;TJK only get the metadata from the 1st ncdf.
                     ;read this metadata from a master CDF only
                    if (readingmastercdf) then atmp=read_mymetadata(vnames[vx], CDFid) else atmp=read_mymetadata_netcdf(vnames[vx], NCDFid)
                  endif else begin ;get the already retrieved data out of the handle
                     handle_value, mhids[vx], atmp
                  endelse
                  mnames=tag_names(atmp)
                  ; If a depend0 is not defined for a variable, read 
                  ;entire ncdf (no time limits applied. RTB

                  nck=where(mnames eq 'DEPEND_0',dum)
                  if(nck[0] ne -1L) then depend0=atmp.depend_0 $
                  else begin 
                     if keyword_set(DEBUG) then print, "No depend_0 attribute, read entire ncdf"
                     start_rec = 0L
                     goto, NO_VAR_ATT
                  endelse
                  nck=where(mnames eq 'VAR_TYPE',dum)
                  if(nck[0] ne -1L) then vartype=atmp.var_type $
                  else begin 
                     if keyword_set(DEBUG) then print, "No variable attribute, read entire ncdf"
                     start_rec = 0L 
                     goto, NO_VAR_ATT 
                  endelse
                  ;RTB - 10/03/97 added code to distinguish b/w epoch and other data
                  ;read all epoch data then apply start and stop times

              ;If depend0 is virtual, skip this section since there's nothing to read                
               vv = where(depend0 eq vir_vars.name, vv_cnt)
               if (vv_cnt ne 1) then begin

                  if(depend0 ne '') then begin
                     table_index = where(table.equiv eq depend0, tcount)
                     if (tcount gt 0) then begin
                        depend0 = table.varname[table_index[0]] 
                        ;another name needs to be used 
                        ;in order to get the data out of the ncdf
                     endif

                     ;print, 'DEBUG, calling ncdf_majority_check and read_myvariable_netcdf to get depend0 epoch, line 2730' 
                     if (readingmastercdf) then begin
                       if (n_tags(atmp) gt 0) then to_column = majority_check(CDFid=CDFid,buf=atmp) else $
                          to_column = majority_check(CDFid=CDFid) 
                       epoch = read_myvariable(depend0,CDFid,vary,dtype,recs,set_column_major=to_column)
                       epoch_varname = depend0
                     endif else begin
                       if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
                          to_column = ncdf_majority_check(NCDFid=NCDFid) 
                       epoch = read_myvariable_netcdf(depend0,NCDFid,vary,dtype,recs,set_column_major=to_column)
                       epoch_varname = depend0
                     endelse

                    endif else begin ;assumes this is the epoch variable
                      if(vartype ne 'metadata') then begin
                        table_index = where(table.equiv eq vnames[vx], tcount)
                        if (tcount gt 0) then begin
                           depend0 = table.varname[table_index[0]]
                           ;another name needs to be used 
                           ;in order to get the data out of the ncdf
                           if (readingmastercdf) then begin
                              if (n_tags(atmp) gt 0) then to_column = majority_check(CDFid=CDFid,buf=atmp) else $
                                 to_column = majority_check(CDFid=CDFid) 
                              epoch = read_myvariable(depend0,CDFid,vary,dtype,recs,set_column_major=to_column)
                              epoch_varname = depend0
                           endif else begin
                              if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
                                 to_column = ncdf_majority_check(NCDFid=NCDFid) 
                              epoch = read_myvariable_netcdf(depend0,NCDFid,vary,dtype,recs,set_column_major=to_column)
                              epoch_varname = depend0
                          endelse

;                           print, 'DEBUG looking for valid epoch recs'
;                           print, 'DEBUG ',stop_timett, start_timett, epoch[0]

                        endif else begin
                           if (readingmastercdf) then begin
                             if (n_tags(atmp) gt 0) then to_column = majority_check(CDFid=CDFid,buf=atmp) else $
                                to_column = majority_check(CDFid=CDFid) 
                             epoch = read_myvariable(vnames[vx],NCDFid,vary,dtype,recs,set_column_major=to_column)
                             epoch_varname = vnames[vx]
                          endif else begin
                             if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
                                to_column = ncdf_majority_check(NCDFid=NCDFid) 
                             epoch = read_myvariable_netcdf(vnames[vx],NCDFid,vary,dtype,recs,set_column_major=to_column)
                             epoch_varname = vnames[vx]
                          endelse

                        endelse
                     endif
                   endelse
                 endif else begin ;when depend0 is virtual, need to define epoch as something. TJK 4/20/2017
                    epoch = 0 
                    recs = 0L
                    if (keyword_set(TSTART) and keyword_set(TSTOP)) then need_timeslice = 1L
                    goto, NO_VAR_ATT 
                 endelse

                  ; fix for isis ncdfs that have first element epoch = last time element epoch. RCJ 02/25/2009.
		  ; Who knows the isis data says this shouldn't have happened.  Don't know if the data providers
		  ; can fix it the data.  For now, this is the s/w fix.
                  if isis_flag then begin
                     if n_elements(epoch) gt 1 then begin
                        if epoch[0] eq epoch[n_elements(epoch)-1] then begin
                           if keyword_set(DEBUG) then print,'ISIS or ALOUETTE data. First and last elements of epoch are the same, deleting last....'
                           epoch=epoch[0:n_elements(epoch)-2]
                           recs=recs-1
                        endif
                     endif   
                  endif
		 
                  ; RCJ 11/21/2003  Added this test when tests using all=1 lead
		  ;   to error because recs was undefined.

                  ;originally set this up so that all netcdf data sets would use epoch
                  ;virtual variables, but are going to try to set GOLD up with real values,
                  ;so we don't want to use this goto.

;include PMC Turbo and FORMOSAT with GOES
;                 if ((n_elements(recs) gt 0) and strcmp('GOES',strupcase(atmp.mission_group[0]),4)) then begin
;                  if ((n_elements(recs) gt 0) and (strcmp('GOES',strupcase(atmp.mission_group[0]),4) or strcmp('FORMOSAT',strupcase(atmp.mission_group[0]),8) or strcmp('PMC TURBO',strupcase(atmp.mission_group[0]),9))) then begin
;change to NOT GOLD, so we don't have to keep adding more cases

                  if ((n_elements(recs) gt 0) and (strcmp('GOLD',strupcase(atmp.mission_group[0]),4) eq 0)) then begin

                    rec_count = 0L   
                    if (keyword_set(TSTART) and keyword_set(TSTOP)) then need_timeslice = 1L
                    goto, NO_VAR_ATT 
                  endif


                  if (n_elements(recs) gt 0) then begin ; if recs is defined
                     if (recs gt 0) then begin ; we're looking at a ncdf w/ data in it!
                        ;valid_recs = where(((epoch lt stop_time) and (epoch gt start_time)),$
		      
                        if (!version.release ge '6.2') then begin
;hopefully the tt2000 values are successfully populated in the
;read_myvariable_netcdf routine, so that we can continue on with the
;normal comparison below.

                               if keyword_set(DEBUG) then etime = systime(1)
			       case size(epoch,/tname) of

			          'LONG64': begin  ;TT2000 case
				           valid_recs=where(cdf_epoch_compare(epoch, start_timett, stop_timett), rec_count)
                                           if (rec_count ge 0) then print, '1st rec = ',valid_recs[0], 'last rec = ',valid_recs[n_elements(valid_recs)-1]
                                           if keyword_set(DEBUG) then print, 'Took ',systime(1)-etime, ' seconds to do cdf_epoch_compare'
				    	   end
			          'DCOMPLEX': begin ;Epoch16 case
				           valid_recs=where(cdf_epoch_compare(epoch, start_time16, stop_time16), rec_count)
                                           if (rec_count ge 0) then print, '1st rec = ',valid_recs[0], 'last rec = ',valid_recs[n_elements(valid_recs)-1]
                                           if keyword_set(DEBUG) then print, 'Took ',systime(1)-etime, ' seconds to do ncdf_epoch_compare'
				             end
				   else: begin
				         valid_recs = where(((epoch le stop_time) and (epoch ge start_time)), rec_count)
				        end	     
			       endcase

                        endif else begin ;original code for regular epoch value and old versions of IDL
                          valid_recs = where(((epoch le stop_time) and (epoch ge start_time)),$
                          rec_count)
                        endelse

                        if (rec_count gt 0) then begin 
                           start_rec = valid_recs[0]
;                           print, 'DEBUG Setting start_rec to valid_recs[0] ', start_rec
                        endif else begin ;read one, only because if I set rec_count to zero
                           start_rec = 0L ;we'll get ALL of the records instead of none!
                           rec_count = 0L ; for netcdf's try setting this to 0 (since there are no valid values).
                        endelse
                     endif else start_rec = 0L ;read the whole ncdf.
		  endif
               endif else begin ; if keyword set start and stop
                  start_rec = 0L ; else, get all records for this variable 
                  rec_count = 0L
               endelse
            endif else begin    ;variables don't vary
               start_rec = 0L & rec_count = 1L
            endelse
            NO_VAR_ATT:
            ; RTB end 9/30/97

            ;TJK - 02/17/98 added check for virtual variables, if a
            ;virtual variable has been requested then there isn't any
            ;data to read from the NCDF so set the data array to zero.
            ;The data will be filled in at the very bottom of this routine.
            read_flag = 1 ; set flag to true
            if (num_virs gt -1) then begin
               vv = where(vnames[vx] eq vir_vars.name, vv_cnt)
               if (vv_cnt ge 1) then begin
                  if (readingmastercdf) then begin 
                    cdf_attget,CDFid,'VAR_TYPE',vnames[vx],vartype
                  endif else begin
                    ncdf_attget,NCDFid,vnames[vx],'VAR_TYPE',vartype
                  endelse
                  if ((vartype eq 'metadata') and(vvarys[vx] eq 'NOVARY') and $
                     (cx eq 0)) then begin ;vv,novary,metadata read from master
                     read_flag = 1
                     if (debug) then print, 'reading vv, novary,non-metadata from master, ',vnames[vx]
                  endif else begin
                     read_flag = 0 ;set read flag to false
                     data = 0 ;set the data array to nothing as a place holder.
                  endelse
               endif
            endif

         if (read_flag) then begin 

           ;double check to see if we've read this variable already in this ncdf,
           ;but if its the depend_0, read it again... (because the above reads of
           ;depend_0 aren't saved into the dhids structure below)
           if (readingmastercdf) then begin ;reading a cdf master
              if (n_tags(atmp) gt 0) then to_column = 0 ;hardcode to false/row when dealing w/ netcdfs 

              data = read_myVARIABLE(vnames[vx],CDFid,$
                                     vary,dtype,recs,start_rec=start_rec, $
                                     rec_count=rec_count, debug=debug,set_column_major=to_column) ; read the data
              if keyword_set(DEBUG) then begin
                 print,'Read data for ',vnames[vx],'. Started at record ',start_rec,' and read ', rec_count, ' records.'
                 ;help, data
                 ;print, '2', dtype
              endif

;TJK 5/28/2013 for the case where no valid time values are found, put
;the valid fill into the data variables array so that nothing will be
;listed/plotted - instead of using the first records data, which is
;definitely not correct. 

              if (n_elements(valid_recs) gt 0) then begin          ;see if valid_recs is defined
                 if (valid_recs[0] eq -1 and (recs gt 0)) then begin ; if recs is defined but no epoch data was found w/i the requested time range
                    if (cdf_attexists(CDFid,'FILLVAL',vnames[vx])) then begin
                       anum = cdf_attnum(CDFid,'FILLVAL') ; find the fill value for this variable and load the data array w/ it
                       cdf_attget,CDFid,vnames[vx],'FILLVAL',vfill
                       if keyword_set(DEBUG) then print, 'For ',cnames[cx],' setting ',vnames[vx],' values to fill ',vfill, ' because no data  w/in start/stop range'
                       data_size = size(data, /struc) ; use the size of the data returned in the 1st record to set up the new array
                       if (data_size.n_dimensions eq 0 and data_size.n_elements eq 1) then begin 
                          data = vfill
                       endif else begin
                          dsize_x = where(data_size.dimensions ne 0)
                          data = make_array(dimension=data_size.dimensions[dsize_x], type=data_size.type, value=vfill)
                       endelse
;print, 'DEBUG1, do we check the fillval for the GOLD data here?  Value of vfill ',vfill
;help, data 

                    endif
                 endif
              endif

           endif else begin ;reading a data netcdf file
              if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
                 to_column = ncdf_majority_check(NCDFid=NCDFid) 

              data = read_myvariable_netcdf(vnames[vx],NCDFid,$
                                            vary,dtype,recs,start_rec=start_rec, $
                                            rec_count=rec_count, debug=debug,set_column_major=to_column) ; read the data
              ncdftyp[vx] = dtype ;11/27/2018 TJK added this here because for GOLD netcdfs we are converting charcter string times to tt2000
              ;print, 'DEBUG special for GOLD, setting ncdftyp = dtype',ncdftyp[vx]

              if keyword_set(DEBUG) then begin
                 print,'Read data for ',vnames[vx],'. Started at record ',start_rec,' and read ', rec_count, ' records.'
                 ;help, data
                 ;print,'1', dtype
              endif

              if (n_elements(valid_recs) gt 0) then begin          ;see if valid_recs is defined
                 if (valid_recs[0] eq -1 and (recs gt 0)) then begin ; if recs is defined but no epoch data was found w/i the requested time range
                    if (ncdf_attexists(NCDFid,'FILLVAL',varname=vnames[vx])) then begin
;no routine with this name
;                       anum = ncdf_attnum(NCDFid,'FILLVAL') ; find the fill value for this variable and load the data array w/ it
                       ncdf_attget,NCDFid,vnames[vx],'FILLVAL',vfill
                       if keyword_set(DEBUG) then print, 'For ',cnames[cx],' setting ',vnames[vx],' values to fill ',vfill, ' because no data  w/in start/stop range'
                       data_size = size(data, /struc) ; use the size of the data returned in the 1st record to set up the new array
                       if (data_size.n_dimensions eq 0 and data_size.n_elements eq 1) then begin 
                          data = vfill
                       endif else begin
                          dsize_x = where(data_size.dimensions ne 0)
                          data = make_array(dimension=data_size.dimensions[dsize_x], type=data_size.type, value=vfill)
                       endelse
; print, 'DEBUG2, do we check the fillval for the GOLD data here?  Value of vfill ',vfill
;help, data 

                   endif
                 endif
              endif
           endelse ;netcdf

          endif ;if read_flag

	  ; RCJ 10/22/2003 This is for cases when the variable is 'novary' in the master ncdf
	  ; but 'vary' in the data ncdfs. The type is then changed to 'novary'.
	  ; It doesn't seem to be a problem for plotting or listing in CDAWeb
	  ; but it is a problem for ncdfs created by write_myncdf.  
	  ; This was found for dataset im_k0_lena (spinsector and polarzone)
;print, 'DEBUG, checking vary for the master'
;stop;
	  if cx[0] eq 0 then begin
             ;2/17/2021 vary isn't defined for te ICON movie virtuals so lets set it here 
             ;mastervary=[mastervary,vary] 
             vary = vvarys[vx] ; setting vary here, otherwise it isn't set
	     mastervary=[mastervary,vary] 
	  endif else begin
	     ; RCJ vx+1 is because mastervary already has a first element: ''
	     if vary eq 'VARY' and mastervary[vx+1] eq 'NOVARY' then $
	        vary=mastervary[vx+1]
	  endelse
	  
         ; Flag arrays of length 1; will check later to see if these have fillval's
         ; which indicates instrument is off  RTB 9/96
         sz=size(data)
         ;print,vnames(vx)
         ;print, sz
         ; Check if data is of 0, 1 or multi-dimension and set flag
         ;  if(sz(0) ge 1) then szck=sz(1)/(sz(sz(0)+2)) else szck=sz(sz(0)+2)
         ; RTB 10/29/97 
         if(sz[0] gt 1) then szck=sz[1]/(sz[sz[0]+2]) else szck=sz[sz[0]+2]
         if(sz[0] eq 3) then  szck=sz[sz[0]]
         ;TJK 3/17/98, added check for read_flag, if its set then this is
         ;NOT a virtual variable, so go ahead w/ the check for a single
         ;record containing fill (indicates instrument is off).
         ;TJK 10/25/99 added further check to see if this NCDF is a master, if 
         ;it is then don't check for "instrument off" since most masters 
         ;don't contain any data.
         if(szck eq 1) and (vnames[vx] ne '') and (read_flag eq 1) and $
            (strpos(cnames[cx],'00000000') eq -1) then begin
            vn_sdat[vnn]=vnames[vx]
            vnn=vnn+1
            ;print, "Add to instrument off list:", vn_sdat
         endif
         ;     
         vvarys[vx] = vary  ; save the record variance flag
	 ;
	 ; RCJ 07/31/2003 commented out line below. If only 1 var is requested
	 ; and it's a vv it's ncdftype is ncdf_epoch!
         ;ncdftyp(vx) = dtype ; save the ncdf data type
	 ;
         ;TJK moved this up above No_VAR_ATT 4/16/98   endif
         ; Process the metadata of the current variable

         if (cx eq 0) then begin ; for only the first ncdf on the list 
            vvarys[vx] = vary ; set value in variable variance array
            if keyword_set(DEBUG) then print,'Reading metadata for ',vnames[vx], ' and NCDF #',cx
            ; read variable metadata

            if (readingmastercdf) then metadata = read_mymetadata(vnames[vx],CDFid) else $
               metadata = read_mymetadata_netcdf(vnames[vx],NCDFid) ; read variable metadata
            mhids[vx] = HANDLE_CREATE() & HANDLE_VALUE, mhids[vx], metadata, /SET
            ; Check metadata for ISTP depend attr's, modify other arrays accordingly
            ; Call to this procedure modified to add parameter dlstid
            ; Ron Yurow (Nov 19, 2018)
            ; follow_mydepends, metadata, vnames, vvarys, cdftyp, dhids, mhids

            follow_mydepends, metadata, vnames, vvarys, ncdftyp, dhids, mhids, dlstid
         endif

         ; Process the data of the current variable
         ;if(strpos(cnames[cx],'00000000') eq -1) OR (vvarys(vx) eq 'NOVARY') then begin
         ; RCJ 09/01 Read the variable geo_coord (from satellite ISIS) even though it is 
         ; a 'novary' variable.
	 ;TJK modified this check for mission_group (moved that check up to where we're
	 ;reading the master) and just check the flag here since its quite possible a
	 ;data ncdf wouldn't have a mission_group global attribute. 9/28/2001
         ; RCJ 11/01 Same for ISIS variable FH
	 ; RCJ 04/23/2003 Had to change the logic associated w/ isis variables.
	 ; We were getting arrays starting w/ a '0' because we had to force
	 ; these 'novary' variables to be read. Further down this first element
	 ; '0' is removed from the array.
         if (strpos(cnames[cx],'00000000') eq -1) OR (vvarys[vx] eq 'NOVARY') $
            then begin
            ; not a master skeleton or NRV data values from a master,
            ; (RCJ 09/01) but make an exception for variable geo_coord from ISIS satellite
	    if (dhids[vx] eq 0) then begin ; create handle to hold data
	       dhids[vx] = HANDLE_CREATE() 
	       if (isis_flag) then begin
		  if (vnames[vx] eq 'FH') or (vnames[vx] eq 'geo_coord') then begin
		     ; RCJ 09/04/2003 Problem when the user only asked for
		     ;'FH': at this point in the program valid_recs was undefined. 
	             ; If, for example, 'freq' and 'FH' were requested, then there was
		     ; no problem because valid_recs would have been calculated for
		     ; 'freq'. My solution was to (re)calculate valid_recs here:
		     ;
		     ; RCJ 10/21/2005  This if statement seems to be causing
		     ; problems now and I'm not getting all of the FH/geo_coord
		     ; points I need. I'm commenting it out so valid_recs will
		     ; *always* be recalculated. I did the same a few lines below,
		     ; same case.
	             ;if n_elements(valid_recs) eq 0 then begin 
                     if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
                           to_column = ncdf_majority_check(NCDFid=NCDFid) 
		        epoch = read_myvariable_netcdf('Epoch',NCDFid,vary,dtype,recs,set_column_major=to_column)
		        ; Above, we know that for FH or geo_coord data time is 'Epoch'
		        valid_recs_isis = where((epoch le stop_time) and $
			      (epoch ge start_time), rec_count)
                        if keyword_set(DEBUG) then print, 'Recalculated - Reading ', rec_count, ' records.'
	             ;endif   
		     tmpdata=0
		     ;for i=0,(n_elements(valid_recs))-1 do tmpdata=[tmpdata,data]
		     if rec_count gt 0 then begin
		        for i=0,rec_count-1 do tmpdata=[tmpdata,data]
   		        data=tmpdata[1:*]
	             endif else data=tmpdata
 		  endif   
	       endif	  
	       HANDLE_VALUE, dhids[vx], data, /SET
            endif else begin ; only append record varying data
               ;if (vvarys(vx) eq 'VARY') then begin  ; append data to previous data
               ; RCJ 09/01 Again, read the varible geo_coord even though it is a 'novary' variable.
               ; RCJ 11/01 Same for ISIS variable FH
               if (vvarys[vx] eq 'VARY') or $
               (isis_flag and (vnames[vx] eq 'geo_coord')) or $
               (isis_flag and (vnames[vx] eq 'FH')) then begin  ; append data to previous data

                  HANDLE_VALUE, dhids[vx], olddata    ; get data from previous ncdf's

                  ; Append only data when the instrument is on. RTB 10/29/97
                  ;print, "vnn=", vnn
                  ;print, vnames
                  ;print, "vn_sdat ",vn_sdat
                  ;if(vnn eq 0) then begin 
                  ;if(vn_sdat(vnn-1) ne vnames(vx)) then $ 
                  ;data = append_myDATA(data,olddata)  ; append new data to old data
                  ;endif else begin
                  ; print, vnames(vx),vnn
                  ;
                  ; RCJ 09/01 If satellite is ISIS and variable is the 3-element array geo_coord 
                  ; (one 3-element array per ncdf) we have to replicate the array
                  ; based on the number of valid_recs (or time elements) in this ncdf, so we can
                  ; have enough points to plot a graph. 
                  ; RCJ 11/01 Same for variable FH, but this is just a scalar.
                  if (isis_flag) then begin
		     if (vnames[vx] eq 'FH') or $
		        (vnames[vx] eq 'geo_coord') then begin
		        ; RCJ 09/04/2003 Problem when the user only asked for
		        ;'FH': at this point in the program valid_recs was undefined. 
	                ; If, for example, 'freq' and 'FH' were requested, then there was
		        ; no problem because valid_recs would have been calculated for
		        ; 'freq'. My solution was to (re)calculate valid_recs here:
			;
			; RCJ 10/21/2005 Commented out this if statement.
			; Reason is stated a few lines above, for the same case.
			;if n_elements(valid_recs) eq 0 then begin 

                        if (n_tags(atmp) gt 0) then to_column = ncdf_majority_check(NCDFid=NCDFid,buf=atmp) else $
                           to_column = ncdf_majority_check(NCDFid=NCDFid) 
		           epoch = read_myvariable_netcdf('Epoch',NCDFid,vary,dtype,recs,set_column_major=to_column)
		           ; Above, we know that for FH or geo_coord data time is 'Epoch'
		           valid_recs_isis = where((epoch le stop_time) and $
			         (epoch ge start_time), rec_count)
                           if keyword_set(DEBUG) then print, 'Recalculated - Reading ', rec_count, ' records.'
			;endif   

		        tmpdata=0
		        ;for i=0,(n_elements(valid_recs))-1 do tmpdata=[tmpdata,data]
		        if rec_count gt 0 then begin
		           for i=0,rec_count-1 do tmpdata=[tmpdata,data]
   		           data=tmpdata[1:*]
	                endif else data=tmpdata
		     endif   
                  endif
;RCJ look for dict_key to help identify vector arrays
		  q = where(strlowcase(tag_names(atmp)) eq 'dict_key') 
		  if q[0] eq -1 then dk='' else dk=atmp.dict_key
;TJK more generic approach to this appending of single records problem
;because we have it for 3 elements vectors as well as spectrogram
;data, e.g. THEMIS L2 ESA
                  n_dims = size(data, /dimensions)
                  handle_value, mhids[vx], cur_var
                  vector=0L

;This was setting an array of just one value to vector, which was not intended.
;                  if (n_elements(cur_var.dim_sizes) eq 1) then begin
                  if (n_elements(cur_var.dim_sizes) eq 1 and cur_var.dim_sizes[0] gt 1) then begin
                      if (cur_var.dim_sizes[0] eq n_dims[0]) then begin
                        vector = 1L ;as in a 1-d array that is just one record
                                ; like 32 element record, all fill,
                                ; that needs to be appended as another
                                ; record, not appended as 32 records 
                    endif
                endif
                  if (keyword_set(DEBUG)) then begin
                    if (vector) then print, 'append_mydata vector flag set' else print, 'append_mydata vector flag not set'
                  endif
                  data = append_myDATA(data,olddata,dict_key=dk,vector=vector)  ; append new data to old data

                  HANDLE_VALUE, dhids[vx], data ,/SET ; save back into handle

               endif
            endelse
         endif
      endif else begin
          if (keyword_set(DEBUG)) then print, 'variable ',vnames[vx], ' not found in ',cnames[cx]
      endelse
      vx = vx + 1 ; increment variable name index
      ENDREP UNTIL (vx eq n_elements(vnames))
      ;print, 'DEBUG closing cdf or netcdf ',cnames[cx]
      if ((n_elements(cnames) gt 1) and (strpos(cnames[cx],'00000000') ne -1)) then begin ;master is a cdf
        cdf_close,CDFid ; this should be the master 
      endif else begin
        ncdf_close,NCDFid ; close the open netcdf data files
      endelse
  
   endfor ; loop thru all ncdfs
     

;   if keyword_set(DEBUG) then print,'Assembling Anonymous Structure'
   ; It is possible that some of the variable names may be padded with blanks
   ; This will likely cause problems later, so trim any blanks around vnames.
      
   ;TJK took out on 3/12/01 - because the replace_bad_chars function now
   ;replaces any non-acceptable characters w/ dollars signs instead of just
   ;removing them.
   ;for i=0,n_elements(vnames)-1 do vnames[i] = strtrim(vnames[i],2)
      
   ; Retrieve the data and metadata from first handle, and append them
   ; together to create a data structure to be output from this function.
      
   HANDLE_VALUE, mhids[0], metadata, /NO_COPY  & HANDLE_FREE,mhids[0]
   if dhids[0] ne 0 then HANDLE_VALUE,dhids[0],data else data = ''
   ds = size(data) & if (ds[0] ne 0) then data = reform(temporary(data)) ; special case
   
   ;IDL 5.3 doesn't allow structure tag names that are not valid variable names,
   ;thus we need to check the vnames array for any NCDF variable names that contain
   ;special characters, e.g. badChars=['\','/','.','%','!','@','#','^','&',
   ; '*','(',')','-','+','=', '`','~','|','?','<','>']  and replace them w/ a "$"
   ; character instead... not sure what other ramifications this will have 
   ; throughout the rest of the system. TJK 4/5/2000
   for t=0, n_elements(vnames)-1 do begin
;      if keyword_set(DEBUG) then print, 'Processing table variable, ',vnames[t]
      table_index = where(table.varname eq vnames[t], tcount)
      ttable_index = where(table.equiv eq vnames[t], ttcount)
      vcount = -1 ;initialize
      if (table_index[0] eq -1) and (ttable_index[0] eq -1) then begin 
         ;add this variable to the table
 ;        if keyword_set(DEBUG) then print, 'found new variable, adding to table, ',vnames(t)
         tfree = where(table.varname eq '' or table.varname eq ' ',fcount)
         if (fcount gt 0) then begin
            table.varname[tfree[0]] = vnames[t]
         endif else begin
            print, '2, Number of variables exceeds the current size ' + $
	        'of the table structure, please increase it, current size is ...' 
            help, table.varname
            return, -1
         endelse
         table_index = where(table.varname eq vnames[t], vcount)
      endif
      if (vcount ge 0) then begin
         vnames[t] = replace_bad_chars(vnames[t], diff)
         table.equiv[table_index[0]] = vnames[t] 
         ;set equiv to either the new changed name or the original
         ;if it doesn't contain any bad chars..
      endif else begin
         if (vcount eq -1) then begin ;already set in the table, assign to what's in equiv.
            if table_index[0] ge 0 then idx = table_index[0]
            if ttable_index[0] ge 0 then idx = ttable_index[0]
            vnames[t] = table.equiv[idx]
         endif
      endelse
   endfor

   if keyword_set(NODATASTRUCT) then begin
      ; Rather than place the actual data into the megastructure, create
      ; a data handle structure field and put the data handle id in it.
      mytype = create_struct('cdftype',ncdftyp[0])  ; create .cdftype structure
      myvary = create_struct('cdfrecvary',vvarys[0]) ; create .cdfrecvary structure - TJK added 8/1/2001
; don't need to add this here anymore, adding it as part of read_mymetadata
;      mymajor= create_struct('ncdfmajor',cinfo.majority)
      mydata = create_struct('handle',dhids[0])    ; create .handle structure
      mysize = create_struct('idlsize',size(data)) ; create .idlsize structure
      mytype = create_struct(mytype,myvary)        ; append the structures - TJK added 8/1/2001
;      mytype = create_struct(mytype,mymajor)        ; append the structures
      mytype = create_struct(mytype,mysize)        ; append the structures
      mydata = create_struct(mytype,mydata)        ; append the structures
      mydata = create_struct(metadata,mydata)      ; append the metadata
      burley = create_struct(vnames[0],mydata)     ; create initial structure
   endif else begin
      ; Place the actual data into the large data structure.  This requires
      ; moving data and can take a long time with large image data arrays.
      if dhids[0] ne 0 then HANDLE_FREE,dhids[0]
      ds = size(data) & if (ds[0] ne 0) then data = reform(data) ; special case
      mytype = create_struct('cdftype',ncdftyp[0]) ; create .cdftype structure
      myvary = create_struct('cdfrecvary',vvarys[0]) ; create .cdfrecvary structure - TJK added 8/1/2001
; don't need to add this here anymore, adding it as part of read_mymetadata
;      mymajor= create_struct('ncdfmajor',cinfo.majority)
      mydata = create_struct('dat',data)          ; create .dat structure
      mytype = create_struct(mytype,myvary)       ; append the structures - TJK added 8/1/2001
;      mytype = create_struct(mytype,mymajor)        ; append the structures
      mydata = create_struct(mytype,mydata)       ; append the structures
      mydata = create_struct(metadata,mydata)     ; append the metadata
      burley = create_struct(vnames[0],mydata)    ; create initial structure
   endelse
      
   burley = correct_varname(burley, vnames, 0)

   ; If more than one variable is being processed, then retrieve the data
   ; and metadata from the handles, and append them into an anonymous struct
   ; and append these structures into a single anonymous struct for output.

   for vx = 1,n_elements(vnames)-1 do begin ; retrieve and append
      HANDLE_VALUE, mhids[vx], metadata, /NO_COPY  & HANDLE_FREE,mhids[vx]
      if dhids[vx] ne 0 then HANDLE_VALUE,dhids[vx],data else data = ''
      ds = size(data) & if (ds[0] ne 0) then data = reform(temporary(data)) ; special case
      if keyword_set(NODATASTRUCT) then begin
         ; Rather than place the actual data into the megastructure, create
         ; a data handle structure field and put the data handle id in it.
         mytype = create_struct('cdftype',ncdftyp[vx]) ; create .cdftype structure
         myvary = create_struct('cdfrecvary',vvarys[vx]) ; create .cdfrecvary structure - TJK added 8/1/2001
; don't need to add this here anymore, adding it as part of read_mymetadata
;	 mymajor= create_struct('ncdfmajor',cinfo.majority)
         mysize = create_struct('idlsize',size(data)) ; create .idlsize structure
         mydata = create_struct('handle',dhids[vx])   ; create .handle structure
         mytype = create_struct(mytype,myvary)        ; append the structures - TJK added 8/1/2001
;         mytype = create_struct(mytype,mymajor)        ; append the structures
         mytype = create_struct(mytype,mysize)        ; append the structures
         mydata = create_struct(mytype,mydata)        ; append the structures
         mydata = create_struct(metadata,mydata)      ; append the metadata
         rick   = create_struct(vnames[vx],mydata)    ; create new structure
         burley = create_struct(burley,rick)          ; create initial structure
      endif else begin
         if (dhids[vx] ne 0) then HANDLE_FREE,dhids[vx]
         mytype = create_struct('cdftype',ncdftyp[vx]) ; create .cdftype structure
         myvary = create_struct('cdfrecvary',vvarys[vx]) ; create .cdfrecvary structure - TJK added 8/1/2001
; don't need to add this here anymore, adding it as part of read_mymetadata
;	 mymajor= create_struct('ncdfmajor',cinfo.majority)
         mydata = create_struct('dat',data)           ; create .dat structure
         mytype = create_struct(mytype,myvary)        ; append the structures - TJK added 8/1/2001
;         mytype = create_struct(mytype,mymajor)        ; append the structures
         mydata = create_struct(mytype,mydata)        ; append the structures
         mydata = create_struct(metadata,mydata)      ; append the metadata
         rick   = create_struct(vnames[vx],mydata)    ; create new structure
         burley = create_struct(burley,rick)          ; append the structures
      endelse
      burley = correct_varname(burley, vnames, vx)
   endfor


   ; Check for conditions where ISTP instrument may be off; data array length of
   ; 1 and equal to the fill value. If true set structure to -1 and return
   ; error and status messages
   ;TJK changed to ne 4/29/98  wvn=where(vn_sdat eq '',wcvn)
   ikill=0
   wvn=where(vn_sdat ne '',wcvn)
   if(wcvn ne 0) then begin
      for vi=0, wcvn-1 do begin
         if(vn_sdat[vi] ne '') then begin
            ;TJK - get the tag index in the burley structure for this variable name -
            ;can't use the variable names since they sometimes contain wierd 
            ;characters like "%" in equator-s
            ttags = tag_names(burley)
            ; RCJ 11/28/00 added line below. vn_sdat still had bad characters in
            ; the variable names and the search for var_type was failing.
            vn_sdat[vi] = replace_bad_chars(vn_sdat[vi], diff)
            tindex = strtrim(string(tagindex(vn_sdat[vi],ttags)),2) ;convert to string
            comm=execute('var_type=burley.('+tindex+').var_type')
;TJK 4/24/2017 add comm eq 0, for when var_type isn't defined - which is
;common for netcdfs.
            if not(comm) then var_type='data'
            if(var_type eq 'data') then begin
               comm=execute('vfill=burley.('+tindex+').fillval')
               if(keyword_set(NODATASTRUCT)) then begin
                  comm=execute('temp=burley.('+tindex+').handle')
                  handle_value,temp,vdat 
               endif else comm=execute('vdat=burley.('+tindex+').dat')
               if(not comm) then print, 'ERROR=execute failed '
               ;TJK 4/17/98, added check for the datatype before doing
               ;the abs function test. If the data_type is byte, then the
               ;abs function cannot be applied, ie. under IDL 5.02 abs(255) is 1.
               data_size = size(vdat)
               data_type = data_size[n_elements(data_size)-2]
               ;TJK added logic to check if the data array size is still equal to
               ;just one value.  If so,then check the fill value, else get out.
               if(data_size[0] gt 1) then $
                  szck=data_size[1]/(data_size[data_size[0]+2]) else $
     	          szck=data_size[data_size[0]+2]
               if(data_size[0] eq 3) then  szck=data_size[data_size[0]]
               if(szck eq 1) then begin  ;data array has single value in it.
                  if (data_type eq 1) then begin
                                ;TJK - 3/9/2007 - comment this out, we
                                ;      really don't want to kick out
                                ;      entirely.
                     ;if (vfill eq vdat(0)) then $
   	             ;   ikill = write_fill(vn_sdat(vi), burley, tmp_str)
                  endif else begin 
                     if (data_type gt 1) then begin
                        ; RCJ 06/06/01 Commented this part out. Maybe we have to rethink
                        ; the way we handle these situations.
                        ; RCJ 02/09/2007 Found case where images from po_k0_uvi were
                        ;   requested but ncdf was small (po_k0_uvi_20070130), 
                        ;   had *one* (fill)value in place of image array.  
                        ;   Ended up here w/ a data_type of 4 (float) and 
                        ;   one (fill)value: -1.0000e+31
	                ;;print, 'detected a non byte value'
                        ;if (abs(vfill) eq abs(vdat(0))) then $
	                ;   ikill = write_fill(vn_sdat(vi), burley, tmp_str)
                     endif ;datatype isn't byte (is gt 1)
                  endelse
               endif else begin
                     ; RCJ 05/01 commented this part out. We don't want to set ikill=1 if at least 
                     ; one of the variables has good data. 
	             ;if (data_size(0) eq 1) then begin
	             ;   fidx = where(vfill eq vdat, fcnt)
	             ;   if (fcnt eq n_elements(vdat)) then begin
                     ;     ;print, 'Found single record vector w/ all fill values'
                     ;     ;print, 'Setting fill message for variable',vn_sdat(vi)
	             ;     ikill = write_fill(vn_sdat(vi), burley, tmp_str)
	             ;   endif
	             ;endif
               endelse
            endif
         endif
      endfor
   endif  
endif else begin ;TJK added check for no varibles to retrieve
   ;get some metadata out of the 1st NCDF or Master NCDF
   v_err = 'ERROR=Variable(s) not available for specified time range.'
   v_stat='STATUS=Variable(s) not available for specified time range. Re-select a different time range.'
   ; Changed method of getting the name of the data set.  The data set can be retrieved from
   ; the logical source global attribute.
   ; Ron Yurow  (March 18, 2016)
   ;slash = rstrpos(cnames[0],'/')
   ;d_set = strmid(cnames[0], slash+1, 9)
   d_set = atmp.logical_source
   d_set = 'DATASET='+strupcase(d_set)
   tmp_str=create_struct('DATASET',d_set,'ERROR',v_err,'STATUS',v_stat)
   ikill=1
endelse

if(ikill) then return, tmp_str
!quiet = quiet_flag ; restore
; Return successfull

if (keyword_set(DEBUG)) then begin
   print, 'num_virs',num_virs+1 
endif

;TJK add check in orig_names array for any variable name that might have
;bad characters in it.  Compare w/ what's been stored in the table structure.
;if (debug) then print, 'orig_names before checking ',orig_names
for i = 0, n_elements(orig_names)-1 do begin
   found = where(orig_names[i] eq table.varname, found_cnt)
   if (found_cnt eq 1) then orig_names[i] = table.equiv[found[0]]
endfor
;if (debug) then print, 'orig_names after checking ',orig_names

if not keyword_set(NOVIRTUAL) then begin
;TJK 3/26/2009 add this code to removed the un-used spots in the vir_vars
;structure arrays because I need to reverse the order of the variables
;so that in case there are virtual variables that depend on other v.v.s
;they'll likely be populated (this was specifically needed for
;wi_h4/m2_swe)

tids = where(vir_vars.flag ge 0, tcount)
if (tcount gt 0) then begin
  if keyword_set(debug) then print, 'Before ', vir_vars.name
  vir_vars2= create_struct('name',vir_vars.name[tids],'flag',vir_vars.flag[tids])
  vir_vars.name = vir_vars2.name
  vir_vars.flag = vir_vars2.flag
  if keyword_set(debug) then print, 'Remove extra virtual variables from list ', vir_vars.name
endif

;TJK add system time check to determine how long our virtual variables
;take to generate.

ttime = systime(1)

q=''  ; RCJ 03/04/2010  This test was triggered by a case where the only 'data' var 
; in the structure was 'thrown out' somewhere in the code above.
; var was thg_ask_nrsq, date was Nov/2007, for which we don't have data.

for i=0,n_tags(burley)-1 do q=[q,burley.(i).var_type]
;reuse q :
q=where(strlowcase(q) eq 'data')
if q[0] eq -1 then begin
   d_set='DATASET='+strupcase(atmp.logical_source)
   v_err="ERROR=No var type 'data' in structure" 
   v_stat='STATUS= Data not available'
   tmp_str=create_struct('DATASET',d_set,'ERROR',v_err,'STATUS',v_stat)
   return, tmp_str
endif

for i = 0, num_virs do begin
   vtags = tag_names(burley)
   ;vindex = tagindex(vir_vars.name[i], vtags) ; find the VV index number
   vindex = tagindex(replace_bad_chars(vir_vars.name[i],diff), vtags) ; find the VV index number
   if (vindex[0] ge 0) then begin
      vartags = tag_names(burley.(vindex))
;      findex = tagindex('FUNCTION', vartags) ; find the FUNCTION index number
      findex = tagindex('FUNCT', vartags) ; find the FUNCT index number
      if (findex[0] ne -1) then begin ;found a virtual value w/ a function definition
         if keyword_set(DEBUG) then print,'VV function being called ',$
            strlowcase(burley.(vindex).(findex)), ' for variable ',vir_vars.name[i]

         case (strlowcase(burley.(vindex).(findex))) of
         'crop_image': begin
                          burley=crop_image(temporary(burley),orig_names,index=vindex)
   		       end   
         'alternate_view': begin
                              burley = alternate_view(temporary(burley),orig_names)
                           end
         'clamp_to_zero': begin
                              burley = clamp_to_zero(temporary(burley),orig_names,index=vindex)
                           end
         'composite_tbl': begin
                              burley = composite_tbl(temporary(burley),orig_names,index=vindex)
                           end
         'arr_slice':  begin
                          burley = arr_slice (temporary(burley), orig_names, index=vindex)
                       end
         'conv_pos': begin
	                ; RCJ 11/21/2003  Added 'index=vindex'. It is necessary if all=1
                        burley = conv_pos(temporary(burley),orig_names,$
                           tstart=start_time, tstop=stop_time,index=vindex)
                     end
         'conv_pos_hungarian': begin
                        burley = conv_pos_hungarian(temporary(burley),orig_names,index=vindex)
                     end
         'conv_pos1': begin
                         burley = conv_pos(temporary(burley),orig_names,$
                            tstart=start_time, tstop=stop_time, $
                            COORD="ANG-GSE",INDEX=vindex)
                      end
         'conv_pos2': begin
                         burley = conv_pos(temporary(burley),orig_names,$
                            tstart=start_time, tstop=stop_time, $
                            COORD="SYN-GEO",INDEX=vindex)
                      end
         'conv_map_image': begin
                              burley = conv_map_image(temporary(burley),orig_names)
                           end
         'calc_p': begin
                      burley = calc_p(temporary(burley),orig_names,INDEX=vindex)
                   end
         'create_vis': begin
                          burley = create_vis(temporary(burley),orig_names)
                       end
         'create_plain_vis': begin
                                burley = create_plain_vis(temporary(burley),orig_names)
                             end
         'create_plmap_vis': begin
                                burley = create_plmap_vis(temporary(burley),orig_names)
                             end
         'apply_qflag': begin
                           burley = apply_qflag(temporary(burley),orig_names,index=vindex)
                        end
         'apply_rtn_qflag': begin
                           burley = apply_rtn_qflag(temporary(burley),orig_names,index=vindex)
                        end
         'apply_rtn_cadence': begin
                           burley = apply_rtn_cadence(temporary(burley),orig_names,index=vindex)
                        end
         'region_filt': begin
                           burley = region_filt(temporary(burley),orig_names,index=vindex)
                        end
         'convert_log10': begin
                             burley = convert_log10(temporary(burley),orig_names)
                          end
         'add_51s': begin ;for po_h2_uvi
                       burley = Add_seconds(temporary(burley),orig_names,index=vindex,seconds=51)
                    end
         'add_1800': begin ;for omni
                       burley = Add_seconds(temporary(burley),orig_names,index=vindex,seconds=1800)
                    end
         'comp_themis_epoch': begin ;for computing THEMIS epoch
                       burley = comp_themis_epoch(temporary(burley),orig_names,index=vindex)
                    end
         'comp_icon_epoch': begin ;for computing ICON epoch
                       burley = comp_themis_epoch(temporary(burley),orig_names,index=vindex, msec=1L)
                    end
         'comp_aim_epoch': begin ;for computing AIM epoch
                       burley = comp_aim_epoch(temporary(burley),orig_names,index=vindex)
                    end
         'comp_themis_epoch16': begin ;for computing THEMIS epoch
                       burley = comp_themis_epoch(temporary(burley),orig_names,index=vindex,/sixteen)
                    end
         'apply_filter_flag': begin ; filter out values based on COMPUTE_VAL and COMPUTE_OPERATOR
                       burley = apply_filter_flag(temporary(burley),orig_names,index=vindex)
                    end
         'apply_esa_qflag': begin
                       burley = apply_esa_qflag(temporary(burley),orig_names,index=vindex)
                    end
         'apply_fgm_qflag': begin ;use the esa function
                       burley = apply_esa_qflag(temporary(burley),orig_names,index=vindex)
                    end
         'apply_gmom_qflag': begin ;use the esa function
                       burley = apply_esa_qflag(temporary(burley),orig_names,index=vindex)
                    end
         'compute_magnitude': begin
                       burley = compute_magnitude(temporary(burley),orig_names,index=vindex)
                    end
         'height_isis': begin
                       burley = height_isis(temporary(burley),orig_names,index=vindex)
                    end
         'flip_image': begin
                       burley = flip_image(temporary(burley),orig_names,index=vindex)
                    end
         'wind_plot': begin
                         burley = wind_plot(temporary(burley),orig_names,index=vindex)
                      end
         'error_bar_array': begin
                           burley=error_bar_array(temporary(burley), $
			                          index=vindex,value=0.02)
   		       end   
         'convert_toev': begin
                           burley=convert_toev(temporary(burley), orig_names, index=vindex)
                       end
         'convert_ni': begin
                           burley=convert_Ni(temporary(burley), orig_names, index=vindex)
                        end
         'correct_fast_by': begin
                           burley = correct_FAST_By(temporary(burley),orig_names,index=vindex)
                        end
         'compute_cadence': begin
                           burley = compute_cadence(temporary(burley),orig_names,index=vindex)
                        end
         'extract_array': begin
                           burley = extract_array(temporary(burley),orig_names,index=vindex)
                        end
         'expand_wave_data': begin
                           burley = expand_wave_data(temporary(burley),orig_names,index=vindex)
                        end
         'make_stack_array': begin
                           burley = make_stack_array(temporary(burley),orig_names,index=vindex)
                        end
         'fix_sparse': begin
                           burley = fix_sparse(temporary(burley),orig_names,index=vindex)
                        end
;6/10/2021 add more virtual functions
        'spdf_compute_mean': begin
                           burley=spdf_compute_mean(temporary(burley), orig_names, index=vindex)
                        end
         'spdf_3d_to_2d_avg_over_col': begin
                   burley =spdf_3d_to_2d_avg(temporary(burley),orig_names,index=vindex,/avg_over_col)
                end
         'spdf_3d_to_2d_avg_over_row': begin
                   burley =spdf_3d_to_2d_avg(temporary(burley),orig_names,index=vindex,/avg_over_row)
                end
         'spdf_sum_over_row': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/sum_over_row)
                end
         'spdf_sum_over_col': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/sum_over_col)
                end
         'spdf_sum_over_col_row': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/sum_col_row)
                end
         'spdf_sum_over_col_z': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/sum_col_z)
                end
         'spdf_sum_over_row_z': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/sum_row_z)
                end
         'spdf_avg_over_row': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/avg_over_row)
                end
         'spdf_avg_over_col': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/avg_over_col)
                end
         'spdf_avg_over_col_row': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/avg_col_row)
                end
         'spdf_avg_over_col_z': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/avg_col_z)
                end
         'spdf_avg_over_row_z': begin
                   burley =spdf_sum_avg_over_col_row_z(temporary(burley),orig_names,index=vindex,/avg_row_z)
                end

         else : print, 'WARNING= No function for:', vtags[vindex]
         endcase
      endif ;if function defined for this virtual variable    
   endif ;found the tag index for this virtual variable
endfor ; for number of virtual variables
 if keyword_set(DEBUG) then print, 'read_mynetcdf took ',systime(1)-ttime, ' seconds to generate VVs.'
endif ;no virtual variable population 

;Add a check for variables that have var_type of data, but that the user didn't request.
;This has just recently become an issue because we want to allow plotting of variables
;that are defined as depends for other variables, e.g. ge_k0_epi.  TJK 11/22/2000
var_stat = 0

;TJK 1/26/01 - add if statement because if one of the virtual variable 
;functions has trouble running (doesn't find any data), burley will be
; equal to -1, then check_myvartype fails...  so basically check to see
;if burley is a structure, by asking how many tags it has, if its not a
;structure, n_tags returns 0

if (n_tags(burley) ne 0) then begin
   var_stat = check_myvartype(burley, orig_names)
   if (var_stat ne 0) then print, 'READ_MYNETCDF, no data to plot/list.'
   ; RCJ 01/14/2013  Add keyword 'all' to call to merge_metadata:
   burley = merge_metadata_netcdf(cnames, burley, all=all)
endif
;TJK 10/25/2006 - if THEMIS data then epoch values had to be computed
;                 (all virtual), thus time subsetting wasn't possible
;                 above, do it here by calling timeslice_mystruct
if (need_timeslice) then begin

    burley = timeslice_mystruct(temporary(burley), start_time16, stop_time16,$
       START_MSEC=START_MSEC, STOP_MSEC=STOP_MSEC, START_USEC=START_USEC, $ 
       STOP_USEC=STOP_USEC, START_NSEC=START_NSEC, STOP_NSEC=STOP_NSEC, $
       START_PSEC=START_PSEC, STOP_PSEC=STOP_PSEC)
endif

Return, burley
end


