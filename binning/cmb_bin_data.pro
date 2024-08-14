;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_bin_data,d,dt_sec= dt_sec,dates=dates, sigmul=sigmul,vars=vars,multiple_modes=multiple_modes $
                     ,cdfepochtype=cdfepochtype,diagnostic=diagnostic,OVERRIDE_DEFAULT_BINNING=override_default_binning $
                     ,nointerpolation=nointerpolation

;+
; EXAMPLE USAGE:
;   status= cmb_bin_data( d,dates=[start,stop],dt_sec = dt_sec)
;
; NAME:
;   cmb_bin_data
;            
; PURPOSE:   
; This procedure creates uniformly spaced binned data for the specified variables and data set.
; Bins containing no data are interpolated using nearest neighbor bins that contain data.
; It then creates these variables and their metadata on the calling level of your current IDL session.
;
; CATEGORY:
; Data binning.
;
; CALLING SEQUENCE:                                   
;  status= cmb_bin_data( d, d,dt_sec= dt_sec,dates=dates,sigmul=sigmul,vars=vars,multiple_modes=multiple_modes,cdfepochtype=cdfepochtype)
;                                                     
; INPUTS:                                             
; d = data structure returned by read_mycdf.pro
;
; Keyword Inputs:
;   dates: string of start/stop times; e.g. start='2014/01/20 00:00:00.000',  stop='2014/01/20 03:00:00.000'
;   dt_sec: time interval in seconds of the time bin width, in double precision.
;   sigmul: if set and sigmul >= 1, sigmul is the multiplicative factor of standard deviation for rejection of data: 
;           5 (default),  4 (less aggressive), 6 (more aggressive). Note is sigmul < 1 or not defined filtering is skipped.
;   multiple_modes =1 (default) then set the non-dominant modes to fill
;                  =0 interpolate all modes to the dominant mode 
;           this keyword was introduced because some multi dependency variables are composed of multiple modes, 
;           an example of possible mixture of modes is ion flux as function of time and energy where energy is time dependent.
;   vars: list of cdf variable names,  i.e. vars = ['Magnitude', 'BRTN']
;       Note: cdf variable names are case sensitive.
;       The default output variable name is the cdf variable name.
;       To rename output variables and/or break them into components use following syntax:
;       vars =['cdfvariablename1=var','cdfvariablename2=var1,var2,var3']
;       the later breaks 'cdfvariablename2' into components named 'var1','var2','var3',
;       Note: the number of specified output components must equal the number of components for that cdf variable.
;       For example vars = ['Magnitude=B0', 'BRTN=Bvec'],
;       would create output variables named 'B0' and 'BVEC'.
;       For example vars = ['Magnitude=B0', 'BRTN=Bx,By,Bz'],
;       would create output variables named 'B0' and 'Bx','By','Bz' (components) instead of 'Magnitude' and 'BRTN' (vector).
;   nointerpolation =1 (leave empty bins empty), 0 (DEFAULT,linear interpolate using non-empty neighboring bins)
;
; OUTPUTS:
;   1 is successful
;   0 if not, original unbinned values returned.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   Unknown.
;
; RESTRICTIONS:
;   Unknown.
;
; FUNCTION:
;
; MODIFICATION HISTORY:
;   Code developed by Aaron Roberts and Scott Boardsen at GSFC.
; NOTES:
; Only bins data if attributes var_type ='data' and if allow_bin is defined, it must not equal allow_bin='FALSE'. 
;-
; Catch statement to help with error handling.
; All errors result in returning a status of 0.
; Ron Yurow (Jan 20, 2021)
CATCH, err_status

IF  err_status NE 0 THEN BEGIN
    PRINT, 'Error index: ', err_status
    PRINT, 'Error message: ', !ERROR_STATE.MSG
    RETURN, 0
ENDIF


if cmb_var_type(d) ne 'STRUCT' then return,0
if n_elements(dates) ne 2 then return,0
if keyword_set(nointerpolation) then  fill_empty_bins=0 else fill_empty_bins=1
if n_elements(dt_sec) eq 0 then dt_sec = 0d0 ; seconds, if not set do not bin
if n_elements(sigmul) eq 0 then sigmul=0
; Force override_default_binning keyword to 0 if not passed.  Ron Yurow (2/3/20)
if n_elements(override_default_binning) eq 0 then override_default_binning = 0;
if sigmul ge 1. then autobad = 1

if dt_sec le 0 and sigmul ge 1. then begin
   cmb_spike_editor0,d,sigmul
end
if dt_sec le 0 then return,0
;if n_elements(cdfepochtype) eq 0 then cdfepochtype='CDF_EPOCH'
depend0 = cmb_cdf_get_depend0(d) 
if n_elements(cdfepochtype) eq 0 then cdfepochtype=depend0[0].cdftype
if cdfepochtype eq 'CDF_EPOCH16' then cdfepochtype = 'CDF_TIME_TT2000'
dtbin = cmb_dtbin( dt_sec, cdfepochtype=cdfepochtype)

time_name = 'EPOCH_BIN'
av = cmb_cdf2user_var(vars,d)
; Check to make sure that a least one variable was found that can be processed
; If there were no binnable data variables available, then exit now. Ron Yurow
if  size (av, /type) ne 8 then return, 0 
tnames = tag_names(d)
nvars = n_elements(av.cdfvar)
epr = cmb_string2epr(dates,cdfepochtypeout=cdfepochtype) ;start/stop times in epoch
tbeg = epr[0]
tend = epr[1]
diagnostic={dtbin:dtbin,tbeg:tbeg, tend:tend}

; Set up some variables 
; Ron Yurow (Nov 3, 021)
ii = 0                  ; index of variable being processed in the data structure.
prepass = 1             ; true during the pre-processing phase.
submode_rcrds = LIST () ; LIST minor mode index vectors sorted by order or depend0
                        ; variable names in the depend_0_name array.
depend_0_name = []      ; array of names of variables that are the epoch (depend0) 
                        ; for a variable with a time variant depend1/depend2 
                        ; (IE one that has multiple modes)
finish = 0              ; true at processing completion

; Change the loop structure to allow for a pre-pass phase where minor mode record
; indices are calculated from variables that have a time variant depend1/depend2
; and a second pass where all remaining processing is completed.   
; Ron Yurow (Nov 3, 2021) 
; for ii = 0, nvars-1 do begin
WHILE (~finish) DO BEGIN 
    print,'----------------------------------------------------' 
    ip = where( tnames eq strupcase(av.cdfvar[ii]) ) & if n_elements(ip) ne 1 then stop & ip = ip[0] ;bug fixed SAB 6/5/2014
    
    if ip eq -1 then goto, skipthisvar
    ; Added check to the keyword override_default_binning before responding to the ALLOW_BIN attribute Ron Yurow (02/03/20)
    if cmb_tag_name_exists('ALLOW_BIN',d.(ip)) and (keyword_set(override_default_binning) eq 0) then begin ;SAB 4/6/2016
       if d.(ip).allow_bin eq 'FALSE' then BEGIN
          print, 'ALLOW_BIN = FALSE, variable:', d.(ip).varname, ' not binned'
          if cmb_tag_name_exists('units',d.(ip)) then  $
                      d.(ip).units = d.(ip).units + ' (not binned)'
          goto, skipthisvar 
       ENDIF
    endif 
    iep=where( strlowcase(d.(ip).depend_0) eq strlowcase(tnames)) & iep=iep[0]
    VALIDMIN = 0
    VALIDMAX = 0
    if cmb_tag_name_exists('VALIDMIN',d.(ip)) and cmb_tag_name_exists('VALIDMAX',d.(ip)) then begin
       VALIDMIN = d.(ip).VALIDMIN
       VALIDMAX = d.(ip).VALIDMAX
    endif
    ;print,'name,valid min/max:', d.(0).varname, validmin, validmax ; for diagnostic
    if iep eq -1 then begin
       print,'CDF variable ' + av.cdfvar[ii] + ' is not dependent on time, skipped.'
       goto,skipthisvar
    endif
    ; set mode to the either the empty array (for prepass/normal processing) or the array of indexes 
    ; of submodes (if available)
    ; Ron Yurow (Nov 3, 2921)
    mode = []

    ; If we are in main processing phase then check if this variable has an epoch that we
    ; minor mode record indices for.  If it does, then set mode to correct array of indices
    ; for its epoch.
    ; Ron Yurow  (Nov 3, 2021)
    if (~prepass) then begin
        if  cmb_tag_name_exists ('depend_0',d.(ip)) && strlen (d.(ip).depend_0) gt 0 then begin
            dpnd_0_ind = where (d.(ip).depend_0 eq depend_0_name, found)
            if  (found gt 0) then begin
                mode = submode_rcrds [dpnd_0_ind [0], *]
            endif
        endif
    endif

    ; Added extra keywords to cmb_cdf_check_if_dependencies_are_time_varying to allow preprocessing
    ; and reformating.
    ; Ron Yurow (Nov 3, 2201)
    ; cmb_cdf_check_if_dependencies_are_time_varying,d, ip, multiple_modes=multiple_modes
    cmb_cdf_check_if_dependencies_are_time_varying,d, ip, multiple_modes=multiple_modes,mode=mode,prepass=prepass
    
    ; Check if we in the pre-pass.  If we are and if cmb_cdf_check_if_dependencies_are_time_varying
    ; returned an array minor mode record indices, then store those indices along with the accompanying 
    ; epoch.  In all cases, we are done with pre-processing and move on to the next variable.
    ; Ron Yurow (Nov 3, 2021)
    if  (prepass) then begin
        if  n_elements (mode) gt 0 then begin
            if  cmb_tag_name_exists ('depend_0',d.(ip)) && strlen (d.(ip).depend_0) gt 0 then begin
                dpnd0 = d.(ip).depend_0
                sink = where (dpnd0 eq depend_0_name, found)
                found = (n_elements (depend_0_name) gt 0)? found : 0
                if  (found eq 0) then begin
                    depend_0_name = [depend_0_name, dpnd0]
                    submode_rcrds.Add, mode
                endif
            endif
         endif
        goto, skipthisvar
    endif
 ;add by SAB 4/17/2014
    other_depend = cmb_cdf_get_dependencies(d, ip, /not_zero, level=level) & if other_depend[0] ne '' then other=', another dependency is ' + other_depend else other=''
    print,'CDF VARIABLE ', av.cdfvar[ii], ', now called ',av.uservar[ii],' whose independent variable is ', time_name, other
    print,'CREATED VARIABLE: ', cmb_var_label(av.uservar[ii], other_depend, time_name)
    note =  cmb_var_label(av.uservar[ii], other_depend, time_name)
    t_d = cmb_epoch_modify( cmb_dat(d.(iep)),cdfepochtypeout=cdfepochtype)
;    help,cdfepochtype, t_d,cmb_dat(d.(iep)) &stop   
    print,'Depend_0', tnames[iep]
;    help,t_d,cdfepochtype
    if dtbin le 0 then print,'Note data is not binned in time, original time dependency of ', av.cdfvar[ii],' is ',tnames[iep]
    values = cmb_dat(d.(ip))
    fillflag= d.(ip).fillval
    fillflag0= d.(ip).fillval ; SAB 2019/06/17
    ;if finite(fillflag) eq 0 then fillflag = cmb_valid_data_range(d,ip) ; SAB 2019/06/17       
    if finite(fillflag) eq 0 then begin  ; SAB 2019/06/17
       fillflag = -1e31
       jj = cmb_fillval(values,fillflag0, /ieq)
       if jj[0] ne -1 then values[jj] = fillflag
    endif
    ibreak = cmb_break_decision(av,ii,size(cmb_dat(d.(ip)),/structure))
    if ibreak eq -1 then goto,skipthisvar    
    if (ibreak) then begin ;break matrix in components
		   vname0 = cmb_var_name_components(av.uservar[ii])
		   for jj = 0, av.nc[ii]-1 do begin
		vname = vname0[jj]
		value = reform(values[jj,*])
		if n_elements(value) eq  1 then goto,skipthisvar
      ; Modified to pass the fillflag0 specially for use as the fill value for
      ; periods where the sample variance can not be calculated.  Ron Yurow (08/08/2019)
		cmb_timebin_array,t_d, value, tbeg, tend, dtbin, $
			 t_out, valueout, fillflag, valueoutuncertaintyofmean, $
			 serout_flag=valueout_nbin, fill_empty_bins=fill_empty_bins, $
			 autobad=autobad, sigmul=sigmul, fillflag0, $
			 VALIDMIN=VALIDMIN, VALIDMAX=VALIDMAX
		   endfor
    endif else begin ;don't break into componets
		vname = av.uservar[ii]
		if n_elements(values) eq  1 then goto, skipthisvar
      ; Modified to pass the fillflag0 specially for use as the fill value for
      ; periods where the sample variance can not be calculated.  Ron Yurow (08/08/2019)
		cmb_timebin_array,t_d, values, tbeg, tend, dtbin, $
			 t_out, valueout ,fillflag, valueoutuncertaintyofmean, $
			 serout_flag=valueout_nbin, fill_empty_bins=fill_empty_bins, $
			 autobad=autobad, sigmul=sigmul, fillflag0, $
			 VALIDMIN=VALIDMIN, VALIDMAX=VALIDMAX
    endelse
    
    if finite(fillflag0) eq 0 then begin  ; SAB 2019/06/17
       ; reset fillflag to intial value
       jj = cmb_fillval(valueout,fillflag, /ieq)
       if jj[0] ne -1 then valueout[jj] = fillflag0
    endif    
    
    if n_elements(d0) eq 0 then d0 = create_struct(time_name, cmb_epoch_modify(t_out,cdfepochtypeout=depend0[0].cdftype) )
    ;cmb_depend_0_modify,d0,d,ip,t_out
    d0 = create_struct(d0, vname, valueout, vname+'_NBIN', valueout_nbin)
    d0 = create_struct(d0, vname + '_bin_DELTA_MINUS_VAR', $
            valueoutuncertaintyofmean, $
            vname + '_bin_DELTA_PLUS_VAR', $
            valueoutuncertaintyofmean ) 
    if cmb_tag_name_exists('epoch', d0) then stop
    skipthisvar:
      ; Move to the next variable.  If we are in the pre-process phase and we have
      ; looked at all available variables, then move to normal processing phase.
      ; If we have processed all variables then exit the loop.
      ; Ron Yurow (Nov 3, 2021)
      ii++
      if  (prepass) then begin
          if  (ii eq nvars) then begin
              prepass = 0
              ii = 0
              ; mode=[] ; <<<< only for testing
          endif
      endif else begin
          if  (ii eq nvars) then finish = 1
      endelse
; Change loop closure to match loop type
; Ron Yurow (Nov 2021)
; endfor ;ii
ENDWHILE
print,'----------------------------------------------------'

print,'Created time variable ' + time_name
if cmb_var_type(d0) ne 'STRUCT' then return,1
d0 = create_struct(d0,'aux', create_struct('time_bin_width_sec',dt_sec, 'epr', epr))
;set time dependent dependencies to that of the dominant instrument mode
for ii = 0, nvars-1 do begin ;add by SAB 4/17/2014
    ip= where( strpos(tnames, strupcase(av.cdfvar[ii]) ) ne -1 ) & ip=ip[0] ;add by SAB 4/17/2014
    if ip[0] ne -1 then cmb_cdf_check_if_dependencies_are_time_varying,d, ip,/set_depend_to_dominant_mode ;add by SAB 4/17/2014
endfor ;add by SAB 4/17/2014

dummy = CMB_CDF_GET_DEPENDENCIES(d,/write,/not_zero, d0=d0) ;  time dependent dependencies for variables that where binned.
; save,d,d0 ; note comment out after testing cmb_updatestructwith_epoch_bin
;cmb_check_delta, d, d0 ; not need SAB 2018/09/26
cmb_cdf_updatedatastructure,d,d0 ; overwrite input handles with handles pointing to the binned data
cmb_add_dependencies_of_binned_data, d, d0 ; SAB 2018/09/26
;cmb_fix_data_dependencies,d,d0 ; superseeded by cmb_add_dependencies_of_binned_data, SAB 2018/09/26
;d = cmb_updatestructwith_epoch_bin(d,d0) ; replace the depend_0 values to 'EPOCH_BIN' for variables that where binned.
;cmb_meta_validate,d  ;fixes depend_0 metadata
;cmb_cdf_nbin_var_type,d ;change new _bin variables to support data.
if cmb_var_type(d0) eq 'STRUCT' then return, 1 else return,0 ; SAB 2018 1/9
;return,1
end
