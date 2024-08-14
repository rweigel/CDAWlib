;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
pro cmb_setemptybin2fill,b,b_nbin,fillval
ii=where( b_nbin eq 0) 
if ii[0] ne -1 then b[ii] = fillval
print,'in cmb_setemptybin2fill fillval=',fillval
end


pro cmb_fill_array,b,xb,b_nbin
; fills empty bins by linear interpolating across them.
; input/output
; b[nc,nt]  a two dimensional array of binned data, trailing dimesnion is time, if scaler b[1,nt]
; xb[nt] is center time of the binned data
; exptrapoled bin values set to value of closest valid bin value
; b_nbin[nc,nt] is the number of data points used in each bin to binning.
ii=where(b_nbin eq 0)
if ii[0] eq -1 then return
si0 = size(b,/structure)
ndims = si0.n_dimensions
;help,si0,/str
if ndims eq 1 then begin
        iz = where(b_nbin eq 0) ;find bins with no data
	iv = where(b_nbin ne 0)
	if iz[0] eq -1 or n_elements(iv) lt 2 then return
	y = b
	yz = interpol(b[iv],xb[iv],xb[iz])
	y[iz] =yz
	b[*]=y
	xr = [min(xb[iv],i0),max(xb[iv],i1)]
	i=where(xb lt xr[0]) & if i[0] ne -1 then b[i]= b[iv[i0]]
	i=where(xb gt xr[1]) & if i[0] ne -1 then b[i]= b[iv[i1]] 
        return
endif else begin
    for ic = 0l,si0.dimensions[0]-1 do begin   
	iz = where(b_nbin[ic,*] eq 0) ;find bins with no data
	iv = where(b_nbin[ic,*] ne 0)
	if iz[0] eq -1 or n_elements(iv) lt 2 then goto, next
	y = b[ic,*]
	yz = interpol(b[ic,iv],xb[iv],xb[iz])
	y[iz] =yz
	b[ic,*]=y
	xr = [min(xb[iv],i0),max(xb[iv],i1)]
	i=where(xb lt xr[0]) & if i[0] ne -1 then b[ic,i]= b[ic,iv[i0]]
	i=where(xb gt xr[1]) & if i[0] ne -1 then b[ic,i]= b[ic,iv[i1]] 
	next:
    endfor
endelse
end

pro cmb_timebin_array,tin00,serin0,t0,tf,dtbin $
    ,tout,serout,fillval, seroutuncertainty, missing_variance_flag $
    ,serout_flag=serout_flag, fill_empty_bins=fill_empty_bins $
    ,autobad=autobad,sigmul=sigmul $
   ,VALIDMIN=VALIDMIN, VALIDMAX=VALIDMAX
;input 
; t0-start time of binning
; tf-end time of binning
; dtbin time increment
; note: t0, tf, dtbin, tin0 must be in the same time units, i.e. Julian days, NSSDC epoch
; serin0  input time series, series of scalars, vectors or  matrices of serin0 = serin0[ .,.,.,nt] nt is the number of timesteps in series
; fillval - fill value
;output
; tout - centered times of binned data ; tout will be in the same time units as tin.
; serout - averaged binned timeseries
; seroutuncertainty - standard deviaton in each bin
; missing_variance_flag - value to indicate a missing sample variance. RY (08/08/2019)
; serout_flag=serout_flag -number of samples per bin

a = serin0
;print,'min/max of a', minmax(a,fill=fillval) ;diagnostic
tin0 = tin00
npts = n_elements(tin0)
;help, fillval, validmin, validmax
cmb_filter_time_series,a,fillval=fillval, validmax=validmax, validmin=validmin ; set values outside of valdimin/max to fillval
;print, 'cmb_timebin_array' & help, a,fillval,validmin,validmax
ii=where( finite(a) eq 0 ) & if ii[0] ne -1 then a[ii]=fillval

if keyword_set(autobad) then begin
   a = cmb_collapse(a, ndimsorg = ndimsorg0)
   ig = cmb_autobad(a,sigmul,fill=fillval, /filter_by_fill)
   ii=where(ig eq 0)
   if ii[0] ne -1 then a[ii]=fillval
   a = cmb_collapse(a, ndimsorg = ndimsorg0,/revert)
endif

if dtbin le 0 then begin ;don't bin data, return full resolution
   tout = tin0
   serout = a
   serout_flag = ''
   return
endif

dt = dtbin
tout = cmb_timesofbin(t0,tf,dt)
ntb = n_elements(tout)

; below if added SAB 6/19/2017
if npts eq 1 then BEGIN ; Ron Yurow pointed out an error is only 1 time instance in timeseries
    print,'***** WARNING ONLY 1 TIME STEP ******'
	serout = serin0 + intarr(n_elements(tout) )
	serout_flag= serout*0
	i = where( tin0 ge tout[0:ntb-2] and tin0 lt tout[1:*])
	if i[0] ne -1 then  serout_flag[i] = 1
	Return 
endif


si0 = size(a,/structure)
if si0.n_dimensions ne 1 then begin ;matrix scalar timeseries
    ndin = si0.dimensions[0:si0.n_dimensions-1]
    nt = ndin[si0.n_dimensions-1] ;time steps
    na = 1l & for i=0,si0.n_dimensions-2 do na = na*ndin[i] ;number of elements in matrix at a timestep
    nd1 = [na,nt]
    a = reform(a,nd1,/overwrite) ; collaspe matrix times series into vector series
    sia = size(a,/structure)
    nc = sia.dimensions[0]
    ndout = ndin & ndout[si0.n_dimensions-1] = ntb ; form internal dimensions of output array
    b = dblarr(na,ntb)*a[0] ;force variable type of b to be the same as a.
endif else begin ;scalar time series
    a = transpose(a)
    b = transpose(dblarr(ntb)*a[0])
    ndout = ntb
    nc=1
endelse
b_nbin = b
b2 = b ;*1d0 ;SAB 9/25/2017 compute uncertainty
iabmap = floor((tin0-t0)/dt)

bin_variance = b

;convert matrix time series into vector time series
for ic = 0l, nc-1 do begin
  ;ig = reform(where( abs(a[ic,*]) lt 0.99*abs(fillval)))
  ig = reform(where( abs(a[ic,*]) ne abs(fillval)))
  ;help,ig & stop
  if ig[0] ne -1 then begin
      ;  This variables are used for the calculation of the variance using 
      ; Welford method
      ; Ron Yurow (July 24, 2018)
      m =    DBLARR (ntb)
      s =    DBLARR (ntb)
      prev = DBLARR (ntb)

      for jpt=0l,n_elements(ig)-1 do begin
		ipt = ig[jpt]
		if iabmap[ipt] ge 0 and iabmap[ipt] lt ntb then begin
		b[ic,iabmap[ipt]]=  b[ic,iabmap[ipt]] + a[ic,ipt]
        ; This method is no longer used for computing variance
        ; Ron Yurow (July 24, 2018)
		; b2[ic,iabmap[ipt]]=  b2[ic,iabmap[ipt]] + a[ic,ipt]^2  ;SAB 9/25/2017 compute uncertainty
		b_nbin[ic,iabmap[ipt]] = b_nbin[ic,iabmap[ipt]] + 1

        ; Use the Welford method to compute the variance.  The Welford method is described in the
        ; following pseudocode
        ; variance(samples):
        ; M := 0
        ; S := 0
        ; for k from 1 to N:
        ;   x := samples[k]
        ;   oldM := M
        ;   M := M + (x-M)/k
        ;   S := S + (x-M)*(x-oldM)
        ; return S/(N-1)
        ; The inner part of the loop is impmlemented below.
        ; Ron Yurow (July 24, 2018)
        prev [iabmap[ipt]] = m [iabmap[ipt]]
        m [iabmap[ipt]] = m [iabmap[ipt]] + (a[ic,ipt] - m [iabmap[ipt]]) / b_nbin [ic,iabmap[ipt]]
        s [iabmap[ipt]] = s [iabmap[ipt]] + (a[ic,ipt] - m [iabmap[ipt]]) * (a [ic,ipt] - prev [iabmap[ipt]])       
		endif
      endfor

      ; Do the actual variance calculation based on the terms previously calculated.
      ; Ron Yurow (July 24, 2018)
      nz = WHERE (b_nbin [ic, *] gt 0)
      bin_variance [ic, nz] = s [nz] / (reform (b_nbin[ic, *]) - 1) [nz]
  endif 
;  if ic eq 5 then stop
endfor

;average
ii=where(b_nbin ne 0)
if ii[0] ne -1 then b[ii] = b[ii]/b_nbin[ii] ; compute mean of each bin


; compute uncertainty of the mean
ii=where(b_nbin gt 2) ;
;if ii[0] ne -1 then b2[ii] = $
;   sqrt((b2[ii]/b_nbin[ii] - b[ii]^2) $
;   *(b_nbin[ii]/(b_nbin[ii]-1))/b_nbin[ii]) ; uncertainty of the mean  ;SAB 9/25/2017 compute uncertainty
if ii[0] ne -1 then BEGIN
    ; No longer needeed, as variance is now calculated using the Welford method.
    ; Ron Yurow (July 24, 2018)
	; sigma2 = (b2[ii] - b_nbin[ii]*b[ii]^2)/(b_nbin[ii]-1) ; standard deviation
	; b2[ii] = sqrt( sigma2 ) ;/b_nbin[ii]) ; comment out uncertainty of mean computatin SAB 3/28/2018
    b2 [ii] = sqrt (bin_variance [ii])  ; Calculate standard deviation based on variance.
endif   
ii=where(b_nbin le 2)
; Changed by Ron Yurow (08/08/2019)
; if ii[0] ne -1 then b2[ii] = fillval ;max(b2) changed  SAB 3/28/2018
if ii[0] ne -1 then b2[ii] = missing_variance_flag
; end compute uncertainty of the mean

;fill in empty bins
if keyword_set(fill_empty_bins) then cmb_fill_array,b,tout,b_nbin else cmb_setemptybin2fill,b,b_nbin,fillval

serout = reform(b,ndout,/overwrite) 
seroutuncertainty = reform(b2,ndout,/overwrite)
 
serout_flag = reform(b_nbin,ndout,/overwrite)

if (where(serout_flag eq 0))(0) eq -1 then emptybin=0 else emptybin = n_elements(where(serout_flag eq 0))
print,'Fraction of missing binned points =  ', float(emptybin)/n_elements(serout_flag)
end
