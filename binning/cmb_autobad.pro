;+
; NAME:
;   cmb_autobad
;            
; EXAMPLE USAGE:
;   i=cmb_autobad(data)
;
; PURPOSE:   
; To filter data returning an array of same size as data of 1's and 0's,
; 0 indicates bad data element and 1 indicates a good data element.
; Based on D. A. Roberts, "An algorithm for finding spurious points in turbulent signals.", 
; COMPUTERS IN PHYSICS, JOURNAL SECTION, SEPT/OCT 1993.
;
; CATEGORY:
; Data fitering.
;
; CALLING SEQUENCE:                                   
; result= cmb_autobad(datain,sigmul,fillval=fillval,nsum=nsum)                    
;                                                     
; INPUTS:                                             
;   datain - one or two dimensional data array of n n_elements: datain[2,n] or datain[n].
;   sigmul - multiplicative factor of standard devidation in each sub array: 5 (default),  4 (less agressive), 6 (more agressive).
;            Absolute values of the (data - mean) > sigmul*standard_deviation 
;            will be flagged as bad (0), if less than then flagged as good (1).
; Keyword Inputs:                                                      
;   fillval - fill values: -1e31 (default).
;   nsum - size of sub-array for each filter step: 100 (default).
;
; OUTPUTS:
;   Interger array of same size as the data array:
;     1 is a good data element, 
;     0 is a bad data element.
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   Unknown.
;
; RESTRICTIONS:
;   Unknown.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;   Code developed by Aaron Roberts and Scott Boardsen at GSFC.
;-

function cmb_autobad0,datain,sigmul,fillval=fillval,nsum=nsum, nfill = nfill, filter_by_fill=filter_by_fill
;input
; datain n elements array of one dimension
; sigmul -multiplicative factor of the running standard deviation
; nsum - size of sub interval
; fillval = data fill value
;output
; igood n elements array of one dimension, 0 bad data point, 1 good data point

if n_elements(fillval) eq 0 then fillval=-1e31
if n_elements(sigmul) eq 0 then sigmul = 5
if n_elements(nsum) eq 0 then nsum=100l

y = datain
npts = n_elements(y)
igood = intarr(npts)


if keyword_set( filter_by_fill ) then begin
  ii=where(y ne fillval)
  nfill = n_elements(where(y eq fillval))
  if (where(y eq fillval))(0) eq -1 then nfill =0
endif else begin
  ii=where(abs(y) lt 0.99*abs( fillval))
  nfill = n_elements(where(abs(y) ge 0.99*abs( fillval)))
  if (where(abs(y) ge 0.99*abs( fillval)))(0) eq -1 then nfill =0
endelse
if ii[0] ne -1 then igood[ii] = 1
ig=where(igood eq 1)

if n_elements(ig) lt nsum  then begin
   print,'Not enough data points in series, only filtered for fill values, returning.'
   return,igood
endif

ng = n_elements(ig)
igmax = max(ig)

; made j0 long (Ron Yurow 5/5/21)
;j0 = nsum-1
j0 = LONG (nsum-1)
ii = ig[0:j0] ;initial sub-interval

icnt=0l ;counter, not used
; made iend long (Ron Yurow 5/5/21)
; iend=0 ;end of data indicator
iend=0l ;end of data indicator

while iend lt 2 do begin

      if min(y[ii]) eq fillval then stop
      ymean = total(y[ii])/nsum
      yvar = sqrt( total((y[ii]-ymean)^2)/(nsum-1) )
      jj=where( abs(y[ii]-ymean) gt sigmul*yvar) ;bad points in sub-interval ii

      ;set igood to 0 for bad data points, and reform sub-interval indices array for next step
      if jj[0] ne -1 then begin
         kk = ii[jj]
         ibad = n_elements(jj)
         if ibad eq nsum then begin
            ja = j0+1 < ng-1
            jb = j0+100 < ng-1
	    ii = ig[ja:jb]
	    j0 = j0 + 100   
         endif else begin
	    if jj[0] eq 0 then ibad=ibad-1
	    igood[kk]=0  ;set bad data point indices to 0
	    ii[jj] = -1  ;set sub internval indices of bad data points to -1
	    ii=ii[1:*]   ;step to next data point
	    ii=ii[where(ii ne -1)] ;remove indices of bad data points
	    j1 = j0 +ibad +1
	    if j1 ge ng then ii = ig[ng-100:ng-1] else  ii = [ii, ig[j0+1:j1]]
	    if n_elements(ii) ne nsum then stop
	    j0 = j1
         endelse
      endif else begin
         j0 = j0+1
         if j0 ge ng then ii = ig[ng-100:ng-1] else  ii = [ii[1:*], ig[j0]] ;reform sub-interval indices array for next step
      endelse
      if max(ii) ge igmax then  iend =iend+1 ;check if end of data is reached.
      icnt= icnt + 1l
endwhile

return,igood
end


function cmb_autobad,datain,sigmul,fillval=fillval,nsum=nsum,ibad=ibad,filter_by_fill=filter_by_fill
;datain[ic,itime] or datain [ic] either a 1 or 2 dimensional array

if n_elements(sigmul) eq 0 then sigmul = 5
if n_elements(nsum) eq 0 then nsum=100
print,'Filtering data using cmb_autobad: sigmul=',sigmul,' nsum=',nsum
si=size(datain,/str)
if si.n_dimensions eq 1 then begin
   ig= cmb_autobad0(datain,sigmul,fillval=fillval,nsum=nsum, nfill=nfill,filter_by_fill=filter_by_fill) 
   ibad=where(ig[*] eq 0)
   if ibad[0] eq -1 then ibad=0 else ibad=n_elements(ibad)-nfill
   print,' Number of fill points = ',nfill,' Percent fill=',nfill*1.0/(n_elements(ig))*100,'%' 
   print,' Number of bad points = ',ibad,' Percent bad=', ibad*1./(n_elements(ig)-nfill)*100,'%' 
endif else begin
ig = fix(datain)*0
for ic = 0l, si.dimensions[0]-1 do begin
    ig[ic,*] = cmb_autobad0(reform(datain[ic,*]),sigmul,fillval=fillval,nsum=nsum, nfill=nfill, filter_by_fill=filter_by_fill)
    ibad=where(ig[ic,*] eq 0)
    if ibad[0] eq -1 then ibad=0 else ibad=n_elements(ibad)-nfill
    print,' Number of fill points = ',nfill,' Percent fill=',nfill*1.0/(n_elements(ig[ic,*]))*100,'%'     
    print,'Component=',ic,' Number of bad points = ',ibad,' Percent bad=', ibad*1./(n_elements(ig[ic,*])-nfill)*100,'%' 
endfor
endelse
return,ig
end