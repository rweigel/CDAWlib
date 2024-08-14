;$Author: ryurow $
;$Date: 2015/03/02 21:01:25 $
;$Header: /home/cdaweb/dev/control/RCS/align_center.pro,v 1.9 2015/03/02 21:01:25 ryurow Exp $
;$Locker:  $
;$Revision: 1.9 $
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------

pro align_center, X0, Xmin1, Xmax1, LOG=log

; Scheme for aligning center of box on (X, Y) position
; ####assumes first 3 points define whether log spaced
; 1995 April 10 original (or earlier)
; Robert.M.Candey@gsfc.nasa.gov 2000 June 23; changed "exp" to "10^"
; 2001 March 21  BC, added check for NANs and change to spline_interp for speed, robustness

Xmin1 = X0 & Xmax1 = X0
w0nan = where(X0 eq X0, w0nanc) ; find all real values

; We need at least 3 points to realign bins around a central point.  If we don't
; have that then just return what we have at this point (values unchanged).  Note,
; this was the previous behavoir if we were passed a set of values that was all
; NaN.
; Ron Yurow (March 2, 2015)

; if w0nanc gt 0 then begin
  if w0nanc gt 2 then begin
   X = X0[w0nan]
   nX = n_elements(X)
;  Removed because this check was failing if the first three values were equally
;  spaced but the interval between other values varied.  The determination about
;  whether we dealing with a log scale or a linear scale will be now be made by
;  setting the LOG keyword.
;  Ron Yurow (Nov. 20, 2015)
;   dx1 = abs(X[1]-X[0]) & dx2 = abs(X[2]-X[1])
;   if (abs(dx1-dx2) lt 1.e-6 * min([dx1, dx2])) then begin ; evenly spaced X
   if ~ KEYWORD_SET (log) then begin ; evenly spaced X
      w = lindgen(nX)+1
      ;    Xs = spline(Xt,X,dindgen(nX+2)-1.) ; add outside points
      Xt = dindgen(nX)
      sCoef = spl_init(Xt,X) ; setup spline coefficients
      Xs = spl_interp(Xt,X,sCoef,dindgen(nX+2)-1.) ; add outside points
      ;  dx = (Xs[w]+Xs[w+1])/2. - (Xs[w-1]+Xs[w])/2. == (Xs[w+1] - Xs[w-1])/2.
      Xmin = (Xs[w-1]+Xs[w])/2.
      Xmax = (Xs[w]+Xs[w+1])/2.
   endif else begin ; assume log spacing
      wh = where(x le 0, wc)
      if (wc eq 0) then begin
         alogX = alog10(x)
      endif else begin
         alogX = X*0 ; all 0's
         wh = where(x gt 0, wc)
         if (wc gt 0) then alogX[wh] = alog10(X[wh])
      endelse
      ;    alogXs = spline(Xt,alogX,dindgen(nX+2)-1.) ; add outside points
      Xt = dindgen(nX)
      sCoef = spl_init(Xt,alogX) ; setup spline coefficients
      alogXs = spl_interp(Xt,alogX,sCoef,dindgen(nX+2)-1.) ; add outside points
      w = lindgen(nX)+1
      ;  dx = exp((alogXs[w]+alogXs[w+1])/2.) - exp((alogXs[w-1]+alogXs[w])/2.)
      Xmin = 10^((alogXs[w-1]+alogXs[w])/2.)
      Xmax = 10^((alogXs[w]+alogXs[w+1])/2.)
   endelse ; log spacing
   Xmin1[w0nan] = Xmin & Xmax1[w0nan] = Xmax
endif ; (w0nanc gt 0) else all NANs
return
end ; align_center
