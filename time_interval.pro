;-------------------------------------------------------------
;+
; NAME:
;       TIME_INTERVAL
; PURPOSE:
;       Find nice time axis tics. (Based on the routine TNAXES)
; DETAILED DESCRIPTION:
;       This routine can be used to find consistent, sensible major (and minor)
;       tic positions for a time axis based on the time interval being 
;       displayed.  It based on the routine TNAXES, but extends it by
;       including specific code to handle time intervals of more than
;       one day.  I.E. one month, one year, two years, and five years.
;       This was specifically designed for the creation of inventory graphs
;       but should be generally useful.
; CATEGORY:
; CALLING SEQUENCE:
;       TIME_INTERVAL, xmn, xmx, nx, mjx1, mjx2, xinc, [mnx2, mnx2, xinc2]
; INPUTS:
;       xmn, xmx = Axis min and max in sec.          in
;       nx = Desired number of axis tics.            in
; KEYWORD PARAMETERS:
;       Keywords:
;         FORM=form  returns a suggested format, suitable
;           for use in formatting time axis labels.
;           Ex: h$:m$:s$, h$:m$, d$
; OUTPUTS:
;       mjx1 = first major tic position in sec.      out
;       mjx2 = last major tic position in sec.       out
;       xinc = Suggested major tic spacing in sec.   out
;       mnx1 = first minor tic position in sec.      out
;       mnx2 = last minor tic position in sec.       out
;       xinc2 = suggested minor tic spacing in sec.  out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       Ron Yurow (March 20, 2019) Original version based on TNAXES.
;
; Copyright (C) 1988, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
 
	PRO TIME_INTERVAL, DX0,DX1,NX, mjx1,mjx2,xi, mnx1, mnx2, xi2, $
	  help=hlp, form=form
 
	DX = DOUBLE(DX1 - DX0)	; Axis range.
 
	IF DX GT 0 THEN BEGIN	;Forward axis.
	  X0 = DOUBLE(DX0)
	  X1 = DOUBLE(DX1)
	ENDIF ELSE BEGIN	; Reverse axis.
	  X0 = DOUBLE(DX1)
	  X1 = DOUBLE(DX0)
	ENDELSE
 
	XINC = (X1-X0)/NX	; Approx. inc size.

;;TJK 10/19/2009 add settings for really high time res. for TH*_L2_SCM
;Write all of this as a formula instead of mutliple if statements.
;So this will be driven by "NX" which is the number of increments
;requested.

        if XINC LT 1.0 then begin
            XINC2 = (X1-X0)/(NX+1)	; Approx. inc size (make it smaller).
            XI = XINC2 & XI2 = XI/2.0
;            print, 'DEBUG TNAXES, using formula for xi, xi2 ',xi, xi2
;            print, 'DEBUG TNAXES, XINC2 = ',Xinc2
            if (XI GE 0.04) then FORM ='s$' else FORM = 's$.f$'
            IF XINC2 LT .0001 THEN begin 
              XI = 0.0001 & xi2 = 0.00005
;              print, 'DEBUG TNAXES, making adjustment for xinc2 lt .0001 ',xi,xi2
            endif
	    GOTO, DONE
	endif
	;------------  1 sec to 1 min  -----------------
	IF XINC LT 60. THEN BEGIN	; XINC in sec < 1 min.
	  XI = 60.
	  xi2 = 15.
	  IF XINC LT 42.4 THEN begin XI = 30. & xi2 = 10. & endif
	  IF XINC LT 21.2 THEN begin XI = 15. & xi2 = 5.  & endif
	  IF XINC LT 12.2 THEN begin XI = 10. & xi2 = 2.  & endif
	  IF XINC LT 7.1  THEN begin XI = 5.  & xi2 = 1.  & endif
	  IF XINC LT 3.2  THEN begin XI = 2.  & xi2 = 0.5 & endif
	  IF XINC LT 1.4  THEN begin XI = 1.  & xi2 = 0.2 & endif
	  FORM = 'h$:m$:s$'
	  if xi gt 30. then form = 'h$:m$'
	  GOTO, DONE
	ENDIF
	;------------  1 min to 1 hr  -------------------
	XINC = XINC/60.
	IF XINC LT 60. THEN BEGIN	; XINC in min < 1 hr.
	  XI = 60.
	  xi2 = 15.
	  IF XINC LT 42.4 THEN begin XI = 30. & xi2 = 10. & endif
	  IF XINC LT 21.2 THEN begin XI = 15. & xi2 = 5.  & endif
	  IF XINC LT 12.2 THEN begin XI = 10. & xi2 = 2.  & endif
	  IF XINC LT 7.1  THEN begin XI = 5.  & xi2 = 1.  & endif
	  IF XINC LT 3.2  THEN begin XI = 2.  & xi2 = 0.5 & endif
	  IF XINC LT 1.4  THEN begin XI = 1.  & xi2 = 0.2 & endif
	  FORM = 'h$:m$'
	  XI = XI*60.	; want step in sec.
	  xi2 = xi2*60.
	  GOTO, DONE
	ENDIF
	;-------------  1 hr to 1 day  -----------------
	XINC = XINC/60.
	IF XINC LT 24. THEN BEGIN	; XINC in hr < 1 day.
	  XI = 24.
	  xi2 = 6.
	  IF XINC LT 17  THEN begin XI = 12. & xi2 = 3.   & endif
	  IF XINC LT 8.5 THEN begin XI = 6.  & xi2 = 2.   & endif
	  IF XINC LT 4.9 THEN begin XI = 4.  & xi2 = 1.   & endif
	  IF XINC LT 2.8 THEN begin XI = 2.  & xi2 = 0.5  & endif
	  IF XINC LT 1.4 THEN begin XI = 1.  & xi2 = 0.25 & endif
	  FORM = 'h$:m$'
	  if xi gt 4 then form = 'h$:m$@d$'
;	  if xi gt 12. then form = 'I$'
	  if xi gt 12. then form = 'y$ n$ d$'
	  XI = XI*3600.	; want step in sec.
	  xi2 = xi2*3600.
	  GOTO, DONE
	ENDIF
	;-------------  1 day to 30.41 days (month) -----------------
	XINC = XINC/24.		; XINC is in days.
	IF XINC LT 30.41 THEN BEGIN	; less then 1 month (30 days).
	  XI  = 30.41
	  xi2 = 4.
	  IF XINC LT 17  THEN begin XI = 15.2 & xi2 = 2.   & endif
	  IF XINC LT 12	 THEN begin XI = 10.  & xi2 = 2.   & endif
	  IF XINC LT 8.5 THEN begin XI = 7.   & xi2 = 1.   & endif
	  IF XINC LT 2.8 THEN begin XI = 2.   & xi2 = 0.5  & endif
	  IF XINC LT 1.4 THEN begin XI = 1.   & xi2 = 0.25 & endif
	  FORM = 'y$ n$ d$
	  XI = XI*86400.	; want step in sec.
	  xi2 = xi2*86400.
	  GOTO, DONE
	ENDIF
	;------------- 30 days (month) to 2 Years-----------------
	XINC = XINC/30.41		; XINC is in months.
	IF XINC LT 24. THEN BEGIN	; less then 2 years (730 days).
	  XI  = 24.
	  xi2 = 2.
	  IF XINC LT 14.5 THEN begin XI = 12.  & xi2 = 4.   & endif
	  IF XINC LT 8.5  THEN begin XI = 6.   & xi2 = 2.   & endif
	  IF XINC LT 2.8  THEN begin XI = 2.   & xi2 = 0.5  & endif
	  IF XINC LT 1.4  THEN begin XI = 1.   & xi2 = 0.25 & endif
	  FORM = 'y$ n$ d$
	  XI = XI*86400.*30.41	; want step in sec.
	  xi2 = xi2*86400.*30.41
	  GOTO, DONE
	ENDIF

	;---------------  greater then 1 years  -----------------
	; Corrected divisor to multiplier REY (05/08/20)
	XINC = XINC*30.41		; XINC is back in days. 
	P = ALOG10(XINC)	; Scale to 1 to 10.
	IF P LT 0 THEN P = P-1.
	P = FIX(P)
	POW = 10.^P
	XI = XINC/POW
	XINC = XI
	;------ Set increment to a nice value -----------
	XI = 10.			; Filter scaled increment
	xi2 = 2.
	IF XINC LT 7.07 THEN begin XI = 5.   & xi2 = 1.   & endif
	IF XINC LT 3.5  THEN begin XI = 2.5  & xi2 = 0.5  & endif
	IF XINC LT 2.24 THEN begin XI = 2.   & xi2 = 0.5  & endif
	IF XINC LT 1.4  THEN begin XI = 1.   & xi2 = 0.25 & endif
	IF XI GE 10. THEN BEGIN
	  XI = 1.
	  P = P + 1.
	  POW = POW*10.
	ENDIF
	XI = 86400*XI*POW	; XI = true increment.
	xi2 = 86400.*xi2*pow
;	FORM = 'I$'
	FORM = 'y$ n$ d$'
 
DONE:	IF DX LE 0. THEN begin XI = -XI & xi2 = -xi2 & endif
;TJK 10/26/2009 - for time ranges less than 1 second, adjust the
;                 min/max values by their offsets... we want them more
;                 precise.  The goal is to have inrange compute major
;                 tick  marks to be exactly in line w/ the start/end 
;                 of the data.  Inrange, by design,
;                 computes values just "inside" the given start/stop.

        if XI GT .01 then INRANGE, XI, DX0, DX1, mjx1, mjx2 else $
          INRANGE, XI, (DX0-XI2), (DX1+XI2), mjx1, mjx2

          INRANGE, XI2, DX0, DX1, mnx1, mnx2

	RETURN
 
	END