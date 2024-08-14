
Pro cdaweb_errplot, X, Low, High, Width = width, color = color, FILLVAL=fillval
;+
; NAME:
;	CDAWEB_ERRPLOT
;
; PURPOSE:
;	Plot error bars over a previously drawn plot.
;
; CATEGORY:
;	J6 - plotting, graphics, one dimensional.
;
; CALLING SEQUENCE:
;	CDAWEB_ERRPLOT, Low, High	;X axis = point number.
;
;	CDAWEB_ERRPLOT, X, Low, High	;To explicitly specify abscissae.
;
; INPUTS:
;	Low:	A vector of lower estimates, equal to data - error.
;	High:	A vector of upper estimates, equal to data + error.
;
; OPTIONAL INPUT PARAMETERS:
;	X:	A vector containing the abscissae.
;
; KEYWORD Parameters:
;	WIDTH:	The width of the error bars, in units of the width of
;	the plot area.  The default is 1% of plot width.
;       
;       COLOR: if defined, assign a color to a graph.  If not defined
;       the default color will be used.
;
;    FILVAL: The fillval to use to the remove invalid error estimates
;    from being plotted.  Optional.
;
; OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	An overplot is produced.
;
; RESTRICTIONS:
;	Logarithmic restriction removed.
;
; PROCEDURE:
;	Error bars are drawn for each element.
;
; EXAMPLES:
;	To plot symmetrical error bars where Y = data values and 
;	ERR = symmetrical error estimates, enter:
;
;		PLOT, Y			;Plot data
;		CDAWEB_ERRPLOT, Y-ERR, Y+ERR	;Overplot error bars.
;
;	If error estimates are non-symetrical, enter:
;
;		PLOT,Y
;		CDAWEB_ERRPLOT, Upper, Lower	;Where Upper & Lower are bounds.
;
;	To plot versus a vector of abscissae:
;
;		PLOT, X, Y		  ;Plot data (X versus Y).
;		CDAWEB_ERRPLOT, X, Y-ERR, Y+ERR  ;Overplot error estimates.
;
; MODIFICATION HISTORY:
; Originally errplot.pro, idl internal routine
;       RCJ, Feb, 2003     Cdaweb_errplot: Based on errplot (idl internal routine). We needed a
;                          'long' loop:  for i=0L,n-1 do begin
;       REY, May 20, 2019  Added keyword fillval.
;-
on_error,2                      ;Return to caller if an error occurs
if n_params(0) eq 3 then begin	;X specified?
    up = high
    down = low
    xx = x
endif else begin                ;Only 2 params
    up = x
    down = low
    xx=findgen(n_elements(up))  ;make our own x
endelse

; Check if fillval is passed.
; Ron Yurow  (May 20, 2019)  
IF  N_ELEMENTS (fillval) ne 0 THEN use_fillval = 1 ELSE use_fillval = 0

color = KEYWORD_SET (color) ? color : 60

w = ((n_elements(width) eq 0) ? 0.01 : width) * $ ;Width of error bars
  (!x.window[1] - !x.window[0]) * !d.x_size * 0.5
n = n_elements(up) < n_elements(down) < n_elements(xx) ;# of pnts
for i=0L,n-1 do begin            ;do each point.
    ; If the flag use_fillval is set, then error estimates which are set to this
    ; value should not be displayed.
    ; Ron Yurow  (May 20, 2019)
    ; Modified to also compare NaN values
    ; Ron Yurow (August 7, 2019)
    ; IF  use_fillval && down[i] eq fillval THEN CONTINUE
    ; IF  use_fillval && up [i] eq fillval THEN CONTINUE
    IF  use_fillval  THEN BEGIN
        IF  FINITE (fillval) THEN BEGIN 
            IF  down[i] eq fillval  THEN CONTINUE 
            IF  up [i] eq fillval THEN CONTINUE
        ENDIF ELSE BEGIN
            IF  ~FINITE (down [i]) THEN CONTINUE 
            IF  ~FINITE (up [i]) THEN CONTINUE
        ENDELSE
    ENDIF
    xy0 = convert_coord(xx[i], down[i], /DATA, /TO_DEVICE) ;get device coords
    xy1 = convert_coord(xx[i], up[i], /DATA, /TO_DEVICE)
    plots, [xy0[0] + [-w, w,0], xy1[0] + [0, -w, w]], $
      [replicate(xy0[1],3), replicate(xy1[1],3)], $
      NOCLIP=!p.noclip, PSYM=0, color=color, /DEVICE
endfor

end
