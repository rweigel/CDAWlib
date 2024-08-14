;$Author: ryurow $ 
;$Date: 2015/06/05 18:55:08 $
;$Header: /home/cdaweb/dev/control/RCS/Colorbar.pro,v 1.31 2015/06/05 18:55:08 ryurow Exp ryurow $
;$Locker: ryurow $
;$Revision: 1.31 $
Pro Colorbar, scale, title, logZ=logZ, position=position, cCharSize=cCharSize,$
              fcolor=fcolor, reverse=reverse, image=image, nResCol=nResCol
;+ Terri Martin and Robert Candey
; 21 June 1993
; added ccharsize ; 1995 July 26
; added save on !x, !y, !p; 1995 Aug 2; BC
; reduced ncolors when not enough pixels; 1995 Sep 14 BC
; added !p.multi=0; 1996 Aug 28 BC
;
; Modified: July 30, 1997 by Tami Kovalick added the Reverse Keyword
;   so that you can reverse the colors in the colorbar.
;
; Modified: October 31, 1997 by T. Kovalick added the image keyword - 
; this keyword modifies the labeling for the color bar so it indicates
; that values in the image above the scale are assigned to the top color.
;
; 2001 Mar 30 BC, changed fcolor to default to !p.color
; 2001 March 30 BC, added nResCol in place of fixed 2 colors
;
; Purpose:
; This procedure creates a colorbar for the right side of a spectrogram
; and image...
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;-
common deviceTypeC, deviceType,file; required for inverting grayscale Postscript
xsave = !x & ysave = !y & zsave = !z & psave = !p
!p.multi = 0

; Check if the scale width is 0.  This is can happen if there is no valid data
; being plotte.  In this case, we need to adjust the top and bottom.
; Change made by Ron Yurow (June 5, 2015)

IF  scale [0] eq scale [1] THEN BEGIN

    scale [0] = scale [0] - scale [0] / 1000.0
    scale [1] = scale [1] + scale [1] / 1000.0

ENDIF 

if (n_elements(position) ne 0) then positiont = position else $
  positiont = [!x.window[1]+0.01, !y.window[0], !x.window[1]+0.04, !y.window[1]]
;	positiont = [0.9, 0.1, 0.93, 0.9]
if not keyword_set(logZ) then logZ=0
if (n_elements(title) le 0) then title = ''
if (n_elements(nResCol) le 0) then nResCol = 2
;TJK 5/8/2003 - replace !d.n_colors w/ !d.table_size because we only need as many
;colors as those in the selected colorbar.
;nColors = !d.n_colors-nResCol ; reserve black and white at ends of colorscale
nColors = !d.table_size-nResCol ; reserve black and white at ends of colorscale
colors = bindgen(nColors) + 1B
if (n_elements(deviceType) ne 0) then if (deviceType eq 2) then $
;	colors = (!d.n_colors-1B) - colors ; invert grayscale for Postscript
	colors = (!d.table_size-1B) - colors ; invert grayscale for Postscript
;if not keyword_set(fcolor) then fcolor=0
if n_elements(fcolor) le 0 then fcolor=!p.color

fontsize = 1.0
if n_elements(cCharSize) le 0 then cCharSize = 0.
if cCharSize gt 0 then begin
  fontsize = cCharSize
endif else begin
; alternative if cCharSize is undefined or le 0
  if !p.charsize gt 0. then fontsize = !p.charsize
  if !y.charsize gt 0. then fontsize = !y.charsize * fontsize
  if (!p.multi[1]>!p.multi[2]) gt 2 then fontsize = fontsize/2.
endelse

plot, [0., 1.], [0., 1.], position = positiont, $
	/nodata, /noerase, xstyle = 4, ystyle = 1+4
;axis, yaxis = 1, ystyle = 1, yrange = scale, ytype=logZ, ycharsize=fontsize, $
;	ytitle = title, ticklen = -0.02*0.78/0.04 ; adjust for narrow window

if (abs(!x.window[1]-!x.window[0])*!d.x_size le 2) then begin
  message, 'Colorbar too narrow', /info
  !x = xsave & !y = ysave & !z = zsave & !p = psave ; restore original settings
  return
endif
colorStep = ceil(float(ncolors)/(abs(!y.window[1]-!y.window[0])*!d.y_size)) > 1L
; could require 2 pixels per color; colorStep = colorStep * 2L
nSteps = fix(nColors / colorStep) < nColors
;if (!d.name eq 'PS') 
if (!d.flags and 1L) then begin ; has scalable pixels (Postscript)
  colorStep = 1L
  nSteps = nColors
endif
;print,'colorStep = ', colorStep, ' nSteps = ', nSteps

if keyword_set(reverse) then colors = reverse(colors)

for i = 0L, nSteps-1 do begin
  polyfill, [0.,1.,1.,0.], (i+[0.,0.,1.,1.])/nSteps, $
	color=colors[i*colorStep], noclip=0
endfor ; i

; replot so the box gets put back over the filled area
plot, [0., 1.], [0., 1.], position = positiont, $
	/nodata, /noerase, xstyle = 4, ystyle = 1+4
; adjust for narrow window
; ystyle: 1=force exact axis range, 4=supress entire axis
  
;  RCJ  11/2013 small change to these lines: ytickformat does not depend on number of tick marks
; and ytype seems to no longer be a valid keyword for axis.
;Get the yticks from IDL for the specified scale (yrange)
  axis, yaxis = 1, ystyle = 1+4, yrange = scale, ylog=logZ, $
  ycharsize=fontsize,color=fcolor,ytickformat='(A1)',ytick_get=yticks
  n_yticks = n_elements(yticks)
;print, 'INFO: IDL default labels = ',yticks,' num lables ',n_yticks, 'for scale = ',scale
  ordered = 1L
  newticks = yticks
  modticks = 0L ;set flag for when we're adjusting with the tick marks
;if log and only one label
  case1 = (logZ and n_yticks lt 2)
;check to see if log scaled and 2 labels found and the scalemin and max are "inside" of the
;yticks, e.g. scale = 2,3 and yticks =1,10 - we want to use 1,10 for log,
;otherwise we get no labels/ticks
  case2=0L
  if(n_yticks eq 2) then case2 = (logZ and (scale[0] gt yticks[0]) and (scale[1] lt yticks[1]))
  if (case1 or case2) then begin
    modticks = 1L
    tmpscale = scale
    if (scale[1] lt scale[0]) then begin
       ordered=0L ;case to handle stack plots which have min at the top, max at the bottom
       tmpscale[0] = scale[1]  & tmpscale[1] = scale[0]
    endif
    newticks = loglevels(tmpscale) ;/coarse might work better in some cases 
    if (n_elements(newticks) eq 1) then newticks = scale ;set them to scale passed in.
;test, what would IDL give us when just asking for 3 ticks (specify 2)
    axis, yaxis = 1, ystyle = 1+4, yrange = scale, ylog=logZ, yticks=2,$
    ycharsize=fontsize,color=fcolor,ytickformat='(A1)',ytick_get=idlyticks
;    print, 'INFO: IF forced 3 ticks: idlyticks would be= ',idlyticks,' num lables ',n_elements(idlyticks)
  endif
if not ordered then yticks = reverse(newticks) else yticks = newticks
;print, 'INFO: using vals from loglevels: yticks = ',yticks,' num lables ',n_elements(yticks)

if (keyword_set(IMAGE)) then begin ;adjust top axis label to be ">= number"
  ;get the tick labels for the color axis - this is a really klugy way
  ;to get the ytick labels back from idl.
;TJK 10/21/2005
; moved this above to get yticks for IMAGES AND other types of plots
;  axis, yaxis = 1, ystyle = 1+4, yrange = scale, ytype=logZ, $
;  ycharsize=fontsize,color=fcolor,ytickname=replicate(' ',30),ytick_get=yticks

  ;yticks = fix(yticks) ;convert to integers
  ;ychar_ticks = string(yticks)
  
  ; RCJ 12/11/00 replace 2 lines above with the following 'if' statement: 
  ; remove decimal if not needed:

  q=where(yticks-long(yticks) ne 0)
  if (q[0] eq -1) then begin
     yticks=long(yticks) ; turn them into integers
     if (logZ) then ychar_ticks = string(yticks,format='(e20.1)') else $
     ychar_ticks = string(yticks)
  endif else begin
     ; RCJ 06/23/2003 Originally we only had the 'f20.1' line, but for numbers
     ; of the order of 10^-4 we needed the 'e' format. This may/may not be
     ; the best setup, depending on tests.
     if (yticks[n_yticks-1]-yticks[0] le 1 or logZ) then $
        ychar_ticks = string(yticks,format='(e20.1)') else $
        ychar_ticks = string(yticks,format='(f20.1)')
  endelse
  
  ychar_ticks = strtrim(ychar_ticks,2) ;trim off the blanks
   
  ydim = size(yticks)
  
  ychar_ticks[ydim[1]-1] = '>='+ychar_ticks[ydim[1]-1]
  ;axis, yaxis = 1, ystyle = 1, yrange = scale, ytype=logZ,ycharsize=fontsize,$
  ; Added separate axis statement to handle case of recalculated labels.  This had
  ; already been done when for the case where the IMAGE keyword is not specified.
  ; Ron Yurow (March 22, 2016)
  if (modticks) then begin
     n_yticks = n_elements(ychar_ticks)
;      print, 'INFO: specifying num ticks and tick values'
;      print, 'INFO: yticks = ',yticks,' num lables ',n_yticks
     axis, yaxis = 1, ystyle = 1,  ylog=logZ, ycharsize=fontsize, $
         color=fcolor,ytitle = title, ticklen = -0.02*0.78/0.04,$
         yrange=scale ,ytickv=yticks, yticks=n_yticks, ytickname=ychar_ticks
  endif else begin
     axis, yaxis = 1, ystyle = 1, yrange = scale, ylog=logZ,ycharsize=fontsize,$
         color=fcolor,ytitle = title, ticklen = -0.02*0.78/0.04, $
         ytickname=ychar_ticks
  endelse

endif else begin ;for plot types besides IMAGE
;TJK 8/8/2014 another attempt at getting more labels for log scaled plots
   if (modticks) then begin
      n_yticks = n_elements(yticks)
;      print, 'INFO: specifying num ticks and tick values'
;      print, 'INFO: yticks = ',yticks,' num lables ',n_yticks
      axis, yaxis = 1, ystyle = 1,  ylog=logZ, ycharsize=fontsize, $
         color=fcolor,ytitle = title, ticklen = -0.02*0.78/0.04,$
         yrange=scale ,ytickv=yticks, yticks=n_yticks
   endif else begin
;      print, 'INFO: IDL default labels based on scale.'
      axis, yaxis = 1, ystyle = 1,  ylog=logZ, ycharsize=fontsize, $
         color=fcolor,ytitle = title, ticklen = -0.02*0.78/0.04,$
         yrange=scale 
   endelse

endelse

!x = xsave & !y = ysave & !z = zsave & !p = psave ; restore original settings
return
end ; colorbar
