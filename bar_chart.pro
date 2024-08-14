;$Author: tkovalic $
;$Date: 2022/07/15 16:05:30 $
;$Header: /home/cdaweb/dev/control/RCS/bar_chart.pro,v 1.25 2022/07/15 16:05:30 tkovalic Exp rcjohns1 $
;$Locker: rcjohns1 $
;$Revision: 1.25 $
;-------------------------------------------------------------------------
; FUNCTION bar_chart, bar_names, bars, times, COLORS, BIGPLOT=BIGPLOT,
; FIVEYEAR=FIVEYEAR, COLORNEW=COLORNEW, long_line=long_line, wide_margin=wide_margin
;   bar_names : strarr(n)
;   bars      : lonarr(n,m)
;   times     : double(m)
;   long_line: 0/1  to draw a line between availability bar and dataset name.
;                   Best when it's difficult to tell what bar goes w/ what dataset
;                   because of the distance between them.
;   wide_margin: 0/1/2  when need wider margin on left side because 
;                       of long dataset names.  Extended to allow
;                       value of 2 for half way between 0 and 1
;
; FUNDAMENTAL PROBLEM:  bar chart showing data availability needs two points
;                       to plot, (start and end), not just a single point like
;                       a time-series plot.
;
; APPROACH:  a) Find first non-zero bars point.  Draw until next zero point.
;            b) repeat until done.
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
;
FUNCTION bar_chart,bar_names,bars,times,COLORS=COLORS,DEBUG=DEBUG,GIF=GIF, PNG=PNG,$
                  TITLE=TITLE,XSIZE=XSIZE,BIGPLOT=bigplot,FIVEYEAR=fiveyear,$
                  COLORNEW=colornew, long_line=long_line, wide_margin=wide_margin

; Validate input parameters.  Bars should be a intarr(n,m), bar_names should
; be strarr(n), times should be dblarr(m) of CDF_EPOCH times.
print,'bar_chart parameter validation TBD.'

; Calculate array sizes
ntags = n_elements(bar_names) & ntimes = n_elements(times)

; Create a subtitle for the plot showing the data start and stop times
CDF_EPOCH,times[0],year,month,day,hour,minute,second,milli,/BREAK
subtitle = 'TIME RANGE='+strtrim(string(year),2)+'/'+strtrim(string(month),2)
subtitle = subtitle + '/' + strtrim(string(day),2) + ' to '
CDF_EPOCH,times[ntimes-1],year,month,day,hour,minute,second,milli,/BREAK
subtitle = subtitle + strtrim(string(year),2)+'/'+strtrim(string(month),2)
subtitle = subtitle + '/' + strtrim(string(day),2)

; Convert the time array into seconds since first time
CDF_EPOCH,times[0],year,month,day,hour,minute,second,milli,/BREAK
CDF_EPOCH,a,year,month,day,0,0,0,0,/COMPUTE_EPOCH
secs   = (times - a) / 1000 & julday = ymd2jd(year,month,day)

; Determine label for time axis based on time range
trange = secs[ntimes-1] - secs[0]
if (trange le 60.0) then tform='h$:m$:s$.f$@y$ n$ d$' $ ;1 minute
else if (trange le 86400L) then tform='h$:m$@y$ n$ d$'$ ; 1 day
else if (trange le 15000000L) then tform='n$ d$@Y$'$ ; le ~6 mo.
;else tform='n$ d$@y$'
; The above format is not good if we're trying to plot decades of inventory data.RCJ 09/26/02
else tform='n$@y$'

; Initialize plotting parameters
pheight=10
if keyword_set(XSIZE) then xs=XSIZE else xs=500
;ys = (ntags * pheight)+100 ; compute window size
ys = (ntags * pheight)+130 ; compute window size - increase size to allow for date line

; Open GIF file or xwindow
case 1 of
 keyword_set(GIF): deviceopen,6,fileOutput=GIF,sizeWindow=[xs,ys]
 keyword_set(PNG): deviceopen,7,fileOutput=PNG,sizeWindow=[xs,ys]
 else: window,/free,xsize=xs,ysize=ys,title='Bar Chart'
endcase
;if keyword_set(GIF) then begin
;  deviceopen,6,fileOutput=GIF,sizeWindow=[xs,ys]
;endif else begin
;  window,/free,xsize=xs,ysize=ys,title='Bar Chart'
;endelse
; ct_bar_chart ; load bar chart color table


; Create empty plot and frame it
;plot,[0,1],[1,0],/nodata,xrange=[secs[0],secs[ntimes-1]],xmargin=[15,4],$ ; allow for date
;plot,[0,1],[1,0],/nodata,xrange=[secs[0],secs[ntimes-1]],xmargin=[15,4],ymargin=[6,2],$
; RCJ 12/11/2003 Changed xmargin to fit longer dataset names 

;plot,[0,1],[1,0],/nodata,xrange=[secs[0],secs[ntimes-1]],xmargin=[17,4],ymargin=[6,4],$
;       xstyle=5,xticklen=0.01,xthick=3.0, yrange=[0,ntags],ystyle=1,$

print,'ntags = ', ntags
ytickm = ntags/10
;TJK IDL has a limit on the number of ticks, labels can't be over 60...
if (ytickm ge 60) then ytickm = 40
if (ytickm lt 1) then ytickm = ntags
;print, 'TJK DEBUG requesting ',ytickm,' y ticks'
; add ytickname set to blanks to over-ride the default numeric labels
yticknames = make_array(ytickm+1, /string, value = ' ') 
;TJK 7/14/2022 add one more space between label and plot 
;max_bar_len = max(strlen(bar_names))
max_bar_len = max(strlen(bar_names)) + 1

;set the xmargin based on the max dataset length and the font
;character size, instead of guessing!
;thisxmargin=[20,4] ;set default
;if keyword_set(wide_margin) then thisxmargin=[40,4] ;original wide_margin setting

char_width = max([!p.charsize, 1.15]) ;1.15 seems to be better
if (char_width le 0) then char_width = 1
thisxmargin=[(max_bar_len*char_width),4] ;set default

plot,[0,1],[1,0],/nodata,xrange=[secs[0],secs[ntimes-1]],xmargin=thisxmargin,ymargin=[8,4],$
       xstyle=5,xticklen=0.01,xthick=2.0, yrange=[0,ntags],ystyle=1,$
       ;yticklen=1.0, ytickname=yticknames, ythick=4.0, yticks=ytickm
       yticklen=1.0, ytickname=yticknames, ythick=4.0, yticks=ytickm,background=cgcolor('white')

;TJK added yticklen=1.0, above to create a y grid, and had to change ystyle
;from 5 to 1 in order to see the grids.  Then had to add ytickname set
;to blanks to over-ride the default numeric labels

plots,[secs[0],secs[0]],[0,ntags]
plots,[secs[ntimes-1],secs[ntimes-1]],[0,ntags]

;c = 48 ;default color blue
;recent_data_color = 100 ; has to match the color set in ingest_database
c = 'BLU8' 
recent_data_color = 'green' ; has to match the color set in ingest_database

;TJK 7/14/2022 add one more space 
;maxl=max(strlen(bar_names))  ; if long_line=1 this will be needed further down
maxl=max(strlen(bar_names))+1  ; if long_line=1 this will be needed further down
; Plot the inventory data
for i=0,ntags-1 do begin ; process each dataset
  bar = bars[i,*] & bar = reform(bar) & from=0L & to=0L & done=0L ;TJK c=max(bar)
  yindx = [1,1]*(ntags-i)-0.5 ;TJK, move the bars down one unit from the top of the plotting box
  while done eq 0 do begin
    w = where(bar[from:ntimes-1] ne 0,wc) ; find where next sub-bar starts
    if wc gt 0 then begin & from = from + w[0]
      u = where(bar[from:ntimes-1] eq 0,uc) ; find sub-bar end
      if uc eq 0 then to=(ntimes-1) else to=from+(u[0]-1)
      
      ; RCJ 20May2022. For datasets with less than a day's worth of data, eg. renu2_h0_* ,
      ;    mark the whole day.  This will also reveal some sparse data.
      if (from eq to) and (from+1 ne n_elements(secs)) then to = to+1

      ;      plots,([secs[from],secs[to]]),([1,1]*ntags-i),thick=8,color=cgcolor(c)
      ;TJK 1/14/2022 try drawing the horizontal bars one "line" down from the top
      plots,([secs[from],secs[to]]),yindx,thick=8,color=cgcolor(c)
      if keyword_set(long_line) then plots,([secs[0],secs[ntimes-1]]),yindx,thick=1,color=cgcolor(c)
    endif else done=1
    from = to+1 & if from eq ntimes then done=1
  endwhile

;TJK add a second loop to plot the more recent data in a bright blue color
  if keyword_set(COLORNEW) then begin

    from=0L & to=0L & done=0L ;TJK c=max(bar)

    while done eq 0 do begin
      w = where((bar[from:ntimes-1] ne 0 and bar[from:ntimes-1] ne cgcolor(c)),wc) ; find where next sub-bar starts
      if wc gt 0 then begin & from = from + w[0]
        u = where((bar[from:ntimes-1] eq 0) or (bar[from:ntimes-1] eq cgcolor(c)),uc) ; find sub-bar end
        if uc eq 0 then begin
          to=(ntimes-1) 
          ;print,'For ',bar_names[i], ' found some recent data ',from, to
          plots,([secs[from],secs[to]]),yindx,thick=8,color=cgcolor(recent_data_color)
        endif else begin
          to = from+(u[0]-1)
          recent = where(bar[from:to] eq cgcolor(recent_data_color), rc) ;look inbetwen from and next 0
          if (rc gt 0) then begin                                   ;for "recent" data
            to=from+(rc-1)
            ;print,'For ',bar_names[i], ' found some recent data ',from, to
            plots,([secs[from],secs[to]]),yindx,thick=8,color=cgcolor(recent_data_color)
          endif
        endelse
      endif else done=1
      from = to+1 & if from eq ntimes then done=1
    endwhile
  endif
;TJK end second loop

;  ypos = convert_coord(([1,1]*ntags-i),/data,/to_device)
;TJK 1/14/2022 adjust the ypos for the data set down one as well
  ypos = convert_coord(yindx,/data,/to_device)

  if keyword_set(long_line) then begin
     for j=strlen(bar_names[i]), maxl do bar_names[i]=bar_names[i]+'_'
     ;print,'DEBUG In long_line section, printing bar_names ', bar_names
     xyouts,5,ypos[1],bar_names[i],/device,color=cgcolor(c)
  endif else xyouts,5,(ypos[1]-2),bar_names[i],/device,color=cgcolor(c)

  ; RCJ 09Mar2020  Change color:
  case c of
    'BLU8': c='Steel Blue'
    'Steel Blue': c='Orange Red'
    'Orange Red': c='BLU8'
  endcase
  
endfor
; Adjust the number of tick marks based on the time interval being plotted.
; Ron Yurow (March 25, 2020)
request_tick_no =  (secs [ntimes-1] - secs [0]) / 86400.0 lt 90? 6 : 12 

; Title and subtitle the plot
if keyword_set(TITLE) then xyouts,(xs/2),(ys-9),TITLE,/device,alignment=0.5

;TJK 9/14/2005 - adding a label at the top (for Bob)
; Ron Yurow 3/24/2020 use previously calculated request_tick_no instead of 
;                      always requesting 12 ticks
timeaxis_text,FORM=tform,JD=julday,CHARSIZE=1.05,TICKLEN=1,$
major=0, NTICKS=request_tick_no, /onlylabel, yvalue=ntags+3.5, BIGPLOT=bigplot, FIVEYEAR=fiveyear
;timeaxis_text,FORM=tform,JD=julday,CHARSIZE=1.05,TICKLEN=1,$
;major=0, NTICKS=12, /onlylabel, yvalue=ntags+3.5, BIGPLOT=bigplot, FIVEYEAR=fiveyear

;timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=0.9,TICKLEN=1,$
;RCJ 12/11/2003  Changed charsize. 0.9 seems too small.
;timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=1.05,TICKLEN=1,$
;              major=0, NTICKS=12, thick=1.0, BIGPLOT=bigplot, FIVEYEAR=fiveyear
; Changed TICKLEN 0 to remove extraneous tick marks at the top and bottom of the plot
; Ron Yurow (March 4, 2020)
; Ron Yurow 3/24/2020 use previously calculated request_tick_no instead of 
;                      always requesting 12 ticks
timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=1.05, TICKLEN=0, $
              major=0, NTICKS=request_tick_no, thick=1.0, BIGPLOT=bigplot, FIVEYEAR=fiveyear
;timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=1.05, TICKLEN=0, $
;              major=0, NTICKS=12, thick=1.0, BIGPLOT=bigplot, FIVEYEAR=fiveyear

if keyword_set(COLORNEW) then begin
  ;  RCJ 11Feb2020  This requires maintenance.  Replacing with 'older' and 'newer', 
  ;       no better idea at the moment
  ;xyouts,((xs/4)+6),20, '= Data added/updated since Sept. 2003', /device
  ;plots,(xs/4),22,/device,psym=6, thick=4, color=recent_data_color
  ;xyouts,((xs - (xs/3))+6),20, '= Prior to Sept. 2003', /device
  ;plots,(xs - (xs/3)),22,/device,psym=6, thick=4, color=c
  xyouts,((xs/4)+6),20, '= Newer data', /device
  plots,(xs/4),22,/device,psym=6, thick=4, color=cgcolor(recent_data_color)
endif

generated = 'Generated by CDAWeb on ' + systime()
xyouts,(xs/2),3, generated, /device, alignment=0.5

;TJK added major=1 above to add a grid on the x axis

if (keyword_set(GIF) or keyword_set(PNG)) then begin
  deviceclose & set_plot,'X'
endif
return,0
end









