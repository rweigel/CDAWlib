;+
;  Generates y-axis labels for time series plots with the plotmerge option
;
;-------------------------------------------------------------------------------;
FUNCTION get_labels, j, num_panels, variable,elist,PLOTMERGE=plotmerge,COMBINE=COMBINE,SINGLE=SINGLE
  compile_opt idl2, hidden
  YTAGS = tag_names(variable)
  a = tagindex('FIELDNAM',YTAGS)
  if (a[0] ne -1) then ylabel = variable.(a[0])

  a = tagindex('LABLAXIS',YTAGS)
  if (a[0] ne -1) then begin ylabel = variable.(a[0])
endif

a = tagindex('LABL_PTR_1',YTAGS)
if (a[0] ne -1)  then begin
  if (elist[j] lt n_elements(variable.(a[0]))) then begin
    q=where(variable.(a[0]) ne '')
    if q[0] ne -1 then begin
      if (variable.(a[0])[elist[j]] ne '') then ylabel = variable.(a[0])[elist[j]]
    endif
  endif
endif

a = tagindex('UNITS',YTAGS)
if (a[0] ne -1) then yunits = variable.(a[0])

a = tagindex('UNIT_PTR',YTAGS)
if (a[0] ne -1) then begin
  if (elist[j] lt n_elements(variable.(a[0]))) then begin
    q=where(variable.(a[0]) ne '')
    if q[0] ne -1 then begin
      if (variable.(a[0])[elist[j]] ne '') then yunits = variable.(a[0])[elist[j]]
    endif
  endif
endif

varindex = STRPOS(variable.varname,'_')
varname =  STRMID(variable.varname,varindex +1)

a = tagindex('SOURCE_NAME',YTAGS)
varsource = (a[0] ne -1 ) ? variable.source_name : ""

ydataset =String(Replicate(32B, 1024))
a = tagindex('LOGICAL_SOURCE',YTAGS)
if (a[0] ne -1) then  ydataset = variable.(a[0])


unit = plotmerge eq 1? yunits : STRPOS(yunits,' ') ne -1 ?  STRMID(yunits,0,STRPOS(yunits,' ')): yunits

sb = (STRSPLIT( ylabel,'-',/EXTRACT))[0]

combinedTitle = STRMID(ydataset,STRPOS(ydataset,'_')  +1)

yLeftLabel = plotmerge eq 1? keyword_set(COMBINE)?$
  ydataset+ '!C'+  unit :  unit:$
  keyword_set(COMBINE)?$
  combinedTitle + '!C' + sb  + '!C' + unit: $
  sb  + '!C' + unit

yRightLabel = plotmerge eq 1? ylabel: $
  Keyword_Set(single)? ydataset : $
  STRUPCASE((STRSPLIT(varsource,'>',/EXTRACT))[0])

if (num_panels eq 1 && plotmerge eq 1 && keyword_set(COMBINE) ) then return,{yLeftLabel:ydataset +'!C' + ylabel  +'!C' +  unit ,yRightLabel:""}
if (num_panels eq 1 && plotmerge eq 1) then return,{yLeftLabel: ylabel  +'!C' +  unit ,yRightLabel:""}
return,  {yLeftLabel:yLeftLabel,yRightLabel:yRightLabel}
end


;+------------------------------------------------------------------------
;$Author: tkovalic $
;$Date: 2021/09/09 15:58:52 $
;$Header: /home/cdaweb/dev/control/RCS/plot_over.pro,v 1.49 2021/09/09 15:58:52 tkovalic Exp tkovalic $
;$Locker: tkovalic $
;$Revision: 1.49 $
;+------------------------------------------------------------------------
; Description:
;    To generate a time series plot that overlay the components on one plot.
;
; Params:
;    Xvar = structure containing the Epoch variable structure of the
;                 type returned by the read_mycdf structure.
;    Yvar = structure containing the variable to be plotted against
;                 the Epoch variable in the Xvar parameter
;
; Keywords:
;    TSTART
;       Forces the time axis to begin at this Epoch time
;
;    TSTOP
;       Forces the time axis to end at this Epoch time
;
;    ELEMENTS
;        if set, then only these elements of a dimensional variable
;                  will be plotted.
;
;    POSITION
;       If set, this routine will draw the plot(s) at this position
;                  of an existing window, rather than open a new one
;    FIRSTPLOT
;       Use this key in conjunction with the position keyword. Set
;                  this flag to indicate that the variable is the first in the
;                  window
;
;    LASTPLOT
;       Use this key in conjunction with the position keyword. Set
;                  this flag to indicate that the variable is the last in the
;                  window
;
;    PANEL_HEIGHT
;       vertical height, in pixels, of each panel
;
;    CDAWEB
;       If set, the plot will have sufficient margin along the Z
;          axis to hold a colorbar
;
;    GIF
;       If set, the plot will be a .gif file instead of Xwindow
;
;    PNG
;       If set, the plot will be a .png file instead of Xwindow
;
;    XSIZE
;       if set, forces the plot window to this width
;
;    YSIZE
;       if set, forces the plot window to this height
;
;    AUTO
;       if set, turns auto-scaling on
;
;    NOGAPS
;       if set, eliminates data gap scanning
;
;    NONOISE
;       if set, filter out values outside 3 sigma from mean
;
;    IGNORE_DISPLAY_TYPE
;       if set, causes the attribute display_type to be ignored
;
;    NOSUBTITLE
;       if set, will not print 'time range = ' subtitle even after
;          the last graph. Needed for timetext case
;
;    ONLYLABEL
;       if set, graph position is calculated but graph is not
;          plotted. However, the x-axis label *is* plotted.
;          Utilized by timetext case
;
;    SCATTER
;       if set, display a scatter plot (each point is plotted as a
;          dot, no lines connect the dots
;
;    COMBINE
;       if set, need to add the dataset name to the y axis label
;
;    DEBUG
;       if set, turns on additional debug output.
;
;    PLOTMERGE
;       if set to 1 will combine the x,y,z vector components on one plot
;          if set to 2 will combine different datasets with commonality into one
;            plot
;
; Output:
;       out = status flag, 0=0k, -1 = problem occured.
;
;------------------------------------------------------------------
;
FUNCTION plot_over, Xvar, Yvar, $
  TSTART=TSTART,TSTOP=TSTOP,ELEMENTS=ELEMENTS,$
  POSITION=POSITION,PANEL_HEIGHT=PANEL_HEIGHT,$
  FIRSTPLOT=FIRSTPLOT,LASTPLOT=LASTPLOT,$
  CDAWEB=CDAWEB,GIF=GIF,PNG=PNG,NOSUBTITLE=NOSUBTITLE,$
  XSIZE=XSIZE,YSIZE=YSIZE, ONLYLABEL=ONLYLABEL,$
  AUTO=AUTO,NOGAPS=NOGAPS,NOVALIDS,$
  err_plus=err_plus,err_minus=err_minus, $
  IGNORE_DISPLAY_TYPE=IGNORE_DISPLAY_TYPE,$
  NONOISE=NONOISE,DEBUG=DEBUG,REPORT=REPORT,$
  SCATTER=SCATTER,NOERRORBARS=NOERRORBARS,$
  COMBINE=COMBINE,_EXTRA=EXTRAS, PLOTMERGE = PLOTMERGE
  compile_opt idl2

  status = 0


  if keyword_set(SCATTER) then psym = 3 else psym = 0
  ; If set, noerrorbars keyword overwrites err_plus and err_minus:
  if keyword_set(noerrorbars) then err_plus = (err_minus = 0)

  if keyword_set(REPORT) then begin & reportflag=1L
  a=size(REPORT) & if (a[n_elements(a)-2] eq 7) then $
    OPENW,1,REPORT,132,WIDTH=132
endif else reportflag=0L

if (n_params() ne 2) then begin
  print,'ERROR=Missing parameter to plot_series function' & return,-1
endif



;rval  = [ 35, 255,  105,   0, 213, 255,  76,  86, 153, 170, 120, 60,  16,  180, 205]
;gval  = [ 35,   0, 227,   0,  94, 128,   0, 180, 153,  68, 100, 100, 69,  100,  92]
;bval  = [ 35,   0,  54, 255,   0,   0, 153, 233,  51, 153,  39, 100, 100, 100,  92]

;CHARCOAL = Reform([rval[0],gval[0],bval[0]],1,3)
;RED = Reform([rval[1],gval[1],bval[1]],1,3)
;PURPLE =  Reform([rval[6] , gval[6] , bval[6]],1,3)
;CADETBLUE = Reform([rval[7] , gval[7] , bval[7]],1,3)
;SIENNA = Reform([rval[4] , gval[4] , bval[4]],1,3)
;GREEN = Reform([rval[2] , gval[2] , bval[2]],1,3)
;BLUE = Reform([rval[3] , gval[3] , bval[3]],1,3)
;ORANGE =  Reform([rval[5] , gval[5] , bval[5]],1,3)
;OLIVE = Reform([rval[8] , gval[8] , bval[8]],1,3)
;PINK =  Reform([rval[9] , gval[9] , bval[9]],1,3)
;DARKGREEN =  Reform([rval[10] , gval[10] , bval[10]],1,3)
;YELLOW =  Reform([rval[11] , gval[11] , bval[11]],1,3)
;CORAL = Reform([rval[12] , gval[12] , bval[12]],1,3)
;AQUA = Reform([rval[13] , gval[13] , bval[13]],1,3)
;INDIANRED = Reform([rval[14] , gval[14] , bval[14]],1,3)

;colors = [ CHARCOAL, RED, CADETBLUE, ORANGE,  PURPLE,  SIENNA, INDIANRED, GREEN, BLUE,   OLIVE, PINK, DARKGREEN,YELLOW,CORAL,AQUA]


color_index = [6,247,86,230,39,211,254,153,56,175,239,100,196,232,99]

;for c = 0 , (size(colors))[1] -1 do begin
;  if (n_elements(color_index) eq 0) then  color_index = [100 + c] else $
 ;   color_index = [color_index,100 + c]
 ; TVLCT, colors[c,*],color_index[c]
;endfor
 TVLCT, 30,30,30, color_index[0]



;theepoch_ptr = []
;thedata_ptr = []
;time_minmax = []

pad_front = BYTARR(n_tags(Xvar))
pad_end   = BYTARR(n_tags(Xvar))

for i = 0 , n_tags(Xvar) -1 do begin

  a = size(Xvar.(i))
  if (a[n_elements(a)-2] ne 8) then begin
    print,'ERROR=1st parameter to plot_timeseries not a structure' & return,-1
  endif else begin
    XTAGS = tag_names(Xvar.(i)) ; avoid multiple calls to tag_names
    a = tagindex('DAT',XTAGS)
    if (a[0] ne -1) then times = Xvar.(i).DAT $
    else begin
      a = tagindex('HANDLE',XTAGS)
      if (a[0] ne -1) then handle_value,Xvar.(i).HANDLE,times $
      else begin
        print,'ERROR=1st parameter does not have DAT or HANDLE tag' & return,-1
      endelse
      b = size(times)
      if ((b[n_elements(b)-2] eq 5) or (b[n_elements(b)-2] eq 9) or $
        (b[n_elements(b)-2] eq 14)) then begin

      endif else begin
        print,'ERROR=1st parameter datatype not a CDF TIME related type' & return,-1
      endelse
    endelse
  endelse

  tszck=size(times)
  if(tszck[tszck[0]+2] ne 1) then $ ; RTB added to prevent reform(scalar)
    times = reform(times) ; eliminate any redundant dimensions

  a = size(Yvar.(i))
  if (a[n_elements(a)-2] ne 8) then begin
    print,'ERROR=2nd parameter to plot_timeseries not a structure' & return,-1
  endif else begin
    YTAGS = tag_names(Yvar.(i)) ; avoid multiple calls to tag_names
    a = tagindex('DAT',YTAGS)
    if (a[0] ne -1) then THEDATA = Yvar.(i).DAT $
    else begin
      a = tagindex('HANDLE',YTAGS)

      if (a[0] ne -1) then begin
        handle_value,Yvar.(i).HANDLE,THEDATA

      endif else begin
        print,'ERROR=2nd parameter does not have DAT or HANDLE tag' & return,-1
      endelse
    endelse
  endelse
  szck=size(thedata)
  if(szck[szck[0]+2] ne 1) then $ ; RTB added to prevent reform(scalar)
    thedata = reform(thedata) ; eliminate any redundant dimensions

  a = size(thedata) & b = a[n_elements(a)-2] & thedata_size = a

  if ((b eq 0) OR (b gt 6 and b lt 12)) then begin
    print,'STATUS=datatype indicates that data is not plottable' & return,-1
  endif else begin

    data_dim = n_elements(data_dim) eq 0 ? a[0] : data_dim eq a[0] ? data_dim : -1
    if (data_dim eq -1) then begin
      str =string("")

      if (plotmerge eq 1) then by = string("Vector.") else by = string("Mission.")
      for k = 0 , n_tags(Xvar) -1 do  str += Yvar.(k).varname + ", "
      print, 'STATUS=Variable(s) ' + str +'have mixed dimensions and cannot be overlayed by '+ by +' Try again without overlay options.' & return,-1
;      print, 'STATUS=' + str +'have mixed dimensions and cannot be displayed on same graph, $
;        + String(13B) + 'possibly plottable without display options.' & return,-1
    endif
    case a[0] of
      0   : begin
        thedata=[thedata,thedata]
        a = size(thedata)
        b = a[n_elements(a)-2] & thedata_size = a
        times=[times,times]
        psym=4
        symsize=2
        print,'STATUS: Found one single point...'

        num_panels = 1L & elist=0
      end
      1   : begin
        num_panels = 1L & elist=0
        if (n_elements(thedata) ne n_elements(times)) then begin
          print,'STATUS=Re-select longer time interval; one value found for ',Yvar.(i).varname,' and not plottable.'
        endif
      end
      2   : begin ; #panels determined by dimensionality or by display type
        elist=indgen(a[1])
        if (n_elements(ELEMENTS) gt 0) then elist = ELEMENTS $
        else begin
          if NOT keyword_set(IGNORE_DISPLAY_TYPE) then begin
            b = tagindex('DISPLAY_TYPE',YTAGS)
            if (b[0] ne -1) then begin ; evaluate the display type
              c = strupcase(Yvar.(i).(b[0])) & c = break_mystring(c,delimiter='>')
              if ((c[0] eq 'TIME_SERIES')AND(n_elements(c) gt 1)) then begin
                d = break_mystring(c[1],delimiter=',')
                elist = long(d) & elist = elist -1
              endif
            endif
          endif
        endelse
      end
      else: begin
        print,'ERROR=Cannot plot data with > 2 dimensions' & return,-1
      end
    endcase
  endelse

  if (n_elements(thedata_ptr) eq 0) then  thedata_ptr = [Ptr_New(thedata)] else $
    thedata_ptr = [thedata_ptr, Ptr_New(thedata)]

  num_panels =  n_elements(elist)
  nogood_counter = intarr(num_panels)

  tbegin = times[0] & tend = times[n_elements(times)-1] ; default to data
  if keyword_set(TSTART) then begin ; set tbegin
    tbegin = TSTART & tbegin16 = TSTART & tbegintt = TSTART & a = size(TSTART)
    if (a[n_elements(a)-2] eq 7) then begin ;if tstart is a string, convert it
      split_ep=strsplit(TSTART,'.',/extract)
      tbegin = encode_CDFEPOCH(TSTART)
      tbegin16 = encode_CDFEPOCH(TSTART,/EPOCH16,msec=split_ep[1]);TJK added for use when data is epoch16
      tbegintt = encode_cdfepoch(TSTART, /TT2000, MSEC=split_ep[1]) ;TJK added for TT2000 time

    endif
  endif

  if keyword_set(TSTOP) then begin ; set tend
    tend = TSTOP & tend16 = TSTOP & tendtt = TSTOP & a = size(TSTOP)
    if (a[n_elements(a)-2] eq 7) then begin ;if tstop is a string, convert it
      split_ep=strsplit(TSTOP,'.',/extract)
      tend = encode_CDFEPOCH(TSTOP)
      tend16 = encode_CDFEPOCH(TSTOP,/EPOCH16,msec=split_ep[1]);TJK added for use when data is epoch16
      tendtt = encode_cdfepoch(TSTOP, /TT2000, MSEC=split_ep[1]) ;TJK added for TT2000 time
    endif
  endif

  pad_front[i] = 0L & pad_end[i] = 0L

  ep16 = 0 & eptt=0
;TJK 5/15/2023 - add in a case for when the epoch data is "double" to                                               
;                handle/convert the tstart/tstop's that can
;                come in as long64s(tt2000)                              
if (size(times[0],/tname) eq 'DOUBLE')then begin
    CDF_EPOCH,tbegin,byear,bmonth,bday,hour,minute,second,milli,/BREAK
    CDF_EPOCH,tbegin,byear,bmonth,bday,hour,minute,second,milli,/compute
    CDF_EPOCH,tend,eyear,emonth,eday,hour,minute,second,milli,/BREAK
    CDF_EPOCH,tend,eyear,emonth,eday,hour,minute,second,milli,/compute
 endif

  if (size(times[0],/tname) eq 'DCOMPLEX')then begin
    ep16 = 1
    tend = tend16
    tbegin = tbegin16
  endif
  if (size(times[0],/tname) eq 'LONG64')then begin
    eptt = 1
    tend = tendtt
    tbegin = tbegintt
  endif

  if (!version.release ge '6.2' and (ep16 or eptt)) then begin
    if (cdf_epoch_compare(times[0], tbegin)) then begin
      if keyword_set(DEBUG) then print,'Padding front of times...'
      times = [tbegin,times] & pad_front[i] = 1L
    endif
    if (cdf_epoch_compare(tend, times[n_elements(times)-1])) then begin
      if keyword_set(DEBUG) then print,'Padding end of times...'
      times = [times,tend] & pad_end[i] = 1L
    endif
  endif else begin
    print, 'tbegin, times0 timesN= ',tbegin, times[0], times[n_elements(times)-1]
    print, 'size of times ',n_elements(times)

    if (tbegin lt times[0]) then begin
      if keyword_set(DEBUG) then print,'Padding front of times...'
      times = [tbegin,times] & pad_front[i] = 1L
    endif
    if (tend gt times[n_elements(times)-1]) then begin
      if keyword_set(DEBUG) then print,'Padding end of times...'
      times = [times,tend] & pad_end[i] = 1L
    endif
  endelse

  rbegin = 0L & w = where((cdf_epoch_compare(times, tbegin) ge 0), wc)
  if (wc gt 0) then rbegin = w[0]

  rend = n_elements(times)-1 & w = where((cdf_epoch_compare(times, tend) le 0),wc)
  if (wc gt 0) then rend = w[n_elements(w)-1]
  if (rbegin ge rend) then begin
    print, 'rbegin and end = ', rbegin, rend
    print,'STATUS=No data within specified time range.'
  endif

  if not (keyword_set(nosubtitle)) then begin
    if (not eptt) then begin

      CDF_EPOCH,tbegin,byear,bmonth,bday,hour,minute,second,milli,/BREAK
      CDF_EPOCH,tend,eyear,emonth,eday,hour,minute,second,milli,/BREAK
    endif else begin ;if tt2000, can still call cdf_epoch but need to specifiy tointeger
      CDF_EPOCH,tbegin,byear,bmonth,bday,hour,minute,second,milli,/TOINTEGER,/BREAK
      CDF_EPOCH,tend,eyear,emonth,eday,hour,minute,second,milli,/TOINTEGER,/BREAK
    endelse

    ical,byear,doy,bmonth,bday,/idoy

    subtitle = 'TIME RANGE='+strtrim(string(byear),2)+'/'+strtrim(string(bmonth),2)
    subtitle = subtitle + '/' + strtrim(string(bday),2)
    subtitle = subtitle + ' (' + strtrim(string(doy),2) + ') to '

    ical,eyear,doy,emonth,eday,/idoy
    subtitle = subtitle + strtrim(string(eyear),2)+'/'+strtrim(string(emonth),2)
    subtitle = subtitle + '/' + strtrim(string(eday),2)
    subtitle = subtitle + ' (' + strtrim(string(doy),2) + ')'
  endif else subtitle=''

  if (not eptt) then begin
    CDF_EPOCH,tbegin,year,month,day,hour,minute,second,milli,/BREAK
    CDF_EPOCH,a,year,month,day,0,0,0,0,/COMPUTE_EPOCH ;a is the beginning of the day
  endif else begin
    CDF_EPOCH,tbegin,year,month,day,hour,minute,second,milli,/BREAK, /TOINTEGER
    CDF_EPOCH,a,year,month,day,0,0,0,0,/COMPUTE,/TT2000 ;a is the beginning of the day
  endelse

  if (ep16)then begin

    CDF_EPOCH16,b,year,month,day,0,0,0,0,0,0,0,/COMPUTE_EPOCH ;a for epoch16

    if keyword_set(DEBUG) then tatime = systime(1)
    ep_diff = cdf_epoch_diff (times, b, /micro_seconds)
    temp2 = ep_diff/1000000.d0
    if keyword_set(DEBUG) then print, 'Took ',systime(1)-tatime, ' seconds to compute time difference WITH NEW cdf_epoch_diff'
    times = temp2

  endif else if (eptt) then begin ;this is the computation for cdf_tt2000
    times  = (times - a) / 1000000000.d0 ; int in seconds from first of day

  endif else begin
    times  = (times - a) / 1000.d0 ; double in seconds from first of day
  endelse

  julday = ymd2jd(year,month,day)

  xranger = dblarr(2)

  if (ep16 or eptt) then begin
    xranger[0] = times[0]
    xranger[1] = times[n_elements(times)-1]

  endif else begin

    xranger[0] = (tbegin-a)/1000
    if ((tbegin-a) eq 0 and (n_elements(times) le 50)) then xranger[0] = times[1]
    xranger[1] = (tend-a)/1000
  endelse
  trange = xranger[1] - xranger[0]
  if (trange gt 0.0 and trange lt 1.0) then tform='h$:m$:s$.f$@y$ n$ d$' $
  else tform='h$:m$:s$@y$ n$ d$'
  ;theepoch_ptr = [theepoch_ptr, Ptr_New(times)]
  if (n_elements(theepoch_ptr) eq 0) then  theepoch_ptr = [Ptr_New(times)] else $
    theepoch_ptr = [theepoch_ptr, Ptr_New(times)]

  ;time_minmax = [[time_minmax], [rbegin,rend]]
  if (n_elements(time_minmax) eq 0) then  time_minmax = [[rbegin,rend]] else $
    time_minmax = [[time_minmax], [rbegin,rend]]


  if keyword_set(err_plus) and keyword_set(err_minus)  then begin
    if (n_elements(*err_plus[i]) ne n_elements(thedata) or $
      n_elements(*err_minus[i]) ne n_elements(thedata)) then begin
      err_plus=0
      err_minus=0
      print,'Plot_timeseries: Could not plot error bars'
    endif
  endif

endfor

if keyword_set(POSITION) then begin ; adding to existing plot
  a = size(POSITION) & b = n_elements(a)
  if ((a[b-1] ne 4)OR(a[b-2] ne 3)) then begin
    print,'ERROR=Invalid value for POSITION keyword' & return,-1
  endif
  if keyword_set(PANEL_HEIGHT) then begin ; verify it
    a = size(PANEL_HEIGHT) & b = n_elements(a)
    if ((a[b-2] le 1)OR(a[b-2] gt 5)) then begin
      print,'ERROR=Invalid value for PANEL_HEIGHT keyword' & return,-1
    endif else psize = PANEL_HEIGHT
  endif else begin
    print,'ERROR=PANEL_HEIGHT keyword must be specified with POSITION keyword'
    return,-1
  endelse
  if keyword_set(FIRSTPLOT) then clear_plot = 0L else clear_plot = 1L
  new_window = 0L ; no new window needed
endif

if (new_window eq 1) then begin
  if keyword_set(GIF) or keyword_set(PNG) then begin
    xs = 640 & ys = 512 & psize = 100 ; set default gif sizes
    if keyword_set(XSIZE) then xs = XSIZE ; override if keyword present
    if keyword_set(YSIZE) then ys = YSIZE ; override if keyword present
    if keyword_set(PANEL_HEIGHT) then begin
      psize = PANEL_HEIGHT & ys = (psize * num_panels) + 100
    endif else psize = ((ys-100) / num_panels)
  endif else begin ; generating an X-window
    a = lonarr(2) & DEVICE,GET_SCREEN_SIZE=a ; get device resolution
    xs = (a[0]*0.66) & ys = (a[1]*0.66) ; compute defaults
    if keyword_set(XSIZE) then xs = XSIZE ; override if keyword present
    if keyword_set(YSIZE) then ys = YSIZE ; override if keyword present
    if keyword_set(PANEL_HEIGHT) then begin
      psize = PANEL_HEIGHT
      ys = (psize * num_panels) + 100
      if (ys gt a[1]) then begin
        print,'ERROR=Computed window Ysize greater than device resolution'
        return,-1
      endif
    endif else psize = ((ys-100) / num_panels)
  endelse
  if (psize lt 50) then begin ; sanity check for #pixels per panel
    print,'ERROR=Insufficient resolution for a ',num_panels,' panel plot'
    return,-1
  endif
endif

if keyword_set(POSITION) then ppos = POSITION $
else begin
  ppos    = fltarr(4)         ; create position array
  ppos[0] = 100               ; default plot x origin
  ppos[2] = (xs - 40)         ; default plot x corner
  ppos[1] = (ys - 30) - psize ; 1st plot y origin
  ppos[3] = (ys - 30)         ; 1st plot y corner
  if keyword_set(CDAWEB) then ppos[2] = xs - 100 ; set margin for spectrogram
endelse

if (new_window eq 1) then begin
  a = tagindex('SOURCE_NAME',YTAGS)
  if (a[0] ne -1) then b = Yvar.(0).SOURCE_NAME else b = ''
  a = tagindex('DESCRIPTOR',YTAGS)
  if (a[0] ne -1) then b = b + '  ' + Yvar.(0).DESCRIPTOR
  window_title = b
endif

if (new_window eq 1) then begin

  if keyword_set(GIF) then begin
    a = size(GIF) & if (a[n_elements(a)-2] ne 7) then GIF = 'idl.gif'
    deviceopen,6,fileOutput=GIF,sizeWindow=[xs,ys]
  endif else if keyword_set(PNG) then begin
    a = size(PNG) & if (a[n_elements(a)-2] ne 7) then PNG = 'idl.png'
    deviceopen,7,fileOutput=PNG,sizeWindow=[xs,ys]
  endif else begin ; open x-window display
    window,/FREE,XSIZE=xs,YSIZE=ys,TITLE=window_title
    clear_plot = 0L ; initialize clear plot flag
  endelse

endif

a = tagindex('FILLVAL',YTAGS)
Yfillval = 1.0e31
if (a[0] ne -1) then begin
  if (Yvar.(0).FILLVAL ne '') then Yfillval = Yvar.(0).FILLVAL
endif

iter_per_panel = plotmerge lt 2 ? num_panels-1 :n_tags(Yvar)-1

if (plotmerge eq 2) then begin

  for j=0,num_panels-1 do begin

    ;thetimes_ptr = []
    thetimes_ptr =  PTRARR(1)

    single = 0

    src = Where(Tag_Names(Yvar.(0)) EQ StrUpCase('SOURCE_NAME'), count) NE -1 ? $
      Yvar.(0).source_name: ""
    source = STRUPCASE((STRSPLIT( src,'>',/EXTRACT))[0])

    for i=1,iter_per_panel do begin
      src = Where(Tag_Names(Yvar.(i)) EQ StrUpCase('SOURCE_NAME'), count) NE -1 ? $
        Yvar.(i).source_name: ""
      single =  STRCMP(source, STRUPCASE((STRSPLIT( src,'>',/EXTRACT))[0]))

    endfor

    for i=0,iter_per_panel do begin

      YTAGS = tag_names(Yvar.(i))
      mytimes = *theepoch_ptr[i];

      if (size(*thedata_ptr[i], /n_dimensions) eq 1) then begin

        mydata = i eq 0?  [Ptr_New(*thedata_ptr[i])]:[mydata,Ptr_New(*thedata_ptr[i])]

        ;if keyword_set (err_plus) then myerr_plus = i eq 0?  [Ptr_New(*err_plus)]: [err_plus,Ptr_New(*err_plus)]
        ;if keyword_set (err_minus) then myerr_minus = i eq 0? [Ptr_New(*err_minus)]: [mydata,Ptr_New(*err_minus)]
	; RCJ 07May2018  Added '[i]' :
        if keyword_set (err_plus) then myerr_plus = i eq 0?  [Ptr_New(*err_plus[i])]: [err_plus,Ptr_New(*err_plus[i])]
        if keyword_set (err_minus) then myerr_minus = i eq 0? [Ptr_New(*err_minus[i])]: [mydata,Ptr_New(*err_minus[i])]
      endif else begin

        mydata = i eq 0? [Ptr_New((*thedata_ptr[i])[(elist[j]),*])]:[mydata,Ptr_New((*thedata_ptr[i])[(elist[j]),*])]

        if keyword_set (err_plus) then myerr_plus = i eq 0? [Ptr_New((*err_plus[i])[(elist[j]),*])]:$
          [myerr_plus,Ptr_New((*err_plus[i])[(elist[j]),*])]
        if keyword_set (err_minus) then myerr_minus = i eq 0? [Ptr_New((*err_minus[i])[(elist[j]),*])]:$
          [myerr_minus,Ptr_New((*err_minus[i])[(elist[j]),*])]
      endelse

      *mydata[i] = reform(*mydata[i]) ; remove any extraneous dimensions

      if keyword_set (err_plus) then *myerr_plus[i]=reform(*myerr_plus[i])
      if keyword_set (err_minus) then *myerr_minus[i]=reform(*myerr_minus[i])

      if (pad_front[i]) then begin
        *mydata[i] = [Yfillval,*mydata[i]] ; add fill point to front
        if keyword_set (err_plus) then *myerr_plus[i]=[(*myerr_plus[i])[0],*myerr_plus[i]]
        if keyword_set (err_minus) then *myerr_minus[i]=[(*myerr_minus[i])[0],*myerr_minus[i]]
      endif
      if (pad_end[i]) then begin
        *mydata[i] = [*mydata[i],Yfillval] ; add fill point to back
        if keyword_set (err_plus) then *myerr_plus[i]=[*myerr_plus[i],(*myerr_plus[i])[n_elements(*myerr_plus[i])-1]]
        if keyword_set (err_minus) then *myerr_minus[i]=[*myerr_minus[i],(*myerr_minus[i])[n_elements(*myerr_minus[i])-1]]
      endif

      rrend=n_elements(*mydata[i])
      if(rrend lt time_minmax[1,i]) then time_minmax[1,i] = rrend-1

      *mydata[i] = (*mydata[i])[time_minmax[0,i]:time_minmax[1,i]]
      mytimes = (mytimes)[time_minmax[0,i]:time_minmax[1,i]]
      if keyword_set (err_plus) then *myerr_plus[i] = (*myerr_plus[i])[time_minmax[0,i]:time_minmax[1,i]]
      if keyword_set (err_minus) then *myerr_minus[i] = (*myerr_minus[i])[time_minmax[0,i]:time_minmax[1,i]]

      w = where(*mydata[i] ne Yfillval,non_fillcount)

      n_goodvals = 0 ; need to initialize
      if (non_fillcount ne 0) then n = where(finite((*mydata[i])[w]) eq 1,n_goodvals)

      if (non_fillcount ne 0 and n_goodvals gt 0) then begin

        *mydata[i] = ((*mydata[i])[w])[n] & mytimes = (mytimes[w])[n]

        if keyword_set (err_plus) then *myerr_plus[i]=((*myerr_plus[i])[w])[n]
        if keyword_set (err_minus) then *myerr_minus[i]=((*myerr_minus[i])[w])[n]
        w=0
      endif else begin
        w=0
        nogood_counter[j] =  1
        
        if (n_elements(ymin) eq 0) then begin
          ymin= 0.0
          ymax= 0.0
        endif else begin
          ymin = [ymin, 0.0]
          ymax = [ymax,0.0]
        endelse

      endelse
      if ((NOT keyword_set(NOVALIDS))AND(non_fillcount gt 0)and $
        (n_goodvals gt 0)) then begin
        Yvmin = 1.0e31
        Yvmax = 1.0e31
        a = tagindex('VALIDMIN',YTAGS)
        if (a[0] ne -1) then begin & b=size(Yvar.(i).VALIDMIN)
        if ( b [n_elements (b) - 2] ne 7 or Yvar.(i).VALIDMIN[0] ne '') then begin
          if (b[0] eq 0) then Yvmin = Yvar.(i).VALIDMIN $
          else Yvmin = Yvar.(i).VALIDMIN[elist[j]]
        endif
      endif
      a = tagindex('VALIDMAX',YTAGS)
      if (a[0] ne -1) then begin & b=size(Yvar.(i).VALIDMAX)
      if ( b [n_elements (b) - 2] ne 7 or Yvar.(i).VALIDMAX[0] ne '') then begin
        if (b[0] eq 0) then Yvmax = Yvar.(i).VALIDMAX $
        else Yvmax = Yvar.(i).VALIDMAX[elist[j]]
      endif
    endif

    w = where(((*mydata[i] gt Yvmax)OR(*mydata[i] lt Yvmin)),wc)

    if (wc gt 0) then begin

      w = where(((*mydata[i] le Yvmax)AND(*mydata[i] ge Yvmin)),wb)

      if (wb gt 0) then begin
        *mydata[i]=(*mydata[i])[w]  & mytimes=mytimes[w]

        if keyword_set (err_plus) then myerr_plus=myerr_plus[w]
        if keyword_set (err_minus) then myerr_minus=myerr_minus[w]
      endif else begin
        a = tagindex('FIELDNAM',YTAGS)
        if (a[0] ne -1) then ylabel = Yvar.(i).(a[0])
        print,'STATUS=No data for at least one component of ',ylabel,' variable.'

        nogood_counter[j] =  1
      endelse
    endif
  endif

  if (nogood_counter[j] eq 0) then begin

    if keyword_set(NONOISE) then begin

      sigminmax=three_sigma(*mydata[i])
      sigmin=sigminmax.(0)
      sigmax=sigminmax.(1)
      w = where(((*mydata[i] gt Sigmax)OR(*mydata[i] lt Sigmin)),wc)

      if (wc gt 0) then begin
        if keyword_set(DEBUG) then print,wc,' values outside 3-sigma...'
        w = where(((*mydata[i] le Sigmax)AND(*mydata[i] ge Sigmin)),wb)

        if (wb gt 0) then begin
          *mydata[i]=(*mydata[i])[w] & mytimes=mytimes[w]

          if keyword_set (err_plus) then *myerr_plus[i]=(*myerr_plus[i])[w]
          if keyword_set (err_minus) then *myerr_minus[i]=(*myerr_minus[i])[w]
        endif
      endif
    endif

    yscaletype = 0L ; initialize assuming natural
    a = tagindex('SCALETYP',YTAGS)
    if (a[0] ne -1) then begin
      if (strupcase(Yvar.(0).SCALETYP) eq 'LOG') then yscaletype = 1L
    endif
    ; screen non-positive data values if creating a logarithmic plot
    if (yscaletype eq 1) then begin
      wle = where(*mydata[i] le 0.0,wcle)
      if (wcle gt 0) then begin
        w = where(*mydata[i] gt 0.0,wc)
        if (wc gt 0) then begin ;if there are good values
          ;TJK 10/01/2004 - change from just throwing out values <=0 to reassigning them
          wmin = min((*mydata[i])[w]);get smallest real value above zero
          wmin = wmin/2 ; make it less than the real smallest value - TJK 10/22/2004
          (*mydata[i])[wle] = wmin
          w = where(*mydata[i] gt 0.0,wc)
          if (wc gt 0) then begin ;if there are good values
            if keyword_set (err_plus) then *myerr_plus[i]=(*myerr_plus[i])[w]
            if keyword_set (err_minus) then *myerr_minus[i]=(*myerr_minus[i])[w]
          endif
          w=0
        endif
      endif
    endif
    goodvals = where(finite(*mydata[i]) eq 1, ngoodvals)

    if (n_elements(ymin) eq 0) then begin
      ymin= min((*mydata[i])[goodvals])
      ymax= max((*mydata[i])[goodvals])
    endif else begin
      ymin = [ymin, min((*mydata[i])[goodvals])]
      ymax = [ymax, max((*mydata[i])[goodvals])]
    endelse


    a = tagindex('SCALEMIN',YTAGS)
    if (a[0] ne -1) then begin & b=size(Yvar.(i).SCALEMIN)
    if (b [n_elements (b) - 2] ne 7 or Yvar.(i).SCALEMIN[0] ne '') then begin
      if (b[0] eq 0) then ymin[i] = Yvar.(i).SCALEMIN $
      else ymin[i] = Yvar.(i).SCALEMIN[elist[j]]
    endif
  endif
  a = tagindex('SCALEMAX',YTAGS)
  if (a[0] ne -1) then begin & b=size(Yvar.(i).SCALEMAX)
  if (b [n_elements (b) - 2] ne 7 or Yvar.(i).SCALEMAX[0] ne '') then begin
    if (b[0] eq 0) then ymax[i] = Yvar.(i).SCALEMAX $
    else ymax[i] = Yvar.(i).SCALEMAX[elist[j]]
  endif
endif
if (keyword_set(AUTO)) then begin ; autoscale based on valid data values
  if (non_fillcount gt 0) then begin ; cant autoscale if all fill data
    ymax[i] = 0.0
    goodvals = where(finite(*mydata[i]) eq 1, ngoodvals)
    ymin[i] = min((*mydata[i])[goodvals])
    ymax[i] = max((*mydata[i])[goodvals])

  endif
endif

max_absolute_diff = 1.000e-20
max_relative_diff = 0.00001
expand_axis = 0
if  (ymax[i] - ymin[i] lt max_absolute_diff) then begin
  expand_axis = 1
endif else begin
  relative_diff = (ymax[i] - ymin[i]) / abs (ymax[i])
  if  (relative_diff le max_relative_diff) then expand_axis = 1
endelse

if (expand_axis) then begin

  ymax[i] = ymax[i] + (ymax[i] * .1)
  ymin[i] = ymin[i] - (ymin[i] * .1)

  if ymax[i] eq 0. and ymin[i] eq 0. then begin
    yscaletype=0
    case SIZE( ymax[i], /TYPE) of
      4:
      5: begin
        ymin[i]=-.1 & ymax[i] = .1
      end
      else : begin
        ymin[i]=-0 & ymax[i]=1
      endelse
    endcase
  endif
endif

yranger = i eq 0 ? [ymin[i],ymax[i]] : [yranger[0] <ymin[i], yranger[1] > ymax[i]]

if (yscaletype ne 1) then begin
  if keyword_set(err_minus) or keyword_set(err_plus) then $
    print,'Adjusting ymin and ymax according to error bars...'
  if keyword_set (err_minus) then  begin
    q=where(*mydata[i] eq ymin[i])

    if q[0] ne -1 then yranger[0]= yranger[0]-max((*myerr_minus[i])[q]) $
    else yranger[0] = yranger[0]

  endif
  if keyword_set (err_plus) then begin
    q=where(*mydata[i] eq ymax[i])

    if q[0] ne -1 then yranger[1] = yranger[1]+max((*myerr_plus[i])[q]) $
    else yranger[1] = yranger[1]
  endif
endif

if (PTR_VALID(thetimes_ptr[0])eq 0)  then  thetimes_ptr = [Ptr_New(mytimes)] else $
  thetimes_ptr = [thetimes_ptr, Ptr_New(mytimes)]

if ((yscaletype eq 1)AND(yranger[0] le 0)) then yranger[0] = 0.00001
endif
endfor

for i=0,iter_per_panel do begin
  if (nogood_counter[j] eq 0) then begin

    ylabel = '' & yunits = '' & yds = '' ; initialize

    if keyword_set(COMBINE) then begin
      a = tagindex('LOGICAL_SOURCE',YTAGS)
      if (a[0] ne -1) then yds = strupcase(Yvar.(i).(a[0]))
    endif

    a = tagindex('AUGMENT_LABL',YTAGS)
    if (a[0] ne -1) then begin
      if (strupcase(yvar.(i).(a[0])) eq 'TRUE') then begin
        a = tagindex('VARNAME',YTAGS)
        if (a[0] ne -1) then begin
          if (n_elements(yds) gt 0) then yds = yds + Yvar.(i).(a[0]) else $
            yds = Yvar.(i).(a[0])
        endif
      endif
    endif

    if keyword_set(onlylabel) then begin

      plot,(*thetimes_ptr[i]),*mydata[i],XSTYLE=4+1,ystyle=4+1,/NODATA,$
        XRANGE=xranger,POSITION=ppos,/DEVICE,NOERASE=clear_plot
      !y.crange[0]=!y.crange[1]
      timeaxis_text,JD=julday,form=tform,/onlylabel
      goto, skipped_graph
    endif else begin

      labels = get_labels(j,iter_per_panel,Yvar.(i),elist, PLOTMERGE  = plotmerge,COMBINE=COMBINE,SINGLE = single)

     first = 0
     mycolor = i  lt n_elements (color_index) ? color_index[i] : Long((!d.table_size -1)*RANDOMU(seed,1))
     
     s =  overlay (i,iter_per_panel,*thetimes_ptr[i],*mydata[i],YUNIT = labels.yLeftLabel,YTITLE= labels.yRightLabel,YRANGE=yranger,$
        YLOG=yscaletype,XRANGE=xranger,POSITION=ppos, NOGAPS= NOGAPS, $
        NOERASE=clear_plot,_EXTRA=EXTRAS,ytick_get=yticks,psize,color=mycolor,psym, first, non_fillcount)
    endelse

    if keyword_set(err_plus) and keyword_set(err_minus) then begin
      mycolor = i  lt n_elements (color_index) ? color_index[i] : 60
      cdaweb_errplot,*thetimes_ptr[i],*mydata[i]-*myerr_minus[i],*mydata[i]+*myerr_plus[i],color=mycolor
    endif

    timeaxis_text,JD=julday,/NOLABELS,TICKLEN=-2.0

  endif else begin

    if (PTR_VALID(thetimes_ptr[0])eq 0)  then  thetimes_ptr = [Ptr_New(mytimes)] else $
      thetimes_ptr = [thetimes_ptr, Ptr_New(mytimes)]

    if(n_elements(*thetimes_ptr[i] eq 1))then  *thetimes_ptr[i] = [((*thetimes_ptr[i]))[0],((*thetimes_ptr[i]))[0] + 1]
    plot,*thetimes_ptr[i],[0,1],/nodata,/device,position=ppos,yticks=1,xticks=1,xminor=1,yminor=1,$
      noerase=clear_plot, xstyle=4+1
    plots,[ppos[0],ppos[2]],[ppos[3],ppos[3]] , /device
    a = tagindex('FIELDNAM',YTAGS)
    if (a[0] ne -1) then ylabel = Yvar.(i).(a[0])
    a = tagindex('LABLAXIS',YTAGS)
    if (a[0] ne -1) then ylabel = Yvar.(i).(a[0])
    a = tagindex('LABL_PTR_1',YTAGS)
    if (a[0] ne -1) then begin
      if (Yvar.(i).(a[0])[0] ne '') then ylabel = Yvar.(i).(a[0])[elist[j]]
    endif
    xyouts, (ppos[2]-ppos[0])/2,ppos[1]+((ppos[3]-ppos[1])/2), /device, $
      'No good values to display for ' + ylabel

  endelse

endfor
for i=0,iter_per_panel  do begin
  ptr_free,thetimes_ptr[i]
  ptr_free, mydata[i]
  if keyword_set (err_plus)  then ptr_free, myerr_plus[i]
  if keyword_set (err_minus)  then ptr_free, myerr_minus[i]
endfor

ppos[3] = ppos[1] & ppos[1] = ppos[1] - psize & clear_plot=1

endfor

endif else begin

  ; extract data for a single panel from the data array
  for j=0,num_panels-1 do begin

    mydata = j eq 0? thedata_size[0] eq 1 ? [Ptr_New(thedata)]: [Ptr_New(thedata[(elist[j]),*])]$
      :thedata_size[0] eq 1 ? [mydata,Ptr_New(thedata)]: [mydata,Ptr_New(thedata[(elist[j]),*])]

    if keyword_set (err_plus) then myerr_plus = j eq 0? thedata_size[0] eq 1 ?  [Ptr_New(*err_plus)]: [Ptr_New((*err_plus)[(elist[j]),*])]$
      :thedata_size[0] eq 1 ? [myerr_plus,Ptr_New(*err_plus)]: [myerr_plus,Ptr_New((*err_plus)[(elist[j]),*])]

    if keyword_set (err_minus) then myerr_minus = j eq 0? thedata_size[0] eq 1 ? [Ptr_New(*err_minus)]: [Ptr_New((*err_minus)[(elist[j]),*])]$
      :thedata_size[0] eq 1 ? [myerr_minus,Ptr_New(*err_minus)]: [myerr_minus,Ptr_New((*err_minus)[(elist[j]),*])]

    *mydata[j] = reform(*mydata[j] ); remove any extraneous dimensions
    if keyword_set (err_plus) then *myerr_plus[j]=reform(*myerr_plus[j])
    if keyword_set (err_minus) then *myerr_minus[j]=reform(*myerr_minus[j])
    ; pad the beginning and end of data if extra time points were added
    if (pad_front[0]) then begin
      *mydata[j]= [Yfillval, *mydata[j]] ; add fill point to front
      if keyword_set (err_plus) then *myerr_plus[j]=[(*myerr_plus[j])[0],*myerr_plus[j]]
      if keyword_set (err_minus) then *myerr_minus[j]=[(*myerr_minus[j])[0],*myerr_minus[j]]
    endif
    if (pad_end[0]) then begin
      *mydata[j] =  [*mydata[j],Yfillval] ; add fill point to back
      if keyword_set (err_plus) then *myerr_plus[j]=[*myerr_plus[j],(*myerr_plus[j])[n_elements(*myerr_plus[j])-1]]
      if keyword_set (err_minus) then *myerr_minus[j]=[*myerr_minus[j],(*myerr_minus[j])[n_elements(*myerr_minus[j])-1]]
    endif

    rrend=n_elements(*mydata[j])

    if(rrend lt rend) then rend = rrend-1

    *mydata[j] = (*mydata[j])[rbegin:rend]
    if keyword_set (err_plus) then *myerr_plus[j] = (*myerr_plus[j])[rbegin:rend]
    if keyword_set (err_minus) then *myerr_minus[j] = (*myerr_minus[j])[rbegin:rend]
    mytimes = times[rbegin:rend]

    w = where(*mydata[j] ne Yfillval,non_fillcount)

    n_goodvals = 0 ; need to initialize
    if (non_fillcount ne 0) then n = where(finite((*mydata[j])[w]) eq 1,n_goodvals)

    if (non_fillcount ne 0 and n_goodvals gt 0) then begin
      *mydata[j] = ((*mydata[j])[w])[n] & mytimes = (mytimes[w])[n]

      if keyword_set (err_plus) then *myerr_plus[j]=((*myerr_plus[j])[w])[n]
      if keyword_set (err_minus) then *myerr_minus[j]=((*myerr_minus[j])[w])[n]
      w=0
    endif else begin
      w=0
      nogood_counter[j] =  1
      if (n_elements(ymin) eq 0) then begin
        ymin= 0.0
        ymax= 0.0
      endif else begin
        ymin = [ymin, 0.0]
        ymax = [ymax,0.0]
      endelse
    endelse

    if ((NOT keyword_set(NOVALIDS))AND(non_fillcount gt 0)and $
      (n_goodvals gt 0)) then begin

      Yvmin = 1.0e31
      Yvmax = 1.0e31

      a = tagindex('VALIDMIN',YTAGS)

      if (a[0] ne -1) then begin & b=size(Yvar.(0).VALIDMIN)
      if ( b [n_elements (b) - 2] ne 7 or Yvar.(0).VALIDMIN[0] ne '') then begin
        if (b[0] eq 0) then Yvmin = Yvar.(0).VALIDMIN $
        else Yvmin = Yvar.(0).VALIDMIN[elist[j]]
      endif
    endif
    a = tagindex('VALIDMAX',YTAGS)
    if (a[0] ne -1) then begin & b=size(Yvar.(0).VALIDMAX)
    if ( b [n_elements (b) - 2] ne 7 or Yvar.(0).VALIDMAX[0] ne '') then begin
      if (b[0] eq 0) then Yvmax = Yvar.(0).VALIDMAX $
      else Yvmax = Yvar.(0).VALIDMAX[elist[j]]
    endif
  endif
  ; proceed with screening

  w = where(((*mydata[j] gt Yvmax)OR(*mydata[j] lt Yvmin)),wc)
  if (wc gt 0) then begin
    if keyword_set(DEBUG) then print,wc,' values outside VALIDMIN/MAX'
    w = where(((*mydata[j] le Yvmax)AND(*mydata[j] ge Yvmin)),wb)
    if (wb gt 0) then begin
      *mydata[j]=(*mydata[j])[w] & mytimes=mytimes[w]
      if keyword_set (err_plus) then *myerr_plus[j]=(*myerr_plus[j])[w]
      if keyword_set (err_minus) then *myerr_minus[j]=(*myerr_minus[j])[w]
    endif else begin
      a = tagindex('FIELDNAM',YTAGS)
      if (a[0] ne -1) then ylabel = Yvar.(0).(a[0])
      print,'STATUS=No data for at least one component of ',ylabel,' variable.'
      nogood_counter[j] =  1
     
    endelse
  endif
endif

if (nogood_counter[j] eq 0) then begin
  ; screen out data outsize 3 standard deviations from the mean
  if keyword_set(NONOISE) then begin
    ;semiMinMax,mydata,Sigmin,Sigmax
    ; RCJ 05/01/2006  Replaced call to semiminmax w/ call to three_sigma
    sigminmax=three_sigma(*mydata[j])
    sigmin=sigminmax.(0)
    sigmax=sigminmax.(1)
    w = where(((*mydata[j] gt Sigmax)OR(*mydata[j] lt Sigmin)),wc)
    if (wc gt 0) then begin
      if keyword_set(DEBUG) then print,wc,' values outside 3-sigma...'
      w = where(((*mydata[j] le Sigmax)AND(*mydata[j] ge Sigmin)),wb)
      if (wb gt 0) then begin
        *mydata[j]=(*mydata[j])[w] & mytimes=mytimes[w]
        if keyword_set (err_plus) then *myerr_plus[j]=(*myerr_plus[j])[w]
        if keyword_set (err_minus) then *myerr_minus[j]=(*myerr_minus[j])[w]
      endif
    endif
  endif

  yscaletype = 0L ; initialize assuming natural
  a = tagindex('SCALETYP',YTAGS)
  if (a[0] ne -1) then begin
    if (strupcase(Yvar.(0).SCALETYP) eq 'LOG') then yscaletype = 1L
  endif
  ; screen non-positive data values if creating a logarithmic plot
  if (yscaletype eq 1) then begin
    wle = where(*mydata[j] le 0.0,wcle)
    if (wcle gt 0) then begin
      w = where(*mydata[j] gt 0.0,wc)
      if (wc gt 0) then begin ;if there are good values
        ;TJK 10/01/2004 - change from just throwing out values <=0 to reassigning them
        wmin = min((*mydata[j])[w]);get smallest real value above zero
        wmin = wmin/2 ; make it less than the real smallest value - TJK 10/22/2004
        (*mydata[j])[wle] = wmin
        w = where(*mydata[j] gt 0.0,wc)
        if (wc gt 0) then begin ;if there are good values
          if keyword_set (err_plus) then *myerr_plus[j]=(*myerr_plus[j])[w]
          if keyword_set (err_minus) then *myerr_minus[j]=(*myerr_minus[j])[w]
        endif
        w=0
      endif
    endif
  endif

  goodvals = where(finite(*mydata[j]) eq 1, ngoodvals)

  if (n_elements(ymin) eq 0) then begin
    ymin= min((*mydata[j])[goodvals])
    ymax= max((*mydata[j])[goodvals])
  endif else begin
    ymin = [ymin, min((*mydata[j])[goodvals])]
    ymax = [ymax,max((*mydata[j])[goodvals])]
  endelse

  a = tagindex('SCALEMIN',YTAGS)
  if (a[0] ne -1) then begin & b=size(Yvar.(0).SCALEMIN)
  if (b [n_elements (b) - 2] ne 7 or Yvar.(0).SCALEMIN[0] ne '') then begin
    if (b[0] eq 0) then ymin[j] = Yvar.(0) .SCALEMIN $
    else ymin[j] = Yvar.(0).SCALEMIN[elist[i]]
  endif
endif
a = tagindex('SCALEMAX',YTAGS)
if (a[0] ne -1) then begin & b=size(Yvar.(0).SCALEMAX)
if (b [n_elements (b) - 2] ne 7 or Yvar.(0).SCALEMAX[0] ne '') then begin
  if (b[0] eq 0) then ymax[j] = Yvar.(0).SCALEMAX $
  else ymax[j] = Yvar.(0).SCALEMAX[elist[i]]
endif
endif
if (keyword_set(AUTO)) then begin ; autoscale based on valid data values
  if (non_fillcount gt 0) then begin ; cant autoscale if all fill data
    ymax[j] = 0.0
    ; replace this with next 2 lines to ignore NaN values ymin = min(mydata,MAX=ymax)
    goodvals = where(finite(*mydata[j]) eq 1, ngoodvals)
    ymin[j] = min((*mydata[j])[goodvals])
    ymax[j] = max((*mydata[j])[goodvals])
  endif
endif

max_absolute_diff = 1.000e-20
max_relative_diff = 0.00001

expand_axis = 0

if  (ymax[j] - ymin[j] lt max_absolute_diff) then begin
  expand_axis = 1
endif else begin
  relative_diff = (ymax[j] - ymin[j]) / abs (ymax[j])
  if  (relative_diff le max_relative_diff) then expand_axis = 1
endelse

if (expand_axis) then begin

  ymax[j] = ymax[j] + (ymax[j] * .1)
  ymin[j] = ymin[j] - (ymin[j] * .1)

  if ymax[j] eq 0. and ymin[j] eq 0. then begin
    yscaletype=0
    case SIZE( ymax[j], /TYPE) of
      4:
      5: begin
        ymin[j]=-.1 & ymax[j] = .1
      end
      else : begin
        ymin[j]=-0 & ymax[j]=1
      endelse
    endcase

  endif
endif

yranger=[ymin[j],ymax[j]]

if (yscaletype ne 1) then begin
  if keyword_set(err_minus) or keyword_set(err_plus) then $
    print,'Adjusting ymin and ymax according to error bars...'
  if keyword_set (err_minus) then  begin
    q=where(*mydata[j] eq ymin[j])

    if q[0] ne -1 then ymin[j]= ymin[j]-max((*myerr_minus[j])[q])
  endif
  if keyword_set (err_plus) then begin
    q=where(*mydata[j] eq ymax[j])
    if q[0] ne -1 then yranger[1] = ymax[j]+max((*myerr_plus[j])[q])
  endif
endif
if ((yscaletype eq 1)AND(yranger[0] le 0)) then yranger[0] = 0.00001


endif
endfor

for j=0,num_panels-1 do begin
  if(n_elements(ymin) gt 0 && n_elements(ymax)) then begin
    first = (where(nogood_counter eq 0))[0] 
    if(nogood_counter[j] eq 0) then begin
    yranger = j eq first ? [ymin[j],ymax[j]] : [yranger[0] <ymin[j], yranger[1] > ymax[j]]
    endif
  endif
endfor

for j=0,num_panels-1 do begin
  
 
  if (nogood_counter[j] eq 0) then begin
    ylabel = '' & yunits = '' & yds = '' ; initialize

    if keyword_set(COMBINE) then begin
      a = tagindex('LOGICAL_SOURCE',YTAGS)
      if (a[0] ne -1) then yds = strupcase(Yvar.(0).(a[0]))
    endif

    a = tagindex('AUGMENT_LABL',YTAGS)
    if (a[0] ne -1) then begin
      if (strupcase(yvar.(a[0])) eq 'TRUE') then begin
        a = tagindex('VARNAME',YTAGS)
        if (a[0] ne -1) then begin
          if (n_elements(yds) gt 0) then yds = yds + Yvar.(0).(a[0]) else $
            yds = Yvar.(0).(a[0])
        endif
      endif
    endif

    if keyword_set(onlylabel) then begin

      plot,mytimes,*mydata[j],XSTYLE=4+1,ystyle=4+1,/NODATA,$
        XRANGE=xranger,POSITION=ppos,/DEVICE,NOERASE=clear_plot
      !y.crange[0]=!y.crange[1]
      timeaxis_text,JD=julday,form=tform,/onlylabel
      goto, skipped_graph
    endif

    labels = get_labels(j,num_panels, Yvar.(0), elist,PLOTMERGE = plotmerge,COMBINE=COMBINE)
    first = (where(nogood_counter eq 0))[0] 
    mycolor = j  lt n_elements (color_index) ? color_index[j] : Long((!d.table_size -1)*RANDOMU(seed,1))

    s =  overlay (j,iter_per_panel,mytimes, *mydata[j],YUNIT = labels.yLeftLabel,YTITLE= labels.yRightLabel,YRANGE=yranger,$
      YLOG=yscaletype,XRANGE=xranger,POSITION=ppos, NOGAPS= NOGAPS, $
      NOERASE=clear_plot,_EXTRA=EXTRAS,ytick_get=yticks,psize,color = mycolor, psym, first, non_fillcount)
   
    if keyword_set(err_plus) and keyword_set(err_minus) then begin
      mycolor = j  lt n_elements (color_index) ? color_index[j] : 60
      cdaweb_errplot,mytimes,*mydata[j]-*myerr_minus[j],*mydata[j]+*myerr_plus[j],color =  mycolor
    endif

    timeaxis_text,JD=julday,/NOLABELS,TICKLEN=-2.0

  endif else begin

    if(n_elements(mytimes) eq 1)then  mytimes = [mytimes,mytimes[0] + 1]
    plot,mytimes,[0,1],/nodata,/device,position=ppos,yticks=1,xticks=1,xminor=1,yminor=1,$
      noerase=clear_plot, xstyle=4+1
    plots,[ppos[0],ppos[2]],[ppos[3],ppos[3]] , /device
    a = tagindex('FIELDNAM',YTAGS)
    if (a[0] ne -1) then ylabel = Yvar.(0).(a[0])
    a = tagindex('LABLAXIS',YTAGS)
    if (a[0] ne -1) then ylabel = Yvar.(0).(a[0])
    a = tagindex('LABL_PTR_1',YTAGS)
    if (a[0] ne -1) then begin
      if (Yvar.(0).(a[0])[0] ne '') then ylabel = Yvar.(0).(a[0])[elist[j]]
    endif
    xyouts, (ppos[2]-ppos[0])/2,ppos[1]+((ppos[3]-ppos[1])/2), /device, $
      'No good values to display for ' + ylabel
  endelse

  ;RCJ 10/01/2015  No longer using 'nogood'
  ;if not (nogood) then begin
  ; draw the time axis by default or if lastplot flag is specified
  if keyword_set(POSITION) then begin
    if keyword_set(LASTPLOT) then begin
      timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=0.9
      time_written = 1L
    endif
  endif else begin
    timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=0.9
    time_written = 1L
    if keyword_set(GIF) or keyword_set(PNG) then begin
      deviceclose
    endif
  endelse

endfor
for j=0,num_panels-1 do begin
  ptr_free, mydata[j]
  if keyword_set (err_plus) then ptr_free, myerr_plus[j]
  if keyword_set (err_minus) then ptr_free, myerr_minus[j]

endfor
ppos[3] = ppos[1] & ppos[1] = ppos[1] - psize & clear_plot=1
endelse

time_written = 0L
if keyword_set(POSITION) then begin
  if keyword_set(LASTPLOT) then begin
    timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=0.9
    time_written = 1L
  endif
endif else begin
  timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=0.9
  time_written = 1L
  if keyword_set(GIF) or keyword_set(PNG) then begin
    deviceclose
  endif
endelse

if (keyword_set(LASTPLOT) and not(time_written) and ((where(nogood_counter eq 0))[0]) eq -1) then begin ;still need to write the time axis
  timeaxis_text,FORM=tform,JD=julday,title=subtitle,CHARSIZE=0.9
endif

skipped_graph:

ptr_free, theepoch_ptr
ptr_free, thedata_ptr
;TVLCT, rVec, gVec, bVec
return,status
end


