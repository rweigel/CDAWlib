function cmb_string2epr,dates,cdfepochtypeout=cdfepochtype
;epr = cmb_string2epr(dates,cdftype=cdftype)
;CDFTYPE         STRING    'CDF_TIME_TT2000'
;dates = ['2013/12/31/ 23:59:55.753', '2014/01/01/ 23:59:51.999']

;Result = CDF_PARSE_EPOCH(Epoch_string) 
;Result = CDF_PARSE_EPOCH16(Epoch_string)
;Result = CDF_PARSE_TT2000(Epoch_string)

;help, cdfepochtype
for idate=0,n_elements(dates)-1 do begin
    date = dates[idate]
    a = strsplit(/ext,date,' ,/,:TZ-')
    yr = long(a[0])
    month = long(a[1])
    dom = long(a[2])
    hr = long(a[3])
    minu = long(a[4])
    sec = float(a[5])
    isec = floor(sec)
    msec = 1000*(sec-isec)
    ;help,date,yr,month,dom,hr,minu,isec,msec,sec
  case cdfepochtype of
  'CDF_EPOCH':begin
      cdf_epoch,epoch,yr,month,dom,hr,minu,isec,msec, /comp
      end
  'CDF_TIME_TT2000':begin
      CDF_TT2000,epoch,yr,month,dom,hr,minu,isec,msec, /comp
      end
  'CDF_EPOCH16':begin
      cdf_epoch16,epoch,yr,month,dom,hr,minu,isec,msec, /comp
      end
  endcase
  if idate eq 0 then epochs = epoch else epochs = [epochs,epoch]
endfor
return,epochs
end

