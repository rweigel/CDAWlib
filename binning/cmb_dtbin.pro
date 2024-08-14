
function cmb_dtbin,dt_sec, cdfepochtype=cdfepochtype
; convert dt_sec to cdfepochtype
; dtbin = cmb_dtbin( dt_sec, cdfepochtype=d.epoch.cdftype)
   case strupcase(cdfepochtype) of
   'CDF_EPOCH':begin
       dtbin = dt_sec*1d3 ; convert to milli-sec
       end
   'CDF_EPOCH16':begin
       dtbin = dt_sec*1d9 ; convert to nano-sec because CDF_EPOCH16 is internally converted to CDF_TIME_TT2000
       end
   'CDF_TIME_TT2000':begin
       dtbin = dt_sec*1d9 ;convert nano-sec
       dtbin = long64(dtbin)
       end   
    'JULIAN':begin
       dtbin = dt_sec/(24d0*3600d0) ; convert to days
       end
    else: dtbin=0
   endcase
return, dtbin
end