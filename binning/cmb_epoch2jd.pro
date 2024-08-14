;
;Copyright 1996-2013 United States Government as represented by the
;Administrator of the National Aeronautics and Space Administration.
;All Rights Reserved.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;

function cmb_epoch2jd,times,inverse=inverse,to_epoch_type=to_epoch_type
; assumption is that a input times 'times' are in the same system, i.e. CDF_EPOCH, CDF_EPOCH16, ..
;NOTE this routine requires modification when the time range straddles a leap second if epoch is CDF_TIME_TT2000 long64.
;given epoch 't' compute jd
;if inverse set then compute epoch given jd 't'
; NOTE: the precission of double precission Julian day is about 20 micro seconds.
if n_elements(inverse) eq 0 then inverse = 0

if inverse eq 0 then begin ; convert epoch to Julian days
   return, CDF_EPOCH_TOJULDAYS(times)
endif else begin           ;convert Julian days to epoch
;   vartype = ['DOUBLE',   'DCOMPLEX',   'LONG64']
;   CDFTYPE = ['CDF_EPOCH','CDF_EPOCH16','CDF_TIME_TT2000']
   if n_elements(to_epoch_type) eq 0 then to_epoch_type='CDF_EPOCH'
   CALDAT, times, Month, Dom, Year, Hr, Minu, Sec
   msec = (sec- floor(sec))*1000.
   isec = fix(sec)
   microsec = (msec- floor(msec))*1000.
   imsec = fix(msec)
   ;help, Month, Dom, Year, Hr, Minu, Sec, msec, microsec
   case to_epoch_type of
   'CDF_EPOCH':begin
       ;CDF_EPOCH, epochs1,Year,month,dom,hr,minu,sec, /COMPUTE_EPOCH
       CDF_EPOCH, epochs,Year,month,dom,hr,minu,isec,msec, /COMPUTE_EPOCH
       ;print,'CDF_EPOCH:',max( abs(epochs-epochs1) ) ; they don't agree, go with the later
       ;stop
       end
   'CDF_EPOCH16':begin
       CDF_EPOCH16, epochs,Year,month,dom,hr,minu,isec,msec,microsec, /COMPUTE_EPOCH
       ;CDF_EPOCH16, epochs1,Year,month,dom,hr,minu,sec, /COMPUTE_EPOCH
       ;print,'CDF_EPOCH16:',max( abs(real_part(epochs-epochs1)) ) ; they agree, go with the later
       ;print,'CDF_EPOCH16:',max( abs(imaginary(epochs-epochs1)) ) ; they don't agree, go with the later
       ;stop
       end
   'CDF_TIME_TT2000':begin
       ;CDF_TT2000, epochs1,Year,month,dom,hr,minu,sec, /COMPUTE_EPOCH
       CDF_TT2000, epochs,Year,month,dom,hr,minu,isec,imsec,microsec, /COMPUTE_EPOCH 
       ;print,'TT:',max( abs(epochs-epochs1) ) ;both agree
       ;stop
       end
   endcase
   return,epochs
endelse
return,0
end