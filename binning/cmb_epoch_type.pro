function cmb_epoch_type,epoch
; epochtype = cmb_epoch_type(epoch)
epochtypes = ['CDF_EPOCH','CDF_EPOCH16','CDF_TIME_TT2000']
vartypes   = ['DOUBLE',      'DCOMPLEX',         'LONG64']
ip = where( cmb_var_type(epoch[0]) eq vartypes)
ip=ip[0]
if ip eq -1 then return,'UNIDENTIFIED'
return, epochtypes[ip]
end
