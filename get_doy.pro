;
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
FUNCTION get_doy,day,month,year

    days_per_month = [31,28,31,30,31,30,31,31,30,31,30,31]

    doy=0
    FOR i=0,month-2 DO BEGIN
        doy = doy + days_per_month[i]
    ENDFOR
    doy = doy + day

    is_leap_year = chk_leap_year(year)
    IF(is_leap_year) AND (month GT 2) THEN doy=doy+1

    RETURN,doy

END
