;-------------------------------------------------------------
; $Id: loglevels.pro,v 1.5 2015/06/05 18:08:40 ryurow Exp rcjohns1 $
;+
; NAME:
;        LOGLEVELS (function)
;
; PURPOSE:
;        Compute default values for logarithmic axis labeling
;        or contour levels. For a range from 1 to 100 these 
;        would be 1., 2., 5., 10., 20., 50., 100.
;        If the range spans more than (usually) 3 decades, only 
;        decadal values will be returned unless the /FINE keyword 
;        is set.
;
; CATEGORY:
;        Tools
;
; CALLING SEQUENCE:
;        result = LOGLEVELS([range | MIN=min,MAX=max] [,/FINE]
;[,COARSE=dec])
;
; INPUTS:
;        RANGE -> A 2-element vector with the minimum and maximum 
;            value to be returned. Only levels _within_ this range
;            will be returned. If RANGE contains only one element,
;            this is interpreted as MAX and MIN will be assumed as
;            3 decades smaller. RANGE superseeds the MIN and MAX 
;            keywords.
;
; KEYWORD PARAMETERS:
;        MIN, MAX -> alternative way of specifying a RANGE. If only 
;            one keyword is given, the other one is computed as
;            3 decades smaller/larger than the given parameter.
;            RANGE superseeds MIN and MAX.
;
;        /FINE -> always return finer levels (1,2,5,...) 
;
;        COARSE -> the maximum number of decades for which LOGLEVELS
;            shall return fine labels. Default is 3. (non-integer 
;            values are possible).
;
; OUTPUTS:
;        A vector with "round" logarithmic values within the given 
;        range.  The result should always contain at least three
;        elements (unless COURSE is set to a value less then 0).
;
;
; SUBROUTINES:
;        none
;
; REQUIREMENTS:
;        none
;
; NOTES:
;        If COARSE is lt 0, the nearest decades will be returned 
;        instead. The result will always have at least two elements.
;        If COARSE forces decades, the result values may be out-of-
;        range if RANGE spans less than a decade.
;        
;        Caution with type conversion from FLOAT to DOUBLE !!
;
; EXAMPLE:
;        range = [ min(data), max(data) ]
;        c_level = LOGLEVELS(range)
;        contour,...,c_level=c_level
;        
;
; MODIFICATION HISTORY:
;        Based function of the same name by Martin Schultz
;
;-


FUNCTION loglevels, range, min=minval, max=maxval, coarse=coarse, fine=fine
 
   ; Define demacation arrays.  These define which inter-decadal labels 
   ; we will return.  Note that demarc_x is experimental and not currently
   ; used.
   demarc_i = [1.0D, 2.0D, 5.0D]
   demarc_d = [1.0D] 
   demarc_x = [1.0D, 2.0D, 3.0D, 4.0D, 5.0D, 6.0D, 7.0D, 8.0D, 9.0D]

   ; Set course to a default value, if not set.
   IF  (N_ELEMENTS (coarse) eq 0) THEN coarse = 3.0

   ; Set fine to a default value, if not set.
   IF  N_ELEMENTS (fine) eq 0 THEN fine = 0

   ; Range superseeds MIN and MAX, so use these values first if we have them.
   IF  (N_ELEMENTS (range) gt 0) THEN BEGIN

       IF (N_ELEMENTS (range) eq 1) THEN BEGIN 

          maxval = range [0] 

       ENDIF ELSE BEGIN

          minval = range [0]
          maxval = range [1]

       ENDELSE

   ENDIF 

   ;  Check if minval is set, if it is then set maxval based on it,
   ;  if needed.
   IF  (N_ELEMENTS (minval) gt 0) THEN BEGIN

       minval = minval [0]
       IF (N_ELEMENTS (maxval) eq 0) THEN maxval = minval * 1000.
    
   ENDIF

   ;  Check if maxval is set, if it is then set minval based on it,
   ;  if needed.
   IF  (N_ELEMENTS (maxval) gt 0) THEN BEGIN

       maxval = maxval [0]
       IF (N_ELEMENTS (minval) eq 0) THEN minval  = maxval * 0.001

   ENDIF

   ; Nothing is defined i.e. neither min nor max was passed to us.
   IF  (N_ELEMENTS (minval) eq 0) THEN BEGIN
       minval = 0.1
       maxval = 100.
   ENDIF

   ; Do some checks
   IF  minval lt 0.0 THEN RETURN, -1
   IF  maxval lt 0.0 THEN RETURN, -1
   IF  minval eq maxval THEN RETURN, -1

   ; Reset 0. to really small values.
   IF  minval eq 0.0 THEN minval = (MACHAR (/DOUBLE)).xmin
   IF  maxval eq 0.0 THEN maxval = (MACHAR (/DOUBLE)).xmin

   ; Convert everything to doubles.
   r = [DOUBLE (minval), DOUBLE (maxval)]

   ; Check the order of the values in the range.  Set revlbl (reverse label order)
   ; appropriately.
   IF r [0] gt r [1] THEN revlbl = 1 ELSE revlbl = 0

   ; Make sure our range is in ascending order.
   r = r [SORT (r)]

   ; Convert range to log10
   lrange = ALOG10 (r)

   ; Number of decades in the label range.
   decades = lrange [1] - lrange [0] 

   ; For small ranges (smaller then 1 decade) will create a set of labels that increase
   ; along a linear scale.
   IF  (decades lt .7) THEN BEGIN

       ; Size of the range
       d = r [1] - r [0]

       ; Find the magnetude to use when calculating labels
       mag = FLOOR (ALOG10 (d))

       ; Increment between consecutive label values
       inc = 10.0D^mag

       ; Smallest label value
       ; bot = (FIX (a / inc, TYPE = 3) - 1) * inc
       bot = FLOOR (r [0] / inc) * inc

       ; If we will calculate too many labels, then increase the increment by a 
       ; factor of two.
       IF FLOOR (d / inc) gt 5 THEN inc = inc * 2

       ; Start creating the interval array.  First value is the smallest label value.
       interval = [bot]

       ; Create all label values after the bottom value.
       i = bot + inc

       while  (i le r [1]) DO BEGIN

           interval = [interval, i]
       
           i = i + inc

       ENDWHILE

       ; Add one more value to the interval array.  This should result in good set
       ; of label values.
       interval = [interval, i]

   ; Otherwise we are creating interdecadal labels appropriate for a logrithmically 
   ; sceled axis.
   ENDIF ELSE BEGIN

       ; Check if we should use course or fine intervals.
       ; Set the demarcation array appropiately
       IF  (decades GT coarse) && ~fine THEN demarc = demarc_d ELSE demarc = demarc_i

       ; Set a and b to decade of the mininum and maximum value.  These values will 
       ; constitute the upper and lower bounds of the array of labels.
       a = FLOOR (lrange [0])
       b = CEIL  (lrange [1])

       ; Create the seed arrays for ranges less than 1 and ranges greater than 1
       neg_seed = ALOG10 (demarc * .1)
       pos_seed = ALOG10 (demarc)

       ; Array to hold label values.
       v = DBLARR (1)

       ; Calculate labels

       ; We will calucalate the for the exponent
       ; array for negative and positive exponents separately.

       ; Calculate the labels for parts of the range that is smaller than 1.
       IF  (a lt 0)  THEN BEGIN

           FOR i = a, (-1) < (b - 1) DO v = [v, neg_seed + (i + 1)]

       ENDIF

       ; Calculate the labels for parts of the range that is larger than 1.
       IF  (b ge 0) THEN BEGIN

           FOR i = 0 > a, b - 1 DO v = [v, pos_seed + i]
       
       ENDIF 

       ; Remove the first (unused) element of the label array
       v = v [1:*]

       interval = 10.0^(v)

   ENDELSE

   ; reverse the interval, if necessary.
   IF  revlbl THEN interval = REVERSE (interval) 

   ; First try to find at least three values that are with the specified range
   vi  = WHERE (interval le r [1] AND interval ge r [0])

   ; If that proves impossible, then try to go outside the range so that we get
   ; at least three values.
   IF N_ELEMENTS (vi) lt 3 THEN BEGIN

      n_interval = N_ELEMENTS (interval)

      IF vi [0] gt 0 THEN vi = [vi [0] - 1, vi]
      IF vi [-1] lt n_interval - 1 THEN vi = [vi, vi [-1] + 1]

   ENDIF   

   ; Remove unneeded values.
   interval = interval [vi]
   
   RETURN, interval

END
