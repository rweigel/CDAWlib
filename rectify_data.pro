;+------------------------------------------------------------------------
; This package of IDL functions facilitates data processing by applying 
; error checking and data normalization procedures to the structure that
; is returned by read_myCDF.  
;-------------------------------------------------------------------------

PRO update_determinants, desc, i, v, depend_list

    ; Get a list of fields for the variable data structure.
    tagnames = TAG_NAMES (v)

    FOR n = 0, N_ELEMENTS (depend_list) - 1 DO BEGIN

        ; Check if the ith attribute in the depend_list is defined for this
        ; variable.
        field = (WHERE (tagnames eq depend_list [n])) [0]

        IF  field ne -1 THEN BEGIN 

            ; And if it has a value.
            target_name = STRCOMPRESS (v.(field), /REMOVE_ALL)

            IF  STRLEN (target_name) gt 0 THEN BEGIN
            
                ; Search through each of the variables in the stucture for one 
                ; That matches the target name.
                target = (WHERE (STRMATCH (desc [*].varname, target_name) eq 1)) [0]

                ; If we couldn't find the determinant varaible, then just move on.
                IF  target eq -1 THEN BREAK

                ; Set the determinant flag (May already have been done).
                desc [target].determinant = 1

                ; Add the index of the variable we checking to the array
                ; of variables the target modifies.  
                IF  desc [target].modify eq !NULL THEN BEGIN

                    desc [target].modify = PTR_NEW ([i])

                ENDIF ELSE BEGIN

                    *desc [target].modify = [*desc [target].modify, i]

                ENDELSE

            ENDIF 

        ENDIF

    ENDFOR

END

FUNCTION create_descriptors, s

    node = {VARNAME:     '',         $  ; Variable name.
            EPOCH:       0,          $  ; True if variable is Epoch.
            NRV:         0,          $  ; True is variable records are NRV
            SORTED:      0,          $  ; True after data is sorted (if needed)
            DETERMINANT: 0,          $  ; True if another vairalbe is dependant
                                        ; on its value. (I.E. DEPEND_0, etc.)
            MODIFY:      PTR_NEW ()  $  ; Array of variables that this variable
                                        ; is a COMPONENT_* or DEPEND_* of.
            }
    ; depend_list is the list of attributes that may specify a variable that
    ; has a dependant relationship with the target variable.
    depend_list = ['DEPEND_0', 'DEPEND_1', 'DEPEND_2', 'COMPONENT_0', 'COMPONENT_1']                     

    desc = REPLICATE (node, N_TAGS (s))

    ; Fill in the variable names first.
    FOR i = 0, N_TAGS (s) - 1 DO desc [i].varname = s.(i).varname

    ; Now complete the rest of the structure.
    FOR i = 0, N_TAGS (s) - 1 DO BEGIN
                                                                       
        desc [i].epoch = STREGEX (s.(i).cdftype,                      $
                                  'CDF_EPOCH(16)?|CDF_TIME_TT2000',   $
                                  /BOOLEAN,                           $
                                  /FOLD_CASE)

        desc [i].nrv = ~STREGEX (s.(i).cdfrecvary, 'VARY', /BOOLEAN, /FOLD_CASE)


        ; Call the update_determinats procedure to create a list of all the 
        ; variables that this variable depends on.
        update_determinants, desc, i, s.(i), depend_list
        
    ENDFOR 
          
    RETURN, desc

END

FUNCTION get_attribute, v, attribute, valid

   ; Find the position of the requested attribute, if it is present in the
   ; variable.
   index = WHERE  (STRUPCASE (TAG_NAMES (v)) eq STRUPCASE (attribute), flag)

   ; Check if the requested attribute exists in the strucure.
   IF  flag ne 0 THEN BEGIN 

       ; Get its value.
       value = STRCOMPRESS (v.(index), /REMOVE_ALL)

       ; Check that it has a proper value.  If so, were done.
       IF  STRLEN (value) gt 0 THEN BEGIN

           valid = 1

           RETURN, value

       ENDIF 

   ENDIF

   ; Requested attribute missing or does not have a value.
   valid = 0

   RETURN, 0

END

PRO put_data, v, d 

    ; Get a list of tag names for the this variable.
    tnames = STRUPCASE (TAG_NAMES (v))
   
    i = WHERE (tnames eq  'HANDLE', cnt)

    IF  cnt eq 0 THEN BEGIN

        i = WHERE (tnames eq 'DAT', cnt)

        IF cnt eq 0 THEN MESSAGE, 'No data found in variable data structure'

        ; Add the new data to the variable.
        v.DAT = d

    ENDIF ELSE BEGIN 

        ; Create a new handle for the data.
        HANDLE_VALUE, v.HANDLE, d, /SET

    ENDELSE       

END 

FUNCTION get_data, v

    ; Get a list of tag names for the this variable.
    tnames = STRUPCASE (TAG_NAMES (v))
   
    i = WHERE (tnames eq  'HANDLE', cnt)

    IF  cnt eq 0 THEN BEGIN

        i = WHERE (tnames eq 'DAT', cnt)

        IF cnt eq 0 THEN MESSAGE, 'No data found in variable data structure'

        ; Get the data.
        val = v.DAT 

    ENDIF ELSE BEGIN 

        ; Get the data.
        HANDLE_VALUE, v.HANDLE, val

    ENDELSE   
   
    ; Were done.
    RETURN, val
 

END


FUNCTION traverse_depends, n, mono, valid, s, desc

FORWARD_FUNCTION traverse_depends
       
    ; Check if the data for this variable has already been processed.
    IF desc [n].sorted eq 1 THEN RETURN, 0

    PRINT, 'Updating data for ', desc [n].varname

    ; count the number of variables we modify.
    cnt = 1

    ; Get the data for for this variable.
    vdata = get_data (s.(n))

    ; Reformat it so that matches the corrected time order.

    ; Check the dimensionality of the data.  How we proceed depends on the
    ; the data structure.
    CASE SIZE (vdata, /N_DIMENSIONS) OF

       1: vdata = vdata [mono [valid]]

       2: vdata = vdata [*, mono [valid]]

       3: vdata = vdata [*, *, mono [valid]]

       4: vdata = vdata [*, *, *, mono [valid]]
  
       ELSE : BEGIN

                 MESSAGE, 'Can not remove time discontinuities from data' + $
                          ' with more than 4 dimensions.'
              END
    ENDCASE
 

    ; Put it back in to the data structure.
    put_data, s.(n), vdata

    ; Set the sorted flag.
    desc [n].sorted = 1

    ; Check if n is has any other variables that depend on it.
    IF  desc [n].determinant eq 0 THEN RETURN, cnt

    ; Find the number of variables that depend on this variables data.
    n_depend = N_ELEMENTS (*desc [n].modify)  

    FOR i = 0, n_depend - 1 DO BEGIN 
        
       next = (*desc [n].modify) [i]
       
       cnt = cnt + traverse_depends (next, mono, valid, s, desc) 

    ENDFOR

    RETURN, cnt

END

PRO sort_time_values, s, desc, VERBOSE=verbose

    ; Find all variables that are of type epoch.
    epochs = WHERE (desc [*].epoch eq 1, cnt)

    ; Loop to find and sort any time values for any variable that is of type 
    ; 'Epoch'.  In addition, any variable that whose values depdend on the 
    ; epoch, either directly or indirectly, will have their values sorted as
    ; as well.
    FOR n = 0, cnt - 1 DO BEGIN

        ; Get the data for for this variable.
        epoch = get_data (s.(epochs [n]))

        ; Number of epochos before screenin for invalid time values.
        nepoch = N_ELEMENTS (epoch)

        ; Get the FILLVAL if there is one.
        string = get_attribute  (s.(epochs [n]), 'FILLVAL', fv)
        
        ; If we do have a FILLVAL, then use it to extract the valid data.
        IF  fv THEN BEGIN 

            READS, string, fillval
             
            ;  Needs to be modified to handle NaN

            valid  = WHERE (epoch ne fillval, nvalid)

        ENDIF ELSE BEGIN 

            valid  = INDGEN (nepoch)
            nvalid = nepoch
        
        ENDELSE

        ; Sort the epochs so that they are monotonic increasing.
        mono = SORT (epoch [valid])

        ; Make sure evary time value is unique.  Duplicate time values will
        ; result in unrecoverable error.
        unique_epoch = UNIQ (mono)

        IF  N_ELEMENTS (unique_epoch) ne nvalid THEN BEGIN

            MESSAGE, 'Duplicate time values found.  Exiting.'

        ENDIF


        IF  KEYWORD_SET (verbose) THEN BEGIN 

            print, "Number of EPOCHS: ", nepoch
            print, "Number of valid EPOCHS: ", nvalid
            print, "Number of unique EPOCHS: ", N_ELEMENTS (unique_epoch)

        ENDIF 

        ; See if the epochs are correctly ordered and if they are all valid.
        ; If this is the case then we are done.
        IF (nvalid eq nepoch) && ARRAY_EQUAL (mono, INDGEN (nepoch)) THEN CONTINUE

        ; Update each variable that depends on this variable, starting with this
        ; variable.
        sink = traverse_depends (epochs [n], mono, valid, s, desc)

        ; stop
               
    ENDFOR

END

; Main wrapper function.
FUNCTION rectify_data, in, VERBOSE=verbose

    work = in

    desc = create_descriptors (work)
 
    sort_time_values, work, desc, VERBOSE=verbose

    RETURN, work

END

; Alternative interface.
PRO rectify_data, s, VERBOSE=verbose

    s = rectify_data (s, VERBOSE=verbose)

END
