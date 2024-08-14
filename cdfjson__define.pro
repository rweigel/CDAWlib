;----------------------------------------------------------------------------
;+
; Creates a CDFJSON instance
;
;
; :KEYWORDS:
;   None
;
;  Based on method from IDLffJSON
;-
;----------------------------------------------------------------------------

function CDFJSON::Init

  compile_opt idl2, hidden
  
  ON_ERROR, 2
  HEAP_NOSAVE, self

  self.atom = 1b
  self.object = 2b
  self.array = 4b
  self.num_test = '-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+\-]?[0-9]+)?'

  ;self._slashes = Ptr_New(/Allocate_Heap)
  ;self._quotes = Ptr_New(/Allocate_Heap)

  RETURN, self->IDL_Object::Init()
end


;----------------------------------------------------------------------------
pro CDFJSON::Cleanup

  compile_opt idl2, hidden

  self->IDL_Object::Cleanup

  ;Ptr_Free, self._slashes
  ;Ptr_Free, self._quotes

end


;----------------------------------------------------------------------------
; Based on SerializeAtom method IDLffJSON class.
;
; Adds CDF_FORMAT keyword to pass the format string for use when printing
; floating point values.

;----------------------------------------------------------------------------
;+
; Generates a JSON-formatted atom value string from an IDL variable
;     An atomic JSON value is a string, number, or true/false/null
;
;
; :PARAMS:
;   atomValue : in, required, type="NULL, BYTE, STRING, NUMERIC"
;      an IDL variable representing the atomic JSON value
;
; :RETURNS:
;   atomString : out, type="STRING"
;      a JSON-formatted string representing an atomic JSON value
;
;  Based on method from IDLffJSON
;-
function CDFJSON::SerializeAtom, atomValue, CDF_FORMAT=cdf_format

  compile_opt idl2, hidden
  ON_ERROR, 2
  
  ; Initialize atomString
  atomString = ''
  tname = SIZE(atomValue, /Tname)
  if (ISA(atomValue, /NULL)) then tname = 'NULL'
  
  switch tname of
  
    'NULL' : begin
      atomString = 'null'
      break
    end
    
    'BYTE' : ; fall thru
    'INT': ; fall thru
    'LONG': ; fall thru
    'UINT': ; fall thru
    'ULONG': ; fall thru
    'LONG64': ; fall thru
    'ULONG64': ; fall thru
    'FLOAT': ; fall thru
    'DOUBLE' : begin
      ; Pass the format string if we got one.
      atomString = self->SerializeNumeric(atomValue, CDF_FORMAT=cdf_format)
      break
    end
    
    'STRING' : begin
      ; Look for our special "null" string, which indicates a null value.
      if (STRCMP(atomValue, '!NULL', /FOLD_CASE)) then begin
        atomString = 'null'
      endif else begin
        atomValue = self->EscapeSpecialChars(atomValue)
        atomString = '"' + atomValue + '"'
      endelse
      break
    end
    
  else : MESSAGE, 'Type ' + tname + ' not allowed with JSON.', /NONAME
  
  endswitch

  return, atomString
end

;----------------------------------------------------------------------------
; Based on SerializeNumeric method IDLffJSON class.
;
; Treats FLOATS and DOUBLES differently when prining.
;
; Uses the format string passed as the value of the keyword CDF_FORMAT when
; printing out values.
;
; Moved prettifing-cleaning up printed FLOAT/DOUBLEs into a seperate method.
;

;----------------------------------------------------------------------------
; Super fast handling of array data.
; Also handles scalars.
;

function CDFJSON::SerializeNumeric, array, CDF_FORMAT=cdf_format
  compile_opt idl2, hidden, static

  tname = SIZE(array, /TNAME)

  switch tname of
  
    'BYTE': begin
      if (ISA(array, /BOOLEAN)) then begin
        result = STRJOIN((['false','true'])[array], ',')
      endif else begin
        result = STRJOIN(STRTRIM(FIX(array),2), ',')
      endelse
      break
    end
    'INT': ; fall thru
    'LONG': ; fall thru
    'UINT': ; fall thru
    'ULONG': ; fall thru
    'LONG64': ; fall thru
    'ULONG64': begin
      result = STRJOIN(STRTRIM(array,2), ',')
      break
    END
    
    'FLOAT': begin
      ; basically copied from DOUBLE, but modified to work specifically with floats

      ; Check if we were passed a format string.  If we were, then we use that when 
      ; printing values.  Otherwise we will use the default format string.
      fmt_string = (KEYWORD_SET (cdf_format)) ?  '(' + cdf_format + ')' : '(g15.7)'

      ; Use a catch statement to make sure that format string we picked up from the 
      ; FORMAT attribute was actually legitamate.  Also set cdf_format to the default
      ; value so subsequent write attempts do not keep causing the error.
      CATCH, error

      IF  (error ne 0) THEN BEGIN

          IF  STRMID (!ERROR_STATE.name, 0, 9) eq 'IDL_M_FMT' THEN BEGIN

              CATCH, /CANCEL
              fmt_string = '(g15.7)'
              cdf_format = fmt_string

          ENDIF ELSE BEGIN

              MESSAGE, /REISSUE_LAST   

          ENDELSE

      ENDIF

      ; Write the value
      result = STRTRIM(STRING(array, FORMAT=fmt_string),2)

      ; Check for format overflow.  If we got '***' then rewrite the value using 
      ; format string that should be able to write any value.
      sink = WHERE (STRPOS (result, '*') ge 0, bad)
      ; IF  (bad gt 0) THEN result = STRTRIM(STRING(array, FORMAT='(g26.17)'), 2)
      IF  (bad gt 0) THEN result = STRTRIM(STRING(array, FORMAT='(g15.7)'), 2)

      if (N_Elements(array) gt 1) then begin
        result = REFORM(result, SIZE(array, /DIMENSION))
      endif

      ; Pretify the values we just wrote out.
      result = self._float_reformat (result) ;

      break

     end
    'DOUBLE' : begin

      ; Check if we were passed a format string.  If we were, then we use that when 
      ; printing values.  Otherwise we will use the default format string.
      fmt_string = (KEYWORD_SET (cdf_format)) ?  '(' + cdf_format + ')' : '(g26.17)'

      ; Use a catch statement to make sure that format string we picked up from the 
      ; FORMAT attribute was actually legitamate.  Also set cdf_format to the default
      ; value so subsequent write attempts do not keep causing the error.
      CATCH, error

      IF  (error ne 0) THEN BEGIN

          IF  STRMID (!ERROR_STATE.name, 0, 9) eq 'IDL_M_FMT' THEN BEGIN

              CATCH, /CANCEL
              fmt_string = '(g26.17)'
              cdf_format = fmt_string

          ENDIF ELSE BEGIN

              MESSAGE, /REISSUE_LAST   

          ENDELSE

      ENDIF

      ; Write the value
      result = STRTRIM(STRING(array, FORMAT=fmt_string),2)

      ; Check for format overflow.  If we got '***' then rewrite the value using 
      ; format string that should be able to write any value.
      sink = WHERE (STRPOS (result, '*') ge 0, bad)
      IF  (bad gt 0) THEN result = STRTRIM(STRING(array, FORMAT='(g26.17)'), 2)

      if (N_Elements(array) gt 1) then begin
        result = REFORM(result, SIZE(array, /DIMENSION))
      endif

      ; Pretify the values we just wrote out.
      result = self._float_reformat (result) ;

      break
    end

    else : MESSAGE, 'Type ' + tname + ' not allowed with JSON.', /NONAME
  endswitch

  ; Concatenate multi-dimensional arrays into a single scalar string.
  while (N_ELEMENTS(result) gt 1) do begin
    result = STRJOIN('[' + result + ']', ",")
  endwhile

  return, result

end

;----------------------------------------------------------------------------
; 'Prettify' floating point values written during the SerializeNumeric
; method.
;
; This code used to be part of the SerializeNumeric method, but was moved
; to its own internal method to simpifly the logic in the above method.
;
; No changes have been made to it.
;

FUNCTION CDFJSON::_float_reformat, in

    ; This function tidies up floating point values after they have been printed 
    ; to a string.

    result = in

    ; Remove all trailing zeroes. The * makes it so we don't have to check
    ; whether there actually were any trailing zeroes.
    foreach rstr, result, i do begin
      trailingZeroPos = STREGEX(rstr, '0*$')
      result[i] = STRMID(rstr, 0, trailingZeroPos)
      trailingZeroPos = STREGEX(rstr, '\.*0*e', LENGTH=len)
      if (trailingZeroPos gt 0) then begin
        result[i] = STRMID(rstr, 0, trailingZeroPos) + STRMID(rstr, trailingZeroPos + len - 1)
      endif
      ;result[i] = result[i].Replace('e+0', 'e+')
      ;result[i] = result[i].Replace('e-00', 'e-')
      ;result[i] = result[i].Replace('e-0', 'e-')
      IF  STREGEX (result[i], '[Ee][+-]$', /BOOLEAN) THEN result[i] = result[i]+'00'
    endforeach
    ; Make sure #'s like "1." get written out as "1.0"
    endsWithDecimalPt = WHERE(STRMID(result, 0, /REV) eq '.', /NULL)
    if (ISA(endsWithDecimalPt)) then $
      result[endsWithDecimalPt] += '0'
    result[WHERE(result eq 'Inf', /NULL)] = '"Infinity"'
    result[WHERE(result eq '-Inf', /NULL)] = '"-Infinity"'
    result[WHERE(result eq 'Infinity', /NULL)] = '"Infinity"'
    result[WHERE(result eq '-Infinity', /NULL)] = '"-Infinity"'
    result[WHERE(result eq 'NaN', /NULL)] = '"NaN"'
    result[WHERE(result eq '-NaN', /NULL)] = '"-NaN"'
    result = STRJOIN(result, ",")

    return, result

END

;----------------------------------------------------------------------------
; Based on Serialize method IDLffJSON class.
;
; Reciperacally called method to convert LIST, Structure, or Hash to JSON
; string.
; 
; When an object/structure is detected, it is checked to see if it has the 
; tag "FORMAT".  This is assumed to be the format attribute for a variable 
; and will contain format string for printing out the variables data values.
;
; We will pass this format string to methods that write out numeric values
; as well as reciprical calls to this method using the keyword parameter
; CDF_FORMAT.
;

;----------------------------------------------------------------------------
;+
; Generates a JSON value from JSON-formatted string data
;
;
; :PARAMS:
;   JSONValue : in, required, type="NULL, BYTE, STRING, NUMERIC, HASH, LIST"
;      the JSON value to serialize
;
; :RETURNS:
;   JSONString : out, type="STRING"
;      a JSON-formatted string
;
;  Based on method from IDLffJSON
;-
;----------------------------------------------------------------------------

function CDFJSON::Serialize, JSONValue, CDF_FORMAT=cdf_format, DEBUG=debug, LOWERCASE=lowercase

  compile_opt idl2, hidden

  if (~Keyword_Set(debug)) then $
    ON_ERROR, 2
  
  JSONString = ''

  type = self->ValueType(JSONValue)
  isStruct = ISA(JSONValue, 'STRUCT')
  
  ;  We will be keeping track of the format attribute associated with each variable.
  ;  If this is a recursive call and we have already visited a variable object, then
  ;  the format will passed to us in the CDF_FORMAT keyword.
  format = (KEYWORD_SET (cdf_format)) ? cdf_format : ''  

  if (type eq self.object) then begin
    if (isStruct) then begin
      keys = TAG_NAMES(JSONValue)
      sink = WHERE (keys eq "FORMAT", cnt)
      ; Check to see if this is a variable object that includes the format attribute.
      ; If it is, then set format to the value of the format attribute.
      IF  cnt ne 0 THEN BEGIN
          format = JSONValue.format
      ENDIF
      if (Keyword_Set(lowercase)) then begin
        keys = keys.ToLower()
      endif
    endif else begin
      ; Use ordered keys if present
      if JSONValue.HASKEY('keyorder') then begin
        keys = JSONValue['keyorder']
      endif else begin
        keys = JSONValue.KEYS()
        ; Handle DICTIONARY, which returns a null string if it's empty.
        if (ISA(keys, /SCALAR, /STRING) && keys eq '') then begin
          keys = !NULL
        endif
      endelse
    endelse
    nElements = N_ELEMENTS(keys)
    dims = nElements
  endif else begin
    nElements = N_ELEMENTS(JSONValue)
    dims = ISA(JSONValue) ? (JSONValue.dim > 1) : nElements
  endelse
  
  case type of
    self.object : ps = ['{','}']
    self.array : ps = ['[',']']
    else : ps = ['', '']
  endcase

  if (nElements eq 0) then begin

    ; Handle undefined variables and !null
    if (type eq self.atom) then begin
      ; modified to pass the format string.
      JSONString += ps[0] + self->SerializeAtom(JSONValue, CDF_FORMAT=format) + ps[1]
    endif else begin
      JSONString += ps[0] + ps[1]
    endelse

  endif else begin

    if (ISA(JSONValue, /NUMBER)) then begin

      ; Fast path for arrays of numbers
      ; modified to pass the format string
      JSONString += ps[0] + self->SerializeNumeric(JSONValue, CDF_FORMAT=format) + ps[1]

    endif else begin

      jsonElements = STRARR(dims)
      reportOnce = 1b

      for iElement = 0, nElements-1 do begin

        idx = (type eq self.object) ? keys[iElement] : iElement

        thisElement = (isStruct && (type ne self.array)) ? $
          JSONValue.(iElement) : JSONValue[idx]

        if (ISA(JSONValue, 'Collection') && $
          ARRAY_EQUAL(thisElement, JSONValue, /QUIET)) then begin
          if (reportOnce) then begin
            reportOnce = 0b
            MESSAGE, /INFO, 'Infinite recursion detected. Skipping element.'
          endif
          continue
        endif

        if (self->ValueType(thisElement) eq self.atom) then begin
          ; Modified to pass the format string
          jsonElements[iElement] = self->SerializeAtom(thisElement, CDF_FORMAT=format)
        endif else begin
          ; Modified to pass the format string
          jsonElements[iElement] = self->Serialize(thisElement, CDF_FORMAT=format, $
            DEBUG=debug, LOWERCASE=lowercase)
        endelse
      
        if (type eq self.object) then begin
          if (ISA(idx,/STRING)) then idx = self.EscapeSpecialChars(idx)
          jsonElements[iElement] = '"' + idx + '":' + jsonElements[iElement]
        endif
          
      endfor

      while (jsonElements.ndim gt 0) do begin
        jsonElements = ps[0] + STRJOIN(jsonElements, ',') + ps[1]
      endwhile
      JSONString += jsonElements

    endelse
  
  endelse

  return, JSONString
end

;----------------------------------------------------------------------------
; This method is unchanged from the original IDLffJSON object

;----------------------------------------------------------------------------
;+
; Escapes special characters in JSON formatting
;
; :PARAMS:
;   atomString : in, required
;      the JSON string in which to escape special characters
;
; :RETURNS:
;   The string with special characters converted to escape characters.
;
; :AUTHOR:
;      Dawn Lenz, VIS
;
; :HISTORY:
;      16Aug2010:  Dawn Lenz, VIS - Original version
;-
;----------------------------------------------------------------------------
function CDFJSON::EscapeSpecialChars, atomString

  compile_opt idl2, hidden, static
  ON_ERROR, 2

  ; Reference:  http://www.json.org
  ; Note: We are not going to escape the / slash (solidus) character.
  ; The JSON spec requires that you handle it if it is escaped (which we
  ; do in ::UnescapeSpecialChars), but it does not require that you escape it.
  ; It is better to not escape it, because then URL's in strings don't
  ; end up looking goofy. If necessary, we could add a keyword to control
  ; this behavior in the future.
  specials = STRING(TRANSPOSE([92b,34b,8b,12b,10b,13b,9b]))
  escapes = ['\\','\"','\b','\f','\n','\r','\t']
  
  newString = atomString
  for i=0,N_ELEMENTS(specials)-1 do begin
    tmp = STRTOK(newString, specials[i], /EXTRACT, /PRESERVE)
    newString = STRJOIN(tmp, escapes[i])
  endfor

  return, newString
end

;----------------------------------------------------------------------------
; This method is unchange from the original IDLffJSON object

;----------------------------------------------------------------------------
;+
; Returns variable type
;
;
; :PARAMS:
;   variable : in ,required
;      the variable whose type to return
;
;
; :AUTHOR:
;   Dawn Lenz, VIS
;
; :HISTORY:
;   22Jun2010:  Dawn Lenz, VIS - Original version
;-
;----------------------------------------------------------------------------

function CDFJSON::ValueType, variable, PARSE=parse

  compile_opt idl2, hidden
  ON_ERROR, 2
  
  ; Fast return
  if (ISA(variable, /NUMBER, /SCALAR)) then $
    return, self.atom

  if (Keyword_Set(parse) && ISA(variable, /NULL)) then $
    return, self.object

  ; Be sure to check for a HASH before checking for isArray, since
  ; a Hash will also have a number of elements > 1, but it is still an object.
  if (ISA(variable, 'HASH')) then $
    return, self.object
  
  ; If we have a structure, it is "always" an array. So only
  ; return "object" if the structure has a single element.
  if (ISA(variable, 'STRUCT') && (N_ELEMENTS(variable) eq 1)) then $
    return, self.object

  if (ISA(variable, 'LIST') || ISA(variable, /ARRAY)) then $
    return, self.array

  RETURN, self.atom
  
end

;----------------------------------------------------------------------------
;+
; CDFJSON class
;     Manages JSON formatting
;     Reference:  http://www.json.org
;
;  Only can be used for serialization
;
;  Based on IDLffJSON
;-
;----------------------------------------------------------------------------

pro CDFJSON__define

  compile_opt idl2, hidden

  void = {IDLffJSON_SpecialChar, control:'', escaped:'', escapeStr:''}

  struct = {CDFJSON, $
            inherits IDL_Object, $
            ; these act like enums, so we can switch on integers not strings
            atom: 1b, $
            object: 2b, $
            array: 4b, $
            num_test: '', $
            ; keywords to Parse()
            ; dictionary: !False, $
            ; foldCase: !False, $
            debug: !False $
            ; string parse helpers, so we only rip through input stream
            ; once to find all backslash and double quote characters
            ;_slashes: Ptr_New(), $
            ;_quotes: Ptr_New(), $
            ;_lineLens: Ptr_New() $
           }
    
end

