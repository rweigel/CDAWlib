;  $Source: /usr/local/share/cvsroot/cdaweb/IDL/twins/dot.pro,v $
;  $Revision: 1.1 $
;  $Date: 2017/05/31 21:45:38 $

function dot, x, y

;+
;  Purpose:
;	
;  Arguments:
;  Preconditions:
;  Postconditions:
;  Invariants:
;  Example:
;  Notes:
;
;  Author:	Pontus Brandt at APL?
;  Modification $Author: ryurow $
;-
 Compile_Opt StrictArr
 return, total(x*y)
end
