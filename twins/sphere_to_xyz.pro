function sphere_to_xyz, coord, degrees=degrees
; $Id: sphere_to_xyz.pro,v 1.1 2017/05/31 21:45:38 ryurow Exp $

;; Convert from lat/lon to xyz coordinates.  lat/lon is radius,
;; lattitude, longitude.
;; Unfortunately, the function is misnamed since it does not use
;; spherical coordinates (i.e., radius, longitude, co-lattitude).

; coord = 3xN array of (r,lat,phi)
; R is radius.
; lat (lattitude) is the angle from the x,y plane (equator) toward +z
; Phi is measured from +x towards the +y in the equator.
; output = 3xN array of (x,y,z)

if (size(coord))(1) ne 3 then $
  message, 'Argument must have first dimension size equal to 3'
r = coord(0, *)
if keyword_set(degrees) then begin
    lat = coord(1, *)*!dtor
    phi = coord(2, *)*!dtor
endif else begin
    lat = coord(1, *)
    phi = coord(2, *)
endelse
z = r*sin(lat)
x = cos(phi)*r*cos(lat)
y = sin(phi)*r*cos(lat)
return, [x, y, z]
end
