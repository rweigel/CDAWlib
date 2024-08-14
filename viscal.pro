; Set up IDL for this program:
; @compile_cdaweb
; @compile_IDLmakecdf
; viscal, 'cdf/po_earth-camera/po_earth-camera-full_vis_19961207_v02.cdf', TARGET = 'temp.cdf', /DISPLAY
; viscal, '/home/cdaweb/data/polar/vis/vis_visible-imager-full/1997/', ARCHIVE = '/data/zdata_1/spdf_private/polar_working/', /OVERWRITE, HOURS = 1
; viscal, 'cdf/po_earth-camera/po_earth-camera-full_vis_20000606_v01.cdf', 'temp.cdf', /DISPLAY, TSTART=t0, TSTOP=t1, CDF_LIST=a
; viscal, '/home/cdaweb/data/polar/vis/vis_earth-camera-full/1998/', ARCHIVE = '/data/zdata_1/spdf_private/polar_working/', /OVERWRITE, HOURS = 1, /DISPLAY, BEGIN_PROC='01-Apr-1998 00:00:00.000'
; v = ['Movie_Image']
; b = read_MyCDF (v, a, /NODATASTRUCT, TSTART=t0, TSTOP=t1, /DEBUG)
; gp = "/home/ryurow/public_html/"
; s = plotmaster(b, /AUTO, /CDAWEB, OUTDIR=gp, /GIF, PID='test', /SMOOTH, /SLOW, /DEBUG)

;----------------------------------------------------------
; PURPOSE:
;  Setup some default values.
;
; INPUTS:
;  None
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Set up default values in the XV_DEFAULTS common block.
;
; COMMON BLOCKS:
;  XV_DEFAULTS
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_SET_DEFAULTS, LUN = lun

   COMMON XV_DEFAULTS, d_master_e, d_master_l, b_ext, var_names, image, wr, cdf_dtype

   ;d_master_e= 'po_earth-camera-full_vis_test_00000000_v01.cdf'
   d_master_e= 'po_vis_earth-camera-calibrated_00000000_v01.cdf'
   d_master_l= 'po_vis_visible-imager-calibrated_00000000_v01.cdf'

   ; Get the default path seperator
   sep       = PATH_SEP ()

   ; Find where masters are kept
   cdaw_data = GETENV ("CDAWDATA")

   IF  STRMID (cdaw_data, 0, /REVERSE_OFFSET) ne sep THEN cdaw_data = cdaw_data + sep

   cdaw_masters = cdaw_data + '0MASTERS' + sep

   ; Check the local directory first for the master
   master = FILE_SEARCH ("",  d_master_e)

   ; If its  not there, then check CDAWeb
   IF  STRLEN (master [0]) eq 0 THEN master = FILE_SEARCH (cdaw_masters,  d_master_e)

   d_master_e = master [0]

   PRINTF, lun, "Setting default MASTER for earth camera to:         ", d_master_e

   ; Check the local directory first for the master
   master = FILE_SEARCH ("",  d_master_l)

   ; If its  not there, then check CDAWeb
   IF  STRLEN (master [0]) eq 0 THEN master = FILE_SEARCH (cdaw_masters,  d_master_l)

   d_master_l = master [0]

   PRINTF, lun, "Setting default MASTER for visible light camera to: ", d_master_l
   PRINTF, lun

   b_ext     = '.bak'
   var_names = ['Epoch',               $
                'Time_PB5',            $   
                'Int_Time_Half',       $   
                'Filter',              $   
                'AltF',                $   
                'PPitch',              $   
	            'SC_Pos_GCI',          $   
                'SC_Vel_GCI',          $   
                'SC_SpinV_GCI',        $   
 	            'V_Zenith',            $
                'Sun_Vctr',            $
                'S_Zenith',            $
                'D_Qual',              $   
                'Post_Gap',            $
                'Mirr_Elv',            $
                'Mirr_Azm',            $
                'Headers']                

   image     = ['Image_Counts_Raw',    $
                'Image_Counts_Clean',  $
                'IImage_Counts']

   wr        = ['Geo_Lat',             $
                'Geo_Lon',             $
                'Rotatn_Matrix',       $
                'Limit_Lo',            $
                'Limit_Hi',            $
                'Ra',                  $
                'Dec',                 $
                'ALTLS',               $
                'On_Earth'] 

   dtype     = {name: "", val: 0}

   cdf_dtype = REPLICATE (dtype, 13)  

   cdf_dtype.name = ['CDF_BYTE',       $
                     'CDF_CHAR',       $
                     'CDF_DOUBLE',     $
                     'CDF_FLOAT',      $
                     'CDF_INT1',       $
                     'CDF_INT2',       $
                     'CDF_INT4',       $
	                 'CDF_REAL4',      $
                     'CDF_REAL8',      $
                     'CDF_UCHAR',      $
                     'CDF_UINT1',      $
                     'CDF_UINT2',      $
                     'CDF_UINT4' ]

   cdf_dtype.val  = [ 1,               $
                      7,               $
                      5,               $
                      4,               $
                      1,               $
                      2,               $
                      3,               $ 
	                  4,               $
                      5,               $
                      7,               $
                      1,               $
                      12,              $
                      13 ]


   RETURN

END 

;----------------------------------------------------------
; PURPOSE:
;  Create a buffer that will hold all of the CDF data.
;
; INPUTS:
;  None.
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Creates storage buffer.
;
; COMMON BLOCKS:
;  XV_DEFAULTS
;  XV_FILE_DATA
;  XV_RECORD_DATA
;
; PROCEDURE:
;   This procedure assumes that at least on record has already been read
;   by calling the procedure XV_GET_RECORD.  This  
;
;   In
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_SETUP_DATA_BUFFER

   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer

   data_buffer = {}
   data_dim = {}

   t_names = TAG_NAMES (record)  

   FOR i = 0, N_TAGS (record) - 1 DO BEGIN

      s = size (record.(i), /STRUCTURE)

      d = s.dimensions
      n = s.n_dimensions
      e = s.n_elements

      ; Correct the dimensions array.  To avoid special cases, we need to treat scalars
      ; as 1 dimensional single elements arrays.
      ;IF n eq 0 THEN BEGIN

      ;   n = 1
      ;   d [0] = 1
 
      ;ENDIF      

      x = make_array (DIMENSION = [e, maxrecs], TYPE = s.type)

      data_buffer = CREATE_STRUCT (data_buffer, t_names [i], x)

      d [n] = maxrecs
      d = d [WHERE (d ne 0)]
      ;x = make_array (DIMENSION = d, TYPE = s.type)

      data_dim = CREATE_STRUCT (data_dim, t_names [i], d)

   ENDFOR

   data_names = TAG_NAMES (data_buffer)

   RETURN

END 

PRO XV_SETUP_IMAGE_BUFFER, mid

   COMMON XV_DEFAULTS, d_master_e, d_master_l, b_ext, var_names, image, gc, cdf_dtype
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer

   image_buffer = {}

   FOR i = 0, N_ELEMENTS (image) - 1 DO BEGIN

       vi = CDF_VARINQ (mid, image [i]) 

       t =  cdf_dtype [WHERE (cdf_dtype.name eq vi.datatype)].val

       x  = make_array (DIMENSION = [vi.dim, maxrecs], TYPE = t)

       image_buffer = CREATE_STRUCT (image_buffer, image [i], x)

   ENDFOR   

END

PRO XV_SETUP_WR_BUFFER, mid

   COMMON XV_DEFAULTS, d_master_e, d_master_l, b_ext, var_names, image, wr, cdf_dtype
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer

   wr_buffer = {}
   
   FOR i = 0, N_ELEMENTS (wr) - 1 DO BEGIN

       vi = CDF_VARINQ (mid, wr [i]) 

       t =  cdf_dtype [WHERE (cdf_dtype.name eq vi.datatype)].val

       IF (N_ELEMENTS(vi.dimvar) eq 1 && vi.dimvar eq 0) THEN BEGIN

          x = make_array  (DIMENSION = [maxrecs], type = t)

       ENDIF ELSE BEGIN 

          x  = make_array (DIMENSION = [vi.dim, maxrecs], TYPE = t)

       ENDELSE 

       wr_buffer = CREATE_STRUCT (wr_buffer, wr [i], x)

   ENDFOR   

END

PRO XV_GET_GEO_FILL, mid

   COMMON XV_GEOCO_FILL, glat_fill, glon_fill

   ; Set up the fill values for geographic longitude and geographic lattitude
   def_lon_fill = -1.0e+31
   def_lat_fill = -1.0e+31

   z_var = 1

   v_num = CDF_VARNUM (mid, 'Geo_Lat', z_var)

   CDF_ATTGET_ENTRY, mid, 'FILLVAL', v_num, tt, glat_fill, status, /ZVARIABLE

   v_num = CDF_VARNUM (mid, 'Geo_Lon', z_var)

   CDF_ATTGET_ENTRY, mid, 'FILLVAL', v_num, tt, glon_fill, status, /ZVARIABLE

END


;----------------------------------------------------------
; PURPOSE:
;  Converts rectangular coordinates to spherical coordinates.
;
; CALLING SEQUENCE:
;  pt = [1.3, 3.2, 1.8]
;  RECSPHD, PT, R, THETA, PHI
;
; INPUTS:
;  LOC   == A 3-element array or a 3xN element array representing
;           cartesian points in 3 space.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  R     == A named variable returning the spherical radius
;  THETA == A named variable returning the THETA angle in DEGREES
;           Theta is the angle relative to the Z axis.
;  PHI   == A named variable returning the PHI angle in DEGREES
;           PHI is the angle in the XY plane.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FORWARD_FUNCTION XV_arrayNorm

PRO XV_recsphd, loc, r, theta, phi
   sz =  size(loc)
   IF(sz(0) EQ 1) THEN BEGIN
      sz = 1
   END ELSE sz = sz(2)

   PHI = dblarr(sz)
   Theta = phi
   R =  XV_arrayNORM(DOUBLE(LOC))

   j =  where(r LE 0, count)
   IF(count GT 0) THEN BEGIN
      THETA(j) =  0.0
      RETURN
   END

   j =  where(r GT 0, count)
   IF(count GT 0) THEN BEGIN
      THETA(j) = ACOS( loc[2,*] / r ) * !RADEG

      PHI(j) =  ATAN(loc[1,*], loc[0,*]) * !RADEG
      k =  where(phi LT 0,count)
      IF(count GT 0) THEN phi(k) = phi(k) + 360.0D0
   END


;   IF (R LE 0) THEN BEGIN
;      THETA = 0.0
;      RETURN
;   END ELSE BEGIN
;      THETA = ACOS( LOC[2] / R ) * !RADEG
;      IF (THETA EQ 0 OR THETA EQ 180) THEN RETURN
;
;      PHI = ATAN(LOC[1], LOC[0]) * !RADEG
;      IF (PHI LT 0) THEN PHI = PHI + 360.0D0
;   END
END


;----------------------------------------------------------
; PURPOSE:
;  Solves the quadratic equation AQ*X*X + QB*X + QC = 0
;
; CALLING SEQUENCE:
;  QA = 3.1
;  QB = 2.2
;  QC = .3
;  QUAD,QA,QB,QC,NP,X1,X2
;
; INPUTS:
;  QA == Scalar coefficient of X^2
;  QB == Scalar coefficient of X^1
;  QC == Scalar coefficient of X^0
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  NP == Number of solutions found.  Will be either 0,1,2.
;  X1 == Solution 1 if exists.
;  X2 == Solution 2 if exists.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:  Rae Dvorsky, Nov 2012   minor modifications
;
;----------------------------------------------------------
PRO XV_QUAD,QA,QB,QC,NP,X1,X2, LUN=lun
   sz = N_ELEMENTS(qa)

   IF ~ KEYWORD_SET (lun) THEN lun = -1

   IF(N_ELEMENTS(Qb) NE sz OR N_ELEMENTS(QC) NE sz) THEN BEGIN
      printf, lun, 'Quad::input arrays not of equal length'
      return
   END

   NP =  dblarr(sz)
   x1 =  dblarr(sz)
   x2 =  dblarr(sz)

   j =  where(qa NE 0, count)
   IF(count EQ 0) THEN RETURN ELSE NP(j) = 0

   FOURAC = 4.D0 * DOUBLE(QA) * DOUBLE(QC)
   BXB = DOUBLE(QB)*DOUBLE(QB)

   Test = BXB - FOURAC

   j =  where(test LT 0, count)
   IF(count GT 0) THEN BEGIN
      NP(j) = 0
   END

   j =  where(test EQ 0, count)
   IF(count GT 0) THEN BEGIN
      np(j) = 1
      x1(j) =  -qb(j) / (2.d0 * qa(j))
   END

   j = where(test GT 0, count)
   IF(count GT 0) THEN BEGIN
      np(j) =  2
      sqrt_term =  sqrt(bxb(j) - fourac(j))
      qa2 =  2.d0 * qa(j)
      x1(j) =  (-qb(j) - sqrt_term) / qa2
      x2(j) =  (-qb(j) + sqrt_term) / qa2
   END
END



;----------------------------------------------------------
; PURPOSE:
;  Initializes the radius routine.  The earth radius
;  data is cached once which allows for quick read
;  access during execution.  This shouldnt be called
;  externally.
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  NONE
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; COMMON BLOCKS:
;  XV_EARTH
;
; MODIFICATION HISTORY:
;  Rae Dvorsky, Feb 2013  reduced array size from 1800 to 900 to 
;			  eliminate two-hemisphere redundancy
;----------------------------------------------------------
PRO XV_INITIALIZE_EARTH_RADIUS
   COMMON XV_EARTH, EARTH_RADIUS, MAX_RADIUS, MIN_RADIUS
   MIN_RADIUS = 6356.779d0
   MAX_RADIUS = 6378.164d0

   axa =  max_radius * max_radius
   bxb =  min_radius * min_radius
   axb =  min_radius * max_radius

   ARRAY_SIZE =  900
   EARTH_RADIUS = DBLARR(ARRAY_SIZE)
   FOR GLAT= 0,ARRAY_SIZE-1 DO BEGIN
      SINANG = SIN(!DTOR*GLAT/10.0)
      SINSQRD = SINANG*SINANG
      COSANG = SQRT( 1.- SINSQRD)
      EARTH_RADIUS(GLAT) = AxB / SQRT(AxA * SINSQRD+ BxB *COSANG*COSANG)
   END
END

;----------------------------------------------------------
; PURPOSE:
;  Returns the earths radius at a specified latitude
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  IN_GLAT == Latitude in degrees in [-90..90]
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  Scalar radius
;
; COMMON BLOCKS:
;  XV_EARTH
;
; MODIFICATION HISTORY:
;  Rae Dvorsky, Feb 2013  changed to use absolute value of GLAT with
;			  one-hemisphere array size
;
;----------------------------------------------------------
FUNCTION XV_RADIUS, IN_GLAT
   COMMON XV_EARTH, EARTH_RADIUS, MAX_RADIUS, MIN_RADIUS
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons

   IF(N_ELEMENTS(EARTH_RADIUS) EQ 0) THEN XV_INITIALIZE_EARTH_RADIUS

;   IF(IN_GLAT LT -90.0 OR IN_GLAT GT 90.0) THEN print,'RADIUS::Glat out of range: ',glat
   GLAT =  ABS(IN_GLAT)

;   j = where(glat LT 0.0, count)
;   WHILE(count GT 0) DO BEGIN
;      glat(j) = 180.0 + glat(j)
;      j =  where(glat LT 0.0, count)
;   END

   j =  where(glat GT 90.0, count)
   WHILE(count GT 0) DO begin
      glat(j) = 180.0 - glat(j)
      j =  where(glat GT 90.0, count)
   END
   return, earth_radius(FIX(GLAT*10.0))
END


;----------------------------------------------------------
; PURPOSE:
;  Converts 2 digit years into 4 digit years.
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  TIME == A 3 element array of [YEAR, DOY, MSEC in DAY]
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  Scalar Year value
;
; COMMON BLOCKS:
;
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_YEAR4, Time
   Year =  Time(0)
   IF (YEAR LT 100) THEN BEGIN
      IF (YEAR GT 50) THEN BEGIN
         YEAR4 = 1900 + YEAR
      END ELSE YEAR4 = 2000 + YEAR
   END ELSE YEAR4 = YEAR
   return,Year4
END

;----------------------------------------------------------
; PURPOSE:
;  Converts year and doy into modified Julian date (MJD);
;        valid for year 1995 and after
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  Year
;  Doy
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  Scalar MJD value
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, May 2013
;
;----------------------------------------------------------
FUNCTION XV_YEAR_DOY_TO_MJD, year,doy
   MJD = 82
   IF (year gt 1996) THEN BEGIN
     FOR K=1996,year-1 DO BEGIN
        MJD = MJD + 365
        IF ((K mod 4) eq 0) then MJD = MJD + 1
     ENDFOR
   ENDIF
   MJD = MJD + doy
   return, MJD
END

FUNCTION XV_arrayNorm, vec
   vxv =  vec*vec
   return,sqrt(vxv(0,*) + vxv(1,*) + vxv(2,*))
END

FUNCTION XV_vNorm, vec
   vxv = fltarr(n_elements(vec))
   vxv =  vec*vec
   return,sqrt(total(vxv))
END

FUNCTION XV_dvNorm, vec
   vxv = dblarr(n_elements(vec))
   vxv =  vec*vec
   return,sqrt(total(vxv))
END

;----------------------------------------------------------
; PURPOSE:
;  Given a sc position relative to the earth and a
;  a directional vector, compute the points of intersection
;  with the earth.
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  SC_POS == SC position in GCI coordinates.
;  LOOK   == a vector originating at the SC
;  ALT    == Assumed altitude of emissions
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  NP     == Number of intersections with the earth.
;            Will be either 0,1,2.
;  POS1   == First position if NP >= 1.
;  POS2   == Second position if NP == 2.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY: Rae Dvorsky, Nov 2012   corrected use of radii constants
;----------------------------------------------------------
PRO XV_VPOINT, SC_POS, LOOK, ALT, NP, POS1, POS2, LUN=lun
						;; alt = altitude
   A = 6378.164d0 + double(ALT)                 ;; radius at equator
   B = 6356.779d0 + double(ALT)                 ;; radius at pole

   sz = N_ELEMENTS(look) / 3.0
   x = dblarr(sz)
   y = dblarr(sz)
   z = dblarr(sz)
   pos1 =  dblarr(3,sz)
   pos2 =  dblarr(3,sz)


   SC_POS = DOUBLE(SC_POS)
   LOOK =  double(LOOK)
   FACTOR =  1.5 * XV_vnorm(sc_pos[*,0])
   Point2 = sc_pos + (look * factor)

   X2 =  Point2(0,*)
   y2 =  point2(1,*)
   z2 =  point2(2,*)
   x2 = reform(x2,sz)
   y2 = reform(y2,sz)
   z2 = reform(z2,sz)

   x1 = sc_pos(0,*)
   y1 = sc_pos(1,*)
   z1 = sc_pos(2,*)
   x1 = reform(x1,sz)
   y1 = reform(y1,sz)
   z1 = reform(z1,sz)

   AxA =  a*a
   BxB =  b*b
   x1_sqr =  x1*x1
   y1_sqr =  y1*y1
   z1_sqr =  z1*z1

   cq = x1_sqr/AxA + y1_sqr/AxA + z1_sqr/BxB - 1

   bq =  2*(x2*x1 - x1_sqr)/AxA + $
	 2*(y2*y1 - y1_sqr)/AxA + $
         2*(z2*z1 - z1_sqr)/BxB

   aq =  (x2-x1)^2 / AxA + $
	 (y2-y1)^2 / AxA + $
         (z2-z1)^2 / BxB

    XV_QUAD, AQ, BQ, CQ, NP, ROOT1, ROOT2, LUN=lun

   j =  where(np GE 1, count)
   IF(count GT 0) THEN BEGIN
      x(j) =  x1(j) + root1(j) * (x2(j) -x1(j))
      y(j) =  y1(j) + root1(j) * (y2(j) -y1(j))
      z(j) =  z1(j) + root1(j) * (z2(j) -z1(j))
      pos1(0,j) =  x(j)
      pos1(1,j) =  y(j)
      pos1(2,j) =  z(j)
   END

   j =  where(np EQ 2, count)
   IF(count GT 0) THEN BEGIN
      x(j) =  x1(j) + root2(j) * (x2(j) -x1(j))
      y(j) =  y1(j) + root2(j) * (y2(j) -y1(j))
      z(j) =  z1(j) + root2(j) * (z2(j) -z1(j))
      pos2(0,j) = x(j)
      pos2(1,j) = y(j)
      pos2(2,j) = z(j)

      j =  where(XV_arrayNorm(SC_POS-POS1) GT XV_arrayNORM(SC_POS-pos2), count)
      IF(count GT 0) THEN BEGIN
         tmp =  pos2(*,j)
         pos2(*,j) =  pos1(*,j)
         pos1(*,j) =  tmp
      END
   END
END



;----------------------------------------------------------
; NAME:   GCIGEO
;
; PURPOSE:
;  Compute the geographic latitude, longitude, and altitude
;  for the input geocentric inertial vector and UT.
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  TIME  == 3 element array containing [YEAR, DOY, MSEC in DAY]
;  SUNRA == Right ascension of the sun
;  GCI_CRDS == 3 element array containing GCI coordinates.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  GLAT == Geographic latitude in degrees.
;  GLON == Geographic longitude in degrees.
;  ALT  == Geographic altitude in kilometers.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
PRO XV_GCIGEO, Time, SUNRA, GCI_CRDS, GLAT, GLONG, ALT
   XV_GEIGSE, GCI_CRDS, SUNRA, GSE_CRDS

   XV_EQTIME, Time, SOLMSEC
   XV_GSEGEO, GSE_CRDS, SOLMSEC, GLAT, GLONG
   j =  where(finite(glat) EQ 1, count)
; alt=altitude
   IF(count GT 0) THEN begin
      ALT = XV_vNORM(gci_crds) - XV_RADIUS(GLAT)
   END ELSE ALT =  0.0
END


;----------------------------------------------------------
; PURPOSE:
;  Rotates a GEI vector to GSE coordinates
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  GEI_CRD = 3 element array of GEI coordinates.
;  SUNRA  == Right ascension of the sun in degrees.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  GSE_CRD == 3 element array containing the GSE coordinates
;             that correspond the the input GEI coordinates.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
PRO XV_GEIGSE, GEI_CRD, SUNRA, GSE_CRD
   SUNRA_IN_RADS =  !DTOR * SUNRA

   SUNRA_COS =  COS(SUNRA_IN_RADS)
   SUNRA_SIN =  SIN(SUNRA_IN_RADS)

   GSE_CRD =  GEI_CRD
   GSE_CRD(0,*) = SUNRA_COS * GEI_CRD(0,*) + SUNRA_SIN * GEI_CRD(1,*)
   GSE_CRD(1,*) = - SUNRA_SIN * GEI_CRD(0,*) + SUNRA_COS * GEI_CRD(1,*)
   GSE_CRD(2,*) = GEI_CRD(2,*)
END

;----------------------------------------------------------
; PURPOSE:
;  Convert GSE to GEI coordinates
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  GSE_CRD == GSE coordinate
;  SUN_RA  == Right ascension of sun.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  GEI_CRD == GEI coordinates
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
PRO XV_GSEGEI, GSE_CRD, SUNRA, GEI_CRD
   SUNRA_IN_RADS =  !DTOR * SUNRA

   SUNRA_COS =  COS(SUNRA_IN_RADS)
   SUNRA_SIN =  SIN(SUNRA_IN_RADS)

   GEI_CRD = DBLARR(3)
   GEI_CRD(0) = SUNRA_COS * GSE_CRD(0) - SUNRA_SIN * GSE_CRD(1)
   GEI_CRD(1) = SUNRA_SIN * GSE_CRD(0) + SUNRA_COS * GSE_CRD(1)
   GEI_CRD(2) = GSE_CRD(2)
END

;----------------------------------------------------------
; PURPOSE:
;  Rotates the GSE vector to geographic Latitude and Longitude.
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  GSE_CRDS == 3 element array or GSE coordinates.
;  SOLMSEC  == apparent solar time in milliseconds.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  GLAT == geographic latitude.
;  GLON == geographic longitude.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
PRO XV_GSEGEO, GSE_CRDS, SOLMSEC, GLAT, GLONG
   XV_RECSPHD,GSE_CRDS,R,THETA,PHI
   GLAT = 90.0 - THETA
   GLONG = (PHI-(DOUBLE(SOLMSEC)-43200000.D0)*.4166667D-5) MOD 360.D0
END

;----------------------------------------------------------
; PURPOSE:
;  Converts universal time to apparent solar time.
;
; CALLING SEQUENCE:
;  NONE
;
; INPUTS:
;  TIME == A 3 element array of [YEAR, DOY, MSEC in DAY]
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  SOLMSEC == apparent solar time.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
PRO XV_EQTIME, Time, SOLMSEC
   Year =  Time(0)
   DOY =  TIME(1)
   UTMSEC =  Time(2)

   YR = XV_YEAR4(YEAR)
   DAYS = DOY + (YR-1981)*365.0 + (YR-1981)/4
   ANG = 279.58D0 + 0.985647D0*(DAYS+DOUBLE(UTMSEC)/864.D5)
   E = -104.7*SIN(!DTOR*ANG) + 596.2*SIN(!DTOR*2.*ANG) + 4.3*SIN(!DTOR*3.*ANG) $
       - 12.7*SIN(!DTOR*4.*ANG) - 429.3*COS(!DTOR*ANG) - 2.0*COS(!DTOR*2.*ANG) $
       + 19.3*COS(!DTOR*3.*ANG)
   SOLMSEC = UTMSEC + ROUND(E*1.D3)
END

FUNCTION XV_GET_SENSOR, fid
   sensorid = CDF_VARNUM (fid, 'Sensor') 
   CDF_VARGET, fid, sensorid, sensor, /ZVARIABLE, rec_start = 0

   RETURN, sensor

END
;----------------------------------------------------------
; PURPOSE:
;  Returns an image or sequence of images beginning with
;  rec_number.
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  images = xv_get_image(fid,0,1)
;
; INPUTS:
;   FID = A fileId that has been created with the CDF_OPEN routine.
;   REC_NUMBER = Where to begin loading images withing the CDF file.
;   NUM_RECORDS = How many images to load
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  A 256x256xN array of images
;
; COMMON BLOCKS:
;  None
;
;
; MODIFICATION HISTORY:
;
; Note:
;
; This function was modified in xvis version 2.50 but modifications were not
; incorperated into this code.  Instead the original version is retained.
;
;----------------------------------------------------------
FUNCTION XV_GET_IMAGE, fid, rec_number, num_records
   countsid = CDF_VARNUM(fid,'Image_Counts')
   CDF_VARGET,fid,countsid,image,rec_start=rec_number,rec_count=num_records,/ZVARIABLE
   return,image
END

;----------------------------------------------------------
; PURPOSE:
;  Returns the number of images present in the CDF file.
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  images = xv_get_num_records(fid)
;
; INPUTS:
;   FID = A fileId that has been created with the CDF_OPEN routine.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  The number of images in the CDF file.
;
; COMMON BLOCKS:
;  None
;
;
; MODIFICATION HISTORY:
;
; Note:
;
; This function was modified in xvis version 2.50 but modifications were not
; incorperated into this code.  Instead the original version is retained.
;
;----------------------------------------------------------
FUNCTION XV_GET_NUM_RECORDS,fid
   countsid = CDF_VARNUM(fid,'Image_Counts')
   CDF_CONTROL,fid,variable=countsid,/Zvariable,get_var_info=info
   return, info.maxrec + 1
END

;----------------------------------------------------------
; PURPOSE:
;  Returns the header
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  images = xv_get_header(fid)
;
; INPUTS:
;   FID = A fileId that has been created with the CDF_OPEN routine.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  Header
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_GET_HEADER, fid
   HeaderId =  CDF_VARNUM(fid,'Headers')
   CDF_VARGET, fid, HeaderId, Header,/zvariable,rec_start=0
   return, Header
END

;----------------------------------------------------------
; Name: XV_GET_All_Improparams
; PURPOSE:  Restores the image processing parameter arrays from ECPsav.dat
;	for Earth camera or LRPsav.dat for Visible (Low Resolution) camera
;
; CALLING SEQUENCE:
;  XV_GET_All_Improparams
;
; OUTPUTS: 
;
; COMMON BLOCKS:
;   XV_RECORD_DATA
;   XV_FLAGS
;   XV_All_IMPROPARAMS
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, April 2013 
; 
; DO WE STILL NEED THIS ?????
;
;----------------------------------------------------------
PRO XV_GET_All_Improparams
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FLAGS, Flags
   COMMON XV_All_IMPROPARAMS, params
   params = {YEAR:0,DOY:0,MJD:0,RATIO:0.0,BIAS:0,$
                 LEFT:0,RIGHT:0,TOP:0,BOTTOM:0}
   If (sensor eq 0) then Restore, 'ecpsav.dat'
   If (sensor eq 1) then Restore, 'lrpsav.dat'
END

;----------------------------------------------------------
; Name: XV_Improparams
; PURPOSE:  Gets the image processing parameters for the current image
;
; CALLING SEQUENCE:
;  XV_Improparams
;
; OUTPUTS: Sets parameters in XV_Improparams common block -
;	     Ratio: intended to adjust color levels in displayed image;
;		    it is not used in XVIS
;	     Bias:  used by Flat Field and Subtract Cosmic Ray routines
;	     Left, Right: used by Subtract Slopes for horizontal correction
;	     Top, Bottom: used by Subtract Slopes for vertical correction
;	     Threshold: used by Subtract Cosmic Ray
;	     FlatField:  Flat Field array
;
; COMMON BLOCKS:
;   XV_RECORD_DATA
;   XV_FLAGS
;   XV_All_IMPROPARAMS
;   XV_IMPROPARAMS
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, April 2013 
;	Rae Dvorsky, Sep 2013,	added "lastmjd" to avoid reading beyond
;				end of "params" array
;
;----------------------------------------------------------
PRO XV_Improparams
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FLAGS, Flags
   COMMON XV_All_IMPROPARAMS, params
   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField, MJD
; get year,doy, and MJD of current image
     year = Record.Time_pb5[0]
     doy = Record.Time_pb5[1]
     MJD = XV_YEAR_DOY_TO_MJD(year,doy)
; search parameter array until MJD has been exceeded, then use previous entry
     sz =  (size(params))(1)
     IPP = 0
     lastmjd = params(sz-1).mjd
     while (params(ipp).mjd lt mjd and params(ipp).mjd ne lastmjd) do ipp=ipp+1
     if (params(ipp).mjd gt mjd and ipp gt 0) then ipp = ipp - 1  
     Ratio = params(IPP).ratio
     Bias = params(IPP).bias
     Left = FIX(params(IPP).left)
     Right = FIX(params(IPP).right)
     Top = FIX(params(IPP).top)
     Bottom = FIX(params(IPP).bottom)
  IF (sensor eq 0) THEN BEGIN
     Threshold = 30
     IF (year lt 2000) THEN Restore, 'ff4sav.dat' $
       ELSE Restore, 'ff5sav.dat'
  ENDIF ELSE BEGIN
     Threshold = 40
     Restore, 'fflrsav.dat'
  ENDELSE
  FLAGS.IPP = 1
END

;----------------------------------------------------------
; NAME:    XV_GET_INTENS_TABLE
;
; PURPOSE:
;  Returns the intensity table stored in the CDF.
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  vec = xv_get_look_vector(fid)
;
; INPUTS:
;   FID = A fileId that has been created with the CDF_OPEN routine.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;   A 256 element vector used to convert compressed insturment 
;   into Kilo Rayleighs (KR)
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
FUNCTION XV_GET_INTENS_TABLE, fid, sensor

   IF  sensor eq 1 THEN BEGIN

       filter_id = CDF_VARNUM (fid, 'Filter')

       CDF_VARGET, fid, filter_id, filter_table, /zvariable, rec_start=1

       filter = filter_table [0]

       intens_id = CDF_VARNUM (fid, 'Intens_Tables')

       CDF_VARGET, fid, intens_id, intens_table, /zvariable, rec_start=1

       intens_table = REFORM (intens_table [*, filter - 1])
       
   ENDIF ELSE BEGIN 

       intens_id = CDF_VARNUM (fid, 'Intens_Table')

       CDF_VARGET, fid, intens_id, intens_table, /zvariable, rec_start=1

   ENDELSE 

   RETURN, intens_table

END

;----------------------------------------------------------
; NAME:    XV_CALC_INTENSITY
;
; PURPOSE:
;  Convert the image from compressed instrument counts to 
;  Kilo Rayleighs (KR).
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;----------------------------------------------------------
FUNCTION XV_CALC_INTENSITY, table, image

   d = SIZE (image, /DIMENSIONS)

   r = FLTARR (d) 

   FOR i = 0, N_ELEMENTS (image) - 1 DO BEGIN

       r [i] = table [image [i]]

   ENDFOR 

   RETURN, r

END

;----------------------------------------------------------
; NAME:    XV_GET_LOOK_VECTOR
;
; PURPOSE:
;  Returns the look direction vectors.
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  vec = xv_get_look_vector(fid)
;
; INPUTS:
;   FID = A fileId that has been created with the CDF_OPEN routine.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  A 3x256x256 matrix representing a 3 vector for each pixel
;  in the image.  This matrix can be used to compute the direction
;  that each pixel is pointed.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;   Written by Rae Dvorsky, 04-May-2005
;	Reads the look vector array from the file LV.DAT if available
;	in the current directory; otherwise, gets the look vector array
;	from the CDF variable Look_Vctr.
;----------------------------------------------------------
FUNCTION XV_GET_LOOK_VECTOR, fid

   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2

   Look_Vector = FLTARR(3,256,256)
   LVF = FindFile('lv.dat',COUNT=FCount)
   IF (FCount EQ 0) THEN BEGIN
       MESSAGE, 'The Look Vector file was not found.', /CONTINUE, /INFORMATIONAL
       MESSAGE, 'The CDF variable Look_Vctr will be used.', /CONTINUE, /INFORMATIONAL
       LookId =  CDF_VARNUM(fid,'Look_Dir_Vctr')
       CDF_VARGET, fid, LookId, Look_Vctr, /zvariable, rec_start=1
       Look_Vector = Look_Vctr
   END ELSE BEGIN
       LVFile = LVF(0)
       OpenR,11,LVFile
       ReadF,11,Look_Vector
       IF (sensor NE 0) THEN ReadF,11,Look_Vector
       close,11
   END
   return, Look_Vector
END

;----------------------------------------------------------
; NAME:   XV_GET_RECORD
;
; PURPOSE:
;  Returns the record structure for each record.  This is
;  basically any information contained in an individual
;  record with the exception of the image itself.
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  vec = xv_get_record(fid,rec_number)
;
; INPUTS:
;   FID = A fileId that has been created with the CDF_OPEN routine.
;   REC_NUMBER = the number of the record to obtain.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  A structure containing the tags in the 'Names' local variable.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_GET_RECORD, fid, rec_number, sensor

   COMMON XV_DEFAULTS, d_master_e, d_master_l, b_ext, var_names, capture, gc, cdf_dtype

   n_var_names = N_ELEMENTS (var_names) 

   ; Determine which instrument we are processing data for.  Set up the var_names
   ; array depending on the instrument.

   ; Processing Data for Earth Camera.
   IF sensor eq 0 THEN names = [var_names [0:n_var_names-4], var_names [n_var_names-1]]
   ; Processing Data for Low Resolution Camera.
   IF sensor eq 1 THEN names = var_names

   Result =  {RECORD:Rec_number}
   FOR i=0,N_ELEMENTS(Names)-1 DO BEGIN
      VarId =  CDF_VARNUM(fid,Names[i])
      ; 0 is a valid variable number (usualy Epoch).
      IF(VarId NE -1) THEN BEGIN
         CDF_VARGET,fid,VarId,Val,/zvariable,rec_start=rec_number
      ENDIF ELSE BEGIN

         MESSAGE, "Non existent variable: " + names [i] + " requested."

      ENDELSE 
      Result =  CREATE_STRUCT(Names(i), Val, Result)
   ENDFOR

   return,Result
END

;----------------------------------------------------------
; PURPOSE:
;  Loads the color table contained in the CDF file.
;
; CALLING SEQUENCE:
;  fid = cdf_open(filename)
;  XV_LOAD_COLOR_TABLE, fid
;
; INPUTS:
;  FID = file id created using CDF_OPEN
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Loads the color table with the values in the CDF file.
;  These values are scaled (if necessary) to fit into
;  the table.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_LOAD_COLOR_TABLE, fid, DISPLAY=display 
   COMMON COLORS, ro,go,bo,rc,gc,bc
   colortable =  intarr(3,256)

   tableid = CDF_VARNUM(fid,'RGBColorTable')
   if(tableid ge 0) THEN BEGIN
      CDF_VARGET,fid,tableid,table,/zvariable
      colortable = table
   END ELSE BEGIN
      rgb = INDGEN(256)
      colortable(0,*) = rgb
      colortable(1,*) = rgb
      colortable(2,*) = rgb
   END

   red = CONGRID(reform(colortable(0,*)), 256 < !D.table_size)
   green = CONGRID(reform(colortable(1,*)),256 < !D.table_size)
   blue =  CONGRID(reform(colortable(2,*)),256 < !D.table_size)
   IF KEYWORD_SET (display) THEN tvlct, red, green, blue
   ro = red
   go = green
   bo = blue
   rc = red
   gc = green
   bc = blue
END

;-------------------------------------------------------------
;+
; NAME:
;       RUNLENGTH
; PURPOSE:
;       Give run lengths for array values.
; CATEGORY:
; CALLING SEQUENCE:
;       y = runlength(x,[r])
; INPUTS:
;       x = 1-d array of values.                  in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       y = X with multiple values squeezed out.  out
;       r = run length of each element in Y.      out
; COMMON BLOCKS:
; NOTES:
; MODIFICATION HISTORY:
;       RES  30 Jan, 1986.
;       R. Sterner, 25 Sep, 1990 --- converted to IDL V2.
;       Johns Hopkins University Applied Physics Laboratory.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
FUNCTION XV_RUNLENGTH,X,R, help=hlp, LUN=lun

   IF ~ KEYWORD_SET (lun) THEN lun = -1

   if (n_params(0) lt 1) or keyword_set(hlp) then begin
      printf, lun, ' Give run lengths for array values.'
      printf, lun, ' y = runlength(x,[r])'
      printf, lun, '   x = 1-d array of values.                  in'
      printf, lun, '   y = X with multiple values squeezed out.  out'
      printf, lun, '   r = run length of each element in Y.      out'
      return, -1
   endif

   ;;---  The easiest way to understand how this works is to try
   ;;---  these statements interactively.
   A = X - SHIFT(X,1)           ;; Distance to next value.
   A(0) = 1                     ;; Always want first value.
   W = WHERE(A NE 0)            ;; Look for value changes.
   Y = X(W)                     ;; Pick out unique values.
   IF N_PARAMS(0) LT 2 THEN RETURN, Y
   R = ([W,N_ELEMENTS(X)])(1:(N_ELEMENTS(Y))) - W ; run lengths.
   RETURN, Y
END


;-------------------------------------------------------------
;+
; NAME:
;       INTERSECT
; PURPOSE:
;       Return the elements common to two given arrays.
; CATEGORY:
; CALLING SEQUENCE:
;       z = intersect(x,y)
; INPUTS:
;       x, y = arrays (not necessarily same size).  in
; KEYWORD PARAMETERS:
; OUTPUTS:
;       z = array of elements in common.            out
; COMMON BLOCKS:
; NOTES:
;       Note: if z is a scalar 0 then no elements were
;         in common.
; MODIFICATION HISTORY:
;       R. Sterner  19 Mar, 1986.
;       R. Sterner, 4 Mar, 1991 --- converted to IDL v2.
;
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;-
;-------------------------------------------------------------
; NOTE: Not sure this even used anymore!!!!
function XV_intersect,x,y, help=hlp, LUN=lun

   if (n_params(0) lt 2) or keyword_set(hlp) then begin
      printf, lun, ' Return the elements common to two given arrays.'
      printf, lun, ' z = intersect(x,y)'
      printf, lun, '   x, y = arrays (not necessarily same size).  in'
      printf, lun, '   z = array of elements in common.            out'
      printf, lun, ' Note: if z is a scalar 0 then no elements were'
      printf, lun, '   in common.'
      return, -1
   endif

   xs = XV_runlength(x(sort(x)), LUN=lun)   ; Keep only unique elements.
   ys = XV_runlength(y(sort(y)), LUN=lun)

   zs = [xs,ys]                 ; Merge the 2 arrays.
   zs = zs(sort(zs))            ; Sort.

   d = zs - shift(zs,1)         ; Find differences between elements.

   w = where(d eq 0, count)     ; Elements common to both arrays
                                ; occur twice, giving 0 diffs.

   if count eq 0 then return, 0 ; Scalar 0 means no common elements.
   return, zs(w)                ; Vector of common elements.

end

;+
; NAME:
;  XV_UNDISTORT
;
; PURPOSE:
;  'Warps' a 256 by 256 pixel VIS image to display
;  the image in the correct spatial resolution
;
; CATEGORY:
;  VIS image analysis
;
; INPUTS:
;  A VIS image
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  An 'undistorted' image
;
; COMMON BLOCKS:
;  XV_FILE_DATA
;
; SIDE EFFECTS:
;  None
;
; RESTRICTIONS:
;  None
;
; EXAMPLE:
;  NewImage = XV_UNDISTORT(Image)
;
; MODIFICATION HISTORY:
;  Written by Kenny Hunt, 9/97
;-
FUNCTION XV_UNDISTORT, Image
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor

   newimage =  bytarr(548,475)
   nonzeros =  where(undistort GE 0)
   newimage(nonzeros) =  image(undistort(nonzeros))

   return,newimage
END

;-------------------------------------------------------------
; NAME: MAKE_ROTATION_MATRIX
;
; PURPOSE:
;       Contructs a view coordinate system basis for one image,
;       to be used instead of record.rotatn_matrix
; CATEGORY:
;
; CALLING SEQUENCE:
;       rotation_matrix = MAKE_ROTATION_MATRIX()
; INPUTS:
;
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       3x3 image view matrix
;
; COMMON BLOCKS:
;       XV_FILE_DATA
;       XV_RECORD_DATA
; NOTES:
;
; MODIFICATION HISTORY:
;	Rae Dvorsky, March 2005
;	Rae Dvorsky, June 2012   modification of Low Resolution pointing
;				  angles due to probable two-axis tip/tilt
;	Rae Dvorsky, March 2015  LR Rotation angle calculation error corrected
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-------------------------------------------------------------
FUNCTION XV_MAKE_ROTATION_MATRIX
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2

; set up initial coordinate basis: Z axis is the spin axis,
;                  X axis points toward nadir, Y axis is Z-cross-X
   SCX = DBLARR(3)
   SCY = SCX
   SCZ = SCX
   SC2E = SCX
   tempY = SCX
   tempX = SCX
   rotX = SCX
   rotY = SCY
   rotX_GCI = SCX
   rotY_GCI = SCX

   SCZ = record.sc_spinv_gci
   IF (sensor eq 1) THEN SCZ = -record.sc_spinv_gci
   sc_pos_len =  XV_dvNorm(record.sc_pos_gci)
   SC2E = -DOUBLE(record.sc_pos_gci)
   sc_pos_len =  XV_dvNorm(SC2E)
   norm_sc2Earth = transpose(SC2E / sc_pos_len)
   tempY = CROSSP( SCZ, norm_sc2Earth )
   tempY_len = XV_dvNorm(tempY)
   SCY = tempY / tempY_len
   tempX = CROSSP( SCY, SCZ )
   tempX_len = XV_dvNorm(tempX)
   SCX = tempX / tempX_len
   BASIS0 = [SCX,SCY,SCZ]
   BASIS0 = REFORM(BASIS0,3,3,/OVERWRITE)
; rotate on Z axis by (-)pitch angle
   Phi = -record.ppitch/10.0
   IF (sensor eq 1) THEN Phi = record.ppitch/10.0
   rotX = [ COS(Phi*!DTOR), SIN(Phi*!DTOR), 0.0 ]
   rotY = [ -SIN(Phi*!DTOR), COS(Phi*!DTOR), 0.0 ]
; rotate to GCI with basis0 matrix
   rotX_GCI = rotX ## BASIS0
   rotY_GCI = rotY ## BASIS0
   BASIS1 = [rotX_GCI,rotY_GCI,SCZ]
   BASIS1 = REFORM(BASIS1,3,3,/OVERWRITE)

; apply mirror pointing angles and alignment corrections
   IF (sensor EQ 0) THEN BEGIN
        HDUMP = FIX(HEADER(38) AND 15)*256 + FIX(HEADER(37)) - 20
	IF (record.time_pb5(0) EQ 1996 AND record.time_pb5(1) LT 110) THEN $
	  Zdeg = DOUBLE(( 10.+Hdump-24.)*.0390625) $     ; original matrix 
	ELSE Zdeg = DOUBLE((10.+Hdump-16.)*.0390625)	 ;   zdeg
	Ydeg = DOUBLE((39.+8.)*.0390625)		 ;  and ydeg

	zdeg = zdeg + (5.0+1.0)*.0390625
	ydeg = ydeg + (-3.0)*0.0390625
   ENDIF
   IF (sensor EQ 1) THEN BEGIN
;** Zdeg and Ydeg determination modifications required due to probable two-axis
;** tip/tilt of axes relative to instrument coordinates
	AZ = DOUBLE(record.Mirr_Azm)
        EL = DOUBLE(record.Mirr_Elv)
	Zdeg = -(EL-108.0)*.08251-(AZ-68.0)*0.0011
	Ydeg = -(AZ-68.0)*.09064-(EL-108.0)*(4.26E-03)- $
		(.25022*(EL-108.0)^2.)/(80.^2.)
	Zdeg = Zdeg + 50. * .01171875	    ; .01171875 is half of a LR pixel
	Ydeg = Ydeg + 73. * .01171875
	Zdeg = -Zdeg
	Ydeg = -Ydeg
   ENDIF
; find image center line-of-sight vector - measured from nominal 0,0
   Theta = 90. - Zdeg
   Phi = Ydeg
   CLOSV = [ SIN(Theta*!DTOR)*COS(Phi*!DTOR), $
		SIN(Theta*!DTOR)*SIN(Phi*!DTOR), COS(Theta*!DTOR) ]
; rotate to GCI with basis1 matrix
   CLOSV_GCI = CLOSV ## BASIS1
	XV_RECSPHD,closv,r,th,ph

; set up the image coordinate basis: X axis is the center line-of-sight,
;	Y axis is spin axis-cross-X (up),
;	Z axis is X-cross-Y (rightward in the spin - line-of-sight plane);
;    for the visible camera, Y and Z are additionally rotated by an angle
;    determined from the mirror elevation
   tempX_len = XV_dvNorm(CLOSV_GCI)
   IMX = TRANSPOSE(CLOSV_GCI/tempX_len)
   tempY = CROSSP( SCZ, TRANSPOSE(IMX) )
   tempY_len = XV_dvNorm(tempY)
   IMY = TRANSPOSE( tempY/tempY_len )
   tempZ = CROSSP( TRANSPOSE(IMX), TRANSPOSE(IMY) )
   tempZ_len = XV_dvNorm(tempZ)
   IMZ = TRANSPOSE( tempZ/tempZ_len )
   newmatr = REFORM([IMX,IMY,IMZ],3,3,/OVERWRITE)

; apply the additional rotation for the elevation of the visible camera
   IF (sensor EQ 1) THEN BEGIN
   	Angle = 2.5 * (record.Mirr_Elv-108.0) / 64.0
	Angle = -Angle
   	CosAng = COS(Angle*!DTOR)
   	SinAng = SIN(Angle*!DTOR)
   	tempZ = SinAng*IMY + CosAng*IMZ
   	tempZ_len = XV_dvNorm(TRANSPOSE(tempZ))
   	IMZ = tempZ/tempZ_len
   	tempY = CosAng*IMY - SinAng*IMZ
   	tempY_len = XV_dvNorm(TRANSPOSE(tempY))
   	IMY = tempY/tempY_len
   ENDIF

; return IMX,IMY,IMZ as the Rotation Matrix for this image
   Return,REFORM([IMX,IMY,IMZ],3,3,/OVERWRITE)
END


;-------------------------------------------------------------
;+
; NAME:
;       XV_LOOKV_TO_GCI
; PURPOSE:
;       Converts the XVIS LOOK vector to GCI coordinates
; CATEGORY:
;
; CALLING SEQUENCE:
;       XV_LOOKV_TO_GCI
; INPUTS:
;       NONE
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       NONE
; COMMON BLOCKS:
;       XV_RECORD_DATA
;       XV_FILE_DATA
;       XV_DERIVED_DATA
;       XV_FLAGS
; NOTES:
;       This routine is useful only within the XVIS application
;       It uses COMMON blocks extensively and certain values within
;       the blocks must be set prior to invocation.
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;	Rae Dvorsky, 03/19/05
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-------------------------------------------------------------
PRO XV_LOOKV_TO_GCI
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons

   ROTATION = XV_MAKE_ROTATION_MATRIX()
   LOOKV_GCI =  REFORM(TRANSPOSE(ROTATION##TRANSPOSE(REFORM(LookVector,3,65536))),3,256,256)

END


;------------------------------------------------------------------
;+
; NAME:
;       XV_GET_PHIS
; PURPOSE:
;       Computes the PHI angles at every pixel within an XVIS image.
;  ===>  [ PHI is the angle between look direction vector and the
;             negative s/c position vector ]
; CATEGORY:
;
; CALLING SEQUENCE:
;       XV_GET_PHIS
; INPUTS:
;       NONE
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       NONE
; COMMON BLOCKS:
;       XV_RECORD_DATA
;       XV_FILE_DATA
;       XV_DERIVED_DATA
;       XV_FLAGS
; NOTES:
;       This routine is useful only within the XVIS application
;       It uses COMMON blocks extensively and certain values within
;       the blocks must be set prior to invocation.
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-----------------------------------------------------------------
PRO XV_GET_PHIS
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons


; Old version -- as of xvis 2.43

;   phis =  dblarr(256,256,/NOZERO)
;   sc_pos = record.sc_pos_gci
;   normal_sc_pos =  TRANSPOSE(-sc_pos / XV_vnorm(sc_pos))

;   Phis = REFORM(acos(reform(lookv_gci,3,65536) ## normal_sc_pos),256,256)

; New version -- from xvis 2.50 -- may not be correct !!!

   phis =  dblarr(256,256,/NOZERO)
   sc_pos = record.sc_pos_gci
   normal_sc_pos =  TRANSPOSE(-sc_pos / XV_vNorm (sc_pos))
   lvgn = reform(lookv_gci,3,65536) ## normal_sc_pos
   lv1 = where(lvgn gt 1.000,count)
   if (count gt 0.) then lvgn(lv1) = 1.000
   Phis = REFORM(acos(lvgn),256,256)
   
END


;------------------------------------------------------------------
;+
; NAME:
;       XV_GET_ALTLS
; PURPOSE:
;       Computes the ALTLS value (altitude at line-of-sight) for every
;       pixel of an XVIS image.
;    ===>    [ These are actually radial distances, not altitudes.]
; CATEGORY:
;
; CALLING SEQUENCE:
;       XV_GET_ALTLS
; INPUTS:
;       NONE
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       NONE
; COMMON BLOCKS:
;       XV_RECORD_DATA
;       XV_FILE_DATA
;       XV_DERIVED_DATA
;       XV_FLAGS
; NOTES:
;       This routine is useful only within the XVIS application
;       It uses COMMON blocks extensively and certain values within
;       the blocks must be set prior to invocation.
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-----------------------------------------------------------------
PRO XV_GET_ALTLS
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
   COMMON XV_FLAGS, Flags

; altls = radial distance of line-of-sight
   ALTLS =  XV_vnorm(record.sc_pos_gci) * sin(phis)

END


;-----------------------------------------------------------------
; NAME:
;       XV_ALT_SZA
; PURPOSE:
;     For every pixel of an XVIS image, computes radial distance and
;	solar zenith angle
; CATEGORY:
;
; CALLING SEQUENCE:
;       XV_ALT_SZA
; INPUTS:
;       NONE
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       NONE
; COMMON BLOCKS:
;       XV_RECORD_DATA
;	XV_EARTH
;       XV_FILE_DATA
;       XV_DERIVED_DATA
;       XV_FLAGS
; NOTES:
;       This routine is useful only within the XVIS application
;       It uses COMMON blocks extensively and certain values within
;       the blocks must be set prior to invocation.
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;	Rae Dvorsky  June 2013  fixed some array arithmetic problems
;-----------------------------------------------------------------
PRO XV_ALT_SZA, LUN=lun
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_EARTH, EARTH_RADIUS, MAX_RADIUS, MIN_RADIUS
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons

   AssumedAlt =  record.altf
			; altf = assumed altitude
   SUN_VCTR =  record.sun_vctr
   SC_POS_LEN =  XV_vnorm(record.sc_pos_gci)

   XV_LOOKV_TO_GCI
   XV_GET_PHIS
   XV_GET_ALTLS		   	; altls = radial distance of line-of-sight

   szas =  fltarr(256,256)
   LOCs =  fltarr(3,65536)
   alts = fltarr(65536)
   coss =  cos(phis) * sc_pos_len
   szas(*) =  -1
   szas = reform(szas,65536)
   sc_pos = fltarr(3,65536)
   sc_pos(0,*) = record.sc_pos_gci(0)
   sc_pos(1,*) = record.sc_pos_gci(1)
   sc_pos(2,*) = record.sc_pos_gci(2)

   lookv_gci =  reform(lookv_gci,3,65536)
   coss =  reform(coss,65536)
   XV_VPOINT, sc_pos, lookv_gci, assumedalt, np, loc1, loc2, LUN=lun

   j =  where(np EQ 0, count)
   IF(count GT 0) THEN BEGIN
      locs(0,j) =  sc_pos(0,j) + lookv_gci(0,j) * coss(j)
      locs(1,j) =  sc_pos(1,j) + lookv_gci(1,j) * coss(j)
      locs(2,j) =  sc_pos(2,j) + lookv_gci(2,j) * coss(j)
      alts(j) = altls(j)
   END

   j =  where(np gt 0, count)
   IF(count GT 0) THEN BEGIN
      sun_vec = fltarr(3,65536)
      norms = sun_vec
      sun_vec(0,*) = record.sun_vctr(0)
      sun_vec(1,*) = record.sun_vctr(1)
      sun_vec(2,*) = record.sun_vctr(2)
      locs(*,j) =  loc1(*,j)
      alts(j) = XV_arrayNorm(locs(*,j))
		; alts = radial distance of points on
		; Earth at Earth radius + assumed altitude; 
		; if not on Earth then radial distance 
		; of line-of-sight at point closest to Earth
     norms(0,j) = locs(0,j)/alts(j)
     norms(1,j) = locs(1,j)/alts(j)
     norms(2,j) = locs(2,j)/alts(j)
      szas(j) = acos(total( sun_vec(*,j) * norms(*,j) , 1)) * !RADEG
   end
   szas = reform(szas,256,256)
   locs =  reform(locs,3,256,256)
   lookv_gci =  reform(lookv_gci,3,256,256)

   ; does this statement actually do anything? it was removed in xvis 2.50
   szas = reform(szas,256,256)

END


;-------------------------------------------------------------
;+
; NAME:     GCI_TO_GEO
;
; PURPOSE:
;       Converts a GCI coordinate into geographic lat,lon,alt (alt=altitude)
; CATEGORY:
;
; CALLING SEQUENCE:
;       GCI_TO_GEO, Pos, gla, glo, alt
; INPUTS:
;       POS == a position in GCI coordinates
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       GLA == geographic latitude
;       GLO == geographic longitude
;       ALT == altitude
; COMMON BLOCKS:
;       XV_RECORD_DATA
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-------------------------------------------------------------
PRO XV_GCI_TO_GEO, Pos, gla, glo, alt
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   sunra = atan(record.sun_vctr(1), record.sun_vctr(0)) * !RADEG
   XV_GCIGEO, Record.time_pb5, sunra, pos, gla, glo, alt
END


;-------------------------------------------------------------
;+
; NAME:
;       GCI_TO_SZA
; PURPOSE:
;       Converts a GCI coordinate into a solar zenith angle
; CATEGORY:
;
; CALLING SEQUENCE:
;       SAngle = GCI_TO_SZA(Loc)
; INPUTS:
;       Pos == a position in GCI coordinates
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       Solar zenith angle in degrees
; COMMON BLOCKS:
;       XV_RECORD_DATA
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-------------------------------------------------------------
FUNCTION XV_GCI_TO_SZA, Pos
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   sz =  N_ELEMENTS(pos) / 3.0
   sunv = fltarr(3,sz)
   npos = fltarr(3,sz)
   sunv(0,*) = record.sun_vctr(0)
   sunv(1,*) = record.sun_vctr(1)
   sunv(2,*) = record.sun_vctr(2)
   normpos = XV_arraynorm(pos)
   npos(0,*) = pos(0,*)/normpos(*)
   npos(1,*) = pos(1,*)/normpos(*)
   npos(2,*) = pos(2,*)/normpos(*)
   gszas = acos( total( sunv*npos, 1 ) ) * !RADEG
   return, gszas
END
;+

; Bobby may not need
;-------------------------------------------------------------
;+
; NAME:      SINGLE_PIXEL_CRD
;
; PURPOSE:
;       Computes coordinates for one image pixel location
; CATEGORY:
;
; CALLING SEQUENCE:
;       coord = SINGLE_PIXEL_CRD( X, Y, ON_EARTH )
; INPUTS:
;       X,Y == pixel location column and row
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       If look direction at input pixel location intersects Earth,
;       GCI coordinates of position on surface of Earth closer to spacecraft;
;       else GCI unit vector for look direction
;
;       ON_EARTH == number of points of intersection with Earth, 0, 1, or 2
;
; COMMON BLOCKS:
;       XV_FILE_DATA
;       XV_RECORD_DATA
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;	Rae Dvorsky, 3/19/05  to call make_rotation_matrix
;		     NOV 2012 to call vpoint instead of point
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-------------------------------------------------------------
;FUNCTION XV_SINGLE_PIXEL_CRD, X, Y, ON_EARTH
;   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
;   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Curr_Limit, sensor, Record2
;   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons

;   ROTATION_MATRIX = XV_MAKE_ROTATION_MATRIX()
;   LookV = REFORM( ROTATION_MATRIX ## LookVector(*,X,Y) )
;   XV_vpoint, record.sc_pos_gci, lookv, record.altf, ON_EARTH, loc1, loc2
;   ON_EARTH =  ON_EARTH[0]
;   IF(ON_EARTH EQ 0) THEN BEGIN
;      sc_pos_len =  XV_vnorm(record.sc_pos_gci)
;      normal_sc_pos =  transpose(-record.sc_pos_gci / sc_pos_len)
;      Phi =  (acos(lookv ## normal_sc_pos))(0)
;      return, record.sc_pos_gci + lookv * cos(phi) * sc_pos_len
;   END ELSE return, loc1
;END

;-------------------------------------------------------------
;+
; NAME:
;       COMPUTE_CRDS
; PURPOSE:
;       Computes all the variables in the XV_DERIVED_DATA block
;       for an entire image
; CATEGORY:
;
; CALLING SEQUENCE:
;       Must set up the common block information properly.  This
;       routine is tightly integrated into the XVIS package.
; INPUTS:
;       None
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
; COMMON BLOCKS:
;       numerous
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-
PRO XV_COMPUTE_CRDS, LUN=lun
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
;   COMMON XV_FLAGS, Flags
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_GEOCO_FILL, glat_fill, glon_fill

;   COMMON XV_WIDS, MainWid, ViewWid, DrawWid, Handlers, HCount

   GLATS = DBLARR(256,256)
   GLONS = DBLARR(256,256)
   DaltS = DBLARR(256,256)

   GLATS [*] = glat_fill
   GLONS [*] = glon_fill
   dalts [*] = 0.D

   SC_POS =  record.sc_pos_GCI
   AssumedAlt =  record.altf
   SUN_VCTR =  record.sun_vctr
   sunra = atan(sun_vctr(1), sun_vctr(0)) * !RADEG

   XV_ALT_SZA, LUN=lun

   valid =  where(szas GE 0, count)
   IF(count GT 0) THEN BEGIN
      XV_GCIGEO, Record.Time_pb5, SUNRA, (REFORM(locs,3,65536))[*,valid],$
			 GLA, GLO, ALT
					; alt = altitude
      GLATS[valid] =  gla
      GLONS[valid] =  glo
      DALTS[valid] =  alt
   END

END

;-------------------------------------------------------------
;+
; NAME:
;       DAYGLOW
; PURPOSE:
;       Computes percentage parameters for the dayglow subtract
;       routine
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;       SZA == solar zenith angle
;       Day == day of year
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       percentages parameterized on solar zenith angles
; COMMON BLOCKS:
;       None
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-
FUNCTION XV_DAYGLOW, SZA, Day
   SZAINTEN =  [[25.0, 20.0, 16.3,  8.3,  0.0,   0.0],$
                [50.0, 40.0, 32.5, 16.5,  0.0,   0.0]]
   SZAANGLE =  [0.0, 30.0, 40.0, 75.0, 90.0, 180.0]

   IF(Day LT 241) THEN BEGIN
      RETURN, INTERPOL(SZAINTEN(*,0),szaangle,sza)
   END ELSE BEGIN
      RETURN, INTERPOL(SZAINTEN(*,1), szaangle,sza)
   END

END


;-------------------------------------------------------------
;+
; NAME:
;       XV_UNPACK_WHERE
; PURPOSE:
;       An often used utility that converts a single value
;       index into a vis image into it's respective row,col
;       values.
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;       INDEX == array of indices into a vis image.
; KEYWORD PARAMETERS:
;       NONE
; OUTPUTS:
;       ROW == array of row indices
;       COL == array of column indices
; COMMON BLOCKS:
;       None
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-
PRO XV_UNPACK_WHERE, index, row, col
   col = index MOD 256
   row = index / 256
END

;-------------------------------------------------------------
;+
; NAME:
;       INIT_COMPRESSION_TABLES
; PURPOSE:
;       Vis images are often compressed and uncompressed.  To
;       do this we create arrays to quickly perform the compression.
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;       None
; KEYWORD PARAMETERS:
;       None
; OUTPUTS:
;       None
; COMMON BLOCKS:
;       Sets up the COMPRESSION_TABLES common block variables
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;	Rae Dvorsky, March 2014  added code for gain other than 255
;       Rae Dvorsky, Dec. 2014   changed ROUND to FIX when making PACK_T
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-
PRO XV_INIT_COMPRESSION_TABLES
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON COMPRESSION_TABLES, PACK_T, UNPACK_T
 
	;unpack table gives packed value for each unpacked value
	;pack table gives unpacked value for each packed value

   UNPACK_T = INTARR(4096) 
   gain = fix(header(13))

 if (gain eq 255) then begin

   ILOW = INTARR(256)
   IHIGH = INTARR(256)

   UNPACK_T(0:63) = INDGEN(64)
   UNPACK_T(64:127) = 64 + INDGEN(64)/2
   UNPACK_T(128:255) = 96 + INDGEN(128)/4
   UNPACK_T(256:511) = 128 + INDGEN(256)/8
   UNPACK_T(512:1023) = 160 + INDGEN(512)/16
   UNPACK_T(1024:2047) =  192 + INDGEN(1024)/32
   UNPACK_T(2048:4095) =  224 + INDGEN(2048)/64

   FOR j=0,255 do BEGIN
      K = WHERE(UNPACK_T eq J)
      ILow(j) = K(0)
      IHigh(j) = K(N_ELEMENTS(K)-1)
   END
 
   PACK_T = FIX((ILow+IHigh)/2.0 + .5)
 endif else begin

  PACK_T = findgen(256)*gain
  UNPACK_T = FIX(findgen(4096)/gain)

 endelse
END

;-------------------------------------------------------------
;+
; NAME:
;       UNPACK
; PURPOSE:
;       Uncompress a compressed image
; CATEGORY:
;
; CALLING SEQUENCE:
;       UncompressedImage = UNPACK(CompressedImage)
; INPUTS:
;       Image
; KEYWORD PARAMETERS:
;       None
; OUTPUTS:
;       An uncompressed image
; COMMON BLOCKS:
;       COMPRESSION_TABLES
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-
FUNCTION XV_UNPACK, Image
   COMMON COMPRESSION_TABLES, PACK_T, UNPACK_T
;  IF(N_ELEMENTS(PACK_T) EQ 0) THEN XV_INIT_COMPRESSION_TABLES

;  Changed so failure to initialize the compression tables will result in an
;  error.
  
   IF  (N_ELEMENTS(PACK_T) EQ 0) THEN BEGIN
      
       MESSAGE, 'XV_UNPACK called without intializing compression tables.'

   ENDIF
 
   newImage =  PACK_T(Image)
   RETURN,newImage
END

;-------------------------------------------------------------
;+
; NAME:
;       PACK
; PURPOSE:
;       Compress an un- compressed image
; CATEGORY:
;
; CALLING SEQUENCE:
;       CompressedImage = UNPACK(UncompressedImage)
; INPUTS:
;       Image
; KEYWORD PARAMETERS:
;       None
; OUTPUTS:
;       An uncompressed image
; COMMON BLOCKS:
;       COMPRESSION_TABLES
; NOTES:
;
; MODIFICATION HISTORY:
;       Kenny Hunt, 9/1/97
;
; Copyright (C) 1998, The University of Iowa Department of Physics and Astronomy
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;-
FUNCTION XV_PACK, Image
   COMMON COMPRESSION_TABLES, PACK_T, UNPACK_T
;   IF(N_ELEMENTS(UNPACK_T) EQ 0) THEN XV_INIT_COMPRESSION_TABLES

;  Changed so failure to initialize the compression tables will result in an
;  error.
  
   IF  (N_ELEMENTS(UNPACK_T) EQ 0) THEN BEGIN

       MESSAGE, 'XV_PACK called without intializing compression tables.'

   ENDIF

   newImage =  UNPACK_T(Image)
   return,BYTE(newImage)
END

;-------------------------------------------------------------

;+
; NAME:
;       XV_SCALE_COLOR_TABLE
; PURPOSE:
;       Fits the color table from the CDF file into the table
;       size used in the current IDL session and the min/max
;       values specified.
; CATEGORY:
;
; CALLING SEQUENCE:
;       XV_SCALE_COLOR_TABLE
; INPUTS:
;       MIN = actual colors begin here.  Anything under this value
;             is set to the min value.
;       MAX = actual colors end here.  Anything over this value
;             is set to the max value.
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; COMMON BLOCKS:
;       COLORS
; NOTES:
;
; MODIFICATION HISTORY:
;-
PRO XV_SCALE_COLOR_TABLE, min, max, DISPLAY=display
   COMMON COLORS, rr, gg, bb, rc, gc, bc

   ; tsize =  n_elements(rr)
   tsize =  max - min
   ;;; need to scale min and max
   tratio = float(tsize) / 256.0
   min = FIX(tratio * min)
   max = FIX(tratio * max)
   IF(tsize GT 0) THEN BEGIN
      ncolors =  max-min+1
      rc(0:min) =  rr(0)
      gc(0:min) =  gg(0)
      bc(0:min) =  bb(0)
      rc(max:*) =  rr(tsize-1)
      gc(max:*) =  gg(tsize-1)
      bc(max:*) =  bb(tsize-1)
      rc(min:max) =  congrid(rr,ncolors)
      gc(min:max) =  congrid(gg,ncolors)
      bc(min:max) =  congrid(bb,ncolors)
      IF KEYWORD_SET (display) THEN tvlct,rc,gc,bc
   END
END

;----------------------------------------------------------
; Procedure name: XV_REMOVE_WEAVE
; PURPOSE:
;  Corrects for the weave pattern in an image
;
; CALLING SEQUENCE:
;  im = DIST(256,256)
;  XV_REMOVE_WEAVE,im
;
; INPUTS:
;  IMAGE == A 2D array of bytes
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Corrects the Image
;
; COMMON BLOCKS:
;  XV_IMPROPARAMS
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, 2005
;  Rae Dvorsky  July 2013  changed to use default threshold
;  Rae Dvorsky  Aug. 2014  changed to black out saturated pixels
;  Rae Dvorsky  March 2015 XV_SUB_COSMIC_RAY call bypassed
;----------------------------------------------------------
Pro XV_Remove_Weave, Image
   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField, MJD
;   XV_SUB_COSMIC_RAY, Image, [-1], 0, 7, Threshold
   k = where(Image eq 255,count)
;   if(count gt 0) then Image(k) = 0
   if(count gt 0) then Image[k] = 0
;   Image = XV_UNPACK(image)
   sz = SIZE(image)
   cols = sz(1)
   rows = sz(2)

   isum = INTARR(5)
   idelavg = INTARR(5)
   colindex = INDGEN((cols-10)/5)*5 + 5
   FOR row=0,rows-1 DO BEGIN
       FOR i=0,4 DO BEGIN
           c = colindex + i
           isum(i) = TOTAL(Image(c,row))
       ENDFOR
       amean = TOTAL(isum)/5.0
       idelavg = FIX((FLOAT(isum)-amean)/49.0)
       FOR c=0,cols-1 DO BEGIN
           i = c MOD 5
           Image(c,row) = Image(c,row) - idelavg(i)
       ENDFOR
   ENDFOR
;   Image =  XV_PACK(Image)
End

;----------------------------------------------------------
; PURPOSE:
;  Reads a single record from a CDF file.
;
; INPUTS:
;  None.  All the relevant information is passed via COMMON
;  blocks.
;
; KEYWORD PARAMETERS:rr
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Sets appropriate flags.
;
; COMMON BLOCKS:
;   XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
;   XV_FLAGS, Flags
;   XV_RECORD_DATA, Image, Record, ROI, LastImage, Curr_Limit, sensor
;   XV_FILE_DATA, Path, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistort
;   XV_WIDS, MainWid, ViewWid, DrawWid, Handlers, HCount
;   COLORS, rr,gg,bb,rc,gc,bc
;
; PROCEDURE:
;
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_READ_RECORD
;   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
   COMMON XV_FLAGS, Flags
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
;   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
;   COMMON XV_WIDS, MainWid, ViewWid, DrawWid, Handlers, HCount
   COMMON COLORS, rr,gg,bb,rc,gc,bc

   Image =  XV_GET_IMAGE(Fid,ImageNum,1)
   Image(0:3,*) = 0
   RECORD =  XV_GET_RECORD(Fid,ImageNum,sensor)
;   IF(sensor EQ 1) THEN BEGIN
;	RECORD2 = XV_GET_RECORD2(Fid,ImageNum)
;   ENDIF

; These statements are removed as there is no need to recalculate color tables 
; or maximum and minimum values every record.  This is now done when the
; CDF is opened.

;  Added in version 2.50
; set color limits
;   IF (Limits(1) eq 0) THEN Begin
;      min =  record.limit_lo  ; on first read use these
;      max =  record.limit_hi
;   END ELSE Begin
;      min = Limits(0)             ; use Limits if available, they may be better
;      max = Limits(1)
;   END

;   tsize =  n_elements(rr)
;   min =  record.limit_lo    ; Derives initial version from CDF.  in 2.5 this is calculated.
;   max =  record.limit_hi    ; Derives initial version from CDF.  in 2.5 this is calculated.
;   tratio = float(tsize) / 256.0
;   min = FIX(tratio * min)
;   max = FIX(tratio * max)
   ; CDF_COLOR flag should always equal one, so we should delete it.
   ; Color calculation is not not done version 2.5 of this routine.
;   IF(tsize GT 0 && Flags.CDF_COLOR EQ 1) THEN BEGIN
      
      ; This statment is automatically done in 2.5
      ; what does CURR_LIMIT do????
      ; seems to be used by widgets for displaying images. is adjustable by user.
;      CURR_LIMIT = [min,max]

      ; These statements were move to the procedure XV_OPEN in 2.5
;      ncolors =  record.limit_hi-record.limit_lo+1
;      rc(0:min) =  rr(0)
;      gc(0:min) =  gg(0)
;      bc(0:min) =  bb(0)
;      rc(max:*) =  rr(tsize-1)
;      gc(max:*) =  gg(tsize-1)
;      bc(max:*) =  bb(tsize-1)
;      rc(min:max) =  congrid(rr,max-min+1)
;      gc(min:max) =  congrid(gg,max-min+1)
;      bc(min:max) =  congrid(bb,max-min+1)
;      tvlct,rc,gc,bc
;   END

   Flags.IPP = 0
;   Flags.LV = 0
;   Flags.ALT = 0
;   Flags.ALTLS = 0
;   Flags.PHI =  0
;   Flags.SZA = 0
;   Flags.LOC =  0
;   Flags.GLAT = 0
;   Flags.GLON = 0

;   WIDGET_CONTROL, Wids.Scale, SET_VALUE=ImageNum
   StartTime = record.time_pb5
   StartTime[2] = StartTime[2] - FIX(record.int_time_half) + 1000 ; add 1 second (1000 msec)
;   WIDGET_CONTROL, TopWids.StartDateBid, SET_VALUE="Start : " + DateToString(StartTime,2)
;   WIDGET_CONTROL, TopWids.CenterDateBid, SET_VALUE="Center: " + DateToString(record.time_pb5,2)


;   XV_UPDATE_VIEW_WINDOW
;   XV_UPDATE_IMAGE_INFO
END

;----------------------------------------------------------
; NAME:  XV_SUB_COSMIC_RAY
;
; PURPOSE:
;  Eliminates the effects of heavy particle events on
;  VIS images.
;
; DISCUSSION:
;  The constant threshold level (as opposed to a level based on a local
;  determination of the standard deviation) is used for several reasons.
;  First we are not removing random noise, but in fact are attempting to
;  remove a real signal, that is the charge liberated by the penetrating
;  high energy particle (e.g. cosmic rays, etc.).  Empirical tests of the
;  cosmic rays indicate that they leave a minimum charge in the CCD pixel
;  of around 30 dn (digital number or "counts").  Second, empirical tests of
;  the standard deviations of the brightest portions of the VIS Earth
;  Camera images where no cosmic rays have penetrated indicate standard
;  deviations of between 8 and 15 dn.  Thus the constant threshold level
;  of 30 corresponds to 2 to 3 or more standard deviations above the mean.
;
;  The setting of a constant threshold level becomes particularly important
;  as we begin entry into the radiation belts.  The increased number of high
;  energy particles would lead to an increase in the number of cases in
;  which the calculation of the local standard deviation is in error (i.e.
;  considerably larger due to neighboring penetrating particle events).
;  Consequently, these events would not be removed under a locally varying
;  threshold condition.  However, with apriori knowledge of the variation of
;  the underlying atmospheric measurement helping to determine a constant
;  threshold, these events can be removed reliably.
;
;  As for a written description of the algorithm, for each pixel the median
;  is determined for the 7 x 7 pixel block centered on that pixel (assuming
;  a mask size of 7).  A median instead of average is used in order to
;  eliminate the adverse impact of the cosmic ray outliers.  Pixels that are
;  more than the threshold above their medians are replaced with their
;  median values. From the above discussion we see that these pixels are
;  2 to 3 or more standard deviations higher than the median value
;  for their 7 x 7 pixel block.  Because the penetrating particles often
;  impact neighboring pixels but to a lesser intensity, these pixels are
;  also replaced with their median values.
;
; CALLING SEQUENCE:
;  im = DIST(256,256)
;  XV_SUB_COSMIC_RAY, IM, [-1], 0, 7, 35
;
; INPUTS:
;  IMAGE == A 2D array of bytes
;  RegionPts == a specified region of interest (indices into the image).
;  Mode == one of 0,1,2.
;          Mode 0:  Automatically performs the routine on entire image.
;          Mode 1:  Do only specified region
;          Mode 2:  Do everything except specified region
;  MaskSize == a scalar designating the NxN subregion
;  Threshld == a scalar designating the cutoff value
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  XV_IMPROPARAMS
;  FLAGS
;
; MODIFICATION HISTORY:
;  Rae Dvorsky, April 2013  brought into compliance with VIS Fortran software
;  Rae Dvorsky, Dec. 2014   corrects surrounding pixels only if they exceed
;			    half the threshold difference from median
;  Rae Dvorsky, March 2015  check CRS flag before proceeding; Bias2 initialized 
;----------------------------------------------------------
; PRO XV_SUB_COSMIC_RAY, Image, RegionPts, Mode, MaskSize, Threshld
PRO XV_SUB_COSMIC_RAY, Image, RegionPts, MaskSize, Threshld
   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField, MJD
   COMMON XV_FLAGS, Flags
;   IF(FLAGS.CRS EQ 1) THEN RETURN
   Bias2 = 0
   If (Flags.IPP eq 1) then begin
      Bias2 = MAX([Bias-10,0])
   End 
   Dx = FIX(MaskSize/2)
   Thrd2 = Threshld/2
   ;;;;;;;;;;;;;;;;;;;;;;;
   ;;; Handle AUTO mode
   ;;;;;;;;;;;;;;;;;;;;;;;
;   IF(mode eq 0) THEN BEGIN
      NewImage = FIX(Image)            ; use pseudo-compressed values for this mode
      Med =  median(NewImage,masksize)
      Med(0:3,*) = bias2
      FOR i=0,dx-1 DO BEGIN
         Med(i,*) = median(reform(NewImage(i,*)),masksize)
         Med(255-i,*) = median(reform(NewImage(255-i,*)),masksize)
         Med(*,i) =  median(NewImage(*,i),masksize)
         Med(*,255-i) = median(NewImage(*,255-i),masksize)
      END

      junk = where((NewImage-med) GT threshld,count)
      IF(count GT 0) THEN BEGIN
         NewImage(junk) = med(junk)
         junk2 = [junk-257, junk-256, junk-255, $
                 junk-1, junk, junk+1, $
                 junk+255, junk+256, junk+257]
         j = where(NewImage(junk2)-med(junk2) gt thrd2,jcount)
	 if (jcount gt 0) then begin
            NewImage(junk2(j)) = med(junk2(j))
	 endif
         Image = NewImage
      END
      RETURN
;   END

   ;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Handle modes 1 and 2
   ;;;;;;;;;;;;;;;;;;;;;;;;
;   IF(mode eq 2) THEN BEGIN
;      x = LINDGEN(256,256)
;      x(RegionPts) = -1
;      RegionPts = WHERE(x ge 0)
;   END

;   Check = WHERE(RegionPts lt 0, NotValid)
;   IF(NotValid gt 0) THEN return

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Set up Median array
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   Med =  median(image,masksize)
;   FOR i=0,dx-1 DO BEGIN
;      Med(i,*) = median(reform(image(i,*)),masksize)
;      Med(255-i,*) = median(reform(image(255-i,*)),masksize)
;      Med(*,i) =  median(image(*,i),masksize)
;      Med(*,255-i) = median(image(*,255-i),masksize)
;   END

;   junk = where((image-med) GT threshld,count)
;   IF(count GT 0) THEN BEGIN
;      junk = intersect(RegionPts, junk)
;      IF(N_ELEMENTS(junk) GT 0) THEN BEGIN
;         junk = [junk-257, junk-256, junk-255, $
;                 junk-1, junk, junk+1, $
;                 junk+255, junk+256, junk+257]
;         Image(junk) =  Med(junk)
;         Image = XV_PACK(image)
;     END ELSE Image=XV_PACK(image)
;      RETURN
;   END

;   Image =  PACK(image)

;   FLAGS.CRS =  1

END

;----------------------------------------------------------
; PURPOSE:
;  Compensates for the drift of the CCD bias in a
;  given scan line.
;
; CALLING SEQUENCE:
;  im = DIST(256,256)
;  XV_SUB_SLOPES, Im, 0, 10, 5, 0
;
; INPUTS:
;  IMAGE == A 2D array of bytes
;  Low   == The average value at the low end of the image.
;  High  == The average value at the high end of the image.
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;  Rae Dvorsky, May 2013   corrects both vertical and horizontal slopes;
;			   supercedes XV_SUB_SLOPE
;  Rae Dvorsky, March 2015 minor changes to comply with Fortran version
;----------------------------------------------------------
PRO XV_SUB_SLOPES, Image, Left, Right, Top, Bottom
;   Image = XV_UNPACK(Image)
   newImage = FLOAT(Image)
   seed = 11131965
   perturbedImage = Image + (randomu(seed,256,256) - 0.5)

;;; horizontal slope subtract
   factor = (right-left) / 255.0
   percentages = (FINDGEN(256) * factor) + left
; quadratic added to take more out of the middle 
   horz_adjust = 3.0*(1.0-((FINDGEN(256)-127.5)/127.5)^2.0)
   FOR i = 0,255 DO BEGIN
      newImage(*,i) = perturbedImage(*,i) - percentages - horz_adjust
   END
   n = WHERE(newImage lt 0.0,ncount)
   if (ncount gt 0) then newImage(n) = 0.0
;   Image = PACK(newImage)

;;; vertical slope subtract
;   Image = UNPACK(Image)
;   newImage = FLOAT(Image)
   perturbedImage = newImage + (randomu(seed,256,256) - 0.5)
   factor = (bottom-top) / 255.0
   percentages = (FINDGEN(256) * factor) + top
   FOR i = 0,255 DO BEGIN
      newImage(i,*) = perturbedImage(i,*) - percentages
   END
   n = WHERE(newImage lt 0.0,ncount)
   if (ncount gt 0) then newImage(n) = 0.0

;   Image = XV_PACK(FIX(newImage))
   Image = FIX (newImage)
END


;----------------------------------------------------------
; PURPOSE:
;  Corrects for features of the CCD
;
; CALLING SEQUENCE:
;  Im = DIST(256,256)
;  XV_FLAT_FIELD
;
; INPUTS:
;  IMAGE == A 2D array of bytes
;  FlatField
;  IBias  ( the Bias value in XV_Improparams should be used)
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  XV_RECORD_DATA
;  XV_IMPROPARAMS
;
; MODIFICATION HISTORY:
;  Rae Dvorsky, April 2013 eliminated unnecessary shift input
;  Rae Dvorsky, Dec. 2014  no longer zeros out image counts less than ibias
;  Rae Dvorsky, March 2015 added use of common blocks; updated to match
;                            Fortran version
;  Rae Dvorsky, Sep. 2016  added EC flatfield array modifications as done in
;                            the original Fortran
;----------------------------------------------------------
PRO XV_FLAT_FIELD, image, flatfield, bias, sensor, time_pb5
;   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Curr_Limit
;   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom,$
;	 Threshold, FlatField, MJD

;   IM = FLOAT(UNPACK(Image))
   IM = FLOAT (Image)
   FF = FlatField
   BB = FLOAT(Bias)

; adjustment for EC flatfield (this may not be necessary but is
;               included because it was in the original Fortran code)
   IF (sensor eq 0) THEN BEGIN
      FOR j=79,0,-1 DO BEGIN
         FF(0,j) = (FF(0,j+1)+FF(1,j+1))/2.0
         FF(255,j) = (FF(254,j+1)+FF(255,j+1))/2.0
         FF(1:254,j) = (FF(0:253,j+1)+FF(1:254,j+1)+FF(2:255,j+1))/3.0
      END
      FOR j=232,255 DO BEGIN
         FF(0,j) = (FF(0,j-1)+FF(1,j-1))/2.0
         FF(255,j) = (FF(254,j-1)+FF(255,j-1))/2.0
         FF(1:254,j) = (FF(0:253,j-1)+FF(1:254,j-1)+FF(2:255,j-1))/3.0
      END
   END
   Z = WHERE(FF le 0.0, count)
   IF (count gt 0) THEN FF(Z) = 1.0

; pre-2000 fix to flatfield array for EC central horizontal band
   year = Time_pb5 [0]
   IF (year lt 2000) THEN BEGIN
      coratio = MAKE_ARRAY(256, /FLOAT, /INDEX)
      co2 = MAKE_ARRAY(128, /FLOAT, /INDEX)
      co2 = co2/127.0
      cleft = 0.25
      cmiddle = 1.00
      cright = 0.75
      coratio(0:127) = (cmiddle-cleft)*co2 + cleft
      coratio(128:255) = (cright-cmiddle)*co2 + cmiddle
      FOR j=99,179 DO FF(0:255,j) = 1.0 - (1.0-FF(0:255,j))*coratio
   ENDIF

; flatfielding happens here
   B = WHERE(Image ge BB, count)
   IF (count gt 0) THEN IM(B) = ((IM(B)-BB)/FF(B) + 0.5) + BB

   B = WHERE(IM lt 0.0, count)
   IF (count gt 0) THEN IM(B) = 0.0

;   Image = PACK(FIX(IM))
    Image = FIX (IM)
; Fix EC sensor dim spot
;  If (Record.Sensor eq 0) then begin
   If (sensor eq 0) then begin

      Im0 = Image(112:114,50:51)
      Im1 = Reform([Image(110:111,48:53),Image(115:116,48:53)],12,2)
      Im2 = [Image(112:114,48:49),Image(112:114,52:53)]
      Med12 = median([Im1,Im2])
      Med0 = median(Im0)
      D =  median([Im1,Im2]) - median(Im0)
      p = Where(Med12-Im0 gt (D*0.75),count)
      If (count gt 0) Then Im0(p) = Im0(p) + D
      Image(112:114,50:51) = Im0

   Endif

END
;----------------------------------------------------------
; PURPOSE:
;
;  Subtract dayglow from Earth camera images
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  XV_DERIVED_DATA
;
; MODIFICATION HISTORY:
;  Rae Dvorsky, June 2013  adjusted to use radial distance unique to each pixel
;  Rae Dvorsky, March 2015 modifications to match Fortran version
;
;----------------------------------------------------------
PRO XV_DAYGLOW_SUBTRACT, Image, Record, LookVector
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
;   newImage = XV_UNPACK(Image)
   newImage = Image
   TIME = Record.Time_PB5
   ALTF =  record.altf
;   compute_crds
;   XV_LOOKV_TO_GCI
;   XV_ALT_SZA
   MaxAlts_rd = xv_radius(Glats)
   Altfs_rd = MaxAlts_rd + Altf	    ; radial distance to Altf km alt
   MaxAlts_rd = MaxAlts_rd + 500.0  ; radial distance to 500.0 km alt 
   BackGround =  15
   Day =  Time(1)

   Junk =  where(Szas GT 0, Count)
   IF(count GT 0) THEN BEGIN
      newImage(junk) =  newImage(junk) - XV_DayGlow(SZAs(junk),Day)
   END

   Junk = where ( Altls GT (maxalts_rd), Count )    ; - off Earth to line-of-sight
   IF(count GT 0) THEN BEGIN
      zeros =  where(newImage(junk) Lt BackGround, Count)
      IF(Count GT 0) THEN BEGIN
         x =  newImage(junk)
         x(zeros) =  BackGround
         newImage(junk) =  x
      END
      newImage(junk) =  newImage(junk) - BackGround
   END

   Junk = where(SZAs LT 0 AND Alts LT MaxAlts_rd, Count)
   IF(count GT 0) THEN BEGIN
      sc_pos = fltarr(3,Count)
      sc_pos(0,*) = record.sc_pos_gci(0)
      sc_pos(1,*) = record.sc_pos_gci(1)
      sc_pos(2,*) = record.sc_pos_gci(2)
      lookv_gci = reform(lookv_gci,3,65536)
      look = fltarr(3,Count)
      look(0,*) = lookv_gci(0,junk)
      look(1,*) = lookv_gci(1,junk)
      look(2,*) = lookv_gci(2,junk)
      scr = XV_vnorm(record.sc_pos_gci)
      Earthv = fltarr(3,Count)
      bvctrs = fltarr(3,Count)
      Earthv(0,*) = -record.sc_pos_gci(0)/scr
      Earthv(1,*) = -record.sc_pos_gci(1)/scr
      Earthv(2,*) = -record.sc_pos_gci(2)/scr
      alphas = acos(total(Earthv*look,1))
      bsides = cos(alphas)*scr
      bvctrs(0,*) = look(0,*)*bsides
      bvctrs(1,*) = look(1,*)*bsides
      bvctrs(2,*) = look(2,*)*bsides
      points = sc_pos + bvctrs
      lookv_gci = reform(lookv_gci,3,256,256)
      pSZAs = XV_GCI_to_SZA(points)
      DayGlows =  XV_DayGlow(pSZAs,Day)
      Percentages =  1.0 - (Alts(Junk)-ALTFs_rd(Junk)) $
				/ (MaxAlts_rd(Junk)-ALTFs_rd(Junk))
      newImage(junk) =  newImage(junk) - $
		( (DayGLows-background)*Percentages + background )
   END

;   Image = XV_PACK(FIX(newImage))
    Image = FIX (newImage)
END

;----------------------------------------------------------
; PURPOSE: Set low-count pixels on Earth's nightside to minimum value
;  
; CALLING SEQUENCE:  XV_NIGHTGLOW_MINIMUM
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, July 2013
;
;----------------------------------------------------------
PRO XV_NIGHTGLOW_MINIMUM, Image, Record, LookVector
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons

;   XV_compute_crds
;   XV_LOOKV_TO_GCI
;   XV_ALT_SZA
   Minimum = 10
   Epix = where( SZAs gt 0.0, ECount)
   If (ECount gt 0) then begin
	NGpix = where(Image(Epix) lt Minimum, NGCount)
	if (NGCount gt 0) then Image(Epix(NGpix)) = Minimum
   Endif
END

;----------------------------------------------------------
FUNCTION XV_HEC4_Weave_Removal, aim, residual, col0, col1
;  Weave removal for EC horizontal smooth 4
     new_aim = aim
     offset = [3,4,7,8,11,12,15,18,19,22,23,26]
     scalfac = [ 1.97,0.00,-0.02,0.88,2.21,-1.09,-0.51,$
			-0.49,0.83,1.22,-1.03,-0.30 ]
     m = where(offset le col0,nm)
     p = where(offset le (255-col1),np)
     nfacs = nm+np
     For C=col0,col1 Do Begin
       if (nm gt 0) then begin
         cm = [offset(m)]
         for N=0,nm-1 do begin
           new_aim(C,*) = new_aim(C,*) - (residual(C-cm(N),*)*scalfac(N))/nfacs
         endfor
       endif
       if (np gt 0) then begin
         cp = [offset(p)]
         for N=0,np-1 do begin
           new_aim(C,*) = new_aim(C,*) - (residual(C+cp(N),*)*scalfac(N))/nfacs
         endfor
       endif
     Endfor
    return, new_aim
END

;----------------------------------------------------------
; PURPOSE: Corrects Earth Camera image for post 01 March 2005 problems.
;  
; CALLING SEQUENCE:  XV_HORIZONTAL_SMOOTH_EC_4, Image
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  XV_RECORD_DATA
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, November 2013, based on Fortran code by J.B. Sigwarth
;
;----------------------------------------------------------
PRO XV_Horizontal_Smooth_EC_4, image
;  COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Curr_Limit, sensor, Record2
     aim = fltarr(256,256)
     avgim = aim
     jmn = intarr(256)
     jmx = jmn
     javg = jmn
  
;  unpack image counts
;    aim = FLOAT(XV_UNPACK(Image))
     aim = FLOAT (Image) 

;  Determine the average signal plus background
     j = INDGEN(256)
     jmn = j - 7
     jmn(WHERE(jmn lt 0)) = 0
     jmx = j + 7
     jmx(WHERE(jmx gt 255)) = 255
     fnj = jmx - jmn
  
   for col=0,255 do begin
      for row=0,255 do begin
         avgim(col,row) = ( TOTAL(aim(col,jmn(row):jmx(row)))  $
		 - aim(col,row) ) / fnj(row)
      endfor
   endfor

;--Do the row mean correction iteration 10 times for all rows so that
;--the first seven rows and last seven rows
;--are corrected to the best of our abilities.
;--Correct the original image by 1/2 of the mean of the residual for each
;--row.  Doing this 10 times should yield an error of
;--less then +/- 0.001 * pixel intensity.
;---Use only 1/2 the rowavg so the iterative solution approaches the final
;---result in a controlled manner (i.e. the solution won't oscillate).
   for iter=1,10 do begin
      residual = aim - avgim	;create residual image
      for row=0,255 do begin
	 rowresid = residual(*,row)
         rowavg = TOTAL(rowresid)/(256.0)/2.0   ;determine half mean
         aim(*,row) = aim(*,row) - rowavg
      endfor
   endfor
;--Now perform the procedure again for rows 8-249.
;--This leaves rows 1-7 and 250-256 untouched "anchor lines"
;--for the iterative solution.  We did the best we could above.
;--Do 50 iterations
   for iter=1,50 do begin
      for row=0,255 do begin
         for col=0,255 do begin
            avgim(col,row) = ( TOTAL(aim(col,jmn(row):jmx(row)))  $
		 - aim(col,row) ) / fnj(row)
         endfor
      endfor
      for ii=1,5 do begin
         residual(*,7:248) = aim(*,7:248) - avgim(*,7:248) ;create residual image
         for row=7,248 do begin
	    rowresid = residual(*,row)
            rowavg = TOTAL(rowresid)/(256.0)/2.0 ;determine half mean
            aim(*,row) = aim(*,row) - rowavg
         endfor
      endfor
   endfor
;--Slope removal of the noise
;--Create the current average image
   for col=0,255 do begin
      for row=0,255 do begin
         avgim(col,row) = ( TOTAL(aim(col,jmn(row):jmx(row)))  $
		 - aim(col,row) ) / fnj(row)
      endfor
   endfor
;--Create the residual image
   residual = aim - avgim
;--Determine the average of each row in the residual image; remove
;-----from residual image; then find best fit line to residual row
;-----and correct for residual slope
   xline = FINDGEN(256) + 1.0
   sumx = TOTAL(xline)
   sumx2 = TOTAL(xline*xline)
   for row=0,255 do begin
      rowavg = TOTAL(residual(*,row))/(256.0)
      aim(*,row) = aim(*,row) - rowavg
      residual(*,row) = residual(*,row) - rowavg
      sumy = TOTAL(residual(*,row))
      sumxy = TOTAL(xline*residual(*,row))
      delta = 256.0*sumx2 - sumx^2.0
      aintercept = (sumx2*sumy-sumx*sumxy)/delta
      bslope = (256.0*sumxy - sumx*sumy)/delta
      aim(*,row) = aim(*,row) - (aintercept + bslope*xline)
   endfor
;--End of slope removal

;--Create new average image
   for col=0,255 do begin
      for row=0,255 do begin
         avgim(col,row) = ( TOTAL(aim(col,jmn(row):jmx(row)))  $
		 - aim(col,row) ) / fnj(row)
      endfor
   endfor
;--Create new residual image
   residual = aim - avgim
   for row=0,255 do begin
      rowsum = TOTAL(residual(*,row))
      rowavg = rowsum/256.0
      residual(*,row) = residual(*,row) - rowavg
   endfor
;--Weave removal
     aim = XV_HEC4_Weave_Removal( aim, residual,   0,   2 )
     aim = XV_HEC4_Weave_Removal( aim, residual,   3,   3 )
     aim = XV_HEC4_Weave_Removal( aim, residual,   4,   6 )
     aim = XV_HEC4_Weave_Removal( aim, residual,   7,   7 )
     aim = XV_HEC4_Weave_Removal( aim, residual,   8,  10 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  11,  11 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  12,  14 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  15,  17 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  18,  18 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  19,  21 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  22,  22 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  23,  25 )
     aim = XV_HEC4_Weave_Removal( aim, residual,  26, 229 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 230, 232 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 233, 233 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 234, 236 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 237, 237 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 238, 240 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 241, 243 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 244, 244 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 245, 247 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 248, 248 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 249, 251 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 252, 252 )
     aim = XV_HEC4_Weave_Removal( aim, residual, 253, 255 )
;--End of weave removal

;--Determine the image bias and correct for it
   ebias = TOTAL(aim(4:7,0:3))
   ebias = ebias + TOTAL(aim(4:7,252:255))
   ebias = ebias + TOTAL(aim(252:255,0:3))
   ebias = ebias + TOTAL(aim(252:255,252:255))
   ebias = ebias/64.0
   im = ROUND(aim - ebias + 10.0)
;---Repack image counts
;   Image = XV_PACK(im)
    Image = im

End
;----------------------------------------------------------
; PURPOSE: Corrects Earth Camera image for post July 2007 problems.
;  
; CALLING SEQUENCE:  XV_HORIZONTAL_SMOOTH_EC_6, Image
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Alters the input image.
;
; COMMON BLOCKS:
;  XV_RECORD_DATA
;
; MODIFICATION HISTORY:
;  Written by Rae Dvorsky, March 2014, based on Fortran code by J.B. Sigwarth
;
;----------------------------------------------------------
PRO XV_Horizontal_Smooth_EC_6, image
;   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Curr_Limit, sensor, Record2
     psf = [0.1090933,0.1277874,0.1315262, $
            0.1200427,0.1351315,0.1395380,0.0937375]
     aim = fltarr(256,256)
     aim2 = aim
     aim3 = aim

;  unpack image counts
;    aim = FLOAT(XV_UNPACK(Image))
     aim = FLOAT(Image)
     aim2 = aim

;  Apply the point spread function to aim2 to get aim3
;     (the simulated image)

   for iter=1,10 do begin

     aim3(0:5,*) = aim2(0:5,*)
     aim3(254:255,*) = aim2(254:255,*)
     for col=6,253 do begin
        for row=0,255 do begin
           aim3(col,row) = 0.0
           for ip=0,6 do begin
              aim3(col,row) = aim3(col,row) + psf(ip)*aim2(col-(ip-1),row) 
           endfor
        endfor
     endfor
;  Get the residual image
     residual = aim - aim3

;  Apply the point spread function to residual to improve aim2
     aim2(0:1,*) = aim(0:1,*)
     aim2(250:255,*) = aim(250:255,*)
     for col=2,249 do begin
        for row=0,255 do begin
           for ip=0,6 do begin
              aim2(col,row) = aim2(col,row) + $
		 (0.5)*psf(ip)*residual(col+(ip-1),row) 
           endfor
        endfor
     endfor

   endfor

;---Repack image counts
;  Image = XV_PACK(ROUND(aim2))
   Image  = ROUND (aim2)
 
End

;----------------------------------------------------------
; Procedure name: XV_SMOOTH
; PURPOSE:
;  Applies a smoothing filter to an image
;
; CALLING SEQUENCE:
;  im = DIST(256,256)
;  XV_SMOOTH,im
;
; INPUTS:
;  IMAGE == A 2D array of bytes
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Smooths the Image
;
; COMMON BLOCKS:
;  None
;
; MODIFICATION HISTORY:  Rae Dvorsky, Aug 2014   factors changed to match Fortran version
;----------------------------------------------------------
Pro XV_Smooth, Image
;   Image = XV_UNPACK(image)
   sz = SIZE(image)
   m = sz(1)
   n = sz(2)
   New_image = FLTARR(M,N)
   IM2 = INTARR(M+2,N+2)
   IM2(1:M,1:N) = Image
   IM2(0,1:N) = IM2(1,1:N)
   IM2(M+1,1:N) = IM2(M,1:N)
   IM2(0:M+1,0) = IM2(0:M+1,1)
   IM2(0:M+1,N+1) = IM2(0:M+1,N)
;   New_image = .4 * Image                         $
;    + .1 * (IM2(0:M-1,1:N)+IM2(2:M+1,1:N)+IM2(1:M,0:N-1)+IM2(1:M,2:N+1)) $
;    +.05*(IM2(0:M-1,0:N-1)+IM2(2:M+1,0:N-1)+IM2(0:M-1,2:N+1)+IM2(2:M+1,2:N+1))
   New_image = .5 * Image                         $<CR>
     +.1124 * (IM2(0:M-1,1:N)+IM2(2:M+1,1:N)+IM2(1:M,0:N-1)+IM2(1:M,2:N+1)) $
     +.0126*(IM2(0:M-1,0:N-1)+IM2(2:M+1,0:N-1)+IM2(0:M-1,2:N+1) $
	+IM2(2:M+1,2:N+1))

;   Image =  XV_PACK(round(New_image))
    Image = ROUND (New_image)
End

;==============================================================
;
; sets color table limits based on sensor parameters and date  
;
; Rae Dvorsky, July 2015                                       
;
;==============================================================
;   

FUNCTION XV_CLIMITS, sensor

   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField, MJD 

IF (sensor eq 0) THEN BEGIN      
;    IF (MJD gt 3429 and Flags.ImType eq 0) THEN BEGIN
     IF (MJD gt 3429) THEN BEGIN
        Low = 130         
        High = 210      
    ENDIF ELSE BEGIN         
        Low = Bias - 3         
        High = 85.0/Ratio + Low      
    ENDELSE  

ENDIF ELSE BEGIN      
    Low = Bias - 3      
    High = 100.0/Ratio + Low   
ENDELSE   

RETURN, [Low,High]

END

;----------------------------------------------------------
; PURPOSE:
;  Process all records in the CDF
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_PROCESS_RECORDS, DISPLAY = display, TSTART = tstart, TSTOP = tend, E0 = e0, E1 = e1, LUN = lun

   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField, MJD

   ; Set up flags to determine what processing we are doing on the file.

   fl_sub_cosmic_ray = 0
   fl_sub_slopes     = 0
   fl_remove_weave   = 0
   fl_flat_field     = 0
   fl_dayglow_sub    = 0
   fl_nightglow_min  = 0
   fl_h_smooth_ec4   = 0
   fl_h_smooth_ec6   = 0
   fl_smooth_filter  = 0

   IF sensor eq 0 THEN PRINTF, lun, "Processing Data for Earth Camera."
   IF sensor eq 1 THEN PRINTF, lun, "Processing Data for Low Resolution Camera."

   ; Set flags for procssing Earth Camera data.

   IF sensor eq 0 THEN BEGIN

      fl_sub_cosmic_ray =  1
      fl_sub_slopes     =  1
      fl_remove_weave   =  1
      fl_flat_field     =  1
      fl_dayglow_sub    =  1
      fl_nightglow_min  =  1

      year = Record.Time_pb5 [0]
      doy  = Record.Time_pb5 [1]
      mjd  = XV_YEAR_DOY_TO_MJD (year, doy)

      IF (mjd gt 3429) THEN fl_h_smooth_ec4 = 1
      IF (mjd gt 4307) THEN fl_h_smooth_ec6 = 1

   ENDIF

   IF sensor eq 1 THEN BEGIN

      fl_sub_cosmic_ray = 1
      fl_sub_slopes     = 1
      fl_flat_field     = 1
      fl_smooth_filter  = 1 

   ENDIF 

   ; Get the intensity table.  Might want to move this up further on the food
   ; chain in order to save a bit of time.
   intens_table = XV_GET_INTENS_TABLE (fid, sensor)

   ; Decide if we are supposed to process on time boxed subset of the records in the 
   ; CDF.  If the parameters E0 and E1 are passed, then we will only process records
   ; between those two epochs.
   IF KEYWORD_SET (e0) && KEYWORD_SET (e1) THEN interval = 1 ELSE interval = 0

   ; Write appropiate log messages about what we are doing

   IF (fl_h_smooth_ec4) THEN BEGIN

       PRINTF, lun, "Data file processing to include: Horizontal Smooth EC4."

   ENDIF

   IF (fl_h_smooth_ec6) THEN BEGIN

       PRINTF, lun, "Data file processing to include: Horizontal Smooth EC6."

   ENDIF

   IF (fl_sub_cosmic_ray) THEN BEGIN

       PRINTF, lun, "Data file processing to include: Cosmic Ray Subtraction."

   ENDIF

   IF (fl_sub_slopes)     THEN BEGIN

       PRINTF, lun, "Data file processing to include: Slope Subtraction."

   ENDIF

   IF (fl_remove_weave)   THEN BEGIN

       PRINTF, lun, "Data file processing to include: Remove Weave."

   ENDIF

   IF (fl_flat_field)     THEN BEGIN

       PRINTF, lun, "Data file processing to include: Flat Field."

   ENDIF

   IF (fl_dayglow_sub)    THEN BEGIN

       PRINTF, lun, "Data file processing to include: Dayglow Subtraction."

   ENDIF

   IF (fl_nightglow_min)  THEN BEGIN

       PRINTF, lun, "Data file processing to include: Nightglow Minimum."

   ENDIF

   IF (fl_smooth_filter)  THEN BEGIN

       PRINTF, lun, "Data file processing to include: Smooth Filter."

   ENDIF

   PRINTF, lun, "Lower Limit: ", Limits [0]
   PRINTF, lun, "Upper Limit: ", Limits [1]

   r_names = TAG_NAMES (record)

   n_rec_tags = N_TAGS (record)

   first = 1
   rec_count = 0
   last_epoch = 0.D

   e = intarr (n_rec_tags)

   FOR i = 0, n_rec_tags - 1 DO e [i] = N_ELEMENTS (record.(i))

   FOR imagenum = 0, maxrecs - 1 DO BEGIN

       XV_READ_RECORD

       IF interval eq 1 THEN BEGIN

          IF CDF_EPOCH_COMPARE (record.epoch, e0) eq -1 THEN CONTINUE

          IF CDF_EPOCH_COMPARE (record.epoch, e1) eq 1  THEN CONTINUE         

       ENDIF

       IF first eq 1 THEN BEGIN

           tstart = CDF_ENCODE_EPOCH (record.epoch, EPOCH = 3)

           b = (BYTE (tstart)) [0:18]

           ; Replace the 'T' between the day and time fields with a space
           b [10]     = 32

           PRINTF, lun, "Data Start:  ", STRING (b)

           ; Replace the '-' with '/' (works with read_MyCDF)'
           b [[4, 7]] = 47

           tstart = STRING (b)

           first = 0

       ENDIF

       image_buffer.image_counts_raw [*, *, rec_count] = xv_get_image (fid, imagenum, 1) 

       XV_COMPUTE_CRDS, LUN=lun
  
       FOR i = 0, n_rec_tags - 1 DO BEGIN

           pos = WHERE (data_names eq r_names (i), cnt)

           IF cnt eq 0 THEN BEGIN

              ; MESSAGE, 'Variable: ' + STRING (r_names (i)) + ' not found in data buffer.'

           ENDIF 

           data_buffer.(pos) [*, rec_count] = REFORM (record.(i), e [i]) 

       ENDFOR

       IF KEYWORD_SET (display) THEN TV, image, 255, 0

       ; Do all required image processing for the data for this record

       Image = XV_UNPACK (Image)

       IF (fl_h_smooth_ec4)   THEN XV_Horizontal_Smooth_EC_4, image
       IF (fl_h_smooth_ec6)   THEN XV_Horizontal_Smooth_EC_6, image
       IF (fl_sub_cosmic_ray) THEN XV_SUB_COSMIC_RAY, image, [-1], 7, threshold
       IF (fl_sub_slopes)     THEN XV_SUB_SLOPES, image, left, right, top, bottom
       IF (fl_remove_weave)   THEN XV_REMOVE_WEAVE, image
       IF (fl_flat_field)     THEN XV_FLAT_FIELD, image, flatfield, bias, sensor, Record.Time_pb5
       IF (fl_dayglow_sub)    THEN XV_DAYGLOW_SUBTRACT, image, record, lookvector
       IF (fl_nightglow_min)  THEN XV_NIGHTGLOW_MINIMUM, image, record, lookvector
       IF (fl_smooth_filter)  THEN XV_SMOOTH, image

       Image =  XV_PACK (Image)

       IF KEYWORD_SET (display) THEN TV, image, 0, 0

       image_buffer.image_counts_clean [*, *, rec_count] = image
       image_buffer.iimage_counts      [*, *, rec_count] = XV_CALC_INTENSITY (intens_table, image)

       wr_buffer.geo_lat       [*, *, rec_count] = glats
       wr_buffer.geo_lon       [*, *, rec_count] = glons
       wr_buffer.rotatn_matrix [*, *, rec_count] = XV_MAKE_ROTATION_MATRIX ()
       wr_buffer.Limit_Lo      [rec_count] = Limits [0]
       wr_buffer.Limit_Hi      [rec_count] = Limits [1]
       wr_buffer.altls         [*, *, rec_count] = ALTLS
       wr_buffer.on_earth      [*, *, rec_count] = SZAs ne -1

       dec  = ASIN (REFORM (LookV_GCI [2, *, *])) * !RADEG
       ra   = ATAN (REFORM (LookV_GCI [1, *, *]), REFORM (LookV_GCI [0, *, *])) * !RADEG
       
       neg  = WHERE (ra LT 0, neg_cnt)

       IF  neg_cnt GT 0 THEN ra [neg] = ra [neg] + 360.0

       wr_buffer.ra            [*, *, rec_count] = ra
       wr_buffer.dec           [*, *, rec_count] = dec

       rec_count = rec_count + 1

       last_epoch = record.epoch

   ENDFOR
   
   tend = CDF_ENCODE_EPOCH (last_epoch, EPOCH = 3)

   b = (BYTE (tend)) [0:18]

   ; Replace the 'T' between the day and time fields with a space
   b [10]     = 32

   PRINTF, lun, "Data End:    ", STRING (b)

   ; Replace the '-' with '/' (works with read_MyCDF)'
   b [[4, 7]] = 47

   tend = STRING (b)

   PRINTF, lun, FORMAT = '("Processed: ", I0, " records.")', rec_count

   RETURN, rec_count

END

;----------------------------------------------------------
; PURPOSE:
;  Process all records in the CDF
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_SAVE_CDF, cdf_out, nrec, LUN=lun

   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor

   skl_v_names = TAG_NAMES (skeleton_buf)

   ; Reformat the dimensions all of the arrays in the data_buffer, so that they
   ; will match the equivalent to variables that we copying them to in the CDF. 
   FOR i = 0, N_TAGS (data_buffer) - 1 DO BEGIN

       ; The RECORD tag is not part of the CDF.  Skip this tag.
       IF  data_names [i] eq 'RECORD' THEN CONTINUE

       ; Find the index in the skeleton buffer of the ith tag in the data_buffer. 
       indx = WHERE (skl_v_names eq data_names [i], cnt)

       ; Check for a member of data buffer that is not pressent in the Master CDF
       IF  cnt eq 0 THEN BEGIN 

           MESSAGE, "Data variable: " + data_names [i] + " not found in Master CDF." 
 
       ENDIF

       d = data_dim.(i)

       d [-1] = nrec 

       x = REFORM (data_buffer.(i) [*, 0:nrec-1], d)

       *skeleton_buf.(indx [0]).data = x

   ENDFOR

   t = {}

   ; Do the image buffers

   *skeleton_buf.image_counts_raw.data   = image_buffer.image_counts_raw   [*, *, 0:nrec-1]
   *skeleton_buf.image_counts_clean.data = image_buffer.image_counts_clean [*, *, 0:nrec-1]
   *skeleton_buf.iimage_counts.data      = image_buffer.iimage_counts      [*, *, 0:nrec-1]

   ; Do all the variables that we rewriting from recalculated data.

   *skeleton_buf.geo_lat.data       = wr_buffer.geo_lat [*, *, 0:nrec-1]
   *skeleton_buf.geo_lon.data       = wr_buffer.geo_lon [*, *, 0:nrec-1]
   *skeleton_buf.rotatn_matrix.data = wr_buffer.rotatn_matrix [*, *, 0:nrec-1]
   *skeleton_buf.limit_lo.data      = wr_buffer.limit_lo [0:nrec-1]   
   *skeleton_buf.limit_hi.data      = wr_buffer.limit_hi [0:nrec-1]
   *skeleton_buf.altls.data         = wr_buffer.altls [*, *, 0:nrec-1]
   *skeleton_buf.on_earth.data      = wr_buffer.on_earth [*, *, 0:nrec-1]
   *skeleton_buf.ra.data            = wr_buffer.ra [*, *, 0:nrec-1]
   *skeleton_buf.dec.data           = wr_buffer.dec [*, *, 0:nrec-1] 

   IF sensor eq 0 THEN BEGIN

      *skeleton_buf.look_dir_vctr_ec.data = LookVector

   ENDIF ELSE BEGIN

      *skeleton_buf.look_dir_vctr.data = LookVector

   ENDELSE

   ; Remove NULL pointers from skeleton_buf
   FOR i = 0, N_TAGS (skeleton_buf) - 1 DO BEGIN

      IF  *skeleton_buf.(i).data NE !NULL THEN BEGIN

          t = CREATE_STRUCT (t, skeleton_buf.(i).varname, skeleton_buf.(i))

      ENDIF 

   ENDFOR 

   status = write_data_to_cdf (cdf_out, t)
           
   PRINTF, lun, "Created CDF: ", cdf_out

   RETURN

END

;----------------------------------------------------------
; PURPOSE:
;  Deletes a CDF that did not have any records written to it.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_DELETE_EMPTY_CDF, cdf_out

   FILE_DELETE, cdf_out, /ALLOW_NONEXISTENT

END

;----------------------------------------------------------
; PURPOSE:
;  Opens a VIS CDF file.
;
; INPUTS:
;  An event structure.
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  Opens the CDF file.  Reads in the first image.  Loads
;  the color table.  Load the record.  Displays the loaded
;  image.  Sets certain appropriate flags.
;
; COMMON BLOCKS:
;  XV_DERIVED_DATA
;  XV_FLAGS
;  XV_RECORD_DATA
;  XV_FILE_DATA
;  COLORS
;
; PROCEDURE:
;
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_OPEN, cdf_file, fc, LUN=lun, DISPLAY = display
;   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
   COMMON XV_FLAGS, Flags
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistort
;   COMMON colors,rr,gg,bb,rc,gc,bc
   COMMON XV_IMPROPARAMS, Ratio, Bias, Left, Right, Top, Bottom, Threshold, FlatField, MJD

   ;CATCH, error
   ;IF error NE 0 THEN BEGIN
   IF 0 THEN BEGIN
      CATCH, /CANCEL

      msg = [!ERR_STRING]

      ; Check for user error.  If it is not a user error, then assume it was
      ; Generated opening a file.
      IF NOT (strmessage (error, /NAME) eq 'IDL_M_USER_ERR') THEN BEGIN

         msg =  ['Error while opening file.', msg]

      ENDIF

      PRINTF, lun, msg, FORMAT = '(A)'

      MESSAGE, /REISSUE_LAST
   END

   IF(cdf_file NE "") THEN BEGIN

      filename = cdf_file

      PRINTF, lun, 'PROCESSING FILE: ', filename

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; set XV_FILE_DATA variables
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      Flags.LOADED = 0
      Fid = CDF_OPEN(Filename)

      MaxRecs = XV_GET_NUM_RECORDS(Fid)
      sensor =  XV_GET_SENSOR (fid)

      ; Check if the CDF seemes to be ligit.  If it is not then we will close and
      ; just exit.
      valid = (sensor eq 0 || sensor eq 1) && MaxRecs > 0

      IF  ~ (valid) THEN BEGIN

          CDF_CLOSE, Fid

          PRINTF, lun, 'COULD NOT PROCESS: ', filename

          RETURN, 0

      ENDIF

      FLAGS.LOADED = 1
      ImageNum=0

      Header =  XV_GET_HEADER(Fid)

      Flags.IPP = 0

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; set XV_RECORD_DATA variables
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      XV_LOAD_COLOR_TABLE, Fid, DISPLAY=display
      XV_READ_RECORD
      LastImage = Image

; set up the compression tables for packing and unpacking data.

      XV_INIT_COMPRESSION_TABLES


; get image processing parameters
      XV_Get_All_Improparams
      XV_Improparams

;  get look vectors
      LookVector =  XV_GET_LOOK_VECTOR(Fid)

; reset colors using XV_CLimits function
; (this is done here because XV_CLimits needs some Improparams)
      Limits = [0,0]
      Limits = XV_CLimits (sensor)

      IF fc eq 0 THEN XV_SCALE_COLOR_TABLE, Limits [0], Limits [1], DISPLAY=display

      RETURN, MaxRecs

   ENDIF ELSE BEGIN

      MESSAGE, "No File was specified."

      RETURN, 0

   ENDELSE


END

;----------------------------------------------------------
; PURPOSE:
;  Open and read a Polar XVIS master CDF.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_READ_MASTER, master_cdf, cdf_out

   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer

   IF NOT KEYWORD_SET (master_cdf) THEN master_cdf = d_master_e

   skeleton_buf = read_master_cdf (master_cdf, cdf_out)

   ; Make sure that group write is set.
   FILE_CHMOD, cdf_out, /G_WRITE

   ; Set group to cdaweb
   SPAWN, 'chgrp cdaweb ' + cdf_out

   RETURN

END
;----------------------------------------------------------
; PURPOSE:
;  Initializes the XVIS application.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;  XV_DERIVED_DATA
;  XV_FLAGS
;  XV_RECORD_DATA
;  XV_WIDS
;  XV_FILE_DATA
;
; MODIFICATION HISTORY:
;       18-AUG-1999 RLD Had to add a lone "WDelete, 0" at the end of this
;                       function because the font calls were apparently
;                       generating an open window call...
;                       If the font calls are removed, take the delete too
;
;----------------------------------------------------------
PRO XV_INITIALIZE_XVIS
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
   COMMON XV_FLAGS, Flags
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor
   COMMON COLORS, rr,gg,bb,rc,gc,bc
;   COMMON XV_CURSOR, Xsc, Ysc, Xim, Yim, Xcd, Ycd, Xlb, Ylb

   IF(N_ELEMENTS(FULL_COLOR) EQ 0) THEN FULL_COLOR = 0

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Initialize some COMMON block variables
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   LastImage = BYTARR(256,256)
   ImageNum = 0
   ROI = [-1]
   Handlers = LONARR(10)
   HCount = 0
   ; Xsc = 0
   ; Ysc = 0
   Filename = ''
   MaxRecs = 1
;   ViewWid = 0L
;   DrawWid = 0L

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; Only way I can figure how to get the current path
   ;;; change to Login directory and change back
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   cd,"",current=path
;   CD,PATH

   ; Replaces the above two lines.
   ; SPAWN, 'pwd', path, /NOSHELL

   FLAGS = { loaded:0,$
             IPP:0,$
;             LV:0,$
;             ALT:0,$
;             ALTLS:0,$
;             PHI:0,$
;             SZA:0,$
;             LOC:0,$
;             GLAT:0,$
;             GLON:0,$
;  	      CRS:0,$
;             XPAND:0,$
;             CDF_COLOR:1,$
             DIST:0}

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Initialize the radius function.  Table lookup is for speed.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   XV_INITIALIZE_EARTH_RADIUS

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Initialize the color table
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   DEVICE, DECOMPOSED = 0

   ; tvlct,rr,gg,bb,/GET
   ; rc = rr
   ; gc = gg
   ; bc = bb
   ; tsize = n_elements(rr)

END

;----------------------------------------------------------
; NAME:      XV_REMOVE_VV
;
; PURPOSE:
;  Since we are getting the basic CDF structure from a master, we need
;  to make sure we don't include any Virtual Variables in the CDF we 
;  are creating.
;
;  This function removes any Virtual Variables that exist in the output
;  CDF which should have already been created by a previous call to 
;  read_master_cdf.
;
;  In addition it simultaneously skelelton_buf structure in order remove
;  any referances to virtual variables from this structure. 
;
; INPUTS:
;  CDF:	Name of output CDF>
;
; KEYWORD PARAMETERS:
;  None.
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  XV_CDF_BUF
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------

PRO XV_REMOVE_VV, cdf, LUN=lun

   COMMON XV_CDF_BUF, skeleton_buf, data_buffer, data_names, data_dim, image_buffer, wr_buffer

   new_struct = {}

   id = CDF_OPEN (cdf)

   ; Check if there are any virtual variables in the CDF.
   r = CDF_INQUIRE (id)

   n_attrs = r.NATTS

   tags = TAG_NAMES (skeleton_buf)

   pos = 0

   FOR i = 0, N_TAGS (skeleton_buf) - 1 DO BEGIN

      v = CDF_VARINQ (id, pos, /ZVARIABLE)

      is_virtual = 0

      FOR atr = 0, n_attrs - 1 DO BEGIN

          CDF_ATTGET_ENTRY, id, atr, v.name, type, val, status, ATTRIBUTE_NAME=aname

          IF STRUPCASE (aname) ne 'VIRTUAL' THEN CONTINUE

          IF  STRUPCASE (val) eq 'TRUE' THEN BEGIN

              is_virtual = 1

              BREAK

          ENDIF
   
      ENDFOR

      IF  is_virtual THEN BEGIN   

          ; PRINTF, lun, "Deleting ", v.name, " as variable is of type 'virtual'."

          CDF_VARDELETE, id, pos, /ZVARIABLE

      ENDIF ELSE BEGIN

          n = WHERE (tags eq STRUPCASE (v.name))

          new_struct = CREATE_STRUCT (new_struct, tags [n], skeleton_buf.(n))

          pos = pos + 1

      ENDELSE      

   ENDFOR
 
   skeleton_buf = new_struct

   PRINTF, lun, FORMAT = '("Creating CDF with ", I0, " variables.")', N_TAGS (skeleton_buf)

   CDF_CLOSE, id

   RETURN

END

;----------------------------------------------------------
; NAME:      XV_SET_COMPRESSION
;
; PURPOSE:
;  Set the compression method for every variable except Epoch.  Epoch
;  will always be uncompressed.
;
;
;  In addition, set each variable that is of type 'VARY' so that initial
;  number of records blocked out for that variable will be exactly the 
;  number of records in the CDF.
;
; INPUTS:
;  CDF:	Name of output CDF
;  REC: Number of records in the CDF
;
; KEYWORD PARAMETERS:
;  None.
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------

PRO XV_SET_COMPRESSION, cdf

   id = CDF_OPEN (cdf)

   cinfo = CDF_INQUIRE (id)

   FOR i = 0, cinfo.NZVARS - 1 DO BEGIN

       vinfo = CDF_VARINQ (id, i, /ZVARIABLE)

       IF  STRUPCASE (vinfo.RECVAR) eq 'NOVARY' THEN CONTINUE

       IF  STRUPCASE (vinfo.DATATYPE) eq 'CDF_EPOCH' THEN CONTINUE

       CDF_COMPRESSION, id, VARIABLE=i, SET_VAR_GZIP_LEVEL=9, SET_VAR_COMPRESSION=5, /ZVARIABLE

   ENDFOR

   CDF_CLOSE, id

   RETURN

END

;----------------------------------------------------------
; NAME:      XV_SET_BLOCKING
;
; PURPOSE:
;  Set the blocking factor for each variable of type 'VARY' 
;
; INPUTS:
;  CDF:	Name of output CDF
;  REC: Number of records in the CDF
;
; KEYWORD PARAMETERS:
;  None.
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------

PRO XV_SET_BLOCKING, cdf, rec

   id = CDF_OPEN (cdf)

   cinfo = CDF_INQUIRE (id)

   FOR i = 0, cinfo.NZVARS - 1 DO BEGIN

       vinfo = CDF_VARINQ (id, i, /ZVARIABLE)

       IF  STRUPCASE (vinfo.RECVAR) eq 'NOVARY' THEN CONTINUE

       CDF_CONTROL, id, VARIABLE=i, SET_EXTENDRECS=rec, /ZVARIABLE

   ENDFOR

   CDF_CLOSE, id

   RETURN

END

;----------------------------------------------------------
; NAME:      XV_CHECK_OVERWRITE
;
; PURPOSE:
;  Check if the processed CDF file would overwrite an already existing
;  file.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_CHECK_OVERWRITE, cdf_out, ovr

   ; Check if the output file already exists.
   IF  FILE_TEST (cdf_out) THEN BEGIN

       ; Check if the overwrite flag is set.  If it isn't, then we will
       ; we throw an error.
       IF NOT KEYWORD_SET (ovr) THEN BEGIN

          MESSAGE, "File: " + STRING (cdf_out) + " already exists " + $
                   "and overwrite flag not set."

       ENDIF

   ENDIF

   RETURN

END

;----------------------------------------------------------
; NAME:      XV_GET_TIME_INTERVAL
;
; PURPOSE:
;  Get the start time and stop time of the data in the CDF.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_GET_TIME_INTERVAL

   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor

   CDF_CONTROL, fid, variable='Epoch', GET_VAR_INFO=vinfo, /ZVARIABLE

   CDF_VARGET, fid, 'Epoch', epochs, REC_COUNT=vinfo.MAXREC + 1

   epoch0 = MIN (REFORM (epochs))
   epoch1 = MAX (REFORM (epochs))

   RETURN, [epoch0, epoch1]
   
END

;----------------------------------------------------------
; NAME:      XV_CDF_TO_EPOCH

; PURPOSE:
;  Convert the date/time embedded in a CDF name to an Epoch.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_CDF_TO_EPOCH, cdf

  f = FILE_BASENAME (cdf)
  s = STRSPLIT (f, '_', /EXTRACT)

  t = ""

  FOR i = 0, N_ELEMENTS (s) - 1 DO BEGIN
     
      t = s [i]

      IF STREGEX (t, '[0-9]+') eq 0 THEN BREAK 

  ENDFOR

  year  = STRMID (t, 0, 4)
  month = STRMID (t, 4, 2)
  day   = STRMID (t, 6, 2)

  CDF_EPOCH, e, year, month, day, /COMPUTE_EPOCH

  RETURN, e
END

;----------------------------------------------------------
; NAME:      XV_NEAREST_HOUR
;
; PURPOSE:
;  Move the beginning and end of the time interval to hour boundaries.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_NEAREST_HOUR, time_interval 

   ; Start of interval
   CDF_EPOCH, time_interval [0], yr, month, day, hr, min, sec, /BREAKDOWN

   daysec = hr * 3600L + min * 60L + sec
   
   new_hour = FLOOR (daysec / 3600.0)

   CDF_EPOCH, t0, yr, month, day, new_hour, 0, 0, /COMPUTE_EPOCH
   
   ; End of interval
   CDF_EPOCH, time_interval [1], yr, month, day, hr, min, sec, /BREAKDOWN

   daysec = hr * 3600L + min * 60L + sec
   
   new_hour = CEIL (daysec / 3600.0)

   CDF_EPOCH, t1, yr, month, day, new_hour, 0, 0, /COMPUTE_EPOCH

   RETURN, [t0, t1]

END

;----------------------------------------------------------
; NAME:      XV_FIND_TARGET_CDF
;
; PURPOSE:
;  Check if the processed CDF file would overwrite an already existing
;  file.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_FIND_TARGET_CDF, r_cdf, r_master, target, arch, af, hours

   ; NOTE. AF (ARCHIVE FORMAT) is not implemented.  It will be ignored.

   ; Need to do this to force call by value.
   cdf = r_cdf
   master = r_master 
   
   ;  Check if target is defined.  If it is, we use that for the name of 
   ;  target CDF to create.
   IF  KEYWORD_SET (target) THEN BEGIN

       cdf_out = (STRSPLIT (target, '.', /EXTRACT)) [0]

       ; If we are creating hour sub-files (the parameter hours is set and not
       ; zero) then add a two didget hour designatator onto the end of the date
       ; string.
       IF  KEYWORD_SET (hours) THEN BEGIN 

           CDF_EPOCH, hours, yr, month, day, hr, min, sec, /BREAKDOWN

           cdf_out = STRING (FORMAT  = '(A, I2.2)', cdf_out, hr)

       ENDIF

   ENDIF ELSE BEGIN 

       ; Extract the basename from the master.  This should just be name of the
       ; master file itself.
       master = FILE_BASENAME (master)

       ; Remove the .cdf file extension, if needed.
       master = (STRSPLIT (master, '.', /EXTRACT)) [0]
       cdf    = (STRSPLIT (cdf, '.', /EXTRACT)) [0]
   
       ; Get the position of the date string from the master.  The date string
       ; is eight digits that indicate Year/Month/Day.  For a master they should
       ; be all zero's
       datepos = stregex (master, '00000000')

       ; Check to make sure the master had a date string.  If it didn't, then when
       ; we create the name target CDF, we will stick it at the end.
       IF  datepos eq -1 THEN BEGIN
     
           datepos = STRLEN (master)
           master  = master + '00000000'

       ENDIF
       

       ; Get the date string position from the CDF we are reprocssing.  
       cdfdatepos = stregex (cdf, '[0-9]{8}', length = l)
  
       ; Check to make sure the CDF file name actually had a date string.
       IF  cdfdatepos eq -1 THEN BEGIN

           MESSAGE, "File name of CDF file being processed does not contain a " + $
                    "recognizable date string."

       ENDIF

       ; Extract the date.
       cdfdate = STRMID (cdf, cdfdatepos, l)

       ; Start out by settting the file name to same name as the master.
       fn = master

       ; If we are creating hour sub-files (the parameter hours is set and not
       ; zero) then add a two didget hour designatator onto the end of the date
       ; string.
       IF  KEYWORD_SET (hours) THEN BEGIN 

           CDF_EPOCH, hours, yr, month, day, hr, min, sec, /BREAKDOWN

           fn = STRING (FORMAT  = '(A, I2.2, A)',                             $
                        STRMID (fn, 0, datepos + 8),                          $
                        hr,                                                   $
                        STRMID (fn, datepos + 8))

       ENDIF

       ; Now replace the '00000000' in file with the date string from the CDF
       STRPUT, fn, cdfdate, datepos
   
       cdf_out = FILE_EXPAND_PATH (arch) + '/' + fn
 
   ENDELSE

   cdf_out = cdf_out + ".cdf"

   RETURN, cdf_out 

END


;----------------------------------------------------------
; NAME:      XV_FIND_MASTER
;
; PURPOSE:
;  Check if the processed CDF file would overwrite an already existing
;  file.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_FIND_MASTER, sensor, master

   COMMON XV_DEFAULTS, d_master_e, d_master_l, b_ext, var_names, capture, gc, cdf_dtype

   IF  NOT KEYWORD_SET (master) THEN BEGIN 

       IF sensor eq 1 THEN master = d_master_l ELSE master = d_master_e

   ENDIF

   RETURN, master

END


;----------------------------------------------------------
; NAME:      XV_GET_CDF_LIST
;
; PURPOSE:
;  Check if the processed CDF file would overwrite an already existing
;  file.
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  None
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
FUNCTION XV_GET_CDF_LIST, source

   ; Check if source specifies a file or a directory.
   IF  FILE_TEST (source, /DIRECTORY) THEN BEGIN

       file_path = FILE_EXPAND_PATH (source)

       file_path = file_path + '/*.cdf'

       out = FILE_SEARCH (file_path, /TEST_REGULAR)

       ; Check to make sure we found at least one file.
       IF  out [0] eq '' THEN BEGIN

           MESSAGE, "Directory " + source + " is empty."

       ENDIF

   ENDIF ELSE out = [source]

   RETURN, out

END


;----------------------------------------------------------
; NAME:      XV_CHECK_INPUT
;
; PURPOSE:
;  Check the input parameters that program is called with.
;
; DISCUSSION:
;  Validates input parameters.   For input parameters that include
;  paths and/or filenames, this checks that the paths are correctly
;  specified.  Also checks that the source file exists and that the
;  target file will not clobber an existing file.
;
; INPUTS:
;  source	Either the name of the CDF to process or the directory
;		containing the CDFs to process.
;
;  target	Name of the new CDF.  By default it will be given a name
;		based the source file.  Not valid if source specifies a
;		directory.
;
;  arch		Archive to store processed CDF's in.  Not valid when 
;		specified with target.
;
;  af		Archive Format.  Currently only the following archive formats
;		have been implemented.
;
;			1	Flat 	Store all files in the arch directory
;
;  master	The name (and optionally path) of the master file to use
;		in place of the default master file.
;
;  log		Path name of directory to write Log files to.  Must exist if
;		specified.
;
;  hours        Number of hours to include in each CDF output file.  
;
; KEYWORD PARAMETERS:
;  None
;
; OUTPUTS:
;  None
;
; SIDE EFFECTS:
;  None
;
; COMMON BLOCKS:
;  XV_DEFAULTS
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;
;----------------------------------------------------------
PRO XV_CHECK_INPUT, source, target, arch, af, master, log, fc, hours

   COMMON XV_DEFAULTS, d_master_e, d_master_l, b_ext, var_names, capture, gc, cdf_dtype

   CATCH, error
   IF error NE 0 THEN BEGIN
      CATCH, /CANCEL

      msg = [!ERR_STRING]

      ; Check for user error.  If it is not a user error, then assume it was
      ; Generated opening a file.
      IF NOT (strmessage (error, /NAME) eq 'IDL_M_USER_ERR') THEN BEGIN

         msg =  ['Error while copying file.', msg]

      ENDIF

      PRINT, msg, FORMAT = '(A)'

      MESSAGE, /REISSUE_LAST
   END

   ; Make sure that we passed either the name of a CDF or a directory to 
   ; to process.
   IF  NOT KEYWORD_SET (source) THEN BEGIN

      MESSAGE, "Procedure called without specifing a CDF or directory " + $
               "to process."

      RETURN

   ENDIF

   ; Make sure that source CDF or path actually exists.
   IF  FILE_TEST (source) eq 0 THEN BEGIN

      MESSAGE, "File or directory " + source + " not found."

      RETURN

   ENDIF 

   ; Check if both target and arch are defined.  It is iillegal to specify
   ; both the name of a target CDF and archive to store it in.
   IF  KEYWORD_SET (arch) && KEYWORD_SET (target) THEN BEGIN

      MESSAGE, "Both an archive and target CDF were specified. "  + $
               "Please choose only one."

      RETURN

   ENDIF 

   ; If an archive was specified, then check to make sure it is a directory
   ; and that it was writable.
   IF KEYWORD_SET (arch) && FILE_TEST (arch, /DIRECTORY, /WRITE) eq 0 THEN BEGIN

      MESSAGE, "Archive directory " + arch + " does not exist or is not writable."

      RETURN

   ENDIF 

   ; If a master was specified, then check to make sure that it exists.
   IF KEYWORD_SET (master) && FILE_TEST (master) eq 0 THEN BEGIN

      MESSAGE, "Master CDF file " + master + " does not exist or is not readable."

      RETURN

   ENDIF 

   ; If a log file directory was specified, then make sure that it exists
   ; and that it is writable.
   IF KEYWORD_SET (log) && FILE_TEST (log, /DIRECTORY, /WRITE) eq 0 THEN BEGIN

      MESSAGE, "Log directory " + log + " does not exist or is not writable."

      RETURN

   ENDIF

   ; Force FC (Full Color) to be set to 1 or 0
   IF KEYWORD_SET (fc) THEN fc = 1 ELSE fc = 0

   ; Force hours (maximum number of hours to include in each CDF file) to be 0
   ; if the keyword was not passed.
   IF N_ELEMENTS (hours) eq 0 THEN hours = 0

RETURN

END

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; NAME:
;       VIS-CAL
;
; PURPOSE:
;  This is the main procedure for processing Polar VIS data
;
; CALLING SEQUENCE:
;  VIS-CAL
;
; INPUTS:
;  None
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;  None
;
; COMMON BLOCKS:
;
; MODIFICATION HISTORY:
;       Written by:     Ron Yurow 04/15/2015
;       Based on code written for XVIS
;
;-------------------------------------------------------------
PRO VISCAL,   source,                    $
              TARGET = target,           $
              ARCHIVE = arch,            $
              AF = af,                   $
              TSTART = tstart,           $
              TSTOP = tstop,             $
              BEGIN_PROC = pbegin,       $
              END_PROC = pend,           $
              HOURS = hours,             $
              USE_MASTER = master,       $
              LOG = log,                 $
              OVERWRITE = ovr,           $
              DISPLAY = display,         $
              FC=fc,                     $
              CDF_LIST = cdf_list
          
 
   COMMON XV_DERIVED_DATA, LookV_GCI, ALTLS, Alts, Phis, SZAs, Locs, Glats, Glons
   COMMON XV_FLAGS, Flags
   COMMON XV_RECORD_DATA, Image, Record, ROI, LastImage, Limits, sensor, Record2
   COMMON XV_FILE_DATA, Filename, Fid, MaxRecs, ImageNum, LookVector, Header, UnDistor

   ; Main level error handling.  Print an error and die!
;   CATCH, Error
;   IF ERROR NE 0 THEN BEGIN
;      CATCH, /CANCEL

;      PRINT, !ERR_STRING

      ; PRINT, !ERR_STRING

;      MESSAGE, 'Errror(s) detected during processing.  Exiting.', /CONTINUE, /NOPRINT

;      PRINT, !ERR_STRING

;      RETURN
;   END

   LOG_FILE_NAME = 'vis_calibration.log'

   ; Check to make sure we were called with legit arguements.
   xv_check_input, source, target, arch, af, master, log, fc, hours

   ; Create a log file if the log keyword was set.
   IF  KEYWORD_SET (log) THEN BEGIN

       log_file = FILE_EXPAND_PATH (log) + "/" + LOG_FILE_NAME
 
       OPENW, lun, log_file, /APPEND, /GET_LUN, WIDTH=200

   ENDIF ELSE lun = -1

   PRINTF, lun, "Processing Started: ", SYSTIME (/UTC)
   PRINTF, lun   

   PRINTF, lun, "Source Dir/File:  ", source
   IF KEYWORD_SET (target) THEN PRINTF, lun, "Target File:      ", target
   IF KEYWORD_SET (arch)   THEN PRINTF, lun, "Archive:          " 
   IF KEYWORD_SET (pbegin) THEN PRINTF, lun, "Start Processing: ", pbegin
   IF KEYWORD_SET (pend)   THEN PRINTF, lun, "End Processing:   ", pend
   IF KEYWORD_SET (hours)  THEN PRINTF, lun, "Hours Per CDF:    ", hours
   IF KEYWORD_SET (master) THEN PRINTF, lun, "Use Master:       ", master
   IF KEYWORD_SET (log)    THEN PRINTF, lun, "Logging To:       ", log_file
   IF KEYWORD_SET (ovr)    THEN PRINTF, lun, "Overwrite CDFs:   ON" ELSE PRINTF, lun, "Overwrite CDFs:   OFF" 
   IF KEYWORD_SET (display)THEN PRINTF, lun, "Display:          ON" ELSE PRINTF, lun, "Display:          OFF"
   PRINTF, lun   

   ; Set up default values
   xv_set_defaults, LUN=lun

   ; Create a display window
   IF  KEYWORD_SET (display) THEN BEGIN

       WINDOW, 0, XSIZE=512, YSIZE=256

   ENDIF ELSE BEGIN

       PRINT, "No option to display data selected."
       PRINT, "If running within a SCREEN virtual terminal then please start IDL using:"
       PRINT, "idl -IDL_DEVICE null"
       PRINT, "In order to make sure IDL will not try to make any X server requests."

   ENDELSE 

   ; Check if we have start time for processing.  If we do, then convert it
   ; to Epoch.
   IF KEYWORD_SET (pbegin) THEN p0 = CDF_PARSE_EPOCH (pbegin)

   ; Get an array of CDFs to process.  This may only be a single CDF. 
   list = xv_get_cdf_list ( source )

   cdf_list = STRARR (1)

   tstart = !NULL 

   master_cdf = !NULL

   ; Process every CDF in the array.
   FOREACH cdf, list DO BEGIN 

       ; Check a start time for processing CDFs was specified.  If it was,
       ; make sure the date of the current CDF is after this time.
       IF KEYWORD_SET (pbegin) THEN BEGIN

          cdf_start_time = XV_CDF_TO_EPOCH (cdf) 

          IF CDF_EPOCH_COMPARE (cdf_start_time, p0) eq -1 THEN CONTINUE

       ENDIF

       ; Perform program initialization.
       xv_initialize_xvis

       ; Try to open the CDF
       t_cdf_recs = xv_open (cdf, fc, LUN=lun, DISPLAY=display)

       IF  t_cdf_recs eq 0 THEN CONTINUE

       proc_rec = 0

       ; Find the start and stop time of the CDF
       ti = xv_get_time_interval () 

       ; Move start and stop time to appropiate hour boundaries. (next day is hour 24)
       ti = xv_nearest_hour (ti) 

       ; Find the number of records in the CDF we are processing.

       ; Find the name of the master CDF to use.
       master_cdf = xv_find_master (sensor, master)

       t0 = ti [0]

       WHILE (t0 lt ti [1]) DO BEGIN

          IF hours eq 0 THEN BEGIN 

               t1 = ti [1] 

               ; Calculate the name of the CDF create
               cdf_out = xv_find_target_cdf ( cdf, master_cdf, target, arch, af, 0 )

          ENDIF ELSE BEGIN 

               t1 = (t0 + hours * 3600000.0D) < ti [1]

               ; Calculate the name of the CDF create
               cdf_out = xv_find_target_cdf ( cdf, master_cdf, target, arch, af, t0 )

          ENDELSE

          ; Check to make sure we are not overwriting an existing file
          xv_check_overwrite, cdf_out, ovr

          ; Try to open and read the master CDF.
          xv_read_master, master_cdf, cdf_out

          ; Remove any Virtual Variables
          xv_remove_vv, cdf_out, LUN=lun

          ; Make sure all appropiate variables are set to use the correct 
          ; compression method.
          xv_set_compression, cdf_out

          ; Setup a data buffer(s) to hold the data from the CDF.
          xv_setup_data_buffer

          mid = CDF_OPEN ( master_cdf )

          xv_setup_image_buffer, mid
          xv_setup_wr_buffer, mid
          xv_get_geo_fill, mid

          CDF_CLOSE, mid

          ; Process the records in the CDF betweem 
          rec = xv_process_records (DISPLAY = display, TSTART = tmp, TSTOP = tstop, E0 = t0, E1 = t1, LUN = lun)
          ; xv_process_all_records, DISPLAY = display, TSTART = t, TSTOP = tstop

          IF rec gt 0 THEN BEGIN

             ; Set the blocking factor to the number records processed for all RV
             ; Variables.
             xv_set_blocking, cdf_out, rec
          
             ; Save the data in the new CDF
             xv_save_cdf, cdf_out, rec, LUN=lun  

             cdf_list = [cdf_list, cdf_out]

             proc_rec+= rec 
 
          ENDIF ELSE BEGIN

             ; No records. Delete the empty CDF.
             xv_delete_empty_cdf, cdf_out

          ENDELSE

          ; Make sure we keep only the earliest start time
          ; IF tstart eq !NULL THEN tstart = t0
          IF tstart eq !NULL THEN tstart = tmp

          IF hours eq 0 THEN BREAK ELSE t0 = t1

       ENDWHILE  

       ; Close the source file.
       CDF_CLOSE, fid

       ; Check to make sure the number of records we processed is the same as the
       ; the number of records in the original CDF.
       IF  proc_rec ne t_cdf_recs THEN BEGIN

           PRINT, lun, "ERROR: ############################################"
           PRINT, lun, FORMAT = '("Source CDF had ", I0, " records.")', t_cdf_recs
           PRINT, lun, FORMAT = '("Only ", I0, " were actually processed.")', proc_rec

           stop

       ENDIF

       FLUSH, lun

        
   ENDFOREACH

   IF  master_cdf THEN cdf_list [0] = master_cdf

   PRINTF, lun, "Processing Ended:   ", SYSTIME (/UTC)

   PRINTF, lun, " ******* FINISHED PROCESSING RUN ******* "

   IF lun gt 0 THEN FREE_LUN, lun

RETURN

END 

