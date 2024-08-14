
; Assign a value based on how separated the colors are.  Currently this is
; based on the maximum separation between any two colors.  This value is used
; when trying to opptimize the color separation for a set of plots.
FUNCTION get_separation, in, pos

   gaps = [!NULL]

   sep = 0

   FOR i = 0, N_ELEMENTS (in) - 1 DO BEGIN

       IF  in [i] eq 0 THEN BEGIN 
   
           sep++ 

       ENDIF ELSE BEGIN 

           gaps = [gaps, sep]

           sep = 0

       ENDELSE 

   ENDFOR

   gaps = [gaps, sep]

   avg = variance (gaps)

   RETURN, avg

END

; Create an initial assignment colors to plots.  This will be optimized by the function
FUNCTION init_color_scale, colors, max_color, MMS1=mms1, MMS2=mms2, MMS3=mms3, MMS4=mms4

   w = intarr (max_color)

;  mms1 : black  (white if dark bkg) 0
;  mms2 : vermillion (red)  213
;  mms3 : bluish-green 158
;  mms4:  sky-blue 86
;

;  Adjusted MMS colors (Hue Optimized)
;  mms1 : black  (white if dark bkg) 0
;  mms2 : vermillion (red)  220
;  mms3 : bluish-green 115
;  mms4 : sky-blue 90

;  Final MMS colors (Hue Optimized)
;  mms1 : black  (white if dark bkg) 0
;  mms2 : vermillion (red)  220
;  mms3 : bluish-green 150
;  mms4 : sky-blue 90

;  color_space = [230, 205, 190, 177, 150, 115, 105, 95, 80, 55, 35, 10]


   ; Number of colors other than MMS colors (may be zero)
   IF N_ELEMENTS (colors) eq 0 THEN colors = 0

   ; Place MMS colors in appropiate spots
   ; IF KEYWORD_SET (mms1) THEN w [0]  = 2
   ; IF KEYWORD_SET (mms2) THEN w [10]  = 2
   ; IF KEYWORD_SET (mms3) THEN w [5]  = 2
   ; IF KEYWORD_SET (mms4) THEN w [4]  = 2


   IF KEYWORD_SET (mms1) THEN mms1 = 1 ELSE mms1 = 0
   IF KEYWORD_SET (mms2) THEN mms2 = 1 ELSE mms2 = 0
   IF KEYWORD_SET (mms3) THEN mms3 = 1 ELSE mms3 = 0
   IF KEYWORD_SET (mms4) THEN mms4 = 1 ELSE mms4 = 0

   IF (mms1) THEN w [11]  = 3
   IF (mms2) THEN w [0]   = 3
   IF (mms3) THEN w [4]   = 3
   IF (mms4) THEN w [7]   = 3

   n_mms = mms1 + mms2 + mms3 + mms4

    ;if nc le 2 then cs=[70,238]
    ;if nc eq 3 then cs=[70,200,238]
    ;if nc eq 4 then cs=[70,130,200,238]
    ;if nc eq 5 then cs=[46,82,128,200,238]
    ;color_space = [235, 205, 190, 177, 150, 115, 105, 95, 80, 55, 35, 10]
    ;                0    1    2    3    4    5    6    7   8   9  10  11

   ; Check if any MMS spacecraft are being ploted.  In this case we can ahead
   ; and asign colors in advance as long as there are four or fewer spacecraft 
   ; whose orbits are being displayed.  
   IF n_mms eq 0 THEN BEGIN

       SWITCH colors OF

           4 : w [4] = 2
           3 : w [11] = 2
           2 : w [8] = 2
           1 : BEGIN 
                 w [0] = 2

                 RETURN, w
               END

       ENDSWITCH

   ENDIF

   ; Block out yellow and light green unless we really need them.
   IF  n_mms + colors lt 12 THEN w [2] = 5
   IF  n_mms + colors lt 10 THEN w [3] = 5 
   IF  n_mms + colors lt 9  THEN w [4] = 5 

   ; Place all other colors in unoccupied spaces.  These will bunch up toward the
   ; beginning of the color array.

   ; Loop through all non-mms colors
   FOR i = 0, colors - 1 DO BEGIN 
       ; Start at the beginning of the color array, place the color at the 
       ; first empty space.  Rewrite to non-loop method using where.
       FOR j = 0, max_color - 1 DO BEGIN

           IF  w [j] eq 0 THEN BEGIN

               w [j] = 1

               BREAK

           ENDIF

       ENDFOR

   ENDFOR

   RETURN, w

END  

; Optimize the color scale array.
FUNCTION create_color_scale, w

    ; Find the size of the color scale being used.
    max_color = N_ELEMENTS (w)

    ; Run until nothing changes.  ?? Do we want to put something here to 
    ; prevent infinite loops ??
    REPEAT BEGIN

       ; Reset changed flag.
       changed = 0

       ; limit sets maximum that color can be moved in the color array in
       ; order to increase the color separation.  Use this to keep colors
       ; from bumping into each other.
       limit = max_color - 1

       ; Note, no need to bother with the last elelment in the color array.  
       FOR i = max_color - 2, 1, -1 DO BEGIN

           ; Check for unoccupied space.  If unoccupied, then move on.
           IF  w [i] eq 0 THEN CONTINUE

           ; Check for unmovable color.  MMS!
           IF  w [i] ge 2 THEN CONTINUE

           ; Check if the color position is already at the limit.  In this case
           ; No more moves are possible, so just exit the loop.
           IF i ge limit THEN BREAK

           ; If we got here, then there is movable color at position i
  
           ; pos0 is the intial positioDeviceClose.pron of the color
           ; pos_tst is the position at which we are testing what will happen
           ; to the separation if we move the color to that position.
           pos0 = i 
           pos_tst = i

           ; Find the current separation factor.  Our goal is to create changes
           ; that will reduce this value.
           current = get_separation (w [0:limit])
    
           ; Initilize the pos (position) and sep (separation) arrays.  Start
           ; with our initial conditions.
           pos = [i]
           sep = [current]

           ; Remove the color from its current positon.
           w [pos_tst] = 0 

           ; Check what moving the color from its current position up to the 
           ; limit does to the separation value.  After this loop completes
           ; we will choose the position with the lowest separation value as
           ; new position of the color.
           WHILE  (1) DO BEGIN

               ; Find the next position to test the color separation at.
               WHILE (pos_tst lt limit) DO BEGIN

                   ; increment to the next position
                   pos_tst++ 

                   ; Just skip over fixed (MMS) colors
                   IF  w [pos_tst] ge 2 THEN CONTINUE

                   ; Check for occupied space.  In this case we will quit testing
                   ; new positions even though we have not yet reached the limit.

                   ; This is an error.
                   IF  w [pos_tst] eq 1 THEN BEGIN 
                 
                       ; PRINT, "Attempted to move color to occupied space.  Exiting."

                       w [pos0] = 1

                       RETURN, w

                   ENDIF

                   ; If we get to here, then we found the next position for the
                   ; test
                   BREAK

               ENDWHILE

               ; Check to make sure we have not reached the limit.  If we have,
               ; stop doing test and go make a decission !!
               IF pos_tst ge limit THEN BREAK

               ; Set the color at the test position 
               w [pos_tst] = 1

               ; Determine what the separation will be after the color is advanced
               ; to the test position. 
               test = get_separation (w [0:limit]) 

               ; Record our results  
               pos = [pos, pos_tst]
               sep = [sep, test]

               ; Since this just a test, remove the color.
               w [pos_tst] = 0

           ENDWHILE

           ; Find the entry in separation test array that generated the lowest
           ; separation value.  If therDeviceClose.proe is tie, wieght the values so that the
           ; position with the highest indice will win.
           m = MIN (REVERSE (FINDGEN (N_ELEMENTS (sep)) * 0.001) + sep, x) 

           ; Calculate the new position
           new_pos = pos [x]

           ; Set the value in the color array to indicate the color is at its new 
           ; position.
           w [new_pos] = 1

           ; Set the changed flag depending on if we had to move a color
           IF new_pos ne pos0 THEN changed = 1

           ; Move the limit back to prior to the position of the current color.	
           limit = new_pos - 1
  
       ENDFOR

    ENDREP UNTIL changed eq 0

    RETURN, w  

END

FUNCTION get_ssc_colors, tagnms

    ; This section was removed in order to use a computed method for assigning 
    ; colors to spacecraft plots.
    ; Ron Yurow (Sep 9, 2016) 
    ;color_scale=255/(numstr+3)
    ; RCJ 02/17/2006  Picking better colors. Avoiding yellow and picking
    ; greens/blues as far from each other as possible.
    ; If the max number of satellites allowed to be plotted increases
    ; more lines have to be added here.
    ;if nc le 2 then cs=[70,238]
    ;if nc eq 3 then cs=[70,200,238]
    ;if nc eq 4 then cs=[70,130,200,238]
    ;if nc eq 5 then cs=[46,82,128,200,238]
    ;if nc eq 6 then cs=[40,70,100,170,200,238]
    ;if nc eq 7 then cs=[40,65,85,110,160,200,238]
    ;if nc eq 8 then cs=[10,40,70,100,130,170,200,238]
    ;  RCJ 12/07/2007  Increasing this number to 12.  It was tough enough to
    ;     find 8 colors that weren't so close to each other....  here go some guesses...
    ;if nc eq 9 then cs=[10,25,40,70,100,130,170,200,238]
    ;if nc eq 10 then cs=[10,25,40,55,70,100,130,170,200,238]
    ;if nc eq 11 then cs=[10,25,40,55,70,100,130,145,170,200,238]
    ;if nc eq 12 then cs=[10,25,40,55,70,100,130,145,170,185,200,238]

    ; The following section creates the color scale array.  The array assigns colors 
    ; (actually indexes into the Rainbow+White color table) for each spacecraft whose
    ; orbit is being plotted.  The MMS spacecraft are special, and if any MMS spacecraft
    ; orbit is being plotted, then it is assigned a spacecraft specific color.  Other 
    ; spacecraft are assigned colors in such as a way to maximize the contrast with 
    ; all the spacecraft in the plot.
    ; Ron Yurow (Sep 9, 2016)

    ; Create flags to indicate the presence of MMS spacecraft in the plot.
    ; Each MMS craft will be assigned a specific color regardless of what
    ; other pacecraft are being plotted.
    mms1 = 0
    mms2 = 0
    mms3 = 0
    mms4 = 0

    ;  Original MMS colors
    ;  mms1 : black  (white if dark bkg) 0
    ;  mms2 : vermillion (red)  213
    ;  mms3 : bluish-green 158
    ;  mms4:  sky-blue 86

    ;  Adjusted MMS colors (Hue Optimized)
    ;  mms1 : black  (white if dark bkg) 0
    ;  mms2 : vermillion (red)  220
    ;  mms3 : bluish-green 150
    ;  mms4 : sky-blue 90

    ; nc is the number of non-mms colors that will be needed.
    ; color_scale is the colors being used for all missions being plotted
    ; map is an array that maps colors from the color space array (non mms mission
    ; colors) color_scale.
    map = WHERE (STREGEX (tagnms, 'MMS') ne 0, cnt)
    nc = cnt 

    IF cnt eq 0 THEN map = -1

    color_scale = INTARR (N_ELEMENTS (tagnms))

    ; Force MMS colors, in any of the spacecraft are one of the MMS spacecraft.
    ; We will do this for all four MMS spacecraft.
    index = WHERE (STREGEX (tagnms, 'MMS1') ne -1, cnt)

    IF cnt THEN BEGIN

       color_scale [index] = 5

       mms1 = 1

    ENDIF

    index = WHERE (STREGEX (tagnms, 'MMS2') ne -1, cnt)

    IF cnt THEN BEGIN 

       color_scale [index] = 220

       mms2 = 1

    ENDIF

    index = WHERE (STREGEX (tagnms, 'MMS3') ne -1, cnt)

    IF cnt THEN BEGIN 

       color_scale [index] = 150 

       mms3 = 1

    ENDIF

    index = WHERE (STREGEX (tagnms, 'MMS4') ne -1, cnt)

    IF cnt THEN BEGIN 

       color_scale [index] = 90

       mms4 = 1

    ENDIF 

    ; Check if there any orbits for non-MMS spacecraft being plotted.  If all
    ; we are plotting are orbits for MMS spacecraft, then we are finished.
    ; (MMS spacecraft are assigned specific question).
    IF map [0] ne -1 THEN BEGIN

      ; Create the color scale. 
      ;w = init_color_scale (nc, 12, mms1=mms1, mms2=mms2, mms3=mms3, mms4=mms4)
      ;  RCJ 08May2019   12 s/c -> 15
      w = init_color_scale (nc, 15, mms1=mms1, mms2=mms2, mms3=mms3, mms4=mms4)

      w = create_color_scale (w)

      ; The colors to assign to 
      ;color_space = [235, 205, 190, 177, 150, 115, 105, 95, 80, 55, 35, 10]
      ;  RCJ 08May2019  Added more colors
      color_space = [235, 205, 190, 177, 150, 115, 105, 95, 80, 55, 35, 10, 220 ,230, 249]

      ind = [1]

      FOR i = 1, 2 DO BEGIN

         tmp = WHERE (w eq i, cnt )

         IF  cnt gt 0 THEN ind = [ind, tmp]

      ENDFOR 

      ind = ind [1:*]

      color_scale [map] = color_space [ind]

    ENDIF

    RETURN, color_scale

END
