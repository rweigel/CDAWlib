;$Author: kovalick $
;$Date: 2017/03/09 21:15:16 $
;$Header: /home/cdaweb/dev/control/RCS/DeviceClose.pro,v 1.13 2017/03/09 21:15:16 kovalick Exp kovalick $
;$Locker: kovalick $
;$Revision: 1.13 $
;
; History:
; 25 Aug 1993 Bobby Candey, Code 632
; Robert.M.Candey.1@gsfc.nasa.gov; 1995 June 22; added GIF
; added Mac/Win and ION, RMC 2001 July 19
;
;Copyright 1996-2013 United States Government as represented by the 
;Administrator of the National Aeronautics and Space Administration. 
;All Rights Reserved.
;
;------------------------------------------------------------------
Pro DeviceClose,command=command

; This procedure closes the print file and submits it to the printer

common deviceTypeC, deviceType, file
;  '(Local=0, PS color=1, PS gray=2, PS BW=3, Tek=4, Zbuf=5, GIF=6, PNG=7, ION=9)? ',deviceType   
if n_elements(deviceType) le 0 then message,'No deviceType defined'
Case deviceType of
  0: begin ; Local windows
    End            
  1: begin ; color Postscript
     device,/close
    if keyword_set(command)  then spawn, command
;     spawn, 'print/queue=NCFCP2_PS idl.ps/del'
     ;print, 'IDL.PS is available for printing to a color Postscript printer'
    End
  2: begin ; grayscale Postscript
     device,/close
    if keyword_set(command)  then spawn, command
;     spawn, 'print/queue=NCFLP1_PS idl.ps/del/notify'
     ;print, 'IDL.PS is available for printing to a Postscript printer'
    End
  3: begin ; Black and white Postscript
     device,/close
    if keyword_set(command)  then spawn, command
;     spawn, 'print/queue=NCFLP1_PS idl.ps/del/notify'
;     print, 'IDL.PS is available for printing to a Postscript printer'
    End
  4: begin ; Tektronix 4105
    End

  5: begin ; Z buffer
     xscale = !x.s
     yscale = !y.s
     device,/close   ;RTB added 11/96
    End            

  6: begin ; GIF image file output
     xscale = !x.s
     yscale = !y.s
     bytemap = tvrd() ; get bitmap from Z device
;     save,xscale,yscale,bytemap,file='bytemap.dat'
     tvlct, r,g,b, /get ; get colortable actually used
     ;TJK 6/20/2016 test the bytemap to see if it has anything in it, if not write a
     ;little one instead of a potentially huge one.
     if (max(bytemap) le 0) then begin
      ;CWG 11/08/2016 - This 'if' statement basically won't allow the printing of the below statement if the file is a .gifjunk file
      ;(the printed message below falsely indicated an error existed)
        if ~(STRPOS(file, '.gifjunk') gt 0) then begin 
          print, 'DEBUG, Overriding size of bytemap because no data found'
          bytemap = BYTARR(2,2)
        endif
     endif
     write_gif, file, bytemap, r,g,b
;    print,' byte map stored in file: ', file ; 'bytemap.dat'
;If you transfer the GIF image to a Macintosh, use Fetch and set its Suffix
;Mapping Custom option to assign the filetype 'GIFf' and use binary transfer.

    ; RCJ 05/22/2014  Added !p.noerase=0 . If over 30 datasets are requested for
    ; plot (ie, more than 30 buffers, the max currently allowed by each call to plotmaster) and the
    ; last of the first 30 is an orbit plot, subsequent plots show as 
    ; all black windows.  Setting !p.noerase=0 fixed the problem in my tests.
    !p.noerase=0
     device,/close   ;RTB added 11/96
    End            

  7: begin ; PNG image file output
     xscale = !x.s
     yscale = !y.s
     bytemap = tvrd() ; get bitmap from Z device
;     save,xscale,yscale,bytemap,file='bytemap.dat'
    tvlct, r,g,b, /get ; get colortable actually used
    if (max(bytemap) le 0) then begin
    ;CWG 11/08/2016 - This 'if' statement basically won't allow the printing of 
    ;the below statement if the file is a .pngjunk file
      ;(the printed message below falsely indicated an error existed)
      if ~(STRPOS(file, '.pngjunk') gt 0) then begin 
        print, 'DEBUG, Overriding size of bytemap because no data found'
        bytemap = BYTARR(2,2)
      endif
    endif

;IDL5.3 and previous versions don't correctly orient the plots inside 
;the file so they must be flipped prior to writting them into the png file.
;TJK 5/29/2002
    IF !VERSION.RELEASE LT '5.4' THEN bytemap = REVERSE(bytemap,2)
    write_png, file, bytemap, r,g,b
     device,/close   ;RTB added 11/96
    End            

  9: begin ; ION
    End            
  Else: MESSAGE,'INCORRECT OUTPUT DEVICE!!!'
endcase
;set_plot,'X' ; RCJ 01/13/2003 This line seems to be missing when I try to run some plots
           ; from the IDL prompt. It doesn't seem to make a difference for plots
	   ; created by CDAWeb.
return
END ; deviceClose

