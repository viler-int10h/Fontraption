;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; Headers and contents for supported import/export file formats
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  head_xbin: db 'XBIN', 26,  80,0, 16,0, '_',  00001010b

  head_bmp:  db 'BM__', 0,0,0,0,0,0      ; 00h: ID, 02h: filesize (imgsize+3E)
             db 3Eh,0,0,0,28h,0,0,0      ; 0Ah: img offset, 0Eh: hdrsize (40)
             db 80h,0,0,0,'__', 0,0      ; 12h: width, 16h: height
             db 1,0,1,0,0,0,0,0          ; 1Ah: planes, 1Ch: bpp, 1Eh: compr.
             db '__', 0,0                ; 22h: image size in bytes
             db 12h,0Bh,0,0,12h,0Bh,0,0  ; 26h: h. res, 2Ah: v. res
             times 8 db 0                ; 2Eh: palette stuff
             db 0FFh,0FFh,0FFh,0,0,0,0,0 ; 36h: color map

  head_com:  db 056h,049h,04Ch,045h,01Ah,000h,083h,0C4h,003h,0B8h
             db 010h,011h,0BDh,019h,001h,0B9h,000h,001h,099h,0BBh,000h
             db '_' ,0CDh,010h,0C3h      ; 15h: height

  head_tsr:  db 0E9h,'_', '_', 000h,000h,080h,0FCh,000h,075h,010h,03Ch,003h
             db 077h,0F2h,053h,089h,0C3h,02Eh,08Ah,09Fh,02Dh,001h,04Bh,05Bh
             db 074h,017h,03Dh,000h,012h,075h,0E1h,080h,0FBh,021h,075h,0DCh
             db 0B0h,021h,0CFh,00Dh,056h,049h,04Ch,045h,01Ah,'_', '_', '_'
             db '_', 09Ch,00Eh,0E8h,0CAh,0FFh,050h,051h,052h,053h,055h,056h
             db 057h,01Eh,006h,00Eh,00Eh,01Fh,007h,0E8h,00Fh,000h,09Ch,00Eh
             db 0E8h,0B5h,0FFh,007h,01Fh,05Fh,05Eh,05Dh,05Bh,05Ah,059h,058h
             db 0CFh,0B8h,010h,011h,0BDh,063h,001h,0BBh,000h,'_', 0B9h,000h
             db 001h,099h,0C3h

  tail_tsr:  db 0B8h,000h,012h,0B3h,021h,0CDh,010h,03Ch,021h,075h,03Eh,0BAh
             db '_', '_', 0E8h,077h,000h,031h,0C0h,08Eh,0D8h,08Eh,0C0h,0C5h
             db 01Eh,040h,000h,081h,07Fh,023h,056h,049h,075h,01Eh,081h,07Fh
             db 025h,04Ch,045h,075h,017h,0FAh,0BEh,001h,001h,0BFh,040h,000h
             db 0A5h,0A5h,0FBh,01Eh,007h,0B4h,049h,0CDh,021h,072h,005h,0BAh
             db '_', '_', 0EBh,003h,0BAh,'_', '_', 00Eh,01Fh,0E8h,040h,000h
             db 0C3h,0B8h,010h,035h,0CDh,021h,0FEh,006h,000h,001h,089h,01Eh
             db 001h,001h,08Ch,006h,003h,001h,0BAh,005h,001h,0B4h,025h,0CDh
             db 021h,0B4h,00Fh,0CDh,010h,03Ch,003h,077h,00Fh,0BBh,02Dh,001h
             db 0D7h,0FEh,0C8h,075h,007h,00Eh,007h,0E8h,'_', '_', 0CDh,010h
             db 08Eh,006h,02Ch,000h,0B4h,049h,0CDh,021h,0BAh,'_', '_', 0B8h
             db 000h,031h,0CDh,021h,0B4h,009h,0CDh,021h,0C3h
             db 'Last TSR font $'
             db 'removed',13,10,'$'
             db 'unremovable!',13,10,'$'

; RELEVANT OFFSETS FOR TSR FORMAT:

;   028h:  'VILE' signature
;   063h:  font data

; Locations to patch in head_tsr ('_' bytes):
; + 02Dh:  modes 0, 1 yes/no
; + 02Fh:  modes 2, 3 yes/no
; + 05Dh:  font height
; + 001h:  OFFSET_INIT: = 063h + (256*height)
; Offsets to patch in tail_tsr:
; + 00Ch:  OFFSET_TXT_PRE   = OFFSET_INIT + 18Dh
; + 03Ch:  OFFSET_TXT_GOOD  = OFFSET_INIT + 19Ch
; + 041h:  OFFSET_TXT_BAD   = OFFSET_INIT + 1A6h
; + 074h:  DISPLC_PREP      = -(OFFSET_INIT + 21h)
; + 081h:  NUMPAR = (OFFSET_INIT shr 4) rounded up
