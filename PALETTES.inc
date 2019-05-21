;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; LIST OF PALETTES
; (A table of pointers to each palette is generated in [pal_list] at runtime)
; Each entry is composed of eight 3-byte RGB values, plus some metadata:
;
; o24: db  attrMap     ; Selector in table of attributes for all UI elements
; o25: db  ASCIIZname  ; Human-readable name
;      db  0           ;     (ASCIIZ)
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pal_entries:
   db 00h,00h,00h, 00h,00h,2Ah, 15h,15h,15h, 2Ah,00h,00h
   db 00h,2Ah,00h, 00h,2Ah,2Ah, 2Ah,2Ah,2Ah, 3Fh,3Fh,3Fh,0,'Default',0
   db 00h,00h,00h, 00h,00h,2Ah, 2Ah,00h,00h, 2Ah,15h,00h
   db 00h,2Ah,00h, 00h,2Ah,2Ah, 2Ah,2Ah,2Ah, 3Fh,3Fh,15h,4,'CMOS Error',0
   db 00h,00h,00h, 15h,15h,15h, 2Ah,00h,00h, 2Ah,00h,2Ah
   db 2Ah,15h,00h, 00h,2Ah,00h, 00h,2Ah,2Ah, 2Ah,2Ah,2Ah,3,'1200bps',0
   db 0Bh,03h,00h, 1Bh,09h,00h, 1Bh,0Dh,1Fh, 26h,14h,02h
   db 3Bh,00h,00h, 30h,1Fh,09h, 35h,21h,37h, 3Bh,34h,09h,2,'Solar A:\',0
   db 00h,00h,0Bh, 18h,08h,00h, 20h,10h,00h, 10h,18h,20h
   db 38h,00h,00h, 1Ch,28h,2Ch, 30h,20h,08h, 38h,30h,08h,0,'Solar B:\',0
   db 00h,00h,00h, 0Bh,0Bh,0Bh, 11h,11h,11h, 18h,18h,18h
   db 1Eh,1Eh,1Eh, 23h,23h,23h, 2Ch,2Ch,2Ch, 3Fh,3Fh,3Fh,1,'White Mono',0
   db 00h,00h,00h, 00h,0Bh,02h, 01h,15h,05h, 01h,1Bh,06h
   db 01h,21h,08h, 02h,28h,09h, 02h,31h,0Bh, 0Dh,3Fh,17h,1,'Green Screen',0
   db 0Ah,00h,00h, 17h,01h,00h, 21h,02h,00h, 2Bh,03h,00h
   db 32h,05h,00h, 39h,09h,00h, 3Fh,15h,00h, 3Fh,26h,00h,1,'Plasmatics',0
   db 1Fh,23h,19h, 1Bh,1Eh,17h, 18h,1Ah,15h, 14h,16h,13h
   db 11h,13h,11h, 0Fh,10h,10h, 0Ch,0Dh,0Eh, 05h,06h,06h,1,'Xtal Logic',0
   db 3Ch,3Bh,37h, 33h,33h,31h, 2Bh,2Ch,2Bh, 20h,21h,21h
   db 1Bh,1Dh,1Eh, 15h,17h,18h, 0Dh,0Fh,10h, 05h,06h,06h,1,'Papirosa',0
   db 07h,06h,0Bh, 0Dh,08h,1Dh, 18h,12h,12h, 22h,1Dh,08h
   db 2Dh,27h,27h, 22h,32h,32h, 37h,32h,1Dh, 3Fh,3Fh,3Fh,1,'Arrakis',0
   db 00h,00h,00h, 00h,00h,2Ah, 15h,15h,15h, 2Ah,00h,00h
   db 00h,2Ah,2Ah, 2Ah,2Ah,2Ah, 3Fh,3Fh,15h, 3Fh,3Fh,3Fh,5,'Boreland',0
   db 00h,00h,00h, 00h,00h,2Ah, 2Ah,00h,00h, 00h,2Ah,2Ah
   db 2Ah,2Ah,2Ah, 15h,3Fh,3Fh, 3Fh,3Fh,15h, 3Fh,3Fh,3Fh,6,'Kommander',0
   db 00h,00h,00h, 00h,00h,2Ah, 15h,15h,15h, 2Ah,00h,00h
   db 15h,15h,3Fh, 2Ah,2Ah,2Ah, 3Fh,3Fh,15h, 3Fh,3Fh,3Fh,7,'Nortilities',0
   db 00h,00h,00h, 00h,00h,2Ah, 15h,15h,15h, 2Ah,00h,00h
   db 00h,2Ah,00h, 2Ah,15h,00h, 2Ah,2Ah,2Ah, 3Fh,3Fh,3Fh,8,'GreasyTools',0
   db 00h,00h,00h, 12h,10h,0Fh, 1Fh,16h,11h, 2Dh,25h,1Eh
   db 2Ch,00h,15h, 11h,26h,12h, 3Fh,3Fh,15h, 3Ah,3Ah,3Ah,2,'Impulsive',0
   db 00h,00h,00h, 12h,10h,0Fh, 0Ah,19h,2Dh, 1Eh,28h,37h
   db 3Fh,15h,15h, 2Ch,2Ch,2Ch, 15h,3Fh,15h, 33h,3Ah,3Fh,2,'Bluetrax',0
   db 00h,00h,00h, 08h,0Bh,13h, 19h,09h,00h, 28h,0Eh,00h
   db 3Fh,08h,10h, 20h,20h,20h, 00h,27h,05h, 3Eh,1Fh,00h,2,'Volcanic',0
   db 26h,26h,33h, 33h,26h,26h, 19h,0Ch,0Ch, 26h,19h,19h
   db 11h,11h,19h, 1Eh,1Eh,1Eh, 11h,00h,00h, 3Fh,35h,35h,9,'Vogonic',0
   db 00h,07h,01h, 00h,10h,00h, 00h,20h,10h, 20h,08h,08h
   db 00h,38h,00h, 30h,18h,18h, 00h,2Ch,23h, 18h,38h,28h,0,'Battery Leak',0
   db 13h,00h,09h, 00h,00h,00h, 13h,21h,13h, 24h,00h,17h
   db 35h,35h,00h, 2Dh,12h,35h, 1Eh,31h,13h, 35h,24h,35h,2,'Acid Reign',0
   db 00h,00h,0Fh, 09h,03h,17h, 10h,05h,21h, 1Bh,05h,25h
   db 25h,05h,25h, 2Ch,07h,2Bh, 31h,10h,37h, 3Eh,18h,25h,1,'Sundown',0
   db 3Fh,3Fh,3Fh, 3Fh,3Fh,15h, 15h,3Fh,3Fh, 3Fh,15h,15h
   db 2Ah,2Ah,2Ah, 2Ah,00h,2Ah, 15h,15h,15h, 00h,00h,00h,0,'Retinal Death',0


;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; TABLE OF UI ELEMENT ATTRIBUTE MAPPINGS
; Each row represents an element. Each byte is the attribute value for
; that element in the corresponding mapping (0-9).
; To get the attribute for an element in the current palette, take its label
; and add the 'attrMap' from the PalList entry (before the palette name).
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

att:                      ; /~~~~~~~~~~~~ Attr Maps ~~~~~~~~~~~~\

;GENERAL (UI font) ........ 0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8 - 9
  .normal_right:        db 0Eh,0Eh,38h,0Ch,68h,58h,3Fh,78h,1Eh,0Eh
  .normal_left:         db 28h,28h,0Bh,68h,5Eh,1Dh,1Bh,4Fh,09h,48h
  .caption:             db 0Ch,0Fh,0Eh,0Dh,6Fh,5Fh,3Dh,79h,58h,0Fh
  .caption_line:        db 0Ah,0Ah,39h,0Ah,69h,58h,38h,78h,1Eh,0Ah
  .key_item:            db 0Dh,0Dh,3Fh,0Bh,6Ah,5Bh,3Eh,7Bh,1Fh,0Ch
  .key_brackets:        db 0Bh,0Bh,3Ah,0Ah,6Ch,5Eh,38h,7Ch,18h,0Ah
  .key_slash:           db 0Ah,0Ah,3Ah,0Ah,6Ch,5Ah,3Ch,7Ch,1Eh,0Ah
  .key_special:         db 0Bh,0Bh,3Ch,0Ah,6Bh,5Bh,3Ah,7Bh,1Fh,0Bh
  .disabled:            db 0Ah,0Ah,3Ah,09h,6Dh,5Ah,38h,7Dh,18h,0Dh
  .checkmark:           db 48h,78h,1Eh,58h,79h,68h,58h,4Eh,48h,4Fh
  .font_ON_name:        db 2Fh,2Fh,0Eh,68h,5Fh,1Fh,1Dh,4Fh,0Fh,4Bh
  .font_ON_num:         db 2Dh,2Dh,0Fh,6Bh,5Ah,1Eh,1Eh,4Eh,0Ch,48h
  .font_ON_brks:        db 2Bh,2Bh,0Ah,6Ah,58h,1Bh,1Bh,49h,0Eh,4Eh
  .font_ON_unsaved:     db 2Eh,28h,0Ch,6Ah,5Ah,1Eh,1Eh,4Eh,0Bh,4Fh
  .font_OFF_name:       db 0Ch,0Ch,39h,0Dh,6Bh,5Fh,3Dh,79h,1Eh,0Bh
  .font_OFF_unsaved:    db 0Bh,0Bh,3Ch,0Ah,6Ah,5Bh,3Ah,7Bh,0Bh,0Fh
;STATUS BAR (UI font) ............................................
  .prog_name:           db 6Bh,6Bh,1Eh,7Ah,4Fh,4Eh,0Fh,2Fh,69h,68h
  .version:             db 68h,68h,1Dh,78h,48h,48h,0Ch,2Eh,68h,6Bh
  .ver_separator:       db 6Dh,6Dh,1Fh,7Eh,4Ah,4Bh,0Bh,28h,6Fh,6Ch
  .ver_date:            db 6Ch,6Ch,1Bh,7Dh,4Eh,49h,0Dh,2Dh,6Bh,69h
;ACTIVE PROMPT (UI font) .........................................
  .prompt_warn:         db 3Fh,78h,48h,2Fh,2Fh,3Fh,2Fh,3Fh,3Fh,7Eh
  .prompt:              db 1Eh,1Dh,2Bh,1Eh,38h,2Dh,48h,1Eh,48h,3Eh
  .prompt_hilite:       db 1Fh,1Fh,2Fh,1Fh,3Fh,2Fh,4Eh,1Fh,4Fh,3Fh
  .prompt_select:       db 5Fh,7Ah,1Eh,3Fh,4Fh,6Bh,2Dh,58h,3Fh,68h
;BOXES (UI font) .................................................
  .box_ON_title:        db 2Fh,2Fh,0Eh,6Ah,5Fh,1Fh,1Eh,4Eh,38h,4Fh
  .box_ON_border:       db 2Fh,2Fh,0Fh,68h,5Bh,1Fh,1Dh,4Fh,0Eh,4Bh
  .box_ON_ruler:        db 2Ch,2Ch,0Bh,68h,58h,1Ch,1Ch,48h,0Ch,48h
  .box_ON_ruler_on:     db 2Fh,2Fh,0Eh,6Ah,5Fh,1Fh,1Eh,4Eh,0Fh,4Fh
  .box_OFF_title:       db 2Eh,2Eh,0Ah,69h,58h,1Ch,1Dh,49h,0Eh,4Eh
  .box_OFF_border:      db 28h,28h,09h,69h,5Eh,1Ah,1Bh,4Dh,0Eh,48h
  .box_OFF_ruler:       db 29h,29h,0Ah,6Ch,5Bh,18h,1Bh,49h,0Bh,4Eh
  .box_OFF_ruler_on:    db 2Dh,2Dh,0Bh,69h,59h,1Ch,1Dh,4Dh,0Eh,4Bh
;BOX CONTENTS (User font) '*':tweak at runtime for editbox (UI font)
  .box_ON_content:      db 16h,16h,15h,50h,17h,16h,15h,47h,47h,21h ;*
  .box_ON_marked:       db 57h,57h,27h,41h,26h,51h,40h,16h,63h,04h ;*
  .box_ON_cursor:       db 34h,34h,03h,72h,46h,34h,24h,35h,15h,65h ;*
  .box_ON_col9:         db 12h,12h,12h,57h,13h,14h,10h,45h,40h,23h ;*
  .box_ON_unsaved:      db 17h,17h,14h,20h,27h,36h,25h,37h,37h,27h
  .box_ON_currchar:     db 70h,70h,60h,70h,75h,73h,46h,63h,75h,70h
  .box_OFF_content:     db 06h,06h,05h,61h,03h,15h,13h,45h,06h,63h ;*
  .box_OFF_marked:      db 27h,27h,23h,24h,24h,41h,31h,12h,37h,46h ;*
  .box_OFF_cursor:      db 15h,15h,13h,75h,57h,34h,24h,63h,47h,14h ;*
  .box_OFF_col9:        db 02h,02h,01h,67h,01h,12h,10h,42h,03h,62h ;*
  .box_OFF_unsaved:     db 05h,07h,04h,21h,02h,35h,23h,35h,36h,65h
  .box_OFF_currchar:    db 50h,50h,16h,05h,75h,53h,36h,16h,75h,14h
;DIALOG (UI font) ................................................
  .dlg_title:           db 48h,1Fh,1Eh,2Fh,5Fh,2Eh,7Ch,1Fh,58h,7Ch
  .dlg_bg:              db 28h,28h,2Fh,18h,4Eh,49h,49h,2Fh,2Eh,48h
  .dlg_ON_label:        db 2Fh,2Fh,2Fh,1Eh,4Ah,4Fh,4Eh,2Fh,2Fh,4Bh
  .dlg_ON_text:         db 1Eh,1Eh,0Dh,58h,1Fh,1Eh,1Dh,4Fh,4Fh,0Ch
  .dlg_ON_hilite:       db 5Fh,78h,1Eh,49h,2Eh,59h,3Fh,1Eh,6Bh,1Eh ;?
  .dlg_ON_vline:        db 1Ah,1Ah,0Ah,59h,1Ch,1Ch,1Bh,4Ah,4Ah,0Ah
  .dlg_OFF_label:       db 28h,2Ch,28h,1Ch,49h,49h,49h,2Eh,2Eh,48h
  .dlg_OFF_text:        db 0Eh,0Dh,0Ah,0Fh,0Bh,0Dh,0Bh,1Dh,0Eh,6Bh
  .dlg_OFF_hilite:      db 0Fh,39h,0Eh,0Ch,0Dh,0Eh,3Dh,09h,48h,68h ;?
  .dlg_OFF_vline:       db 0Ah,0Ah,0Ah,09h,0Ah,0Ah,09h,1Ah,0Ah,6Ah