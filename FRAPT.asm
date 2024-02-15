; FONTRAPTION v1.0
; assemble w/FASM
; requirements: VGA, DOS 3.0+, 224K RAM

; NOTE... coding styles and practices are quite mixed throughout the source.
; Mostly because the project was started with the goal of being as size-
; optimized as possible, but that went out the window about half way through.
; So if something doesn't make any bloody sense whatsoever, there ya go.

;  -VileR, 5/2019

;-----------------------------------------------------------------------------
;#################################  MACROS  ##################################
;-----------------------------------------------------------------------------
macro memcp srcptr, destptr, words {    ; Memory copy: SI, DI, number of words
    if srcptr eq 0
           xor   si, si
    else if srcptr eq si
    else
           mov   si, srcptr
    end if
    if destptr eq 0
           xor   di, di
    else if destptr eq di
    else
           mov   di, destptr
    end if
           mov   cx, words
           rep   movsw
}

macro dmsg id, [message] {
  common
    label  debug_msg_#id
  forward
    db message
  common
    .end = $ - debug_msg_#id
}

macro debug_prompt id, cursor_pos {
  local ..x, ..y
  if DEBUGMODE
    cmp    byte[cs:state.started], 1
    je     ..y
    push   ds
    push   es
    push   bp
    push   ax
    push   bx
    push   cx
    push   dx
    push   cs
    pop    es
    push   cs
    pop    ds
  if cursor_pos eq
    mov    ah, 3                         ; get cursor position to DX
    xor    bh, bh
    int    10h
  else
    mov    dx, cursor_pos                ; use specified position
  end if
    mov    ax, 1301h                     ; write string at cursor
    mov    bl, 7                         ; BH = page, BL = attr
    mov    bp, debug_msg_#id             ; ES:BP = string to write
    mov    cx, debug_msg_#id#.end        ; CX = #chars in string
    int    10h                           ; (DX = cursor position)
    xor    ax, ax
    int    16h                           ; get keypress
    cmp    al, 1Bh                       ; ESC?
    jne    ..x
    jmp    debug_exit
  ..x:
    mov    dx, debug_msg_cr
    mov    ah, 9
    int    21h
    pop    dx
    pop    cx
    pop    bx
    pop    ax
    pop    bp
    pop    es
    pop    ds
  ..y:
  end if
}


;-----------------------------------------------------------------------------
;###############################  CONSTANTS  #################################
;-----------------------------------------------------------------------------
  CHRMAP_SELECT     equ 1 shl 1                   ; Charmap Select reg. flags
  CHRMAP_TALLACTIVE equ 1 shl 2
  CHRMAP_2ACTIVE    equ 1
  CHRMAP_PVW_SET    equ 1 shl 5
  CHRMAP_PVW_CLR    equ 110011b

  CURSOR_ON         equ 0                         ; Cursor Start reg. values
  CURSOR_OFF        equ 1 shl 5

  SCR_ED25          equ 0                         ; UI screen numbers
  SCR_ED50          equ 1
  SCR_FILES         equ 2

  ROW               equ 160                       ; Screen offsets (VRAM)
  VRAM_ED25         equ ROW
  VRAM_ED50         equ ROW*49
  VRAM_FILES        equ ROW*25
  VRAM_PVW_AREA     equ ROW*98
  VRAM_PVW_TEXT     equ ROW*139+20*2

  VRAM_UI16_FONT    equ 0                         ; Font offsets (VGA plane 2)
  VRAM_UI8_FONT     equ 04000h
  VRAM_FONT1        equ 08000h
  VRAM_FONT2        equ 0C000h
  VRAM_UIPVW_FONT   equ 02000h

  GRID1             equ 200                       ; Specific UI characters
  GRID2             equ 201
  GUIDE1            equ 202
  GUIDE2            equ 203
  TAB_BORDER        equ 204
  DRIVE_ICON        equ 205
  MODIFY_UI16       equ VRAM_UI16_FONT+(GRID1*32)
  MODIFY_UI8        equ VRAM_UI8_FONT+(GRID1*32)
  ARROWS            equ 27,24,25,26

  NUM_PALETTES      equ 25                        ; Palette stuff
  NUM_ATTRMAPS      equ 10
  PAL_GETMAP        equ 24
  PAL_GETNAME       equ 25
  BOXOUTER_ATTDIFF  equ NUM_ATTRMAPS*4
  BOXINNER_ATTDIFF  equ NUM_ATTRMAPS*6
  UNSAVED_ATTDIFF   equ NUM_ATTRMAPS*2
  FD_ATTDIFF        equ NUM_ATTRMAPS*4

  QLEN              equ 72                        ; File dialog stuff
  ILEN              equ 63
  I_VRAM            equ VRAM_FILES+ROW*3+14*2
  DTA               equ 80h
  MAX_FILES         equ 2048

  SL                equ byte[scratch]             ; Fake scratch 'register'
  SH                equ byte[scratch+1]
  SX                equ word[scratch]

  OP_JAE            equ 073h                      ; Some opcode bytes
  OP_JA             equ 077h
  OP_STOSB          equ 0AAh
  OP_STOSW          equ 0ABh

  SIG_LEN           equ 6                         ; For BIOS tests
  SIG_AREA_OFFS     equ 0E000h
  SIG_AREA_LEN      equ 128-SIG_LEN
  EQUIPMENT_LIST    equ 0410h
  ONE_FLOPPY_FLAG   equ 0504h
  KBD_TESTKEY       equ 9100h
  KBD_HEAD_PTR      equ 041Ah
  KBD_BUFF          equ 001Eh

  UNDO              equ font.fspec                ; Size of undo buffer

  DEBUGMODE         equ 0                         ; Step-by-step VGA setup
                                                  ;    (w/o video disable)

;-----------------------------------------------------------------------------
;###############################  STRUCTURES  ################################
;-----------------------------------------------------------------------------
struc font {
  .height:   dw ?              ; keep high byte 0
  .unsaved:  dw ?              ; word for alignment
  .padding:  times 16   db ?   ; padding (always zero!) for vertical centering
  .data:     times 8192 db ?   ; always stores full 32-line chars
  .padding2: times 32   db ?   ; more padding (safety margin for font ops)
  .changes:  times 256  db ?   ; 1 if character was modified, 0 otherwise
  .fspec:    times 128  db ?   ; Supplied, then converted to DOS truename
  .fpath:    times 128  db ?   ; Path only (populated from DOS truename)
  .fname:    times 14   db ?   ; Name only (populated from DOS truename)
  .can_undo: dw ?              ;     (+2b padding to ensure terminating 0)
}
virtual at 0
  font   font
  sizeof.font = $
end virtual

struc edbox_attrs
{
  .px0:      db ? ; BG content
  .px1:      db ? ; FG content
  .px0_mark: db ? ; BG marked
  .px1_mark: db ? ; FG marked
  .px0_curs: db ? ; BG cursor
  .px1_curs: db ? ; FG cursor
  .px0_col9: db ? ; BG col9
  .px1_col9: db ? ; FG col9
}
virtual at 0
  edbox_attrs edbox_attrs
end virtual

;-----------------------------------------------------------------------------
;##############################  CODE BEGINS  ################################
;-----------------------------------------------------------------------------
use16
org 100h

  include 'main.inc'           ; initialize + jump to editor + exit routine

;-----------------------------------------------------------------------------
;###########################  SUBROUTINES + DATA  ############################
;-----------------------------------------------------------------------------
  include 'debug.inc'
  include 'dos.inc'
  include 'str.inc'
  include 'vga.inc'
  include 'screen.inc'
  include 'filedlg.inc'
  include 'fontops.inc'
  include 'palettes.inc'
  include 'formats.inc'

;-----------------------------------------------------------------------------
;########################  TEXT, STRINGS, LOCATIONS  #########################
;-----------------------------------------------------------------------------

; Plain strings / prompt strings - - - - - - - - - - - - - - - - - - - - - - -

  txt:
    .ctrl_arr:   db 'Ctrl+',ARROWS,'  ',0
    .fdt_l:      db 'LOAD FONT',0
    .fdt_s:      db 'SAVE FONT',0
    .fdt_i:      db 'IMPORT COM/BMP/XBIN',0
    .fdt_e:      db 'EXPORT .___',0
    .fdt_ext=$-4
    .fdbar:      db ' ',ARROWS,'  ',0,'Select ',179,0,' PgUp/PgDn  ',0,'Prev/'
                 db 'next page ',179,0,' ',17,0C4h,0D9h,0,'  Go ',179,0
                 db ' TAB  ',0,'File/dir ',179,0,' ESC  ',0,'Back ',0
                 db 'Enter file/directory name',0
    .fd_label:   db 'Filename:',0
    .reading:    db 'Reading...',0
    .ext_all:    db '*',0
    .ext_com:    db 'COM',0
    .ext_bmp:    db 'BMP',0
    .ext_xbin:   db 'XB',0
    .format:     db 'Export format: ',0
    .fmt_com:    db 'Persist for BIOS text modes: ',0
    .too_many:   db '(too many',0,'files...)',0
    .exported_f: db 'exported font',0
    .m_f:        db 'Font ',0
    .m_s:        db 'saved',0
    .m_l:        db 'loaded',0
    .m_i:        db 'imported',0
    .m_e:        db 'exported',0
    .charset:    db ' Character Set: ',0
    .what_char:  db 'Character (or ALT+numeric code):',0
    .load_rom:   db 'Get VGA ROM font: ',0
    .loaded_rom: db 'ROM font loaded',0
    .lose:       db 'Lose changes in font ',0
    .both_fonts: db 'both fonts' ;no 0!
    .yes_no:     db ' (y/N)?',0
    .exists:     db 'File exists! Overwrite (y/N)?',0
    .stored:     db 'Stored in clipboard',0
    .swapped:    db 'Chars ___',29,'___ swapped',0
    .height:     db ' Height:',0
    .lines:      db ' lines ',0
    .noname:     db '<NoName>',0
    .del_line:   db 'Dele',0
    .ins_line:   db 'Inser',0
    .dup_line:   db 'Duplica',0
    .finishline: db 'ted line at cursor, font height modified',0
    .done_h=$-21
    .bad_vga:    db 'Set machine=vgaonly in DOSBox for 9-dot mode',0
    .dosbox=$-22
    .no_VGA:     db 'VGA not found',13,10,0
    .CR=$-3
    .no_RAM:     db 'Not enough RAM',0
    .no_DOS3:    db 'DOS 3+ required',0
    .size_err:   db 'Not a raw binary font: ',0
    .dup_args:   db 'Identical files specified',0
    .open_err:   db 'Cannot open ',0
    .wrt_err:    db 'Cannot write ',0
    .fmt_err:    db 'Invalid import format',0
    .ht_err:     db 'Invalid font height',0
    .xb_nofont:  db 'No XBIN font data',0
    .bmp_err:    db 'BMP must be 1-bit color, 128 by 16*(1..32) pixels',0
    .dup_file:   db 'Selected file is being edited as font _!',0
    .dup_of=$-3
    .from_vga:   db 'Active VGA charset used for font 2',0
    .welcome:    db 'Have fun! -VileR',0

    ;prompt strings: 01h = highlight attr on/off, 02h = select attr on/off

    .set_height: db 'Set font height ',1,'[',25,'/',24,']',1,':',0
    .updown=$-10
    .set_pal:    db 'Set palette',0
    .slide:      db 1,'[',ARROWS,']',1,': Displace char bitmap(s), '
                 db 'any other key to end',0
    .pvw_bar:    db 1,'[',25,24,1,'/',1,27,26,']',1,': Colors, ',1,'[F10]'
                 db 1,': 40/80c, ',1,'[T]',1,'ext',0
    .pvw_input:  db 'Input text, ',1,17,0C4h,0D9h,1,' to end, '
                 db 1,'[ESC]',1,': cancel',0
    .xb_512:     db '512-char XBIN font: import part ',1,'[1/2]',1,'?',0

; Data for preview screen- - - - - - - - - - - - - - - - - - - - - - - - - - -

  pvw:
    .pangram:    db 'Pack my box with five dozen liquor jugs.',0
    .threefour:  db 219,250,219,254,178,220,178,220
                 db 177,222,177,221,176,008,176,010
    .boxes:      db 'ÖÄÒÄÄ·ÕÍÑÍÍ¸ÉÍËÍÍ»ÚÄÂÄÄ¿',0
                 db 'ÇÄ×ÄÄ¶ÆÍØÍÍµÌÍÎÍÍ¹ÃÄÅÄÄ´',0
                 db 'ÓÄÐÄÄ½ÔÍÏÍÍ¾ÈÍÊÍÍ¼ÀÄÁÄÄÙ',0
    .math:       db '³xüúëxðäûxýü',0
    .final1:     db 'Char. ', 0,  '   H/V Alignment:', 0
    .final2:     db '#___  ', 0,  'H:',219,0, "0iImTwZ"    ; 7
    .final3:     db 'Tiled:', 0,  'V:',0, 'xXyd_"$ '       ; 8

; Attribute pointer + ASCIIZ string- - - - - - - - - - - - - - - - - - - - - -

  attstr_status_bar:
    dw att.prog_name
    db 'Fontraption ',0
    dw att.version
    db ' v1.11',0
    dw att.ver_separator
    db ' þ ',0
    dw att.ver_date
    db '09/2020 ',0

; Some dual-screen locations, no strings attached- - - - - - - - - - - - - - -

  loc_tab1:        db  1, 0
  loc_tab2:        db 22, 0
  loc_tab1unsaved: db 19, 1
  loc_tab2unsaved: db 40, 1
  loc_cap_line:    db 43, 10
  loc_pixel00:     db  3, 5

; Location+attr+ASC0 strs (use locate_n_draw_x2, AX = draw_attr_asc0)- - - - -

  loc_attstr_esc:
    db 44, 8
    dw att.key_special
    db '[ESC] to exit',0
  loc_attstr_editbox_cap:
    db 53, 10
    dw att.caption
    db ' EDIT CHARACTER ',0
  loc_attstr_fontbox_cap:
    db 52, 10
    dw att.caption
    db ' SELECT CHAR/RANGE ',0

; Ellipsis locations (use w/locate_n_draw_x2, AX = draw_ellipsis)- - - - - - -

  loc_ellipsis:
    db 51,1,  51,2,  54,4,  53,5,  53,6,  74,1,  71,7,  71,8,  73,13,  69,21
    ellipsis_count = 10

; FORMATTED STRINGS - Markup:- - - - - - - - - - - - - - - - - - - - - - - - -

  ; bit 7 = 0?  Normal character;  1? CONTROL CODE:
  ; bit 6 = 0?  Set normalRight;   1? Set keyHighlight.   Check bit 5:
  ; bit 5 = 0?  Draw brackets (bits0-3) cells apart, set keyHighlight, NEXT
  ;         1?  Just skip (bits0-3) spaces ahead; NEXT
  ; 00h = String terminator
  ; 01h = Escape next character (interpret as ASCII, not a control code)
  ; '/' = Display but use special attribute keySlashes

    KEY    equ 80h or 40h            ; Set keyHighlight
    SKP    equ 80h or 20h            ; Skip ahead (OR w/number of spaces)
    SNK    equ 80h or 40h or 20h     ; Skip'n'Key - "; combines the two above
    BK1    equ 80h or 1              ; Brackets w/1 space inside
    BK2    equ 80h or 2              ;      "     2
    BK3    equ 80h or 3              ;      "     3
    SPC    equ SKP or 1              ; Single space (skipped, not drawn!)
    SPC2   equ SKP or 2              ; Double space (skipped, not drawn!)
    SNK1   equ SNK or 1
    SNK2   equ SNK or 2

 ; No location:

    formatted_number: db BK1,'1',0   ; Drawing tab 2? inc the '1' on-screen

 ; Formatted w/location (use w/locate_n_draw_x2, AX = draw_formatted_asc0):

  loc_formatted:
       db 44, 1, BK2,'^S',SPC,'ave',(SNK or 5),'F2',0
       db 44, 2, BK2,'^L',SPC,'oad',(SNK or 5),'F3',0
       db 44, 4, BK2,'^G',SPC,'et VGA',0
       db 44, 5, BK2,'^I',SPC,'mport',0
       db 44, 6, BK2,'^E',SPC,'xport',0
       db 64, 1, 'Set height',(SNK or 3),'F4',0
       db 64, 8, 'Preview',(SNK or 5),'F10',0
  .fP: db 64, 7, 'Palette',(SNK or 6),'F9',0
       db 44,12, 'Move',SNK1,ARROWS,0
       db 44,15, 'Mark',SNK1,'LShift+',ARROWS,0
       db 44,17, BK2,'^A',SPC2,'Select all',0
       db 64,12, 'Char/Font',(SNK or 3),'TAB',0
       db 64,13, 'Goto char',(SNK or 5),'G',0
       db 64,15, 'Prev/Next',(SNK or 3),'-/+',0
       db 64,18, 'Erase',(SNK or 9),'E',0
       db 64,19, 'Fill',(SNK or 10),'F',0
       db 64,20, 'Invert',(SNK or 8),'I',0
       db 64,21, 'Slide',(SNK or 9),'S',0
       db 64,22, 'Flip',(SNK or 8),'X/Y',0

 ; Redrawn on TAB (depend on current box):

  loc_f_editbox:
       db 44,13, 'Draw',SNK1,'Space/',17,1,0C4h,1,0D9h,0
       loc_formatted_count = 20

  loc_f_fontbox:
       db 44,13, 'Edit',0

 ; 'Stateful' elements can be either enabled or disabled; see update_stateful

  loc_f_stateful:
       .drag_key= $+7 ; replace "RShift" with "Ctrl" if supported in BIOS
       .drag:     db 44,14, 'Drag',SNK1,'RShift+',ARROWS,0
       .guide:    db 64,17, 'Guide',(SNK or 7),'|/_',0
       .unmark:   db 44,18, BK2,'^D',SPC2,'Deselect',0
       .cut:      db 44,19, BK2,'^X',SPC2,'Cut',0
       .copy:     db 44,20, BK2,'^C',SPC2,'Copy',0
       .paste:    db 44,21, BK2,'^V',SPC2,'Paste',0
       .revert:   db 44, 3, BK2,'^R',SPC,'eload',0
       .undo:     db 44,22, BK2,'^Z',SPC2,'Undo',0
       .8_9_dot:  db 64, 6, '8/9-dot cell',SNK1,'F8',0
       .dup_ln:   db 64, 4, 'Dup line',(SNK or 5),'F7',0
       .ins_ln:   db 64, 3, 'Insert line',SNK2,'F6',0
       .del_ln:   db 64, 2, 'Delete line',SNK2,'F5',0
       .swap:     db 64,14, 'Swap chars',(SNK or 4),'W',0
       loc_f_stateful_count = 13

;-----------------------------------------------------------------------------
;###############################  VARIABLES  #################################
;-----------------------------------------------------------------------------
align 2

; INITIALIZED . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  preview_attr:
    .regular:      db 06h
    .inverse:      db 60h
  pvw_txtlen:      dw 40
  seg_vga:         dw 0A000h
  seg_screen:      dw 0B800h
  seg_bios:        dw 0F000h

; UNINITIALIZED . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  seg_fseg:        dw ?                    ; init to CS+1000h
  seg_upper:       dw ?                    ; init to CS+1B00h
  seg_top:         dw ?                    ; init to CS+2200h
  pal_list:        times NUM_PALETTES dw ? ; table of palette pointers

  bof_init0 = $  ;++++++++++++++++++ BEGIN AREA TO ZERO-INIT ON STARTUP ++++++

  arg1_ptr:        dw ?            ; for filenames entered on command line
  arg2_ptr:        dw ?
  scratch:         times 8192 db ? ; scratch space for temp ops
  action_msg:      times 32   db ? ; and another one for file action messages

  state:                           ; -------- "*" = keep high byte 0 ---------
   .started:       db ?            ; editor started? (1=yes)
   .currfont:      db ?            ; current user font: 0=font1, -1=font2
   .currfont_ptr:  dw ?            ; POINTER to active font's structure
   .screen:        dw ? ;*         ; current screen (EDITOR)
   .preview:       dw ?            ; HI=curr, LO=backup / 0=no, 1=80c, -1=40c
   .clock_mode_80: db ?            ; for flipping 9/8-dot mode (test bit 0)
   .pal_attrmap:   dw ? ;*         ; number of attr_map for current palette
   .palette:       dw ? ;*         ; current palette
   .currbox:       db ?            ; active box: 0=edit box, 1=charset box
   .currchar:      dw ? ;*         ; current selected character (both fonts)
   .hoverchar:     dw ? ;*         ; character under cursor in font box
   .guide_cols:    db ?            ; guide columns (8-bit mask)
   .guide_rows:    times 32 db ?   ; guide rows: 0=clear, FF (not 0)=set
   .chrmark_rng_h: dw ?            ; editbox mark cols; from (HI), to (LO)
   .chrmark_rng_v: dw ?            ; editbox mark rows; from (HI), to (LO)
   .chrmark_mask:  db ?            ; editbox mark cols, 8bit mask  ]__ <must
   .chrmark_rows:  times 32 db ?   ; editbox mark rows, 8bit masks ]   <AND!
   .fntmark:       db ?            ; font mark (selection): 0/1 (off/on)
   .fntmark_start: db ?            ; font mark (selection): First char
   .fntmark_end:   db ?            ; font mark (selection): Last char
   .cursor_mask:   db ?            ; editbox cursor: h. position (bit-mask)
   .cursor_pos:    dw ?            ; editbox cursor position: LO=col, HI=row
   .drag:          dw ?            ; LO=dragging? (0/1); HI=draggee (0/1)
   .can_revert:    dw ?            ; set by update_stateful for CURRENT font
   .can_copy:      dw ?            ; set by update_stateful for CURRENT font
   .can_paste:     dw ?            ; set by update_stateful for CURRENT font
   .clip_type:     db ?            ; 0: character (partial/full), 1: range
   .clip_len:      dw ?            ; clip type 1: number of characters
   .clip_dim:      dw ?            ; clip type 0: #rows (HI), #cols (LO)
   .clip_mask:     db ?            ; clip type 0: 8-bit column mask
   .is_bad_vga:    db ?            ; detectd DOSBox, but machine!=vgaonly
   .int16h_func:   db ?            ; 10h if supported by BIOS, otherwise 0
   .drag_flag:     db ?            ; 1=LShift, 2=Ctrl (if int16_func==10h)
   .dragged_last:  db ?            ; was previous cursor move a drag op? (0/1)
   .tw:            dw ?            ; another word of temporary storage

  fdlg:                            ; ---------- FILE DIALOG STATE ------------
   .actkey:        dw ?            ; key code corresponding to action
   .in_len:        dw ? ;*         ; current length of input
   .count:         dw ?            ; number of files found
   .pagebase:      dw ?            ; number of first file on current page
   .fnum_rel:      dw ?            ; number of selected file (rel.to pagebase)
   .widget:        dw ?            ; proc: jump here when retriying input
   .query:         times 128 db ?  ; final query string to DOS; path+spec
   .input:         times 128 db ?  ; user input (actual limit: ILEN-2)
   .path:          times 128 db ?  ; path we're looking at (supplied)
   .truepath:      times 128 db ?  ; path we're looking at (truenamed)
   .lastgood:      times 128 db ?  ; last known GOOD truepath we've seen
   .drive_a:       db ?            ; is drive A available? (0/1)
   .drive_b:       db ?            ; is drive B available? (0/1)
   .faildrv:       dw ?            ; blacklist this drive: "A:" or "B:"
   .focus:         db ?            ; focus: 0=text input, 1=directory list
   .allow_new:     db ?            ; new file creation allowed? 0=no, 1=yes
   .dirs_only:     db ?            ; 1=input ends w/ '\' (never treat as file)
   .too_long:      db ?            ; number of files in dir exceeds MAX_FILES
   .export_fmt:    dw ? ;*         ; export format: 0=COM, 1=BMP, 2=XBIN
   .export_com:    dw ? ;*         ; COM type: 0=none, 1=40c, 2=80c, 3=both

  align 2
  font1 font                       ; user font the first
  font2 font                       ; user font the second

  eof_init0 = $  ;++++++++++++++++++ END AREA TO ZERO-INIT ON STARTUP ++++++++

  editbox_ON_att   edbox_attrs     ; box_ON (active) attributes + bit 3
  editbox_OFF_att  edbox_attrs     ; box_OFF (inactive) attributes + bit 3
  .end = $

;-----------------------------------------------------------------------------
;##############################  FILES SEGMENT  ##############################
;-----------------------------------------------------------------------------
org 0                              ; CS+1000h -> [seg_fseg]

fseg:
  .fname_ptrs:    times 4096  db ?
  .aux:           times 4096  db ?
  .fnames:        times 32768 db ?
  .pos:           times 80    dw ?
  .end = $        ; :B000

;-----------------------------------------------------------------------------
;##############################  UPPER SEGMENT  ##############################
;-----------------------------------------------------------------------------
org 0                              ; CS+1B00h -> [seg_upper]

upper:
  .clipboard:     times 8192  db ?
  .font1_undo:    times UNDO  db ?
  .font2_undo:    times UNDO  db ?
  .end = $        ; :6268 -> :7000

;-----------------------------------------------------------------------------
;###############################  TOP SEGMENT  ###############################
;-----------------------------------------------------------------------------
org 0                              ; CS+2200h -> [seg_top]

top:
  .8x14:          times 8192  db ?
  .9x14:          times 8192  db ?
  .8x8:           times 8192  db ?
  .8x16:          times 8192  db ?
  .9x16:          times 8192  db ?
  .tmp:           times 6000h db ?
  .end = $        ; :FFFF