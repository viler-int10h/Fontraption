  ;++++++++++++++++++++++++++ FONT BANK OPERATION ++++++++++++++++++++++++++;
  ;                                                                         ;
  ; CHARSETS IN DUAL FONT MODE:                                             ;
  ;                                                                         ;
  ;   Editor UI font    = charset A (attr bit3 = 1); high FG colors         ;
  ;   Current user font = charset B (attr bit3 = 0); low FG colors          ;
  ;                                                                         ;
  ; MAP OF VGA PLANE 2:                                                     ;
  ;                                                                         ;
  ;   FONT:                           ADDR:         CHARSET SELECT:         ;
  ;   Editor UI font 8x16 (25 rows)   A000:0000h    000b                    ;
  ;   Editor UI font 8x8  (50 rows)   A000:4000h    001b                    ;
  ;   User font 1 (any size)          A000:8000h    010b                    ;
  ;   User font 2 (any size)          A000:C000h    011b                    ;
  ;   UI font for preview (any size)  A000:2000h    100b                    ;
  ;                                                                         ;
  ; POSSIBLE STATES OF CHARMAP SELECT REG (3C4h IDX 3)                      ;
  ;                                                                         ;
  ;   CURR. USER FONT:   ROWS:  CHARSETS:         ABAABB (REG VALUE):       ;
  ;   #1, =< 16 lines    25     A=000b, B=010b    000010b = 2               ;
  ;   #1,  > 16 lines    50     A=001b, B=010b    000110b = 6               ;
  ;   #2, =< 16 lines    25     A=000b, B=011b    000011b = 3               ;
  ;   #2,  > 16 lines    50     A=001b, B=011b    000111b = 7               ;
  ;                                                                         ;
  ;   IN PREVIEW MODE - CHARSET A=100b, B=(keep): 1000??b REG VALUE         ;
  ;   User font height < 16? - duplicate 8x8  UI font + padding             ;
  ;   User font height >=16? - duplicate 8x16 UI font + padding             ;


;-----------------------------------------------------------------------------
  vga_setup_all: ; Do all the nasty tweaks before starting the editor
;-----------------------------------------------------------------------------
  ; Start with a nice and standard 80x25 mode

    mov    ax, 3
    int    10h

  ; Dump VGA ROM fonts to top segment (conveniently keeps 8x16 font active)

    push   es  ;1;
    push   ds  ;2;

    call   vga_init_font_IO    ; set up VGA state for font I/O
    mov    es, [seg_vga]       ; first let's wipe out ALL of plane 2, or we'll
    xor    ax, ax              ;    get garbage in fun and unexpected places
    mov    cx, 8000h           ;    (VGA BIOS doesn't clean up after itself)
    xor    di, di
    rep    stosw
    call   vga_end_font_IO     ; restore VGA state to normal operation

    push   es  ;3;
    pop    ds  ;2;             ; (source: A000h)
    mov    es, [cs:seg_top]
    mov    bp, vga_dump_font   ; save some bytes, eh?
    xor    di, di              ; start @ offset 0 for 8x14 ('monochrome') font
    xor    bl, bl              ; block specifier 000b (target = A000:0000)
    mov    ax, 1101h           ; LOAD ROM MONOCHROME PATTERNS (PS,EGA,VGA)
    int    10h
    call   bp                  ; ...8x14 font to top.8x14
    call   bp                  ;    ...again, to top.9x14 (for later tweaking)
    mov    ax, 1102h           ; LOAD ROM 8x8 DBL-DOT PATTERNS (PS,EGA,VGA)
    int    10h
    push   di  ;3;
    push   di  ;4;
    call   bp                  ; ...8x8 font to top.8x8
    pop    di  ;3;             ; ...(gotta clean this one up afterwards)
    mov    bx, 8
    call   font_cleanup.v
    xor    bl, bl              ; reset block specifier; fixes ET4K (+others?)
    mov    ax, 1104h           ; LOAD ROM 8x16 CHARACTER SET (VGA)
    int    10h
    call   bp                  ; ...8x16 font to top.8x16
    call   bp                  ;    ...again, to top.9x16 (for later tweaking)

  ; Copy ROM 8x8 font to font RAM (for 50-line UI) so we can customize glyphs
    
    pop    si  ;2;  ;******    ; start with top.8x8
    mov    di, VRAM_UI8_FONT
    push   ds
    push   es
    pop    ds                  ; source: seg_top
    pop    es                  ; target: seg_vga
    call   vga_dump_font.r     ; reverse-"dump" it

  ; Fix 9-dot versions of our dumped fonts w/alternate wide chars

    mov    bh, 5               ; 05h = ROM 9x14 *alternates*
    call   vga_dump_widechars
    mov    bh, 7               ; 07h = ROM 9x16 *alternates*
    call   vga_dump_widechars
    
    pop    ds  ;1;
    pop    es  ;0;

  ; Disable blinking and cursor

    cli
    mov    dx, 3DAh            ; read port 3DA (status) to put port 3C0 in
    in     al, dx              ;    the index-accept (not data-accept) state
    mov    dl, 0C0h            ; Attribute address register
    mov    al, 30h             ; Attribute Mode Control (idx 10h) OR bit 5(!)
    out    dx, al
    mov    al, 00000100b       ; b7:   b6:   b5:   b4:  b3:   b2:  b1:   b0:
    out    dx, al              ; P54S  8BIT  PPM   ---  BLINK LGE  MONO  GFX
    call   vga_set_cursor.off
    sti

  ; Are we in DOSBox with a non-"vgaonly" machine?
  
    push   es  ;1;
    mov    es, [seg_bios]      ; scan BIOS area for "DOSBox" signature
    mov    di, SIG_AREA_OFFS   ; DI -> area to search in
    mov    dx, SIG_AREA_LEN    ; DX =  how many bytes to search
    xor    ax, ax              ; AL =  detection flag (init zero)
  .scan:      
    mov    si, txt.dosbox      ; what are we looking for again?
    mov    cx, SIG_LEN         ; and how long is it?
    repe   cmpsb               ; check for a match
    jne    @f                  ; not found? - try again
    inc    ax                  ; AL = 1? got DOSBox
    jmp    short .machine
@@: dec    dx                  ; one less byte to look through
    jnz    .scan               ; are we there yet?
  .machine:  
    pop    es  ;0;
    xchg   ax, cx              ; move flag to CL
    or     cl, cl
    jz     .clock              ; not in DOSBox: skip machine check
    mov    ax, 1203h           ; yes? set an invalid vertical resolution
    mov    bl, 30h
    int    10h
    cmp    al, ah              ; supported (AL=12h)?
    jne    @f                  ; - no: bad VGA machine; keep problem flag set
    dec    cx                  ; - yes: we have machine=vgaonly; clear flag
@@: mov    al, 2               ; set a sane resolution again, just in case
    int    10h

  ; Get clock mode, for value of 9/8-dot flag

  .clock:
    mov    dx, 3C4h                      ; Sequencer (address) port
    mov    al, 1                         ; Clocking mode register (index)
    out    dx, al
    inc    dx                            ; Sequencer (data) port
    in     al, dx
    or     cl, cl                        ; check DOSBox-VGA detection result
    jz     @f                            ; - 0? all clear
    or     al, cl                        ; - 1? force 8-dot/char clock mode 
    inc    byte[state.is_bad_vga]        ;      + indicate problem
@@: mov    [state.clock_mode_80], al

  ; Modify UI fonts and upload our two user fonts

    call   vga_mod_UI_glyphs               ; Inject 2 UI fonts w/custom glyphs
    mov    word[state.currfont_ptr],font2  ; Start w/font 2
    stc                                    ; On-screen vertical centering ON
    call   vga_upload_font
    mov    word[state.currfont_ptr],font1  ; Then font 1
    stc                                    ; Vertically center this one too
    call   vga_upload_font

  ; Line up VGA palette entries so they're straight 0..15, not EGA-like

    mov    dh, 3
    mov    cx, 16
@@: mov    dl, 0DAh            ; Read port 3DA (status) to put port 3C0 in
    in     al, dx              ;    the index-accept (not data-accept) state
    mov    dl, 0C0h            ; Attribute address register
    jcxz   @f                  ; Value will be CL-1; so break if CX=0
    mov    al, cl
    dec    ax                  ; AL=CL-1, to set registers 15..0
    out    dx, al              ; Index byte
    out    dx, al              ; =Data byte
    loop   @b                  ; Done? - after the last extra 3DA read, set
@@: mov    al, 20h             ;     bit 5 of attribute controller address
    out    dx, al              ;     so VGA can access palette again

   ; Done setting up VGA - now we can fall through to:

;-----------------------------------------------------------------------------
  vga_set_palette: ; Find appropriate attrMap for new palette + SET palette
;
; In:      [state.palette] = palette number
;-----------------------------------------------------------------------------
    mov    bx, [state.palette]           ; High byte 0
    shl    bx, 1
    mov    dx, [pal_list+bx]
    call   vga_set_DAC                   ; ES:DX -> table of 3*8 bytes (R,G,B)
    mov    bx, dx
    mov    al, PAL_GETMAP
    xlatb                                ; AL = attribute map index
    mov    [state.pal_attrmap], al

  ; Generate editbox (UI font) attributes:

    mov    si, att.box_ON_content        ; let's start here
    cbw                                  ; AL (attrMap index) always below 80h
    add    si, ax                        ; snap into column
    mov    di, editbox_ON_att            ; our modded attributes will go here

    mov    cx, 8                         ; 8 attributes to be converted
.x: lodsb
    call   .mod_it                       ; swap nybbles to AH, set b3 in both
    stosb                                ; 'px0' version
    xchg   al, ah
    stosb                                ; 'px1' version
    add    si, (NUM_ATTRMAPS-1)          ; next attr in current palette's map
    cmp    cl, 5
    jne    @f
    add    si, NUM_ATTRMAPS*2            ; skip those 2 middle rows (CAREFUL!)
@@: loop   .x
    ret

  .mod_it:
    push   cx
    mov    bl, al                        ; IN: original attribute in AL
    mov    ah, al
    mov    cl, 4
    shr    ah, cl                        ; new right nybble
    shl    bl, cl                        ; new left nybble
    or     ah, bl                        ; AH = nybble-swapped AL
    or     ax, 808h                      ; set bit 3 for both
    pop    cx
    ret

;-----------------------------------------------------------------------------
  vga_set_clock_mode: ; Toggle 8/9 dots/cell or 40/80 column mode
;
; In:   state.preview - says if this is preview-related. If so, the switch is
;                       40-col/80-col; otherwise switch 8/9 dots/cell
;-----------------------------------------------------------------------------
    mov    si, state.clock_mode_80       ; Grab our value
    mov    di, si
    lodsb
    test   byte[state.preview+1], 0FFh   ; We goin' in/out of preview?
                                         ;     (high byte = *current* state)
    pushf  ;1;                           ;     hold that thought
    jz     .toggle_8_9                   ; Nope, go play with dots
    jns    .do_it                        ; Yep: bit 7 clear = 80c, use as-is
    or     al, 1000b                     ;              set = 40c, change it
    jmp    short .do_it                  ;     don't save anything!
.toggle_8_9:
    xor    al, 1                         ; Toggle bit 0 (8/9-dot mode)
    stosb                                ;   ...DO save it here
.do_it:
    mov    cl, al                        ; Put it aside
    mov    dx, 3C4h                      ; Sequencer (address) port
    mov    ax, 0100h                     ; 00 - Synchronous reset: SET
    out    dx, ax
    mov    ah, cl                        ; Grab our value again
    inc    ax                            ; 01 - Clocking mode register
    out    dx, ax
    popf   ;0;                           ; are we switching dot modes?
    jnz    @f                            ;  - no, just clear reset state
                                         ; if we are, flip Clock Select too:
    mov    dl, 0CCh                      ; Miscellaneous Output Register
    in     al, dx                        ;    (read address)
    and    al, 0FBh                      ; isolate bit 2 (clock select)
    xor    cl, 1                         ; flip bit 0 in our value
    shl    cl, 1
    shl    cl, 1
    and    cl, 4                         ; ...to bit 2 (and isolate it)
    or     al, cl
    mov    dl, 0C2h                      ; Miscellaneous Output Register
    out    dx, al                        ;    (write address)
    mov    dl, 0C4h                      ; Sequencer (address) port again     
@@: mov    ax, 0300h                     ; 00 - Synchronous reset: CLEAR
    out    dx, ax
    ret

;-----------------------------------------------------------------------------
  vga_upload_font = _start_UF ; Upload ACTIVE user font to respective VGA bank
;
; In:  CF = Displace (to vertically center <16-line fonts)? Clear=no; set=yes
;-----------------------------------------------------------------------------
@@: clc
    _start_UF = $
    mov    si, [state.currfont_ptr]      ; SI-> font structure
    mov    al, 0                         ; Preserve CF please
    jnc    .o                            ; CF clear? Don't displace
    lodsb                                ; AL = height (byte 0 of font struct)
    dec    si                            ; SI-> font structure, I said
    cmp    al, 16
    jae    @b                            ; Don't displace >=16-line fonts
    sub    al, 16
    sar    al, 1                         ; AL = -padding (16-height)/2 + 1(^*)
.o: mov    di, VRAM_FONT1                ; User font 1 goes here
    cmp    si, font2                     ; Should we do user font 2 instead?
    jb     @f
    mov    di, VRAM_FONT2                ; Yep - user font 2 goes here
@@: cbw
    xchg   bx, ax                        ; BX = negative(!) padding
    lea    si, [si+bx+font.data]         ; SI-> *padded* (displaced) font data
    push   es
    mov    es, [seg_vga]
    call   vga_init_font_IO              ; Set up VGA state for font I/O
    mov    cx, 4096
    rep    movsw                         ; DO IT
    call   vga_end_font_IO               ; Restore VGA state to normal I/O
    pop    es
    ret

;-----------------------------------------------------------------------------
  vga_mod_UI_glyphs:  ; Add custom glyphs for UI elements (grids, arrows...)
;-----------------------------------------------------------------------------
    push   es
    mov    es, [seg_vga]                 ; prepare VGA RAM write: ES:DI->dest.
    mov    di, MODIFY_UI16               ; target offset in UI16 font

    call   vga_init_font_IO              ; set up VGA state for font I/O
    mov    ah, [state.clock_mode_80]
    mov    dx, 00AAh
    mov    bl, dl

    mov    cx, 2
  .do5chars:                   ;-----OUTER LOOP (x2: UI16 font & UI8 font)-- -
    push   cx                  ;
    mov    si, ui_8dot_grids   ;
    mov    cl, 5               ;
  .do1char:                    ;   ,-------MIDDLE LOOP (x4 chars)---------- -
    push   cx                  ;   ;
    mov    cl, 16              ;   ;
  .do_copy:                    ;   ;   ,----INNER LOOP (x16 bytes/char)--- -
    lodsb                      ;   ;   ;  read glyph scanline
    test   ah, 1               ;   ;   ;  are we in 9-dot or 8-dot mode?
    jnz    @f                  ;   ;   ;  - bit1=1: 8-dot; go on
    cmp    al, bl              ;   ;   ;  - bit1=0: 9-dot; needs transform?
    jne    @f                  ;   ;   ;          - not AAh: no, go on
    mov    al, 092h            ;   ;   ;          - yes, transform for 9 dots
    @@:                        ;   ;   ;
    stosb                      ;   ;   ;  write glyph scanline
    loop   .do_copy            ;   ;   `---------------------------------- -
    add    di, 16              ;   ; skip to next char pos in VGA RAM
    pop    cx                  ;   ;
    loop   .do1char            ;   `--------------------------------------- -
    pop    cx                  ;
    mov    byte[si-10], dh     ; modify tab_border character: in iteration #1,
    mov    byte[si-13], dl     ;     convert UI16 to UI8; in iteration #2,
    xchg   dl, dh              ;     <= make it UI16 again for future calls
    mov    di, MODIFY_UI8      ; target offset in UI8 font for iteration #2
    loop   .do5chars           ;-------------------------------------------- -

    mov    di, MODIFY_UI16+(5*32)        ; do the drive icon
    push   di
    mov    cl, 3*(32/2)
    xor    ax, ax
    rep    stosw
    pop    di
    mov    al, 3
@@: add    di, 3
    mov    cl, 9
    rep    movsb
    add    di, 32-12
    dec    ax
    jnz    @b

    call   vga_end_font_IO               ; restore VGA state to normal I/O

    pop    es
    ret

  ui_8dot_grids: ; To get the 9 dot glyphs, we modify AAh to 92h
                 ; Chars 201, 203 not needed in 50-line mode, because we
                 ;    only use one-cell grid squares there
                 ; Tab border char is modified in-place for the UI8 font

    db     0AAh, 00h, 80h, 00h, 80h, 00h, 80h, 00h        ; 200d (GRID1)
    db      80h, 00h, 80h, 00h, 80h, 00h, 80h, 00h
    db     0AAh, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0            ; 201d (GRID2)
    db     0FFh, 80h,0AAh, 80h,0AAh, 80h,0AAh, 80h        ; 202d (GUIDE1)
    db     0AAh, 80h,0AAh, 80h,0AAh, 80h,0AAh, 80h
    db     0FFh, 00h,0AAh, 00h,0AAh, 00h,0AAh, 00h        ; 203d (GUIDE2)
    db     0AAh, 00h,0AAh, 00h,0AAh, 00h,0AAh, 00h
    db     0,0,0,0,0,0, 0AAh, 0,0,0,0,0,0,0,0,0           ; 204d (TAB_BORDER)
    
    db     07Fh,060h,060h,060h,06Fh,060h,060h,060h,07Fh   ; drive icon
    db     0FFh,000h,000h,07Eh,0FFh,07Eh,000h,000h,0FFh
    db     0FEh,006h,036h,006h,0F6h,006h,006h,006h,0FEh

;-----------------------------------------------------------------------------
  vga_get_font_info:  ; Use the BIOS (ugh) to get font pointers and state
;
; IN:     BH = desired pointer:
;                 00h INT 1Fh pointer
;                 01h INT 43h pointer
;                 02h ROM 8x14 font
;                 03h ROM 8x8 font
;                 04h ROM 8x8 font (high 128 characters)
;                 05h ROM 9x14 *alternates*
;                 06h ROM 8x16 font
;                 07h ROM 9x16 *alternates*
; Out:    ES:BP = specified pointer
;         CX    = bytes/character of on-screen font (not the requested font!)
;         DL    = highest character row on screen
;-----------------------------------------------------------------------------
    mov    ax, 1130h
    int    10h
    ret

;-----------------------------------------------------------------------------
  vga_init_font_IO:  ; Set up VGA state for font reading/writing (plane 2)
;                      This and the next 2 routines modified + FIXED from:
;                      https://wiki.osdev.org/VGA_Fonts
;-----------------------------------------------------------------------------
    mov    dx, 03ceh
    mov    ax, 5                         ; Write mode 0, disable even/odd GFX
    out    dx, ax
    mov    ax, 0204h                     ; Read map: set plane 2 for reads
    out    dx, ax
    mov    ax, 0406h                     ; Map VGA mem to seg A000h
    out    dx, ax
    mov    dl, 0c4h
    mov    ax, 0402h                     ; Map mask: set plane 2 for writes
    out    dx, ax
    mov    ax, 0604h                     ; Disable even/odd mode in SEQ
    out    dx, ax
    ret

;-----------------------------------------------------------------------------
  vga_end_font_IO:  ; Restore VGA state to normal operation (planes 0, 1)
;-----------------------------------------------------------------------------
    mov    dx, 03c4h
    mov    ax, 0302h                     ; Map mask: set planes 0,1 for writes
    out    dx, ax
    mov    ax, 0204h                     ; Enable even/odd mode in SEQ
    out    dx, ax
    mov    dl, 0ceh
    mov    ax, 1005h                     ; Write mode 1? disable even/odd GFX?
    out    dx, ax
    mov    ax, 0304h                     ; Read mask: set planes 0,1 for reads
    out    dx, ax
    mov    ax, 0E06h                     ; Map VGA mem to seg B800h,
    out    dx, ax                        ;     chain odd/even, text mode
    ret

;-----------------------------------------------------------------------------
  vga_dump_font: ; Init font I/O, dump font, end font I/O
;
; In:  DS   =  seg_vga (A000h)
;      ES:DI-> offset @ seg_top (CS+2000h)
; Out: ES:DI->   "  + 8KB
;-----------------------------------------------------------------------------
    xor    si, si
.r: call   vga_init_font_IO              ; Set up VGA state for font I/O
    memcp  si, di, 4096                  ; copy the entire 8 KB font as-is
    call   vga_end_font_IO               ; Restore VGA state to normal I/O
    ret

;-----------------------------------------------------------------------------
  vga_dump_widechars:  ; Fix dumped 9-dot font w/alternate ROM chars
;
; In:  BH = 05h (9x14) or 07h (9x16); see vga_get_font_info
;
; Format of alternate font table [array]:
; Offset  Size    Description
; 00h     BYTE    character to be replaced (00h = end of table)
; 01h     N BYTEs graphics data for character, one byte/scanline
;-----------------------------------------------------------------------------
    call   vga_get_font_info             ; ES:BP -> alternate font table
    mov    dx, 7                         ; DX    =  lines/2 (WORDS) per char
    mov    di, top.9x14                  ; DI    -> destination in top_seg
    cmp    bh, dl
    jne    @f
    inc    dx
    mov    di, top.9x16
@@: push   es  ;1;
    pop    ds  ;0;
    mov    si, bp                        ; DS:SI -> alternate font table
    mov    es, [cs:seg_top]              ; ES:DI -> destination

@@: lodsb                                ; AL    =  char to be replaced
    cmp    al, 0                         ;          00h = end of table
    je     @f
    xor    ah, ah
    mov    cl, 5
    shl    ax, cl                        ; AL    =  char*32 = target offset
    push   di  ;1;                       ;          save offset of font start
    add    di, ax                        ; ES:DI -> target for char
    mov    cx, dx                        ; CX    =  WORDS per char
    rep    movsw                         ;          DUMP IT!
    pop    di  ;0;
    jmp    short @b                      ; NEXT
@@: ret                                  ; end of table? done here

;-----------------------------------------------------------------------------
  vga_dump_to_fnt2: ; Download current VGA RAM font to our second font buffer
;-----------------------------------------------------------------------------
    mov    di, font2
    push   di  ;1;
    mov    word[di+font.fspec], '?'      ; Name font 2 '?',0 (later <NoName>)

    push   ds  ;2;                       ; Get VGA font info (func. 1130h):
    push   es  ;3;
    xor    bx, bx                        ;     don't care about font ptrs now
    mov    [di+font.unsaved], bl         ;     unsaved = 0 while we're at it
    call   vga_get_font_info
    mov    [di], cl                      ;     returned height

    mov    ds, [seg_vga]
    pop    es  ;2;                       ; Set up buffer for data
    mov    di, font2.data
    call   vga_dump_font                 ; Do it

    pop    ds  ;1;
    pop    di  ;0;                       ; DI -> font2, CX = height
    call   font_cleanup                  ; Remove garbage in slack space!
    ret

;-----------------------------------------------------------------------------
  vga_set_DAC: ; Set the 16-color palette from two identical 8-color ranges
;
; In:       ES:DX -> table of 3*CX bytes (R,G,B)

; When in dual-font mode, foreground bit 3 (CGA intensity) selects the font,
; so we compose our 16-color palettes out of identical 8-color halves.
; BIOS call since it's easier and we don't particularly care about speed here.
;-----------------------------------------------------------------------------
    mov    ax, 1012h                     ; SET BLOCK OF DAC REGISTERS
    push   ax  ;1;
    xor    bx, bx                        ; BX = starting color register
    mov    cx, 8                         ; CX = number of registers to set
    int    10h
                                         ; fix some VGA BIOSes which like
    pop    ax  ;0;                       ;     to corrupt AX (Tseng 4000?)
    mov    bx, cx                        ; 8 more
    int    10h
    ret

;-----------------------------------------------------------------------------
  vga_place_cursor: ; Places cursor wherever DI is pointing at, and ENABLES it
;-----------------------------------------------------------------------------
    mov    dx, 3d4h                      ; 3D4 - CRTC
    mov    ax, di                        ; DI should come back in one piece
    shr    ax, 1                         ; Byte count -> character count
    push   ax
    mov    al, 0Eh                       ; 0E = Cursor Location High
    out    dx, ax
    pop    ax
    mov    ah, al
    mov    al, 0Fh                       ; 0F = Cursor Location Low
    out    dx, ax                        ; ...*and* fall through to CURSOR_ON:

;-----------------------------------------------------------------------------
  vga_set_cursor: ; Sets cursor to full block, toggles on/off depending on AH
;-----------------------------------------------------------------------------
  .on:
    xor    ah, ah                        ; CURSOR_ON = 0
    jmp    short @f
  .off:
    mov    ah, CURSOR_OFF
@@: mov    dx, 3d4h                      ; 3D4 - CRTC
    mov    al, 0Ah                       ; 0A = Cursor Start: b.0-4 = scanline
    out    dx, ax                        ;                    b.  5 = DISABLE
    mov    ax, 1F0Bh                     ; 0B = Cursor End:   b.0-4 = scanline
    out    dx, ax                        ;                    b.5-6 = skew (0)
    ret

;-----------------------------------------------------------------------------
  vga_init_preview:  ; Set up CRTC stuff for preview and switch to it
;
; In:  BP = font height
;      byte[state.preview+1] = columns setting: 1 for 80col, -1 for 40col
;-----------------------------------------------------------------------------
    _COLS  equ byte[state.preview+1]

  ; First let's calculate some register values....

    mov    di, scratch         ; DI -> Computed values for word-outs go here

    mov    dx, 3d4h            ;       3D4 - CRTC address
    push   dx  ;2;
    mov    ah, 07h
    mov    al, ah              ;       07h - Overflow reg
    out    dx, al
    inc    dx                  ;       3D5 - CRTC data
    in     al, dx              ;       read current contents
    xchg   ah, al
    stosw                      ; SW0 = r07h (OVERFLOW), current contents
    xchg   ax, cx              ; CX  = same; keep it so we can mess with it
    push   di  ;1;

    dec    dx                  ;       3D4 - CRTC address
    mov    ah, 11h
    mov    al, ah              ;       11h - V. retrace end + protect bit
    out    dx, al
    inc    dx                  ;       3D5 - CRTC data
    in     al, dx              ;       read current contents
    and    al, 01111111b       ;       ABOLISH SAFE SPACE
    xchg   al, ah
    stosw                      ; SW1 = r11h (VERTICAL RETRACE END, UNPROT.)

    mov    bx, bp
    mov    ax, 100
    cmp    bl, 4
    jb     @f
    mov    ax, 400
    div    bl
@@: mov    dh, al              ; DH  = height<4  ? 100 : 400/height

    cmp    bl, 5
    jnb    @f
    xor    dl, dl              ; DL  = height<5  ?   0 :
@@: cmp    bl, 21
    jna    @f
    mov    dl, 41              ;       height>21 ?  41 :
    jmp    short .a
@@: mov    dl, 101
    sub    dl, dh              ;
    shr    dl, 1               ;       else: (101-DH)/2

.a: mov    ax, bp              ;       Max scanline = font_height - 1
    dec    ax                  ;       (bits 0-4; 5-7 always 0!)
    mov    ah, 09h
    xchg   al, ah
    stosw                      ; SW2 = r09h (MAXIMUM SCANLINE)

    mov    al, dh
    mul    bl
    dec    ax                  ;       VDE = height*DH - 1
    push   ax  ;3;
    test   ah, 1               ;       bit 8 of VDE = 1?
    jz     @f
    or     ch, 10b             ;       - Yes: bit 1 of Overflow = 1
    jmp    short .b
@@: and    ch, 11111101b       ;       - No:  bit 1 of Overflow = 0
.b: mov    ah, 12h
    xchg   al, ah
    stosw                      ; SW3 = r12h (VERTICAL DISPLAY END, b0-7)

    pop    ax  ;2;             ;       VDE... again
    sub    ax, bp              ;       Line Compare = VDE-height
    test   ah, 1               ;       bit 8 of LC = 1?
    jz     @f
    or     ch, 10000b          ;       - Yes: bit 4 of Overflow = 1
    jmp    short .c
@@: and    ch, 11101111b       ;       - No:  bit 4 of Overflow = 0
.c: mov    ah, 18h
    xchg   ah, al
    stosw                      ; SW4 = r18h (LINE COMPARE, b0-7)

    xchg   ax, cx
    stosw                      ; SW5 = r07h (OVERFLOW) - the *new* value

    mov    al, 80              ;       Start address stuff now
    mul    dl
    add    ax, VRAM_PVW_AREA/2 ;       DL*80 + start of 100-line virtual page
    test   _COLS, 0FFh
    jns    @f                  ;       SF clear = 80 columns
    add    ax, 20              ;       40 columns? - add 20 to start address
@@: mov    bl, al
    mov    al, 0Ch
    stosw                      ; SW6 = r0Ch (START ADDRESS HIGH)
    inc    ax
    mov    ah, bl
    stosw                      ; SW7 = r0Dh (START ADDRESS LOW)

  ; Calculate padding (and source) for copy of UI font

    xor    si, si              ; (VRAM_UI16_FONT = 0)
    mov    ax, 16              ; but do we use that one, or UI8_FONT?
    mov    cx, bp              ; ah... that depends on the USER font's height
    cmp    cl, al              ; is it 16 or more?
    jae    @f                  ; - yep:  keep the tall one
    mov    si, VRAM_UI8_FONT   ; - nope: switch to the stumpy version
    mov    al, 8
@@: sub    cx, ax
    jns    @f                  ; got a negative number? - reset to 0
    sar    cx, 1               ;     fake "negative padding"
    sub    si, cx
    xor    cx, cx              ; padding = user font height, minus
@@: shr    cx, 1               ;     preview font height, / 2

  ; Commence duplication... with padding!

    mov    di, VRAM_UIPVW_FONT
    call   vga_init_font_IO
    push   ds  ;3;
    push   es  ;4;
    mov    ds, [seg_vga]       ; VGA plane 2 is both source and target
    push   ds  ;5;
    pop    es  ;4;
    xor    al, al              ; set up some nice blank lines
    rep    stosb               ; write them out
    mov    cx, 256*16          ; copying within VRAM is *slow*, but we'll live
    rep    movsw
    pop    es  ;3;
    pop    ds  ;2;

  ; Upload current user font... WITHOUT padding

    clc                        ; DO. NOT. PAD.
    call   vga_upload_font     ; (ends font I/O, too)

  ; Update font map with our shiny new UI font, too

    mov    dl, 0C4h            ; Sequencer (address) port
    mov    ax, 0303h           ; r03h : CHARACTER MAP SELECT
    out    dx, al
    inc    dx                  ; Sequencer (data) port
    in     al, dx              ; get whatever's in there now
    dec    dx                  ; data port again
    and    al, CHRMAP_PVW_CLR  ; twiddle some bits
    or     al, CHRMAP_PVW_SET  ; twiddle some other bits
    xchg   ah, al              ; swap them bytes
    out    dx, ax              ; and hope for the best

  ; Output everything to VGA registers

    pop    si  ;1;             ; Stored values/indices, starting at SW+1 (!!!)
    pop    dx  ;0;             ; 3D4 - CRTC (address) port
    mov    cl, 7               ; ...that's how many we have
    cli
@@: lodsw
    out    dx, ax
    loop   @b
    sti

  ; Now... do we need to switch to 40 or 80 columns?

    mov    si, go_40col        ; grab those values
    test   _COLS, 0FFh         ; 
    js     @f                  ; SF set? - +++++ GO 40-COLUMN +++++
    mov    si, go_80col        ;  clear? - +++++ GO 80-COLUMN +++++
@@: call   vga_crtc_horiz_out  ; send 'em all out
    call   vga_set_clock_mode  ; do the ugly bits
    ret                        ; get ye back

;-----------------------------------------------------------------------------
  vga_end_preview:  ; Bring CRTC back to something resembling sanity
;
; In:  [scratch] = original overflow + VRE regs
;-----------------------------------------------------------------------------
    mov    si, scratch
    mov    dx, 3D4h                      ; CRTC address
    push   dx  ;1;            
    lodsw                                ; r07h : OVERFLOW (original value)
    push   si  ;2:            
    out    dx, ax             
    mov    ax, 8F12h                     ; r12h : VERTICAL DISPLAY END
    out    dx, ax

  ; Go back to sensible 80-column values while these regs are unprotected

    mov    bx, state.preview
    mov    ax, [bx]
    push   ax
    mov    byte[bx+1], 1                 ; a bit of fakery here...
    mov    si, go_80col                  ; where's that list of values?
    call   vga_crtc_horiz_out            ; send 'em all out
    call   vga_set_clock_mode            ; BACK TO THE 80's
    pop    ax
    mov    [bx], ax

  ; Re-protect the SNOWFLAKE registers and get out

    pop    si  ;1;                       ; SW1
    pop    dx  ;0;                       ; CRTC address
    cli                                  
    lodsw                                ; r11h : VERTICAL RETRACE END
    or     ah, 10000000b                 ; set bit 7 (CRTC protect bit)
    out    dx, ax                        ; no need to worry about the other 
    sti                                  ;     registers - the next routines 
    ret                                  ;     called will deal with them

;-----------------------------------------------------------------------------
  vga_crtc_horiz_out:  ; Update CRTC horizontal control regs (0-5)

; In:  SI-> list of wanted values (6 bytes)
;      DX = 3D4h (CRTC address)
;      b7 of r11h (CRTC protect bit) CLEAR
;-----------------------------------------------------------------------------
    cli
    mov    cl, 6
    xor    ax, ax                        ; Start w/register index 0
@@: lodsb                                ; Get value
    xchg   al, ah                        ; Swap: AL=index, AH=value
    out    dx, ax                        ; Do it
    inc    ax                            ; Next index
    xchg   al, ah                        ; Swap: AL=value, AH=index
    loop   @b                            ; NEXT!
    sti
    ret

  go_80col:        db 05Fh, 04Fh, 050h, 082h, 055h, 081h;__________
  go_40col:        db 02Dh, 027h, 028h, 090h, 02Bh, 0A0h;          \
  ;         ___________/     /     |      \__     \______           \
  ;        /            ____/      |         \           \          End
  ;    Horiz.          /        Start        End        Start      Horiz.
  ;    Total     End Horiz.     Horiz.      Horiz.      Horiz.    Retrace
  ;               Display      Blanking    Blanking    Retrace

;-----------------------------------------------------------------------------
  vga_switch_screen: ; Set up CRTC, vars and UI fonts + switch active screen
;
; In:  byte[state.screen] = screen (SCR_ED25 / SCR_ED50 / SCR_FILES)
;-----------------------------------------------------------------------------
    mov    bx, [state.screen]            ; Use BX as lookup index (BH=0)
    shl    bl, 1
    mov    bx, [crtc_vals+bx]            ; now BX points to byte-value list
    mov    si, crtc_indexes              ; ...and SI to index list

    mov    dx, 3d4h                      ; 3D4 - CRTC
    mov    cx, 5                         ; Write to 5 registers

    cli
@@: mov    ah, [bx]                      ; AH = value
    lodsb                                ; AL = index
    out    dx, ax                        ; Word write to CRTC
    inc    bx                            ; Next value
    loop   @b
    sti

  ; Update Char Map Select reg based on screen/font state

    mov    dl, 0C4h                      ; Sequencer (address) port
    mov    ax, CHRMAP_SELECT*256 + 3     ; AH = value, AL = index
    test   byte[state.screen], 1         ; Set only in SCR_ED50
    jz     @f
    or     ah, CHRMAP_TALLACTIVE         ; ...means we set 8x8 as UI font
@@: test   byte[state.currfont], 1       ; Which user font is active?
    jz     @f
    or     ah, CHRMAP_2ACTIVE
@@: out    dx, ax
    ret

  crtc_vals:       dw .ed25, .ed50, .files
    .ed25:         db 000h, 050h, 07Fh, 010h, 00Fh
    .ed50:         db 00Fh, 050h, 087h, 010h, 007h
    .files:        db 007h, 0D0h, 07Fh, 010h, 00Fh
                    ;  |     |     |     |     |
  crtc_indexes:    db 00Ch, 00Dh, 018h, 007h, 009h
  ;        ___________/  ___/      |      \____   \______________
  ;       /        _____/          |           \                 \
  ;   Start       /        Line Compare:      Overflow:*      Max Scanline:
  ;  Address   Start        bits 0-7 of     bit 4 = 1 (bit    bit 6 = 0 (bit
  ;   High    Address      split screen       8 of split-     9 of split-
  ;             Low       start scanline     screen start)    screen start)
  ;
  ;                              |________ FOR SPLIT SCREEN ________|

  ; * All bits in Overflow normally protected, EXCEPT for bit 4
  ;      For 25 rows split starts @ line 400-16-1 = 0101111111b
  ;      For 50 rows split starts @ line 400-8-1  = 0110000111b
