; CONVENTIONS - FUNCTION NAME PREFIXES:
;
;   'screen': function can safely be called from non-'screen' functions in any
;             part of the program, because it sets ES to VRAM + restores on
;             exit, and ensure correct color attributes
;     'draw': may be called safely ONLY from a 'screen' function
;   'update': for dynamic UI elements; should be safely callable from 'screen'
;             functions or from anywhere else, as they depend on state and have
;             to be changed dynamically

;-----------------------------------------------------------------------------
  screen_init_text_IO: ; Save some space because this is used a lot
;-----------------------------------------------------------------------------
    mov    es, [seg_screen]              ; VGA text mode RAM
    mov    bx, [state.pal_attrmap]       ; lookups into attr_map
    ret

;-----------------------------------------------------------------------------
  screen_gen_editor: ; Draw everything! (Call whenever changing palettes)
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    call   screen_status_bar             ; Status bar (line 0)

    xchg   ax, cx                        ; CL=0 here => AL = NULL char
    mov    ah, [att.normal_right+bx]     ; Fill common background
    mov    cx, 80*99
    rep    stosw

    mov    ah, [att.normal_left+bx]      ; Left panels:
    mov    dh, 2
    mov    dl, 21                        ; ... for 25-line screen
    mov    di, VRAM_ED25+(3*ROW)
@@: mov    cl, 43
    rep    stosw
    add    di, (80-43)*2
    dec    dl
    jnz    @b
    mov    dl, 44                        ; ... for 50-line screen
    mov    di, VRAM_ED50+(5*ROW)
    dec    dh
    jnz    @b

  ; Draw dynamic UI elements

    call   update_tabs
    call   ed_set_box.init               ; Draws initial box stuff
    call   update_lbox_grid

  ; Draw dual-screen elements (stateful labels done in ed_get_key)

    mov    cl, loc_formatted_count       ; String count (keep it correct!)
    mov    si, loc_formatted             ; Point to first one
@@: push   cx
    mov    ax, draw_formatted_asc0       ; How/what are we drawing?
    clc
    call   locate_n_draw_x2              ; Do it
    pop    cx
    loop   @b

    mov    cl, ellipsis_count            ; Do the same for ellipses
    mov    si, loc_ellipsis              ; Point to first one
@@: push   cx
    mov    ax, draw_ellipsis             ; How/what are we drawing?
    clc
    call   locate_n_draw_x2              ; Do it
    pop    cx
    loop   @b

    mov    si, loc_attstr_esc            ; And why not Esc too
    mov    ax, draw_attr_asc0            ; How/what are we drawing?
    clc
    call   locate_n_draw_x2              ; Do it
    mov    si, loc_attstr_esc2
    mov    ax, draw_attr_asc0
    clc
    call   locate_n_draw_x2              ; and again for 2nd one

    call   update_box_legend             ; Box-specific caption+line+labels

  ; Done: return to sender

    pop    es                            ; Restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  screen_gen_files:   ; Create file dialog; title varies depending on action
;-----------------------------------------------------------------------------
    mov    ax, [fdlg.actkey]             ; presentation varies by action key
    cmp    ax, 1205h                     ; EXPORT?
    jne    @f                            ;  - nah: continue
    mov    si, .fmt                      ;  - yeah: look up which extension
    mov    bx, [fdlg.export_fmt]         ;          to put in the title
    shl    bx, 1
    mov    si, [si+bx]
    mov    di, txt.fdt_ext               ;          + do it
    times  2 movsw
@@: mov    di, file_keys                 ; see ed_get_key
    mov    cx, NUM_FKEYS                 ;  "
    repne  scasw                         ; find keycode in the list
    add    di, .titles-2-file_keys       ; find pointer to corresponding title
    mov    si, [di]                      ; get the text
    mov    di, si
    xor    al, al
    mov    cl, 0FFh                      ; a big enough max length
    repne  scasb                         ; now find the terminating zero
    sub    di, si                        ; ...this is our length
    and    di, 0FFFEh                    ; ...make sure it's even, so we can
    mov    bp, di                        ; ...use BP for centering later

    push   es  ;1;                       ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    mov    di, VRAM_FILES                ; fill area with solid background
    mov    cx, 24*ROW/2
    mov    ah, [att.normal_right+bx]     ; <TEMP?>
    mov    al, 20h
    rep    stosw

    mov    di, VRAM_FILES+ROW+2          ; now set location for THE BOX
    mov    ah, [att.dlg_title+bx]        ; attribute for first row only
    push   ax  ;2;
    mov    dx, 22                        ; row counter
@@: mov    cx, (ROW/2)-2                 ; box width
    rep    stosw
    add    di, 2*2
    mov    ah, [att.dlg_bg+bx]           ; attribute for all other rows
    dec    dx
    jnz    @b

    pop    ax  ;1;                       ; restore title attribute
    mov    di, VRAM_FILES+ROW+80         ; acquire target
    sub    di, bp                        ; center the text...
    call   draw_asc0                     ; ...and write it out

    pop    es  ;0;                       ; restore previous I/O target
    ret

  .titles:
    dw txt.fdt_s, txt.fdt_s, txt.fdt_l, txt.fdt_l, txt.fdt_i, txt.fdt_e
    ;  F2/save    ^S/save    F3/load    ^L/load    ^I/import  ^E/export
  .fmt:
    dw txt.ext_com,  txt.ext_bmp,  txt.ext_xbin
    ;  'COM'         'BMP'         'XB'

;-----------------------------------------------------------------------------
  screen_gen_preview: ; Create preview page; varies depending on font height
;
; In:  SI -> current font structure
; Out: BP  = font height
;-----------------------------------------------------------------------------
    mov    bp, word[si]                  ; BP: current user font height
.known_h:                                ;     (call here if already set)
    push   bp                            ;     + save it for returning

  ; Prepare current character number (for tiling)

    mov    al, [state.currchar]          ; tile current character
    mov    di, pvw.final2 + 1            ; target for decimal codepoint
    mov    dh, OP_STOSB                  ; no attributes
    call   str_dec_byte                  ; write it

  ; Draw to desired page

    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    mov    bx, draw_attr_asc0.x          ; our favorite routine to call here

    mov    al, 20h                       ; fire blanks
    mov    ah, [preview_attr.regular]    ; snatch wanted attribute
    mov    di, VRAM_PVW_AREA             ; stomp over the entire virtual page
    mov    cx, 100*ROW/2
    rep    stosw

  ; "Pack my liquor box with five dozen jugs"

    push   ax
    mov    ah, [preview_attr.inverse]    ; swap FG/BG for this one
    mov    di, VRAM_PVW_TEXT
    mov    si, pvw.pangram
    call   bx  ;draw_attr_asc0.x
    pop    ax

    mov    di, ROW
    cmp    bp, 21                        ; skip separating row if height>21
    ja     @f
    shl    di, 1
@@: add    di, VRAM_PVW_TEXT

  ; Draw all 256 characters

    xor    al, al                        ; start at 0
.r: mov    cl, 32                        ; chars per row (CH = 0 here)
.c: stosw                                ; DO IT
    inc    al                            ; value++
    jz     @f                            ; exit if all 256 done (wrapped to 0)
    loop   .c
    add    di, ROW-32*2
    jmp    short .r

  ; Draw three-four stuff

@@: sub    di, ROW*7-2                   ; si = pvw.threefour
    push   di ;1;                        ; for upper-block fixups
    mov    cl, 8
@@: lodsb
    times  3 stosw
    lodsb
    times  4 stosw
    add    di, ROW-7*2
    loop   @b

    mov    dx, -(32+1)*2+15*2
    cmp    bp, 23                        ; skip separating row if height>23
    ja     @f
    mov    dx, 7*2+80+15*2
@@: add    di, dx
    push   di ;2;                        ; for upper-half integral fixups

  ; Draw boxes and math stuff

    mov    cl, 3                         ; SI = pvw.boxes
@@: call   bx  ;draw_attr_asc0.x
    add    di, ROW-(24*2)
    loop   @b
    sub    di, ROW*2+14*2
    call   bx  ;draw_attr_asc0.x

    mov    dx, ROW*2-13*2
    cmp    bp, 22                        ; skip separating row if height>22
    ja     @f
    add    dx, ROW
@@: add    di, dx

  ; Draw last 3 rows

    mov    dx, 3                         ; SI = pvw.final1
.l: call   bx  ;draw_attr_asc0.x
    inc    di
    inc    di
    mov    al, [state.currchar]          ; tile current character
    mov    cl, 13
    call   draw_char_CX_times
    add    di, 3*2 + 1
    call   bx  ;draw_attr_asc0.x         ; print reminder of line
    cmp    dl, 3                         ; top row only:
    je     .m                            ;    skip
    mov    cl, 9                         ; else:
    sub    cl, dl                        ;    draw (9-DL) 'alignment test'
@@: mov    al, 219                       ;    characters
    stosw
    lodsb
    stosw
    loop   @b
.m: add    di, ROW-40*2                  ; next row
    dec    dx
    jnz    .l

  ; Do single-char fixups

    pop    di ;1;
    sub    di, 14*2
    mov    al, 244                       ; upper-half integral
    stosw
    add    di, ROW*2-2
    inc    ax                            ; lower-half integral
    stosw
    pop    di ;0;
    add    di, ROW*2+4*2
    mov    cl, 2
    mov    al, 223                       ; upper block char
@@: stosw                                ; do fixup
    inc    di
    inc    di
    stosw
    add    di, ROW-3*2
    loop   @b

  ; All done

    pop    es                            ; restore previous I/O target
    pop    bp                            ; return height
    ret

;-----------------------------------------------------------------------------
  screen_status_bar: ; Cursor OFF, draws status bar @ addr. 0 for split-screen
;-----------------------------------------------------------------------------
    call   vga_set_cursor.off
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    xor    di, di
    mov    cx, 80
    mov    ah, [att.version+bx]          ; nice attribute for messages
    mov    al, 20h
    rep    stosw
    mov    di, 2
    mov    si, attstr_status_bar
    call   draw_attr_asc0
    mov    al, 0B3h                      ; '³' vertical line
    stosb
    mov    di, (68-VER_STRLEN)*2         ; spaces before version string
    stosb
    inc    di
    mov    cl, 3
@@: call   draw_attr_asc0
    loop   @b
    pop    es                            ; restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  screen_status_msg: ; Prints message to status bar
;
; In:       SI -> pointer to ASCIIZ string
; Destroys: CX, AX, BX
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    mov    di, 30
    call   draw_asc0
    mov    cx, 62*2                      ; fill the rest with NULLs
    sub    cx, di                        ;     (Do 79-(DI/2) times)
    shr    cl, 1
    xor    ax, ax
    mov    ah, [att.version+bx]
    rep    stosw
    pop    es                            ; restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  screen_status_prompt: ; Displays a configurable prompt in the status bar
;
; In:       AH = display parameters:
;                bit 0: STYLE;  0 = normal prompt; 1 = warning prompt
;                bit 1: CURSOR; 0 = off; 1 = on
;                bit 2: RETURN; 0 = immediate, 1 = wait for a key first
;                bit 3: WIPE;   0 = keep contents & print @ DI; 1 = clear bar
;           SI-> ASCIIZ string to display
;           DI-> screen VRAM target (if AH bit 3 is set)
; Out:      AX = key code (if requested w/AH bit 2)
; Destroys: CX, AX, DX, BX
;-----------------------------------------------------------------------------
    push   ax
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    xchg   ax, dx                        ; DH <- AH (display parameters)
    test   dh, 1
    mov    ah, [att.prompt+bx]           ; parameter bit 0 set?
    jz     @f                            ;    - nope, do not panic
    mov    ah, [att.prompt_warn+bx]      ;    - yep, use warning attr.

@@: test   dh, 8                         ; parameter bit 3 set?
    jz     @f                            ;    - no, just go print
    xor    al, al                        ;    - yep, wipe the bar first
    xor    di, di
    mov    cx, 80
    rep    stosw
    mov    di, 2

@@: mov    dl, [att.prompt_hilite+bx]    ; yet another BORING drawing routine
    mov    dh, [att.prompt_select+bx]    ;    for ASCIIZ strings with custom
  .go_markup:                            ;    markup... only used for propmpts
    lodsb                                ;    so let's inline this nonsense
    cmp    al, 0
    je     .msg_done                     ; 0? - Get out
    cmp    al, 1
    jne    @f                            ; 1? - Swap normal <-> highlight
    xchg   ah, dl
    jmp    short .go_markup
@@: cmp    al, 2
    jne    @f                            ; 2? - Swap normal <-> select
    xchg   ah, dh
    jmp    short .go_markup
@@: stosw                                ; none of the above? WRITE THE SUCKER
    cmp    di, 158
    ja     .msg_done                     ; about to exceed row length? ABORT
    jmp    short .go_markup

  .msg_done:
    call   vga_set_cursor.off
    pop    es                            ; restore previous I/O target
    pop    ax
    test   ah, 2                         ; parameter bit 1 set?
    jz     @f                            ;    - no; begone, ye blinkin' block
    call   vga_place_cursor              ;    - yes; enable the cursor at DI
@@: test   ah, 4
    jz     @f                            ; parameter bit 2 set?
    xor    ax, ax
    int    16h                           ;    - wait for a keystroke
@@: ret

;-----------------------------------------------------------------------------
  screen_status_menu: ; Displays menu of selectable options in the status bar
;
; In:       DI -> screen VRAM target
;           SI -> menu:  DW num_options; then:
;                        times num_options DB 'text' (EXACTLY 4 CHARS)
;                        times num_options DW return_val
; Out:      CF  = clear? option was selected; set? user pressed ESC
;           DX  = number of selected option if CF clear
; Destroys: EVERYTHING
;-----------------------------------------------------------------------------
    push   es  ;1;                       ; save previous I/O target
    call   vga_set_cursor.off            ; shoo the cursor away

    lodsw                                ; number of options
    xchg   ax, cx
    mov    bp, cx                        ;   + back it up
    push   di  ;2;                       ; screen VRAM target
    mov    di, scratch                   ; construct menu in scratch
    mov    al, ' '
@@: stosb                                ; space
    times  2 movsw                       ; copy 4 chars of text
    times  2 stosb                       ; space*2
    loop   @b
    xor    al, al                        ; terminating zero
    stosb
    xchg   cx, dx                        ; DX = CX=0= selected option number

    pop    di  ;1;                       ; screen VRAM target
  .menu_loop:
    call   screen_init_text_IO           ; ES -> VGA text, BX = attr_map
    mov    si, scratch                   ; menu please
    push   di  ;2;
    mov    ah, [att.prompt_hilite+bx]
    call   draw_attr_asc0.x
    pop    di  ;1;                       ; screen VRAM target
    push   di  ;2;
    mov    al, 14
    mul    dl
    add    di, ax                        ; VRAM target += (14*option#)
    mov    al, 11h
    stosb
    mov    al, [att.prompt_select+bx]
    mov    cx, 6
    call   draw_char_CX_times            ; actually attr, but DI is odd
    dec    di
    dec    di
    mov    al, 10h
    stosb
  .get_key:
    xor    ax, ax                        ; now let's go get some input
    int    16h
    cmp    ax, 011Bh                     ; Esc?
    jne    @f
    stc                                  ;   - chicken out w/CF set
    jmp    short .done_here
@@: cmp    ax, 1C0Dh                     ; Enter?
    jne    @f
    clc                                  ;   - return w/CF clear, DX=selection
    jmp    short .done_here
@@: push   cs  ;3;                       ; neither Esc nor Enter?
    pop    es  ;2;
    mov    di, .keys                     ;   - see if we increase/decrease the
    mov    cl, MENUKEYS                  ;     option number: consult keylist
    repne  scasw
    jne    .get_key                      ; key not in list? try again
    pop    di  ;1;                       ; screen VRAM target... AGAIN
    cmp    cx, MENUFWD
    jb     @f
    inc    dx                            ; num_option++
    cmp    dx, bp                        ;     >= allowed number?
    jne    .menu_loop                    ;     - no, ok
    xor    dx, dx                        ;     - yes, wrap to 0
    jmp    short .menu_loop
@@: dec    dx                            ; num_option--
    jns    .menu_loop                    ;     - if >=0, ok
    add    dx, bp                        ;     - if negative, wrap to max
    jmp    short .menu_loop
  .done_here:
    pop    di  ;1;                       ; keep the stack tidy
    pop    es  ;0;                       ; restore previous I/O target
    pushf                                ; preserve CF
    push   dx                            ; preserve DX (cursor OUTs mangle it)
    call   screen_status_bar             ; clear that crap
    pop    dx
    popf
    ret

  .keys:
    dw     0F09h  ; Tab
    dw     3920h  ; Space
    dw     4800h  ; Up arrow
    dw     4838h  ; Up arrow*
    dw     4D00h  ; Right arrow
    dw     4D36h  ; Right arrow
    MENUFWD = 4                          ; CX>=this if key means forward
    dw     4B00h  ; Left arrow
    dw     4B34h  ; Left arrow*
    dw     5000h  ; Down arrow
    dw     5032h  ; Down arrow*
    MENUKEYS = ($-.keys)/2

;-----------------------------------------------------------------------------
  update_filedlg: ; Update file dialog state and contents
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

  ; Status bar first

    mov    ah, [att.prog_name+bx]        ; prepare attribute #1
    mov    dh, [att.version+bx]          ; ...and #2
    xor    di, di
    mov    si, txt.fdbar
    mov    cx, 10                        ; CAREFUL
@@: call   draw_attr_asc0.x              ; draw key legend
    xchg   ax, dx                        ; switch attributes
    loop   @b
    test   byte[fdlg.focus], 1           ; where's our focus?
    pushf  ;1;                               (save the answer)
    jnz    @f                            ; - dir list: keep legend as-is
    mov    di, 2                         ; - text input: relocate
    xchg   ax, dx                        ;   ...switch attributes again
    call   draw_attr_asc0.x              ;   ...clobber left half w/prompt
    mov    cl, 15                        ;   ...and add some blank space for
    rep    stosw                         ;      good measure

  ; Now see about those attributes

@@: lea    bp, [bx+FD_ATTDIFF]           ; BP = transform 'ON' attrs to 'OFF'
    popf   ;0;                           ; where were we again?
    pushf  ;1;                           ;   (nope, not done with this yet)
    jz     @f                            ; - text input: start w/'ON'
    xchg   bx, bp                        ; - dir list: start w/'OFF'

  ; Take care of input label+field

@@: mov    si, txt.fd_label
    mov    di, VRAM_FILES+ROW*3+6
    mov    ah, [att.dlg_ON_label+bx]     ; set a proper attribute
    call   draw_attr_asc0.x              ; print that label
    mov    ah, [att.dlg_ON_text+bx]      ; now for the input field contents
    call   update_filedlg_inp.a          ; - SHOW 'EM

  ; Show the selected directory

    xchg   bx, bp                        ; swap 'ON' and 'OFF' attrs
    mov    ah, [att.dlg_ON_label+bx]
    mov    al, 196                       ; 'Ä' horizontal line
    mov    si, fdlg.truepath
    mov    di, VRAM_FILES+ROW*5+6
    push   di ;2;
    mov    cx, 74
    rep    stosw                         ; fill out a nice separating border
    pop    di ;1;
    call   draw_attr_asc0.x              ; show current path
    stosw                                ; ...and another blank

  ; Draw file list area

    mov    di, VRAM_FILES+ROW*6+6
    mov    bp, di                        ; store this destination
    mov    ah, [att.dlg_ON_vline+bx]
    mov    dx, 16                        ; rows
.r: mov    cl, 5                         ; column
    jmp    short @f                      ; no separator before first column
.c: mov    al, 179                       ; '³' vertical line
    stosw
@@: push   cx
    mov    cl, 14
    mov    al, ' '
    rep    stosw
    pop    cx
    loop   .c
    add    di, 6*2                       ; assume position for next row
    dec    dx
    jnz    .r
    cmp    word[fdlg.count], dx          ; still haven't got any files? (DX=0)
    jne    @f
    mov    si, txt.reading               ; - then say we're busy reading
    mov    di, bp
    call   draw_attr_asc0.x
    jmp    short .done

  ; Got files? List 'em now

@@: mov    cx, 80                        ; each page has 5*16=80 files
    mov    si, [fdlg.pagebase]           ; fseg.fname_ptrs = 0, so...
    mov    ah, [att.dlg_ON_text+bx]
    mov    dx, [fdlg.count]              ; DX counts down from total
    mov    bp, [fdlg.fnum_rel]           ; BP = # of selected file on page;
    shl    bp, 1                         ;      make it OFFSET (for position)
    push   si  ;1;
    push   ds  ;2;
    mov    ds, [seg_fseg]                ;   .++++++ DS <- FILES SEG +++++++,
    push   bx  ;3;                       ;   |  save attr map               |
    sub    dx, si                        ;   |  fseg.fname_ptrs = 0, so...  |
    shl    si, 1 ;fseg.fname_ptrs=0      ;   |  SI-> file #1                |
    mov    bx, fseg.pos                  ;   |  BX -> VRAM position table   |
    add    bp, bx                        ;   |  BP -> pos. for current file |
  .print:                                ;   |                              |
    mov    di, [bx]                      ;   |  get first one               |
    call   draw_fname                    ;   |                              |
    inc    bx                            ;   |  next VRAM position          |
    inc    bx                            ;   |                              |
    dec    dx                            ;   |  counter--                   |
    loopnz .print                        ;   |  exceeded file count? ABORT  |
    mov    di, [bx]                      ;   |  pos++ (for 'too many' case) |
    pop    bx  ;2;                       ;   |  retore attr map             |
    push   di  ;3;                       ;   |                              |
    mov    al, [cs:att.dlg_ON_hilite+bx] ;   |  highlight selected file     |
    mov    di, [ds:bp]                   ;   |                              |
    dec    di                            ;   |                              |
    mov    cl, 14                        ;   |                              |
    call   draw_char_CX_times            ;   |  (this zeroes out CX)        |
    pop    di  ;2;                       ;   |                              |
    pop    ds  ;1;                       ;   `+++++++ DS <- BASE SEG +++++++'

  ; Print a notice if list is incomplete (dir contains to many files)

    pop    dx  ;0;                       ; DX = pagebase
    cmp    [fdlg.too_long], cl           ; is the flag zero? (CL=0)
    je     .done                         ; - yep: all files were listed
    cmp    dx, (MAX_FILES/80)*80         ; - are we on the last page?
    jne    .done                         ;   - no: go on
    mov    si, txt.too_many              ;   - yes: show the warning
    call   draw_attr_asc0.x
    add    di, ROW-(9*2)
    call   draw_attr_asc0.x

  .done:
    popf   ;0;                           ; check our focus once more
    jz     @f                            ; - text input: keep cursor on
    call   vga_set_cursor.off            ; - dir list: dismissed

@@: pop    es                            ; restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  update_file_hilite:  ; Update highlight in dir list (ON/OFF)
;
; In:  CF = 0 to set highlight OFF, 1 to set it ON
;      AX = number of file on current page (0..80)
;-----------------------------------------------------------------------------
    push   bx
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    push   ds
    push   di
    push   cx
    push   ax

    jnc    @f
    add    bx, NUM_ATTRMAPS              ; CF set? use highlight attr.
@@: mov    ds, [seg_fseg]
    mov    di, fseg.pos                  ; position table
    add    di, ax
    add    di, ax
    mov    di, [di]                      ; locate target character cell
    mov    al, [cs:att.dlg_ON_text+bx]
    dec    di                            ; go to previous cell's attr. byte
    mov    cl, 14                        ; CH = 0 if we're here
    call   draw_char_CX_times            ;

    pop    ax
    pop    cx
    pop    di
    pop    ds
    pop    es
    pop    bx
    ret

;-----------------------------------------------------------------------------
  update_filedlg_inp:  ; Update file dialog input field
;
; IN:      [fdlg.input] = ASCIIZ string
;-----------------------------------------------------------------------------
    mov    ah, -1
  .a:                                    ; call here if attribute is known
    push   di                            ; PRESERVE INPUT LOCATION
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    cmp    ah, -1
    jne    @f                            ; not coming from update_filedlg?
    mov    ah, [att.dlg_ON_text+bx]      ;     then always use "ON" attribute
@@: mov    si, fdlg.input
    mov    di, I_VRAM
    call   draw_attr_asc0.x

    push   ax  ;!;                       ; save crap attr + NULL char
    call   vga_place_cursor              ; show cursor after end of string
    pop    ax  ;!;
    mov    cx, I_VRAM+ILEN*2
    sub    cx, di
    shr    cx, 1
    rep    stosw                         ; fill w/blanks to end of input!

    pop    es                            ; restore previous I/O target
    pop    di                            ; RESTORE INPUT LOCATION
    ret

;-----------------------------------------------------------------------------
  update_preview_inp:  ; Update preview screen w/custom text string
;
; IN:      [pvw_txtlen]  = length of input string
;          [pvw.pangram] = ASCIIZ string
;          BP           =< 32 if input was ended (enter/esc)
;-----------------------------------------------------------------------------
    push   di                            ; PRESERVE INPUT LOCATION
    push   es                            ; I/O target = screen: ES -> VGA text
    mov    es, [seg_screen]              ;     mode RAM (don't touch BX here!)

    mov    al, [preview_attr.inverse]
    mov    ah, al
    cmp    bp, 32
    jna    @f
    mov    al, 70h
    cmp    al, ah
    jne    @f
    mov    al, 60h
@@: mov    ah, al
    mov    di, VRAM_PVW_TEXT
    push   di
    mov    si, pvw.pangram
    call   draw_attr_asc0.x
    pop    di
    mov    ax, [pvw_txtlen]
    cmp    al, 40
    jb     @f
    dec    ax
@@: shl    ax, 1
    add    di, ax
    call   vga_place_cursor              ; show cursor after end of string

    pop    es                            ; restore previous I/O target
    pop    di                            ; RESTORE INPUT LOCATION
    ret

;-----------------------------------------------------------------------------
  update_box_legend:  ; Redraws caption, line, and box-specific labels
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    mov    si, loc_cap_line              ; Draw caption line on right side
    mov    ax, draw_caption_line         ; How/what are we drawing?
    clc
    call   locate_n_draw_x2              ; Do it (both screens)

    mov    si, loc_attstr_editbox_cap    ; Now do the actual caption, and the
    mov    dx, loc_f_editbox             ;    box-specific labels
    test   byte[state.currbox], 1
    jz     @f
    mov    si, loc_attstr_fontbox_cap
    mov    dx, loc_f_fontbox
    @@:
    mov    ax, draw_attr_asc0            ; First the caption
    clc
    call   locate_n_draw_x2              ; Do it (both screens)

    mov    si, dx                        ; box-specific label
    mov    ax, draw_formatted_asc0       ;    (Space/Enter legend)
    clc
    call   locate_n_draw_x2              ; Do it (both screens)

    pop    es                            ; Restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  update_stateful:   ; Updates all 'stateful' action labels on screen
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    mov    bp, .do_it

  ; DRAG, GUIDE: enabled only in editbox

    mov    si, loc_f_stateful.drag
    test   byte[state.currbox], 1        ; ZF set = enabled (only for editbox)
    call   bp                            ; SI -> drag
    call   bp                            ; SI -> guide

  ; DESELECT: enabled if selection exists for current box
  ; CUT, COPY: font box = always enabled; editbox = as above

    pushf  ;1;
    pushf  ;2;
    mov    di, state.chrmark_mask        ; word to check for editbox
    jz     @f                            ; ZF set? = editbox (from prev check)
    mov    di, state.fntmark             ; word to check for fontbox
@@: xor    ax, ax
    cmp    word[di], ax                  ; got a relevant marking?
    je     @f                            ; - no? skip
    dec    ax
@@: inc    ax                            ; - yes? ZF should be set now
    push   ax ;3;
    call   bp                            ; UNMARK
    pop    ax ;2;                        ; 0 or 1
    popf   ;1;                           ; for cut/copy:
    jz     @f                            ; - editbox? - reuse condition
    xor    ax, ax                        ; - fontobx? - always allow
@@: dec    ax
    mov    [state.can_copy], ax          ; bit 0 = 1 if enabled
    inc    ax
    call   bp                            ; CUT
    call   bp                            ; COPY

  ; PASTE: enabled if clip type and dimensions match relevant box

    mov    al, 1
    popf   ;0;                           ; ZF set? = editbox
    jz     .e
    cmp    word[state.clip_len], 0       ; FONTBOX: length must be > 0
    jne    .g
    inc    ax                            ;   fail? ensure ZF won't be set
@@: jmp    short .g                      ;   all ok? ensure it WILL be
.e: dec    ax
    cmp    word[state.clip_dim], 0       ; EDITBOX: dimensions must be > 0
    jne    .g                            ;   ok? keep AL=0 so ZF will be set
    dec    ax                            ;   fail? -1 - ensure it won't be
.g: sub    al, byte[state.clip_type]     ; good _type: 1=fontbox, 0=editbox
    call   bp
    mov    al, 1
    jz     @f                            ; (flags preserved!)
    dec    ax
@@: mov    byte[state.can_paste], al

  ; REVERT: enabled if active font isn't '<NoName>' & has unsaved changes

    mov    di, [state.currfont_ptr]      ; Point at current font
    mov    al, byte[di+font.unsaved]     ; AL = 1 if unsaved
    mov    ah, byte[di+font.fname]
    cmp    ah, '<'
    je     @f
    cbw                                  ; AH = 0 if **NOT** '<NoName>'
@@: dec    ax                            ; ZF set if both conditions true
    call   bp                            ; draw
    mov    al, 1
    jz     @f                            ; (flags preserved!)
    dec    ax
@@: mov    byte[state.can_revert], al

  ; UNDO: enabled if current font has undo buffer

    cmp    byte[di+font.can_undo], 1
    call   bp

  ; 9-DOT CELL: enabled unless we're in DOSBox with a non-"vgaonly" config

    test   byte[state.is_bad_vga], 1
    call   bp

  ; LGE: enabled if we're in 9-dot mode only

    test   byte[state.clkmode80], 1
    call   bp

  ; DUP LINE, INS LINE: enabled if current font is <32 lines tall

    test   byte[di], 100000b             ; DI -> current font (.height)
    call   bp                            ; dup line
    call   bp                            ; ins line

  ; DEL LINE: enabled if current font is >1 line tall

    mov    al, 33
    sub    al, [di]
    test   al, 100000b
    call   bp

  ; SWAP CHARS: enabled if current char != hover char

    mov    al, byte[state.currchar]
    mov    ah, byte[state.hoverchar]
    cmp    al, ah
    mov    al, ah
    jne    @f                            ; invert sense of zero flag (UGLY)
    inc    al
@@: xor    al, ah
    call   bp

    call   update_8_9_dot_hilite         ; update these too
    pop    es                            ; Restore previous I/O target
    ret

.do_it:
    push   di                            ; preserve font ptr if applicable
    push   bp                            ; function ptr, too
    pushf                                ; and flags for repeated ops
    mov    ax, draw_formatted_asc0
    jz     @f                            ; ZF set?   - draw label as ENABLED
    mov    ax, draw_formatted_asc0.off   ; ZF clear? - draw label as DISABLED
@@: clc
    call   locate_n_draw_x2              ; Do it
    popf
    pop    bp
    pop    di
    ret

;-----------------------------------------------------------------------------
  update_lbox_char:  ; Updates actual character + marking in edit box
;-----------------------------------------------------------------------------
    push   es  ;-1                       ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    push   es  ;-2
    push   ds  ;-3
    pop    es  ;-2                       ;      Draw to scartch RAM first, to
    mov    di, scratch                   ;          speedup later VRAM update
    push   di  ;-3
    mov    ax, [state.currchar]          ;      High byte = 0
    mov    cl, 5
    shl    ax, cl                        ;      char*32 (offset in font.data)
    xchg   bx, ax                        ;           `-> BX
    mov    ax, [state.currfont_ptr]
    mov    si, ax
    mov    cx, word[si]                  ; CX = height (hi byte 0)
    push   cx  ;-4
    add    ax, font.data                 ;      Point to current font data
    add    bx, ax              ;+^ lea?  ; BX-> line 0 of current character
    mov    si, editbox_ON_att            ; SI-> editbox attributes
    test   byte[state.currbox], 1        ;      Is editbox active?
    jz     @f                            ;        - yep, keep active attrs
    mov    si, editbox_OFF_att           ;        - nope, use inactive ones
@@: mov    bp, state.chrmark_rows        ; BP-> row 0 of editbox mark
    mov    ah, 10000000b                 ; AH = current col/px (rotating mask)
    mov    al, byte[state.clkmode80]     ; AL = Bit0 = 0? 9-dot; = 1? 8-dot

  .row:
    push   cx  ;-5
    mov    dh, byte[bx]                  ; DH = char row bitmask (0=BG, 1=FG)
    mov    dl, byte[bp]
    and    dl, byte[state.chrmark_mask]  ; DL = ANDed mark bit mask
    mov    cl, 8                         ;      (0=off, 1=on)
  .pel:
    push   si  ;-6                       ;      "px0" at start
    test   dh, ah                        ;      BG or FG?
    jz     @f                            ;        - BG; "px0"
    inc    si                            ;        - FG; "px1"
@@: test   dl, ah                        ;      Marked or not?
    jz     @f                            ;        - N; keep
    inc    si                            ;        - Y: advance two more to
    inc    si                            ;             "px?_mark"
@@: movsb                                ;      Copy to scratch
    ror    ah, 1                         ;      Next column to check
    test   si, 1                         ;    * if was BG, bit0 *SET* (movsb^)
    pop    si  ;-5
    loop   .pel                          ;      Next pixel
    push   si  ;-6                       ;      Done with 8 columns; have 9th?
    jnz    @f                            ;    * Was last SI odd? (=BG ^)
    inc    si                            ;      - no: pixel was FG, add one
@@: add    si, edbox_attrs.px0_col9      ;      Get col9 attr, whether BG/FG
    test   al, 1                         ;      Need to draw the 9th?
    jnz    @f                            ;      - nope, skip it
    movsb                                ;      - yep, do it
@@: pop    si  ;-5
    inc    bx                            ;      Next line of current char
    inc    bp                            ;      Next row of editbox mark
    pop    cx  ;-4
    loop   .row

    test   al, 1                         ;      Do the following only if in
    jnz    .ok                           ;          9-dot mode?
    add    si, edbox_attrs.px0_col9      ;      Get col9 attr - now BG only!
    pop    cx  ;-3                       ;      Restore font height
    pop    di  ;-2                       ; DI-> Scratch
    push   di  ;-3
    push   cx  ;-4
    test   byte[state.attr_modectl], 4   ;      if B2=0 (LGE off), force BG
    jz     @f                            ;          column 9
    mov    ah, [state.currchar]          ;      UGLY HACK IS UGLY
    and    ah, 0E0h                      ;      Are you a line-graphics char
    cmp    ah, 0C0h                      ;        w/a right extension?
    je     .ok                           ;        - yes? okay then
@@: add    di, 8                         ;        - no? NO 9TH COLUMN FOR YOU!
    movsb
    dec    si
    loop   @b
  .ok:

  ; Finished drawing in scratch RAM - copy to the screen:

    pop    cx  ;-3                       ;      Restore font height
    pop    si  ;-2                       ; SI-> Scratch
    pop    es  ;-1                       ;      Now write to VGA
    mov    dh, al                        ; DH = Bit0 = 0? 9-dot; = 1? 8-dot
    mov    di, VRAM_ED25+5*ROW+3*2       ;      ED25     "     "
    mov    al, byte[state.screen]
    inc    ax
    test   al, 2
    jz     @f
    mov    di, VRAM_ED50+10*ROW+6*2      ;      ED50 screen: target offset
    mov    al, -1
@@: cbw
    xchg   ax, bx                        ; BX = 1 for ED25, -1 for ED50

  macro _w_px { lodsb                    ;      Get attribute
                inc   di                 ;      Advance to attribute byte
                stosb                    ;      Copy attr
                add   di, bx             ;      ED50? - effectively "dec di"
                stosb  }                 ;      ED25? - paint next char

  .w_row:
    push   di
    rept   8 {_w_px}                     ;      Unrolled the inner loop...
    test   dh, 1                         ;      9-dot (paint an extra cell)?
    jnz    .w_next
    _w_px
  .w_next:
    pop    di
    add    di, ROW                       ;      Next row on screen
    loop   .w_row

  ; Now just update cursor on screen

    mov    si, [state.currfont_ptr]      ; SI-> Current font
    mov    ax, [state.currchar]          ;      High byte = 0
    mov    cl, 5
    shl    ax, cl                        ;      char*32 (offset in font.data)
    xchg   bx, ax                        ;           `-> BX
    mov    al, [state.cursor_pos+1]      ;      Load row only
    cbw
    cmp    al, byte[si]                  ;      If we switched fonts, cursor
    jb     @f                            ;         row may be >= new font's
    mov    al, byte[si]                  ;         height, so let's clamp it
    dec    ax                            ;         to the bottom-most row
    mov    [state.cursor_pos+1], al      ;         (height-1)
@@: add    bx, ax                        ; BX-> offset of line in current char
    mov    al, [si+bx+font.data]         ; AL =    "

    mov    si, editbox_ON_att \          ; SI-> active box/px0 cursor attr.
               + edbox_attrs.px0_curs
    test   byte[state.currbox], 1        ;      Is editbox active?
    jz     @f                            ;        - yep, keep active attr
    mov    si, editbox_OFF_att \         ;        - nope, use inactive one
               + edbox_attrs.px0_curs
@@: test   al, [state.cursor_mask]       ;      Pixel at cursor is FG?
    jz     @f
    inc    si                            ;        - use px1 cursor attr
@@: lodsb                                ; AL = attribute
    push   ax  ;-2
    mov    ax, [state.cursor_pos]
    call   locate_lbox_cell
    pop    ax  ;-1
    inc    di                            ; DI-> *attribute* byte
    stosb
    dec    bp
    jz     @f
    inc    di                            ;      If ED25, draw again
    stosb
@@: pop    es                            ; Restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  update_lbox_grid:  ; Updates grid & guides in the edit box
;
; Always call after updateLBoxOuter and whenever guides are changed
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;      mode RAM, BX = attr_map index
    push   bx                            ;      Save attrMap index

    mov    dl, byte[state.screen]        ; DL = 0? - ED25; 1? ED50
    mov    dh, byte[state.clkmode80]     ; DH = Bit0 = 0? 9-dot; = 1? 8-dot
    xor    ax, ax
    call   locate_lbox_cell              ; DI-> VRAM target
@@: mov    si, [state.currfont_ptr]      ;      Point to current font
    mov    cx, word[si]                  ; CX = Height (offset 0; hi byte 0)
    mov    si, state.guide_rows          ; SI-> Array of 32 horizontal guides
    mov    bh, byte[state.guide_cols]    ; BH = Bit mask for vertical guides

  .row:
    push   cx
    push   di
    mov    cl, 8
    mov    bl, 10000000b                 ; BL = Bit mask for current column
    mov    ah, byte[si]                  ;      Horiz. guide (0? no, -1? yes)
    or     ah, bh                        ; AH = ORed bitmasks to test against
  .cell:
    mov    al, GRID1                     ; AL = Char to display
    jcxz   @f                            ;      Skip next check if on col 9
    test   bl, ah                        ;      Draw guide char here?
    jz     @f
    inc    ax
    inc    ax ;GUIDE1
@@: stosb                                ;      Draw first grid char
    inc    di
    test   dl, 1                         ;      ED50? - skip to next cell
    jnz    @f
    inc    ax ;GRID2/GUIDE2              ;      ED25? - draw second grid char
    stosb
    inc    di                            ;      Next char on screen
@@: ror    bl, 1                         ;      Next column in bit mask
    jcxz   .row_done                     ;      Forced 9th col? don't loop!
    loop   .cell
    test   dh, 1                         ;      9-dot (need an extra char)?
    jnz    .row_done
    jmp    short .cell                   ;      Do one last time...
  .row_done:
    pop    di
    add    di, ROW                       ;      Next row on screen
    inc    si                            ;      Next horizontal guide row
    pop    cx
    loop   .row

    pop    bx                            ;      Restore attrMap index
    pop    es                            ; Restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  update_lbox_out: ; updates edit box title (decimal ASCII code + character)
;
; This has to be redone whenever the active font/screen is switched, AFTER
; state.screen is updated but BEFORE the start address is changed
; Also whenever current character changes (updates title and rulers)
;-----------------------------------------------------------------------------
    push   es  ;1;                       ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    mov    al, byte[state.clkmode80]     ; bit0: 0->9-dot; 1->8-dot
    and    ax, 1                         ; AX: =   "
    xchg   ax, bp                        ; BP: =   "
    test   byte[state.currbox], 1        ; Is editbox active?
    jnz    @f                            ; Nope - use inactive attribute
    sub    bx, BOXOUTER_ATTDIFF          ; Yep  - switch to active
@@: mov    ah, [att.box_OFF_title+bx]    ; save this for later (title)
    push   ax  ;2;
    mov    ah, [att.box_OFF_border+bx]   ; <- Initial attr
    mov    dl, [att.box_OFF_ruler+bx]
    mov    dh, [att.box_OFF_ruler_on+bx]
    mov    si, [state.currfont_ptr]      ; Point to current font
    lodsb                                ; Byte 0 in struct holds height
    mov    di, VRAM_ED50+9*ROW+5*2       ; 50-line screen: target offset
    mov    bx, 9                         ;                 inner width
    mov    si, ROW-11*2                  ;                 skip each row
    test   byte[state.screen], 1         ; ed25?
    jnz    @f                            ; no, keep ed50 values
    mov    di, VRAM_ED25+4*ROW+2*2       ; 25-line screen: target offset
    shl    bx, 1                         ;                 inner width (grid
    shl    bp, 1                         ;                 cell = two chars)
    sub    si, 18                        ;                 skip each row
@@: sub    bx, bp                        ; 9 grid cells, -1 if in 8-dot mode
    push   di  ;3;                       ; will need it later for ruler
    push   ax  ;4;                       ; height

  ; Draw the border

    push   dx  ;5;                       ; temp, for height
    mov    dx, ax
    mov    al, 0DAh                      ; 'Ú' top-left
    stosw
    call   .draw_hr
    mov    al, 0BFh                      ; '¿' top-right
    stosw
    call   .space_it
    call   .draw_rows
    mov    al, 0C0h                      ; 'À' bottom-left
    stosw
    call   .draw_hr
    mov    al, 0D9h                      ; 'Ù' bottom-right
    stosw
    call   .space_it
    pop    dx  ;4;

  ; Blank out any extra rows on bottom

    pop    ax  ;3;                       ; Font height
    push   ax  ;4;
    mov    cl, 16                        ; Max rows we'd need to blank out
    cmp    bl, 10                        ; Are we in ed50?
    ja     @f                            ;    No, go on
    mov    cl, 33                        ;    yes, set the bigger maximum
@@: sub    cl, al                        ; ACTUAL # of rows to blank out
    inc    cx
    add    bx, bp                        ; Cover for 8-dot extra margins
    add    bl, 4                         ; Cover for ruler too
    sub    si, 4                         ; Cover for ruler when skipping too
    jcxz   .draw_ruler                   ; CX=0? then no rows to blank out
@@: add    di, si
    push   cx  ;5;
    mov    cx, bx
    mov    al, ' '                       ; Blank out the row
    rep    stosw
    pop    cx  ;4;
    loop   @b

  ; Draw the ruler

  .draw_ruler:
    pop    ax  ;3;                       ; height, again
    pop    di  ;2;                       ; Restore initial screen location
    mov    ah, dl                        ; Use ruler attr (note AH <128 !)
    mov    cl, al                        ; Start with font height
    push   cx  ;3;
    cmp    bl, 14                        ; Are we in ed50?
    mov    bl, 2                         ;    - No? extra spacing then
    ja     @f                            ;      ...and go on
    xor    bl, bl                        ;    - Yes? no space for you!
@@: xor    al, al
  .digit:
    add    di, ROW-2*2                   ; skip row in VRAM
    push   ax
    push   cx
    mov    cx, [state.cursor_pos]        ; CL = col, CH = row
    cmp    al, ch                        ; Highlight the digit?
    jne    @f
    mov    ah, dh
@@: call   str_hex_byte
    pop    cx
    pop    ax
    inc    ax                            ; Next digit
    loop   .digit

    lea    di, [di+bx+78*2]
    mov    al, ' ' ;temp                 ; If going from taller->shorter font
    stosw                                ;     erase leftover V-ruler digit
    or     bx, bx                        ; in which screen are we again?
    pushf  ;4;                           ;     (save the answer)
    jnz    @f
    stosw                                ; ED50? - erase 2 digits, and
    add    di, ROW-5*2                   ;    prepare a different position
    add    di, bp                        ; account for 8/9 dot mode
@@: sub    di, bp                        ; account for 8/9 dot mode
    add    di, ROW+4*2

  ; Height caption

    mov    si, txt.height
    call   draw_attr_asc0.x
    popf   ;3;                           ; get our bearings again
    jnz    @f
    inc    di
    inc    di
    sub    di, bp
    sub    di, bp
@@: pop    cx  ;2;                       ; restore font height
    mov    ax, cx
    mov    ah, dh                        ; use
    mov    dh, OP_STOSW                  ; do it w/attributes
    call   str_dec_byte                  ; write ASCII code
    mov    byte[es:di-6], 0              ; delete leading zero

  ; Update the title (decimal ASCII code + character)

    pop    ax  ;1;                       ; retrieve saved attribute for title
    mov    dl, byte[state.currchar]

    mov    di, VRAM_ED50+8*ROW+6*2       ; 50-line screen: r8,c6
    call   .do_it1
    mov    di, VRAM_ED25+4*ROW+8*2       ; 25-line screen: r4,c8 if 9-dot
    test   byte[state.clkmode80], 1      ; 8-dot mode?
    jz     .do_it2                       ; - nope, print already
    dec    di                            ; - yep, back up one cell
    dec    di                            ; ... fall through ...
    jmp    short .do_it2
 .do_it1:
    push   es
 .do_it2:
    mov    al, ' '
    push   ax ;' '
    stosw
    mov    al, dl
    push   ax ;chr
    mov    dh, OP_STOSW                  ; Do it w/attributes
    call   str_dec_byte                  ; Write ASCII code
    mov    al, ':'
    stosw
    mov    al, ' '
    stosw
    pop    ax ;chr
    and    ah, 0F7h                      ; Convert to user font
    stosw
    pop    ax ;' '                       ; ...and back, w/space
    stosw
    pop    es                            ; Restore previous I/O target
    ret

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  .draw_hr:
    mov    al, 0C4h                      ; 'Ä' horizontal line
    mov    cx, bx
    rep    stosw                         ;     fall through here
    ret
  .draw_rows:
    shl    bx, 1                         ; Count bytes (for screen addr.)
    mov    cl, dl                        ; Font height (CH=0 because of prior
    .new_r:                              ;                   drawHR call!)
    add    di, si                        ; Skip before row
    mov    al, 0B3h                      ; '³' vertical line
    stosw
    add    di, bx
    stosw
    push   cx
    call   .space_it
    pop    cx
    loop   .new_r
    add    di, si                        ; Skip after last row
    shr    bx, 1                         ; Count characters again
    ret
  .space_it:                             ; Add spaces after each row if 8-dot
    mov    cx, bp                        ; BP=0? (9-dot mode)
    jcxz   @f                            ;     - do nothing
    mov    al, ' '
    rep    stosw
@@: ret

;-----------------------------------------------------------------------------
  update_rbox_in: ; updates font box CONTENTS on ACTIVE screen
;
; This has to be redone whenever the active font/screen is switched, AFTER
; state.screen is updated but BEFORE the start address is changed
;-----------------------------------------------------------------------------
    push   es  ;-1                       ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    push   bx  ;-2                       ; save attrMap index
    push   es  ;-3
    push   ds
    pop    es                            ; Draw to scartch RAM first for speed
    mov    di, scratch
    push   di  ;-4
    push   di  ;-5

    test   byte[state.currbox], 1        ; Attributes: is charset box active?
    jz     @f                            ; nope - use inactive attributes
    sub    bx, BOXINNER_ATTDIFF          ; yep  - switch to active
@@: xor    ax, ax                        ; Write 256 chars w/plain content attr
    mov    ah, [att.box_OFF_content+bx]
    mov    cx, 256
    push   cx  ;-6
@@: stosw
    inc    ax
    loop   @b

    mov    si, [state.currfont_ptr]      ; which font are we in?
    add    si, font.changes              ; track modified chars w/ [si+bp]
    xor    bp, bp
    mov    al, [att.box_OFF_unsaved+bx]
    pop    cx  ;-5
    pop    di  ;-4
    inc    di                            ; -> odd (attribute)
    push   di  ;-5
.u: test   byte[si+bp], 1                ; character is unsaved?
    jz     @f                            ;    - nope, proceed
    mov    byte[di], al                  ;    - yep, use attr
@@: inc    bp
    inc    di
    inc    di
    loop   .u

    mov    si, state.fntmark             ; do we have a selection?
    lodsb
    or     al, al
    jz     .h                            ;    - no: skip
    lodsw                                ;    - yes: AL=start, AH=end
    mov    cl, al                        ;      CX = first char
    mov    di, cx
    shl    di, 1
    add    di, scratch
    inc    di
    sub    ah, al
    mov    cl, ah
    inc    cx                            ;      # of characters to mark
    mov    al, [att.box_OFF_marked+bx]
@@: stosb
    inc    di
    loop   @b
.h: pop    di  ;-4                       ; character at cursor (hover)
    mov    cl, [state.hoverchar]
    mov    al, [att.box_OFF_currchar+bx]
    shl    cx, 1
    add    di, cx
    stosb

    test   byte[state.attr_modectl], 4   ; LGE on?
    jnz    .w
    mov    di, scratch+0C0h*2
    mov    ax, 0840h
    mov    cx, 32
@@: xor    word[di], ax
    inc    di
    inc    di
    loop   @b

  ; Finished drawing in scratch RAM - copy to the screen:

.w: pop    si  ;-3                       ; SI-> Scratch
    pop    es  ;-2                       ;      Now write to VGA
    mov    di, VRAM_ED25+5*ROW+25*2
    test   byte[state.screen], 1         ; 50-line screen?
    jz     @f
    mov    di, VRAM_ED50+10*ROW+22*2
@@: mov    cx, 16
@@: push   cx
    mov    cl, 16
    rep    movsw
    add    di, (80-16)*2
    pop    cx
    loop   @b
    pop    bx  ;-1                       ; restore attrMap index
    pop    es  ;0                        ; restore previous I/O target
    ret


;-----------------------------------------------------------------------------
  update_rbox_out: ; updates font box border + rulers according to state
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index

    push   bx                            ; save attrMap index
    test   byte[state.currbox], 1        ; is charset box active?
    jz     @f                            ; nope - use inactive attributes
    sub    bx, BOXOUTER_ATTDIFF          ; yep  - switch to active
@@: push   bx                            ; save MODIFIED attrMap index
    mov    al, [att.box_OFF_title+bx]    ; set up inactive attributes
    mov    ah, [att.box_OFF_border+bx]   ; <- initial
    mov    dl, [att.box_OFF_ruler+bx]
    mov    bx, ax                        ; AH, AL attrs now in BH, BL
    mov    di, VRAM_ED25+(4*ROW)+24*2    ; border for 25-line screen
    call   .draw_border
    mov    di, VRAM_ED50+(9*ROW)+21*2    ;     ...for 50-line screen
    call   .draw_border
    mov    si, txt.charset
    push   si
    mov    ah, bl
    mov    di, VRAM_ED25+(4*ROW)+25*2    ; title for 25-line screen
    call   draw_attr_asc0.x
    pop    si
    mov    di, VRAM_ED50+(8*ROW)+22*2    ;    ...for 50-line screen
    call   draw_attr_asc0.x
    mov    ah, dl

    pop    bx                            ; restore MODIFIED attrMap index
    mov    di, VRAM_ED25+(5*ROW)+23*2    ; rulers  for 25-line screen
    call   .draw_rulers
    mov    di, VRAM_ED50+(10*ROW)+20*2   ;      ...for 50-line screen
    call   .draw_rulers
    pop    bx                            ; restore attrMap index
    pop    es                            ; restore previous I/O target
    ret

  .draw_rulers:
    cwd                                 ; reuse DL to determine H/V dir
    mov    dh, byte[state.hoverchar]     ; for checking nybbles against
    mov    cl, 4                         ;     those of current (hover)
    shr    dh, cl                        ;     char; start w/high = row
    .dir:
    xor    al, al
    mov    cl, 16
    .d:
    push   ax
    cmp    dh, al                        ; highlight current nybble?
    jne    @f
    mov    ah, [att.box_OFF_ruler_on+bx]
@@: call   str_hex_nybble
    pop    ax
    test   dl, 1
    jnz    @f
    add    di, 79*2                      ; vertical
    @@:                                  ; horizontal
    inc    ax                            ; next digit
    loop   .d
    inc    dx                            ; next direction:
    add    di, 164                       ; - reposition for H-ruler start
    mov    dh, byte[state.hoverchar]     ; - refresh our nybble to check
    and    dh, 0Fh                       ;   for highlight: low (column)
    cmp    dl, 1                         ; - if vertical, go horizontal
    jna    .dir
    ret

  .draw_border:
    mov    al, 0DAh                      ; 'Ú' top-left
    stosw
    call   .draw_hr
    mov    al, 0BFh                      ; '¿' top-right
    stosw
    call   .draw_rows
    mov    al, 0C0h                      ; 'À' bottom-left
    stosw
    call   .draw_hr
    mov    al, 0D9h                      ; 'Ù' bottom-right
    stosw
    ret
  .draw_hr:
    mov    al, 0C4h                      ; 'Ä' horizontal line
    mov    cx, 16
    rep    stosw
    ret
  .draw_rows:
    mov    al, 0B3h                      ; '³' vertical line
    mov    cl, 16
@@: add    di, (80-18)*2
    stosw
    add    di, 16*2
    stosw
    loop   @b
    add    di, (80-18)*2
    ret

;------------------------------------------------------------------------------
  update_8_9_dot_hilite: ; updates 8/9 dots/cell checkmark according to state
;------------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    mov    si, loc_f_stateful.8_9_dot    ; where are we drawing?  right here
    mov    ax, draw_8_9_dot_hilite       ; how/what are we drawing?
    clc
    call   locate_n_draw_x2              ; do it
    pop    es                            ; restore previous I/O target
   ;ret                                  ; fall through: must update LGE too

;------------------------------------------------------------------------------
  update_lge_hilite:    ; updates Line Gr. Enable checkmark according to state
;------------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    mov    si, loc_f_stateful.lge
    mov    ax, draw_lge_hilite
    clc
    call   locate_n_draw_x2
    pop    es
    ret

;-----------------------------------------------------------------------------
  update_tabs: ; updates both tabs for user fonts
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    mov    ah, [att.normal_left+bx]      ; lower edge first
    mov    al, 0DFh                      ; upper half-block
    mov    di, VRAM_ED25+(2*ROW)         ; for 25-line screen
    mov    cx, 43
    rep    stosw
    mov    di, VRAM_ED50+(4*ROW)         ; for 50-line screen
    mov    cl, 43
    rep    stosw

    mov    si, loc_tab1                  ; point to first tab
    mov    cl, 2
@@: push   cx
    mov    ax, draw_font_tab             ; how/what are we drawing?
    stc
    call   locate_n_draw_x2              ; do it
    pop    cx
    mov    si, loc_tab2                  ; point to second tab (loc_tab2)
    loop   @b
    pop    es                            ; restore previous I/O target
                                         ; AND FALL THROUGH:

;-----------------------------------------------------------------------------
  update_unsaved: ; updates "font unsaved" asterisks on both tabs/screens
;-----------------------------------------------------------------------------
    push   es                            ; I/O target = screen: ES -> VGA text
    call   screen_init_text_IO           ;     mode RAM, BX = attr_map index
    mov    si, loc_tab1unsaved           ; where we gonna draw?
    mov    ax, draw_unsaved              ; how/what to draw?
    push   ax
    stc
    call   locate_n_draw_x2              ; do it
    mov    si, loc_tab2unsaved           ; where we gonna draw?
    pop    ax                            ; how/what to draw?
    stc
    call   locate_n_draw_x2              ; do it again
    pop    es                            ; restore previous I/O target
    ret

;-----------------------------------------------------------------------------
  draw_unsaved: ; draws "font unsaved" asterisk on one tab, both screens
;-----------------------------------------------------------------------------
    push   bp
    mov    bp, font1.unsaved             ; point at font 1's unsaved flag
    xor    al, al                        ; ... and currfont value
    cmp    si, loc_tab2unsaved           ; ... but are we really doing font 1?
    jna    @f
    add    bp, sizeof.font               ; ... no, point at font2's flag
    inc    ax                            ; ... and currfont value
@@: mov    ah, [state.currfont]          ; now see about the current font
    and    ah, 1                         ;     (get rid of irrelevant bits)
    cmp    ah, al                        ; is it the one we're drawing?
    mov    ah, [att.font_ON_unsaved+bx]  ;    - YES, use active attr
    mov    dh, [att.normal_left+bx]
    je     @f
    mov    ah, [att.font_OFF_unsaved+bx] ;    - NO, use inactive attr
    mov    dh, [att.normal_right+bx]
@@: mov    al, '*'                       ; AX: draw for unsaved
    mov    dl, ' '                       ; DX: draw for non-unsaved
    test   byte[bp],1                    ; is the font we're drawing unsaved?
    jnz     @f                           ;    - yep, keep 'em
    xchg   dx, ax                        ;    - no, swap 'em
@@: stosw                                ; DRAW!
    pop    bp
    ret

;-----------------------------------------------------------------------------
  draw_8_9_dot_hilite: ; Draws 8/9-dot checkmark in appropriate location
;-----------------------------------------------------------------------------
    inc    di                            ; let DI be the *attribute*
    mov    dx, di                        ;     location for 8 dots
    add    dx, 4                         ;     and DX for 9 dots
    mov    al, [att.checkmark+bx]
    mov    ah, [att.normal_right+bx]
    test   byte[state.is_bad_vga], 1     ; 9-dot allowed?
    jz     @f
    mov    ah, [att.disabled+bx]
@@: test   byte[state.clkmode80], 1      ; are we in 9-dot mode?
    jnz    @f                            ; - no, go on
    xchg   dx, di                        ; - yes, switch 'em
@@: stosb
    xchg   dx, di
    xchg   ah, al
    stosb
    ret

;-----------------------------------------------------------------------------
  draw_lge_hilite: ; Draws Line Gfx Enable checkmark in appropriate location
;-----------------------------------------------------------------------------
    add    di, 15                        ; "+/-" (attribute)
    mov    dx, di                        ;     location for [+]
    add    dx, 4                         ;     and DX for [-]
    mov    al, [att.checkmark+bx]
    mov    ah, [att.normal_right+bx]
    test   byte[state.clkmode80], 1      ; are we in 9-dot mode?
    jz     @f
    ret                                  ; no; action disabled
@@: test   byte[state.attr_modectl], 4   ; line graphics enabled?
    jnz    @f                            ; - yes, go on
    xchg   dx, di                        ; - no, switch 'em
@@: stosb
    xchg   dx, di
    xchg   ah, al
    stosb
    ret

;-----------------------------------------------------------------------------
  draw_fname:    ; Pretty-print a filename on screen
;
; In:  DS:SI -> filename *pointer*
;      ES:DI -> screen area
;      AH     = attribute
; Out: DS:SI -> NEXT filename pointer
;-----------------------------------------------------------------------------
    push   si  ;1:
    push   dx  ;2;
    push   bx  ;3;
    lea    bx, [di+18]                   ; jump here for file extension
    mov    si, [si]                      ; get pointer
    mov    dl, [si]                      ; get attribute byte
    inc    si                            ; ... to DL

.l: lodsb                                ; start hauling bytes
    cmp    al, 0
    je     .done                         ; end of string?... k bye
    cmp    dl, 4                         ; check our entry's attribute:
    ja     .c                            ; - file (5)? check case + dot
    je     .d                            ; - normal dir (4)? check dot only
@@: stosw                                ; - anything else? print char as-is
    jmp    short .l
.c: cmp    al, 'A'                       ; convert uppercase to lowercase
    jb     .d
    cmp    al, 'Z'
    ja     .d
    or     al, 20h
.d: cmp    al, '.'                       ; do we hve a dot?
    jne    @b                            ;    ... nope - just print
    mov    di, bx                        ;    ... yep - skip to ext. position
    jmp    short .l                      ;        DON'T draw the bloody dot!

  .done:
    cmp    dl, 1                         ; for a drive (1) - show the icon
    jne    @f
    mov    di, bx
    mov    al, DRIVE_ICON
    stosw
    inc    ax
    stosw
    inc    ax
    stosw

@@: pop    bx  ;2;
    pop    dx  ;1;
    pop    si  ;0;
    inc    si                            ; next filename pointer
    inc    si
    ret

;-----------------------------------------------------------------------------
  draw_font_tab:  ; Draws a single upper tab with the font's number and name
;
; In:       ES:DI-> VRAM location(!)
; Destroys: DX, CX
;-----------------------------------------------------------------------------
    xor    dl, dl                        ; let's say we're drawing tab 1
    cmp    si, loc_tab2                  ; ... IS THAT A FACT?
    pushf  ;1;                           ;     if SI > loc_tab2, we're
    jna    @f                            ;     actually drawing tab 2
    inc    dx                            ; now DL = tab we're drawing (0/1)
@@: mov    dh, [state.currfont]          ; ... and DH =  current font (0/1)
    and    dh, 1
    cmp    dh, dl                        ; do they match - is this tab active?
    pushf  ;2;                           ;   (..ZF=0? inactive, 1? active..)
    mov    dx, .plot20_and_inc_row       ;   (..prepare drawing function..)
    je     .active                       ; - yep, we're drawing an active tab
                                         ; - nope, it's an inactive tab
  ;inactive:
    mov    ah, [att.normal_right+bx]
    mov    al, ' '
    call   dx                            ; draw 20 chars, next row
    call   dx
    test   bp, 1                         ; which screen are we in?
    pushf  ;3;                           ;   (..ZF=0? ed25; ZF=1? ed50..)
    jnz    @f                            ; - 25-line: go on
    call   dx                            ; - 50-line: two more rows of spaces
    call   dx
@@: mov    ah, [att.normal_left+bx]
    mov    al, 0DFh                      ; upper half-block for bottom edge
    call   dx
    jmp    short .do_num

  .active:
    mov    ah, [att.normal_left+bx]
    mov    al, 0DFh                      ; upper half-block for top edge
    call   dx
    mov    al, ' '
    call   dx
    test   bp, 1                         ; which screen are we in?
    pushf  ;3;                           ;   (..ZF=0? ed25; ZF=1? ed50..)
    jnz    @f                            ; - 25-line: go on
    call   dx                            ; - 50-line: two more rows of spaces
    call   dx
@@: mov    al, TAB_BORDER
    call   dx

  .do_num:
    sub    di, (80+26)*3                 ; move to location
    popf   ;2;                           ; are we in the 50-line screen?
    jnz    @f                            ; - nope, go on
    sub    di, 80*2                      ; - yep, retreat another row
@@: mov    si, formatted_number
    call   draw_formatted_asc0
    popf   ;1;                           ; is this an active tab?
    jz     .active2                      ; - yep
    mov    ah, [att.font_OFF_name+bx]    ; - nope, prepare name attr for later
    jmp    short .fix_num
  .active2:
    mov    ah, [att.font_ON_name+bx]     ; prepare name attr for later
    mov    al, [att.font_ON_brks+bx]     ; fix attributes for number
    inc    di
    stosb
    sub    di, 5
    stosb
    inc    di
    mov    al, [att.font_ON_num+bx]
    stosb
  .fix_num:
    sub    di, 2
    popf   ;0;                           ; is this tab #2?
    jna    @f
    mov    byte[es:di], '2'              ; yes, turn '1'->'2'
   ;inc    byte[es:di]                   ;     <- was this, but PCem barfed
    mov    si, font2.fname               ; ... and prepare font2 name
    jmp    short .do_name
@@: mov    si, font1.fname               ; no, just prepare font1 name
  .do_name:
    add    di, 6
    call   draw_attr_asc0.x
    ret

  .plot20_and_inc_row:
    mov    cx, 20
    rep    stosw
    add    di, (80-20)*2
    ret

;-----------------------------------------------------------------------------
  draw_caption_line: ; draws "decorative" line for right-side legend caption
;
; In:  ES:DI-> screen location
;-----------------------------------------------------------------------------
    mov    ah, [att.caption_line+bx]
    mov    al, 0C4h                      ; 'Ä' horizontal line
    mov    cl, 37
    rep    stosw
    ret

;-----------------------------------------------------------------------------
  draw_ellipsis: ; draws a '...' using the 'disabled' attribute
;
; In:  ES:DI-> screen location
;         BL = attribute map index for current palette (BH=0)
; Out: ...
;-----------------------------------------------------------------------------
    mov    ah, [att.disabled+bx]
    mov    al, '.'
    stosw
    stosw
    stosw
    ret

;-----------------------------------------------------------------------------
  draw_attr_CX_times:  ; draws ONLY attribute CX times to screen at [ES]:DI
  draw_char_CX_times = _real_DCCX    ;...char  "    "
;
; In: ES:DI-> screen location (EVEN; start at character position)
;        CX = times to repeat attribute or character
;        AL = attribute or character
;-----------------------------------------------------------------------------
@@: inc    di                            ; drawing an attribute? start here!
    _real_DCCX = $                       ; drawing a character? start here!
    stosb
    loop   @b
    ret

;-----------------------------------------------------------------------------
  draw_asc0 = _real_DA0  ; draws plain ASCIIZ string, keeps on-screen attrs
;
; In: ES:DI-> screen location (EVEN; start at character position)
;     DS:SI-> plain string
;-----------------------------------------------------------------------------
@@: stosb                                ; don't start here...
    inc    di
    _real_DA0 = $                        ; ...start HERE!
    lodsb
    cmp    al, 0
    jne    @b
    ret

;-----------------------------------------------------------------------------
  draw_attr_asc0:  ; draws ASCIIZ string with attribute
;
; In: ES:DI-> screen location
;     DS:SI-> attrMap byte, *then* ASCIIZ string
;        BL = attribute map index for current palette (BH=0)

; Call 'draw_attr_asc0.x' if attribute already in AH
;-----------------------------------------------------------------------------
    push   bx
    lodsw
    xchg   ax, bx                        ; BX = attrMap entry
    xlatb                                ; AL = attribute byte...
    mov    ah, al                        ;      ...move to AH
    pop    bx                            ; bring BX back
.x: lodsb
    cmp    al, 0
    jne    @f
    ret
@@: stosw
    jmp    short .x

;-----------------------------------------------------------------------------
  draw_formatted_asc0: ; draws a formatted ASCIIZ string w/control codes
;
; For stateful option labels: call normally if enabled, ".off" if disabled
; In:   ES:DI-> screen location
;       DS:SI-> control-formatted ASCIIZ string
;          BL = attribute map index for current palette (BH=0)
; Uses: CL, CH, DL, DH = relevant attributes
;       BX (backed up) = temp flag-testing space
;-----------------------------------------------------------------------------
    mov    dh, [att.key_slash+bx]        ; stateful option label is ENABLED
    mov    dl, [att.key_brackets+bx]
    mov    ch, [att.key_item+bx]
    mov    cl, [att.normal_right+bx]
    jmp    short @f
  .off:                                  ; stateful option label is DISABLED
    mov    dh, [att.disabled+bx]
    mov    dl, dh
    mov    cx, dx

@@: mov    ah, cl ; default
    xor    bp, bp
    push   bx                            ; save attr_map index
.read:
    lodsb
    cmp    al, 0                         ; null? terminate:
    jne    @f
    pop    bx                            ;   - restore attr_map index
    ret                                  ;   - DONE HERE, GET BACK
@@: cmp    al, 1                         ; 01h? Next char is not a control
    jne    @f                            ;     code - draw it as-is
    lodsb
    jmp    short .norm
@@: test   al, 80h                       ; bit 7: is control code?
    jz     .norm
    test   al, 40h                       ; bit 6: which attribute to set?
    mov    ah, cl                        ;   - CLEAR: normalRight
    jz     @f
    mov    ah, ch                        ;   - SET: keyHighlight
@@: mov    bx, ax                        ; prepare for space-skipping antics
    xor    bh, bh                        ;     (abuse BX)
    and    bx, 1111b                     ;     how many spaces ahead (b0-b3)?
    shl    bx, 1                         ;     *2 to get VRAM char intervals
    test   al, 20h                       ; bit 5: set? just skip 'em
    jz     @f                            ;        clear? see about brackets
    add    di, bx
    jmp    short .read                   ; NEXT
@@: test   al, 08h                       ; bit 5 clear? do brackets crap here
    mov    ah, dl                        ; brackets attribute
    mov    al, '['
    stosw
    mov    al, ']'
    mov    [es:bx+di], ax
    mov    ah, ch                        ; set keyhighlight
    jmp    short .read                   ; NEXT
.norm:
    cmp    al, '/'
    jne    @f
    push   ax
    mov    ah, dh                        ; slashes attribute
    stosw
    pop    ax
    jmp    short .read                   ; NEXT
@@: stosw
    jmp    short .read                   ; NEXT

;-----------------------------------------------------------------------------
  locate_n_draw_x2: ; locates & draws stuff on both screens (25,50)
;
; In:       DS:SI-> Any sort of string with a location word in front
;              AX-> How to draw: function to call after locating
;              CF = Clear for everything but tabs; if set (tabs), location
;                   in 50-line screen (only) will be decremented by 1 row
; Destroys: BP
;-----------------------------------------------------------------------------
    push   ax  ;1;
    push   si  ;2;
    push   ax  ;3;
    pushf      ;4;

    mov    bp, 1
    call   locate ;50
    popf       ;3;
    jnc    @f
    sub    di, ROW
@@: pop    ax  ;2;
    call   ax

    pop    si  ;1;
    call   locate ;25
    pop    ax  ;0;
    call   ax

    ret

;-----------------------------------------------------------------------------
  locate_lbox_cell:  ; Calculates location for grid cell in edit box
;
; In:     AX  = Cell position (low byte = column, high byte = row)
; Out: ES:DI -> VRAM to draw in
;-----------------------------------------------------------------------------
    mov    bp, [state.screen]
    and    bp, 1
    mov    si, loc_pixel00
    push   ax
    call   locate                        ; DI -> top-left corner location
    pop    ax
    inc    bp                            ; back to pre-locate value
    jz     @f
    sub    di, 154                       ; ED50? - adjust TOP-LEFT (0,0) pos.
    jmp    short .n
@@: shl    al, 1                         ; ED25? - make it double
.n: push   bp
    push   di                            ; save top-left corner location
    xor    bp, bp                        ; pretend we're locating a normal
    call   locate.ax_ok                  ;    screen coord. in ED25, then just
    pop    ax                            ;    restore top-left corner location
    add    di, ax                        ;    + add to the offset we just got
    sub    di, VRAM_ED25                 ; ...oh yeah, and take out the trash
    pop    bp
    ret

;-----------------------------------------------------------------------------
  locate:   ; Calculates location for things on both 25 & 50-line UI screens
;
; In:  DS:SI -> Location word (low byte = column, high byte = row)
;         BP  = 0 (SCR_ED25) or 1 (SCR_ED50)
; Out: ES:DI -> VRAM to draw in
;      DS:SI -> Byte *after* location word (e.g. start of string)
;         BP  = BP-1 (!)
;  Destroys:    DX

; Column remains identical for both screens, but the row changes:
; row_ed50 = (2*row_ed25)+1+VRAM_ED50-VRAM_ED25
;-----------------------------------------------------------------------------
    VDIFF  equ VRAM_ED50-VRAM_ED25

    lodsw                                ; AX = location word: AL=col, AH=row
 .ax_ok:                                 ; <- entry point if already got it
    push   dx                            ;      don't destroy
    cwd                                  ; DX = 0 (AH<128, so bit7 never set)
    mov    dl, ah                        ; DX = row
    cbw                                  ; AH = 0 (AL<128, ...); AX = COL
    mov    di, ax                        ; DI = col
    shl    di, 1                         ; DI = col VRAM offset (*2)

  ; Multiply row by 160 = AX*(128+32) = AX<<7+AX<<5:

    mov    ax, dx                        ; AX = DX = row
    mov    cl, 5
    shl    ax, cl                        ; AX = row*32
    mov    cl, 7
    shl    dx, cl                        ; DX = row*128
    add    ax, dx                        ; AX = row*160

    dec    bp                            ; see what screen we're plotting to
    jnz    .do25                         ; !0 (*was* 0)? - ED25
    shl    ax, 1                         ;  0 (*was* 1)? - ED50
    add    ax, ROW+VDIFF                 ;      AX = (2*(row25)+1)*160 + VDIFF
 .do25:
    add    ax, VRAM_ED25
 .fin:
    add    di, ax                        ; DI = col VRAM + row VRAM offsets
    pop    dx
    ret