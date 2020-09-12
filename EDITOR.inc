; All functions called here should assume ES = DS = CS on entry

;-----------------------------------------------------------------------------
  ed_get_key: ; Get a keystroke, set some hepful regs + figure out what to do
;-----------------------------------------------------------------------------

  HND_OFFSET  equ  (key_handlers-key_codes)
  NUM_KEYS    equ  (HND_OFFSET)/2
  NUM_FKEYS   equ  (key_handlers-file_keys)/2

    call   update_stateful               ; redraw all stateful option labels

    push   cs
    pop    es                            ; I/O target = base segment vars
    mov    ah, [state.int16h_func]       ; use 10h if supported, otherwise 0
    int    16h                           ; get key: AH=scancode, AL=ASCII

    call   str_upper_al                  ; lowercase to uppercase
    mov    cl, ah
    and    cl, 0E0h                      ; zero AL (ascii) for all 4#h/5#h
    cmp    cl, 40h                       ;    scancodes, for simpler handling
    jne    @f
    xor    al, al
@@: cmp    ah, 73h                       ; do the same for ctrl+arrow combos
    jb     @f
    xor    al, al

@@: push   ax
    call   screen_status_bar             ; clean it up on every keypress
    pop    ax
    xchg   ax, bx                        ; BX = keycode
    mov    ah, 2
    int    16h                           ; get shift flags (AL)
    mov    ah, byte[state.currbox]

    push   ax ;!;
    test   al, byte[state.drag_flag]     ; RShift / Ctrl pressed?
    jz     .setdrag                      ;    - not dragging anything
    call   font_get_char_row             ;    - dragging, BUT WHAT? (destroys
    test   al, [state.cursor_mask]       ;      AX, hence the push/pop)
    mov    bp, di                     ;>>> BP-> char row (if we care)
    mov    ah, 1
    mov    al, byte[state.drag_flag]
    jnz    .setdrag
    dec    ah
 .setdrag:                               ; DRAGGING?: AL bit0=1, AH=draggee;
    mov    word[state.drag], ax          ; otherwise: AL bit0=0, AH=don't care
    pop    ax ;!;

    xchg   ax, bx                     ;>>> AX=key, BL=shift flags, BH=currbox
    mov    di, key_codes
    mov    cx, NUM_KEYS
    repne  scasw                         ; found keycode in the list?
    jne    ed_get_key                    ;    - nah, try again
    mov    dx, word[di+HND_OFFSET-2]     ;    - yeah, get handler address
    mov    si, [state.currfont_ptr]   ;>>> SI-> font structure
    test   bh, 1                      ;>>> ZF-> box ID check
    mov    [fdlg.actkey], ax          ;>>> store key for file dialog too
    call   dx                            ; do it
    jmp    short ed_get_key              ; NEXT!

  align 2

  key_codes:
    dw     0F09h                         ; Tab
    dw     011Bh                         ; Esc
    dw     0231h                         ; 1
    dw     0332h                         ; 2
    dw     4800h                         ; Up arrow
    dw     4B00h                         ; Left arrow
    dw     4D00h                         ; Right arrow
    dw     5000h                         ; Down arrow
    dw     8D00h                         ; Ctrl+Up arrow (*)
    dw     7300h                         ; Ctrl+Left arrow (*)
    dw     7400h                         ; Ctrl+Right arrow (*)
    dw     9100h                         ; Ctrl+Down arrow (*)
    dw     3920h                         ; Space
    dw     1C0Dh                         ; Enter
    dw     0D2Bh                         ; +
    dw     4E00h                         ; + (modified)
    dw     0C2Dh                         ; -
    dw     4A00h                         ; - (modified)
    dw     1245h                         ; E
    dw     2146h                         ; F
    dw     2247h                         ; G
    dw     1749h                         ; I
    dw     1F53h                         ; S
    dw     1157h                         ; W
    dw     2D58h                         ; X
    dw     1559h                         ; Y
    dw     1E01h                         ; Ctrl+A
    dw     2E03h                         ; Ctrl+C
    dw     2004h                         ; Ctrl+D
    dw     2207h                         ; Ctrl+G
    dw     1312h                         ; Ctrl+R
    dw     2F16h                         ; Ctrl+V
    dw     2D18h                         ; Ctrl+X
    dw     2C1Ah                         ; Ctrl+Z
    dw     3E00h                         ; F4
    dw     3F00h                         ; F5
    dw     4000h                         ; F6
    dw     4100h                         ; F7
    dw     4200h                         ; F8
    dw     4300h                         ; F9
    dw     4400h                         ; F10
    dw     2B7Ch                         ; |
    dw     0C5Fh                         ; _
    dw     8000h                         ; (Ctrl+)Alt+9
    file_keys:
    dw     3C00h                         ; F2
    dw     1F13h                         ; Ctrl+S
    dw     3D00h                         ; F3
    dw     260Ch                         ; Ctrl+L
    dw     1709h                         ; Ctrl+I
    dw     1205h                         ; Ctrl+E

  key_handlers:
    dw     ed_set_box                    ; Tab
    dw     ed_key_esc                    ; Esc
    dw     ed_set_font                   ; 1
    dw     ed_set_font                   ; 2
    dw     ed_key_up                     ; Up arrow
    dw     ed_key_left                   ; Left arrow
    dw     ed_key_right                  ; Right arrow
    dw     ed_key_down                   ; Down arrow
    dw     ed_key_up                     ; Ctrl+Up arrow (*)
    dw     ed_key_left                   ; Ctrl+Left arrow (*)
    dw     ed_key_right                  ; Ctrl+Right arrow (*)
    dw     ed_key_down                   ; Ctrl+Down arrow (*)
    dw     ed_key_action                 ; Space
    dw     ed_key_action                 ; Enter
    dw     ed_next_char                  ; +
    dw     ed_next_char                  ; + (modified)
    dw     ed_prev_char                  ; -
    dw     ed_prev_char                  ; - (modified)
    dw     ed_block.erase                ; E
    dw     ed_block.fill                 ; F
    dw     ed_goto_char                  ; G
    dw     ed_block.invert               ; I
    dw     ed_slide                      ; S
    dw     ed_swap                       ; W
    dw     ed_block.flip_x               ; X
    dw     ed_block.flip_y               ; Y
    dw     ed_mark_all                   ; Ctrl+A
    dw     ed_copy                       ; Ctrl+C
    dw     ed_unmark                     ; Ctrl+D
    dw     ed_get_rom                    ; Ctrl+G
    dw     ed_revert                     ; Ctrl+R
    dw     ed_paste                      ; Ctrl+V
    dw     ed_copy                       ; Ctrl+X
    dw     ed_undo                       ; Ctrl+Z
    dw     ed_height                     ; F4
    dw     ed_one_line                   ; F5
    dw     ed_one_line                   ; F6
    dw     ed_one_line                   ; F7
    dw     ed_dot_mode                   ; F8
    dw     ed_palette                    ; F9
    dw     ed_preview                    ; F10
    dw     ed_guide.v                    ; |
    dw     ed_guide.h                    ; _
    dw     ed_cheat_code                 ; (Ctrl+)Alt+9
    ;file_keys:
    dw     file_dialog.with_fname        ; F2
    dw     file_dialog.with_fname        ; Ctrl+S
    dw     file_dialog                   ; F3
    dw     file_dialog                   ; Ctrl+L
    dw     file_dialog                   ; Ctrl+I
    dw     ed_export                     ; Ctrl+E


                ;++++++++++++++++++++++++++++++++++++++++++++
                ;##########    HELPER FUNCTIONS    ##########
                ;++++++++++++++++++++++++++++++++++++++++++++

;-----------------------------------------------------------------------------
  ed_do_lbox:  ; Update editbox (char+cursor + outside - NO NEED TO DO GRID)
;-----------------------------------------------------------------------------
    call   update_lbox_char              ; Active screen only (+ cursor)
    call   update_lbox_out               ; Active screen only (+ title)
    ret

;-----------------------------------------------------------------------------
  ed_do_rbox:  ; Update fontbox (inside + outside - call .i for innards only)
;-----------------------------------------------------------------------------
    call   update_rbox_out               ; Updates both screens
.i: call   update_rbox_in                ; Active screen only
    ret

;-----------------------------------------------------------------------------
  ed_get_fntmark: ; Check if fontbox mark exists; if not, make one at cursor
;-----------------------------------------------------------------------------
    mov    di, state.fntmark
    test   byte[di], 1                    ; Fontbox mark exists?
    jnz    @f                             ; - Yes, keep it
    push   di
    push   ax
    mov    al, byte[di]
    inc    ax                             ; - No: set fntmark to 1
    stosb
    pop    ax                             ;   [state.hoverchar]
    stosb                                 ;   + fntamrk_start = "
    stosb                                 ;   + fntmark_end   = "
    pop    di
@@: inc    di                             ; DI -> fntmark_start
    ret

;-----------------------------------------------------------------------------
  ed_get_chrmark: ; Check if editbox mark exists; if not, make one at cursor
;-----------------------------------------------------------------------------
    mov    dl, [state.cursor_mask]
    mov    di, state.chrmark_mask
    cmp    byte[di], 0                   ; Do we have an existing mark?
    jne    @f                            ; - Yes: keep it
    mov    byte[di], dl                  ; - No: set chrmark=cursor mask
    push   ax
    push   bx
    mov    al, [state.cursor_pos+1]          ; current row
    cbw
    mov    bx, ax
    mov    byte[state.chrmark_rows+bx],-1    ; gotta have it filled in :/
    std                                      ; -about-face-
    dec    di
    stosb                                    ;   _rng_v FROM = current row
    stosb                                    ;   _rng_v TO   = current row
    mov    al, [state.cursor_pos]            ; current column
    stosb                                    ;   _rng_h FROM = current col
    stosb                                    ;   _rng_h TO   = current col
    cld                                      ; -flip dir again-
    pop    bx
    pop    ax
@@: mov    di, state.chrmark_rng_h
    ret

;-----------------------------------------------------------------------------
  ed_chrmark_update_v: ; Update editbox mark after vertical move
;
; In: AX = new cursor row post-move
;     DI-> low byte of _rng_h (TO)
;     DH = JA if moved down, JAE if moved up
;     BX = 0  if moved down,   1 if moved up
;-----------------------------------------------------------------------------
    mov    byte[.op], dh                 ; Modify conditional jump
    jmp    short @f                      ; prevent prefetch issues
@@: inc    di
    inc    di                            ; DI-> _rng_v, "TO" row
    cmp    al, byte[di+bx]               ; New row vs "FROM"(up) / "TO"(down)
  .op:                                   ;   ( ">" if down, ">=" if up)
    jae    @f                            ; - Yep, set "to" row...
    inc    di                            ; - Nope, set "from" row...
@@: stosb                                ; ...to cursor row
    xor    ax, ax                        ; Now fill up chrmark_rows according
    mov    di, state.chrmark_rows        ;     to vertical range
    mov    bx, [state.chrmark_rng_v]     ; BH = "from" row, BL = "to" row
    inc    bl                            ;                  ^ flipping point
    mov    cl, 32                        ; Max height
  .do_row:                               ; AL = row mask = 0; AH = row counter
    cmp    ah, bh                        ;     Reached flip point?
    jne    @f                            ;     - No, keep current mask
    xchg   bh, bl                        ;     - Yes, flip from/to check
    not    al                            ;       and flip that mask too
@@: stosb                                ;     Set row to mask
    inc    ah                            ;     Counter++
    loop   .do_row                       ; Next row
    ret

;-----------------------------------------------------------------------------
  ed_chrmark_update_h: ; Update editbox mark after horizontal move
;
; In: AX = new cursor column post-move
;     DI-> low byte of _rng_h (TO)
;     DH = JA if moved right, JAE if moved left
;     BX = 0  if moved right,   1 if moved left
;-----------------------------------------------------------------------------
    mov    byte[.op], dh                 ; Modify conditional jump
    jmp    short @f                      ; prevent prefetch issues
@@: cmp    al, byte[di+bx]               ; New col vs "FROM"(<-) / "TO"(->)
  .op:                                   ;   ( ">" if right, ">=" if left)
    jae    @f                            ; - Yep, set "to" column...
    inc    di                            ; - Nope, set "from" column...
@@: stosb                                ; ...to cursor column
    xor    ax, ax                        ; Start w/empty mask + counter
    mov    dx, 8000h                     ; DL = no bits on, DH = MSB on
    mov    bx, [state.chrmark_rng_h]     ; BH = "from" col, BL = "to" col
    inc    bl                            ;                  ^flipping point
    mov    cl, 8
  .do_col:                               ; AL = column bit; AH = counter
    cmp    ah, bh                        ;     Reached flip point?
    jne    @f                            ;     - No, keep current bit value
    xchg   bh, bl                        ;     - Yes, flip from/to check
    xchg   dh, dl                        ;       and flip that mask too
@@: or     al, dl                        ;     Set column bit
    ror    dx, 1                         ;     Rotate
    inc    ah                            ;     Counter++
    loop   .do_col                       ; Next column
    mov    di, state.chrmark_mask
    stosb                                ; Writing the result MIGHT HELP
    ret

;-----------------------------------------------------------------------------
  ed_check_drag:  ; Check after cursor has moved - if dragged, apply pixel
;-----------------------------------------------------------------------------
    mov    si, state.dragged_last
    mov    ax, [state.drag]              ; OK, cursor moved; check drag status
    test   al, byte[state.drag_flag]     ; did we drag anything?
    jnz    @f
    mov    byte[si], 0                   ; - no: make note for next time
    jmp    ed_do_lbox                    ;   and just update cursor in lbox
@@: test   byte[si], 1                   ; - yes: is this a new drag op?
    jnz    @f                            ;   * no: don't touch undo buffer
    inc    byte[si]                      ;   * yes: adjust for next check
    call   font_update_undo_buf          ;          and update undo state
@@: mov    al, [state.cursor_mask]       ;   get bit corresponding to cursor
    test   ah, 1                         ;   check draggee
    jz     @f                            ;   pixel ON?
    or     byte[bp], al                  ;   - yes, add it (BP -> target)
    jmp    short .end                    ;     ...aaand done
@@: not    al                            ;   - no, pixel OFF
    and    byte[bp], al                  ;     ...subtract it
    .end:                                ; FALL THROUGH

;-----------------------------------------------------------------------------
  ed_apply_change: ; Note that font/current char have changed + update screen
;-----------------------------------------------------------------------------
    mov    bx, [state.currchar]          ; if called here, only current char
.1: mov    cx, 1                         ;     has changed
@@: mov    si, [state.currfont_ptr]
    lea    di, [bx+si+font.changes]      ; update changes array
    mov    al, 1
    rep    stosb
    mov    [si+font.unsaved], al         ; set unsaved flag
    call   update_unsaved                ; show it on screen, too
    call   update_lbox_char              ; update character in editbox
    stc                                  ; on-screen vertical centering ON
    call   vga_upload_font               ; upload the font
    jmp    ed_do_rbox.i                  ; let's display the changes as well
  .many:
    mov    ax, [state.fntmark_start]     ; if called here, all marked chars
    xchg   ah, al                        ;     have changed
    xor    bx, bx
    mov    bl, ah                        ; BX: from
    xor    ah, ah                        ; AX: to
    sub    ax, bx
    inc    ax
  .pasted_many:                          ; called here? AX=#chars, BX=currchar
    xchg   ax, cx                        ; CX: number of chars to do
    jmp    short @b                      ; so go do it
  .hover:
    mov    bx, [state.hoverchar]
    jmp    short .1

;-----------------------------------------------------------------------------
  ed_warn_unsaved:  ; Create 'lose changes' prompt in scratch + show it
;
; In:  CF = method: 0 = current font, 1 = both fonts (before exit)
; Out: CF = answer: 0 = NO, 1 = YES
;-----------------------------------------------------------------------------
    mov    di, scratch
    push   di
    pushf
    mov    si, txt.lose
    call   str_copy_asc0
    mov    al, '1'
    test   byte[state.currfont], 1
    jz     @f
    inc    ax
@@: dec    di
    stosb
    mov    si, txt.yes_no
    popf
    jnc    @f
    sub    di, 6
    mov    si, txt.both_fonts
@@: call   str_copy_asc0
    pop    si                            ; SI -> scratch (message)
    mov    ah, 1111b                     ; +wipe +getkey +cursor +warning
    call   screen_status_prompt
    and    al, 11011111b                 ; got ASCII in AL; lower -> upper
    cmp    al, 'Y'
    clc
    jne    @f
    stc
@@: pushf
    call   screen_status_bar
    popf
    ret


              ;++++++++++++++++++++++++++++++++++++++++++++++
              ;##########    KEYPRESS FUNCTIONS    ##########
              ;++++++++++++++++++++++++++++++++++++++++++++++

;-----------------------------------------------------------------------------
  ed_set_box: ; Change focus between edit/font box (optional) and redraw them
;-----------------------------------------------------------------------------
    not    byte[state.currbox]           ; swap the little suckers
    jmp    short .noinit
  .init:                                 ; call here on init (no switching)
    mov    ax, [state.currchar]
    mov    [state.hoverchar], ax         ; a little conceit
  .noinit:                               ; call here to keep curr/hover chars
    call   ed_do_rbox
    call   ed_do_lbox
    call   update_box_legend             ; box-specific caption+line+labels
    ret

;-----------------------------------------------------------------------------
  ed_set_font: ; Sets active font to either 1 or 2, depending on keystroke
;-----------------------------------------------------------------------------
    sub    ah, 2                         ; 0 = font 1, 1 = font 2
    mov    al, byte[state.currfont]
    and    al, 1
    cmp    ah, al                        ; selected font == current one?
    jne    @f
    ret                                  ; - yep, nothing to see here
@@: not    byte[state.currfont]          ; - nope, flip it...
    cmp    al, 0                         ;   but to what?
   ;aaa                                  ;     <- was this, but PCem barfed
    mov    ax, font1
    jnz    @f
    mov    ax, font2
@@: mov    [state.currfont_ptr], ax      ; ...and fall through:

;-----------------------------------------------------------------------------
  ed_switch_to_font: ; Does some heavy lifting, call after active font was set
;-----------------------------------------------------------------------------
    mov    si, [state.currfont_ptr]
    cmp    byte[si], 16                  ; height of current font
    mov    di, state.screen
    mov    al, SCR_ED25                  ; - short enough? do the 25-row thing
    jbe    @f
    mov    al, SCR_ED50                  ; - too tall? go 50
@@: stosb
    call   ed_set_box.noinit             ; just draw whatever is needed
    call   update_lbox_grid
    call   update_tabs
    call   vga_switch_screen             ; everything ready? pull the switch
    ret

;-----------------------------------------------------------------------------
  ed_key_up: ; Handle up arrow key   (for both edit & font boxes)
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    al, [state.hoverchar]
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_fntmark                ; - Yes, make sure we have a mark
@@: sub    al, 10h
    mov    [state.hoverchar], al
    popf   ;!;                           ; Left shift pressed?
    jz     .fin                          ; - No, skip mark stuff
    cmp    al, byte[di]                  ; - Yes, test 'cursor' vs. mark start
    jb     @f                            ;      below? - set start
    inc    di                            ;      above? - set end
@@: stosb                                ;      ...to 'cursor'
    .fin:
    jmp    ed_do_rbox
  .editbox:
    mov    al, [state.cursor_pos+1]      ; Get *current* row
    cbw                                  ; AX =   "
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_chrmark                ; - Yes, make sure we have a mark
@@: dec    bp                            ; MOVE: ptr to new row (for drag)
    dec    al                            ; New row = current row - 1
    jns    @f                            ; SF set? - we've wrapped around
    lodsb                                ; AL <= byte[si] = font height
    add    bp, ax                        ; Wrap pointer to bottom
    dec    ax                            ; Wrap to bottom: new row = height-1
@@: mov    [state.cursor_pos+1], al
    popf   ;!;                           ; Left shift pressed?
    jz     @f                            ;   - No, skip mark stuff
    mov    bx, 1                         ;   - Yes: set up for mark update
    mov    dh, OP_JAE                    ;        * indicate we've moved UP
    call   ed_chrmark_update_v           ;        * update mark (vertical)
@@: jmp    ed_check_drag                 ; Now do the drag check

;-----------------------------------------------------------------------------
  ed_key_down: ; Handle down arrow key (for both edit & font boxes)
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    al, [state.hoverchar]
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_fntmark                ; - Yes, make sure we have a mark
@@: add    al, 10h
    mov    [state.hoverchar], al
    popf   ;!;                           ; Left shift pressed?
    jz     .fin                          ; - No, skip mark stuff
    cmp    al, byte[di+1]                ; - Yes, test 'cursor' vs. mark end
    jbe    @f                            ;      below? - set start
    inc    di                            ;      above? - set end
@@: stosb                                ;      ...to 'cursor'
    .fin:
    jmp    ed_do_rbox
  .editbox:
    mov    al, [state.cursor_pos+1]      ; Get *current* row
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_chrmark                ; - Yes, make sure we have a mark
@@: inc    bp                            ; Ptr to new row (for dragging)
    inc    ax
    cmp    al, byte[si]                  ; Compare to char height
    jb     @f
    xor    al, al                        ; >=height? - we've wrapped around
    sub    bp, [si]                      ; Wrap pointer to top, too
@@: mov    [state.cursor_pos+1], al
    popf   ;!;                           ; Left shift pressed?
    jz     @f                            ;   - No, skip mark stuff
    mov    bx, 0                         ;   - Yes: set up for mark update
    mov    dh, OP_JA                     ;        * indicate we've moved DOWN
    call   ed_chrmark_update_v           ;        * update mark (vertical)
@@: jmp    ed_check_drag                 ; Now do the drag check

;-----------------------------------------------------------------------------
  ed_key_left: ; Handle left arrow key (for both edit & font boxes)
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    al, [state.hoverchar]
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_fntmark                ; - Yes, make sure we have a mark
@@: test   al, 0Fh
    jnz    @f
    add    al, 10h                       ; Position x0? wrap around to xF
@@: dec    ax                            ; Otherwise move left
    mov    [state.hoverchar], al
    popf   ;!;                           ; Left shift pressed?
    jz     .fin                          ; - No, skip mark stuff
    cmp    al, byte[di]                  ; - Yes, test 'cursor' vs. mark start
    jb     @f                            ;      below? - set start
    inc    di                            ;      above? - set end
@@: stosb                                ;      ...to 'cursor'
    .fin:
    jmp    ed_do_rbox
  .editbox:
    mov    al, [state.cursor_pos]        ; Get *current* column
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_chrmark                ; - Yes, make sure we have a mark
@@: dec    ax                            ; New column = current-1
    rol    byte[state.cursor_mask], 1
    jnc    @f
    mov    al, 7                         ; CF set? - we've wrapped around
@@: mov    [state.cursor_pos], al
    popf   ;!;                           ; Left shift pressed?
    jz     @f                            ;   - No, skip mark stuff
    mov    bx, 1                         ;   - Yes: set up for mark update
    mov    dh, OP_JAE                    ;        * indicate we've moved LEFT
    call   ed_chrmark_update_h           ;        * update mark (horizontal)
@@: jmp    ed_check_drag                 ; Now do the drag check

;-----------------------------------------------------------------------------
  ed_key_right: ; Handle right arrow key (for both edit & font boxes)
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    al, [state.hoverchar]
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_fntmark                ; - Yes, make sure we have a mark
@@: inc    ax                            ; Move right
    test   al, 0Fh
    jnz    @f
    sub    al, 10h                       ; Position x0? wrap around
@@: mov    [state.hoverchar], al
    popf   ;!;                           ; Left shift pressed?
    jz     .fin                          ; - No, skip mark stuff
    cmp    al, byte[di+1]                ; - Yes, test 'cursor' vs. mark end
    jbe    @f                            ;      below? - set start
    inc    di                            ;      above? - set end
@@: stosb                                ;      ...to 'cursor'
    .fin:
    jmp    ed_do_rbox
  .editbox:
    mov    al, [state.cursor_pos]        ; Get *current* column
    test   bl, 2                         ; Left shift pressed?
    pushf  ;!;                           ;     (save answer)
    jz     @f                            ; - No, go on
    call   ed_get_chrmark                ; - Yes, make sure we have a mark
@@: inc    ax                            ; New column = current+1
    ror    byte[state.cursor_mask], 1
    jnc    @f
    xor    al, al                        ; CF set? - we've wrapped around
@@: mov    [state.cursor_pos], al
    popf   ;!;                           ; Left shift pressed?
    jz     @f                            ;   - No, skip mark stuff
    mov    bx, 0                         ;   - Yes: set up for mark update
    mov    dh, OP_JA                     ;        * indicate we've moved RIGHT
    call   ed_chrmark_update_h           ;        * update mark (horizontal)
@@: jmp    ed_check_drag                 ; Now do the drag check

;-----------------------------------------------------------------------------
  ed_guide: ; Set horizontal/vertical guide on/off at current cursor position
;-----------------------------------------------------------------------------
  .v:
    jnz    @f                            ; proceed only in editbox
    mov    al, [state.cursor_mask]
    xor    byte[state.guide_cols], al
    jmp    short .do_it
  .h:
    jnz    @f                            ; proceed only in editbox
    mov    al, [state.cursor_pos+1]      ; row only
    cbw
    xchg   ax, bx
    not    byte[state.guide_rows+bx]
  .do_it:
    call   update_lbox_grid
@@: ret

;-----------------------------------------------------------------------------
  ed_one_line:  ; Delete, insert, or duplicate line at cursor position
;                 across the entire font
;
; In:   AX = 3F00h (delete), 4000h (insert), 4100h (duplicate)
;-----------------------------------------------------------------------------
    xchg   al, ah
    xchg   ax, bp                        ; BP = keycode
    push   si
    push   si
    lodsb                                ; AL = font height
    lea    si, [bp+.actions]             ; check restriction
    cmp    al, [si]                      ; height OK for requested action?
    jne    @f                            ;    - yes, proceed
    add    sp, 4                         ;    - no, laters (tidy stack first)
    ret
@@: call   font_update_undo_buf
    pop    di                            ; DI -> current font's height
    mov    byte[di+font.unsaved], 1      ; note that we're making changes
    mov    ah, [si+3]                    ; get desired height modification
    add    al, ah                        ; apply it
    stosb                                ; make it so
    mov    al, ah
    cbw                                  ; AX =  -1 (del) or 1 (ins/dup)
    inc    ax
    jz     @f
    dec    ax                            ; AX =  0 (del) or 1 (ins/dup)
@@: mov    bl, [state.cursor_pos+1]
    xor    bh, bh                        ; BX =  cursor row
    xor    dl, dl                        ; DL =  counter
    mov    dh, [si+6]                    ; DH =  value to AND target line with
    lea    si, [font.data-1+bx+di]       ; SI -> desired line in char #0
  .loop_it:
    push   si
    push   si
    mov    cl, 32                        ; (CH = 0 from undo update)
    sub    cx, bx
    mov    di, scratch
    push   di
    rep    movsb                         ; copy next 32b to scratch
    pop    si                            ; SI -> scratch
    pop    di                            ; DI -> desired char line
    and    [di], dh                      ; clear it (if INSerting a blank)
    add    di, ax                        ; DI +=  0 (del) or 1 (ins/dup)
    inc    si
    sub    si, ax                        ; SI +=  1 (del) or 0 (ins/dup)
    mov    cl, 32
    sub    cx, bx
    sub    cx, ax
    rep    movsb                         ; get lines back to where we want 'em
    pop    si
    add    si, 32                        ; next char
    inc    dl                            ; counter++
    jnz    .loop_it                      ; iterated through all 256 chars?

    mov    di, scratch                   ; compose status message:
    push   di
    shl    bp, 1
    mov    si, [.texts+bp]               ; - say what we've done
    call   str_copy_asc0
    dec    di
    mov    si, txt.finishline            ; - finish that sentence
    mov    byte [si+20], 'f'
    call   str_copy_asc0
    pop    si
    call   screen_status_msg

    pop    di
    call   font_post_proc.after_height   ; done: do the dance
    ret

  .actions = $-3Fh
         ; Del,   Ins,   Dup             ; DATA TABLE FOR ACTIONS
    db      1,    32,    32              ; <- verboten font heights
    db     -1,     1,     1              ; <- height adjustment to make
    db     -1,     0,    -1              ; <- to AND w/line at cursor position

  .texts = $-(3Fh*2)
    dw     txt.del_line
    dw     txt.ins_line
    dw     txt.dup_line

;-----------------------------------------------------------------------------
  ed_height:  ; Prompt for a new font height and set it
;-----------------------------------------------------------------------------
    push   si ;1;
    push   si ;2;
    mov    si, txt.set_height
    mov    ah, 1000b                     ; +wipe -getkey -cursor -warning
    call   screen_status_prompt
    pop    si ;1;
    mov    bl, byte[si]                  ; BL = new height
  .get_it:
    mov    di, scratch
    push   di ;2;
    mov    al, bl
    mov    dh, OP_STOSB
    call   str_dec_byte                  ; write height as 3-digit decimal
    mov    al, 2                         ; CLOSING markup: 'select' attr OFF!
    push   ax ;3;
    stosb
    mov    si, txt.lines
    call   str_copy_asc0
    xor    al, al
    stosb                                ; add terminating zero
    pop    ax ;2;                        ; leading "0" can go play in traffic
    pop    di ;1;                        ; so we can replace it with:
    stosb                                ; OPENING markup: 'select' attr ON!
    mov    si, scratch
    mov    di, 48
    push   bx ;2;
    mov    ah, 0100b                     ; -wipe +getkey -cursor -warning
    call   screen_status_prompt          ; out: AX = keycode
    pop    bx ;1;
    cmp    al, 1Bh                       ; ESC?
    jne    @f
    pop    si ;0;
    jmp    short .get_out
@@: cmp    ah, 48h                       ; Increase? (up arrow)
    jne    @f
    cmp    bl, 32
    jae    .get_it
    inc    bx
    jmp    short .get_it
@@: cmp    ah, 50h                       ; Decrease? (down arrow)
    jne    @f
    cmp    bl, 1
    jna    .get_it
    dec    bx
    jmp    short .get_it
@@: cmp    al, 0Dh                       ; Enter?
    jne    .get_it
  .got_it:
    pop    si ;0;                        ; SI -> current font height
    cmp    bl, byte[si]                  ; new height = current height?
    je     .get_out                      ; nothing to see - move along
    push   si ;1;
    call   font_update_undo_buf
    pop    di ;0;
    mov    byte[di], bl
    mov    byte[di+font.unsaved], 1
    call   font_post_proc.after_height   ; SANITIZE & DISPLAY
    call   screen_status_bar
    mov    si, txt.done_h
    mov    byte [si], 'F'
    call   screen_status_msg
    ret
  .get_out:
    call   screen_status_bar
    ret

;-----------------------------------------------------------------------------
  ed_preview:    ; Generate preview screen, show it, return
;----------------------------------------------------------------------------
    mov    di, state.preview             ; retrieve column mode:
    push   di  ;!;
    mov    ax, [di]                      ; get low byte (memorized value)
    mov    ah, al                        ;     to high byte (current value)
    mov    [di], ax                      ; store setting

  .draw:
    call   screen_gen_preview            ; redraw page (bonus: BP = height!)
    call   vga_init_preview              ; go through some boring VGA setup

  .get_key:
    mov    ah, 1100b                     ; +wipe +getkey -cursor -warning
    mov    si, txt.pvw_bar               ; status bar w/keystroke grab: do we
    call   screen_status_prompt          ;    want to redecorate, or get out?
    cmp    ah, 48h                       ; UP ARROW:
    jne    @f
    mov    ax, 1001h                     ;     ... foreground++
    jmp    short .got_it
@@: cmp    ah, 50h                       ; DOWN ARROW:
    jne    @f
    mov    ax, 7887h                     ;     ... foreground--
    jmp    short .got_it
@@: cmp    ah, 4Dh                       ; RIGHT ARROW:
    jne    @f
    mov    ax, 0110h                     ;     ... background++
    jmp    short .got_it
@@: cmp    ah, 4Bh                       ; LEFT ARROW:
    jne    @f
    mov    ax, 8778h                     ;     ... background--
    jmp    short .got_it
@@: cmp    ah, 44h                       ; F10:
    jne    @f
    neg    byte[state.preview+1]         ;     ... flip 40/80 column value
    call   vga_init_preview              ;     ... make it happen
    jmp    short .get_key
@@: cmp    ah, 14h                       ; T:
    jne    .get_out
    call   .get_text                     ;     ... ask for custom text
    jmp    short .get_key

  .got_it:
    mov    di, preview_attr
    add    word[di], ax                  ; store our fancy new color scheme
    and    word[di], 7777h               ; but keep it in a sane range
    call   screen_gen_preview.known_h    ; now, apply it
    jmp    short .get_key

  .get_out:
    call   vga_end_preview               ; restore VGA to an agreeable state
    call   font_post_proc.after_undo     ; back to editor + re-pad fonts
    call   screen_status_bar             ; decontaminate status row

    pop    di  ;!;                       ; point at state.preview... again
    mov    ax, [di]                      ; going out of preview:
    mov    al, ah                        ; shift hi byte (curr. value) to low
    xor    ah, ah                        ; byte (memorize) + clear hi byte
    mov    [di], ax                      ; store setting
    ret                                  ; that's all folks

  .get_text: ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    push   bp                            ; PRESERVE FONT HEIGHT
    mov    ah, 1000b                     ; +wipe -getkey -cursor -warning
    mov    si, txt.pvw_input             ; status bar -
    call   screen_status_prompt          ;    show some friendly instructions

    mov    si, pvw.pangram
    push   si
    memcp  si, scratch+8, 20             ; make a backup
    mov    bx, pvw_txtlen
    mov    ax, [bx]
    mov    [di], ax                      ; memorize old length too
    pop    di                            ; overwrite this
    mov    bp, di
    add    di, [bx]

.k: call   update_preview_inp            ; show what we've got first
    xor    ax, ax                        ; grab a key
    int    16h
    cmp    ax, 4B00h                     ; left arrow?
    je     .erase
    cmp    ah, 0Eh                       ; backspace?
    je     .erase
    cmp    ah, 01h                       ; esc?
    je     .cancel
    cmp    ah, 1Ch                       ; enter? = DONE
    je     .end_input
    cmp    al, 0                         ; ignore non-ASCII keystrokes
    je     .k

    cmp    di, pvw.pangram+40            ; first: are we at end of string?
    je     @f                            ; - yes: point to previous
    inc    byte[bx]                      ; - no:  character count ++
    inc    di                            ;        + don't retreat
@@: dec    di
    stosb
    jmp    short .k                      ;    - now show and get another

  .erase:
    cmp    di, bp                        ; are we at the beginning?
    je     .k                            ; ...do nothing
    dec    di
    mov    byte[di], ' '                 ; overwrite previous char w/space
    dec    byte[bx]                      ; ...character count --
    jmp    short .k

  .cancel:
    memcp  scratch+8, bp, 20             ; ESC pressed? - restore old string
    mov    ax, [si]                      ; ...and the length too
    mov    [bx], ax
  .end_input:
    pop    bp
    call   update_preview_inp
    ret

;----------------------------------------------------------------------------
  ed_swap:       ; Swap selected character w/fontbox cursor (hover) character
;----------------------------------------------------------------------------
    mov    al, [state.currchar]
    mov    ah, [state.hoverchar]
    cmp    al, ah
    jne    @f                            ; same char? do nothing
    ret

@@: push   ax  ;1;                       ; save character codes
    mov    bp, .copy
    call   font_point_to_char            ; SI-> font;
    mov    bx, di                        ; BX-> currchar
    mov    dx, si                        ; DX-> font
    mov    si, di
    mov    di, scratch
    push   di  ;2;                       ; current char @ scratch
    call   bp
    mov    si, dx
    call   font_point_to_char.hover      ; SI-> font; DI-> hoverchar
    push   di  ;3;                       ; hover char @ font
    mov    si, di
    mov    di, bx
    call   bp                            ; current char <= hoverchar
    pop    di  ;2;
    pop    si  ;1;
    call   bp                            ; hover char <= copy of current char

    mov    di, txt.swapped + 6           ; compose message
    mov    dh, OP_STOSB
    pop    ax  ;0;                       ; AL = character code #1
    mov    bh, ah                        ; BH = character code #2
    call   str_dec_byte
    inc    di
    mov    al, bh
    call   str_dec_byte
    mov    si, txt.swapped               ; report
    call   screen_status_msg

    call   ed_apply_change               ;   ONE
    jmp    ed_apply_change.hover         ;   TWO

  .copy:
    memcp  si, di, 16                    ; single char
    ret

;-----------------------------------------------------------------------------
  ed_goto_char:  ; Prompt for a character, select it, redraw boxes
;-----------------------------------------------------------------------------
    mov    si, txt.what_char
    mov    ah, 1110b                     ; +wipe +getkey +cursor -warning
    call   screen_status_prompt          ; out: AX = keycode
    cmp    ah, 01h                       ; was it ESC?
    je     @f                            ; ...back away slowly
    mov    byte[state.currchar], al
@@: call   screen_status_bar
    jmp    short @f

;-----------------------------------------------------------------------------
  ed_next_char:  ; Increment current char and redraw both edit/font boxes
;-----------------------------------------------------------------------------
    inc    byte[state.currchar]
    jmp    short @f

;-----------------------------------------------------------------------------
  ed_prev_char:  ; Decrement current char and redraw both edit/font boxes
;-----------------------------------------------------------------------------
    dec    byte[state.currchar]
@@: jmp    ed_set_box.init

;-----------------------------------------------------------------------------
  ed_mark_all:   ; Select entire editbox, or entire font
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    di, state.fntmark
    stosb                                ; AL=01 (ASCII code for Ctrl+A)
    dec    ax                            ; state.fntmark_start = 0
    stosb
    dec    ax                            ; state.fntmark_end = 255
    stosb
    call   update_rbox_in
    ret
  .editbox:
    mov    di, state.chrmark_rng_h
    mov    ax, 7                         ; H. range: 0..7
    stosw
    lodsw                                ; AX = word[si] = height (hi byte 0)
    mov    cx, ax                        ; CX = height
    dec    ax
    stosw                                ; V. range: 0..height-1
    mov    al, 0FFh
    inc    cx                            ; count=CX+1 (_mask + CX*height rows)
    rep    stosb
    call   update_lbox_char
    ret

;-----------------------------------------------------------------------------
  ed_unmark:   ; DEselect entire editbox, or entire font
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    di, state.fntmark
    xor    al, al
    stosb                                ; fntmark
    stosb                                ; fntmark_start
    stosb                                ; fntmark_end
    jmp    ed_do_rbox.i                  ; repaint innards only
  .editbox:
    mov    di, state.chrmark_rng_h       ; start with h. range
    xor    ax, ax
    mov    cx, 37                        ; (_rng_h to + from, rng_v to + from,
    rep    stosb                         ;      + _mask + 32 rows)
    call   update_lbox_char
    ret

;-----------------------------------------------------------------------------
  ed_key_action: ; Space / Enter - draw and select stuff
;-----------------------------------------------------------------------------
    jz     .editbox
  .fontbox:
    mov    ax, [state.hoverchar]
    mov    [state.currchar], ax          ; set
    jmp    ed_do_lbox
    ret
  .editbox:
    call   font_update_undo_buf          ; copy to undo buffer
    call   font_get_char_row             ; AL=row mask, DI->row, SI=DI+1(!)
    xor    al, [state.cursor_mask]       ; flip that bit
    stosb                                ; DO IT
    jmp    ed_apply_change

;-----------------------------------------------------------------------------
  ed_slide:    ; Enter 'slide' mode w/status prompt: cursor keys reposition
;                character bitmap(s); edges wrap around
;-----------------------------------------------------------------------------
    push   si ;1;
    pushf
    mov    si, txt.slide
    mov    ah, 1000b                     ; +wipe -getkey -cursor -warning
    call   screen_status_prompt          ; show the prompt
    mov    dx, 1                         ; assume one char to do
    mov    di, state.tw                  ; prepare post-action call location
    popf
    jnz    .fontbox
    pop    si ;0;                        ; in editbox? - SI -> font struct
    mov    word[di], ed_apply_change     ; - post-action call = current char
    call   font_point_to_char            ; - DI -> current char
    jmp    short .common
  .fontbox:                              ; in fontbox? we may have more
    mov    si, state.fntmark             ; see if there's any selection
    lodsb
    cmp    al, 0
    jne    @f
    pop    si ;0;                        ; NO: point to font struct again
    mov    word[di], \                   ; - post-action call = apply change
           ed_apply_change.hover         ;   to highlighted character
    call   font_point_to_char.hover      ; - DI -> highlighted character
    jmp    short .common
@@: lodsb                                ; got mark? - get first char to do
    xor    ah, ah
    mov    dx, ax
    lodsb                                ; ...and the last one
    xchg   ax, dx
    sub    dx, ax                        ; AL = first char;
    inc    dx                            ; DX = number of chars to do
    pop    si ;0;                        ; point to font struct again
    mov    word[di], \                   ; - post-action call = apply changes
           ed_apply_change.many          ;   to the entire marked range
    call   font_point_to_char.in_al
  .common:
    mov    bx, [si]
    dec    bx                            ; BX = height-1
    xor    bp, bp                        ; indicate nothing's been done yet

  .get_key:
    xor    ax, ax
    int    16h                           ; get us a keystroke
    push   di  ;1;
    mov    di, .keys
    mov    cx, 4
    repne  scasw                         ; scan the list
    je     @f
    pop    di  ;0;
    call   screen_status_bar             ; non-cursor key? - bye bye
    ret
@@: or     bp, bp                        ; otherwise, check: first action?
    jnz    @f                            ;     - no: keep undo buffer intact!
    push   di                            ;     - yes:
    call   font_update_undo_buf          ;           update undo buffer
    pop    di
@@: mov    cx, bx                        ; - BX = last row, so:
    inc    cx                            ; - CX = number of char rows
    add    di, (4-1)*2                   ; - where's our action?
    mov    bp, [di]                      ; - here
    pop    di  ;0;                       ; - restore char pointer
    push   dx  ;1;                       ; - save char count + first char
    push   di  ;2;
  .go:                                   ; DO IT:
    push   cx  ;3;                       ;    DI -> first char   BP -> action
    call   bp                            ;    BX =  height-1     CX =  height
    pop    cx  ;2;                       ;    DX =  counter
    add    di, 32                        ; next char
    dec    dx                            ; any more to go?
    jnz    .go                           ; - yep, loop
    push   bx  ;3;                       ; - nope:
    push   bp  ;4;
    mov    ax, [state.tw]                ;   ... apply changes
    call   ax
    pop    bp  ;3;
    pop    bx  ;2;
    pop    di  ;1;                       ;   ... restore count + first char
    pop    dx  ;0;

    jmp    short .get_key                ; ...and repeat

  .horiz: ;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    push   di
.l: cmp    ah, 4Bh
    jne    .r
    rol    byte[di], 1                   ; slide left
    jmp    short @f
.r: ror    byte[di], 1                   ; slide right
@@: inc    di
    loop   .l                            ; next!
    pop    di
    ret

  .vert:  ;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    push   bx                            ; save number of last line
    push   di                            ;     and the pointer to line #0

    push   di
    mov    si, di
    mov    di, scratch
    push   di
    push   cx
    rep    movsb                         ; make a temp copy in scratch
    pop    cx
    pop    si                            ; SI -> temp copy
    pop    di                            ; DI -> line #0 of original char

    cmp    ah, 48h
    jne    .d
    mov    al, [di]                      ; UP:   memorize line #0
    inc    si                            ;       read from temp copy +1
    jmp    short @f                      ;       (BX->wrap target: last line)
.d: mov    al, [di+bx]                   ; DOWN: memorize last line
    dec    si                            ;       read from temp copy -1
    xor    bx, bx                        ;       BX -> wrap target: line #0
@@: rep    movsb                         ; slide 'em
    pop    di                            ; restore pointer to line #0
    mov    [di+bx], al                   ; copy memorized line to wrap target
    pop    bx                            ; restore number of last line
    ret                                  ; all done

             ; Up       Left     Right    Down
  .keys:   dw  4800h,   4B00h,   4D00h,   5000h
  .dirs:   dw  .vert,   .horiz,  .horiz,  .vert

;-----------------------------------------------------------------------------
  ed_block:  ; Perform block operation on character, range, or marked area
;
; Operation subroutines are callable directly - .fill, .erase, .invert,
; .flip_x, or.flip_y (so they can mooch off the same code)
;-----------------------------------------------------------------------------
  .flip_y:
  .flip_x:
  .invert:
  .fill:
    mov    dl, -1                        ; indicate a fill OR invert op
    jmp    short @f                      ;    (preserve ZF)
  .erase:
    mov    dl, 0                         ; indicate an erase op (preserve ZF)
@@: cbw                                  ; AX = ASCII only (preserve ZF)
    mov    bp, ax                        ; BP = ascii code of keystroke
    call   font_update_undo_buf          ; copy to undo buffer
                                         ;    (leaves the flags unmolested)
    jz     .editbox

  .fontbox:   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    push   si
    mov    si, state.fntmark             ; see if there's any selection
    lodsb
    cmp    al, 0
    jne    @f
    pop    si                            ; point to font struct again
    call   .do_hover                     ; no mark? - do hoverchar
    jmp    ed_apply_change.hover
@@: lodsb                                ; got mark? - get first char to do
    xor    ah, ah
    mov    cx, ax
    lodsb                                ; ...and the last one
    xchg   ax, cx
    sub    cx, ax
    inc    cx                            ; CX = number of chars to do
    pop    si                            ; point to font struct again
@@: push   cx
    push   ax
    call   .do_chr_in_al
    pop    ax
    inc    ax
    pop    cx
    loop   @b
    jmp    ed_apply_change.many

  .editbox:   ; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    mov    dh, [state.chrmark_mask]
    cmp    dh, 0
    jne    @f
    call   .do_curr                      ; no mark? - do the full thing
  .one_done:
    jmp    ed_apply_change

  ; Editbox has selection: apply op to marked area only

@@: test   dl, 0FFh                      ; got mark? - see if we erase or fill
    jnz    @f                            ;    filling? keep mark as-is
    not    dh                            ;    erasing? invert it for deletion
@@: call   font_point_to_char
    mov    ax, [state.chrmark_rng_v]     ; mark rows; from (HI), to (LO)
    mov    cl, al
    sub    cl, ah
    inc    cx                            ; CX = number of rows to do
    xchg   al, ah
    cbw
    add    di, ax                        ; DI = start

  ; Determine operation + execute

.r: cmp    bp, 'I'                       ; what exactly are we doing again?
    je     .i
    jna    @f
    mov    bx, [state.chrmark_rng_h] ;!; ; +++FLIPPING+++
    call   .fxy
    jmp    short .one_done
@@: test   dl, 0FFh
    jnz    @f
    and    byte[di], dh                  ; +++ERASING+++
    jmp    short .next
@@: or     byte[di], dh                  ; +++FILLING+++
    jmp    short .next
.i: xor    byte[di], dh                  ; +++INVERTING+++
  .next:
    inc    di
    loop   .r
    jmp    short .one_done

  .fxy:                                  ; if we're here - must be flip-X/Y
    push   di ;1;                        ;     <- start row in original char
    mov    si, di
    mov    dl, dh                        ; so, DH = chrmark_mask
    not    dl                            ; and DL = its inverse
    mov    di, scratch                   ; first copy ONLY the relevant rows
    push   cx                            ;     to scratch space
@@: lodsb
    and    al, dh                        ; keep only marked bits in our copy
    and    byte[si-1], dl                ; + create mark-shaped hole in source
    stosb
    loop   @b
    pop    cx
    dec    di                            ; now DI -> last row in copy
    mov    si, di                        ;  ...SI ->  "
    std                                  ; turn around for next string ops!

    cmp    bp, 'Y'                       ; +++FLIPPING Y+++
    jne    .fx
  .finish_up:
    pop    di ;0;                        ; DI -> original char's start row
@@: lodsb                                ; refill mark-shaped hole in the
    or     [di], al                      ;     original char w/wanted data
    inc    di                            ;     (flip-x op jumps here too!)
    loop   @b
    cld
    ret

  .fx:                                  ; +++FLIPPING X+++
    mov    dx, bx                        ; ...mark h. range (HI=from, LOW=to)
    add    dh, dl                        ; ...find # bits to shift flipped
    sub    dh, 7                         ;        results: FROM_col+TO_col-7
    push   cx ;2;
    .rev_n_shift:
    push   cx ;3;                        ; ...we're walking backwards: let's
    call   .reverse_8                    ; ...flip row in-place (SI--, DI--!)
    add    cl, dh                        ; ...CL now 0, so *add* shift value
    jns    @f                            ;        for quick sign-checking
    neg    cl                            ; ...shift value is negative?
    shl    byte[si+1], cl                ;    - flip sign and shift LEFT
    jmp    short .X
@@: shr    byte[si+1], cl                ;    - otherwise shift RIGHT
.X: pop    cx ;2;
    loop   .rev_n_shift
    cld                                  ; ...stop walking backwards
    pop    cx ;1;
    inc    si                            ; ...now SI -> x-flipped start row
    jmp    short .finish_up              ;        E-X-E-C-U-T-E

  ; Working on a full character - apply op to all of it - - - - - - - - - - -

  .do_curr:                             ; ++++++ FULL CHAR OPS - LOOP +++++++
    call   font_point_to_char
    jmp    short @f
  .do_hover:
    call   font_point_to_char.hover
    jmp    short @f
  .do_chr_in_al:
    call   font_point_to_char.in_al
@@: mov    cx, [si]                      ; number of rows = font height
    cmp    bp, 'I'
    je     .op_invert
    ja     .op_flip
    mov    al, dl                        ; FILL  (DL=ff)  /  ERASE  (DL=0)
    rep    stosb
    ret
  .op_invert:                            ; INVERT (background <-> foreground)
    xor    byte[di], dl
    inc    di
    loop   .op_invert
    ret
  .op_flip:
    push   si
    mov    dh, -1                        ; "it's a FAKE:" ...h. mark
    mov    bx, 7                         ;                ...h. range
    call   .fxy
    pop    si
    ret

  .reverse_8:
    mov    cl, 8
    xor    bl, bl
    lodsb
@@: ror    al, 1
    rcl    bl, 1
    loop   @b                            ; Careful with that CL
    xchg   ax, bx
    stosb
    ret

;-----------------------------------------------------------------------------
  ed_dot_mode: ; Flip VGA clock mode between 8/9 dots/cell, update highlight
;-----------------------------------------------------------------------------
;@: xor    ah, [state.clock_mode_80]     ; is the new mode same as now?
;   test   ah, 1                         ; let's have a look, here's how
;   jz     @f                            ; new value? - yep, allow
;   ret                                  ; the same? - do nothing, ciao

    test   byte[state.is_bad_vga], 1     ; is 9-dot mode permitted?
    jz     @f                            ; all good, let's go and hit it
    mov    si, txt.bad_vga               ; a good VGA ain't fitted?
    call   screen_status_msg             ; no go, we're screwed - admit it
    ret
@@: call   vga_set_clock_mode            ; dot-mode switch - throw it
    call   update_checkmark              ; and let the user know it
    call   vga_mod_UI_glyphs             ; upload new glyphs, don't blow it
    call   update_lbox_grid              ; redraw the grid to show it
    jmp    ed_do_lbox

;-----------------------------------------------------------------------------
  ed_cheat_code:  ; Let DOSBox users bypass the better-judgment check, and
;                   allow 9-dot mode even if not configured for "vgaonly"
;                   (I guess this is good for future-proofing...)
;-----------------------------------------------------------------------------
    test   bl, 100b                      ; ctrl key pressed?
    jz     @f
    mov    byte[state.is_bad_vga], al    ; zero out the problem flag
@@: ret

;-----------------------------------------------------------------------------
  ed_palette: ; Change palette, rebuild entire frickin' UI, print message
;-----------------------------------------------------------------------------
    mov    bx, state.palette             ; BX -> current palette number
    push   bx                            ;    (keep the location)
    mov    bp, str_copy_asc0             ; we'll be calling this one a lot
    mov    di, scratch                   ; compose message in scratch space
    push   di                            ;    (keep this location too):
    mov    si, txt.set_pal
    call   bp
    dec    di
    mov    si, txt.updown
    call   bp
    dec    di
    mov    al, ' '
    stosb
    mov    ax, [bx]                      ; - what's the palette number again?
    inc    ax                            ; - convert zero-based -> one-based
    push   di                            ; - save location of color start
    mov    dh, OP_STOSB                  ;    don't want any attributes
    call   str_dec_byte
    mov    al, 2                         ; - color end
    stosb
    mov    ax, '-'*256+' '
    stosw
    stosb
    mov    al, 1                         ; - open highlight
    stosb
    mov    ax, [bx]
    shl    ax, 1                         ; - use palette num as word index
    xchg   ax, bx
    mov    si, [pal_list+bx]   ;;ADD     ; - look up the pointer
    add    si, PAL_GETNAME     ;;EM      ; - ASCIIZ name string
    xchg   ax, bx
    call   bp
    dec    di
    xor    al, al                        ; - terminating zero
    stosb
    pop    di
    mov    al, 2
    stosb

    pop    si                            ; point to scratch space as source
    mov    ah, 1100b                     ; +wipe +getkey -cursor -warning
    call   screen_status_prompt          ; out: AX = keycode
    pop    bx                            ; BX -> state.palette
    mov    dx, [bx]

    cmp    al, 1Bh                       ; check keystroke: ESC?
    jne    @f
    jmp    short .get_out
@@: cmp    al, 0Dh                       ; Enter?
    jne    @f
    jmp    short .get_out
@@: cmp    ah, 48h                       ; Increase? (up arrow)
    jne    @f
    cmp    dl, NUM_PALETTES-1
    je     .r
    inc    dx
    jmp    short .apply
@@: cmp    ah, 50h                       ; Decrease? (down arrow)
    jne    .r
    or     dl, dl
    jz     .r
    dec    dx

  .apply:
    mov    [bx], dx                      ; store the new palette number
    call   vga_set_palette               ; do the VGA magic
    call   screen_gen_editor             ; redraw the whole lot
    call   update_stateful
.r: jmp    ed_palette                    ; ...and repeat

  .get_out:
    call   screen_status_bar
    ret

;-----------------------------------------------------------------------------
  ed_revert:  ; Revert to saved (reopen file associated with current font)
;-----------------------------------------------------------------------------
    test   byte[state.can_revert], 1
    jnz    @f
    ret
@@: mov    bp, si                        ; BP -> font structure
    clc                                  ; indicate current font only
    call   ed_warn_unsaved
    jc     @f                            ; user backed out?
    ret                                  ; ABORT!

@@: lea    dx, [bp+font.fspec]           ; use truename (no need to re-parse!)
    call   dos_open_n_check              ; if OK: CF clear, BX = file handle,
    jnc    @f                            ;        SI -> filespec, AX = size
    mov    si, scratch                   ; failed? show scary warning prompt
    mov    ah, 1101b                     ; +wipe +getkey -cursor +warning
    call   screen_status_prompt
    ret
@@: push   bp ;1;                        ; opened successfully
    call   font_opened                   ; set some stuff, READ & CLOSE FILE
    pop    di ;0;                        ; DI -> font structure
    jmp    font_post_proc.after_revert   ; SANITIZE & DISPLAY

;-----------------------------------------------------------------------------
  ed_get_rom:  ; Grab one of the VGA fonts we stored in top segment
;-----------------------------------------------------------------------------
    push   si ;1;
    test   byte[si+font.unsaved], 1
    jz     @f
    clc                                  ; indicate current font only
    call   ed_warn_unsaved
    jc     @f                            ; user backed out?
    pop    si ;0;                        ; tidy stack
    ret                                  ; ABORT!
@@: mov    si, txt.load_rom              ; nice, normal, non-scary prompt
    mov    ah, 1000b                     ; +wipe -getkey -cursor -warning
    call   screen_status_prompt
    mov    si, .menu
    call   screen_status_menu            ; out: DX = option number
    pop    di ;0;                        ; DI -> font structure
    jnc    @f
    ret                                  ; ESC was pressed? - chicken out
@@: push   di ;1;
    mov    al, dl
    mov    bx, .height
    xlatb
    stosb                                ; font.height = AL
    dec    di                            ; be kind, rewind
    add    di, font.data                 ; ES:DI -> target in base seg
    mov    si, .fdata
    mov    bx, dx
    shl    bx, 1                         ; *2 for word lookup
    mov    si, [si+bx]                   ; get word ptr from .fdata
    push   ds ;2;
    mov    ds, [seg_top]                 ; DS:SI -> source in seg_top
    memcp  si, di, 4096                  ; DO IT
    pop    ds ;1;
    mov    si, txt.loaded_rom            ; spell it out
    call   screen_status_msg
    pop    di ;0;                        ; DI -> font structure
    jmp    font_post_proc                ; SANITIZE & DISPLAY

 .menu:    dw 5
           db ' 8x8',   '8x14',   '9x14',   '8x16',   '9x16'
 .height:  db 8,        14,       14,       16,       16
 .fdata:   dw top.8x8,  top.8x14, top.9x14, top.8x16, top.9x16

;-----------------------------------------------------------------------------
  ed_export:  ; Let user select export format before jumping to dialog
;-----------------------------------------------------------------------------
    push   si  ;1;
    mov    si, txt.format                ; show prompt
    mov    ah, 1000b                     ; +wipe -getkey -cursor -warning
    call   screen_status_prompt
    mov    si, .menu                     ; ...and menu
    call   screen_status_menu            ; out: DX = option number
    jc     .bye

    mov    [fdlg.export_fmt], dx
    or     dx, dx                        ; selection is 0 (.com)?
    jnz    @f
    mov    si, txt.fmt_com               ; - show yet another prompt
    mov    ah, 1000b                     ;   +wipe -getkey -cursor -warning
    call   screen_status_prompt
    mov    si, .com_mnu                  ; - and another menu
    call   screen_status_menu            ; - out: DX = option number
    jc     .bye
    mov    [fdlg.export_com], dx         ; - store result
@@: pop    si ;0;                        ; SI -> font structure
    mov    byte[fdlg.allow_new], 1       ; export: new file creation allowed
    jmp    file_dialog.safe

 .bye:
    pop    si ;0;                        ; tidy stack
    ret                                  ; ESC was pressed? - chicken out

 .menu:    dw 3
           db '.COM',   '.BMP',   'XBIN'
 .com_mnu: dw 4
           db 'None',   '40c ',   '80c ',    'Both'

;-----------------------------------------------------------------------------
  ed_undo:  ; Restore current font from undo buffer
;-----------------------------------------------------------------------------
    test   byte[si+font.can_undo], 1
    jnz    @f
    ret
@@: push   si                            ; (1) save segments + font pointer
    push   es                            ; (2)   "
    push   ds                            ; (3)   "
    push   cs
    pop    es                            ; target = base
    mov    ds, [seg_upper]               ; source = upper, for undo
    mov    di, si                        ; DI -> current font
    mov    si, upper.font1_undo
    cmp    di, font1
    je     @f
    mov    si, upper.font2_undo
@@: memcp  si, di, UNDO/2
    pop    ds                            ; (3) restore segments + font pointer
    pop    es                            ; (2)   "
    pop    si                            ; (1)   "
    xor    al, al
    mov    byte[si+font.can_undo], al    ; THOU HAST FORFEITETH THINE UNDO!
    mov    byte[state.dragged_last], al  ; make immediate new drag op undoable
    jmp    font_post_proc.after_undo     ; SANITIZE & DISPLAY

;-----------------------------------------------------------------------------
  ed_copy:  ; Copy selected block (editbox) or range (fontbox) to clipboard
;-----------------------------------------------------------------------------
    push   ax ;1;
    push   si ;2;
    pushf     ;3;
    test   byte[state.can_copy], 1
    jnz    @f
    add    sp, 6                         ; NO COPY FOR YOU - tidy stack
    ret                                  ; and get outta here
@@: popf   ;2;
    jz     .editbox

  .fontbox:
    pushf  ;3;
    mov    ax, [state.fntmark_start]     ; AL: from, AH: to
    push   ax  ;4;
    call   font_point_to_char.in_al      ; DI -> row 0 of first char
    mov    si, di                        ; SI ->  "
    pop    ax  ;3;
    sub    ah, al
    mov    al, ah
    xor    ah, ah
    inc    ax                            ; AL = # chars to copy
    mov    di, state.clip_type
    mov    byte[di], 1                   ; .clip_type: indicate it's a range
    mov    word[di+1], ax                ; .clip_len:  how long is it?
    mov    word[di+3], 0                 ; .clip_dim:  can't paste in editbox!
    mov    cl, 5
    shl    ax, cl                        ; AX = # BYTES to copy
    xchg   ax, cx
  .do_it:
    push   es  ;4;
    mov    es, [seg_upper]
    xor    di, di                        ; ES:DI -> upper.clipboard
    rep    movsb                         ; EXECUTE
    pop    es  ;3;
    popf       ;2;                       ; done - do we have to cut?
    pop    si  ;1;
    pop    ax  ;0;
    pushf      ;1;
    cmp    al, 18h                       ; ...check for ctrl-X
    jne    @f
    popf       ;0;
    call   ed_block.erase                ; ...yep, kill 'em all
    jmp    short .msg
@@: popf       ;0;
  .msg:
    mov    si, txt.stored
    call   screen_status_msg             ; be talkative
    ret

  .editbox:
    pushf  ;3;
    call   font_point_to_char            ; DI -> row 0 of current char
    mov    si, state.chrmark_rng_h
    lodsw
    xchg   ax, dx                        ; DX = mark cols: from (HI), to (LO)
    lodsw                                ; AX = mark rows: from (HI), to (LO)
    xor    bx, bx
    mov    bl, ah
    lea    si, [di+bx]                   ; SI -> first row of marked area
    mov    cl, al
    sub    cl, ah
    inc    cx                            ; CL = # of rows (bytes to copy)
    mov    di, state.clip_type
    xor    ax, ax
    stosb
    stosw                                ; .clip_type and _len = 0
    sub    dl, dh
    inc    dx
    mov    al, dl                        ; .clip_dim (LO) = number of columns
    mov    ah, cl                        ; .clip_dim (HI) = number of rows
    stosw
    mov    al, [state.chrmark_mask]
    stosb                                ; .clip_mask = column *mask*
    jmp    .do_it                        ; go on - common code from here

;-----------------------------------------------------------------------------
  ed_paste:   ; Paste from clipboard
;-----------------------------------------------------------------------------
    pushf  ;1;
    test   byte[state.can_paste], 1
    jnz    @f
    popf   ;0;                           ; DENIED - tidy stack
    ret                                  ; ...and bye
@@: call   font_update_undo_buf          ; allow undo (keeps flags unmolested)
    popf   ;0;
    jz     .editbox

  .fontbox:
    push   si  ;1;
    call   font_point_to_char.hover      ; DI -> row 0 of hover(!) char
    lea    dx, [si+font.padding2]        ; DX -> limit (end of font data)
    mov    cx, [state.clip_len]          ; CX =  number of chars to dump
    mov    bx, [state.hoverchar]         ; BX =  where we start from
    xor    ax, ax                        ; AX =  counter
    push   ds  ;2;
    mov    ds, [seg_upper]
    xor    si, si                        ; DS:SI -> upper.clipboard
@@: push   cx  ;3;
    mov    cx, 16
    rep    movsw                         ; copy one char (32b)
    pop    cx  ;2;
    inc    ax                            ; count up!
    cmp    di, dx                        ; reached limit?
    jae    @f
    loop   @b                            ; no, go on
@@: pop    ds  ;1;
    pop    di  ;0;
    push   ax
    push   bx
    call   font_cleanup                  ; in case source was a taller font
    pop    bx
    pop    ax
    jmp    ed_apply_change.pasted_many   ; AX=counter, BX=hoverch.: we're good

  .editbox:
    mov    ch, [si]                      ; CH = font height
    call   font_get_char_row             ; DI -> current char row at cursor
    mov    si, state.clip_dim
    lodsw                                ;
    xchg   ax, bx                        ;      BH = row count,
    lodsb  ;.clip_mask                   ;      BL = column count
    mov    ah, al                        ; AH = column *mask* of clip
    mov    dh, al
    mov    dl, [state.cursor_pos]        ; DL = column at cursor
    xor    bp, bp
@@: rol    dh, 1                         ;      wiggle it
    inc    bp
    jnc    @b
    rcr    dh, 1
    dec    bp                            ; BP = first col of orig clip mask
    mov    cl, dl
    shr    dh, cl                        ; DH = shifted clip mask
    mov    cl, bh                        ; CL = row count
    mov    al, [state.cursor_pos+1]      ;      check cursor row
    add    al, cl
    cmp    al, ch                        ;      never exceed font height!
    jna    @f
    sub    al, ch
    sub    cl, al                        ; CL = adjusted row count
@@: xor    ch, ch                        ; CX =  "
    mov    bh, dh
    not    bh                            ; BH = inverted shifted clip mask
    push   ds  ;1;
    mov    ds, [seg_upper]
    xor    si, si                        ; DS:SI -> upper.clipboard

@@: mov    bl, [es:di]                   ; grab current char row
    and    bl, bh                        ; punch a hole in it
    lodsb                                ; get clip row
    and    al, ah                        ; isolate the relevant bits
    xchg   cx, bp
    shl    al, cl                        ; wiggle it!
    xchg   cx, bp
    xchg   cl, dl
    shr    al, cl                        ; wiggle it some more!
    xchg   cl, dl
    or     al, bl                        ; fill hole with clip...
    stosb
    loop   @b

    pop    ds  ;0;
    jmp    ed_apply_change

;-----------------------------------------------------------------------------
  ed_key_esc: ; (in editor) - checks for usnaved files, handles exit flow
;-----------------------------------------------------------------------------
    mov    [state.tw], bl                ; memorize shift flags
    push   si  ;1;                       ; remember active user font
    mov    cx, 0101h
    xor    ch, [font1.unsaved]           ; byte(!)
    xor    cx, [font2.unsaved]           ; word(!)
    jnz    @f
    stc                                  ; both fonts unsaved, indicate that
    jmp    short .warn                   ;    (and don't switch fonts either)
@@: cmp    cl, ch                        ; CH=CL=1? => both fonts saved
    je     .can_quit
    mov    ah, 2                         ; only one is unsaved, gotta switch
    test   ch, 1                         ; is font 1 our culprit?
    jz     @f
    inc    ah                            ; no; then it's obviously font 2
@@: call   ed_set_font                   ; make the switch
    clc                                  ; indicate current font only
  .warn:
    call   ed_warn_unsaved
    pop    si  ;0;
    jc     .can_quit                     ; CF set? = user selected YES
    ret                                  ; CF clear? = chickened out

  .can_quit:                             ; QUIT
    mov    ax, 3
    int    10h
    test   byte[state.tw], 2             ; was Left Shift pressed w/ESC?
    jz     @f                            ; nah:, skip the font-setting

  ; Leave last active user font on screen

    mov    di, scratch                   ; Squeeze the bloody thing first!
    call   font_squeeze                  ; ES:BP -> sqzd data; CX=256 (#chars)
    xor    bl, bl                        ; BL = block to load in map 2
    mov    bh, al                        ; BH = bytes per character pattern
    mov    ax, 1110h                     ; LOAD USER-SPECIFIED PATTERNS
    cwd                                  ; DX = char offset into map 2 block
    int    10h

@@: ; Fall through to good_exit (WITHOUT cleaning up the stack)

