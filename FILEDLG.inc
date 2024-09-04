; All functions called here should assume ES = DS = CS on entry *unless noted*

;-----------------------------------------------------------------------------
  file_dialog:  ; Prepare and show navigation dialog for file operations

; In:      word[fdlg.actkey] = the key code that brought us here
;          SI -> font structure
;-----------------------------------------------------------------------------
    mov    byte[fdlg.allow_new], 0       ; load/import: can't create new files

  ; Check and warn if unsaved changes exist

    test   byte[si+font.unsaved], 1      ; SI -> font struct (from caller)
    jz     .safe
    push   si  ;1;
    clc                                  ; indicate current font only
    call   ed_warn_unsaved
    pop    si  ;0;
    jc     .safe                         ; user approved? good!
    ret                                  ; user backed out? ABORT!

  ; If we're saving, inject current font's filename into input field

  .with_fname:
    mov    byte[fdlg.allow_new], 1       ; save: new file creation allowed
    push   si  ;1;
    add    si, font.fname
    cmp    byte[si], '<'                 ; keep empty if "<NoName>"
    je     @f
    mov    di, fdlg.input
    mov    bx, di
    call   str_copy_asc0
    sub    di, bx
    dec    di
    mov    [fdlg.in_len], di
@@: pop    si  ;0;

  ; Initialize some values

  .safe:                                 ; no checks needed (op=save/export)
    push   si  ;1;                       ; store font pointer
    call   screen_gen_files              ; draw the file screen!
    pop    si  ;0;
    add    si, font.fpath                ; get true pathname for active font
    push   si  ;1;
    mov    di, fdlg.truepath             ; copy it for display
    call   str_copy_asc0
    pop    si  ;0;
    mov    di, fdlg.query                ; and for the initial query too
    call   str_copy_asc0
    sub    di, fdlg.query+1
    push   di  ;1;                       ; store its length (minus final 0)
    xor    ax, ax
    mov    byte[fdlg.focus], al          ; always begin w/text input field
    mov    di, fdlg.pagebase
    times  2 stosw                       ; ...and zero page/file numbers

  ; Present initial display and proceed

    mov    byte[state.screen], SCR_FILES
    call   vga_switch_screen             ; switch them screens
    push   es  ;2;
    mov    es, [seg_vga]
    call   vga_init_font_IO
    mov    ds, [seg_top]
    memcp  top.8x16+32*128, 32*128, 512  ; reset UI16 font (VRAM_UI16_FONT=0)
    call   vga_end_font_IO
    push   cs
    pop    ds
    pop    es  ;1;
    pop    bp  ;0;                       ; get that string-length back
    call   file_readdir                  ; go get a directory listing
    call   update_filedlg                ; ...and show it
    jmp    file_get_text                 ; let's get some input in here

  ; Back to editor

  .exit_w_msg:                           ; show an informative status message:
    mov    si, txt.m_f
    mov    di, action_msg                ; write it up in our scratch area
    call   str_copy_asc0
    dec    di
    push   di
    mov    di, file_keys                 ; scan list of action keys
    mov    ax, [fdlg.actkey]
    mov    cx, 6
    repne  scasw
    lea    si, [di+(.msgs-file_keys-2)]  ; get message
    mov    si, [si]
    pop    di
    call   str_copy_asc0
    jmp    short @f
  .exit:
    mov    byte[action_msg], 0
@@: call   file_del_input                ; clean up after ourselves
    call   vga_set_cursor.off
    call   screen_status_bar
    call   ed_switch_to_font             ; back to normal view
    mov    si, action_msg
    call   screen_status_msg             ; say what we did (if applicable)
    ret

  .msgs:   dw txt.m_s, txt.m_s, txt.m_l, txt.m_l, txt.m_i, txt.m_e
              ; F2     Ctrl+S   F3       Ctrl+L   Ctrl+I   Ctrl+E

;-----------------------------------------------------------------------------
  file_del_input:  ; Zero out the entire input string (and the char count)
;-----------------------------------------------------------------------------
    xor    ax, ax
    mov    byte[fdlg.in_len], al
    mov    di, fdlg.input
    mov    cx, ILEN
    rep    stosb
    ret

;-----------------------------------------------------------------------------
  file_chgfocus:   ; Change focus between filename field / dir list
;-----------------------------------------------------------------------------
    not    byte[fdlg.focus]
    call   update_filedlg
    ret

;-----------------------------------------------------------------------------
  file_nav_list:   ; Navigate file list
;-----------------------------------------------------------------------------
    mov    word[fdlg.widget], $          ; where to return on retry
    call   file_del_input
    mov    byte[fdlg.dirs_only], 0       ; files are always valid here!
    mov    bx, fdlg.pagebase             ; first, update input field w/
    push   bx  ;1;                       ;    current selection's name
    mov    si, [bx]
    add    si, [bx+2]
    shl    si, 1
    mov    di, fdlg.input
    mov    bx, di
    push   ds  ;2;
    mov    ds, [seg_fseg]
    mov    si, [si]
    inc    si
    call   str_copy_asc0
    pop    ds  ;1;
    sub    di, bx
    dec    di
    mov    [fdlg.in_len], di

    mov    bx, [state.pal_attrmap]
    mov    ah, [att.dlg_OFF_text+bx]
    call   update_filedlg_inp.a
    call   vga_set_cursor.off

@@: pop    si  ;0;                       ; SI -> pagebase
  .again:
    xor    ax, ax
    cwd                                  ; DX = will hold file# difference
    int    16h                           ; get a keystroke
    mov    al, ah                        ; AL = scancode

    cmp    al, 0Fh                       ; TAB
    jne    @f
    call   file_chgfocus
    jmp    file_get_text
@@: cmp    al, 01h                       ; ESC
    jne    @f
    jmp    file_dialog.exit
@@: cmp    al, 1Ch                       ; Enter
    jne    @f
    jmp    file_get_text.try_it          ; entry chosen: handle it

@@: mov    di, .keys                     ; check for Up, Left, Right, Down,
    mov    cl, 6                         ;           PgUp, PgDn
    repne  scasb
    jne    .again                        ; none of the above? get another
    add    di, 6-1                       ; found? get corresponding value
    mov    al, [di]
    cbw
    xchg   ax, dx                        ; DX = difference in file number
    mov    di, si                        ; DI-> pagebase

  .chg_pos:
    mov    cl, 80                        ; CX = 0 on entry to this proc(!)
    lodsw
    xchg   ax, bx                        ; BX = pagebase
    lodsw                                ; AX = # of selected file on page
    clc
    call   update_file_hilite            ; highlight OFF for current file
    add    ax, dx                        ; AX = NEW position
    jns    .not_neg
    sub    bx, cx                        ; position negative? - back one page
    jns    @f                            ; page negative?
    xor    ax, ax                        ; - yep, jump to the zero position
    jmp    short .new_pos                ;   (don't update page number)
@@: add    ax, cx                        ; - nope, add 80 again
    jmp    short .new_page               ;   ...and flip to PREVIOUS page
  .not_neg:
    mov    dx, [fdlg.count]              ; DX = overflow condition
    mov    bp, ax
    add    bp, bx                        ; BP = (new)AX + pagebase
    cmp    bp, dx                        ; gone too far?
    pushf  ;1;                           ;     (save that answer)
    cmp    ax, cx
    jae    @f
    popf   ;0;                           ; AX<80 (stay on page) - check total:
    jb     .new_pos                      ; - still good?  go ahead
  .back_down:
    mov    ax, dx                        ; - too big: jump to last position
    sub    ax, bx                        ;   on current page
    dec    ax
    jmp    short .new_pos
@@: add    bx, cx                        ; AX>=80 (next page) - advance
    cmp    bx, dx                        ; is our new pagebase too big?
    jb     @f
    sub    bx, cx                        ; - yes, back down
    popf   ;0;                           ;   ...(tidy stack)
    jmp    short .back_down
@@: popf   ;0;                           ; - nope, but did we overshoot
    jb     @f                            ;   with the TOTAL?
    mov    ax, dx                        ;     - yep, jump too last position
    sub    ax, bx                        ;       on NEW page
    dec    ax
    jmp    short .new_page
@@: sub    ax, cx                        ;     - all ok, just normalize AX
  .new_page:                             ;   ...and flip to new page:
    mov    [di], bx                      ; memorize new pagebase
    mov    [di+2], ax                    ; and the new selected file too
    call   update_filedlg                ; redraw the screen
    jmp    file_nav_list
  .new_pos:
    mov    [di+2], ax                    ; memorize new selected file number
  .just_hilite:
    stc
    call   update_file_hilite            ; highlight ON for new selected file
    jmp    file_nav_list

             ; Up,    Left,  Right, Down,  PgUp,  PgDn
  .keys:   db  48h,   4Bh,   4Dh,   50h,   49h,   51h
  .vals:   db   -1,   -16,    16,     1,   -80,    80

;-----------------------------------------------------------------------------
  file_get_text:   ; Get temporary filespec from keyboard
;-----------------------------------------------------------------------------
    mov    word[fdlg.widget], $          ; where to return on retry
    mov    di, fdlg.input
    mov    bp, di
    add    di, [fdlg.in_len]

  .get_it:
    call   update_filedlg_inp            ; show what we've got first
    xor    ax, ax                        ; grab a key
    int    16h
    cmp    ax, 4B00h                     ; left arrow?
    je     .erase
    cmp    ah, 0Eh                       ; backspace?
    je     .erase
    cmp    ah, 01h                       ; esc?
    je     .quit
    cmp    ah, 0Fh                       ; tab?
    je     .focus
    cmp    ah, 1Ch                       ; enter? = DONE
    je     .got_it
    cmp    al, '*'                       ; ignore this...
    je     .get_it
    cmp    al, '?'                       ; ...and this...
    je     .get_it
    cmp    al, ' '                       ; ...and this too
    je     .get_it
    cmp    al, 0                         ; ...and any other non-ASCII garbage
    je     .get_it

    cmp    di, fdlg.input+ILEN-2         ; OK.. first, are we at the limit?
    je     .get_it                       ;    - ignore
    stosb                                ; valid character! add to input
    inc    byte[fdlg.in_len]             ;    - character count ++
    jmp    short .get_it                 ;    - now show and get another

  .focus:
    call   file_chgfocus                 ; TAB pressed - change focus
    jmp    file_nav_list                 ;     and go navigate the list
  .quit:
    jmp    file_dialog.exit              ; ESC pressed - say bye

  .erase:
    cmp    di, bp                        ; are we at the beginning?
    je     .get_it                       ; ...do nothing
    dec    di
    mov    byte[di], 0                   ; ...prev char->0 (end mark)
    dec    byte[fdlg.in_len]             ; ...character count --
    jmp    short .get_it

  .got_it:
    cmp    di, bp                        ; blank input VERBOTEN
    je     .get_it
    xor    al, al
    mov    byte[di], al                  ; CURRENT char = 0
    cmp    byte[di-1], '\'               ; does input end w/backslash?
    jne    @f
    inc    ax                            ; - if so, signify that a directory
@@: mov    byte[fdlg.dirs_only], al      ;   is always intended
    call   update_filedlg_inp

  ; What have we got?

  .try_it:
    mov    si, fdlg.truepath             ; first - copy current truepath
    push   si  ;1;
    mov    bx, si
    mov    di, fdlg.lastgood
    call   str_copy_asc0                 ;     to backup
    mov    si, bx
    mov    di, fdlg.path                 ;     to untrue path
    push   di  ;2;
    call   str_copy_asc0
    mov    bx, di
    dec    bx                            ; BX -> path end (for appending)
    mov    al, '\'
    cmp    byte[bx-1], al                ; * does truepath end with '\'?
    jne    @f                            ;   - nope (means we're not in root)
    dec    bx                            ;   - yep, back down another step

@@: pop    di  ;1;                       ; DI -> path start
    push   di  ;2;
    mov    si, fdlg.input                ; now go over the input string:
    cmp    byte[si], al                  ; * is the first char a backslash?
    jne    @f                            ;   - nope; next check
    inc    di                            ;   - yep; put input right after ":"
    inc    di
    jmp    short .c
@@: cmp    byte[si+1], ':'               ; * is the second char a colon?
    je     .c                            ;   - yep; replace entire path
    mov    di, bx                        ;   - nope; append input to path end
    stosb                                ;     (WITH the backslash)
.c: call   str_copy_asc0
    cmp    word[di-2], ':'               ; AFTER copying: ends w/colon + null?
    jne    @f                            ;   - if so, make it colon+DOT+null
    mov    word[di-1], '.'               ;     to keep DOS from whining

  ; Get truename and hope for the best

@@: pop    si  ;1;                       ; SI -> path
    pop    di  ;0;                       ; DI -> truepath
    mov    bp, di                        ; BP ->  "
    mov    ax, [si]
    call   str_upper_al                  ; capitalize first character
    cmp    ax, [fdlg.faildrv]            ; if path starts with blacklisted
    je     .problem                      ;     drive letter - auto-fail
    call   dos_get_truename
    jnc    @f
    jmp    .problem                      ; problem string is PATH

  ; Check if truename is a duplicate of our other opened font

@@: xor    cx, cx                        ; DI -> truepath: get its length
@@: inc    cx
    cmp    byte[di], 0                   ; find terminating zero
    je     @f
    inc    di
    jmp    short @b
@@: mov    di, bp                        ; truepath again
    mov    si, font1.fspec               ; SI -> string we'll compare against
    mov    al, 1
    add    al, [state.currfont]          ; AL = number of font in OTHER tab(!)
    jz     @f
    mov    si, font2.fspec               ; AL=1? => compare against font2
@@: add    al, 31h                       ; convert to font number
    mov    [txt.dup_of], al              ; put it in potential error message
    push   di
    repe   cmpsb                         ; compare the two truepaths
    pop    di
    jne    @f                            ; - unequal? proceed
    mov    si, txt.dup_file              ; - equal? - show error and abort
    jmp    .prob_prompt

  ; If the truepath can be opened, it's a file

@@: mov    dx, di                        ; DX -> truepath
    test   byte[fdlg.dirs_only], 1       ; if input ended with a backslash -
    jnz    @f                            ;    don't treat as file; skip this!
    xor    al, al                        ; open mode: read
    call   dos_open_file
    jc     @f                            ; if we've failed, treat as directory
    jmp    file_action                   ; otherwise go forth and conquer

  ; If it can't, assume it's a directory, and feed it to DOS

@@: mov    si, bp                        ; SI -> truepath
    push   si  ;1;
    mov    di, fdlg.query
    call   str_copy_asc0
    dec    si                            ; point at terminating zero
    sub    bp, si
    neg    bp                            ; keep length in BP
    call   file_readdir                  ; go get a directory listing
    pop    si  ;0;                       ; point SI back at truepath
    jc     @f
    call   file_del_input                ; clean up after ourselves
    call   update_filedlg                ; show the list too
    mov    ax, [fdlg.widget]
    jmp    ax                            ; ...and start over

  ; Neither file nor dir exists:

@@: test   byte[fdlg.dirs_only], 1       ; input ended with a backslash?
    jnz    .problem                      ;  - yes: no new file - auto-complain
    test   byte[fdlg.allow_new], 1       ; new file allowed (save/export)?
    jz     .problem                      ;  - no: complain
    mov    dx, si                        ;  - yes: DX <- truepath
    jmp    file_action.new               ;    ...and go create one

  .problem:
    push   si  ;1;                       ; failed? SI points to our culprit
    mov    di, scratch                   ;    string: compose an error message
    mov    bx, di                        ;    (temporary backup)
    mov    si, txt.open_err
    call   str_copy_asc0
    pop    si  ;0;
    dec    di
    mov    cl, 80
    call   str_upper_asc0                ; capitalize (max 80 chars)
    mov    si, bx                        ; SI -> scratch: big red warning msg
  .prob_prompt:
    mov    ah, 1101b                     ; +wipe +getkey -cursor +warning
    call   screen_status_prompt
    mov    si, fdlg.lastgood             ; restore backed up truepath
    mov    di, fdlg.truepath
    call   str_copy_asc0
    call   update_filedlg                ; clear the status bar
    call   vga_set_cursor.on
    mov    ax, [fdlg.widget]
    jmp    ax                            ; go get better input

;-----------------------------------------------------------------------------
  file_action: ; Decide which action to take on a file opened for reading
;
; In:      AX    = file handle
;          DX,DI = true (canonicalized) path
;          word[fdlg.widget] = jump to on exit: file_get_text / file_nav_list
;-----------------------------------------------------------------------------
    xchg   ax, bx                        ; close temporary open file
    call   dos_close_file
    jmp    short .not_new                ; BX = temporary "file exists" flag

  .new:                                  ; (call here if nothing to close)
    xor    bx, bx                        ; BX = 0: no file exists
  .not_new:
    mov    di, file_keys                 ; scan list of action keys
    mov    ax, [fdlg.actkey]
    mov    cx, 6
    repne  scasw
    lea    si, [di+(.key_actions - \     ; get action
           file_keys - 2)]
    mov    bp, [state.currfont_ptr]      ; which font are we acting on?
    lodsw
    jmp    ax

  .check_if_exists:
    or     bx, bx                        ; BX=0? => we don't have a stale
    jnz    @f                            ;    handle - no file exists, so
    ret                                  ;    we return with ZF set
@@: mov    si, txt.exists                ; file does exist - show prompt
    mov    ah, 1111b                     ; +wipe +getkey +cursor +warning
    push   dx
    call   screen_status_prompt
    pop    dx
    and    al, 11011111b                 ; ASCII code in AL; lower -> upper
    cmp    al, 'Y'
    ret                                  ; return w/ZF=0 if YES was selected

  .load:
    call   dos_open_n_check              ; if OK: CF clear, BX = file handle,
    jnc    @f                            ;        SI -> filespec, AX = size
    mov    si, scratch                   ; failed? use scary warning prompt
    mov    ah, 1101b                     ; +wipe +getkey -cursor +warning
    call   screen_status_prompt
    jmp    .cancel
@@: push   bp ;1;                        ; opened successfully
    call   font_opened                   ; set some stuff, READ & CLOSE FILE
    pop    di ;0;                        ; DI -> font structure
    call   font_post_proc.after_revert   ; SANITIZE & DISPLAY
    jmp    file_dialog.exit_w_msg

  .save:
    call   .check_if_exists              ; returns ZF=0 if safe to create new
    jz     @f                            ;     or overwrite; ZF=1 for cancel
    jmp    .cancel                       ; for <386 safety
@@: call   dos_save_font
    jnc    @f                            ; all OK? - proceed
    jmp    .cannot_write
@@: mov    si, dx                        ; saved successfully:
    mov    di, [state.currfont_ptr]
    push   di  ;1;
    add    di, font.fspec
    call   str_copy_asc0                 ; - set our new truename
    pop    di  ;0;
    call   font_post_proc.after_revert   ; - parse it and reset changes, etc.
    jmp    file_dialog.exit_w_msg        ; - bye bye

  .import:
    xor    al, al                        ; open for reading
    call   dos_open_file                 ; ignore errors; hope for the best
    xor    cx, cx
    xor    dx, dx
    xchg   ax, bx                        ; BX <= file handle for seek
    mov    ax, 4202h                     ; LSEEK to end of file
    int    21h                           ;    (returns DX:AX = size)
    mov    cx, 6000h                     ; max number of bytes to read
    cmp    ax, cx                        ; file is bigger than that?
    jna    @f
    xchg   ax, cx                        ; AX = byte count
@@: push   ax  ;1;
    call   dos_seek_start                ; set file position pointer to 0
    mov    ds, [seg_top]                 ;++++++++ TEMP FILE SEGMENT +++++++++
    mov    dx, top.tmp                   ; DS:DX = buffer for data
    pop    cx  ;0;                       ; CX = byte count
    push   dx  ;1;
    mov    ah, 3Fh                       ; DOS 2+ - Read from file or device
    int    21h                           ;          (BX = handle)
    call   dos_close_file
    pop    si  ;0;                       ; DS:SI = beginning of file data
    call   file_importer                 ;++++++ RESTORES BASE SEGMENT! ++++++
    jnc    @f
    jmp    .cancel                       ; - import was bad? - try again
@@: call   font_post_proc                ; - all good? finalize (DI->font)
    jmp    file_dialog.exit_w_msg

  .export:
    call   .check_if_exists              ; returns ZF=0 if safe to create new
    jz     @f                            ;     or overwrite; ZF=1 for cancel
    jmp    .cancel                       ; for <386 safety
@@: call   dos_create_file               ; returns AX = file handle
    jnc    @f
    jmp    .cannot_write
@@: xchg   ax, bx
    call   dos_seek_start                ; set file position pointer to 0
    call   file_exporter                 ; go write some data
    call   dos_close_file
    jmp    file_dialog.exit_w_msg

  .cannot_write:
    mov    si, txt.wrt_err               ; problem? - complain
    mov    di, scratch
    push   di ;1;
    call   str_copy_asc0                 ; compose message in scratch
    dec    di
    mov    si, dx                        ; ...filename
    call   str_copy_asc0
    pop    si ;0;
    mov    ah, 1101b                     ; +wipe +getkey -cursor +warning
    call   screen_status_prompt          ; .... and fall through: ...
  .cancel:
    mov    si, fdlg.lastgood
    mov    di, fdlg.truepath
    call   str_copy_asc0
    call   update_filedlg
    mov    ax, [fdlg.widget]
    jmp    ax

  .key_actions: dw .save,   .save,   .load,   .load,   .import, .export
                  ; F2       Ctrl+S   F3       Ctrl+L   Ctrl+I   Ctrl+E

;-----------------------------------------------------------------------------
  file_importer:  ; Try to parse and process data imported from file
;
; In:      ES = CS = base segment
;          DS = temporary file segment (seg_top)!
;          SI = beginning of raw imported data
; Out:     DS = ES = CS = base segment(!)
;          CF = set on error; clear on success
;          DI-> font structure (if successful)
;-----------------------------------------------------------------------------
    push   si ;1;
    mov    cx, 2
    mov    di, head_xbin
    repe   cmpsw                         ; is it XBIN?
    pop    si ;0;
    jne    @f
    jmp    .read_xbin
@@: cmp    word[si], 'BM'                ; is it BMP?
    jne    @f
    jmp    .read_bmp
@@: push   si ;1;
    mov    cl, 8                         ; assume COM: take checksum of
    xor    dx, dx                        ;    first 16 bytes in DX
@@: lodsw
    add    dx, ax
    loop   @b
    pop    si ;0;
    cmp    dx, 8696h                     ; is it a PCMag FontEdit .COM?
    jne    @f
    jmp    .read_pcmag
@@: cmp    dx, 0EF10h                    ; is it a NON-TSR .COM (own format)?
    jne    @f
    jmp    .read_nontsr
@@: cmp    word[si+28h], 'VI'            ; is it a TSR .COM (own format)?
    jne    @f
    cmp    word[si+2Ah], 'LE'
    jne    @f
    jmp    .read_tsr

@@: mov    si, txt.fmt_err               ; complain about format
  .error:
    push   cs
    pop    ds                            ; restore base segment
    mov    ah, 1101b                     ; +wipe +getkey -cursor +warning
    call   screen_status_prompt
    stc                                  ; signal FAIL
    ret

  .bmp_err:
    mov    si, txt.bmp_err
    jmp    short .error
  .read_bmp:
    xor    ax, ax
    cmp    [si+14h], ax                  ; width (high word) must be 0
    jne    .bmp_err
    cmp    [si+18h], ax                  ; height (high word) must be 0
    jne    .bmp_err
    inc    ax
    cmp    [si+1Ch], ax                  ; color depth must be 1 bpp
    jne    .bmp_err
    cmp    word[si+12h], 128             ; bmp width
    jne    .bmp_err
    mov    ax, [si+16h]                  ; bmp height:
    test   al, 0Fh                       ; - divisible by 16?
    jnz    .bmp_err
    mov    cl, 4
    shr    ax, cl                        ; - AL = height
    cmp    ax, 32                        ; - too large?
    ja     .bmp_err
    add    si, [si+0Ah]                  ; SI -> start of bitmap
    mov    di, [cs:state.currfont_ptr]
    push   di
    stosb                                ; write font height
    xchg   ax, bx                        ; BX = height
    add    di, font.data-1+240*32        ; start writing in REVERSE ORDER
    mov    cl, 16                        ; do 16 rows of 16 chars each
  .bmp_char_row:                         ;------------------------------------
    push   cx                            ; outer loop:
    add    di, bx                        ; - target last scanline within char
    dec    di                            ;   (which is its height minus 1)
    push   bx                            ; for each row, do (height) scanlines
  .bmp_scanline:                         ;    ,-------------------------------
    mov    cl, 16                        ;    | for each scanline, do 16 bytes
  .bmp_byte:                             ;    |   ,---------------------------
    not    byte[si]                      ;    |   | invert a single byte
    movsb                                ;    |   | ...and copy it
    add    di, 32-1                      ;    |   | position of same line
    loop   .bmp_byte                     ;    |   |    in next char
    sub    di, 32*16+1                   ;    |   `---------------------------
    dec    bx                            ;    | next scanline
    jnz    .bmp_scanline                 ;    `-------------------------------
    pop    bx                            ; restore height
    sub    di, 32*16-1                   ; target preceding group of 16 chars
    pop    cx                            ; restore char row counter
    loop   .bmp_char_row                 ;------------------------------------
    jmp    .read_finish_bmp              ; close up shop

  .read_xbin:
    mov    ax, [si+09h]                  ; AL = height, AH = XBIN flags:
    test   ah, 2                         ; flag bit 1 set?
    jnz    @f                            ; - yes: we have a font
    mov    si, txt.xb_nofont             ; - no: get out
    jmp    .error
@@: mov    bx, 0Bh                       ; add to SI if no palette
    test   ah, 1                         ; flag bit 0 set?
    jz     @f                            ; - no: keep BX as-is
    add    bl, 30h                       ; - yes: account for palette size
@@: cmp    al, 0                         ; check font height
    je     @f
    cmp    al, 32
    jna    .x1
@@: mov    si, txt.ht_err
    jmp    .error
  .x1:                                   ; height is okay, awesome
    test   ah, 16                        ; flag bit 4 set?
    jz     .x2
    push   ds                            ; - means font is 512 chars:
    push   cs
    pop    ds
    push   ax
    push   bx
    push   si
    mov    si, txt.xb_512                ;   ask which half to import
    mov    ah, 1110b                     ;   +wipe +getkey +cursor -warning
    call   screen_status_prompt
    pop    si
    pop    bx
    cmp    al, '2'                       ;   part 2 selected? -
    pop    ax
    jne    @f
    mov    bh, al                        ;   add 256*font_height to BX
@@: pop    ds
  .x2:
    jmp    .read_finish

  .read_nontsr:
    mov    al, [si+15h]                  ; AL = height
    cmp    al, 0                         ; is it ok?
    je     @f
    cmp    al, 32
    jna    .n1
@@: mov    si, txt.ht_err
    jmp    .error
  .n1:
    mov    bx, 19h                       ; data offset
    jmp    .read_finish

  .read_tsr:
    mov    al, [si+5Dh]                  ; AL = height
    cmp    al, 0                         ; is it ok?
    je     @f
    cmp    al, 32
    jna    .p1                           ; data offset for TSR = same as PCMag
@@: mov    si, txt.ht_err
    jmp    .error

  .read_pcmag:
    mov    al, [si+32h]                  ; AL = height
    cmp    al, 0                         ; is it ok?
    je     @f
    cmp    al, 32
    jna    .p1
@@: mov    si, txt.ht_err
    jmp    .error
  .p1:
    mov    bx, 63h                       ; ...AND FALL THROUGH...

  ; Finish up: BX = data offset from SI; AL = height

  .read_finish:
    lea    dx, [si+bx]                   ; DS:DX -> font data to unsqueeze
    mov    di, [cs:state.currfont_ptr]
    push   di
    stosb                                ; write font height (in AL)
    mov    cl, al                        ; and store it in CX too
    add    di, font.data-1               ; ES:DI -> unsqueeze destination
    call   font_unsqueeze
  .read_finish_bmp:
    pop    di                            ; restore font pointer
    push   cs
    pop    ds                            ; ...and base segment
    clc                                  ; very nice, great success
    ret

;-----------------------------------------------------------------------------
  file_exporter:  ; Prepare data in requested export format + write to file
;
; In:      BX = file handle (preserved)
; Out:
;-----------------------------------------------------------------------------
    mov    es, [seg_top]                 ;++++++++ TEMP FILE SEGMENT +++++++++
    mov    di, top.tmp                   ; start writing here
    push   di  ;1;                       ;     (save location for DOS)
    mov    si, .fmt_actions
    mov    ax, [fdlg.export_fmt]         ; determine format
    shl    ax, 1
    add    si, ax
    lodsw                                ; AX -> action
    mov    bp, [state.currfont_ptr]      ; BP -> active font structure
    jmp    ax

  .fmt_xbin:
    mov    si, head_xbin
    mov    al, [bp]                      ; AL = font height:
    mov    [si+9], al                    ; add it to our header
    mov    cl, 11                        ; size of XBIN header (bytes)
    rep    movsb
    mov    si, bp
    call   font_squeeze
    mov    ax, 0700h                     ; create a nice 16x16 charset table
    mov    dx, ax
    mov    cx, 16
  .xbin_row:
    push   cx
    mov    cl, 16
@@: stosw
    inc    ax
    loop   @b
    mov    cl, 80-16
    xchg   ax, dx
    rep    stosw
    xchg   ax, dx
    pop    cx
    loop   .xbin_row
    push   di                            ; memorize EOF location
    sub    di, 15*160+(160-28*2)
    mov    si, attstr_status_bar+2       ; program name
    call   draw_asc0
    mov    si, txt.exported_f            ; 'exported font'
    call   draw_asc0
    pop    di                            ; restore EOF location
    jmp    .write_file

  .fmt_bmp:
    mov    si, head_bmp
    mov    ax, [bp]                      ; AX = font height
    mov    dx, ax                        ; DX =   "
    mov    cl, 4
    shl    ax, cl                        ; AX = image height in pixels
    mov    [si+16h], ax
    shl    ax, cl                        ; AX = image size in bytes
    inc    ax                            ;      add 2 for padding (total file
    inc    ax                            ;      size should divide by 4)
    mov    [si+22h], ax
    mov    cl, 3Eh                       ; BMP header size
    add    ax, cx
    mov    [si+2], ax
    rep    movsb                         ; write header
    lea    si, [bp+font.data+240*32]     ; start writing in REVERSE ORDER
    mov    cl, 16                        ; do 16 rows of 16 chars each
  .bmp_char_row:                         ;------------------------------------
    push   cx                            ; outer loop:
    add    si, dx                        ; - target last scanline within char
    dec    si                            ;   (which is its height minus 1)
    push   dx                            ; for each row, do (height) scanlines
  .bmp_scanline:                         ;    ,-------------------------------
    mov    cl, 16                        ;    | for each scanline, do 16 bytes
  .bmp_byte:                             ;    |   ,---------------------------
    movsb                                ;    |   | copy a single byte
    not    byte[es:di-1]                 ;    |   | ...and invert it
    add    si, 32-1                      ;    |   | position of same line
    loop   .bmp_byte                     ;    |   |    in next char
    sub    si, 32*16+1                   ;    |   `---------------------------
    dec    dx                            ;    | next scanline
    jnz    .bmp_scanline                 ;    `-------------------------------
    pop    dx                            ; restore height
    sub    si, 32*16-1                   ; target preceding group of 16 chars
    pop    cx                            ; restore char row counter
    loop   .bmp_char_row                 ;------------------------------------
    xchg   ax, cx
    stosw                                ; write 2 extra padding bytes
    jmp    .write_file                   ; DI -> EOF position

  .fmt_com:
    mov    si, .com_actions
    mov    ax, [fdlg.export_com]         ; determine subformat for .COM
    shl    ax, 1
    add    si, ax
    cwd                                  ; DX = values for unaffected modes
    mov    cx, 0101h                     ; CX = values for affected modes
    lodsw                                ; AX -> action
    jmp    ax

  .non_tsr:
    mov    si, head_com
    mov    al, [bp]                      ; get font height
    mov    [si+15h], al                  ; ...and update the header with it
    mov    cx, 25
    rep    movsb                         ; write header
    mov    si, bp                        ; point to structure
    call   font_squeeze
    jmp    .write_file                   ; DI -> EOF position

  .tsr_40c:
    xchg   cx, dx                        ; CX = unaffected; DX = affected
    jmp    short .tsr_common
  .tsr_all:
    mov    dx, cx                        ; CX = affected; DX = affected
  .tsr_80c:
  .tsr_common:
    mov    si, head_tsr
    mov    [si+2Dh], dx                  ; patch values for modes 0, 1
    mov    [si+2Fh], cx                  ; patch values for modes 2, 3
    mov    ah, [bp]
    mov    [si+5Dh], ah                  ; patch font height
    mov    al, 60h                       ; AX = *relative* offset of non-
    mov    [si+1], ax                    ;      resident tail (-3); patch it
    mov    cx, 63h                       ; header size
    rep    movsb                         ; copy header
    mov    si, bp                        ; point to structure
    xchg   dx, ax
    add    dl, 3                         ; fix to absolute offset (+3)
    call   font_squeeze                  ; squash them data
    mov    ax, dx
    mov    si, tail_tsr                  ; we're not done yet...
    add    ax, 18Dh                      ; AX = abs. tail offset+100h+18Dh
    mov    [si+0Ch], ax                  ; patch text offset #1
    add    ax, 0Fh                       ; AX = abs. tail offset+100h+19Ch
    mov    [si+3Ch], ax                  ; patch text offset #2
    add    ax, 0Ah                       ; AX = abs. tail offset+100h+1A6h
    mov    [si+41h], ax                  ; patch text offset #3
    sub    ax, 185h
    neg    ax                            ; AX = -(tail offset + 21h)
    mov    [si+74h], ax
    mov    cl, 4
    shr    dx, cl                        ; transform to paragraph count
    add    dx, 11h                       ; add 10h for PSP + round up by one
@@: mov    [si+81h], dx                  ; patch paragraph count for TSR call
    mov    cx, 0B5h                      ; tail size
    rep    movsb                         ; ...and fall through...:

  .write_file:                           ; DI -> last byte to write +1
    push   es
    pop    ds                            ; DS -> temp file segment
    push   cs
    pop    es                            ; ES -> restored base segment
    pop    dx  ;0;                       ; DS:DX -> data to write
    mov    cx, di
    dec    di
    sub    cx, dx                        ; CX = byte count
    mov    ah, 40h                       ; DOS 2+ - Write to file or device
    int    21h                           ;          (BX = handle)

    push   cs
    pop    ds                            ; restore DS to base segment too!
    ret

  ;(Sub)-format value:    0  ,      1   ,     2   ,     3
  .fmt_actions:  dw  .fmt_com, .fmt_bmp, .fmt_xbin
  .com_actions:  dw  .non_tsr, .tsr_40c, .tsr_80c,  .tsr_all

;-----------------------------------------------------------------------------
  file_readdir: ; Read a directory and sort it
;
; In:      BP         = query length (terminating 0), for str_gen_fquery
;          fdlg.query = temporary query string WITHOUT extension
; Out:     CF         = set on error, clear on success
;-----------------------------------------------------------------------------
    mov    si, txt.ext_all               ; search extension 1 '.*'
    call   str_gen_fquery                ; add it (temp ES<-DS)
    call   dos_find_first                ; dummy test to see if dir is valid
    jnc    @f                            ; (may fail on empty disks, but meh)
    ret                                  ;    problem? - return w/carry set

@@: mov    di, fdlg.count                ; reset some parameters:
    xor    ax, ax                        ; zero out our file count + pagebase
    times  3 stosw                       ;         + selected file (fnum_rel)
    mov    byte[fdlg.too_long], al       ;         + the too-long flag
    push   bp  ;1;
    call   update_filedlg                ; display 'reading' status
    pop    bp  ;0;

    push   es  ;1;
    mov    es, [seg_fseg]                ;++++++++ INIT FILELIST I/O +++++++++

    ; Get file list (dots changed to ascii 01; get_list calls preserve BP)

    call   dos_get_list.dirs             ; (gets drives too)
    mov    ax, [fdlg.actkey]
    cmp    ax, 1709h                     ; ctrl+I
    je     .import
    cmp    ax, 1205h                     ; ctrl+E
    je     .export
    call   dos_get_list.files            ; LOAD/SAVE: keep .* query for files
    jmp    short .sort                   ;            and go sort 'em
  .import:
    mov    si, txt.ext_com               ; OP=IMPORT: do three queries -
    call   str_gen_fquery                ;     1) .COM
    call   dos_get_list.files
    mov    si, txt.ext_bmp
    call   str_gen_fquery                ;     2) .BMP
    call   dos_get_list.files
    mov    si, txt.ext_xbin
    call   str_gen_fquery                ;     3) .XB
    call   dos_get_list.files
    jmp    short .sort
  .export:                               ; OP=EXPORT:
    mov    si, screen_gen_files.fmt
    mov    bx, [fdlg.export_fmt]         ;     look up the selected
    shl    bx, 1                         ;     file extension
    mov    si, [si+bx]
    call   str_gen_fquery                ;     add it
    call   dos_get_list.files            ;     get list, and:

    ; Sort 'em and restore the dots

  .sort:
    push   ds  ;2;
    xor    si, si ;fseg.fname_ptrs=0     ; mergesort arg1: array to be sorted
    xor    ax, ax                        ; mergesort arg2: first item
    mov    cx, [fdlg.count]              ; mergesort arg3: last item = number
    push   cx  ;3;                       ;     of entries (save for post-sort)
    push   cx  ;4;
    dec    cx                            ;     minus 1 (point to last one)
    shl    cx, 1                         ;     *2 (size of pointer)

    push   es
    pop    ds                            ;   ,++++++ DS <- files seg +++++,
    push   si  ;arg1                     ;   |                            |
    push   ax  ;arg2                     ;   | PUSH ARGUMENTS AND EXECUTE |
    push   cx  ;arg3                     ;   |    RECURSIVE MERGE SORT    |
    call   file_sort_dir                 ;   |    --------------------    |
    mov    si, fseg.fnames               ;   | now restore filename dots: |
    pop    cx  ;3;                       ;   |           CX = file count  |
    xor    bx, bx                        ;   |                            |
@@: mov    bl, [si+15]                   ;   | get dot offset within name |
    mov    byte[si+bx], '.'              ;   | fix it up                  |
    add    si, 16                        ;   | next file                  |
    loop   @b                            ;   |                            |
    mov    bx, -2  ;pointer              ;   | now scan: BX = pointer     |
    pop    cx  ;2; ;max range            ;   |           CX = file count  |
@@: inc    bx                            ;   | next file                  |
    inc    bx                            ;   |                            |
    mov    si, [bx] ;get fname           ;   | if attr. byte=3, that's    |
    cmp    byte[si], 3                   ;   | ".." - we want this one    |
    loopne @b                            ;   |                            |
    pop    ds  ;1;                       ;   `++++++ DS <- base seg. +++++'

    pop    es  ;0;                       ;+++++++* END FILELIST I/O ++++++++++
    jne    @f                            ;
    shr    bx, 1                         ; did we find a ".." entry? if so,
    mov    [fdlg.fnum_rel], bx           ;     set it as the selection
@@: clc                                  ; clear CF (for great success)
    ret

;-----------------------------------------------------------------------------
  file_sort_dir:     ; Merge-sorts filename pointers; converted for 8086/.COM
;                      (SIZE-optimized version)
;
; Implements: MergeSort(int array[], int left, int right)
; In:         ES, DS            = files segment
;             word[sp+6]: arg1 -> array to be sorted
;             word[sp+4]: arg2  = first item
;             word[sp+2]: arg3  = last item (num_ptrs-1)*2
;             fseg.aux         -> auxiliary array for sorting
;-----------------------------------------------------------------------------
    push   ax                            ; save used registers
    push   bp
    mov    bp, sp                        ; compose stack frame

    mov    ax, [bp+6]                    ; AX = right
    mov    dx, [bp+8]                    ; DX = left
    cmp    ax, dx                        ; left = right?
    je     @f                            ; - yep:  exit
    add    ax, dx                        ; - nope: AX = left+right
    shr    ax, 1                         ; AX = mid (left+right)/2
    and    al, 0FEh                      ;     ENSURE IT'S EVEN

    mov    dx, [bp+10]
    push   dx                            ; 1ST RECURSIVE CALL:
    mov    dx, [bp+8]
    push   dx                            ;     sort(arr, left, mid)
    push   ax
    call   file_sort_dir                 ;     call self

    mov    dx, [bp+10]
    push   dx                            ; 2ND RECURSIVE CALL:
    inc    ax
    inc    ax
    push   ax                            ;     sort(arr, mid+1, right)
    dec    ax
    dec    ax
    mov    dx, [bp+6]
    push   dx
    call   file_sort_dir                 ;     call self

    mov    dx, [bp+10]
    push   dx                            ; merge(arr, left, mid, right)
    mov    dx, [bp+8]
    push   dx
    push   ax
    mov    dx, [bp+6]
    push   dx
    call   .merge

@@: pop    bp
    pop    ax

    ret    3*2

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.merge:                                  ; merge(arr, left, mid, right)
                                                ;+10  +8    +6    +4
    push   bp
    mov    bp, sp                        ; compose stack frame

    mov    dx, .roll1                    ; size-optimize

    mov    bx, [bp+10]                   ; BX = &array
    mov    si, [bp+8]                    ; SI = l
    mov    di, [bp+6]
    inc    di                            ; DI = m+1
    inc    di
    xor    cx, cx                        ; CX = k = 0

  .pt1:
    cmp    si, [bp+6]                    ; while (l<=m && r<=j)
    jg     .pt2
    cmp    di, [bp+4]
    jg     .pt2

    mov    ax, [bx+si]                   ; AX = a[l]

    push   si                            ; if (a[l] < a[r]) ++++++++++++++++++
    push   di                            ;    COMPARE TIME!
    push   cx
    mov    si, ax                        ; - a[l]
    mov    di, [bx+di]                   ; - a[r]
    mov    cx, 13                        ; - length: attr byte+fname
    repz   cmpsb                         ; - DO IT!
    pop    cx
    pop    di
    pop    si                            ; +++++++++++++++++++++++++++++++++++

    jae    @f
    call   dx                            ; (size-optimize): aux[k] = a[l]
    jmp    short .pt1

@@: mov    ax, [bx+di]                   ; else
    call   .roll2                        ; (size-optimize) aux[k] = a[r]
    jmp    short .pt1

  .pt2:
    cmp    si, [bp+6]                    ; while (l<=m)
    jg     @f
    mov    ax, [bx+si]                   ; aux[k] = a[l];
    call   dx                            ;    (size-optimize)
    jmp    short .pt2

@@: cmp    di, [bp+4]                    ; while (r<=j)
    jg     @f
    mov    ax, [bx+di]                   ; aux[k] = a[r];
    call   .roll2                        ;    (size-optimize)
    jmp    short @b

@@: mov    si, [bp+8]                    ; SI = i
    add    bx, si
    mov    cx, 0                         ; CX = k = 0
    mov    si, [bp+4]
    sub    si, [bp+8]
    inc    si                            ; SI = j-i+1 = aux.length
    inc    si

@@: cmp    cx, si                        ; for (k=0; k<aux.length; k++)
    jge    @f
    xchg   cx, si                        ;.... can't calc EA w/CX
    mov    ax, [fseg.aux+si]             ;.... a[i+k] = aux[k]
    mov    [bx+si], ax                   ;....
    xchg   cx, si                        ;....
    inc    cx                            ; k++
    inc    cx
    jmp    short @b

@@: pop    bp
    ret    4*2

  .roll1:
    xchg   cx, si                        ;.... can't calc EA w/CX
    mov    [fseg.aux+si], ax             ;.... aux[k] = a[l]
    xchg   cx, si                        ;....
    inc    cx                            ; k++
    inc    cx
    inc    si                            ; l++
    inc    si
    ret
  .roll2:
    xchg   cx, si                        ;.... can't calc EA w/CX
    mov    [fseg.aux+si], ax             ;.... aux[k] = a[r]
    xchg   cx, si                        ;....
    inc    cx                            ; k++
    inc    cx
    inc    di                            ; r++
    inc    di
    ret