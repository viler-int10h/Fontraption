;++++++++++++++++++++++++++++++  STARTUP CODE  +++++++++++++++++++++++++++++++

  ; Got RAM?

    mov    ax, word[02h]                 ; PSP: first segment after memory
    push   ds                            ;     allocated by DOS
    pop    bx
    sub    ax, bx
    cmp    ax, 3800h                     ; check for 224 KB (/16)
    ja     @f
    mov    si, txt.no_RAM                ; nope, that's not enough
    call   str_print_asc0
    jmp    error_exit_w_CR

  ; Got VGA?

@@: mov    ax, 1A00h                     ; read display combination (VGA)
    int    10h
    cmp    al, 1Ah                       ; AL=1Ah if function supported
    jne    @f
    cmp    bl, 7                         ; VGA w/mono analog display (probably
    je     found_vga                     ;     a bad idea, but what the heck)
    cmp    bl, 8                         ; VGA w/color analog display
    je     found_vga
@@: mov    si, txt.no_VGA                ; none of the above? too bad
    call   str_print_asc0
    jmp    error_exit
  found_vga:

  ; Got DOS 3.0+?

    mov    ah, 30h                       ; get DOS version number
    int    21h
    cmp    al, 3                         ; AL = major version
    jae    @f
    mov    si, txt.no_DOS3               ; too old - whine about it
    call   str_print_asc0
    jmp    error_exit_w_CR

  ; Replace the DOS critical error handler

@@: mov    ax, 2524h                     ; target: int 24h (don't bother
    mov    dx, dos_new_int24             ;     saving the old one, since DOS
    int    21h                           ;     restores it on termination)

  ; Spot the locations for the two extra segments we'll need

    mov    ax, cs
    add    ah, 10h
    push   ax  ;1;
    mov    [seg_fseg], ax                ; <- CS+1000h
    add    ah, 0Bh
    mov    [seg_upper], ax               ; <- CS+1B00h
    add    ah, 07h
    mov    [seg_top], ax                 ; <- CS+2200h

  ; While we're here, create video positioning table for directory list

    pop    es  ;0;                       ; files segment
    mov    ax, VRAM_FILES+ROW*6+8        ; start value
    mov    dx, 5                         ; column counter
    mov    di, fseg.pos
.c: mov    cx, 16                        ; row counter
@@: stosw
    add    ax, ROW
    loop   @b
    sub    ax, ROW*16-15*2               ; start of next col
    dec    dx
    jnz    .c
    push   cs
    pop    es

  ; Zero out a hunk of the uninitialized data space

    mov    di, bof_init0
    mov    cx, (eof_init0-bof_init0)/2
    xor    ax, ax
    rep    stosw

  ; Check for command line arguments

    cmp    byte[80h], 2                  ; length of command tail
    jb     get_vga_font                  ; <2? no arguments, grab from VGA

    mov    si, 82h                       ; 82h = beginning of command tail
    push   si
    mov    cx, 128
    mov    di, si                        ; if we have anything at all,
    call   str_upper_asc0                ; uppercase it in-place
    pop    si

  find1start:                            ; start scanning
    lodsb
    cmp    al, ' '
    jz     find1start
    cmp    al, 0Dh                       ; nothing but spaces? really?
    je     get_vga_font
    dec    si                            ; yay, first filename starts here
    mov    word[arg1_ptr], si
    inc    si
  find1end:
    lodsb
    cmp    al, ' '
    je     next_cmd_arg                  ; we might just have another arg!
    cmp    al, 0Dh                       ; reached the end?
    je     handle_args
    cmp    si, 255                       ; cmdline too long? give it a rest
    je     handle_args
    jmp    short find1end                ; keep scanning
  next_cmd_arg:                          ; go look for another filename
    mov    byte[si-1], 0                 ; now arg1_ptr => ASCIIZ string
  find2start:
    lodsb
    cmp    al, ' '
    je     find2start
    cmp    al, 0Dh                       ; just spaces after first filename?
    je     handle_args
    dec    si                            ; found beggining of second filename
    mov    word[arg2_ptr], si
    inc    si
  find2end:
    lodsb
    cmp    al, ' '
    je     handle_args
    cmp    al, 0Dh
    je     handle_args
    cmp    si, 255                       ; we're over the limit - abort
    je     handle_args
    jmp    short find2end                ; keep scanning

  handle_args:
    mov    byte[si-1], 0                 ; arg1(&2?)_ptr => ASCIIZ string(s)
    mov    dx, [arg1_ptr]
    mov    bp, font1                     ; we have one or more arguments:
    call   get_one_file                  ; try to open first file

    mov    dx, [arg2_ptr]
    inc    dx
    dec    dx                            ; no second argument? go copy active
    jz     get_vga_font                  ;     VGA font to user font 2 (only)

    mov    bp, font2                     ; a-ha, another argument!
    call   get_one_file                  ; have a shot at file 2
    jmp    got_both_fonts                ; no VGA stuff needed - skip ahead

  get_one_file:
    call   dos_open_n_check              ; if OK: CF clear, BX = file handle,
    jnc    @f                            ;        SI -> filespec, AX = size
    jmp    error_exit_w_CR
@@: jmp    font_opened                   ; read data & finish up (returns too)

  ; Less than 2 files specified - crib active font from VGA

  get_vga_font:
    call   vga_dump_to_fnt2              ; grab our data to font #2
    cmp    byte[font1.height], 0         ; does font #1 exist?
    jne    got_both_fonts                ; - yep, we've read it from a file
    memcp  font2, font1, (sizeof.font)/2 ; - nope, mirror #2 to #1 too!

  ; Now that we have two fonts, let's initialize a few things

  got_both_fonts:
    mov    si, font1
    mov    [state.currfont_ptr], si      ; store active font (1) pointer too
    call   str_parse_fspec               ; set filespec fields for both fonts
    mov    si, font2                     ;    (gets DOS truename from filespec
    call   str_parse_fspec               ;    and splits it into fpath/fname)
    mov    byte[state.currchar], 'A'     ; set default character
    mov    al, 01000000b
    mov    [state.cursor_mask], al       ; cursor mask
    mov    ax, 0101h
    mov    [state.cursor_pos], ax        ; initial editbox cursor position
    inc    byte[state.preview]           ; initial preview mode = 80 columns

  ; Ensure that we're not editing the same file twice

    cmp    byte[font2.fname], '<'        ; except if '<NoName>' - in that
    pushf  ;1;                           ;     case, don't bother
    je     @f
    mov    si, font1.fspec
    mov    di, font2.fspec
    mov    cx, 128/2                     ; at this point memory is still
    repe   cmpsw                         ;     zeroed, so we can compare the
    jne    @f                            ;     maximum length
    mov    si, txt.dup_args
    call   str_print_asc0
    jmp    error_exit_w_CR

  ; Check status of A: and B: drives (for subsequent file ops)

@@: push   ds
    xor    bx, bx                        ; BL = A: status, BH = B: status
    mov    dx, 'B:'                      ; DX = to be flagged inaccessible
    mov    ds, bx                        ; zero segement for BIOS data
    mov    ax, [EQUIPMENT_LIST]          ; BIOS - equipment word
    shr    al, 1                         ; bit 0: 0 = no floppies, 1 = floppies
    jnc    short .done                   ;        1 = floppies exist
    mov    cl, 5
    shr    al, cl                        ; bits 6-7 = number of drives -1
    jz     short @f                      ; only one floppy available
    mov    bx, 0101h                     ; both A: and B: available;
    mov    dl, bl                        ;     none are inaccesible
    jmp    short .done
@@: mov    ax, [ONE_FLOPPY_FLAG]         ; what is the single drive mapped to?
    or     al, al                        ; 0 = logical A:, 1 = logical B:
    jz     short @f                      ;        (DOSBox fails here!)
    inc    bh                            ; - B:
    dec    dx                            ;     (flag A: as inaccessible)
    jmp    short .done
@@: inc    bx                            ; - A:
  .done:                                 ;     (keep B: as inaccessible)
    pop    ds
    mov    di, fdlg.drive_a
    mov    [di], bx                      ; memorize A:/B: availability
    mov    [di+2], dx                    ; ...and the blacklisted drive letter

  ; Test BIOS support for Enhanced Keyboard functions

    xor    ax, ax
    mov    ds, ax                        ; set data/extra segments to 0
    mov    es, ax
    mov    di, KBD_HEAD_PTR
    mov    ax, KBD_BUFF
    mov    dx, ax
    mov    bx, KBD_TESTKEY               ; Ctrl+down
    cli                                  ; do not disturb
    mov    word[di+4], bx                ; push fake keystroke into buffer
    stosw                                ; reset keyboard buffer head to
    inc    ax                            ;     buffer start offset, and set
    inc    ax                            ;     the tail to one word ahead
    stosw                                ;     (indicates available keystroke)
    sti
    mov    ah, 11h                       ; CHECK FOR ENHANCED KEYSTROKE
    int    16h                           ; AX = result
    cli
    mov    word[di-4], dx                ; reset head/tail to same location
    mov    word[di-2], dx                ;     (indicates empty buffer)
    sti
    push   cs                            ; restore segments
    pop    ds
    push   cs
    pop    es
    cmp    ax, bx                        ; did we get the expected value back?
    mov    al, 1                         ;     (prepare drag_flag: RShift)
    jne    @f                            ; - no, got garbage
    mov    byte[state.int16h_func], 10h  ; - yes, enhanced function available:
    mov    al, 4                         ;     * set drag_flag to Ctrl
    mov    cx, txt.fdt_l-txt.ctrl_arr    ;     * update key legend too
    mov    si, txt.ctrl_arr
    mov    di, loc_f_stateful.drag_key
    rep    movsb
@@: mov    [state.drag_flag], al         ; store shift flag for dragging

  ; Generate table of palette pointers

    mov    si, pal_entries
    mov    di, pal_list
    mov    cx, NUM_PALETTES
  pal_ptr:
    mov    ax, si                        ; get current offset
    stosw                                ; add it to the list
    add    si, PAL_GETNAME               ; let's locate the next one (this may
@@: inc    si                            ;    run one extra time; don't care)
    cmp    byte[si-1], 0                 ;    find the terminating zero
    jne    @b
    loop   pal_ptr

  ; Set up VGA

    cmp    byte[font1.height], 16        ; initial active screen is ED25 - if
    jbe    @f                            ;    font1 is taller than 16 lines,
    inc    byte[state.screen]            ;    let's make it ED50
@@: call   vga_setup_all                 ; initialize VGA (and palette!)

  ; Temporarily disable the video and draw initial screen contents

  IF ~DEBUGMODE
    mov    ah, [state.clock_mode_80]
    or     ah, 20h                       ; bit 5 = screen disable
    mov    al, 1                         ; Clocking Mode Register (Index 01h)
    mov    dx, 3C4h
    out    dx, ax
  END IF
    call   screen_gen_editor             ; get crackin' on the screens

  ; Show something informative

    mov    si, txt.welcome               ; be user-friendly!
    popf   ;0;                           ; is font 2 from VGA?
    jne    @f
    mov    si, txt.from_vga
    cmp    byte[font1.fname], '<'        ; font 1 too?
    jne    @f
    mov    di, scratch
    push   di
    call   str_copy_asc0
    sub    di, 7
    mov    si, txt.both_fonts            ; ...watch it...
    call   str_copy_asc0
    mov    byte[di-8], al                ; (terminate the string)
    pop    si
@@: call   screen_status_msg
    call   ed_switch_to_font             ; start the show

  ; Re-enable video and proceed

    debug_prompt vid_en

    mov    ah, [state.clock_mode_80]
    mov    al, 1                         ; Clocking Mode Register (Index 01h)
    mov    dx, 3C4h
    out    dx, ax

    inc    byte[state.started]           ; indicate that editor is started
    include 'editor.inc'                 ; falls through to ed_get_key

;+++++++++++++++++++++++++++++++  TERMINATION  +++++++++++++++++++++++++++++++

  good_exit:                             ; exit OK (no error)
    xor    al, al
    jmp    short terminate
  error_exit_w_CR:                       ; print CR before error exit
    mov    si, txt.CR
    call   str_print_asc0
  error_exit:                            ; exit w/code 1 (error)
    mov    al, 1
  terminate:                             ; DOS 2+ - Exit with return code
    mov    ah, 4Ch
    int    21h
