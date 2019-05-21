;-----------------------------------------------------------------------------
  dos_new_int24:  ; Replaces the critical error handler to avoid invisible
;                   prompts during file access
;-----------------------------------------------------------------------------
    mov    al, 3                         ; always fail
    iret

;-----------------------------------------------------------------------------
  dos_find_first:  ; DOS 2+ - FindFirst - find first matching file
;
; In:      fdlg.query =  query string to DOS, including extension
; Out:     CF         =  set on error, clear on success
;          DTA buffer =  result (PSP offset 80h)
;-----------------------------------------------------------------------------
    mov    ah, 4Eh
    jmp    short @f

;-----------------------------------------------------------------------------
  dos_find_next:   ; DOS 2+ - FindNext - find next matching file

; In:      fdlg.query =  query string to DOS, including extension
;          DTA buffer =  contains data block from previous call
; Out:     CF         =  set on error, clear on success
;          DTA buffer =  result (PSP offset 80h)
;-----------------------------------------------------------------------------
    mov    ah, 4Fh
@@: mov    cx, 10h                       ; CX = attribute mask: +directories
    mov    dx, fdlg.query                ; DS:DX -> ASCIIZ filespec
    jmp    short doscall

;-----------------------------------------------------------------------------
  dos_create_file:  ; DOS 2+ - create or truncate file
;
; In:  DS:DX -> ASCIIZ filename
; Out: CF set? error; clear? success
;      AX = error code if failed, file handle if successful
;-----------------------------------------------------------------------------
    xor    cx, cx                        ; CX = file attributes: normal file
    mov    ah, 3Ch
    jmp    short doscall

;-----------------------------------------------------------------------------
  dos_seek_start:  ; DOS 2+ - LSEEK - SET CURRENT FILE POSITION
;
; In:  BX = file handle
;-----------------------------------------------------------------------------
    mov    ax, 4200h                     ; seek: AL = 00 (origin=start)
    xor    cx, cx                        ; CX:DX = signed offset from origin
    cwd
    jmp    short doscall

;-----------------------------------------------------------------------------
  dos_close_file: ; Just what it says on the tin
;
; In:  BX -> file handle
; Out: CF set? error (AX = error code); clear? success
;-----------------------------------------------------------------------------
    mov    ah, 3Eh
    jmp    short doscall

;-----------------------------------------------------------------------------
  dos_open_file:  ; Should be self-explanatory too
;
; In:  DS:DX -> ASCIIZ filename
;      AL = mode: 0 = Read, 1 = Write, 2 = R/W
; Out: CF set? error; clear? success
;      AX = error code if failed, file handle if successful
;-----------------------------------------------------------------------------
    mov    ah, 3Dh
    doscall:
    int    21h
    ret

;-----------------------------------------------------------------------------
  dos_get_truename:  ; Gets canonical ("true") name from supplied filespec
;
; In:  DS:SI -> ASCIIZ filename or path
;      ES:DI -> 128-byte buffer for canonicalized name
; Out: CF set on error
;      ES:DI -> buffer filled with qualified name e.g. D:\PATH\FILE.EXT
;-----------------------------------------------------------------------------
    mov    ah, 60h
    int    21h

    pushf     ;1;
    push   di ;2;
    cmp    word[di],'\\'                 ; check for MSCDEX "\\x.\A." crap
    jne    @f
    lea    si, [di+3]
    mov    ax, si
    mov    di, .mscdex
    cmpsw
    jne    @f
    cmpsw
    jne    @f
    xchg   ax, si                        ; found some? fix it:
    mov    al, [si-1]                    ; ...get drive letter
    mov    ah, ':'                       ; ...add colon
    inc    si
    inc    si
    mov    [si], ax                      ; put it where the 'A.' was
    pop    di ;0;
    push   di ;1;
    call   str_copy_asc0                 ; now copy to expected location
@@: pop    di ;1;
    popf      ;0;
    ret

  .mscdex: db '.\A.'

;-----------------------------------------------------------------------------
  dos_open_font:  ; Open a font file and determine if the size makes sense
;
; In:       DS:DX -> ASCIIZ filename
; Out:      CF = set? error; clear? success, in which case:
;                            BX = file handle
;                            AX = file size (AH: font height if OK, -1 if bad)
; Destroys: AX,BX,CX,DX(!)
;-----------------------------------------------------------------------------
    xor    al, al                        ; open for reading
    call   dos_open_file
    jc     .open_fail                    ; FAIL - return w/carry set
    xor    cx, cx
    xor    dx, dx
    xchg   ax, bx                        ; BX <= file handle for seek
    mov    ax, 4202h                     ; LSEEK to end of file
    int    21h                           ;    (returns DX:AX = size)
    inc    dx
    dec    dx
    jnz    .bad_font                     ; DX>0? that's WAY too big, man
    cmp    al, 0
    jne    .bad_font                     ; AL>0? => size not multiple of 256!
    cmp    ah, 1                         ; AL=0? => AH is our font height
    jb     .bad_font
    cmp    ah, 32                        ; can't be too tall though!
    ja     .bad_font
    clc                                  ; all OK, return w/carry clear
    ret
  .bad_font:
    call   dos_close_file
    mov    ah, -1                        ; ...and return...
  .open_fail:
    ret

;-----------------------------------------------------------------------------
  dos_open_n_check: ; Open font, print message if error, return status
;
; In:  DS:DX -> filename
; Out: CF=1? error (+prints relevant message). CF=0? ok AND:
;      SI -> filename, BX = handle, AX = file size (AH = font height)
;-----------------------------------------------------------------------------
    push   dx                            ; don't mangle our filename
    call   dos_open_font
    pop    dx
    jnc    @f                            ; CF set? *fail*
    mov    si, txt.open_err
    jmp    short .print_err
  @@:                                    ; hold it, we're still not through!
    cmp    ah, -1                        ; bad font (otherwise, AH = height)
    clc                                  ; give it the benefit of doubt
    jne    .k                            ; all good? signal success
    mov    si, txt.size_err
  .print_err:
    push   bp
    mov    bp, str_print_asc0            ; default - use teletype out
    cmp    byte[state.started], 1
    jne    @f
    mov    bp, str_copy_asc0             ; [EDITOR STARTED] - use this one
    mov    di, scratch                   ; [EDITOR STARTED] - write target
@@: call   bp                            ; say what the matter is
    mov    si, dx                        ; keep filename in SI
    dec    di                            ; [EDITOR STARTED] - deal w/term. 0?
    call   bp                            ; spit out the filename
    pop    bp
    stc                                  ; signal failure
.k: mov    si, dx                        ; always return filename in SI
    ret

;-----------------------------------------------------------------------------
  dos_readfont_n_close: ; Unsqueeze font data into buffer then close the file

; In:  BX = file handle
;      CL = char height (CH=0)
;      DI-> destination buffer
;-----------------------------------------------------------------------------
    push   cx  ;1;
    push   cx  ;2;
    call   dos_seek_start                ; set file position pointer to 0

  ; Read data

    pop    cx  ;1;
    xchg   cl, ch                        ; read (char*height) bytes
    mov    dx, scratch                   ; DS:DX = buffer for data
    mov    ah, 3Fh                       ; DOS 2+ - Read from file or device
    int    21h                           ;          (BX = handle)
    pop    cx  ;0 ;                      ; get height again
    call   font_unsqueeze                ;
    jmp    dos_close_file                ; returns too

;-----------------------------------------------------------------------------
  dos_save_font: ; Create file, squeeze font data into buffer, write and close
;
; In:   DS:DX -> ASCIIZ filename (preserved)
;       BP    -> current font pointer
; Out:  CF     = set on error, clear on success
;-----------------------------------------------------------------------------
    call   dos_create_file
    jnc    @f
    ret

  ; Seek to beginning

@@: push   dx  ;1;
    xchg   ax, bx                        ; BX = file handle
    call   dos_seek_start                ; set file position pointer to 0h

  ; Write data

    mov    si, bp
    mov    di, scratch
    call   font_squeeze                  ; gotta squeeze it first!
    mov    dx, bp                        ; DS:DX -> data to write
    xchg   ax, cx                        ; CX <- AX: bytes per char
    xchg   cl, ch                        ; CX *= 256: number of bytes to write
    mov    ah, 40h                       ; DOS 2+ - Write to file or device
    int    21h                           ;          (BX = handle)
    pop    dx  ;0;                       ; point DX back at truename
    jnc    @f
    ret                                  ; error: return w/carry set
@@: jmp    dos_close_file                ; all good: close; returns too

;-----------------------------------------------------------------------------
  dos_get_list:  ; add to filelist according to query (preserves BP)
;
; In:       fdlg.count = current file count
;           fdlg.query = full query string (path & *.extension)
;           ES        -> files segment(!)
; Out:      CF         = set on error, clear on success
;           fdlg.count = updated file count (reset to 0 if .dirs)
; Destroys: all except BP
;-----------------------------------------------------------------------------
  .dirs:
    mov    si, scratch                   ; DRIVE-LETTER LIST FIRST!
    xor    bx, bx                        ; initialize file count to 0
    mov    word[si], 'A'*256+1           ; start w/"A" + sorting attribute 1
    mov    word[si+2], ':'               ; colon + zero byte
    mov    dx, [fdlg.drive_a]            ; DL: status of A:; DH: status of B:
    mov    cx, 2                         ; check both floppy drives:
.f: test   dl, 1                         ;   - drive present?
    jz     @f
    call   .add_drvletter                ;   - then add to list
@@: xchg   dl, dh                        ; swap A/B status
    inc    byte[si+1]                    ; increment drive letter
    loop   .f                            ; next floppy

    mov    cx, 24                        ; DRIVES C: AND ABOVE: do 24 times
.h: mov    di, scratch+10                ; point to dummy FCB
    push   si
    inc    si                            ; point to starting letter
    mov    ah, 29h                       ; parse filename into FCB
    int    21h
    pop    si
    cmp    al, 0FFh                      ; invalid drive specifier?
    je     @f
    call   .add_drvletter                ;   - nope, all good: add to list
@@: inc    byte[si+1]                    ; increment drive letter
    loop   .h                            ; next non-floppy
    push   bp
    mov    bp, bx                        ; done with drives? update count

    mov    bh, 10h                       ; BH: 10h if dirs only
    jmp    short @f
  .files:
    xor    bh, bh                        ; ...   0 if files only

    push   bp
    mov    bp, [fdlg.count]
@@: call   dos_find_first                ; go go go!
@@: jc     .problem
    call   .add_fname                    ; add filename to list
    cmp    bp, MAX_FILES                 ; too many? bail out
    je     .max_files
    call   dos_find_next                 ; keep going
    jmp    short @b

  .max_files:
    mov    al, 12h                       ; indicate no more
    mov    [fdlg.too_long], al           ; ...and put something in the flag
  .problem:
    mov    [fdlg.count], bp              ; save file count
    pop    bp                            ; ...don't forget this guy
    cmp    al, 12h                       ; no more files?
    je     .finish                       ; - finish up
    stc                                  ; - otherwise, indicate error
    ret
  .finish:
    clc                                  ; - signal all clear
    ret                                  ; ...and we're done

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  .add_drvletter:
    push   si                            ; always copy from scratch
    push   cx
    xchg   bx, bp
    call   .do_it
    xchg   bx, bp
    pop    cx
    pop    si
    ret

  .add_fname:
    mov    si, DTA+15h                   ; point at returned attribute
    lodsb
    and    al, 10h                       ; ...isolate directory bit
    cmp    al, bh                        ; ...is it what we want?
    je     @f                            ;    ...then proceed
    ret                                  ;    ...otherwise, beat it

@@: test   al, 10h
    mov    al, 4                         ; AL = our own attribute: directory?
    jnz    @f
    inc    ax                            ; ...if dir bit unset = file

@@: add    si, 8                         ; point at returned filename
    cmp    word[si], '..'                ;  - double dot?
    jne    @f                            ;    ...just decrease sorting
    dec    ax                            ;        attribute to 3
@@: cmp    word[si], '.'                 ;  - single dot?
    jne    @f
    mov    byte[si], '\'                 ;    ...change to '\'
    dec    ax                            ;       and make the sorting
    dec    ax                            ;       attribute 2
@@: dec    si                            ; detune origin for copy
    mov    byte[si], al                  ; write sorting attribute

    .do_it:
    push   bp  ;2;                       ; ready to write?
    mov    cl, 4
    shl    bp, cl
    lea    di, [fseg.fnames+bp]          ; offset in fnames = count*16
    push   di  ;3;                       ; ...keep it aside
    call   str_copy_asc0                 ; copy sort attribute + filename
    pop    ax  ;2;                       ; now get that offset back
    dec    cx
    shr    bp, cl
    mov    di, bp                        ; index in fname_ptrs (0!) = count*2
    stosw                                ; write it

    pop    bp  ;1;
    inc    bp                            ; count ++
    ret

