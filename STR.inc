;-----------------------------------------------------------------------------
  str_parse_fspec: ; Convert fspec to DOS truename, set fpath/fname fields
;
; In:       DS:SI -> font structure (not filespec)
;           ES=DS=CS (!)
; Out:      font#.fspec = DOS truename
;           font#.fpath = Drive and path only
;           font#.fname = 8.3 filename only
; Destroys: BX, DX
;-----------------------------------------------------------------------------
    mov    bx, si                        ; preserve pointer to font structure
    mov    dx, scratch                   ; keep one to scratch handy, too
    mov    di, dx                        ;   (we'll manipulate truename there)
    add    si, font.fspec                ; point to font's filespec
    call   dos_get_truename              ; ignore errors, fingers crossed
    mov    si, dx                        ; copy returned truename from scratch
    mov    di, bx                        ;      back to fspec
    add    di, font.fspec
    call   str_copy_asc0

  ; Split truename in scratch into fpath/fname:

    mov    di, si                        ; DI=SI=end of ASCIIZ str in scratch
    std                                  ; about-face
    mov    al, '\'                       ; where's our backslash? let's scan
    mov    cl, al                        ; search limit > max length (12)
    repne  scasb                         ; ignore potential of ending up back
                                         ;   at the start w/no backslash found
    cld                                  ; found our '\' - do a U-turn again
    xor    al, al                        ; replace it with a terminating zero:
    inc    di                            ; - back up one step
    stosb                                ; - do it: now DI points to start
                                         ;          of pathless filename
    mov    si, di                        ; copy pathless name to font#.fname
    mov    di, bx
    add    di, font.fname
    push   di  ;1;
    call   str_copy_asc0
    mov    si, dx                        ; copy drive + path to font#.fpath
    mov    di, bx
    add    di, font.fpath
    call   str_copy_asc0

    pop    di  ;0;                       ; if font#.fname is '?' then a zero
    cmp    word[di], '?'                 ;    byte, this is a VGA-originating
    jne    @f                            ;    font - overwrite it with
    mov    si, txt.noname                ;    '<NoName>',0
    call   str_copy_asc0
@@: ret

;-----------------------------------------------------------------------------
  str_gen_fquery:  ; Generate filespec query string from supplied path + ext.
;
; In:      SI        -> ASCIIZ, file extension
;          BP         = length of fdlg.query (position of terminating zero)
;          fdlg.query = ASCIIZ *path*, final backslash optional
; Out:     fdlg.query = final query string for DOS - path + filespec
;-----------------------------------------------------------------------------
    push   es                            ; PRESERVE FOR CALLER

    push   ds
    pop    es
    lea    di, [fdlg.query-1+bp]
    mov    al, '\'
    cmp    byte[di], al                  ; got backslash at the end?
    je     @f
    inc    di
@@: stosb                                ; write the backslash
    mov    ax, '*.'                      ; and this too
    stosw
    call   str_copy_asc0                 ; now copy extension

    pop    es                            ; RESTORE FOR CALLER
    ret

;-----------------------------------------------------------------------------
  str_print_asc0: ; In: SI -> ASCIIZ string; uses BIOS (teletype output)
;-----------------------------------------------------------------------------
    xor    bh, bh
.l: mov    ah, 0Eh                       ; IBM XT bios seems to stomp AH
    lodsb
    or     al, al
    jnz    @f
    ret
@@: int    10h
    jmp    short .l

;-----------------------------------------------------------------------------
  str_copy_asc0:  ; In: DS:SI -> source, ES:DI -> target
;-----------------------------------------------------------------------------
@@: lodsb
    or     al, al
    stosb
    jnz    @b
    ret

;-----------------------------------------------------------------------------
  str_upper_asc0: ; In: DS:SI -> source, ES:DI -> target, CX = max length(!)
;-----------------------------------------------------------------------------
@@: lodsb
    call   str_upper_al
    or     al, al
    stosb
    loopnz @b
    ret

;-----------------------------------------------------------------------------
  str_upper_al: ; AL = charcter to convert
;-----------------------------------------------------------------------------
    cmp    al, 'a'
    jb     @f
    cmp    al, 'z'
    ja     @f
    and    al, 0DFh
@@: ret

;-----------------------------------------------------------------------------
  str_hex_byte: ; Print hex byte or nybble as ASCII
;
; In:      AL = value,   AH = attribute
;          ES:DI -> pointer to screen position
;-----------------------------------------------------------------------------
    push   ax
    push   cx
    mov    cl, 4
    shr    al, cl
    or     al, al                        ; is AL a leading zero?
    jnz    .nz                           ;   - nope, nonzero digit: print it
    stosw                                ;   - yep, print a null char instead
    jmp    short .z                      ;       ...and go on
  .nz:
    call   str_hex_nybble
  .z:
    pop    cx
    pop    ax                            ; fall through to second nybble

  str_hex_nybble:                        ; call directly to print ONE nybble
    and    al, 0Fh                       ; get rid of those 4 high bits
    add    al, 90h		                    ; pretend we've added two BCD values
    daa                                  ; low nybble>9? subtract 10d, set CF
                                         ;     and clear the high nybble
    adc    al, 40h                       ; add w/CF, for next DAA's benefit
    daa                                  ; this one ensures we have either
    stosw                                ;      digits or (uppercase) A-F
    ret

;-----------------------------------------------------------------------------
  str_dec_byte: ; Prints unsigned 8-bit value in AL as 3-digit decimal number
;
; In:       DH = method: OP_STOSB (no attributes) or OP_STOSW (attributes)
;           AL = value
;           AH = attribute (if DH=OP_STOSW)
;           ES:DI -> pointer to screen position
; Destroys: CL   (AX, DX preserved)
;-----------------------------------------------------------------------------
    push   dx
    mov    [cs:.op], dh
    jmp    short @f                      ; prevent prefetch issues
@@: push   ax                            ; backup original char/attr
    mov    dh, ah                        ; DH: attribute backup
    aam                                  ; AH: (##_), AL: (__#)
    push   ax                            ; [1] DIGIT 3
    mov    al, ah                        ; AL: (##_)
    aam                                  ; AH: (#__), AL: (_#_)
    push   ax                            ; [2] DIGIT 2 + attribute
    mov    al, ah                        ; AL: (#__)
    mov    cx, 3
@@: add    al, '0'
    mov    ah, dh                        ; attribute
    .op:   db  0                         ; do it (stosb/stosw go here!)
    pop    ax
    loop   @b
    pop    dx
    ret