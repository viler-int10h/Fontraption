; All functions called here should assume ES = DS = CS on entry

;-----------------------------------------------------------------------------
  font_cleanup: ; Zero out slack space of unsqueezed font to remove garbage
;
; In:       ES:DI-> font structure (not data)
; Destroys: BX, CX, DX, DI, AL
;-----------------------------------------------------------------------------
    mov    bx, [di]                      ; BX = height
    add    di, font.data
.v: xor    al, al                        ; call here if BX, DI already set up
    mov    dx, 32
    sub    dl, bl                        ; DX = 32-height; BX = height
    mov    cx, 256
@@: push   cx
    add    di, bx
    mov    cx, dx
    rep    stosb                         ; CLEANSE FILTH
    pop    cx
    loop   @b
    ret

;-----------------------------------------------------------------------------
  font_opened:  ; Initialize font state vars, then go read font and close it
;
; In:       DS:BP-> font structure (not data)
;           SI-> filespec
;           BX = file handle
;           AH = font height
;-----------------------------------------------------------------------------
    mov    [bp], ah                      ; store font height (b.0 in struct)
    lea    di, [bp+font.fspec]
    call   str_copy_asc0                 ; copy font filespec (DS:SI->ES:DI)
    mov    [bp+font.unsaved], cl         ; clear unsaved flag (CL=0)
    mov    cx, [bp]                      ; height again (high byte is 0)
    lea    di, [bp+font.data]            ; read font into buffer and close
    jmp    dos_readfont_n_close          ; IN: BX = handle, DI->buffer,
                                         ;     CL = height (CH=0!)
                                         ; returns, too

;-----------------------------------------------------------------------------
  font_post_proc:  ; Sanitize font after load/get ROM/undo/etc., and set it
;
; In:  ES:DI-> font structure (not data)
;      * Local labels are for calling when less extensive cleanup is needed
;-----------------------------------------------------------------------------
    push   di ;1;
    mov    word[di+font.fspec], '?'      ; '?',0 (to <NoName> later)
.r: xor    ax, ax
    mov    word[di+font.can_undo], ax    ; no can undo
    inc    di
    inc    di                            ; font.unsaved
    stosw
    add    di, font.changes - 4          ; font.changes
    mov    cx, 128
    rep    stosw
    pop    di ;0;
    mov    si, di
    push   di ;1;
    call   str_parse_fspec
    pop    di ;0;
  .after_height:
    call   font_cleanup
  .after_undo:
    stc                                  ; padding? yes please
    call   vga_upload_font               ; make it happen
    call   ed_switch_to_font             ; heavy guns (against height changes)
    ret

  .after_revert:
    push   di ;1;
    jmp    short .r

;-----------------------------------------------------------------------------
  font_get_char_row:  ; Gets char row at cursor pos to AL + point to/after it
;
; In:       State field values: currfont_ptr, currchar, cursor_pos
; Out:      AL = desired character row (bit mask)
;           DI-> pointer to this row in DS
;           SI-> pointer to one row *after*(!)
; Destroys: CL
;-----------------------------------------------------------------------------
    push   bx
    mov    si, [state.currfont_ptr]      ; point to current font
    mov    bx, [state.currchar]          ; get number of current char
    mov    cl, 5                         ; ...*32 -> char offset in font
    shl    bx, cl
    mov    al, [state.cursor_pos+1]      ; row
    cbw
    add    bx, ax                        ; row offset
    lea    si, [bx+si+font.data]         ; final source
    mov    di, si                        ; ...also target
    lodsb
    pop    bx
    ret

;-----------------------------------------------------------------------------
  font_update_undo_buf: ; Copy entire current font to its own undo buffer
;
; Destroys: DI, CX
;-----------------------------------------------------------------------------
    pushf                                ; preserve flags for editor's benefit
    push   si                            ; let's save this too
    push   ds
    mov    es, [seg_upper]
    mov    si, [state.currfont_ptr]
    mov    byte[si+font.can_undo], 1     ; indicate undo is possible
    mov    di, upper.font1_undo
    cmp    si, font1
    je     @f
    mov    di, upper.font2_undo
@@: memcp  si, di, UNDO/2
    pop    es                            ; restore ES <- DS
    pop    si
    popf
    ret

;-----------------------------------------------------------------------------
  font_point_to_char:  ; Point DI at row 0 of some character in current font
;
; In:       AL = number of character to point to (if calling .al)
;                otherwise points to state.currchar
;           SI-> font structure (preserved)
; Out:      DI-> row 0 of requested character
; Destroys: AX, CL
;-----------------------------------------------------------------------------
    mov    al, [state.currchar]
  .in_al:
    push   si
    add    si, font.data
    xor    ah, ah
    mov    cl, 5
    shl    ax, cl
    add    ax, si
    xchg   ax, di
    pop    si
    ret
  .hover:
    mov    al, [state.hoverchar]
    jmp    short .in_al

;-----------------------------------------------------------------------------
  font_squeeze:  ; Convert 32b/char font to [bytes/char=height] format
;
; In:       SI-> font *structure*
;           DI-> destination
; Out:      BP-> destination
;           AX = font height (bytes per char)
;           CX = 256 (!)
; Destroys: CX, AX
;-----------------------------------------------------------------------------
    push   di
    lodsw                                ; AX = height (word 0 of font struct)
    add    si, font.data-2               ; point at 32b/char data
    mov    cx, 256
    push   cx
@@: push   cx
    mov    cx, ax
    rep    movsb                         ; copy them bytes
    add    si, 32
    sub    si, ax                        ; next 'inflated' char
    pop    cx
    loop   @b
    pop    cx
    pop    bp
    ret

;-----------------------------------------------------------------------------
  font_unsqueeze:  ; Does the exact opposite of font_squeeze! <O> (no cleanup)
;
; In:       DS:DX -> source buffer to unsqueeze from
;           ES:DI -> destination
;           CX     = font height (bytes per char); preserved
; Destroys: DI, BP
;-----------------------------------------------------------------------------
    mov    si, dx
    mov    bp, 256                       ; char counter
@@: push   cx
    rep    movsb                         ; write height*bytes as is
    pop    cx
    add    di, 32
    sub    di, cx                        ; inflate char to 32 bytes
    dec    bp
    jnz    @b                            ; NEXT
    ret