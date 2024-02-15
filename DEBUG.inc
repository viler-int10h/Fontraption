if DEBUGMODE

;-----------------------------------------------------------------------------
  debug_exit: ; Set up 80x25 mode before we're out
;-----------------------------------------------------------------------------
    mov    ax, 3
    int    10h
    jmp    error_exit

;-----------------------------------------------------------------------------
;################################  MESSAGES  #################################
;-----------------------------------------------------------------------------

    debug_msg_cr:  db 13,10,'$'

    dmsg   start, 'ESC TO EXIT, ANY OTHER KEY TO ADVANCE',13,10,13,10, \
                  'Video mode 3 was set.',13,10, \
                  'Clear plane 2 and read 8x14 font:'

    dmsg   read_8x8,   'Read 8x8 font:'
    dmsg   read_8x16,  'Read 8x16 font:'
    dmsg   copy_8x8,   'Copy 8x8 to font RAM:'
    dmsg   read_9dot,  'Read alternate 9-dot chars:'
    dmsg   noblink,    'Disable blinking:'
    dmsg   nocursor,   'Disable cursor:'
    dmsg   read_clock, 'Read clocking mode register:'
    dmsg   mod_uifont, 'Modify UI font data:'
    dmsg   userfonts,  'Set user fonts:'
    dmsg   re_pal,     'Rearrange palette entries:'
    dmsg   dac1,       'Set DAC register block 1:'
    dmsg   dac2,       'Set DAC register block 2:'
    dmsg   fin_init,   'Finish initial VGA setup:'
    dmsg   crtcvert,   'Modify vertical CRTC regs:'
    dmsg   charmap,    'Set Char Map Select:'
    dmsg   vid_en,     'Video enable:'

end if