; Color Viewer, Color Matcher & Color Table
; Created by rdagger 2024
; These programs allow you to cycle through the Astrocade color palette and compare colors
; Viewer: Controller 1 joystick left & right cycles through colors.  Knob provides faster
;         color cycling (requires trigger pull)
; Matcher: Controller 1 joystick left & right cycles through colors, Up & down change
;          selected row.  Knob provides faster color cycling (requires trigger pull)
; Table: Controller 1 joystick left & right cycles through color pages (trigger limits cycle to 1 column)
;          Up & down changes background color.  Knob provides faster background cycling (requires trigger pull)
; MIT License
; Gratefully aided by the wealth of information at Bally Alley
; https://ballyalley.com/ml/ml_homebrew/ml_homebrew.html
; Bally Astrocade color palette:
; https://ballyalley.com/ml/ml_docs/astrocade_palette.html
; Developed using VS Code and MAME emulator/debugger
; https://www.mamedev.org
; Assembled with Zmac 1.3 which is available on Bally Alley
; zmac -i -o colors.bin colors.asm

INCLUDE     "HVGLIB.H"              ; Home Video Game Library Header

VL	    EQU     96	                ; Number of Vertical Lines used for screen data
TBACK   EQU     $4F01               ; Table BACKground color
TBOUT   EQU     $4F02               ; Table Background color OUTline
HEX0    EQU     $4F03               ; Staring HEX0 value for color table
HEXCNT  EQU     $4F04               ; Remaining HEX CouNT to draw (0..64) for color table
HEXVAL  EQU     $4F05               ; Current color table HEX VALue
CURX    EQU     $4F06               ; CURrent X for  for color table cell
CURY    EQU     $4F07               ; CURrent Y for  for color table row
CURCO   EQU     $4F08               ; CURrent color options for color table
SELCROW EQU     $4F09               ; SELected Color ROW (matcher)
COLORS  EQU     $4F0A               ; Stored COLORS (8 bytes)
CBOUND  EQU     00010100b           ; Frame color=00, Horizontal Color BOUNDary=010100 (20=80/4)
LFL     EQU     192                 ; Low priority Foreground Line (96 x 2) for color table
X0      EQU     10                  ; Starting X (first column) – centered for color table
Y0      EQU     7                   ; Starting Y for color table
CSTEP   EQU     24                  ; Column Step for color table
RSTEP   EQU     8                   ; Row Step for color table
EXTOPTS EQU     01000010b           ; Extended Options 0=lead zero, 1=alt font, 000010=2 digits for color table 

        ORG     FIRSTC              ; First byte of Cartridge
        DB      $55                 ; User Cartridge Sentinel

VIEWERMENU:
    DW      MATCHERMENU             ; Init menu item for viewer
    DW      PRGNAME                 ; ... program 1 title address
    DW      VIEWER                  ; ... program 1 start address
MATCHERMENU:
    DW      TABLEMENU               ; Init menu item for matcher
    DW      PRGNAME2                ; ... program 2 title address
    DW      MATCHER                 ; ... program 2 start address
TABLEMENU:
    DW      MENUST                  ; Init menu item for table
    DW      PRGNAME3                ; ... program 3 title address
    DW      TABLE                   ; ... program 3 start address


; Interrupt routine table color table
;   [Divides the screen into 11 zones with different color pallets]
;   NOTE: Interrupts IRVEC1-11 must go here to ensure they are on the same page
IRVEC1:     DW      ISR1            ; Interrupt Service Routine #1
IRVEC2:     DW      ISR2            ; Interrupt Service Routine #2
IRVEC3:     DW      ISR3            ; Interrupt Service Routine #3
IRVEC4:     DW      ISR4            ; Interrupt Service Routine #4
IRVEC5:     DW      ISR5            ; Interrupt Service Routine #5
IRVEC6:     DW      ISR6            ; Interrupt Service Routine #6
IRVEC7:     DW      ISR7            ; Interrupt Service Routine #7
IRVEC8:     DW      ISR8            ; Interrupt Service Routine #8
IRVEC9:     DW      ISR9            ; Interrupt Service Routine #9
IRVEC10:    DW     ISR10            ; Interrupt Service Routine #10
IRVEC11:    DW     ISR11            ; Interrupt Service Routine #10


VIEWER:
    CALL    INIT_SCREEN             ; Initialize screen
    LD      B, 0                    ; Set default starting colors 0-7 
    CALL    SETCOLORSV              ; Set and store the colors
    CALL    DRAWCOLORSV             ; Draw palette

VIEWERLOOP:
    SYSSUK  SENTRY                  ; UPI SENse TRansition
    DW      ALKEYS                  ; ... Keymask Address (ALL KEYS)
    SYSSUK  DOIT                    ; UPI DOIT table, branch to translation handler
    DW      DOTBLV                  ; ... Table Address (Doit TABle)
    JR      VIEWERLOOP              ; Repeat main loop


MATCHER:
    CALL    INIT_SCREEN             ; Initialize screen
    SYSSUK  COLSET                  ; Set color palettes
    DW      COLTAB                  ; ... with the values at Palettes
    LD      B, 4                    ; Counter (Loop will go from last to first)
    LD      D, $F9                  ; Last color
    LD      E, 3                    ; Initial selected row
    LD      HL, SELCROW             ; Address to selected row
INITCOLORLOOP:
    LD      (HL), E                 ; Store selected row
    CALL    SETCOLORM               ; Set and store row color
    CALL    DRAWCOLORBAR            ; Draw the color bar
    LD      A, D                    ; Load current color to accumulator
    SUB     $53                     ; Next color
    LD      D, A                    ; Store color back to D 
    DEC     E                       ; Decrement selected row
    DJNZ    INITCOLORLOOP           ; Repeat until first selected row
    CALL    DRAWCOLORHEX            ; Draw the color hex values
    CALL    DRAWARROW               ; Draw arrow pointing at selected color        

MATCHERLOOP:
    SYSSUK  SENTRY                  ; UPI SENse TRansition Y
    DW      ALKEYS                  ; ... Keymask Address (ALL KEYS)
    SYSSUK  DOIT                    ; UPI DOIT table, branch to translation handler
    DW      DOTBLM                  ; ... Table Address (Doit TABle)
    JR      MATCHERLOOP             ; Repeat main loop


TABLE:
    CALL    INIT_SCREEN             ; Initialize screen

INITTABLELOOP:
    LD      A, 00001100b            ; Current color options at 3 so first will be 1
    LD      (CURCO), A              ; Store color options
    XOR     A                       ; Initialize starting hex color value to zero
    LD      (HEX0), A               ; Store starting hex value
    OUT     (COL0L), A              ; Background COL0 Left to black
    OUT     (COL0R), A              ; Background COL0 Right to black
    LD      (TBACK), A              ; Store Table BACKground color
    LD      A, IRVEC1 >> 8          ; Load Low Foreground Routine VECtor high byte
    LD      I, A                    ; Store high byte to interrupt vector register
    LD      A, IRVEC1 AND $FF       ; Load Low Foreground Routine VECtor low byte
    OUT     (INFBK), A              ; Output low byte to INterrupt FeedBacK register
    LD      A, LFL                  ; load Low priority Foreground Line
    OUT     (INLIN), A              ; Output line to INterrupt LINe register
    CALL    DRAWTABLE               ; Draw initial table
    LD      A, (TBOUT)              ; Load Table Background OUTline color
    CALL    DRAWBG                  ; Draw BackGround text

TABLELOOP:
    SYSSUK  SENTRY                  ; UPI SENse TRansition Y
    DW      ALKEYS                  ; ... Keymask Address (ALL KEYS)
    SYSSUK  DOIT                    ; UPI DOIT table, branch to translation handler
    DW      DOTBLT                  ; ... Table Address (Doit TABle)
    SYSSUK  PAWS                    ; Pause                 
    DB      11                      ; ... for 11 interrupts
    JR      TABLELOOP                ; Loop back to wait for the switch press again


; ****** HANDLEJOYV
; Purpose: Handle joystick movement (Viewer)
; Arguments: B=controller values
HANDLEJOYV:
    LD      A, B                    ; Load controller data to accumulator
    AND     00001111b               ; Mask to only joystick
    BIT     CHRIGH, A               ; Check controller right bit
    JR      Z, CHECKLEFTV           ; Jump to check left if not set
    LD      A, (COLORS)             ; Load the first color table color
    ADD     A, 8                    ; Increment the color to next 8
    LD      B, A                    ; Store new starting color
    JR      SETANDDRAWV             ; Proceed to the set and draw
CHECKLEFTV:
    BIT     CHLEFT, A               ; Check controller left bit
    RET     Z                       ; Return from subroutine if not set
    LD      A, (COLORS)             ; Load the first color table color
    SUB     8                       ; Decrement the color to previous 8
    LD      B, A                    ; Store new starting color
SETANDDRAWV:
    CALL    SETCOLORSV              ; Set and store colors
    CALL    DRAWCOLORSV             ; Draw palette
    RET                             ; Return from subroutine

; ****** HANDLEPOTV
; Purpose: Handle knob (potentiometer) movement (Viewer)
;          Trigger interlock implemented because knob is sensitive to joystick movement
; Arguments: B=pot value
HANDLEPOTV:
    IN     A, (SW0)                 ; Load accumulator with controller 1 switches
    BIT    CHTRIG, A                ; Check trigger bit (knob only works if trigger pulled)
    RET    Z                        ; Return from subroutine if trigger interlock not engaged
    LD     A, B                     ; Load pot value to accumlator
    CPL                             ; Flip pot value so counter clockwise decreases and clockwise increases
    AND    11111000b                ; Mask lower bits to limit to multiples of 8
    LD     B, A                     ; Store pot value
    LD     A, (COLORS)              ; Load previous starting color
    CP     B                        ; Check if new color equals previous
    RET    Z                        ; Return if no change
    CALL    SETCOLORSV              ; Set and store colors
    CALL    DRAWCOLORSV             ; Draw palette
    RET                             ; Return from subroutine

; ****** HANDLEJOYM
; Purpose: Handle joystick movement (Matcher)
; Arguments: B=controller values
HANDLEJOYM:
    LD      A, B                    ; Load controller data to accumulator
    AND     00001111b               ; Mask to only joystick
CHECKDOWNM:
    BIT     CHDOWN, A               ; Check controller down bit
    JR      Z, CHECKUPM             ; Jump to check down if not set
    LD      A, (SELCROW)            ; Load the first color table color
    INC     A                       ; Increment the selected color row
    AND     3                       ; Roll over selected color to zero at 4
    JR      SETANDDRAWM             ; Proceed to the set and draw
CHECKUPM:
    BIT     CHUP, A                 ; Check controller up bit
    JR      Z, CHECKLEFTM           ; Skip to left check if not set
    LD      A, (SELCROW)            ; Load the first color table color
    DEC     A                       ; Decrement the selected color row
    CP      $FF                     ; Check if minimum reached
    JR      NZ, SETANDDRAWM         ; Continue to set and draw
    ADD     A, 4                    ; Rollover to maximum
SETANDDRAWM:
    LD      (SELCROW), A            ; Store new selected row
    CALL    GETSELECTEDADDRESS      ; Get color address of selected color
    LD      D, (HL)                 ; Load selected color
    CALL    SETCOLORM               ; Set and store row color
    CALL    DRAWCOLORHEX            ; Draw color hex values
    CALL    DRAWARROW               ; Draw arrow pointing at selected color
    RET                             ; Return from subroutine
CHECKLEFTM:
    BIT     CHLEFT, A               ; Check controller left bit   
    JR      Z, CHECKRIGHTM          ; Jump to check right if not set
    CALL    GETSELECTEDCOLOR        ; Get the selected color
    DEC     A                       ; Decrease selected color
    JR      CHANGESELECTEDCOLOR     ; Jump to change selected color
CHECKRIGHTM:
    BIT     CHRIGH, A               ; Check controller right bit 
    RET     Z                       ; Return if not set
    CALL    GETSELECTEDCOLOR        ; Get the selected color
    INC     A                       ; Increase selected color
CHANGESELECTEDCOLOR:
    CALL   GETSELECTEDADDRESS       ; Get color address of selected color
    LD     (HL), A                  ; Store color
    LD     D, A                     ; Place color byte in register D
    CALL   SETCOLORM                ; Set and store row color
    CALL   DRAWCOLORBAR             ; Draw color bar
    CALL   DRAWCOLORHEX             ; Draw color hex values
    CALL   DRAWARROW                ; Draw arrow pointing at selected color
    RET                             ; Return from subroutine

; ****** HANDLEPOTM
; Purpose: Handle knob (potentiometer) movement (Matcher)
;          Trigger interlock implemented because knob is sensitive to joystick movement
; Arguments:    B=pot value
HANDLEPOTM:
    IN     A, (SW0)                 ; Load accumulator with controller 1 switches
    BIT    CHTRIG, A                ; Check trigger bit (knob only works if trigger pulled)
    RET    Z                        ; Return from subroutine if trigger interlock not engaged
    IN      A, (POT0)               ; Reload pot value to ensure it is the latest (REQUIRED)
    CPL                             ; Flip pot value so counter clockwise decreases and clockwise increases
    LD     D, A                     ; Store pot value to D
    CALL   SETCOLORM                ; Set and store row color
    CALL   DRAWCOLORBAR             ; Draw color bar
    CALL   DRAWCOLORHEX             ; Draw color hex values
    CALL   DRAWARROW                ; Draw arrow pointing at selected color
    RET                             ; Return from subroutine

; ****** HANDLEJOYT
; Purpose: Handle joystick movement (Table)
; Arguments: B=controller values
HANDLEJOYT:
    LD      A, B                    ; Load controller data to accumulator
    AND     00001111b               ; Mask to only joystick & trigger
CHECKDOWNT:
    BIT     CHDOWN, A               ; Check controller down bit
    JR      Z, CHECKUPT             ; Jump to check down if not set
    LD      B, -1                   ; Set background adjuster to decrement
    JR      TABLEBG                 ; Jump to table background
CHECKUPT:
    BIT     CHUP, A                 ; Check controller up bit
    JR      Z, CHECKLEFTT           ; Skip to left check if not set
    LD      B, 1                    ; Set background adjuster to increment
TABLEBG:
    LD      A, (TBACK)              ; Load Table BACKground color
    ADD     A, B                    ; Decrement background color
    OUT     (COL0L), A              ; Background COL0 Left to black
    OUT     (COL0R), A              ; Background COL0 Right to black
    LD      (TBACK), A              ; Store Table BACKground color
DRAWBG:
    AND     $0F                     ; Mask color to retrieve luminance
    CP      $04                     ; Compare luminance
    JR      C, DARKBG               ; If luminance < $4 (range $0-$3) skip to dark luminance
    CP      $08                     ; Compare luminance
    JR      C, LIGHTBG              ; If luminance < $8 (range $4-$7) skip to light luminance
    CP      $0C                     ; Compare luminance
    JR      C, DARKBG               ; If luminance < $C (range $8-$B) skip to dark luminance
LIGHTBG
    XOR     A                       ; Background color is light so set background text outline to black
    LD      (TBOUT), A              ; Store Table Background OUTline color
    JR      DRAWBGTEXT              ; Jump to background text & outline drawing
DARKBG:
    LD      A, 7                    ; Background color is dark so set background text outline to white
    LD      (TBOUT), A              ; Store Table Background OUTline color
DRAWBGTEXT:
    SYSSUK  RECTAN                  ; Draw filled rectangle for background text outline 
    DB      128                     ; ... X coordinate for UL corner of rectangle
    DB      82                      ; ... Y coordinate for UL corner of rectangle
    DB      16                      ; ... X-size of rectangle in pixels
    DB      7                       ; ... Y-size of rectangle in pixels
    DB      11111111b               ; ... Color mask
    LD      HL, TBACK               ; Load current background color address
    LD      B, EXTOPTS              ; Load extended options
    LD      C, 00000011b            ; Load character options for this column
    LD      D, 83
    LD      E, 130
    SYSTEM  DISNUM                  ; Display hex background color value
    SYSSUK  RECTAN                  ; Draw filled rectangle for background text outline 
    DB      128                     ; ... X coordinate for UL corner of rectangle
    DB      82                      ; ... Y coordinate for UL corner of rectangle
    DB      2                       ; ... X-size of rectangle in pixels
    DB      7                       ; ... Y-size of rectangle in pixels
    DB      11111111b               ; ... Color mask
    RET                             ; Return from subroutine
CHECKLEFTT:
    BIT     CHLEFT, A               ; Check controller left bit   
    JR      Z, CHECKRIGHTT          ; Jump to check right if not set
    IN     A, (SW0)                 ; Load accumulator with controller 1 switches
    BIT    CHTRIG, A                ; Check trigger bit
    LD      A, -64                  ; Set decrement to 1 page
    JR      Z, MOVETABLE            ; Skip to move table if trigger modifier not active
    LD      A, -11                  ; Set decrement to 1 column
    JR      MOVETABLE               ; Jump to move table
CHECKRIGHTT:
    BIT     CHRIGH, A               ; Check controller left bit   
    RET     Z                       ; Return if no joystick movement
    IN     A, (SW0)                 ; Load accumulator with controller 1 switches
    BIT    CHTRIG, A                ; Check trigger bit
    LD      A, 64                   ; Set increment to 1 page
    JR      Z, MOVETABLE            ; Skip to move table if trigger modifier not active
    LD      A, 11                   ; Set increment to 1 column
MOVETABLE:
    LD      HL, HEX0                ; Load starting hex color value address
    ADD     A, (HL)                 ; Adjust starting hex color value
    LD      (HL), A                 ; Save starting hex color value
    CALL    DRAWTABLE               ; Redraw Table
    JR      DRAWBGTEXT              ; Draw BackGround text

; ****** HANDLEPOTT
; Purpose:      Handle knob (potentiometer) movement (Table)
;               Trigger interlock implemented because knob is sensitive to joystick movement
; Arguments:    B=pot value
HANDLEPOTT:
    IN     A, (SW0)                 ; Load accumulator with controller 1 switches
    BIT    CHTRIG, A                ; Check trigger bit (knob only works if trigger pulled)
    RET    Z                        ; Return from subroutine if trigger interlock not engaged
    IN      A, (POT0)               ; Reload pot value to ensure it is the latest (REQUIRED)
    CPL                             ; Flip pot value so counter clockwise decreases and clockwise increases
    OUT     (COL0L), A              ; Background COL0 Left to black
    OUT     (COL0R), A              ; Background COL0 Right to black
    LD      (TBACK), A              ; Store Table BACKground color
    JR      DRAWBG                  ; Redraw the background text (Return handled by jumped routine)


; ****** SETCOLORSV
; Purpose: Set and store colors (Viewer)
; Arguments: B=starting color
; Clobbers: A, BC, HL
SETCOLORSV:
    LD      HL, COLORS              ; Load starting address
    LD      C, 0                    ; Set up a counter
FILL_LOOPV:
    LD      A, 7                    ; Need to flip output port
    SUB     C                       ; Subtract counter from maximum port   
    PUSH    BC                      ; Store BC to stack
    LD      C, A                    ; Load port
    OUT     (C), B                  ; Output color to port C (register C is required)
    POP     BC                      ; Restore BC from stack
    LD      (HL), B                 ; Store the color value to memory
    INC     HL                      ; Next color memory location
    INC     B                       ; Increment B to store the next value (0 to 7)
    INC     C                       ; Increment counter
    LD      A, C                    ; Load counter to accumulator
    CP      8                       ; Check counter
    RET     Z                       ; Return if counter complete
    JR      FILL_LOOPV              ; Repeat loop

; ****** DRAWCOLORSV
; Purpose:      Draw color bands (Viewer)
; Clobbers: A, BC, DE, HL
DRAWCOLORSV:
    XOR     A                       ; Color mask = 0
    LD      B, $60                  ; Height of color bar 
    LD      C, $14                  ; Width of color bar
    LD      D, $00                  ; Y-coordinate
    LD      E, $8C                  ; Starting X coordinate
    LD      HL, COLORS+7            ; Color index
COLORLOOPV:
    SYSTEM  RECTAN                  ; Draw wall rectangle
    PUSH    BC                      ; Store BC to stack
    LD      C, 00001100b            ; 00=no enlarge, 00=plop, 1100=colors
    CALL    DISPHEX                 ; Display color hex value
    POP     BC                      ; Restore BC from stack
    CP      $FF                     ; Check if maximum color mask reached
    JR      NZ, NEXTCOLORV          ; Skip to next color mask
    XOR     A                       ; Roll over color mask to zero if maximum reached
    JR      NEXTBARV                ; Skip to next color bar
NEXTCOLORV:
    ADD     A, $55                  ; Next color mask
NEXTBARV:
    PUSH    AF                      ; Store AF to stack
    LD      A, E                    ; Load current X coordinate to accumulator
    OR      A                       ; Check if X is zero
    JR      Z, ENDCOLORLOOPV        ; Skip to end if zero
    SUB     C                       ; Subtract color bar width from X
    LD      E, A                    ; Load new X back to register E
    POP     AF                      ; Restore AF from stack
    DEC     HL                      ; Decrement color address
    JR      COLORLOOPV              ; Repeat loop
ENDCOLORLOOPV:
    POP     AF                      ; Restore AF from stack
    RET                             ; Return from subroutine

; ****** SETCOLORM
; Purpose:      Set and store row color (Matcher)
; Arguments:    D=specified color
; Clobbers: A, C
SETCOLORM:
    PUSH    HL                      ; Store HL to stack
    LD      HL, COLORS              ; Load starting address
    LD      A, (SELCROW)            ; Load selected color row
    LD      C, 0                    ; Set up a counter
    OR      A                       ; Test for row zero
    JR      Z, SETROW               ; Skip loop if zero
GETROW:
    INC     C                       ; Increment counter
    INC     HL                      ; Increment color address
    CP      C                       ; Check if counter equals selected row
    JR      NZ, GETROW              ; Repeat loop until selected row reached
SETROW:
    LD      (HL), D                 ; Store color
    OUT     (C), D                  ; Output color
; Background frame color
    LD      A, C                    ; Load selected row / color mask value to accumulator
    RRCA                            ; Rotate right twice to move color mask to
    RRCA                            ; ... frame color position
    AND     $C0                     ; Mask out bits to keep frame color
    OR      CBOUND                  ; OR with frame color, horizontal color boundary
    OUT     (HORCB), A              ; Set frame color to selected color
; Match left side frame color to selected color
    LD      A, C                    ; Load selected row / color mask value to accumulator
    ADD     A, 4                    ; Add 4 to set to left side colors
    LD      C, A                    ; Store left side output color register
    OUT     (C), D                  ; Output selected color to matching color number on left side
; Left side drawing color
    XOR     1                       ; Select new left side drawing color mask value
    PUSH    AF                      ; Store color mask value for left side drawing color
    LD      C, A                    ; Store left side drawing color register
    LD      A, $07                  ; Color white
    OUT     (C), A                  ; Output drawing color white
; Left side background color      
    POP     AF                      ; Restore color mask value for left side drawing color
    XOR     3                       ; Select new left side background color mask value
    LD      C, A                    ; Store left side drawing color register
    XOR     A                       ; Color black
    OUT     (C), A                  ; Output drawing color black
    POP     HL                      ; Restore HL from stack
    RET                             ; Return from subroutine

; ****** DRAWARROW
; Purpose: Draw arrow pointing at selected color (Matcher)
; Clobbers: A, BC, DE
DRAWARROW:
    LD      D, $5                   ; Y-coordinate offset
    LD      E, $38                  ; X coordinate
    LD      A, (SELCROW)            ; Load selected color row
    OR      A                       ; Check if row zero
    JR      Z, DRAWARROWTEXT        ; Skip to draw arrow text if zero
    LD      B, A                    ; Load selected row to B
    XOR     A                       ; Zero accumulator
DRAWARROWYLOOP:
    ADD     A, $18                  ; Add height to Y
    DJNZ    DRAWARROWYLOOP          ; Repeat loop until selected row reached
DRAWARROWTEXT:
    ADD     A, D                    ; Add Y coordinate offset to Y
    LD      D, A                    ; Load Y coordinate to D
    CALL    GETSELECTEDCOLOR        ; Get the selected color
    AND     $0F                     ; Mask color to retrieve luminance
    CP      $04                     ; Compare luminance
    JR      C, DARKLUM              ; If luminance < $4 (range $0-$3) skip to dark luminance
    CP      $08                     ; Compare luminance
    JR      C, LIGHTLUM             ; If luminance < $8 (range $4-$7) skip to light luminance
    CP      $0C                     ; Compare luminance
    JR      C, DARKLUM              ; If luminance < $C (range $8-$B) skip to dark luminance
    CP      $10                     ; Compare luminance
    JR      C, LIGHTLUM             ; If luminance < $10 (range $C-$F) skip to light luminance
LIGHTLUM
    LD      A, (SELCROW)            ; Load selected color row
    XOR     2                       ; Set accumulator to black color mask for light background
    JR      SETLUM                  ; Jump to set luminance
DARKLUM:
    LD      A, (SELCROW)            ; Load selected color row            
    XOR     1                       ; Set accumulator to white color mask for dark background
SETLUM:
    RLA                             ; Rotate drawing color bits left twice
    RLA                             ; ... to position before background bits
    OR      01000000b               ; Set text enlargement 
    LD      C, A                    ; Load color mask to C
    LD      A, (SELCROW)            ; Load selected color row (color mask)
    OR      C                       ; Set color mask background to selected color
    LD      C, A                    ; Load color mask to C
    LD      A, "a"                  ; Set text character to right arrow
    SYSTEM  CHRDIS                  ; Draw arrow to screen
    RET                             ; Return from subroutine

; ****** DRAWCOLORBAR
; Purpose: Draw color bar (Matcher)
; Clobbers: A
DRAWCOLORBAR:
    PUSH    BC                      ; Preserver BC to stack
    PUSH    DE                      ; Preserver DC to stack
    LD      C, $00                  ; Color mask
    LD      D, $00                  ; Y-coordinate
    LD      A, (SELCROW)            ; Load selected color row
    LD      E, A                    ; Store selected color
    OR      A                       ; Check if selected color is zero
    JR      Z, DRAWBAR              ; Skip to draw bar
    LD      B, 0                    ; Initialize counter
LOOPCOLORM:
    INC     B                       ; Increment counter
    LD      A, $55                  ; Next color mask
    ADD     A, C                    ; Adjust color mask
    LD      C, A                    ; Store color mask
    LD      A, $18                  ; Next Y coordinate
    ADD     A, D                    ; Adjust Y coordinate
    LD      D, A                    ; Store Y coordinate
    LD      A, E                    ; Load selected color
    CP      B                       ; Test if selected color reached
    JR      NZ, LOOPCOLORM          ; Repeat loop until color reached
DRAWBAR:
    LD      A, C                    ; Color mask
    LD      C, $50                  ; X-size (1/2 horizontal screen)
    LD      B, $18                  ; Y-size (1/4 vertical screen)
    LD      E, $50                  ; X-coordinate (1/2 horizontal screen)
    SYSTEM RECTAN                   ; Draw color bar rectangle
    POP     DE                      ; Restore DE from stack
    POP     BC                      ; Restore BC from stack
    RET                             ; Return from subroutine

; ****** DRAWCOLORHEX
; Purpose: Draw color bar hex values (Matcher)
; Clobbers: A, BC, DE, HL
DRAWCOLORHEX:
    LD      D, $5                   ; Starting Y-coordinate
    LD      E, $1                   ; X coordinate
    LD      HL, COLORS              ; Color index
    LD      A, (SELCROW)            ; Load selected color row
    XOR     2                       ; Background color mask value
    PUSH    DE                      ; Preserve DC to stack
    CALL    EXPANDCOLOR             ; Expand 2 bit color mask to other 3 pixels
    LD      B, VL                   ; Height
    LD      C, $24                  ; Width
    LD      D, $00                  ; Y coordinate
    LD      E, $00                  ; X coordinate
    SYSTEM  RECTAN                  ; Draw black rectangle background for hex values
    LD      C, $2C                  ; Width
    LD      E, $24                  ; X coordinate
    LD      A, (SELCROW)            ; Load selected color row (for color mask)
    CALL    EXPANDCOLOR             ; Expand 2 bit color mask to other 3 pixels
    SYSTEM  RECTAN                  ; Fill remaining portion of left screen with selected color
    POP     DE                      ; Restore DE from stack
    CALL    GETTEXTCOLORMASK        ; Get color mask for color hex text
    OR      01000000b               ; Set text enlargement
    LD      C, A                    ; Store combined color mask
DRAWCOLORHEXLOOP:
    CALL    DISPHEX                 ; Display color hex value
    LD      A, D                    ; Load current Y coordinate to accumulator
    CP      $4D                     ; Check if Y is at maximum
    RET     Z                       ; Return from subroutine when maximum reached
    INC     HL                      ; Increment color address
    ADD     A, $18                  ; Increase Y to next color bar
    LD      D, A                    ; Store Y to D
    JR      DRAWCOLORHEXLOOP        ; Repeat loop

; ****** GETTEXTCOLORMASK
; Purpose: Get the 4 bit color mask for drawing text (Matcher)
; Returns: A=color mask
; Clobbers: A
GETTEXTCOLORMASK:
    PUSH    BC                      ; Preserve BC to stack
    LD      A, (SELCROW)            ; Load selected color row
    XOR     2                       ; Background color mask value
    LD      B, A                    ; Store background color
    XOR     3                       ; Drawing color mask value
    RLA                             ; Rotate drawing color bits left
    RLA                             ; ... to position before background bits
    OR      B                       ; Combine drawing and background color masks
    POP     BC                      ; Restore BC from stack
    RET                             ; Return from subroutine

; ****** GETSELECTEDADDRESS
; Purpose: Get the address of the selected row color (Matcher)
; Returns: HL=selected address
; Clobbers: HL
GETSELECTEDADDRESS:
    PUSH    AF                      ; Preserve AF to stack
    PUSH    BC                      ; Preserve BC to stack
    LD      A, (SELCROW)            ; Load selected color row
    LD      C, A                    ; Store selected color row to C
    LD      HL, COLORS              ; Load colors address          
    OR      A                       ; Check if row zero selected
    JR      Z, GETCOLORLOOPDONE     ; Skip done if zero
    XOR     A                       ; Zero counter
GETCOLORLOOP:   
    INC     HL                      ; Next color address
    INC     A                       ; Increment counter
    CP      C                       ; Check if counter equals selected color row
    JR      NZ, GETCOLORLOOP        ; Repeat until counter equals row
GETCOLORLOOPDONE:
    POP     BC                      ; Restore BC from stack
    POP     AF                      ; Restore AF from stack
    RET                             ; Return from subroutine

; ****** GETSELECTEDCOLOR
; Purpose:      Returns the selected color (Matcher)
; Returns:      A=Selected color
GETSELECTEDCOLOR:
    PUSH    HL                      ; Preserve HL to stack
    CALL    GETSELECTEDADDRESS      ; Get selected color address
    LD      A, (HL)                 ; Load selected color
    POP     HL                      ; Restore HL from stack
    RET                             ; Return from subroutine

; ****** EXPANDCOLOR
; Purpose: Expand 2 bit color mask to 8 bit color mask
; Arguments: A=2 bit color mask
; Returns: A=8 bit color mask
EXPANDCOLOR:
    PUSH    BC                      ; Preserve BC to stack
    LD      B, A                    ; Preserve initial color mask
    RLCA                            ; Shift left 2 bits
    RLCA                            ; ... (bit 0 -> bit 2)
    OR      B                       ; Combine shifted with the original value
    RLCA                            ; Shift left 2 bits
    RLCA                            ; ... (bit 2 -> bit 4)
    OR      B                       ; Combine shifted with the original value
    RLCA                            ; Shift left 2 bits
    RLCA                            ; ... (bit 4 -> bit 6)
    OR      B                       ; Combine shifted with the original value
    POP     BC                      ; Restore BC from stack
    RET                             ; Return from subroutine

; ****** INIT_SCREEN routine
; Purpose: Set up screen boundaries, colors and label
INIT_SCREEN:                        ; Initialize screen
    DI                              ; Disable interrupts
    SYSTEM  INTPC                   ; Begin interpreter mode
    
    DO      SETOUT                  ; Set output ports
    DB      VL*2                    ; ... with VBLANK line set to line 96
    DB      CBOUND                  ; ... Frame color, Horizontal Color Boundary
    DB      00001000b               ; ... with screen interrupt enabled 

    DO      (FILL)                  ; Screen fill
    DW      NORMEM                  ; ... Destination 4000h
    DW      (VL*40)                 ; ... Bytes to move
    DB      00000000b               ; ... Fill with

    EXIT                            ; Exit interpreter mode
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; ****** DISPHEX
; Purpose: Draw color values in hex
; Arguments: C=color mask, D=Y, E=X, HL=Address of value to display
DISPHEX:
    PUSH    HL                      ; Preserve registers to stack
    PUSH    DE
    PUSH    AF  
    LD      A, (HL)                 ; Load value to display from color table
    PUSH    AF 
    PUSH    BC                     
    INC     E                       ; Adjust X coordinate for slight margin
    INC     E
    INC     E
	LD      C, A                    ; Load value to display
	SRL     C                       ; Shift right logical
	SRL     C                       ; Shift right logical
	SRL     C                       ; Shift right logical
	SRL     C                       ; Shift right logical
	LD      B, 0                    ; Zero B
	LD      HL, HEXDIG              ; Load address of hex digits
	ADD     HL, BC                  ; Adjust address for the current digit
	LD      A, (HL)                 ; Load the digit ASCII
	POP     BC
    SYSTEM  (CHRDIS)                ; Display character
	POP     AF                      ; Restore byte to display from stack
    PUSH    BC
	AND     $0F                     ; Mask out upper nibble 
	LD      C, A                    ; Load value to display to C
	LD      B, 0                    ; Zero B
	LD      HL, HEXDIG              ; Load address of hex digits
	ADD     HL, BC                  ; Adjust address for the current digit
	LD      A, (HL)                 ; Load the digit ASCII
	POP     BC
    SYSTEM  (CHRDIS)                ; Display character
    POP     AF                      ; Restore registers from stack
    POP     DE
    POP     HL
    RET                             ; Return from subroutine
HEXDIG:
    DB      "0123456789ABCDEF"      ; Hexadecimal digits

; ****** DRAWTABLE
; Purpose: Draws 64 hexadecimal colors values starting at HEX0 in a 6x11 grid
; Arguments: C=color mask, D=Y, E=X, HL=Address of value to display
; Clobbers: AF, BC, DE, HL
DRAWTABLE:
    SYSSUK FILL                     ; Clear screen
    DW      $4000                   ; .. destination address
    DW      (VL*40)                 ; .. total bytes to clear (byte width x Vertical Lines)
    DB      00000000b               ; .. black color mask
    LD      IX, ALTFONT3            ; Load address of custom ALTernative FONT    
    LD      A, (HEX0)               ; Load starting HEX0 color value
    LD      (HEXVAL), A             ; Store starting HEX color VALue
    LD      A, 64                   ; Set counter to 64
    LD      (HEXCNT), A             ; Store counter
    LD      A, X0                   ; Load starting X0 coordinate
    LD      (CURX), A               ; Store CURrent X
    LD      B, 6                    ; Set number of columns
ColLoopOuter:
    LD      A, Y0                   ; Load starting Y0 coordinate
    LD      (CURY), A               ; Store CURrent Y
    LD      HL, CURCO               ; Load address of CURrent Character Options
    LD      A, (HL)                 ; Load current color options   
    ADD     A, 00000100b            ; Change mask to next color (valid 1-3)
    CP      00010000b               ; Check if color overflowed past color 3
    JR      NZ, COLORMASK           ; Continue to COLOR MASK if no overflow
    LD      A, 00000100b            ; Wrap back to color 1
COLORMASK:
    LD      (HL), A                 ; Store color options for current column
    LD      C, 11                   ; Set 11 rows per column
RowLoopInner:
    LD      A, (CURY)               ; Load CURrent Y coordinate
    LD      D, A                    ; Store current Y
    LD      A, (CURX)               ; Load CURrent X coordinate
    LD      E, A                    ; Store current X
    PUSH    BC                      ; Preserve row & column counters
    LD      HL, HEXVAL              ; Load current HEX color VALue
    LD      B, EXTOPTS              ; Load EXTended OPTionS
    LD      A, (CURCO)              ; Load CURrent Character Options for this column
    LD      C, A                    ; Store standard alphanumeric character options byte
    SYSTEM  DISNUM                  ; Display hex color value
    POP     BC                      ; Restore row & column counters
    LD      HL, HEXVAL              ; Load address of current HEX color VALue
    INC     (HL)                    ; Increment hex color value
    LD      HL, HEXCNT              ; Load address of HEX color CouNTer
    DEC     (HL)                    ; Decrement outer loop counter
    LD      A, (HL)                 ; Load counter
    OR      A                       ; Check if counter reached zero
    JR      Z, DoneHexGrid          ; Jump if loop done
    LD      A, (CURY)               ; Load CURrent Y
    ADD     A, RSTEP                ; Increment Y by Row STEP
    LD      (CURY), A               ; Save CURrent Y
    DEC     C                       ; Decrement inner loop counter
    JR      NZ, RowLoopInner        ; Continue inner loop if counter not zero
    LD      A, (CURX)               ; Load CURrent X
    ADD     A, CSTEP                ; Increment X by Column STEP
    LD      (CURX), A               ; Save CURrent X
    DJNZ    ColLoopOuter            ; Continue outer loop if counter not zero
DoneHexGrid:
    RET                             ; Return from subroutine


; --- ALTFONT: FoNT descriptor for custom 5x6 font
ALTFONT3:
    DB      $AA                     ; FonT BASE character (42 + 10000000b)
    DB      $06                     ; FonT Frame X Size width
    DB      $06                     ; FonT Frame Y Size height
    DB      $01                     ; FonT X size of char in BYTEs
    DB      $05                     ; FonT Y SIZe height in bits
    DW      FONT3                   ; FonT Pattern Table address

FONT3:
    DEF5    $70, $88, $F8, $88, $88 ; A
    DEF5    $F0, $88, $F0, $88, $F0 ; B
    DEF5    $78, $80, $80, $80, $78 ; C
    DEF5    $F0, $88, $88, $88, $F0 ; D
    DEF5    $F8, $80, $E0, $80, $F8 ; E
    DEF5    $F8, $80, $E0, $80, $80 ; F
    DEF5    $F8, $98, $98, $98, $F8 ; 0
    DEF5    $30, $70, $30, $30, $78 ; 1
    DEF5    $F8, $18, $78, $80, $F8 ; 2
    DEF5    $F8, $18, $78, $18, $F8 ; 3
    DEF5    $98, $98, $F8, $18, $18 ; 4
    DEF5    $F8, $80, $F8, $18, $F8 ; 5
    DEF5    $78, $80, $F8, $98, $F8 ; 6
    DEF5    $F8, $18, $18, $30, $30 ; 7
    DEF5    $F8, $98, $F8, $98, $F8 ; 8
    DEF5    $F8, $88, $F8, $18, $18 ; 9


; Interrupt Service Routine #1
; ----------------------------
;   1) Setup next interrupt (IRVEC2) to occur on screen Line 14
;   2) Output white to color registers COL1L for number 1
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR1:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 14*2                 ; Screen line [below summary text and player]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC2 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #2
; ----------------------------
;   1) Setup next interrupt (IRVEC3) to occur on screen Line 22
;   2) Output green to color registers COL1L for number 2
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR2:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 22*2                 ; Screen line [below treasure row #1]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC3 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    INC     A                       ; Increment color for row 2
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #3
; ----------------------------
;   1) Setup next interrupt (IRVEC4) to occur on screen Line 30
;   2) Output cyan to color registers COL1L for number 3
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR3:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 30*2                 ; Screen line [below treasure row #2]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC4 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 2                    ; Adjust color for row 3
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #4
; ----------------------------
;   1) Setup next interrupt (IRVEC5) to occur on screen Line 38
;   2) Output orange to color registers COL1L for number 4
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR4:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 38*2                 ; Screen line [below treasure row #2]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC5 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 3                    ; Adjust color for row 4
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #5
; ----------------------------
;   1) Setup next interrupt (IRVEC1) to occur on screen Line 46
;   2) Output violet to color registers COL1L for number 5
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR5:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 46*2                 ; Screen line [below escaped text]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC6 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 4                    ; Adjust color for row 5
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #6
; ----------------------------
;   1) Setup next interrupt (IRVEC2) to occur on screen Line 54
;   2) Output white to color registers COL1L for number 6
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR6:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 54*2                 ; Screen line [below summary text and player]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC7 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 5                    ; Adjust color for row 6
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #7
; ----------------------------
;   1) Setup next interrupt (IRVEC3) to occur on screen Line 62
;   2) Output green to color registers COL1L for number 7
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR7:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 62*2                 ; Screen line [below treasure row #1]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC8 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 6                    ; Adjust color for row 7
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #8
; ----------------------------
;   1) Setup next interrupt (IRVEC4) to occur on screen Line 70
;   2) Output cyan to color registers COL1L for number 8
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR8:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 70*2                 ; Screen line [below treasure row #2]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC9 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 7                    ; Adjust color for row 8
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #9
; ----------------------------
;   1) Setup next interrupt (IRVEC5) to occur on screen Line 78
;   2) Output orange to color registers COL1L for number 9
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR9:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 78*2                 ; Screen line [below treasure row #2]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC10 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 8                    ; Adjust color for row 9
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    ADD     A, 11                   ; Next color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #10
; ----------------------------
;   1) Setup next interrupt (IRVEC1) to occur on screen Line 86
;   2) Output violet to color registers COL1L for X-Coordinate
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR10:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 86*2                 ; Screen line [below escaped text]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC11 & $FF        ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 9                    ; Adjust color for row 10
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    LD      A, (TBOUT)              ; Load Table Background OUTline color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine

; Interrupt Service Routine #11
; ----------------------------
;   1) Setup next interrupt (IRVEC1) to occur on screen Line 94
;   2) Output violet to color registers COL1L for X-Coordinate
; NOTE: IM2 vector table is in page $20xx. I is set once in init.
;       ISRs don't need to reload I. Instead update only low byte via INFBK.
ISR11:       
    EX      AF,AF'                  ; Preserve AF with shadow register
    LD      A, 94*2                 ; Screen line [below escaped text]
    OUT     (INLIN), A              ; Port $D, INterrupt LINe
    LD      A, IRVEC1 & $FF         ; Get Low Order Byte Address
    OUT     (INFBK), A              ; Port $D, INterrupt FeedBacK register
    LD      A, (HEX0)               ; Load staring hex color value
    ADD     A, 10                   ; Adjust color for row 11
    OUT     (COL1L), A              ; COLor 1 Left
    ADD     A, 11                   ; Next color
    OUT     (COL2L), A              ; COLor 2 Left
    ADD     A, 11                   ; Next color
    OUT     (COL3L), A              ; COLor 3 Left
    ADD     A, 11                   ; Next color
    OUT     (COL1R), A              ; COLor 1 Right
    ADD     A, 11                   ; Next color
    OUT     (COL2R), A              ; COLor 2 Right
    LD      A, (TBOUT)              ; Load Table Background OUTline color
    OUT     (COL3R), A              ; COLor 3 Right
    EX      AF,AF'                  ; Restore AF
    EI                              ; Enable interrupts
    RET                             ; Return from subroutine


; ****** DOit TaBLes
; Purpose: Handle user joystick input (Viewer)
DOTBLV:
        RC SP0, HANDLEPOTV, $00     ; Routine to handle knob movement
        RC SJ0, HANDLEJOYV, ENDx    ; Routine to handle joystick movement

; Purpose: Handle user joystick input (Matcher)
DOTBLM:
        RC SP0, HANDLEPOTM, $00     ; Routine to handle knob movement
        RC SJ0, HANDLEJOYM, ENDx    ; Routine to handle joystick movement

; Purpose: Handle user joystick input (Table)
DOTBLT:
        RC SP0, HANDLEPOTT, $00     ; Routine to handle knob movement
        RC SJ0, HANDLEJOYT, ENDx    ; Routine to handle joystick movement

PRGNAME:    
    DB      "COLOR VIEWER"          ; Program 1 name
    DB      0                       ; ... must be followed by 0

PRGNAME2:    
    DB      "COLOR MATCHER"         ; Program 2 name
    DB      0                       ; ... must be followed by 0

PRGNAME3:    
    DB      "COLOR TABLE"           ; Program 3 name
    DB      0                       ; ... must be followed by 0

; Color Table
; https://ballyalley.com/ml/ml_docs/astrocade_palette.html
COLTAB:   
    DB      $00                     ; Color 3 Left Black
    DB      $00                     ; Color 2 Left Black 
    DB      $00                     ; Color 1 Left Black
    DB      $07                     ; Color 0 Left White
    DB      $00                     ; Color 3 Right Black
    DB      $53                     ; Color 2 Right Red
    DB      $A6                     ; Color 1 Right Green
    DB      $F9                     ; Color 0 Right Blue
