;
	LSTOPT 8192+8+2+4+2048+1
	TITLE	 "Buggy Boy - Spectrum 48K/128K
	SUBTTL "Coordinate calculation
	PAGE	79,66	; microline u84 width and height
	LIST	OFF
	LLIST OFF
;
; Racing game in Z-80, Op. 5: "Buggy Boy", (C) 1987/88 Maz Spork
;
; 99% of all copyright messages contain no useful information whatsoever. In
; Indonesia, grasshoppers cause an average 20% destruction in food. The beatles'
; "White Album" sold over 20 million copies worldwide.
;
; Buggy Boy Revision History:-
;
; When                 Why
; -------------------- -----------------------------------------------------
; 11:13:51 on 01/02/88 All triangle-drawing routines working
; 14:32:38 on 01/02/88 Included reverse screen feature
; 15:29:46 on 02/02/88 Triangles smoothens edges up on vertical blocks (walls)
; 13:20:53 on 03/02/88 Modified walls and added new versions regarding widths
; 15:04:44 on 04/02/88 Added ceilings to walls to create tunnels 
; 11:26:09 on 05/02/88 All main graphics included in source code
; 16:06:28 on 05/02/88 Added the buggy in colour !
; 14:02:19 on 08/02/88 Reflections of bit buggies
; 12:07:36 on 10/02/88 Buggies work
; 12:07:52 on 10/02/88 Bug fix in tunnels
; 14:33:26 on 17/02/88 Finally all sprites horizontally clipped !
; 11:16:34 on 18/02/88 Shallow water patch
; 13:15:01 on 19/02/88 Water at the side of the road
; 14:06:30 on 22/02/88 Water bugs.
; 14:06:41 on 22/02/88 Left wall upper triangle drawn when it shouldn't be
; 14:07:04 on 22/02/88 Right wall stack-tidyness problems fixed
; 21:32:26 on 01/03/88 Front end and tracks/track selection
; 21:32:46 on 01/03/88 Buggyboy logo (1st validation)
; 21:32:53 on 01/03/88 Timegates and 100/250/500 points gates with poles
; 20:10:13 on 02/03/88 Big start banner (with poles)
; 20:10:23 on 02/03/88 Buggy motion wobble
; 20:10:33 on 02/03/88 Tilted buggies
; 15:39:01 on 03/03/88 Initiated collision detection
; 23:45:40 on 03/03/88 Flags flutter
; 11:55:04 on 04/03/88 Action server system laid down
; 11:55:27 on 04/03/88 Action Crash
; 11:55:33 on 04/03/88 Action Tilt
; 11:36:48 on 04/03/88 Friction
; 15:11:08 on 04/03/88 Touching ups on cosmetics on various routines
; 15:12:09 on 04/03/88 Black tunnels
; 15:12:27 on 07/03/88 Scoring system
; 15:13:40 on 08/03/88 Panel on screen
; 16:35:15 on 08/03/88 jumping initiated
; 10:30:27 on 11/03/88 Background mountains
; 10:30:37 on 11/03/88 Explosions
;
; How the labels in this source relate to the reference names in the
; documentation:
;
; Source Label	Reference	Comments
; -------------- ---------------------	-------------------------------------
; START		BB_KR_#1	Pre-natal initialisation
; AUTOMOTION	BB_DC_#3	Move buggy according to road bends
; CENTRE		BB_DC_#4	Calculate new track centre
; DELAY1MS		BB_KR_#9	Domestic delay 1 millisecond (spectrum)
; MOVESCR		BB_KR_#11	Generic Block Move/Copy
; SPRITEDRAW	BB_KR_#6	Linear monochrome Bit-blit
; TEXT		BB_KR_#7	Character string printing
; VDU		BB_KR_#	Character print
; KEYBOARD		BB_KR_#	Keyboard input/action
; MAINLOOP		BB_DC_#	Main wraparound loop
; SELECT		BB_KR_#	Selection screens
; JOYSTICK		BB_KR_#	Domestic input routines
;
; Notes on description of parameters and variables:-
;
; Descr. Means
; ------ -------------------------
;  (N)    8-bit signed integer
;  (P)    8-bit unsigned integer
;  (I)   16-bit signed integer
;  (U)   16-bit unsigned integer
;  (Y)   32-bit signed integer
;  (X)   32-bit unsigned integer
;  (G)   Global variable
;  (L)   Local variable
;  (M)   Metavariable
;
; - as used in parameter passing description at the start of a routine or sub-
; routine, or in the variables section.
;
; The documentation distinguishes between a parameter being an actual pointer
; or an actual scalar value passed. An equals sign (=) is used to denote scalar
; values, a pointer (->) is used to denote a pointer.
;
; Any routine can on entry assume contents of local variables as garbage
; and use them as scratch pads. Note that the variables and lists of
; variables are in a fixed order, so new amendments should be added to
; the end of the list. Note that a 'local variable' is a generic term for
; a variable with no global meaning. Thus it has nothing to do with local
; labels.
;
; Any local LABEL in three digits (eg. $100) denotes self-modifying code
; on that address (or that + the following). This is always the case.
;
; Metasyntactic labels and variables are not used due to the ambiguosity
; regarding readability.
;
;
; List of macros (in alphabetical order, wow):
;
; Macro	Parameters	Description
; -------- ---------------------------	------------------------------------------------------- 
; BOUNDARY	<size>    	This aligns the PC to next (PC/size)=0
; DEF	<label>,(<initial>)	Define labels sequentially
; DTST	<reg>	    	Decrements a 16-bit register and check zero
; HILL	<height> 	This creates data for hills, specify signed height
; JOYSTICK UP/DOWN/LEFT/RIGHT/FIRE	Reads i/p control ports
; MOUNT	<1-4>,<degree>	Creates background hills and things
; OBJCT	<object>,<prim>,<offset>	Creates objects
; RCHAR	<no>,V/H,U/D/L/R	Creates road image
; RDBLK	<no>,<speed>,<sign>	Creates road blocks
; STRNG	<word>,<byte> 	Defines text in one line, adds 0 to end
;
; ------ Don't you just love cryptic macros?
BOUNDARY	MACRO	SIZE
$0	EQU	$
$1	EQU	($/SIZE)*SIZE	; last match of boundary
	IF	$1=$
	ORG	$1	; in case current PC already matches
	ELSE
	ORG	$1+SIZE	; else do alignment
	ENDIF
	ENDM

HILL	MACRO		; HEIGHT is the desired height
CLIMB	DEFL	0	; current climb
	IF	@1<8000H	; in case it's a positive climb
	DO	@1	; accelerate climb @1 times
CLIMB	DEFL	CLIMB+1	; do acceleration
	DB	(CLIMB & 0FFH)	; insert that byte
	LOOP		; & repeat
	DO	@1	; prepare to go down again
CLIMB	DEFL	CLIMB-1
	DB	(CLIMB & 0FFH)	
	LOOP		
	ELSE		
	DO	(-@1)	
CLIMB	DEFL	CLIMB-1	
	DB	(CLIMB & 0FFH)	
	LOOP
	DO	(-@1)
CLIMB	DEFL	CLIMB+1
	DB	(CLIMB & 0FFH)
	LOOP		
	ENDIF
 	ENDM

DTST	MACRO
	IFS	[@1] [BC]
	DEC	BC
	LD	A,B
	OR	C
	ENDIF
	IFS	[@1] [DE]
	DEC	DE
	LD	A,D
	OR	E
	ENDIF
	IFS	[@1] [HL]
	DEC	HL
	LD	A,H
	OR	L
	ENDIF
	ENDM

OBJCT	MACRO	TYPE,PRIM,SIGN
	DW	PRIM
	DB	SIGN,TYPE
	ENDM

RDBLK	MACRO	RDB,SPD,SGN
	DB	(SGN*128)+(SPD*64)+(RDB)
	ENDM

CHAR	MACRO
MC1	DEFL	0	; direction
MC2	DEFL	0	; v/h swap
	IFS	[@3] [L]
MC1	DEFL	0
	ENDIF
	IFS	[@3] [R]
MC1	DEFL	40H
	ENDIF
	IFS	[@3] [U]
MC1	DEFL	80H
	ENDIF
	IFS	[@3] [D]
MC1	DEFL	0C0H
	ENDIF
	IFS	[@2] [V]
MC2	DEFL	10H
	ENDIF
	IFS	[@2] [H]
MC2	DEFL	20H
	ENDIF
	IFS	[@2] [VH]
MC2	DEFL	30H
	ENDIF
	DB	@1!MC1!MC2	; define the byte
	ENDM

JOYSTICK	MACRO
	LD	BC,(CTRL_@1)	; port
	IN	A,(C)
	LD	C,A
	LD	A,(2+CTRL_@1)	; key
	AND	C	; gives Z if key pressed
	ENDM

DEF	MACRO
	IFS	[@2] []
INITVALUE	DEFL	INITVALUE+1	; add 1 if 
	ELSE
INITVALUE	DEFL	@2
 	ENDIF
@1	EQU	INITVALUE
	ENDM

STRNG	MACRO
	DW	@1
	DB	@2,@3
	ENDM

MOUNT	MACRO
	DB	(@1*64)+@2
	ENDM

; ------ Spectrum colour codes and combinations
PAPER	EQU	8
I_WHITE	EQU	7
I_YELLOW	EQU	6
I_CYAN	EQU	5
I_GREEN	EQU	4
I_MAGENTA	EQU	3
I_RED	EQU	2
I_BLUE	EQU	1
I_BLACK	EQU	0
P_WHITE	EQU	7*PAPER
P_YELLOW	EQU	6*PAPER
P_CYAN	EQU	5*PAPER
P_GREEN	EQU	4*PAPER
P_MAGENTA	EQU	3*PAPER
P_RED	EQU	2*PAPER
P_BLUE	EQU	1*PAPER
P_BLACK	EQU	0*PAPER
BRIGHT	EQU	40H
FLASH	EQU	80H

; ------ Define actual physical (foreground) colours
BLACK	EQU	I_BLACK
BLUE	EQU	I_BLUE
RED	EQU	I_RED
GREEN	EQU	I_GREEN
MAGENTA	EQU	RED+BLUE
CYAN	EQU	GREEN+BLUE
YELLOW	EQU	GREEN+RED
WHITE	EQU	RED+GREEN+BLUE

; ------ Main equates and definitions of constants
TRUE	EQU	-1	; logical truth
FALSE	EQU	0	; logical false
VERTCH	EQU	TRUE	; vertical change?
HORIZCH	EQU	TRUE	; horizontal change?
LANES	EQU	3	; 1/3 lanes
PDSREENTRY	EQU	7800H+16H	; monitor re-entry point
WIDTH	EQU	15	; width of track
WIDTH2	EQU	WIDTH*32/3
MSB	EQU	1
LSB	EQU	0	; for neat source code
VSPEED	EQU	3
HSPEED	EQU	15
UPSIDEDOWN	EQU	FALSE	; set if screen is to be reversed
APPEARANCE	EQU	14	; primitive on which to make objects appear
MIDSCREEN	EQU	128	; middle of screen ( > to nearest pixel)
BUGCOLOUR	EQU	TRUE
BUGBRITE	EQU	TRUE
BUGGYON	EQU	TRUE	; don't draw buggy
COLOUR	EQU	YELLOW
EOX	EQU	TRUE	; end-of-text
MORE_TXT	EQU	0FFH
END_TXT	EQU	0
THICKTEXT	EQU	FALSE	; set for extra thick text
IXFLAG   	EQU 	0
IXDELAY  	EQU 	1
IXFACTOR 	EQU 	2
IXOFFSET 	EQU 	3
BUGGIES	EQU	6	; 6 different buggy images to reflect
BIGHILL	EQU	0
MEDIUMHILL	EQU	2
SMALLHILL	EQU	1
CLOUD	EQU	3

; ------ The track numbers
OFFROAD	EQU	1
NORTH	EQU	2
WEST	EQU	3
EAST	EQU	4
SOUTH	EQU	5

; ------ Screen related equates
DF_REAL	EQU	04000H
DF_PSEUDO	EQU	0F000H
AT_LENGTH	EQU	00300H
DF_LENGTH	EQU	01800H
KEYB_IO	EQU	0FEH
BORD_IO	EQU	0FEH

; ------ Named graphic pieces, "logical" dynamic objects
	DEF	PALMTREE,1
	DEF	BEECHTREE
	DEF	PINETREE
	DEF	LOG
	DEF	BOULDER
	DEF	FENCE
	DEF	TIMEGATE
	DEF	PTS100
	DEF	PTS250
	DEF	PTS500
	DEF	FLAG1
	DEF	FLAG2
	DEF	ROCK
	DEF	BARREL
	DEF	SHRUBBERY
	DEF	BRICKWALL
	DEF	CRASHOBJECT
	DEF	SPLASH
	DEF	HAND1
	DEF	HAND2
	DEF	HAND3
	DEF	GO
	DEF	BANNER

; ------ More object pieces, but these are generic - ie. computed
	DEF	VLINE,20H
	DEF	HLINE
	DEF	SNGLHLINE
	DEF	THICKLINE
	DEF	STOREL
	DEF	STORER
	DEF	PATCHL
	DEF	PATCHR
	DEF	BLACKPL
	DEF	BLACKPR

; ------ These are all through the same control routines
	DEF	WALLL	; left wall, smallest distance possible
	DEF	WALLR	; right wall, smallest distance
	DEF	WALLLX	; left wall, to edge of screen
	DEF	WALLRX	; right wall, to edge of screen
	DEF	TUNNL	; left tunnel segment
	DEF	TUNNR	; right tunnel segment
	DEF	TUNNLX	; left tunnel, to edge of screen
	DEF	TUNNRX	; right tunnel, to edge of screen
	DEF	SLOPEL	; as walls, but for slopes
	DEF	SLOPER
	DEF	SLOPELX
	DEF	SLOPERX
	DEF	HASSELBLAD

; ------ Control object (not mapped, merely controls special events)
	DEF	MAPSEGMENT	; new segment on map display
	DEF	GOBLACK
	DEF	GOYELLOW

; ------ Start at 8000, Run code from here and grab free memory
	ORG	8000H
	EXEC	$
	SEND	COMPUTER1

; ------ Set up hardware stack pointer, interrupt vector and interrupt mode
START	DI
	LD	SP,START
	IM	2
	LD	A,VECTORS/100H
	LD	I,A
	EI

; ------ Start actual game setup
	LD	A,BRIGHT+WHITE
	CALL	CLEARSCR
	CALL	BB_LOGO	; buggyboy logo
	LD	HL,MSG_START
	CALL	TEXT
	XOR	A	; no road maps
	CALL	SELECT	; select option from txt_start
	DEC	A
	JP	Z,GAME
	DEC	A
	JP	Z,0
	DEC	A
	JP	Z,PDSREENTRY	; go back home!
	DEC	A
	JP	Z,DEFKEYS
	DEC	A
	JP	Z,SAVEGAME
	RST	0

	ORG	8080H
INTRPT	JP	INTERRUPT

GAME	LD	A,P_YELLOW
	CALL	CLEARSCR
	CALL	WAITKEY
	LD	HL,5A03H
	LD	BC,1A07H
	LD	E,P_BLUE+I_WHITE
	CALL	BOX
	LD	HL,580DH
	LD	BC,100AH
	LD	E,P_YELLOW+I_BLUE
	CALL	BOX
	LD	HL,582EH
	LD	BC,0E07H
	LD	E,P_YELLOW+I_RED
	CALL	BOX
	LD	HL,400DH
	LD	B,10H
	LD	A,16
	CALL	ROW_OF_A
	LD	HL,480DH
	LD	B,10H
	CALL	ROW_OF_A
	LD	HL,402DH
$1	PUSH	HL
	LD	A,16
	CALL	VDUX
	POP	HL
	PUSH	HL
	LD	A,L
	ADD	0FH
	LD	L,A
	LD	A,16
	CALL	VDUX
	POP	HL
	LD	A,L
	ADD	20H
	LD	L,A
	JR	NC,$1
	LD	HL,MSG_SELECT
	CALL	TEXT
	LD	A,0FFH	; "We want road maps"
	CALL	SELECT
	LD	(TRACK_TYPE),A	; Current track 1-5
	PUSH	AF
	LD	B,30H
	LD	C,1
$2	PUSH	BC
	HALT
	LD	A,B
	AND	3
	JR	NZ,$9
	POP	BC
	INC	C
	INC	C
	PUSH	BC
	LD	HL,580DH
	LD	E,9
$5	LD	D,10H
$4	LD	A,C
	AND	7
	ADD	2
	CP	7
	JR	NZ,$10
	LD	A,1
$10	LD	C,A
	OR	P_YELLOW
	LD	(HL),A
	INC	L
	DEC	D
	JR	NZ,$4
	LD	A,L
	ADD	10H
	LD	L,A
	JR	NC,$6
	INC	H
$6	DEC	E
	JR	NZ,$5
	LD	HL,(TEMPW1)
	LD	B,7
	LD	A,C
	OR	P_YELLOW
$7	LD	(HL),A
	INC	L
	DJNZ	$7
$9	CALL	CHOOSECOLX
	RLCA
	RLCA
	RLCA
	OR	BRIGHT
	LD	E,A
	LD	HL,582EH
	LD	BC,0E07H
	CALL	BOXA
	POP	BC
	DJNZ	$2
	LD	A,P_YELLOW+I_BLUE
	CALL	CLEARSCR
	CALL	TURN2YELLI
	POP	AF
	LD	HL,4000H
	CALL	DRAW_MAP_A	; map on screen
	LD	HL,MSG_PANEL
	CALL	TEXT

; q&d cols!
 LD A,P_YELLOW+BLUE+BRIGHT
 LD HL,5815H
$99 LD (HL),A
 INC L
 LD (HL),A
 INC L
 INC A
 CP YELLOW+P_YELLOW+BRIGHT
 JR NZ,$99

; ------ Pre-natal Initialisation
	LD	BC,CLR_END-CLR_START-1
	LD	HL,CLR_START
	LD	DE,CLR_START+1
	LD	(HL),0
	LDIR		; clear them all

; ------ Verious pointers to be set up
	LD	HL,OBJTABLE
	LD	( OBJ_PTR ),HL	; nice spaced-out indirect mode
	LD	HL,SEGMENTS
	LD	( SEG_PTR ),HL
	LD	HL,TRACK
	LD	( TRACK_PTR ),HL
	LD	HL,HORIZTBL
	LD	( HORIZ_PTR ),HL
	LD	HL,SEGMENTS
	LD	( SEG_PTR ),HL
	LD	HL,FRACTIONS
	LD	( BUGGYFRAC ),HL
	LD	HL,4080H
	LD	( TEMPW1 ),HL
	LD	A,100
	LD	( HORIZON ),A	; reference horizon
	LD	A,#60
	LD	( TIMER+1 ),A
	LD	A,1
	CALL	SETSEED	; Random seed (<>0)
	CALL	NEWSEGMENT	; set up the first couple of primitives

; ------ Main loop
MAINLOOP	CALL	KEYBOARD

	LD	BC,(BUGGY)	; use BUGGY as ref.
	CALL	CENTRE	; make up the centre of the track offsets
	CALL	NEW_ROW1	; two x two rows of road

	LD	A,(TRACKFRAC)
	CALL	FRACTBASE	; work out new fraction base table
	LD	(FRACT_PTR),HL

	LD	IX,P0XLIST
	CALL	CALC_FRAC	; draw the lanes
	LD	IX,P2XLIST
	CALL	CALC_FRAC

	CALL	PARAMOUNT	; background mountains first, then
	CALL	OBJECTS	; print the sprites, next
	CALL	STATISTICS	; score, time, speed etc.
	CALL	COLLISION	; collision detection
	CALL	FLASHCMAP	; flash colourmap

	HALT		; synchronize
	LD	A,P_YELLOW
	CALL	CLEARATTR	; clear attribute area

	IF	BUGGYON
	 CALL	DRAWBUGGY	; Draw the main buggy... or
	ELSE
	 CALL	DELAY10MS	; wait appropriately
	ENDIF

	CALL	MOVESCR	; move all down
	CALL	FAST_CLS	; and clear the upper again
	CALL	UPDATE	; all delays etc

	JP	MAINLOOP

; ------ Update all flags and delay counters
UPDATE	LD	HL,FRAME	; every game loop
	INC	(HL)
	LD	HL,CMAPDELAY	; countdown colourmapdelay
	DEC	(HL)
	JP	P,$1
	LD	(HL),0
$1	LD	HL,TILTDELAY
	DEC	(HL)	; just decrement delay
	JP	P,$2
	LD	(HL),0
$2	LD	HL,CRASHFLAG
	LD	A,(HL)
	OR	A
	JR	Z,$3
	DEC	(HL)
	BIT	0,(HL)
	JR	Z,$3
	LD	HL,SPEED
	LD	A,(HL)
	SRL	(HL)
$3	LD	A,0FEH
	IN	A,(0FEH)
	RRA
	JR	C,$4
	CALL	WAITKEY
	LD	BC,00FEH
	CALL	WAIT4KEY
$4	RET

; ------ I/P handler for keyboard/joystick + actions
KEYBOARD	CALL	AUTOMOTION
	CALL	COSMETICS

; ------ Actual speed of buggy
	LD	A,(JUMPFLAG)
	OR	A
	JR	NZ,KEYB13	; not in the air
	LD	A,(CRASHFLAG)
	OR	A
	JR	NZ,KEYB13
	LD	HL,SPEED
	JOYSTICK DOWN
	JR	NZ,KEYB11
	REPEAT 3
	DEC	(HL)
KEYB11	JOYSTICK UP
	JR	NZ,KEYB13
	REPEAT VSPEED
	INC	(HL)

; ------ Left/right steering/velocity
KEYB13	LD	A,(BUGSTATE)
	NEG
	ADD	A
	ADD	A
	ADD	A
	ADD	A
	LD	L,A
	LD	H,0
	JP	P,KEYB17
	DEC	H
	NEG
KEYB17	LD	B,A
	LD	A,(SPEED)
	OR	A
	CCF
	ADC	0
	CP	B	; is speed more than vertical speed
	JP	NC,KEYB18
	RL	L
	JP	NC,KEYB19
	NEG
KEYB19	LD	L,A
KEYB18	LD	A,(JUMPFLAG)	; no steering in the air!
	OR	A
	JR	NZ,KEYB12
	LD	DE,(BUGGY)
	ADD	HL,DE
	LD	(BUGGY),HL

; ------ Change image if tilting
KEYB12	LD	A,(TILT)
	OR	A	; any tilting?
	JR	Z,KEYB14
	LD	E,A	; E holds tilt image
	LD	A,(TILTDES)
	LD	(TILT),A	; new 'destination' image
	JR	KEYB15

; ------ Select steering
KEYB14	LD	E,A
KEYB15	LD	D,0	; assume straight image
	JOYSTICK LEFT
	JR	NZ,KEYB3
	DEC	D
KEYB3	JOYSTICK RIGHT
	JR	NZ,KEYB5
	INC	D
KEYB5	JOYSTICK FIRE
	JR	NZ,KEYB4
	RL	D

; ------ Update buggy's vertical position and speed
KEYB4	LD	A,(CRASHFLAG)
	OR	A
	JR	Z,KEYB42
	RRA
	LD	A,6
	JR	C,KEYB41
	NEG
KEYB41	LD	E,A	; E is frame 5 @ either mirror state
KEYB42	LD	A,(BUGSTATE)
	CP	D	; how does the current picture relate
	JP	Z,KEYB33	; to this 'desired' image
	JP	P,KEYB22
	INC	A
	JP	KEYB33
KEYB22	DEC	A
KEYB33	LD	(BUGSTATE),A	
	LD	(BUGFRAME),A
	LD	B,A
	LD	A,E	; zero if no tilt
	OR	A
	JR	Z,KEYB34
	LD	(BUGFRAME),A
	LD	B,A

; ------ Possibly reflect certain images
KEYB34	LD	A,B
	OR	A
	JP	Z,KEYB21
	JP	P,KEYB20
	NEG
KEYB20	LD	L,A
	LD	H,0
	LD	DE,REFLAGS
	ADD	HL,DE
	LD	A,(HL)
	EX	DE,HL
	XOR	B
	JP	P,KEYB21	; no reflection needed - it's already there
	LD	A,B
	OR	A
	JP	P,KEYB24
	NEG
KEYB24	LD	B,A
	ADD	A
	ADD	B	; *3
	LD	L,A
	LD	H,0
	LD	BC,BB_DIMS
	ADD	HL,BC
	LD	C,(HL)
	INC	HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	LD	A,(DE)
	CPL
	LD	(DE),A	; reflected
	CALL	REFLECT

KEYB21	LD	HL,(BUGGY)
	LD	A,H
	DEC	A
	JP	Z,RIGHTEDGE
	CP	0FEH
	JP	NC,INSIDEEDGE
	LD	HL,0FF00H
	JP	INSIDEEDGE
RIGHTEDGE	LD	HL,000FFH
INSIDEEDGE	LD	(BUGGY),HL

; ------ Add fraction to position
	LD	HL,TRACKFRAC
	LD	A,(SPEED)
	ADD	(HL)
	LD	(HL),A
	RET	NC	; still same primitive

; ------ Fraction wraparound to be dealt with here
	LD	BC,#0001	; that's worth 1 point
	CALL	ADDSCORE
	LD	HL,(TRACK_PTR)
	INC	L
	LD	A,(HL)
	LD	(TRACK_PTR),HL
	LD	HL,(MILEAGE)
	INC	HL
	LD	(MILEAGE),HL

; ------ Store values to move buggy
	OR	A
	LD	B,0
	JP	P,KEYB43
	NEG	
	DEC	B	; modify code
KEYB43	LD	L,A
	LD	A,B
	LD	(POLARISAT),A
	XOR	A
	LD	(LASTBFRAC),A	; reset this
	LD	H,A
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	LD	DE,FRACTIONS
	ADD	HL,DE
	LD	(BUGGYFRAC),HL	; points to fractions for buggy motion

; ------ Check for possible new points to be put on
	LD	HL,PRIMSLEFT
	DEC	(HL)
	LD	A,(HL)
	CP	10H
	CALL	C,NEWSEGMENT	; if it's time for new things to come
	LD	HL,(HORIZ_PTR)
	INC	HL
	LD	(HORIZ_PTR),HL
	LD	DE,HORIZTBN
	AND	A
	SBC	HL,DE
	RET	C
	LD	HL,HORIZTBL
	LD	(HORIZ_PTR),HL
	RET

; ------ Control code for jumping buggies
COSMETICS	LD	IX,JUMPFLAG
	LD	HL,TILTDELAY
	LD	A,(IX+IXFLAG)
	OR	A
	JR	Z,KEYB54	; no jumping!
	DEC	(IX+IXDELAY)
	JP	P,KEYB56	; don't 
	LD	(IX+IXDELAY),1
	DEC	(IX+IXFACTOR)
	JR	NZ,KEYB56
	LD	(IX+IXDELAY),2
KEYB56	LD	A,(IX+IXFACTOR)
	ADD	(IX+IXOFFSET)
	LD	(IX+IXOFFSET),A	; offset
	JR	NZ,KEYB57
	LD	(IX+IXFLAG),A	; no jump, zero the flag
KEYB57	LD	(HL),0
	JR	KEYB58

; ------ Contol code for tilting buggy
KEYB54	LD	A,(HL)
	OR	A
	JR	NZ,KEYB50	; wait...
	LD	(HL),3
KEYB58	LD	A,(TILT)
	LD	B,A
	OR	A
	JR	Z,KEYB50	; No tilting anyway
	JP	P,KEYB51	; Tilting left
	NEG
KEYB51	DEC	A
	CP	3
	JR	NZ,KEYB52
	XOR	A
KEYB52	RL	B
	JR	NC,KEYB53
	NEG
KEYB53	LD	(TILTDES),A
KEYB50	RET

; ------ This handles the vertical motion of the buggy according to the bends of the road
AUTOMOTION	LD	HL,(BUGGYFRAC)	; pointer to buggyfractions
	LD	A,(TRACKFRAC)
	AND	0F8H
	RRCA
	RRCA
	RRCA
	ADD	L
	LD	L,A
	LD	A,(HL)	; absolute fraction value
	ADD	A	; a bit more vertical speed
	LD	C,A
	LD	HL,LASTBFRAC
	SUB	(HL)	; minus last fraction
	LD	(HL),C	; now this is the last fraction
	LD	HL,(TRACK_PTR)	; pointer to the "target" so to speak
	LD	B,0
	LD	C,A	; C is the automatic offset (-32<x<32)
	OR	A
	RET	Z	; no change
	LD	HL,(BUGGY)	; now do actual offset
	LD	A,(POLARISAT)
	OR	A
	LD	A,(MOUNTVALUE)
	JR	Z,$0	; jump if add
	SBC	HL,BC
	DEC	A
	JR	$1
$0	ADD	HL,BC
	INC	A
$1	LD	(BUGGY),HL
	LD	(MOUNTVALUE),A
	RET

; ------ Display statistics
STATISTICS	LD	A,(SPEED)
	CALL	BINTODEC
	LD	(TEMPW1),HL
	LD	HL,TEMPW1+1
	LD	DE,40EFH
	LD	A,2
	CALL	DECIMAL
	LD	HL,SCORE+2
	LD	DE,4099H
	LD	A,3
	CALL	DECIMAL
	LD	HL,TIMEFLAG
	BIT	7,(HL)
	JR	Z,$0
	XOR	A
	LD	(HL),A
	LD	DE,4031H
	DEC	HL
	OR	(HL)
	JP	Z,START
	LD	A,1
	CALL	DECIMAL
$0

	RET

; ------ This will introduce and initialise at least 16 new primitives
NEWSEGMENT	LD	HL,(SEG_PTR)
$0	LD	B,(HL)	; get new type of segment
	INC	HL
	LD	A,B
	AND	1FH
	JP	NZ,$1	; last (this is actually when the
	LD	HL,OBJTABLE
	LD	(OBJ_PTR),HL
	LD	HL,1
	LD	(MILEAGE),HL
	LD	HL,SEGMENTS	; course is complete...)
	JP	$0
$1	LD	(SEG_PTR),HL
	ADD	A,A
	LD	L,A
	LD	H,0
	LD	DE,ROADBLOCKS-2	; this is (yeuch) the offset
	ADD	HL,DE	; HL now points to nibble data
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A	; get address from table
	LD	A,B
	RLCA
	RLCA
	AND	3
	LD	B,A	; speed in bit 0, sign in bit 1

; ------ Now find the point beyond the horizon where the points should go
	LD	DE,(TRACK_PTR)
	LD	A,(PRIMSLEFT)
	ADD	E
	LD	E,A

; ------ Start inserting the nibbles into the track
$3	LD	A,(HL)	;	Track data
	RRCA
	RRCA
	RRCA
	RRCA		; isolate high nibble
	AND	0FH
	CP	0FH	; is it The End?
	JP	Z,$9
	BIT	1,B	; is it negative?
	JP	Z,$6
	NEG
$6	LD	(DE),A
	INC	E
	BIT	1,B
	JP	Z,$7
	LD	(DE),A	; do it twice if "speed permits" (!)
	INC	E
$7	LD	A,(HL)	; isolate low nibble
	AND	0FH
	CP	0FH
	JP	Z,$9
	BIT	1,B	;	again consider negation
	JP	Z,$2
	NEG
$2	LD	(DE),A
	INC	E
	BIT	0,B
	JP	Z,$8
	LD	(DE),A	; again do it twice if necessary
	INC	E
$8	INC	HL	; next position in nibble table
	JP	$3	

; ------ Now update the primitives left counter (work out how much was put in)
$9	LD	A,(TRACK_PTR)	;	the original start
	SUB	E	; the 'new' start + 'old' primsleft
	NEG		; make it positive
	LD	(PRIMSLEFT),A	; new counter

; ------ Make sure that all points on the screen are valid
	CP	10H	; Do all this again if there are less
	JP	C,NEWSEGMENT	; than 16 primitives on the screen!
	RET

; ------ This will update OBJ_PRT after a predecessive loop
FINDHORIZ	LD	DE,(MILEAGE)
	LD	IX,(OBJ_PTR)
	LD	C,0	; flag "No objects in sight"
$1	LD	L,(IX+0)
	LD	H,(IX+1)
	AND	A
	SBC	HL,DE
	JP	NC,$2
	LD	C,0FFH
	LD	L,0
$2	LD	A,L
	CP	APPEARANCE+1
	RET	NC	; back now if behind horizon
	INC	C	; increase no. of things on the track
	INC	IX
	INC	IX
	INC	IX	; this will always return IX pointing
	INC	IX	; to the NEXT object!
	JP	$1

; Module name: Draw all objects on the track
;
; Entry parameters: 
;	none
;
; Exit parameters: 
;	none
;
; Notes: This will provide service routines (eg. SPRITEDRAW, VERTLINE) with
; relevant information regarding clipping caused by hills.

OBJECTS	XOR	A
	LD	(NOSPRITES),A
	LD	HL,0
	LD	(LASTLXY),HL
	LD	(LASTRXY),HL
	LD	(LASTDFL),HL
	LD	(LASTDFR),HL
	CALL	FINDHORIZ
	LD	A,C
	OR	A
	JP	Z,OBJECTS2	; go back now if track is empty!

; ------ Carry out the main loop for printing things
OBJECTS1	PUSH	BC
	LD	HL,NOSPRITES	; 1 more object on screen
	INC	(HL)
	LD	DE,(MILEAGE)
	LD	L,(IX-4)	; this is the abs. primitive value
	LD	H,(IX-3)
	AND	A
	SBC	HL,DE	; form relative distance in HL
	PUSH	HL	; save relative distance to primitive
	LD	A,L
	LD	(PRIMITIVE),A	; store primitive here for reference
	LD	E,L	; L is actually the primitive here
	LD	H,0	; form address from angle
	LD	BC,YCOORDS	; offset in YCOORDS
	ADD	HL,HL	; two bytes per entry
	ADD	HL,BC	; index into the table
	LD	A,(HL)	; this is the actual starting coord.
	LD	B,A
	SUB	20H
	LD	C,A
	INC	HL	; forward to second byte
	LD	A,(HL)	; maximum horizon at this point
	SUB	C	; calculate clipping factor
	LD	(VERTCLIP),A	; this is an ABSOLUTE SCREEN value !!
	INC	HL
	LD	A,(HL)
	SUB	B
	JR	NC,$6
	XOR	A
$6	LD	(NEXTY),A	; difference between last and current
	LD	B,0
	LD	A,(IX-2)	; this is the X-offset
	PUSH	BC	; save Y-coordinate
	PUSH	IX
	CALL	XCALC	; find 'real' X-offset
	POP	IX
	POP	BC
	LD	DE,MIDSCREEN
	ADD	HL,DE	; now real coordinate
	LD	(XOFFSET),HL
	LD	H,0

; ------ Do some processing regarding the picture to actually draw
	LD	B,L	; grab X coordinate
	LD	A,(IX-1)	; this is the LOGICAL OBJECT no.
	CP	VLINE	; is it not a sprite?
	JP	NC,$5	; do not do sprite processing if so.
	CP	BANNER
	JP	NZ,$8
	LD	A,>POLEPLUS2
	LD	($100),A
	LD	A,>GATEHW2
	LD	($101),A
	LD	A,>GATEHT2
	LD	(DRAWPX),A
	JP	$9
$8	CP	TIMEGATE
	JP	C,$4
	CP	PTS500+1
	JP	NC,$4
	LD	A,>POLEPLUS
	LD	($100),A
	LD	A,>GATEHWIDTH
	LD	($101),A
	LD	A,>GATEHEIGHT
	LD	(DRAWPX),A
$9	LD	A,(PRIMITIVE)
	LD	E,A
	PUSH	BC
	LD	A,7	; height of gate's poles
	CALL	QUICKXCALC
	POP	BC

	LD	E,A
	ADD	C	; new Y coordinate
	LD	C,A

	LD	A,(VERTCLIP)
	SUB	E
	JR	Z,$2
	JR	NC,$3
$2	LD	A,1
$3	LD	(TEMPB2),A	; "new" vertclip for the banner

	LD	A,(PRIMITIVE)
	SRL	A
	LD	D,A
	LD	H,<POLEPLUS	; change if bigger pole
	ADD	>POLEPLUS
$100	EQU	$-1
	LD	L,A
	LD	A,(HL)	; height of pole

	ADD	E	; add gate's height

	LD	HL,VERTCLIP
	SUB	(HL)
	JP	Z,$70
	JP	C,$70

	LD	E,A

	PUSH	BC	; save X&Y
	LD	A,E	; height of pole
	LD	(TEMPB1),A
	LD	H,<GATEHWIDTH
	LD	A,D
	LD	DE,(XOFFSET)
	ADD	>GATEHWIDTH
$101	EQU	$-1
	LD	L,A
	LD	A,B
	ADD	(HL)
	LD	B,A
	JR	NC,$71
	INC	D
$71	LD	A,D
	OR	A
	PUSH	HL
	CALL	Z,DRAWPOLES	; this is the RIGHT pole
	POP	HL
$72	POP	BC
	PUSH	BC
	LD	DE,(XOFFSET)
	LD	A,B
	SUB	(HL)
	LD	B,A
	JR	NC,$73
	DEC	D
$73	LD	A,D
	OR	A
	CALL	Z,DRAWPOLES
	POP	BC
$70	LD	A,(TEMPB2)
	INC	A
	LD	(VERTCLIP),A

$4	LD	A,(IX-1)
	CP	FLAG1
	JR	NZ,$20
	LD	HL,FRAME
	BIT	0,(HL)
	JR	Z,$20
	INC	A
$20	LD	L,A
	LD	H,0	; else form a 16-bit address offset
	LD	DE,GRAPHINDEX-1
	ADD	HL,DE	; pointer to translated image
	LD	A,(HL)	; this is the actual BB_GD_#nn picture frame
	POP	HL	; L is primitive
	RES	0,L	; 2 frames/primitive but 2 bytes/entry so div 2, mul 2
	LD	DE,IMAGETABLE	; holds offset for the three stored
	ADD	HL,DE	; images
	ADD	(HL)	; calculate REAL image
	LD	D,A	; keep in D
	INC	HL	; this is the magnification S,M,L
	LD	E,(HL)	; E is 0-2.

; ------ Draw the bitmapped object on the pseudoscreen
	PUSH	IX	; this is used in drawing sprites
	LD	A,(PRIMITIVE)
	OR	A
	CALL	NZ,SPRITEDRAW
	POP	IX
	JP	OBJECTS4

; ------ Enter here if we are dealing with a 'special' object
$5	LD	(SPECIAL),A	; store 'special' object here
	POP	DE
	PUSH	DE

	CP	VLINE
	JR	Z,$25
	CP	WALLL	; all specials below wall value must
	JP	NC,$25	; be referred to as bottom primitive

$27	LD	A,255
	PUSH	AF
	JP	$26

$25	LD	A,10	; height of 10
	PUSH	BC
	CALL	QUICKXCALC	; find the real X coordinate = height
	POP	BC
	PUSH	AF	; computed height of object
	ADD	C	; change Y ... a bit
	LD	C,A

$26	LD	(TEMPCOORDS),BC	; store the coordinates here temporarily
	CALL	PLOT
	LD	B,A	; B is pixel position 0-7
	LD	A,(VERTCLIP)
	LD	E,A
	POP	AF	; height back again
	SUB	E	; vertically clip sprite from horizon	
	POP	DE	; E is primitive
	JP	C,OBJECTS4	; don't draw if it's completely below
	JP	Z,OBJECTS4	; the horizon (or tangents it)
	LD	C,A	; C is height
	LD	D,E	; let there be primitive in D
	LD	A,0FFH	; Assume black wall-pattern
	LD	(WALPATTERN),A

	EXX
	LD	DE,OBJECTS4
	LD	A,0	; this is the 'special' object drawn
SPECIAL	EQU	$-1
	PUSH	DE
	SUB	VLINE
	ADD	A
	LD	L,A
	LD	H,0
	LD	DE,JUMPTABLE	; special cases stored here.
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	PUSH	DE	; pointer to service routine
	EXX
 	RET		; go to appropriate routine

OBJECTS3	POP	HL
OBJECTS4	DEC	IX	; to point to the previous one
	DEC	IX
	DEC	IX
	DEC	IX
	POP	BC
	DEC	C
	JP	NZ,OBJECTS1
OBJECTS2	LD	(OBJ_PTR),IX
	RET

DRAWPOLES	CALL	PLOT
	LD	B,A
	LD	D,<GATEHEIGHT
	LD	A,(PRIMITIVE)
	AND	0FEH
	ADD	>GATEHEIGHT
DRAWPX	EQU	$-1
	LD	E,A
	EX	DE,HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	EX	DE,HL
	ADD	HL,DE
	LD	A,(TEMPB1)
	LD	C,A
	LD	A,(PRIMITIVE)
	LD	E,A
	JP	VERTLINEX

NEW_ROW1	LD	IX,P0XLIST	; do primitives for set one
	CALL	FAST_ROW	; into 'list 0'

NEW_ROW2	LD	HL,(TRACK_PTR)
	INC	L
	LD	(TRACK_PTR),HL
	LD	HL,(HORIZ_PTR)
	INC	HL
	LD	(HORIZ_PTR),HL
	LD	HL,128
	LD	(MIDDLE),HL
	LD	IX,P1XLIST	; do primitives for set two
	CALL	FAST_ROW	; into 'list 1'
	LD	HL,(TRACK_PTR)
	DEC	L
	LD	(TRACK_PTR),HL
	LD	HL,(HORIZ_PTR)
	DEC	HL
	LD	(HORIZ_PTR),HL
	RET

; ------ Set a 'long' pixel on the screen, coordinated by B,C as X,Y.
PIXEL	ADD	A,A
	LD	E,A
	LD	D,0
	LD	HL,WIDTHS
	ADD	HL,DE
	LD	D,(HL)
	INC	HL
	LD	E,(HL)
	PUSH	DE

	CALL	PLOT	; find address of coordinated raster

	POP	DE
	LD	B,0
	JP	Z,$2
$1	RR	E
	RR	D
	RR	B
	DEC	A
	JP	P,$1
$2
	LD	A,E
	OR	(HL)
	LD	(HL),A
	LD	A,D
	INC	L
	OR	(HL)
	LD	(HL),A
	INC	L
	LD	A,B
	OR	(HL)
	LD	(HL),A
	DEC	L
	DEC	L
	EX	DE,HL
	RET

; ------ Calculate base of fraction table for desired fraction in A
FRACTBASE	AND	11111000B	; A is fraction 0-255, nov mod. -1(8)
	LD	H,0
	LD	L,A
	ADD	HL,HL
	ADD	HL,HL	; and 4, giving 32
	LD	A,FRACTIONS/100H
	ADD	H	; add to base of fractions (MSB)
	LD	H,A
	RET		; HL now base of fraction table for A

; Module name: Calculation of fractions between two already calculated rows of
;		primitive points, both X and Y.
;
; Entry parameters: 
;	none
;
; Exit parameters: 
;	none whatsoever
;

CALC_FRAC	LD	HL,YCOORDS	; find Y coords here
	LD	(TEMPW1),HL
	LD	IY,P0YLIST	; this is where original coords are
	EXX
	LD	HL,(FRACT_PTR)	; hold pointer to fractions in HL'
	LD	E,L	; and keep original L here
	LD	D,0	; top Y coordinate
	EXX
	LD	B,32	; 32 points in all to find
	JP	$10

; ------ Now start massive loop for all 32 points on the track
$0	LD	A,B
	CP	10H	; if halfway through,
	JP	NZ,$11
	EXX
	LD	D,0	; clear top as well
	EXX
	LD	IY,P0YLIST	; reset pointer to Y values

; ------ This deals with primitive 32 and 16 - they're the very bottom ones
$10	EXX
	LD	B,(IX+0)	; X-coordinate no fraction
	LD	A,(IY+0)	; Y-coordinate no fraction
	JP	$5	

$11	LD	C,0	; flags "No errors in fraction calc"
	LD	A,(IX+4EH)
	SUB	(IX+00H)	; this is the X-offset
	OR	A
	JP	P,$3	; it was positive, jump

; ------ Fraction was negative
	NEG		; otherwise make result positive and
	CP	32
	JP	C,$7	; jump if no good
	LD	C,0FFH
$7	SUB	1
	ADC	0	; subtract 1 if a not zero
	EXX
	ADD	E	; pointer to fractions
	LD	L,A	; make address
	LD	A,(IX+00H)	; get original X
	SUB	(HL)	; and add fraction
	JP	$2	; so there!

; ------ This is for the case of the fraction being positive
$3	CP	32
	JP	C,$8	; illegal distance for sure !
	LD	C,0FFH
$8	EXX
	SUB	1	; sub 1 if a>0 only
	ADC	0
	ADD	E	; pointer to fractions
	LD	L,A
	LD	A,(IX+0)	; X coordinate again
	ADD	(HL)	; add fractional part.

; ------ X coordinate has now been found, arrives here in A
$2	LD	B,A	; resort to B for safe keeping
	EXX
	LD	A,(IY+00H)	; this is the first Y coord
	SUB	(IY+4FH)	; minus where it will go later
	JP	NC,$4	; it was positive
 
; ------ The Y difference was negative
	NEG
	CP	32	; check range again (shouldn't fail)
	JP	C,$9
	LD	C,0FFH
$9	EXX
	SUB	1	; again sub 1 if a is not zero
	ADC	0
	ADD	E	; fraction pointer
	LD	L,A	; now address
	LD	A,(IY+00H)
	ADD	(HL)	; add fraction to original Y
	JP	$5

; ------ In case the difference was positive, come here
$4	CP	32	; this shouldn't really happen !
	JP	C,$6	; - 32 pixels should always be in rge.
	LD	C,0FFH
$6	EXX
	SUB	1	; a = a - (a>0)
	ADC	0
	ADD	E	; fractional part pointer
	LD	L,A	; make address of that
	LD	A,(IY+0H)	; grab 1st Y again
	SUB	(HL)	; minus fractional part

; ------ Everything is wonderful now, and we've got the Y coordinate in A
$5	PUSH	HL
	LD	HL,(TEMPW1)
	LD	(HL),A	; this is the actual coordinate !!
	INC	HL
	LD	(HL),D	; assume horizon on top

	SUB	1EH	; subtract maximum primitive height
	JP	C,$22	; but primitive is actually off-screen

	CP	D	; smaller that the top?
	JP	C,$22
	LD	D,A
	LD	C,A
	LD	(HL),A	; new maximum horizon

	LD	A,(IX+4FH)
	OR	(IX+01H)
	JP	NZ,$22	; outside if any of the hi-bytes are <> 0

	EXX
	INC	C	; was there an error (overflow in particular)?
	LD	A,B	; A is counter
	EXX
	JP	Z,$22	; jump if a fraction error occurred

	PUSH	DE

	DEC	A	; dec. to be 0-31, and to be 0-15
	AND	0FH	; this is now width of primitive point

	CP	15	; if NOT the bottom one,
	CALL	NZ,PIXEL	; set pixel with B=x, C=y

	POP	DE
$22
	POP	HL
	EXX
	LD	HL,(TEMPW1)
	INC	HL
	INC	HL
	LD	(TEMPW1),HL

	INC	IX
	INC	IX
	INC	IY
	DEC	B
	JP	NZ,$0
	RET

; ------ Quickly calculate an X-coordinate/height from an angle/row (A row E angle)
QUICKXCALC	ADD	A
	ADD	A
	ADD	A
	ADD	A	; row number is multiplied by 16
	LD	L,A
	LD	H,0	; form 16-bit node index from that
	LD	D,H
	LD	BC,XPOSNS	; XPOSNS is offset
	ADD	HL,BC	; find row "node" entry
	ADD	HL,DE	; find angle "element" entry within
	LD	DE,(FRACT_PTR)	; base of fraction element table
	LD	A,(HL)	; get first X to mess with
	INC	HL
	SUB	(HL)	; find difference from next X
	ADD	E	; add to base of fraction table
	LD	E,A	; and form new fraction pointer
	LD	A,(DE)	; get fraction
	ADD	(HL)	; and add to the 'distant' X
	RET		; A is now the X offset
;
; Module name: Calculation of the centre line offsets, relative to buggys
;	    position.
;
; Entry parameters: 
;	BC holds current buggy's position (I)
;
; Exit parameters: 
;	none, but OFFSETS are initialised with the offsets (I)
;
; Notes: The 'middle' of the screen is pixel 128, therefore the signed
; X coordinate.

CENTRE	LD	IX,OFFSETS	; store values here for offsets
	LD	E,16
	LD	HL,YREFPOSNS	; base of Y references
$1	LD	A,(HL)	; get a Y reference
	OR	A
	JP	P,$2
	LD	A,127
$2	PUSH	HL
	PUSH	BC
	PUSH	DE
	CALL	MUL16	; bc (signed) times a (unsigned)
	CALL	DIV128S	; Divide result
	LD	(IX+0),L	; Insert that value
	LD	(IX+1),H
	INC	IX
	INC	IX
	POP	DE
	POP	BC
	POP	HL
	INC	HL	; next angle
	DEC	E
	JR	NZ,$1
	RET
;
; Module name: Calculation of y coordinates, 17:02:35 on 07/12/87
;
; Entry parameters: 
;	(HORIZON) holds current horizon
;
; Exit parameters: 
;	none, but YFRAC has been initialised with 16 values, each being
;	a Y position from the bottom upwards according to the horizon.



YCALC	LD	A,(HORIZON)	; current horizon position
	LD	L,A
	LD	H,0
	LD	BC,YPOSNS-50*16	; offset is here minus the missing 50
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,BC
	LD	B,16
	LD	IX,(HORIZ_PTR)
$1	LD	A,(HL)
	LD	(DE),A
	PUSH	DE
	LD	D,0
	LD	A,(IX+0)
	ADD	A
	ADD	A
	ADD	A
	ADD	A
	OR	A
	JP	P,$2
	DEC	D
$2	LD	E,A 
	IF VERTCH			; only if it is required,
	 ADD	HL,DE	; move pointer within table of Y offs.
	ENDIF
	POP	DE
$3	INC	DE
	INC	IX
	INC	HL
	DJNZ	$1
	RET

; ------ Wait for all keys being released
WAITKEY	XOR	A
	IN	A,(KEYB_IO)
	CPL
	AND	1FH
	RET	Z	; ret if no keys pressed
	JR	WAITKEY

; ------ Wait for a key being pressed via BC
WAIT4KEY	IN	A,(C)
	CPL
	AND	1FH
	RET	NZ	; Key pressed is bit set
	JR	WAIT4KEY
;
	SUBTTL "Screen handling
;
; Module name: X,Y to address,pixel mask
;
; Entry parameters: 
;	B is X (0-255) (P) 
;	C is Y (0-127) (P)
;
; Exit parameters: 
;	HL is pseudo display file address
;	A is pixel 0-7 (from LEFT!)
;	z if pixel is 0
;	nc,p
;
PLOT	LD	A,127
	SUB	C
	LD	L,A
	LD	H,0
	REPEAT 5
	ADD	HL,HL
	LD	DE,DF_PSEUDO
	ADD	HL,DE

	LD	A,(JUMPOFFSET)
	ADD	H
	LD	H,A

; ------ Find the vertical byte position
	LD	A,B	; this is X
	RRA
	RRA
	RRA		; minus pixel pos
	AND	1FH	; strip off junk in bit 567
	ADD	L
	LD	L,A	; HL is now real address
	LD	A,B
	AND	7	; A is now pixel position
	RET

CLEARSCR	LD	(BCKCOLOUR),A
	LD	E,A
	LD	D,A
	RRA
	RRA
	RRA
	AND	7
	HALT
	OUT	(BORD_IO),A
	LD	(TEMPSP),SP
	DI
	LD	SP,5B00H
	LD	B,0C0H
$0	PUSH	DE
	PUSH	DE
	DJNZ	$0
	LD	BC,0C00H
	LD	DE,0
$1	PUSH	DE
	PUSH	DE
	DEC	BC
	LD	A,B
	OR	C
	JP	NZ,$1
	LD	SP,(TEMPSP)
	EI

; ------ Fast pseudo-screen clear
FAST_CLS	LD	HL,DF_PSEUDO+1000H
	LD	B,0
FAST_CLS1	DI
	LD	(TEMPSP),SP
	LD	SP,HL
	LD	DE,0
$1	REPEAT 8
	PUSH	DE
	DJNZ	$1
	LD	SP,(TEMPSP)
	EI
	RET

; ------ print A at address HL
HEXTOASC	LD	(DFADDR),HL
HEXTOASCX	PUSH	AF
	RRCA
	RRCA
	RRCA
	RRCA
	CALL	$1
	POP	AF
$1	AND	0FH
	CP	0AH
	JP	C,$2
	ADD	A,7
$2	ADD	30H
	JP	VDU
;
; Module name: Character string printing on main display file
;
; Entry parameters: 
;	HL -> display file (word) and ascii characters, ending in
;		  00: end-of-text
;		  FF: new display-file address following
;
; Exit parameters: 
;	HL -> byte after the end-marker (FF)
;	C  =  no. of chars printed (inclusive the FF markers, but excluding the end-marker)
;	DE =  next display-file address
;	A  =  00
;	F  =  Z,NC,P,PE
;
; Preserves B, IX, IY.

TEXT	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
TEXT_ADR	LD	(DFADDR),DE
	LD	C,0	; characters printed
$1	LD	A,(HL)
	INC	HL
	OR	A
	RET	Z
	CP	255
	JP	Z,TEXT
	PUSH	HL
	PUSH	BC
	CALL	VDU
	POP	BC
	POP	HL
	INC	C
	JR	$1

VDUX	LD	(DFADDR),HL
VDU	LD	L,A
	LD	H,0
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	LD	B,8
	CP	20H
	JR	NC,$10	; which chrset to select
	LD	DE,RDCHRBASE
	ADD	HL,DE
	LD	DE,(DFADDR)
	JR	$13
$10	LD	DE,03C00H
	ADD	HL,DE
	LD	DE,(DFADDR)
$1	LD	A,(HL)
	LD	C,A
	SRL	C
	OR	C

	IF	THICKTEXT
	 LD	C,(HL)
	 SLA	C	
	 OR	C
	ENDIF

	LD	(DE),A
	INC	HL
	INC	D
	DJNZ	$1
	JR	$12
$13	LD	A,(HL)
	LD	(DE),A
	INC	HL
	INC	D
	DJNZ	$13
$12	LD	A,D
	SUB	8
	LD	D,A
	RL	D
	RL	D
	RL	D
	INC	DE
	RR	D
	RR	D
	RR	D
	LD	(DFADDR),DE
	RET

TEXTP	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	(DFADDR),DE
$1	LD	A,(HL)
	INC	HL
	OR	A
	RET	Z
	CP	255
	JP	Z,TEXT
	PUSH	HL
	CALL	VDUP
	POP	HL
	JR	$1

VDUP	LD	L,A
	LD	H,0
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	LD	DE,3C00H
	ADD	HL,DE
	LD	DE,(DFADDR)
	LD	B,8
$1	LD	A,(HL)	
	LD	C,A
	SRL	A
	OR	C
	LD	(DE),A
	LD	A,E
	ADD	20H
	LD	E,A
	INC	HL
	DJNZ	$1
	INC	DE
	LD	(DFADDR),DE
	RET

; Module name: Interleaved bitmap low level graphics primitive for objects
;
; Entry parameters: 
;	D  = physical object number, as listed in OBJECTDATA (P)
;	E  = magnification 0-2 (*.5/*1.0/*2.0) (P)
;	B  = X coordinate (P)
;	C  = Y coordinate (P)
;
; Exit parameters: 
;	as listed in CALCSPRITE since it exits through that routine.
;
; Notes: This takes into consideration the actual pixel widths of the things
; to draw. Consequently, the images are drawn 'from the middle' - ie., the
; X coordinate supplied in B refers to the 'real' middle of the object.

SPRITEDRAW	LD	IX,CALCM1
	LD	A,E
	DEC	A	; NZ=large, M=small, Z=medium
	JP	M,$0
	JP	Z,$1
$2	LD	(IX+0),0
	LD	(IX+2),0
	LD	HL,LARGE
	JP	CALCSPRITE
$1	LD	(IX+0),0
	LD	(IX+2),1FH
	LD	HL,MEDIUM
	JP	CALCSPRITE
$0	LD	(IX+0),1FH
	LD	(IX+2),1FH
	LD	HL,SMALL

; Module name:	Find a physical object's location and dimensions and
;	     Calculate its destination on screen (depending on width)
;
; Entry parameters: 
;	B  = X coordinate (P)
;	C  = Y coordinate (P)
;	D  = logical object to display (P)
;	(VERTCLIP) = vertical clipping factor (if any) (P)
;
; Exit parameters: 
;	B  = width in bytes (P)
;	C  = height in pixel-lines (P)
;	E  = vertical clip factor in pixel rows (P)
;	IX ->address of 1st byte of the graphic data (U)
;	H  ->rotation table MSB (P)
;	L  = Actual X coordinate as a 2's complement number (N)
;	DE'->display-file address (U)
;	BC'= 0020H (I)
;	HL'= distance in memory between bottom and top Y coordinate / 2 (U)
;	carry is set if the object is not visible due to	horizontal clipping

CALCSPRITE	LD	(DIVERT),HL
	PUSH	BC	; save coordinates
	LD	L,D	; HL is now the object in question
	LD	H,0
	ADD	HL,HL	; times 4
	ADD	HL,HL
	LD	DE,OBJECTDATA-1	; index to LAST entry in the node
	ADD	HL,DE
	LD	D,(HL)	; get hi-byte of address
	DEC	HL
	LD	E,(HL)	; get lo-byte of address
	DEC	HL
	LD	C,(HL)	; get height in pixel rows
	DEC	HL
	LD	A,(HL)	; get width in PIXELS
	RRCA
	RRCA
	RRCA		; now in bytes
	LD	B,A
	AND	0E0H	; was there any fraction here
	JP	Z,CALC0	; jump if no fraction
	INC	B	; otherwise round up to next integer
CALC0	LD	A,B	; now get rid of the fraction in bits 567
	AND	1FH	; max width anyway
	LD	B,A
	PUSH	DE
	POP	IX	; IX must point to graphic data
	LD	A,C	; grab height again
	EXX
	LD	HL,WORKSP
	LD	DE,WORKSP+1
	LD	BC,1FH
	LD	(HL),B
	LDIR		; clear worksp table
	LD	L,A
	LD	H,0
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,HL
	EX	DE,HL	; DE is real distance between top&bottom
	LD	BC,20H
	EXX
	LD	A,(HL)	; pixel width again
	AND	A
CALCM1	RRA		; half width
	AND	A
CALCM2	RRA		; .25 width
	LD	HL,(XOFFSET)
	LD	E,A
	LD	D,0
	AND	A
	SBC	HL,DE	; sub to find 'real' width
	LD	(XOFFSET),HL
	POP	DE
	PUSH	BC	; save width
	LD	C,E
	LD	B,L
	CALL	PLOT	; find pixel and displayfile address
	PUSH	HL
	EXX
	POP	HL	; must be HL' which holds df address
	EXX
	SRL	A	; only 4 rotation tables, div. by 2
	ADD	ROTTAB/100H	; find hi-byte of rotation table
	POP	BC
	LD	HL,(XOFFSET)
	SRA	H
	RR	L
	SRA	H
	RR	L
	SRA	H	; signed 16-bit division by 8
	RR	L
	LD	H,A	; make H point to appropriate rottab
	LD	A,(VERTCLIP)
	SUB	2
	LD	E,A	; E is the vertical clip factor
	JP	0
DIVERT	EQU	$-2	; divert to appropriate size sprite drawing routine

; ------ Medium (normal) sized sprites
MEDIUM	LD	A,C
	SUB	E
	RET	Z	; return if it's clipped by a horizon
	RET	C
	LD	C,A
	LD	A,L
	OR	A
	JP	P,$10	; no left-hand clipping
	NEG
	LD	D,A
	LD	A,B
	SUB	D	; is it greater than the width?
	RET	C	; yes, return if it's completely off screen
	LD	B,A	; this is the new width
	LD	A,D	; this is the left clip factor
	LD	($101),A	; add this to the screen address
	LD	($102),A	; add this to the graphic pointer
	XOR	A
	LD	($100),A	; jump to PC+2 down there - ie. don't jump
	JP	$12	; if it's left clipped it won't be right clipped

$10	XOR	A
	LD	($101),A
	LD	A,$99-$98	; jump offset if no left clip
	LD	($100),A	; modify jump down there

	LD	A,L	; right hand
	ADD	B	; edge by adding width
	SUB	20H
	JP	C,$12	; inside screen - no right hand clip
	INC	A
	LD	D,A
	LD	A,B
	SUB	D
	RET	C	; completely off right
	LD	B,A	; new width
	LD	A,D
	LD	($201),A	; add this to graphic pointer
	XOR	A
	LD	($200),A
	LD	A,$4-$5
	LD	($300),A	; make sure the last byte is ON
	JP	$11

$12	LD	A,$96-$97
	LD	($200),A	; modify jump so right isn't clipped
	XOR	A
	LD	($300),A

$11	EXX
	AND	A
	SBC	HL,DE	; 'normal' height subtract from base
	SBC	HL,DE
	LD	DE,0
$101	EQU	$-2
	ADD	HL,DE
	EX	DE,HL
	LD	H,MSKTAB/100H
	EXX
	INC	B

; Main loop entry parameters:
;	B = width in bytes
;	C = height in bytes
;	IX -> data
;	H -> rotation table MSB
;	D = 00H
;	DE' is pseudo DFaddress
;	H' -> mask table MSB
;	BC' = 0020H

$1	PUSH	BC	; save counters first
	EXX
	PUSH	DE	; save the pseudo df address
	EXX
	LD	IY,WORKSP	; shove graphic values here
	LD	D,0	; assume starting offset is 0

	JR	$	; jump to $99 if no left clip
$100	EQU	$-1
$98	LD	DE,0
$102	EQU	$-2
	ADD	IX,DE
	LD	A,(IX-1)	; take the prevoius one for a start
	OR	80H	;	   --------
	LD	L,A
	LD	D,(HL)
$99
	
; ------ This is the main loop of the real-size bitblit
$2	LD	A,(IX+0)	; get byte to display
	LD	L,A	; prepare index
	SRL	L	; create low-page LSB index
	LD	E,(HL)	; get left-hand
	OR	80H	; prepare hi-page LSB index
	LD	L,A
	LD	A,B	; is it the last loop?
	DEC	A
	LD	A,D	; then only use right hand from last time
	JR	$4
$300	EQU	$-1
$5	JP	Z,$3
$4	OR	E	; else insert the left one from now
$3	LD	E,A	; store accumulated graphic in E

; ------ Now prepare to calculate the mask data
	EXX
	LD	L,A	; form immediate address
	LD	A,(DE)	; screen
	AND	(HL)	; less mask
	OR	L	; but with the graphic
	LD	(DE),A	; insert into pseudo screen

; ------ Now both pre- and postcompensation for horizontal lines must be done
	PUSH	DE	; save the pseudo address
	EX	DE,HL
	SBC	HL,BC	; fetch previous line on screen
	LD	A,(DE)
	AND	(HL)	; mask that out with CURRENT mask
	OR	(IY+0)	; and then insert the graphic from
	LD	(HL),A	; last line and then back on screen.
	ADD	HL,BC
	ADD	HL,BC	; go to the line below
	LD	A,(DE)
	AND	(HL)	; and just mask it out
	LD	(HL),A
	EX	DE,HL	; pointers back
	POP	DE	; pseudo df back
	INC	E	; next byte pos.
	EXX

	LD	A,E
	LD	(IY+0),A	; now insert the accumulated graphic for this line

; ------ That's one byte done
	LD	D,(HL)	; that right-hand one
	INC	IX
	INC	IY	; increment those pointers
	DJNZ	$2

	DEC	IX
	DEC	IY

	JR	$200	; jp to $96 if no right clip
$200	EQU	$-1
$97	LD	BC,0
$201	EQU	$-2
	ADD	IX,BC

$96	EXX
	EX	DE,HL
	POP	HL
	ADD	HL,BC	; to next line
	EX	DE,HL
	EXX
	POP	BC
	DEC	C
	JP	NZ,$1
	RET

;
; Self-modifying code in Quadruple-sized sprites:-
;
; $100 : relative jump; 0 if left-hand clipping, $99-$98 if no clip
; $101 : pre-offset for screen pointer (16 bits, only used once)
; $102 : pre-offset for graphics pointer (16 bits)
; $200 : relative jump, 0 if right-hand clipping, $96-$97 if no clip
; $201 : post-offset for graphics pointer (16 bits)
; $300 : relative jump, 0 if no right-hand clipping, $4-$5 if clipping
;
; ------ Entry point for 2x magnification
LARGE	AND	A
	RL	B	; width *2
	LD	A,C
	DEC	E
	RET	Z
	RET	C	; carry here if totally clipped by
	LD	C,A	; horizon (or just skimming)
	LD	A,L	; left clip
	OR	A
	JP	P,$10	; jump if not outside in any way
	NEG
	LD	D,A	; this is how much is out
	LD	A,B
	SUB	D
	RET	C	; totally clipped - don't draw nothin', it ain't worth it
	LD	B,A	; new width
	LD	A,D
	LD	($101),A	; add to screen starting pointer
	RRA
	LD	($102),A	; modify pre-adder to graphics pointer
	OR	A	; was it zero bytes to add to graphics?
	JP	Z,$15	; yes, insert zero 
	XOR	A
	LD	($100),A	; guaranteed no right hand clipping now
	JP	$12
$15	LD	A,$99-$98
	LD	($100),A
	JP	$12

$10	XOR	A
	LD	($101),A	; give it a no-clip
	LD	A,$2-$98
	LD	($100),A	; no left-hand clip.
	LD	A,L	; right-hand clip factor
	ADD	B
	SUB	1FH
	JP	C,$12	; no right clip detected
	JP	Z,$12

	LD	D,A	; D is now actual clip factor
	LD	A,B	; Dramatically get back width.
	SUB	D	; is clip factor > width? should we draw anything at all?
	RET	C	; totally and utterly out of the question - and screen
	LD	B,A	; new width for object ('real' on-screen size)
	LD	A,D
	RRA
	LD	($201),A	; patch the width post-adder for graphics pointers
	XOR	A
	LD	($200),A
	LD	A,$4-$5
	LD	($300),A	; leave out last 'roll-in' byte
	DEC	B
	DEC	B
	JP	$11

$12	LD	A,$96-$97	; make sure no right-hand clipping occurs
	LD	($200),A
	XOR	A
	LD	($300),A

$11	EXX
	SBC	HL,DE
	SBC	HL,DE
	SBC	HL,DE
	SBC	HL,DE	; 4 times oversampling
	LD	DE,0
$101	EQU	$-2	; don't really need to do this every time
	ADD	HL,DE
	EX	DE,HL
	LD	H,MSKTAB/100H
	EXX

$1	PUSH	BC	; save counters first
	EXX
	PUSH	DE	; save the pseudo df address
	EXX
	LD	D,0	; now assume no left hand leftover. This is quite sensible

	BIT	7,B	; is there nothing to print (this would mean
	JP	NZ,$35	; that the right-hand clipping has problems)

	JR	$
$100	EQU	$-1
$98	LD	DE,0
$102	EQU	$-2	; what to add to the graphics pointer
	ADD	IX,DE

	LD	A,(IX-1)	; grab last byte
	OR	80H
	LD	L,A
	LD	D,(HL)

$99	BIT	0,B	; is the left nibble (to be a byte) on screen at all?
	JP	Z,$2	; jump if it is

	LD	A,(IX+0)
	LD	L,A
	SRL	L
	LD	E,(HL)
	OR	80H
	LD	L,A
	LD	A,E
	OR	D
	EXX
	CALL	EXPANDR
	EXX
	DEC	B
	INC	IX
	LD	D,(HL)

; ------ This is the main loop of the dual-size bitblit
$2	LD	A,(IX+0)	; get byte to display
	LD	L,A	; prepare index
	SRL	L	; create low-page LSB index
	LD	E,(HL)	; get left-hand
	OR	80H	; prepare hi-page LSB index
	LD	L,A
	LD	A,B	; test for possible exclusion of the
	DEC	A	; right hand 'roll-in' byte
	DEC	A
	LD	A,D
	JR	$4
$300	EQU	$-1	; if no right-hand clip, always or with E
$5	JP	M,$3
$4	OR	E

; ------ Now prepare to calculate the mask data
$3	EXX
	CALL	EXPAND
	EXX

	LD	D,(HL)	; that right-hand one
	INC	IX
	DEC	B
	DEC	B
	JP	P,$2

	DEC	IX

	JR	$
$200	EQU	$-1

$97	INC	IX
$35	LD	A,(IX+0)
	LD	L,A
	SRL	L
	LD	A,(HL)
	OR	D
	BIT	0,B
	EXX

	JP	Z,$25
	CALL	EXPAND
	INC	IX
	JP	$26

$25	CALL	EXPANDL

$26	EXX
	LD	BC,0
$201	EQU	$-2
	ADD	IX,BC

$96	EXX
	EX	DE,HL
	POP	HL
	LD	BC,40H
	ADD	HL,BC
	EX	DE,HL
	EXX

	LD	A,E
	ADD	D	; what's this then?

	POP	BC
	DEC	C
	JP	NZ,$1
	RET

; ------ This will expand a byte into four and mask it onto screen
EXPAND	LD	H,MSKTAB/100H
	LD	L,A
	LD	L,(HL)	; this is now the mask
	LD	H,MAGTAB/100H+1
	LD	C,A	; store initial graphic here
	LD	A,(DE)
	AND	(HL)
	LD	B,L
	LD	L,C
	OR	(HL)
	LD	(DE),A
	DEC	H
	INC	DE	; must be 16 bits ?
	LD	L,B
	LD	A,(DE)
	AND	(HL)
	LD	L,C
	OR	(HL)
	LD	(DE),A
	LD	A,E
	ADD	A,20H
	LD	E,A
	JP	NC,$3
	INC	D
$3	LD	L,B
	LD	A,(DE)
	AND	(HL)
	LD	L,C
	OR	(HL)
	LD	(DE),A
	INC	H
	DEC	DE	; ho-humm ?
	LD	L,B
	LD	A,(DE)
	AND	(HL)
	LD	L,C
	OR	(HL)
	LD	(DE),A
	LD	A,E
	SUB	1EH
	LD	E,A
	RET	NC
	DEC	D
	RET

; ------ Only the right-hand one
EXPANDR	LD	H,MSKTAB/100H
	LD	L,A
	LD	L,(HL)	; this is now the mask
	LD	H,MAGTAB/100H
	LD	C,A	; store graphic here
	LD	A,(DE)
	LD	B,(HL)
	AND	B	; store here
	LD	L,C
	OR	(HL)
	LD	(DE),A
	LD	A,E
	ADD	A,20H	; next line
	LD	E,A
	JP	NC,$3
	INC	D
$3	LD	A,(DE)
	AND	B
	OR	(HL)
	LD	(DE),A
	LD	A,E
	SUB	1FH
	LD	E,A
	RET	NC
	DEC	D
	RET

; ------ Left one solely
EXPANDL	LD	H,MSKTAB/100H
	LD	L,A
	LD	L,(HL)	; this is now the mask
	LD	H,MAGTAB/100H+1
	LD	C,A	; store graphic here
	LD	A,(DE)
	LD	B,(HL)
	AND	B
	LD	L,C
	OR	(HL)
	LD	(DE),A
	LD	A,E
	ADD	A,20H
	LD	E,A
	JP	NC,$3
	INC	D
$3	LD	A,(DE)
	AND	B
	OR	(HL)
	LD	(DE),A
	RET

; ------ Entry point for 2x shrinking 
SMALL	SRL	C
	LD	A,C
	SUB	E
	RET	Z
	RET	C
	LD	C,A

	LD	A,L
	OR	A
	RET	M

	ADD	B
	CP	20H
	RET	NC

	EXX
	RES	4,E
	SBC	HL,DE
	EX	DE,HL
	LD	H,MSKTAB/100H
	INC	E
	EXX

$1	PUSH	BC
	EXX
	PUSH	DE
	EXX
	LD	IY,WORKSP
	LD	D,0	; lefthand hangover starts as 00

$2	LD	C,H	; save ROTTAB pointer in c temporarily
	LD	H,SHRTAB/100H	; shrink constants
	LD	L,(IX+0)
	INC	IX
	LD	E,(HL)
	DEC	B	; decline 2nd byte if it's not there!
	JP	Z,$3
	LD	L,(IX+0)
	INC	IX
	LD	A,(HL)
	RRCA		; make the 2nd byte the 'high' nibble
	RRCA
	RRCA
	RRCA
	OR	E
	LD	E,A	; e is now graphic
	DEC	B	; quicker than a jump to keep B as is
$3	INC	B
	LD	A,E

	LD	H,C
	LD	L,A
	SRL	L
	LD	E,(HL)
	OR	80H
	LD	L,A
	LD	A,E
	OR	D	; this is the actual byte

	EXX
	LD	L,A
	LD	A,(DE)
	AND	(HL)
	OR	L
	LD	(DE),A

	PUSH	DE	; save the pseudo address
	EX	DE,HL
	SBC	HL,BC	; fetch previous line on screen
	LD	A,(DE)
	AND	(HL)	; mask that out with CURRENT mask
	OR	(IY+0)	; and then insert the graphic from
	LD	(HL),A	; last line and then back on screen.
	ADD	HL,BC
	ADD	HL,BC	; go to the line below
	LD	A,(DE)
	AND	(HL)	; and just mask it out
	LD	(HL),A
	EX	DE,HL	; pointers back
	POP	DE	; pseudo df back

	INC	E	; next byte pos.
	EXX
	LD	A,E
	OR	D
	LD	(IY+0),A	; now insert the graphic for this line

	LD	D,(HL)	; next left-hand hangover
	INC	IY
	DJNZ	$2

	LD	A,D

	EXX
	LD	L,A
	LD	A,(DE)
	AND	(HL)
	OR	L
	LD	(DE),A

	EX	DE,HL
	POP	HL
	ADD	HL,BC
	EX	DE,HL
	EXX

	POP	BC
	PUSH	BC
	LD	C,B
	LD	B,0
	ADD	IX,BC	;	skip a line of graphics
	POP	BC
	DEC	C
	JP	NZ,$1
	RET

; ------ ix-> data, B is width, C is height
BB_LOGO	LD	DE,4005H
	LD	IX,LOGO
	LD	B,11
	LD	C,4

	PUSH	DE	; use these for colours
	PUSH	BC

$0	PUSH	BC
	PUSH	DE
$1	PUSH	BC
	PUSH	DE

	LD	B,8
$2	LD	H,MAGTAB/100H+1
	LD	L,(IX+0)
	LD	A,(HL)
	LD	(DE),A
	INC	D
	LD	(DE),A
	INC	E
	DEC	H
	LD	A,(HL)
	LD	(DE),A
	DEC	D
	LD	(DE),A
	DEC	E
	INC	D
	INC	D
	LD	A,D
	AND	7
	JR	NZ,$6
	LD	HL,-07E0H
	ADD	HL,DE
	EX	DE,HL
$6	INC	IX
	DJNZ	$2

	POP	DE
	POP	BC
	INC	E
	INC	E
	DJNZ	$1

	POP	DE
	POP	BC
	LD	HL,40H
	ADD	HL,DE
	EX	DE,HL
	DEC	C
	JR	NZ,$0

; ------ Now the colours
	POP	BC
	POP	DE
	RL	B
	RL	C
$3	LD	A,D
	RRCA
	RRCA
	RRCA
	AND	3
	OR	58H
	LD	D,A
$4	PUSH	BC
	PUSH	DE

$5	LD	A,(IX+0)
	LD	(DE),A
	INC	E
	INC	IX
	DJNZ	$5

	POP	DE
	POP	BC
	LD	A,E
	ADD	20H
	LD	E,A
	DEC	C
	JR	NZ,$4
	RET

;
; Module name: Vertical line-draw
;
; Entry parameters: 
;	HL = pseudo df-address
;	E = primitive no. 0-14
;	C = height, B = pixel position
;

VERTLINE	LD	A,(XOFFSET+1)
	OR	A
	RET	NZ	; not out of screen
VERTLINEX	LD	A,E	; primitive
	OR	A
	RET	Z
	SRL	A	; subtract this from X coord (pixel)
	LD	D,A
	LD	A,B
	SUB	D
	JR	NC,$0
	DEC	L
	EX	AF,AF'
	LD	A,(XOFFSET)
	OR	A
	RET	Z
	EX	AF,AF'
$0	AND	7
	LD	B,A
	PUSH	HL
	LD	A,E
	AND	0EH	; two bytes/prim but also 2 prims per width, so skip bit 0
	LD	E,A
	LD	D,0
	LD	HL,WIDTHS2
	ADD	HL,DE
	LD	E,(HL)	; get graphic
	INC	HL	; get mask
	LD	D,(HL)
	POP	HL

	PUSH	BC
	LD	A,B	;	get pixel pos
	LD	C,D	; BC is mask, DE is graphic
	LD	B,0FFH	; zero out the high bytes
	LD	D,0
	OR	A
	JP	Z,$2
$1	AND	A
	RR	E	;	rotate to fit
	RR	D
	SCF
	RR	C
	RR	B
	DEC	A
	JP	P,$1
$2	EXX
	POP	BC	; B' must be counter now
	LD	B,C
$4	EXX
	LD	A,(HL)	; mask one piece on
	AND	C
	OR	E
	LD	(HL),A
	INC	L
	LD	A,(HL)	; and another
	AND	B
	OR	D
	LD	(HL),A
	LD	A,1FH	; next row
	ADD	L
	LD	L,A
	JP	NC,$3
	INC	H	; if necessary, increment MSB
$3	EXX
	DJNZ	$4
	RET

; ------ Convert pixel no. in A to pixel mask which is returned in B (also A)
NO2MASK	OR	A
	JP	Z,$2	; if it was zero, then return blank mask
	XOR	A	; else prepare to rotate
$1	SCF
	RRA		; add 1 bit
	DJNZ	$1	; however many times desired (<8)
$2	LD	B,A	; B holds mask on exit too
	LD	A,D
	DEC	A
	JP	Z,$3	; bottom primitive -> draw to end of line
	JP	M,$3
	LD	A,L
	AND	1FH	; X position
	CP	5
	JP	C,$3	; less than 5 goes to end-of-screen
	CP	1BH
	JP	NC,$3	; more than 1A also goes to end
	LD	D,4
	RET		; otherwise assume width of 4
$3	LD	D,0
	RET

; Module name: Left wall drawing - closely linked to drawing OBJECTS
;
; Entry parameters: 
;	HL-> display-file (U)
;	B  = pixel position (from LEFT) (P)
;	C  = height of wall (P)
;	D  = primitive no. (P)
;
; Additional notes: SPECIAL contains the actual object to be drawn; if
; it happens to be a TUNNxx, the wall drawing will be modified.
; Note that it is absolutely necessary to draw the RIGHT wall first when
; a tunnel is the end result.

LVLINE	LD	D,0	; again, to the end of the screen
	LD	A,10101010B
	LD	(WALPATTERN),A
	LD	A,B
	CALL	LVLINE2
	LD	HL,0
	LD	(LASTLXY),HL
	RET

LVLINE1	LD	A,B
LVLINE2	CALL	NO2MASK	; grab the mask from the pixel specification

; ------ Check to see if the primitive is at the bottom AND if it's the end of a tunnel
	LD	A,(PRIMITIVE)
	OR	A
	JP	NZ,$28
	INC	C	; One line deeper
	PUSH	HL
	LD	HL,(LASTLXY)	; These are both 0 if there is no 'active' tunnel
	LD	A,L	; continuing from last primitive. Remember that
	OR	H	; a TUNNLX/RX cancels tunnels.
	POP	HL
	RET	Z	;	Return already here if the tunnel end is near(est)
	LD	A,0FFH
	LD	(WALPATTERN),A

; ------ Insert the images to AND and OR with in the main loop
$28	LD	A,B
	LD	(LAYER),A
	CPL
	LD	(MASK),A	; complemented image creates display-file exclusion
	LD	A,(WALPATTERN)
	LD	(ORIGPAT),A
	AND	B	; get original image back again
	LD	(GRAPHIC),A	; store edge mask here

; ------ Find out whether to do N bytes or to go to the edge of the screen
	LD	A,D
	OR	A
	JP	Z,$8
	ADD	20H	; add a line offset to that number
	LD	E,A
	LD	A,D	; get the no-to-fill back again
	LD	D,0	; and zero out the high byte of the offset
	JP	$9

; ------ Calculate distance to edge of screen if that is what is desired
$8	LD	A,L	; displayfile
	AND	1FH	; x coordinate byte boundary
	ADD	20H	; subtract in effect 32 from that
	LD	E,A	; this is the line offset
	SUB	20H	; X coordinate div. 8 (byte boundary)

; ------ Modify code to jump past the fills not needed
$9	ADD	A
	EXX
	LD	E,A
	LD	D,0
	LD	HL,$10
	AND	A
	SBC	HL,DE
	LD	($0),HL
	EXX

	LD	A,B
	EX	AF,AF'
	LD	A,(WALPATTERN)
	LD	B,A

	PUSH	HL
	PUSH	BC	; save
	PUSH	DE

	EX	AF,AF'
	LD	B,A

; ------ Find the bottom line of the image to draw
	PUSH	HL	; save 1st display-file address
	EX	DE,HL
	LD	L,C
	LD	H,0
	DEC	L
	REPEAT 5	; 32 bytes per line
	ADD	HL,HL
	ADD	HL,DE	; plus the original

$35	PUSH	HL	; save 2nd display-file address
	LD	A,0FFH
	LD	(WALPATTERN),A
	LD	A,(TEMPY)
	SUB	C
	LD	(LOWERY),A
	LD	C,A
	LD	HL,(LASTLXY)
	LD	A,H
	OR	L
	JP	NZ,$14

; ------ If it's the 1st segment of a tunnel or wall, store these values
	POP	HL
	LD	A,L
	AND	0E0H
	LD	L,A
	LD	(BOTDF),HL
	LD	A,(TEMPY)
	LD	(TOPY),A
	LD	A,(LOWERY)
	LD	(BOTY),A
	POP	HL
	LD	A,L
	AND	0E0H
	LD	L,A
	LD	(TOPDF),HL
	LD	A,(PRIMITIVE)
	LD	(BACKPRIM),A
	JP	$5
$14
	LD	HL,(TEMPCOORDS)
	LD	A,(LASTLX)
	SUB	H	; lastX-tempX
	LD	D,A
	POP	HL
	JP	M,$39	; not to be drawn if slope is going left
	JP	Z,$39	; or indeed if it's a straight line

	LD	A,(LASTLY2)
	INC	A
	SUB	C
	NEG
	LD	E,A

	LD	A,(XOFFSET+1)
	OR	A
	JP	NZ,$39

	LD	A,B
	PUSH	BC
	CALL	LINEDRAW
	POP	BC

$39	LD	A,(SPECIAL)
	CP	TUNNL
	JP	C,$6

; ------ Right, then - it's tunnel ceiling drawing time
	LD	A,(PRIMITIVE)
	OR	A
	JP	NZ,$16
	LD	A,(SPECIAL)
	CP	TUNNLX
	JP	Z,$4	; only if tunnel end
$16	LD	HL,(TOPDF)
	LD	A,(TOPY)
	LD	C,A
	LD	A,(BACKPRIM)
	CP	APPEARANCE	; was it at the horizon?
	JP	C,$7
	LD	HL,(BOTDF)
	LD	A,(BOTY)
	LD	C,A
$7	INC	C
	LD	A,(TEMPY)
	SUB	C	; this is the height of the black
	JP	Z,$17
	JP	C,$17
	LD	B,A	; height
	LD	DE,-1
	CALL	$13	; draw a big black square
$17	LD	A,(PRIMITIVE)
	NEG		; -1 to -16
	ADD	20
	LD	B,A	; this is the height then !
	LD	A,(TEMPY)
	ADD	B
	JP	P,$15
	AND	7FH	; this is what's too much
	SUB	B
	NEG
	JP	Z,$4	; leave
	LD	B,A
$15	LD	A,(SPECIAL)
	CP	TUNNLX
	JP	NZ,$19	; black if not end of tunnel
	LD	A,(PRIMITIVE)
	OR	A
	JP	Z,$19	; if it's not at the bottom, draw stippled
$18	LD	DE,0AAAAH
$19	CALL	$13
	JP	$4	; leave

$13	DI
	LD	(TEMPSP),SP
	LD	SP,HL
$12	REPEAT 16
	PUSH	DE
	DJNZ	$12
	LD	HL,0
	ADD	HL,SP
	LD	SP,(TEMPSP)
	EI
	RET

$6	LD	HL,(TEMPCOORDS)
	LD	A,(LASTLX)
	SUB	H
	JP	M,$4

	LD	D,A
	LD	A,(LASTLY1)
	DEC	A
	SUB	L	; lastY1-lastX (for upper wall)
	NEG
	LD	E,A
	LD	A,(XOFFSET+1)
	OR	A
	JP	NZ,$4	; make sure to miss out the triangle if it's off-screen

	POP	HL	; reget dfaddr
	LD	A,B
	CALL	LINEDRAW
	JP	$5

$3	POP	HL	; skip the top entry on stack
$4	POP	HL
$5	LD	HL,(TEMPCOORDS)
	LD	(LASTLXY),HL
	LD	A,(LOWERY)
	LD	(LASTLY2),A
	LD	A,(SLPWIDTH)
	LD	(LASTWIDTH),A

	LD	A,(XOFFSET+1)
	OR	A
	JP	Z,$53	; only continue 
	POP	DE	; out!
	POP	BC	; clean stack and goback.
	POP	HL
	RET

; ------ Insert parameters which change during printing loop
$53	EXX
	LD	BC,(GRAPHIC)	; graphic & mask
	LD	A,(ORIGPAT)
	LD	E,A
	LD	A,(LAYER)
	LD	D,A
	EXX

; ------ Get back all parameters
	POP	DE
	POP	BC
	POP	HL

; ------ Edge byte shoved on screen
$1	LD	A,(HL)
	EXX
	AND	B
	OR	C
	EXX
	LD	(HL),A

; ------ Jump into fill string of opcodes
$33	JP	0FFFFH	; this is patched to index from $10
$0	EQU	$-2
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B
	DEC	L
	LD	(HL),B

; ------ Flows through to here where the next pixel row is found
$10	ADD	HL,DE
	DEC	C
	JP	NZ,$1
	RET

; Module name: Right wall drawing - closely linked to drawing OBJECTS
;
; Entry parameters: 
;	HL = display-file address
;	A  = pixel position (from LEFT)
;	C  = height of wall
;	D  = primitive no.

RVLINE	LD	D,0	; 0 means "to the edge of the screen"
	LD	A,10101010B
	LD	(WALPATTERN),A
	LD	A,B
	CALL	RVLINE2
	LD	HL,0
	LD	(LASTRXY),HL
	RET
RVLINE1	LD	A,B	; B was pixel position
RVLINE2	CALL	NO2MASK	; B is now pixel image mask

; ------ Check to see if the primitive is at the bottom AND if it's the end of a tunnel
	LD	A,(PRIMITIVE)
	OR	A
	JP	NZ,$31	; Quit now if already wrong
	INC	C	; and 1 more line
	PUSH	HL
	LD	HL,(LASTRXY)	; These are both 0 if there is no 'active' tunnel
	LD	A,L	; continuing from last primitive. Remember that
	OR	H	; a TUNNLX/RX cancels tunnels.
	POP	HL
	RET	Z	;	Return already here if the tunnel end is near(est)
	LD	A,0FFH
	LD	(WALPATTERN),A

; ------ Insert edge mask
$31	LD	A,B
	LD	($2),A	; modify modifier etc.
	CPL
	LD	B,A
	LD	A,(WALPATTERN)
	AND	B
	LD	($11),A
	LD	A,D
	OR	A	; is it too close to the edge?
	JP	Z,$8
	SUB	20H
	NEG
	LD	E,A
	LD	A,D
	LD	D,0
	JP	$9
$8	LD	A,L
	AND	1FH	; Byte boundary
	INC	A
	LD	E,A	; this is the actual offset!!
	SUB	20H
	NEG		; number of bytes to insert

; ------ Modify the jump statement below to skip desired no. of fills
$9	ADD	A
	EXX
	LD	E,A
	LD	D,0
	LD	HL,$10
	AND	A
	SBC	HL,DE
	LD	($0),HL	; modify the jump instruction below
	EXX

	LD	A,B
	EX	AF,AF'
	LD	A,(WALPATTERN)
	LD	B,A
	PUSH	HL
	PUSH	BC
	PUSH	DE
	EX	AF,AF'
	LD	B,A

; ------ Find the bottom line of the image to draw
	PUSH	HL	; save 1st display-file address
	EX	DE,HL
	LD	L,C
	LD	H,0
	DEC	L
	REPEAT 5	; 32 bytes per line
	ADD	HL,HL
	ADD	HL,DE	; plus the original
	PUSH	HL	; save 2nd display-file address
	LD	A,0FFH	; that colour
	LD	(WALPATTERN),A	; is the selected wallpattern
$7	LD	A,(TEMPY)
	SUB	C
	LD	(LOWERY),A	; lowery=y-height
	LD	C,A
	LD	HL,(LASTRXY)	; skip the following code if this is
	LD	A,H	; the first segment - just draw a square, no triangles
	OR	L	; and store all relevant information.
	JP	Z,$3	; don't draw anything if it's the first wall
	LD	HL,(TEMPCOORDS)	; Just drawn X and Y
	LD	A,(LASTRX)	; previous X
	SUB	H
	JP	P,$3	; do not draw anything if line slopes right
	LD	D,A	; this is deltaX
	LD	A,(LASTRY2)	; this is BOTTOM y coordinate from before
	INC	A
	SUB	C
	NEG
	LD	E,A	; recalculate the 
	POP	HL	; get display-file address back
	LD	A,(XOFFSET+1)	; Is X out of screen anyway?
	OR	A
	JP	NZ,$99
	LD	A,B	; retrieve the pixel mask again
	PUSH	DE
	PUSH	BC
	CALL	LINEDRAW	; draw triangle at bottom corner
	POP	BC
	POP	DE
$99	LD	A,(SPECIAL)
	CP	TUNNL	; was it a tunnel segment (>TUNNL)
	JP	NC,$4	; yes, don't draw rest
	LD	HL,(TEMPCOORDS)
	LD	A,(LASTRY1)
	DEC	A
	SUB	L	; -(lasty1-tempy) (for upper wall)
	NEG
	LD	E,A
	LD	A,(XOFFSET+1)
	OR	A
	JP	NZ,$4	; exit now if it was out of screen.
	POP	HL	; display-file address
	LD	A,B	; pixel mask
	CALL	LINEDRAW
	JP	$5
$3	POP	HL
$4	POP	HL	; skip the top entry on stack
$5	LD	HL,(TEMPCOORDS)
	LD	(LASTRXY),HL
	LD	A,(LOWERY)
	LD	(LASTRY2),A
	POP	DE
	POP	BC
	POP	HL
	LD	A,(XOFFSET+1)
	OR	A
	RET	NZ	; return now if it's off screen

; ------ Start drawing the square part now
$1	LD	A,(HL)
	AND	0
$2	EQU	$-1
	OR	0
$11	EQU	$-1
	LD	(HL),A

; ------ Deal with rest of line - jump into string of fill opcodes
	JP	0FFFFH
$0	EQU	$-2
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B
	INC	L
	LD	(HL),B

; ------ Next pixel line
$10	ADD	HL,DE
	DEC	C	; c holds number of pixel-lines left to draw
	JP	NZ,$1
	RET

; Module name: Horizontal linedraw, any thickness, 15:20:53 on 17/02/88
;
; Entry parameters: 
;	HL-> displayfile (U)
;	E = primitive (P)
;	C = height (P)
;	B = width (P)
;

HORIZLINE	LD	A,(SPECIAL)
	CP	THICKLINE
	LD	A,(NEXTY)
	JR	Z,$1
	SRL	A
$1	OR	A
	RET	Z
	LD	C,A
	LD	A,L
	AND	0E0H
	LD	L,A
HORIZ0	LD	DE,-1
	LD	A,(SPECIAL)
	CP	THICKLINE
	JR	Z,$4
	LD	E,-20H
	ADD	HL,DE
	CALL	RANDOM
	LD	E,A
	AND	7
	INC	A
	LD	B,A
	XOR	A
	SCF
$1	RRA
	DJNZ	$1
	LD	D,A
	LD	A,E
	RRA
	RRA
	RRA
	AND	7
	INC	A
	LD	B,A
	XOR	A
	SCF
$3	RRA
	DJNZ	$3
	LD	E,A
$4	DEC	HL
	LD	(HL),E
	DEC	L
	LD	(HL),D
	DO	15
	 DEC	L
	 LD	(HL),E
	 DEC	L
	 LD	(HL),D
	LOOP
	DEC	C
	JP	NZ,HORIZ0
	RET

; ------ Draw this primitive as a black line across the screen
WIDEPRIM	LD	A,L
	AND	0E0H
	LD	L,A
	LD	B,20H
	LD	A,0FFH
	DEC	HL
$1	LD	(HL),A
	DEC	L
	DJNZ	$1
	RET


;
	SUBTTL "Collision detection and actions
;
; Module name: Collision detection and action
;
; Entry parameters: 
;	none
;
; Exit parameters: 
;	A = status
;
; Objects which make you crash/slide:
; 	Palmtree, Beechtree, Pinetree, Boulder, Fence, Brick Wall
;	Barrel
;
; Other objects:
;	Log (Bounce)
;	Rock (Side wheelie)
;	100pts/250pts/500pts (Points)
;	Timegate
;	Flag (points & sequence)
;	Shrubbery (Slow down)
;	Start banner (nothing)
;	
; Anything => MAPSEGMENT is largely ignored (but not completely)
;

COLLISION	LD	IX,(OBJ_PTR)	; 1st object in sight 
	CALL	TEST_ROAD
	EXX
	OR	A
	JR	NZ,$0	; nothing in site
	LD	(HITFLAG),A	; reset hit-flag
	RET
$0	CP	VLINE
	JR	C,$1
	CP	WALLL
	RET	C	; not in range!
	LD	L,CRASHOBJECT*2	; anything above vline+1 crashes!
	JR	$2
$1	ADD	A
	LD	L,A
	LD	A,(HITFLAG)
	OR	A
	RET	NZ	; must be free of a collision to make another
$2	LD	A,0FFH
	LD	(HITFLAG),A
	LD	H,0
	LD	DE,ACTIONTBLE-2
	ADD	HL,DE	
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	(HL)

; ------ This does nothing
A_GOBACK	RET

; ------ Crash the buggy
A_CRASH	LD	A,(IX+3)
	CP	HASSELBLAD
	JR	NZ,$5
$3	LD	BC,0
	LD	(BUGGY),BC
$5	EX	AF,AF'
	EXX
	ADD	HL,DE
	LD	A,L
	OR	A
	JP	P,$4
	NEG
$4	LD	B,A
	LD	A,E	; this is half width

	SRL	A
	SUB	B	; crash or just skid a bit

	LD	C,A
	EX	AF,AF'
	CP	HASSELBLAD	; make sure this will crash - even when flying
	JR	Z,$0
	CP	WALLL	; more than left wall is a slide
	JP	NC,$9
	EX	AF,AF'
	JR	C,$1	; jump if crash

; ------ The buggy has crashed!
$6	LD	A,(JUMPFLAG)
	OR	A
	RET	NZ	; can't crash if flying except for tunnels
	LD	A,(SPEED)
	CP	20H
	RET	C	; too slow to crash !
$0	LD	A,(CRASHFLAG)
	OR	A
	RET	NZ	; check for already being in crash
	LD	A,10H
	LD	(CRASHFLAG),A	; set to 12 frames explosion and 4 frames pause
	RET

; ------ Pump buggy left or right a bit but maintain speed
$9	LD	C,-12
$1	LD	A,(JUMPFLAG)
	OR	A
	RET	NZ	; can't slide in the air
	LD	A,C
	RL	L	; which way?
	LD	B,0FFH	; assume left
	JR	C,$2	; good assumption
	NEG
	INC	B	; otherwise make it positive
$2	LD	C,A
	LD	HL,(BUGGY)
	AND	A
	REPEAT 3
	SBC	HL,BC	; update buggy's position
	LD	(BUGGY),HL
	RET

; ------ Bouncing on a log
A_BOUNCE	LD	A,(CRASHFLAG)
	OR	A
	RET	NZ	; not if crashing!
	LD	A,0FFH
	LD	(JUMPFLAG),A
	LD	A,(SPEED)
	LD	C,2
	CP	150
	JR	C,$0
	INC	C
$0	LD	A,C
	LD	(JUMPFACTOR),A
	XOR	A
	LD	(JUMPOFFSET),A
	LD	(JUMPDELAY),A
	RET

; ------ Getting a Timegate
A_TIME	LD	HL,TGPICKED
	INC	(HL)
	RET

; ------ Going through Points gates
A_100	LD	BC,#0100
	JR	A_GATE
A_250	LD	BC,#0250
	JR	A_GATE
A_500	LD	BC,#0500
A_GATE	CALL	ADDSCORE
	RET

; ------ Collecting a Flag
A_FLAG	LD	BC,#0030
	CALL	ADDSCORE
	RET

; ------ Hitting a Rock
A_ROCK	LD	A,(TILT)
	OR	A
	RET	NZ	; already tilting
	LD	A,(JUMPFLAG)
	OR	A
	RET	NZ
	EXX
	ADD	HL,DE
	LD	BC,0405H
	BIT	7,L
	JR	Z,$1
	LD	BC,0FCFBH
$1	LD	A,B
	LD	(TILT),A
	LD	(TILTDES),A	; be the same...
	CALL	RANDOM
	AND	0FH
	INC	A
	LD	(TILTDELAY),A
	LD	A,(SPEED)
	CP	100
	RET	C
	LD	A,C
	LD	(TILTDES),A	
	RET

; ------ Getting slowed down by a Shrubbery
A_SLOW	LD	A,(SPEED)
	AND	A
	RRA
	LD	(SPEED),A	; half speed
	RET

; ------ Grab an object from the (object) table
TEST_ROAD	LD	L,(IX+0)
	LD	H,(IX+1)	; DE is now its position
	LD	BC,(MILEAGE)
	AND	A
	SBC	HL,BC
	JP	Z,NEXT_ONE	; zero - it's already off!
	LD	A,(IX+3)
	CP	MAPSEGMENT
	JP	NC,NEXT_ONE
	LD	A,L	; 'effective' (relative) primitive found
	CP	3
	JP	NC,LAST_ONE	; more than two - not on yet, and quit

; ------ An object has been found that might have X-coordinate collision
	LD	H,0
	LD	A,(IX+2)	; X-position/row no.
	LD	B,A
	ADD	A
	JR	Z,$2	; zero - keep zero.
	ADD	A
	ADD	A
	ADD	A	; find its position
	NEG
	BIT	7,B	; was it negative in the first place?
	JR	NZ,$2	; jump if not -
	DEC	H
$2	LD	L,A	; HL is now the 'middle' of this thing
	EXX
	LD	H,0
	LD	L,(IX+3)
	LD	C,L
	LD	A,L
	CP	VLINE
	JR	C,$3
	CP	HASSELBLAD
	LD	A,040H
	JR	Z,$5
	LD	A,20
	JR	$5
$3	LD	DE,GRAPHINDEX-1
	ADD	HL,DE
	LD	A,(HL)
	ADD	2	; we want the biggest one
	LD	L,A
	LD	H,0
	ADD	HL,HL
	ADD	HL,HL
	LD	DE,OBJECTDATA-4
	ADD	HL,DE
	LD	A,(HL)	; A is now half width
$5	EXX
	ADD	A
	LD	E,A
	LD	D,0
	LD	A,H
	ADD	80H
	LD	H,A
	PUSH	HL
	ADD	HL,DE
	LD	BC,(BUGGY)
	PUSH	HL
	LD	HL,8000H
	ADD	HL,BC
	LD	C,L
	LD	B,H
	POP	HL
	AND	A
	SBC	HL,BC
	POP	HL
	JP	C,NEXT_ONE	; not hit
	SBC	HL,DE
	SBC	HL,BC	; difference
	JP	NC,NEXT_ONE
	LD	A,(IX+3)
	RET
LAST_ONE	XOR	A	; 0 means no collision
	RET
NEXT_ONE	LD	BC,4
	ADD	IX,BC
	JP	TEST_ROAD
;
; ------ Clear the attribute area in the colour in A
CLEARATTR	LD	(TEMPSP),SP	; clear attributes
	LD	SP,5B00H-1
	LD	E,A
	LD	D,A
	LD	A,(JUMPOFFSET)
	ADD	07H
	LD	L,A
	SUB	10H
	NEG
	LD	B,A	; how many lines in yellow
$0	REPEAT 15
	PUSH	DE
	DEC	SP
	DEC	SP
	DJNZ	$0
	SET	6,E
	SET	6,D
	LD	B,L
$1	REPEAT 15
	PUSH	DE
	DEC	SP
	DEC	SP
	DJNZ	$1
	LD	SP,(TEMPSP)	
	RET

; Module name: Calculation of X coordinates, 12:01:20 on 11/12/87
;
; Entry parameters: 
;	A = row number (-16 to 16) (N)
;	E = angle (0-15) (P) (but please only use 0-14)
;
; Exit parameters: 
;	HL = the X offset value of the row (I)
;
; Notes: This takes into consideration the buggy's possible offset from the
; centre, the road's movement up the screen and the value is given as the
; normal fraction between the point and its 'future'.

XCALC	OR	A
	JP	P,XCPOS
	NEG		; must be postitive for now

; ------ Find the original X offset value asked for if NEGATIVE
	CALL	XCALC1	; pointers and calculated constants
	LD	H,(IX+1)	; the offset(0)
	LD	A,(BC)	; add the Xposition(0)
	LD	L,A
	LD	A,(IX+0)
	SUB	L
	LD	L,A
	JP	NC,$2	; 16-bit addition
	DEC	H

; ------ Now for the second primitive so a fraction can be found
$2	LD	A,E
	OR	A	; is primitive 0?
	RET	Z	; no fraction - return if so.
	INC	BC	; forward to the next x position
	PUSH	HL	; save it
	LD	H,(IX+3)	; offset(1)
	LD	A,(BC)
	LD	L,A	; add offset(1) to xposn(1)
	LD	A,(IX+2)
	SUB	L
	LD	L,A
	JP	NC,$4	; only decrement hibyte if necessary
	DEC	H
$4	EXX
	PUSH	BC
	EXX
	POP	DE
	ADD	HL,DE
	JP	FINDFRACT

; ------ Find the original X offset value asked for if POSITIVE
XCPOS	CALL	XCALC1	; set up pointers etc
	LD	H,(IX+1)	; the offset(0)
	LD	A,(BC)	; add the Xposition(0)
	ADD	(IX+0)
	LD	L,A
	JP	NC,$2	; 16-bit addition
	INC	H

; ------ Now for the second primitive so a fraction can be found
$2	LD	A,E	; primitive
	OR	A
	RET	Z	; no fraction if bottom primitive
	INC	BC	; forward to the next x position
	PUSH	HL	; save it
	LD	H,(IX+3)	; offset(1)
	LD	A,(BC)
	ADD	(IX+2)	; add offset(1) to xposn(1)
	LD	L,A
	JP	NC,$4	; only increment hibyte if necessary
	INC	H
$4	EXX
	PUSH	BC
	EXX
	POP	DE	; this is the interprimitive
	ADD	HL,DE

; ------ Now find the fraction according to the current state
FINDFRACT	LD	DE,(FRACT_PTR)
	POP	BC	;	get back original offset
	AND	A
	SBC	HL,BC	; HL is now the difference
	LD	A,L
	OR	A
	JP	P,$5
	NEG
	ADD	E
	LD	E,A
	EX	DE,HL
	LD	A,C
	ADD	(HL)
	JP	NC,$6
	INC	B
	JP	$6
$5	ADD	E
	LD	E,A	; create a fraction-table index from it
	EX	DE,HL
	LD	A,C	; this is the fraction
	SUB	(HL)	; add to the low byte
	JP	NC,$6
	DEC	B
$6	LD	C,A
$7	EXX
	PUSH	HL
	EXX
	POP	HL
	ADD	HL,BC	; actually add the fraction!
	RET

; ------ Call with angle in E and row in A, rets various pointers
XCALC1	LD	D,A
	LD	A,E
	EXX
	LD	BC,(TRACK_PTR)
	LD	HL,0
$1	EX	AF,AF'
	LD	A,(BC)	; get some motion data from track
	LD	D,0	; assume positive
	OR	A
	JP	P,$2	; good assumption
	DEC	D	; else make hi-byte negative, ie. sex
$2	LD	E,A	; and insert low-byte
	ADD	HL,DE	; do the sub/add
	INC	C	; next entry in track table
	EX	AF,AF'
	DEC	A	; decrement primitive counter
	JP	NZ,$1
$3	LD	BC,(TRACK_PTR)
	LD	A,(BC)
	OR	A
	LD	B,0
	JP	P,$4
	DEC	B
$4	LD	C,A	; BC' is now interprimitive offset
	EXX		; HL' is now track motion offset
	
; ------ Set up various index pointers
	LD	L,E	; E is still primitive
	LD	H,0
	LD	BC,OFFSETS	; offset (!) to offset table
	ADD	HL,HL
	ADD	HL,BC	; points to right 16-bit signed offset
	PUSH	HL
	LD	A,D	; get row number back
	ADD	A
	ADD	A	; multiple up to 16
	ADD	A
	ADD	A
	LD	L,A
	LD	H,0	; x16 of original
	LD	D,H	; make DE an offset it cannot refuse...
	LD	BC,XPOSNS
	ADD	HL,BC
	ADD	HL,DE	; points to real X
	LD	C,L	; BC -> appropriate 8-bit X offset (P)
	LD	B,H
	POP	IX	; IX -> appropriate 16-bit offset (I)
	RET
;
COLOURMAP	LD	HL,CMAPDELAY
	LD	A,(HL)
	OR	A
	LD	(HL),5
	RET	NZ	; not if cmapdelay is still not down
	LD	DE,(MAP_PTR)
	LD	HL,(MAP_DEST)
	LD	A,(HL)
	AND	0F8H
	OR	RED
	LD	(HL),A
	LD	A,(DE)

	INC	A
	JP	Z,START	; end of track data ***
	DEC	A

	INC	DE
	LD	(MAP_PTR),DE
	RLCA
	RLCA
	AND	3
	ADD	>MOVMTAB
	LD	C,A
	LD	B,<MOVMTAB
	LD	A,(BC)
	LD	E,A
	ADD	L
	LD	L,A
	LD	(MAP_DEST),HL
	RET

FLASHCMAP	LD	HL,(MAP_DEST)
	LD	A,(FRAME)
	AND	8	; roughly 0.64 sec's flash rate
	LD	A,(HL)
	JR	Z,$0
	AND	0F8H
	OR	BLUE
	LD	(HL),A
	RET
$0	AND	0F8H
	OR	RED
	LD	(HL),A
	RET
;
; ------ Move pseudo screen to main screen - 4K to be moved
MOVESCR	LD	BC,SCRTAB
	LD	A,80H
	DI
	LD	(TEMPSP),SP	; use stack to get data from pseudo
	LD	SP,DF_PSEUDO
MOVESCR1	EX	AF,AF'	; save counter while things go on
	LD	A,(BC)
	LD	L,A	; get low byte of df address
	INC	BC
	LD	A,(BC)
	LD	H,A	; and high byte
	INC	BC
	INC	L	; only 30 bytes
	INC	SP
	DO	0FH	; repeat this 15 times (for 30 bytes):
	 POP	DE	;  get a word of pseudo data
	 LD	(HL),E	;  insert 1st word
	 INC	L
	 LD	(HL),D	;  insert 2nd word
	 INC	L
	LOOP
	INC	SP
	EX	AF,AF'	; get counter back
	DEC	A
	JP	NZ,MOVESCR1
	LD	SP,(TEMPSP)	; restore stack
	EI		; and interrupt state
	RET

; ------ Delay 10 milliseconds
DELAY10MS	LD	C,10
$0	CALL	DELAY1MS
	DEC	C
	JR	NZ,$0
	RET

; ------ Delay 1 millisecond
DELAY1MS	LD	B,35
$0	REPEAT 4
	LD	IX,(0)	; 80 cycles
	INC	HL	;  6 cycles
	DEC	B	;  4 cycles
	JP	NZ,$0	; 10 cycles
	RET		; --------- = 100 x 35 = 3500 cycles

; ------ Reflect all buggy images, enter HL->data, C=no. of characters
REFLECT	LD	D,REFTAB/100H	; point to reflections
$0	LD	A,(HL)
	INC	HL
	OR	A
	JP	Z,$1	; don't reflect
	LD	B,8
$2	LD	E,(HL)
	LD	A,(DE)
	LD	(HL),A
	INC	HL
	DJNZ	$2
$1	DEC	C
	JP	NZ,$0
	RET

; ------ Draw buggy contained in (BUGFRAME) which is a number -6 to 6
DRAWBUGGY	LD	A,(CRASHFLAG)
	OR	A
	JR	Z,$9
	CP	4
	RET	C
$9	LD	HL,DF_PSEUDO+610H
	LD	A,(JUMPFLAG)
	OR	A
	JR	Z,$6	; test for jumping
	LD	A,(JUMPOFFSET)
	LD	C,A
	LD	A,H
	SUB	C
	LD	H,A
$6	LD	A,(BUGFRAME)
	OR	A
	JR	NZ,$1
	INC	H
	LD	C,A
	LD	A,(FRAME)
	LD	B,A
	LD	A,(JUMPFLAG)
	OR	A
	JR	Z,$4	; not jumping
	LD	A,(JUMPFACTOR)
	OR	A
	JP	M,$5	; downwards
	JR	$0
$4	LD	A,(SPEED)
	CP	24	; <30 means no swap
	JR	C,$0	; no skid (intet lort ellerva')
	CP	48
	JR	NC,$2	; <60 means rotate 6
	RR	B
	RR	B
$2	CP	72
	JR	NC,$3
	RR	B
	RR	B
$3	RR	B
	JR	C,$0
$5	DEC	H
	LD	C,3
$0	LD	A,C
$1	CP	-6
	JR	Z,$8
	CP	6
	JR	NZ,$7
$8	INC	H
	INC	H
	JR	STATICDRAW
$7	CP	4
	JR	C,STATICDRAW
	CP	-4+1
	JR	NC,STATICDRAW
	DEC	H

; Module name: Static objects with colour; time-constant routine ( <8ms )
;
; Exit parameters: 
;	A = buggy image -n to n
;	HL = df address (middle of screen)

STATICDRAW	LD	C,A
	LD	B,3FH
	CP	6
	JR	Z,$1
	CP	-6
	JR	NZ,$2
$1	LD	B,7FH
$2	LD	A,B
	LD	(STATM9),A	; buggies = nonbright (take out if buggies nonbright)
	LD	A,C
	ADD	BUGGIES
	ADD	A
	ADD	A
	ADD	A
	LD	C,A
	LD	B,0
	LD	IX,BB_TABLE
	ADD	IX,BC
	LD	A,(IX+1)	; offset
	LD	(STATM5),A
	LD	(STATM6),A
	LD	A,(IX+2)	; direction of L
	LD	(STATM1),A
	LD	(STATM2),A
	LD	(STATM3),A
	LD	(STATM4),A
	LD	A,(IX+3)
	LD	(STATM7),A
	OR	A
	JP	P,$0
	NEG
$0	LD	(STATM8),A
	LD	E,(IX+6)	; this is "STW"
	LD	D,(IX+7)	; offset
	ADD	HL,DE	; make desired df address
	PUSH	HL
	EXX
	POP	DE
	LD	A,E
	AND	1FH
	LD	E,A
	LD	A,D
	LD	D,59H
	SUB	DF_PSEUDO/100H
	ADD	A
	ADD	A
	ADD	A
	LD	L,A
	LD	H,0
	ADD	HL,HL
	ADD	HL,HL	; x 20H
	ADD	HL,DE
	LD	E,(IX+0)
	LD	D,0	; offset
	EXX
	LD	E,(IX+4)	; address of buggy
	LD	D,(IX+5)
	EX	DE,HL
	LD	(TEMPSP),SP
	DI
	LD	SP,HL
	EX	DE,HL
	LD	C,20H
STATIC0	LD	B,0
STATM8	EQU	$-1
STATIC1	DEC	SP
	POP	AF	; A is attribute
	OR	A
	JP	Z,STATIC4	; jump if this character is not there (!)
	JP	M,STATIC2	; jump if this character is to be MASKED

	EXX
	IF	BUGCOLOUR
	AND	0
STATM9	EQU	$-1
	LD	(HL),A	; put attribute onto real screen
	ENDIF
STATM1	NOP
	EXX
	LD	A,L
	
	DO	3

	POP	DE
	LD	(HL),E
	ADD	C
	LD	L,A
	LD	(HL),D
	ADD	C
	LD	L,A

	LOOP

	POP	DE	; 7 and 8
	LD	(HL),E
	ADD	C
	LD	L,A
	LD	(HL),D
	ADD	0	; to next byte, this wraps around zero.
STATM5	EQU	$-1
	LD	L,A

STATIC3	DJNZ	STATIC1	; all characters

	SUB	0	; back to the 1st
STATM7	EQU	$-1
	LD	L,A
	INC	H	; to next line
	EXX
	ADD	HL,DE	; to next line
	LD	A,H
	EXX

	CP	05BH	; out of screen?
	JP	C,STATIC0	; not yet

LEAVESTAT	LD	SP,(TEMPSP)	; get Stack pointer back again
	EI
	RET

STATIC2	EXX
	INC	A	; end-of thingy
	JR	Z,LEAVESTAT

;	IF	BUGCOLOUR
;	LD	(HL),P_YELLOW	; no colour
;	ENDIF

STATM2	NOP		; next attr address anyway
	EXX

	DO	3

	POP	DE
	LD	A,(HL)
	OR	E
	LD	(HL),A
	LD	A,L
	ADD	C
	LD	L,A
	LD	A,(HL)
	OR	D
	LD	(HL),A
	LD	A,L
	ADD	C
	LD	L,A

	LOOP

	POP	DE
	LD	A,(HL)
	OR	E
	LD	(HL),A
	LD	A,L
	ADD	C
	LD	L,A
	LD	A,(HL)
	OR	D
	LD	(HL),A

	LD	A,L
	ADD	0
STATM6	EQU	$-1
	LD	L,A
	JP	STATIC3

STATIC4
STATM3	NOP		; skip character
	EXX	

STATM4	NOP
	EXX
	LD	A,L
	JP	STATIC3

; Module name: Draw patches of deep water
;
; Entry parameters: 
;	HL-> displayfile (U)
;	E = primitive (P)
;	C = height (P)
;	B = pixel position (P)
;
; Exit parameters: 
;	none.
;
; Notes: WATERL is entry point for shaded patch, WATERBL is entry point for
;        black water.

WATERBL	LD	A,11111111B
	JP	WATERGAFFL
WATERL	LD	A,10101010B
WATERGAFFL	LD	(WALPATTERN),A
	LD	A,(XOFFSET+1)
	OR	A
	RET	NZ
	LD	A,(LASTDFL+1)
	OR	A
	JP	Z,GRABPRAMSL	; exit now if it was out of screen

	PUSH	BC	; save original starting point
	PUSH	HL
	LD	A,(LASTDFL+1)
	LD	($101+1),A	; store hi-byte
	LD	A,L
	AND	1FH
	LD	C,A	; C is now offset
	LD	A,(LASTDFL)
	AND	0E0H
	OR	C
	LD	($101),A	; $101 now holds app. addr. for a downwards line

	LD	A,B
	EX	AF,AF'
	LD	BC,(TEMPCOORDS)
	LD	DE,(LASTXYL)
	LD	A,C	; dy=lasty-currenty
	SUB	2
	SUB	E
	LD	E,A
	LD	A,D	; dx=lastx-currentx
	SUB	B
	LD	D,A
	JP	NC,$1	; jump if drawing upwards
	NEG
	LD	D,A
	XOR	A
	SUB	E	; invert Y
	LD	E,A
	LD	HL,(LASTDFL)
	LD	($101),HL
	LD	A,(LASTPPOSL)	; get parameters from the other point
	EX	AF,AF'
$1	LD	A,E	; Y delta
	LD	($100),A
	LD	E,A
	EX	AF,AF'	; get back pixel position

	ADD	>BITPOSNS
	LD	C,A
	LD	B,<BITPOSNS
	LD	A,D	; test for delta X being zero and
	OR	A	; don't draw a triangle if it is.
	LD	A,(BC)	; pixel mask

	CALL	NZ,LINEDRAW	; draw offending triangle if it's there

	LD	A,0
$100	EQU	$-1	; height of wall
	OR	A
	JP	P,$4
	NEG
$4	LD	C,A
	LD	HL,0
$101	EQU	$-2	; low byte of HL
	LD	A,L
	AND	1FH
	JP	Z,$5	; jump if zero
	LD	($102),A	; this is the width
	SUB	20H
	NEG
	LD	E,A
	LD	D,0
	LD	A,L
	AND	0E0H
	LD	L,A
	LD	A,(WALPATTERN)	; fill with this
$2	LD	B,0
$102	EQU	$-1
$3	LD	(HL),A
	INC	L
	DJNZ	$3
	ADD	HL,DE
	DEC	C
	JP	NZ,$2
$5	POP	HL
	POP	BC

; ------ Store info about the point chosen
GRABPRAMSL	LD	A,B
	LD	(LASTPPOSL),A	; store last pixel position
	LD	BC,(TEMPCOORDS)
	LD	(LASTXYL),BC
	LD	(LASTDFL),HL
	RET

; Module name: Draw patches of deep water
;
; Entry parameters: 
;	HL-> displayfile (U)
;	E = primitive (P)
;	C = height (P)
;	B = pixel position (P)
;
; Exit parameters: 
;	none.
;
WATERBR	LD	A,11111111B
	JP	WATERGAFFR
WATERR	LD	A,10101010B
WATERGAFFR	LD	(WALPATTERN),A
	LD	A,(XOFFSET+1)
	OR	A
	RET	NZ
	LD	A,(LASTDFR+1)
	OR	A
	JP	Z,GRABPRAMSR	; exit now if it was out of screen
	PUSH	BC	; save original starting point
	PUSH	HL

	LD	A,(LASTDFR+1)
	LD	($101+1),A	; store hi-byte
	LD	A,L
	AND	1FH
	LD	C,A	; C is now offset
	LD	A,(LASTDFR)
	AND	0E0H
	OR	C
	LD	($101),A	; $101 now holds app. addr. for a downwards line

	LD	A,B
	EX	AF,AF'
	LD	BC,(TEMPCOORDS)
	LD	DE,(LASTXYR)
	LD	A,C	; dy=lasty-currenty
	SUB	2
	SUB	E
	LD	E,A

	LD	A,B	; dx=lastx-currentx
	SUB	D
	LD	D,A

	JP	NC,$1	; jump if drawing upwards

	XOR	A
	SUB	E	; invert Y
	LD	E,A
	LD	HL,(LASTDFR)
	LD	($101),HL
	LD	A,(LASTPPOSR)	; get parameters from the other point
	EX	AF,AF'
	JP	$0
$1	NEG
	LD	D,A
$0	LD	A,E	; Y delta
	LD	($100),A
	LD	E,A
	EX	AF,AF'	; get back pixel position
	ADD	>BITPOSNS2
	LD	C,A
	LD	B,<BITPOSNS2
	LD	A,D	; test for delta X being zero and
	OR	A	; don't draw a triangle if it is.
	LD	A,(BC)	; pixel mask

	CALL	NZ,LINEDRAW	; draw offending triangle if it's there

	LD	A,0
$100	EQU	$-1	; height of wall
	OR	A
	JP	P,$4
	NEG
$4	LD	C,A
	LD	HL,0
$101	EQU	$-2	; low byte of HL
	INC	L
	LD	A,L
	AND	1FH
 	JP	Z,$5
	SUB	20H
	NEG
	OR	A
	JP	Z,$5	; jump if zero
	JP	M,$5
	LD	($102),A	; this is the width
	ADD	20H
	LD	E,A
	LD	D,0
	LD	A,L
	OR	1FH
	LD	L,A
	LD	A,(WALPATTERN)	; fill with this
$2	LD	B,0
$102	EQU	$-1
$3	LD	(HL),A
	DEC	L
	DJNZ	$3
	ADD	HL,DE
	DEC	C
	JP	NZ,$2
$5	POP	HL
	POP	BC

; ------ Store info about the point chosen
GRABPRAMSR	LD	A,B
	LD	(LASTPPOSR),A	; store last pixel position
	LD	BC,(TEMPCOORDS)
	LD	(LASTXYR),BC
	LD	(LASTDFR),HL
	RET

; ------ Main IRQ entry point
INTERRUPT	DI
	PUSH	HL
	PUSH	AF
	LD	HL,TIMER
	DEC	(HL)
	JP	P,$1
	LD	(HL),44	; 1.136Hz (A bit faster than seconds) 
	INC	HL
	LD	A,(HL)
	SUB	1	; I wonder if DEC sets N...
	DAA		; decimal it must be!
	LD	(HL),A
	INC	HL
	DEC	(HL)	; set flag
$1	LD	A,191	; ENTER, i think
	IN	A,(KEYB_IO)
	RRA
	JP	NC,START	; restart
INT2	POP	AF
	POP	HL
	EI
	RETI

; ------ 16-bit signed divide HL by constant 80H, very fast
DIV128S	BIT	7,H
	JP	Z,DIV128U
	CALL	NEG16
	CALL	DIV128U
NEG16	AND	A	; negate 16-bit HL
	LD	DE,0
	EX	DE,HL
	SBC	HL,DE
	RET

; ------ 16-bit unsigned divide by 80H, very fast
DIV128U	LD	A,L
	RLA		; divide DE by 80H fast.
	LD	A,H
	RLA
	LD	L,A
	LD	H,0
	JP	NC,$1
	INC	H	; offending bit
$1	RET	

; Module name: Signed 16-bit / unsigned 8-bit to signed 16-bit multiply
;	    Got working at 15:57:04 on 08/12/87
; 
; Entry parameters: 
;	A (P) and BC (I) values to multiply
;
; Exit parameters: 
;	HL is result (I)
;

MUL16	EXX
	LD	HL,0	; HL' holds sum
	EXX
	LD	D,128	; start off with this bit
$1	LD	H,B
	LD	L,C	; get base
	OR	A	; multiply with zero?
	JP	Z,$6	; yes, jump and return
$5	SUB	D	; no, check that bit position
	JP	NC,$3	; jump if that was smaller
	ADD	D	; else add back
	SRL	D	; try next bit
	JP	$5
$3	SRL	D	; else prepare to multiply
	JP	C,$4	; multiply with one, so don't
	LD	E,D	; save D
$2	ADD	HL,HL	; add up until D rolls out
	SRL	D
	JP	NC,$2
$4	PUSH	HL
	EXX
	POP	BC	; fetch result in BC'
	ADD	HL,BC	; add to sum
	EXX
	LD	D,E	; retrieve D from E
	JP	$1	; go for next bit
$6	EXX		; HL is now result, as a 16-bit
	RET		; signed integer. Wasn't that quick?

;
; Module name:  Random number generator
;
; Entry parameters: 
;	none
;
; Exit parameters: 
;	A = new random number (P), (N)
;	and (SEED) is updated
;
; Notes:
;
; 4 billion (4,000,000,000) elements between sequence wraparound.
;
; Call SETSEED with seed initializer (<>0) and subsequently call RANDOM
; to obtain 8-bit random numbers. Seed 0 will produce infinite zeros
;

RANDOM	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	HL,(SEED)
	LD	DE,(SEED+2)
	LD	B,7
RND1	LD	A,L
	AND	48H
	ADD	38H
	SLA	A
	SLA	A
	RL	D
	RL	E
	RL	H
	RL	L
	DJNZ	RND1
	LD	(SEED),HL
	LD	(SEED+2),DE
	LD	A,L
	POP	BC
	POP	DE
	POP	HL
	RET
;
; Initialize SEED (32-bit) to the contents of A
;
SETSEED	PUSH	HL
	RLCA
	RLCA
	NEG
	LD	H,A
	LD	L,A
	LD	(SEED+2),HL
	LD	L,A
	LD	(SEED),HL
	CALL	RANDOM
	POP	HL
	RET

; The flag byte contains:-
;
; Bit	Meaning
; --------	----------------------------------------------
; 0-3	Character 0-14 (15 is end-of-list)
; 4	Vertically swapped
; 5	Horizontally swapped
; 7-8	0=left,1=right,2=up,3=down
;
; ------ Drawing road maps on top of the screen. HL-> appropriate map, DE=df address offset
DRAW_MAP	LD	(MAP_PTR),HL
	PUSH	DE
	EXX
	POP	DE
	EXX
	LD	A,D
	RRCA
	RRCA
	RRCA
	AND	3
	OR	58H
	LD	B,A
	LD	C,E
	LD	(MAP_DEST),BC

$20	LD	A,(HL)
	CP	0FFH
	RET	Z

	AND	0FH
	BIT	5,(HL)	; swapped?
	JR	Z,$0
	INC	A
$0	ADD	A
	ADD	A
	ADD	A	; multiply by 8

	EXX
	LD	C,A
	LD	B,0
	LD	HL,RDCHRBASE
	ADD	HL,BC
	EXX

	LD	A,(HL)

	EXX
	BIT	4,A	; horizontally swapped?
	JR	NZ,$10

	LD	B,8
	BIT	5,A
	JR	NZ,$3	; jump if it's mirror'd vertically

$1	LD	A,(HL)
	LD	(DE),A
	INC	D
	INC	HL
	DJNZ	$1
	JR	$2

$3	DEC	HL
	LD	A,(HL)
	LD	(DE),A
	INC	D
	DJNZ	$3

$2	LD	A,D
	SUB	8
	LD	D,A

	EXX
	LD	A,(HL)
	RLCA
	RLCA
	AND	3
	EXX
	ADD	>MOVMTAB
	LD	L,A
	LD	H,<MOVMTAB
	LD	A,(HL)
	ADD	E
	LD	E,A

	EXX
	INC	HL

	JR	$20

$10	BIT	5,A
	LD	B,REFTAB/100H
	LD	A,8
	JR	NZ,$13

$11	EX	AF,AF'
	LD	C,(HL)
	LD	A,(BC)
	LD	(DE),A
	INC	D
	INC	HL
	EX	AF,AF'
	DEC	A
	JR	NZ,$11
	JR	$2

$13	DEC	HL
	EX	AF,AF'
	LD	C,(HL)
	LD	A,(BC)
	LD	(DE),A
	INC	D
	EX	AF,AF'
	DEC	A
	JR	NZ,$13
	JR	$2

; Module name: Option drivers
;
; Entry parameters: 
;	
; Exit parameters: 
;	
SELECT	LD	(FLAGB1),A
	LD	IX,ATTRTAB
	LD	B,0	; no options yet
$0	LD	E,(HL)
	INC	HL
	LD	D,(HL)	; grab printing address
	LD	A,E
	OR	D
	INC	A
	JP	Z,$1	; jump if last option
	DEC	HL
	LD	A,D
	RRCA
	RRCA
	RRCA
	AND	03H
	OR	58H	; make attr address
	LD	(IX+0),E
	LD	(IX+1),A	; remember attr address
	CALL	TEXT	; write out text
	LD	(IX+2),C	; no of chars
	INC	IX
	INC	IX
	INC	IX
	INC	B	; 1 more option
	JR	$0

; ------ Select option
$1	LD	HL,ATTRTAB
	LD	D,B	; boundaries
	LD	E,1	; use option 1

$10	LD	A,(FLAGB1)
	OR	A
	JR	Z,$11

	PUSH	HL
	PUSH	DE
	LD	HL,582EH
	LD	BC,0E07H
	LD	E,P_YELLOW+I_RED
	CALL	BOX
	POP	DE
	POP	HL
	PUSH	HL
	PUSH	DE
	LD	HL,400DH
	LD	A,E
	CALL	DRAW_MAP_A
	POP	DE
	POP	HL

$11	LD	A,1
	LD	(TEMPB1),A
	CALL	CHOOSECOL
	CALL	FILL_DE_A

	LD	BC,6000H	; timeout on wait
$8	DEC	BC
	LD	A,B
	OR	C
	JR	NZ,$8

$2	CALL	CHOOSECOL
	CALL	FILL_DE_A

	JOYSTICK UP
	JR	Z,$3
	JOYSTICK DOWN
	JR	Z,$4
	JOYSTICK FIRE
	JR	Z,$5
	JR	$2	; no, boring.

$3	LD	A,E
	DEC	A
	JR	Z,$2
	LD	A,(BCKCOLOUR)
	CALL	FILL_DE_A
	DEC	HL
	DEC	HL
	DEC	HL
	DEC	E
	JR	$10
$4	LD	A,E
	CP	D
	JR	NC,$2
	LD	A,(BCKCOLOUR)
	CALL	FILL_DE_A
	INC	HL
	INC	HL
	INC	HL
	INC	E
	JP	$10

$5	LD	A,(BCKCOLOUR)
	CALL	FILL_DE_A
	LD	A,E	; selected option [1..n]
	RET

FILL_DE_A	PUSH	DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	B,(HL)
	DEC	HL
	DEC	HL
	AND	7
	LD	C,A
	LD	(TEMPW1),DE
$1	LD	A,(DE)
	AND	0F8H
	OR	C
	LD	(DE),A
	INC	E
	DJNZ	$1	
	POP	DE
	RET

; ------ HL=attraddr, e=attr, b=width, c=height
BOXA	PUSH	BC
	PUSH	HL
$4	LD	A,B
	LD	D,L
$1	LD	(HL),E
	INC	L
	DJNZ	$1
	LD	B,A
	LD	A,D
	ADD	20H
	LD	L,A
	JR	NC,$2
	INC	H
$2	DEC	C
	JP	NZ,$4
	POP	HL
	POP	BC
	RET

BOX	CALL	BOXA
BOXD	LD	A,H
	AND	3
	RLCA
	RLCA
	RLCA
	OR	40H
	LD	H,A
	RL	C
	RL	C
	RL	C	; x8 for pixels
$2	LD	A,B
	LD	E,L
$3	LD	(HL),0
	INC	L
	DJNZ	$3
	LD	B,A
	LD	L,E
	CALL	NXTROW
	DEC	C
	JR	NZ,$2
	RET

NXTROW	INC	H
	LD	A,H
	AND	7
	RET	NZ
	LD	A,L
	ADD	20H
	LD	L,A
	RET	C
	LD	A,H
	SUB	8
	LD	H,A
	RET

; ------ Stuff B characters of A at address HL
ROW_OF_A	LD	(DFADDR),HL
$1	PUSH	AF
	PUSH	BC
	CALL	VDU
	POP	BC
	POP	AF
	DJNZ	$1
	RET

; ------ Draws map A (1-5) at screen offset HL (starting byte)
DRAW_MAP_A	LD	(TEMPW1),HL
	DEC	A
	ADD	A
	ADD	A
	LD	L,A
	LD	H,0
	LD	DE,MAPTABLE
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	PUSH	DE
	LD	A,(DE)
	LD	DE,(TEMPW1)
	ADD	E
	LD	C,A
	LD	B,D
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	DE,HL
	ADD	HL,BC
	EX	DE,HL
	CALL	TEXT_ADR
	
	LD	A,(HL)
	LD	DE,(TEMPW1)
	ADD	E
	LD	E,A
	INC	HL

	CALL	TEXT_ADR

	POP	HL
	LD	DE,(TEMPW1)
	LD	A,(HL)
	INC	HL
	ADD	E
	LD	E,A
	JP	DRAW_MAP

; ------ Choose a colour
CHOOSECOL	HALT		; check keys
CHOOSECOLX	LD	A,(TEMPB1)
	INC	A
	LD	(TEMPB1),A
	RRA
	RRA
	AND	7
	ADD	>FOURCOLS
	LD	C,A
	LD	B,<FOURCOLS
	LD	A,(BC)
	RET
;
; ------ Define keys
DEFKEYS	CALL	WAITKEY
	JP	START

; ------ Save all object code to tape
SAVEGAME	LD	IX,5B00H
	LD	DE,9500H
	XOR	A
	CALL	04C2H
	JP	START

; ------ Turn screen black
TURN2BLACK	XOR	A	; zero is black
TURN2C	LD	C,A
	LD	A,(PRIMITIVE)
	DEC	A
	RET	NZ
	LD	A,C
	RRA
	RRA
	RRA
	AND	7
	OUT	(BORD_IO),A
	LD	(BCKCOLOUR),A
	LD	HL,5800H
$0	LD	A,(HL)
	AND	7
	OR	C
	OR	40H
	LD	(HL),A
	DEC	L
	JP	NZ,$0
	LD	B,18H	
	RES	6,C
$1	LD	(HL),C
	LD	A,L
	ADD	1FH
	LD	L,A
	LD	(HL),C
	INC	HL
	DJNZ	$1
	RET

; ------ Turn screen yellow
TURN2YELLI	LD	A,P_YELLOW
	JR	TURN2C

; Module name:  Print score with various digits
;
; Entry parameters: 
;	DE = display-file address
;	HL ->number to print (LSB)
;	A  = Bytes to print, ie BCD digits / 2
;
DECIMAL	LD	(DFADDR),DE
	LD	E,0
	ADD	A
	LD	B,A
$0	LD	A,(HL)
	PUSH	AF
	REPEAT 4
	RRCA
	CALL	$1
	POP	AF
	DEC	B
	CALL	$1
	DEC	HL
	DJNZ	$0
	RET
$1	AND	0FH
	JR	NZ,$2
	BIT	0,E
	JR	NZ,$2
	LD	A,B
	DEC	A
	JR	Z,$2
	LD	A," "
	JR	$3
$2	LD	E,0FFH
	ADD	"0"
$3	EXX
	CALL	VDU
	EXX
	RET

; ------ Add to score
ADDSCORE	LD	HL,SCORE
ADD3	LD	A,(HL)
	ADD	C
	DAA
	LD	(HL),A
	INC	HL
	LD	A,(HL)
	ADC	B
	DAA
	LD	(HL),A
	RET	NC
	INC	HL
	LD	A,(HL)
	INC	A
	DAA
	LD	(HL),A
	RET	
;
; Module name: Fast calculation of a row of primitive points.
;
; Entry parameters: 
;	(TRACK_PTR) = position on the track to indicate bends, hills etc.
;	IX = pointer to coordinate destination data
;
; Note: This routine will calculate the primitive points (no fraction) of a
; set of primitives at the position on the track indicated in (TRACK_PTR).
; The points, two sets for the left and right side of the track, are stored
; in two tables, each 32 bytes long, each element holding X,Y on-screen for
; that primitive. By calling this routine twice for a position, a fractional
; part can easily be found by 'drawing' a straight line between the points.

FAST_ROW	LD	DE,0040H	; add 40 hex to make indexing reach
	ADD	IX,DE
	PUSH	IX
	POP	DE	; destination for YCALC
	PUSH	DE
	CALL	YCALC	; set down all Y values here
	POP	IX
	LD	DE,0020H
	ADD	IX,DE
	EXX
	LD	BC,(TRACK_PTR)
	LD	HL,128
	EXX
	LD	IY,OFFSETS	; if buggy has moved!
	LD	HL,XPOSNS+16*WIDTH	; max width for outer track
	LD	B,16	; counter for the angles

; ------ Now do this for all sixteen angles
$1	PUSH	BC
	PUSH	HL	; Hold this (it's gettin' modified!)

; ------ Physical track motion, or how to 'make the fake'. ( Take the cake )
	EXX
	LD	A,(BC)	; get new track movement
	INC	C
	LD	D,0
	OR	A
	JP	P,$2	; find out wheter movement is + or -
	DEC	D	; Make hi-byte minus 1 if negative
$2	LD	E,A
 IF HORIZCH
	ADD	HL,DE	; and so subtract 16-bit +ve/-ve
 ENDIF
	PUSH	HL
	PUSH	HL
	PUSH	HL
	PUSH	HL
	EXX		; This is new middle

; ------ Find both lines' qualities
	LD	E,(HL)	; E holds X pos. for outer line
	LD	A,L
	SUB	WIDTH2
	LD	L,A
	LD	D,(HL)	; D holds X pos. for inner line
	LD	C,(IY+0)
	LD	B,(IY+1)	; BC holds buggy's position (offset)

; ------ Find the right-hand coordinate for the OUTER line 
	POP	HL	; 'middle' of track for this Y
	LD	A,D	; save D
	LD	D,0	; make DE offset for outer line
	ADD	HL,DE	; add line offset to middle of track
	ADD	HL,BC	; add buggys position to that
	LD	(IX-060H),L	; insert right
	LD	(IX-05FH),H	; coordinate into here

; ------ Find the left-hand coordinate for the OUTER line
	POP	HL	; 'middle' of track again
	AND	A	; clear carry only, save a
	SBC	HL,DE	; this time subtract the line width
	ADD	HL,BC	; but still add on the buggy's pos.
	LD	(IX-040H),L	; insert left
	LD	(IX-03FH),H	; coordinate into here

; ------ Find the right-hand coordinate for the INNER line
	POP	HL	; 'middle'
	LD	E,A	; DE must become offset for inner
	LD	D,0	
	ADD	HL,DE	; offset the width
	ADD	HL,BC	; and the buggy
	LD	(IX+040H),L
	LD	(IX+041H),H	; coordinate into here

; ------ Find the left-hand coordinate for the INNER line
	POP	HL	; middle
	AND	A
	SBC	HL,DE	; offset width
	ADD	HL,BC	; and buggy
	LD	(IX+060H),L	; insert coordinate
	LD	(IX+061H),H

; ------ That's one primitive done
	POP	HL	; Pointer to X data
	INC	HL	; next X coord
	INC	IX
	INC	IX
	INC	IY
	INC	IY
	POP	BC
	DJNZ	$1
	RET

; ------ Convert to decimal
BINTODEC	LD	B,100
	CALL	$0
	LD	H,C
	LD	B,10
	CALL	$0
	RLC	C
	RLC	C
	RLC	C
	RLC	C
	ADD	C
	LD	L,A
	RET
$0	LD	C,0FFH
$1	INC	C
	SUB	B
	JR	NC,$1
	ADD	B
	RET

; ------ Draw up the mountains in the distance
PARAMOUNT	LD	A,(JUMPOFFSET)
	ADD	DF_PSEUDO/100H+5
	LD	D,A
	LD	E,0
	EXX
	LD	A,(MOUNTVALUE)
	LD	C,A
	AND	3	; Low 2 bits measure rotation
	XOR	3
	ADD	ROTTAB/100H
	LD	(HIBYTE),A
	SRL	C
	SRL	C
	LD	B,0
	LD	HL,MOUNTAINS
	ADD	HL,BC
	ADD	HL,BC
	LD	B,20H

$0	PUSH	BC
	LD	A,(HL)	; character
	EXX
	PUSH	DE
	CALL	ONE_CHR
	EXX
	INC	HL
	LD	A,(HL)
	EXX
	CALL	ONE_CHR
	POP	DE
	INC	E
	EXX
	POP	BC
	INC	C
	INC	HL	; next character
	BIT	6,C
	JR	Z,$2	; outside?
	LD	C,0	; clear Counter
	LD	HL,MOUNTAINS
$2	DJNZ	$0
	RET

ONE_CHR	INC	D	; assume bad character
	ADD	A
	RET	Z	; no character - good assumption
	DEC	D	; bad assumption - back again
	ADD	A	; offset
	LD	L,A
	LD	H,0
	ADD	HL,HL
	LD	BC,HILLDATA-8	; 1 character less, so offset fits #1
	ADD	HL,BC	; actual character
	LD	A,8
	LD	B,0
HIBYTE	EQU	$-1
$1	EX	AF,AF'
	LD	C,(HL)
	SRL	C
	LD	A,(BC)
	LD	C,A
	LD	A,(DE)
	OR	C
	LD	(DE),A
	LD	C,(HL)
	SET	7,C
	INC	E
	LD	A,(BC)
	LD	(DE),A
	DEC	E
	INC	HL
	LD	A,E
	ADD	20H
	LD	E,A
	JR	NC,$7
	INC	D
$7	EX	AF,AF'
	DEC	A
	JP	NZ,$1
	RET


;
; Module name: Line drawing / triangle drawing with a right angle on the axae
;
; Entry parameters: 
;	HL-> display file (U)
;	A  = pixel mask (P)
;	E  = deltaY (N)
;	D  = deltaX (N)
;	C  = initial width of thing (P)
;
; Exit parameters: 
;	none
;
LINEDRAWX	PUSH	DE
	EX	AF,AF'
	LD	A,C
	ADD	1FH
	LD	E,A
	LD	D,0
	JP	LINEDRAW1
LINEDRAW	PUSH	DE	; initiate transfer of the deltas
	LD	DE,20H
	LD	C,1	; C holds no. of bytes to fill at
	EX	AF,AF'
LINEDRAW1	EXX		; a new line (+1)
	LD	A,(WALPATTERN)
	LD	C,A
	LD	HL,WALPATS
	LD	B,13	; 13 values to modify
$3	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	DE,HL
	CP	(HL)
	JP	Z,$4
	LD	(HL),A
	EX	DE,HL
	DJNZ	$3
$4	POP	DE	; D'E' now holds X and Y respectvely
	LD	A,E	; test deltaX's sign
	OR	A
	JP	P,$0	; if positive, just keep as is
	NEG
	LD	E,A	; else negate it
	EXX
	LD	A,E	; also negate this
	NEG
	LD	E,A
	LD	D,255
	EXX
$0	LD	A,D	; this is DeltaX (signed)
	OR	A
	JP	P,$1	; make sure it's positive
	NEG
$1	LD	C,A	; c=abs(DeltaX)
	LD	A,E	; Do the same for DeltaY
	OR	A
	JP	P,$2
	NEG
$2	CP	C	; abs(DeltaY)-abs(DeltaX)
	JP	C,DXDY	; jump if dX>dY>0 applies
DYDX	LD	A,D	; deltax
	OR	A	; is it negative?
	JP	M,LINEL	; yes, draw line to the left
LINER	LD	A,D	; e=deltax
	LD	B,E	; counter=deltay
	SLA	E	; deltay=2*deltay
	SLA	D	; deltax=2*deltax
	SUB	E	; e=deltax-2*deltay
	JP	LINER3
LINER0	EX	AF,AF'	; hold e in a'
	EXX
	ADD	HL,DE
	LD	B,C	; B holds no. of lines to fill in +1
	JP	LINER4	; skip 1st
LINER5	LD	(HL),0	; fill byte - change to whatever
LINEMOD1	EQU	$-1
	INC	L
LINER4	DJNZ	LINER5
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD2	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	EXX
	EX	AF,AF'
LINER3	JP	M,LINER1	; only stay if larger than zero
	JP	Z,LINER1
	ADD	D	; D is deltaX
	SUB	E	; if e>0 e=e-(2*deltax-2*deltay)
	EX	AF,AF'
	SCF
	RRA		; move image right
	JP	NC,LINER2	; byte boundary?
	LD	A,80H
	EXX
	INC	L	; next byte now
	DEC	E	; 1 less to add MODIFY HERE
	INC	C	; 1 more to fill
	EXX
LINER2	EX	AF,AF'
	DJNZ	LINER0	; keep going until Y has reached limit
	RET
LINER1	ADD	D	; e=e+2*deltay
	DJNZ	LINER0
	RET

LINEL	NEG		; negate deltaX so make it positive
	LD	D,A	; deltax
	LD	B,E	; deltay
	SLA	E	; 2*deltay
	SLA	D	; 2*deltax
	SUB	E	; e=deltax-2*deltay
	JP	LINEL3
LINEL0	EX	AF,AF'	; hold e in a'
	EXX
	ADD	HL,DE	; go to next/previous line
	LD	B,C	; this is the number of offset bytes+1
	JP	LINEL4	; jump to avoid 1st if there aren't any
LINEL5	LD	(HL),0
LINEMOD3	EQU	$-1
	DEC	L	; fill in a byte
LINEL4	DJNZ	LINEL5	; repeat until current-1 is reached
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD4	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	EXX
	EX	AF,AF'
LINEL3	JP	M,LINEL1	; (start here) - is A negative
	JP	Z,LINEL1
	ADD	D	; D is deltaX
	SUB	E	; if e>0 e=e-(2*deltax-2*deltay)
	EX	AF,AF'
	SCF
	RLA		; rotate another bit in
	JP	NC,LINEL2
	LD	A,01H	; this is what a 'fresh' byte looks like
	EXX
	DEC	L	; one back
	INC	E	; also 1 less to add (C+E=width)
	INC	C	; but 1 more to fill in
	EXX
LINEL2	EX	AF,AF'
	DJNZ	LINEL0
	RET
LINEL1	ADD	D	; if (e<=0) e+=2*deltay;
	DJNZ	LINEL0
	RET

; ------ This is for the case DX>DY>0
DXDY	LD	A,D	; deltax
	OR	A
	JP	M,LINEV	; Left hand side line?
LINEH	LD	B,D	; this is horiz. pixel count
	LD	A,E	; This is DeltaY
	SLA	D
	SLA	E
	SUB	D	; DeltaY-2*DeltaX is initial e
	JP	LINEH3
LINEH0	EX	AF,AF'
	EXX
	SCF
	RRA		; grab a bit more
	JP	NC,LINEH1
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD5	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	INC	L	; next byte ahead
	DEC	E	; 1 less to add
	INC	C	; 1 more to fill in
	LD	A,80H
LINEH1	EXX
	EX	AF,AF'
LINEH3	JP	M,LINEH2
	JP	Z,LINEH2
	ADD	E	; correct the error if it was positive
	SUB	D
	EX	AF,AF'
	DJNZ	LINEH6
	JP	LINEH10	; insert last bit & return
LINEH6	EXX
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD6	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	ADD	HL,DE	; next/previous line now
	LD	B,C
	JP	LINEH4
LINEH5 	LD	(HL),0
LINEMOD7	EQU	$-1
	INC	L
LINEH4	DJNZ	LINEH5
	EXX
	EX	AF,AF'
	JP	LINEH0
LINEH2	ADD	E	; add 2*deltax to e if it is still 
	DJNZ	LINEH0	; negative and go again for another
	EX	AF,AF'
LINEH10	EXX
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD8	EQU	$-1
	LD	(HL),A	; now insert pixel
	RET
LINEV	NEG
	LD	D,A
	LD	B,D	; this is horiz. pixel count
	LD	A,E
	SLA	D
	SLA	E
	SUB	D
	JP	LINEV3
LINEV0	EX	AF,AF'
	EXX
	SCF
	RLA		; grab a bit more
	JP	NC,LINEV1
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD9	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	DEC	L	; next byte ahead
	INC	E	; 1 less to add
	INC	C	; 1 more to fill in
	LD	A,01H
LINEV1	EXX
	EX	AF,AF'
LINEV3	JP	M,LINEV2
	JP	Z,LINEV2
	ADD	E
	SUB	D
	EX	AF,AF'
	DJNZ	LINEV6
	EXX
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD10	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	RET
LINEV6	EXX
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD11	EQU	$-1
	LD	(HL),A	; now insert pixel
	LD	A,B
	ADD	HL,DE	; next line now
	LD	B,C
	JP	LINEV4	; B is fill factor +1, so skip 1st
LINEV5 	LD	(HL),0	; fill in to current position - 1
LINEMOD12	EQU	$-1
	DEC	L
LINEV4	DJNZ	LINEV5
	EXX
	EX	AF,AF'
	JP	LINEV0
LINEV2	ADD	E	; if e is still negative, add 2*deltay
	DJNZ	LINEV0
	EX	AF,AF'
	EXX
	LD	B,A
	OR	(HL)
	AND	0
LINEMOD13	EQU	$-1	
	LD	(HL),A	; now insert pixel
	LD	A,B
	RET

	SUBTTL "Object data tables
;
; Module name: Object data tables
;
; Usage: This table contains, for every physical object in the game, a
; dimension set (x,y) and an address where the object can be found.
; Note that the physical object names, BB_GD_#nn, are the same as defined
; in the program documentation (BB_DOC_#1)

OBJECTDATA

BB_GD_#1  	DB	0,0
	DW	0
BB_GD_#2  	DB	64,0
	DW	0
BB_GD_#3  	DB	16,32
	DW	PALMTREES
BB_GD_#4  	DB	21,43
	DW	PALMTREEM
BB_GD_#5  	DB	27,55
	DW	PALMTREEL
BB_GD_#6  	DB	16,32
	DW	BEECHTREES
BB_GD_#7  	DB	21,43
	DW	BEECHTREEM
BB_GD_#8  	DB	27,55
	DW	BEECHTREEL
BB_GD_#9  	DB	16,32
	DW	PINETREES
BB_GD_#10 	DB	21,43
	DW	PINETREEM
BB_GD_#11 	DB	27,55
	DW	PINETREEL
BB_GD_#12 	DB	24,7
	DW	LOGS
BB_GD_#13 	DB	32,10
	DW	LOGM
BB_GD_#14 	DB	40,12
	DW	LOGL
BB_GD_#15 	DB	24,24
	DW	BOULDERS
BB_GD_#16 	DB	32,32
	DW	BOULDERM
BB_GD_#17 	DB	40,40
	DW	BOULDERL
BB_GD_#18 	DB	24,19
	DW	FENCES
BB_GD_#19 	DB	32,24
	DW	FENCEM
BB_GD_#20 	DB	40,32
	DW	FENCEL
BB_GD_#21 	DB	32,12
	DW	TIMES
BB_GD_#22 	DB	43,16
	DW	TIMEM
BB_GD_#23 	DB	53,20
	DW	TIMEL
BB_GD_#24 	DB	32,12
	DW	P100S
BB_GD_#25 	DB	43,16
	DW	P100M
BB_GD_#26 	DB	53,20
	DW	P100L
BB_GD_#27 	DB	32,12
	DW	P250S
BB_GD_#28 	DB	43,16
	DW	P250M
BB_GD_#29 	DB	53,20
	DW	P250L
BB_GD_#30 	DB	32,12
	DW	P500S
BB_GD_#31 	DB	43,16
	DW	P500M
BB_GD_#32 	DB	53,20
	DW	P500L
BB_GD_#33 	DB	16,24
	DW	FLAG1S
BB_GD_#34 	DB	21,32
	DW	FLAG1M
BB_GD_#35 	DB	27,40
	DW	FLAG1L
BB_GD_#36 	DB	16,24
	DW	FLAG2S
BB_GD_#37 	DB	21,32
	DW	FLAG2M
BB_GD_#38	DB	27,40
	DW	FLAG2L
ABB_GD_#39	DB	10,4
	DW	ROCKS
BB_GD_#40 	DB	12,6
	DW	ROCKM
BB_GD_#41 	DB	16,8
	DW	ROCKL
BB_GD_#42 	DB	12,16
	DW	BARRELS
BB_GD_#43 	DB	16,21
	DW	BARRELM
BB_GD_#44 	DB	20,26
	DW	BARRELL
BB_GD_#45 	DB	24,24
	DW	SHRUBS
BB_GD_#46 	DB	32,32
	DW	SHRUBM
BB_GD_#47 	DB	40,40
	DW	SHRUBL
BB_GD_#48 	DB	0,0	; the buggies are not initialised here
	DW	0
BB_GD_#49 	DB	0,0
	DW	0
BB_GD_#50 	DB	0,0
	DW	0
BB_GD_#51 	DB	0,0
	DW	0
BB_GD_#52 	DB	0,0
	DW	0
BB_GD_#53 	DB	0,0
	DW	0
BB_GD_#54 	DB	0,0
	DW	0
BB_GD_#55 	DB	0,0
	DW	0
BB_GD_#56 	DB	0,0
	DW	0
BB_GD_#57 	DB	0,0
	DW	0
BB_GD_#58 	DB	0,0
	DW	0
BB_GD_#59 	DB	0,0
	DW	0
BB_GD_#60 	DB	0,0
	DW	0
BB_GD_#61 	DB	0,0
	DW	0
BB_GD_#62 	DB	0,0
	DW	0
BB_GD_#63 	DB	0,0
	DW	0
BB_GD_#64	DB	0,0
	DW	0
BB_GD_#65	DB	0,0
	DW	0
BB_GD_#66	DB	0,0
	DW	0
BB_GD_#67	DB	0,0
	DW	0
BB_GD_#68	DB	0,0
	DW	0
BB_GD_#69	DB	0,0
	DW	0
BB_GD_#70	DB	0,0
	DW	0
BB_GD_#71	DB	0,0
	DW	0
BB_GD_#72	DB	0,0
	DW	0
BB_GD_#73	DB	0,0
	DW	0
BB_GD_#74	DB	0,0
	DW	0
BB_GD_#75	DB	0,0
	DW	0
BB_GD_#76	DB	24,19
	DW	BWALLS
BB_GD_#77	DB	32,24
	DW	BWALLM
BB_GD_#78	DB	40,32
	DW	BWALLL
BB_GD_#79	DB	0,0
	DW	0
BB_GD_#80	DB	0,0
	DW	0
BB_GD_#81	DB	0,0
	DW	0
BB_GD_#82	DB	0,0
	DW	0
BB_GD_#83	DB	0,0
	DW	0
BB_GD_#84	DB	0,0
	DW	0
BB_GD_#85	DB	0,0
	DW	0
BB_GD_#86	DB	0,0
	DW	0
BB_GD_#87	DB	0,0
	DW	0
BB_GD_#88	DB	0,0
	DW	0
BB_GD_#89	DB	0,0
	DW	0
BB_GD_#90	DB	0,0
	DW	0
BB_GD_#91	DB	64,16
	DW	STARTS
BB_GD_#92	DB	85,22
	DW	STARTM
BB_GD_#93	DB	106,27
	DW	STARTL

; ------ Table of magnifications and different images to use acc. to primitive
IMAGETABLE	DB	2,2	; Image index offset 0-2, magnf. 0-2
	DB	1,2
	DB	0,2
	DB	2,1
	DB	1,1
	DB	0,1
	DB	2,0
	DB	1,0
	DB	0,0

;	Trigonometry tables:
;
;	          1      m			                     
;	 y(m) = -----   ---	 																						
;	        tan a    c																								
;	        																								
; For each m is a table of 16 bytes, for each a is a byte,	 
; 16*m+a. a ranges from 1 to 16 degrees viewpoint->road.    
; The values correspond to the horizon values of 50 to 150  
; inclusive, so the first is for a horizon of 50.											
;
; ------ Horizon arc-tangens tables
YPOSNS:	HEX 0F161B1F222527282A2C2D2E2F303132
	HEX 0F171C20232527292B2C2E2F30313233
	HEX 10171C202326282A2C2D2F3031323334
	HEX 10181D212427292B2D2E303132333435
	HEX 10181E2225272A2C2E2F313233343536
	HEX 10191E2225282B2D2E30313334353637
	HEX 11191F2326292B2D2F31323435363738
	HEX 111A1F23272A2C2E3032333536373839
	HEX 111A2024272A2D2F313334363738393A
	HEX 121A2025282B2E303233353638393A3B
	HEX 121B2125292C2E3133343637393A3B3C
	HEX 121B21262A2D2F31333537383A3B3C3D
	HEX 121C22272A2D3032343638393B3C3D3E
	HEX 131C22272B2E31333537393A3C3D3E3F
	HEX 131D23282C2F313436383A3B3D3E3F40
	HEX 131D24282C2F323537393A3C3E3F4041
	HEX 141E24292D303335383A3B3D3E404142
	HEX 141E252A2E313436383A3C3E3F414243
	HEX 141E252A2E323537393B3D3F40424344
	HEX 151F262B2F3235383A3C3E4041434445
	HEX 151F262B303336393B3D3F4142444546
	HEX 1520272C3034373A3C3E404243454647
	HEX 1520272D3135383A3D3F414344464748
	HEX 1621282D3235383B3E40424345474849
	HEX 1621282E3236393C3E4143444648494A
	HEX 1622292F33373A3D3F41434547494A4B
	HEX 17222A2F34383B3E40424446484A4B4C
	HEX 17222A3034383C3E41434547494A4C4D
	HEX 17232B3035393C3F424446484A4B4D4E
	HEX 18232B31363A3D40434547494B4C4E4F
	HEX 18242C32363A3E414346484A4C4D4F50
	HEX 18242C32373B3F424447494B4D4E5051
	HEX 18252D33383C3F4245484A4C4E4F5152
	HEX 19252D34393D404346484B4D4F505253
	HEX 19262E34393D414447494C4E4F515354
	HEX 19262E353A3E4245484A4C4F50525455
	HEX 1A262F353B3F4246484B4D4F51535556
	HEX 1A2730363B404346494C4E5052545657
	HEX 1A2730373C4044474A4D4F5153555758
	HEX 1B2831373D4145484B4E505254565859
	HEX 1B2831383D4246494C4F51535557595A
	HEX 1B2932393E42464A4D4F525456585A5B
	HEX 1B2932393F43474B4E50535557595B5C
	HEX 1C2A333A3F44484B4E515456585A5C5D
	HEX 1C2A333A4045494C4F525557595B5D5E
	HEX 1C2B343B4145494D505355585A5C5E5F
	HEX 1D2B353C41464A4E515456595B5D5F60
	HEX 1D2B353C42474B4F5255575A5C5E6061
	HEX 1D2C363D43484C4F5355585B5D5F6162
	HEX 1E2C363E43484D505356595B5E606263
	HEX 1E2D373E44494D5154575A5C5F616364
	HEX 1E2D373F454A4E5255585B5D60626465
	HEX 1E2E383F454B4F5356595C5E61636566
	HEX 1F2E3840464B5053575A5D5F61646667
	HEX 1F2F3941474C5054585B5E6062656768
	HEX 1F2F3941474D5155585C5E6163666869
	HEX 202F3A42484D5256595C5F626467696B
	HEX 20303B42494E53575A5D606365676A6C
	HEX 20303B434A4F53575B5E616466686B6D
	HEX 21313C444A5054585C5F626567696C6E
	HEX 21313C444B5055595D606366686A6D6F
	HEX 21323D454C51565A5E616467696B6E70
	HEX 21323D464C52575B5E6265676A6C6E71
	HEX 22333E464D53575C5F6366686B6D6F72
	HEX 22333E474E53585C606367696C6E7073
	HEX 22333F474E54595D6164676A6D6F7174
	HEX 23343F484F555A5E6265686B6E707275
	HEX 2334404950555A5F6366696C6F717376
	HEX 2335414950565B6063676A6D70727477
	HEX 2435414A51575C6064686B6E71737578
	HEX 2436424B52585D6165696C6F72747679
	HEX 2436424B52585E62666A6D707275777A
	HEX 2437434C53595E63676A6E717376787B
	HEX 2537434C545A5F64686B6F727477797C
	HEX 2537444D545B6064696C6F7375787A7D
	HEX 2538444E555B6165696D707376797B7E
	HEX 2638454E565C61666A6E7174777A7C7F
	HEX 2639454F565D62676B6F7275787B7D80
	HEX 26394650575E63686C707376797C7E81
	HEX 263A4750585E64686D7174777A7D7F82
	HEX 273A4751595F64696E7175787B7E8083
	HEX 273B48515960656A6E7276797C7F8184
	HEX 273B48525A60666B6F73777A7D808285
	HEX 283C49535B61676C7074787B7E818386
	HEX 283C49535B62686D7175787C7F828487
	HEX 283C4A545C63686D7276797D80838588
	HEX 293D4A555D63696E73777A7E81848689
	HEX 293D4B555D646A6F73787B7F8284878A
	HEX 293E4B565E656B7074787C7F8385888B
	HEX 293E4C565F666B7175797D808486898C
	HEX 2A3F4D575F666C71767A7E8184878A8D
	HEX 2A3F4D5860676D72777B7F8285888B8E
	HEX 2A404E5861686E73787C808386898C8F
	HEX 2B404E5961686F74797D8184878A8D90
	HEX 2B404F5962696F75797E8185888B8E91
	HEX 2B414F5A636A70757A7E8286898C8F92
	HEX 2C41505B636B71767B7F83878A8D9093
	HEX 2C42505B646B72777C8084888B8E9194
	HEX 2C42515C656C72787D8185898C8F9295
	HEX 2C43515D656D73797E82868A8D909396
	HEX 2D43525D666E74797E83878B8E919497

	BOUNDARY 100H	; align to next page boundary

TRACK	DS	100H	; 256 bytes hold the track

; ------ Odd Rotation tables, One pixel
ROTTAB	HEX 000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F
	HEX 202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F
	HEX 404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F
	HEX 606162636465666768696A6B6C6D6E6F707172737475767778797A7B7C7D7E7F
	HEX 0080008000800080008000800080008000800080008000800080008000800080
	HEX 0080008000800080008000800080008000800080008000800080008000800080
	HEX 0080008000800080008000800080008000800080008000800080008000800080
	HEX 0080008000800080008000800080008000800080008000800080008000800080

; ------ Three pixels
	HEX 0000000001010101020202020303030304040404050505050606060607070707
	HEX 08080808090909090A0A0A0A0B0B0B0B0C0C0C0C0D0D0D0D0E0E0E0E0F0F0F0F
	HEX 1010101011111111121212121313131314141414151515151616161617171717
	HEX 18181818191919191A1A1A1A1B1B1B1B1C1C1C1C1D1D1D1D1E1E1E1E1F1F1F1F
	HEX 0020406080A0C0E00020406080A0C0E00020406080A0C0E00020406080A0C0E0
	HEX 0020406080A0C0E00020406080A0C0E00020406080A0C0E00020406080A0C0E0
	HEX 0020406080A0C0E00020406080A0C0E00020406080A0C0E00020406080A0C0E0
	HEX 0020406080A0C0E00020406080A0C0E00020406080A0C0E00020406080A0C0E0

; ------ Five pixels
	HEX 0000000000000000000000000000000001010101010101010101010101010101
	HEX 0202020202020202020202020202020203030303030303030303030303030303
	HEX 0404040404040404040404040404040405050505050505050505050505050505
	HEX 0606060606060606060606060606060607070707070707070707070707070707
	HEX 0008101820283038404850586068707880889098A0A8B0B8C0C8D0D8E0E8F0F8
	HEX 0008101820283038404850586068707880889098A0A8B0B8C0C8D0D8E0E8F0F8
	HEX 0008101820283038404850586068707880889098A0A8B0B8C0C8D0D8E0E8F0F8
	HEX 0008101820283038404850586068707880889098A0A8B0B8C0C8D0D8E0E8F0F8

; ------ Seven pixels
	HEX 0000000000000000000000000000000000000000000000000000000000000000
	HEX 0000000000000000000000000000000000000000000000000000000000000000
	HEX 0101010101010101010101010101010101010101010101010101010101010101
	HEX 0101010101010101010101010101010101010101010101010101010101010101
	HEX 00020406080A0C0E10121416181A1C1E20222426282A2C2E30323436383A3C3E
	HEX 40424446484A4C4E50525456585A5C5E60626466686A6C6E70727476787A7C7E
	HEX 80828486888A8C8E90929496989A9C9EA0A2A4A6A8AAACAEB0B2B4B6B8BABCBE
	HEX C0C2C4C6C8CACCCED0D2D4D6D8DADCDEE0E2E4E6E8EAECEEF0F2F4F6F8FAFCFE

; ------ Table of reflections - index with LSB to find mirror image
REFTAB	HEX 008040C020A060E0109050D030B070F0088848C828A868E8189858D838B878F8
	HEX 048444C424A464E4149454D434B474F40C8C4CCC2CAC6CEC1C9C5CDC3CBC7CFC
	HEX 028242C222A262E2129252D232B272F20A8A4ACA2AAA6AEA1A9A5ADA3ABA7AFA
	HEX 068646C626A666E6169656D636B676F60E8E4ECE2EAE6EEE1E9E5EDE3EBE7EFE
	HEX 018141C121A161E1119151D131B171F1098949C929A969E9199959D939B979F9
	HEX 058545C525A565E5159555D535B575F50D8D4DCD2DAD6DED1D9D5DDD3DBD7DFD
	HEX 038343C323A363E3139353D333B373F30B8B4BCB2BAB6BEB1B9B5BDB3BBB7BFB
	HEX 078747C727A767E7179757D737B777F70F8F4FCF2FAF6FEF1F9F5FDF3FBF7FFF

; ------ Table of masks - index with graphic and get mask (it works)
MSKTAB	HEX FFFCF8F8F1F0F0F0E3E0E0E0E1E0E0E0C7C4C0C0C1C0C0C0C3C0C0C0C1C0C0C0
	HEX 8F8C888881808080838080808180808087848080818080808380808081808080
	HEX 1F1C181811101010030000000100000007040000010000000300000001000000
	HEX 0F0C080801000000030000000100000007040000010000000300000001000000
	HEX 3F3C383831303030232020202120202007040000010000000300000001000000
	HEX 0F0C080801000000030000000100000007040000010000000300000001000000
	HEX 1F1C181811101010030000000100000007040000010000000300000001000000
	HEX 0F0C080801000000030000000100000007040000010000000300000001000000

; ------ Table of magnifications, two pages
MAGTAB	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 00030C0F30333C3FC0C3CCCFF0F3FCFF00030C0F30333C3FC0C3CCCFF0F3FCFF
	HEX 0000000000000000000000000000000003030303030303030303030303030303
	HEX 0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F
	HEX 3030303030303030303030303030303033333333333333333333333333333333
	HEX 3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F
	HEX C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3C3
	HEX CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCF
	HEX F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3
	HEX FCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

; ------ "Midgets in Diminuendo"
SHRTAB	HEX 0000101000001010202030302020303000001010000010102020303020203030
	HEX 4040505040405050606070706060707040405050404050506060707060607070
	HEX 0000101000001010202030302020303000001010000010102020303020203030
	HEX 4040505040405050606070706060707040405050404050506060707060607070
	HEX 8080909080809090A0A0B0B0A0A0B0B08080909080809090A0A0B0B0A0A0B0B0
	HEX C0C0D0D0C0C0D0D0E0E0F0F0E0E0F0F0C0C0D0D0C0C0D0D0E0E0F0F0E0E0F0F0
	HEX 8080909080809090A0A0B0B0A0A0B0B08080909080809090A0A0B0B0A0A0B0B0
	HEX C0C0D0D0C0C0D0D0E0E0F0F0E0E0F0F0C0C0D0D0C0C0D0D0E0E0F0F0E0E0F0F0
 
; ------ Table of fractions, each is n/32 of a delta y or x
FRACTIONS	HEX 0000000000000000000000000000000000000000000000000000000000000000
	HEX 0000000000000000000000000000000101010101010101010101010101010101
	HEX 0000000000000001010101010101010101010101010101020202020202020202
	HEX 0000000000010101010101010101010202020202020202020202030303030303
	HEX 0000000101010101010101020202020202020203030303030303030404040404
	HEX 0000000101010101010202020202020303030303030304040404040405050505
	HEX 0000010101010102020202020203030303030404040404050505050505060606
	HEX 0000010101010202020202030303030404040404050505050506060606070707
	HEX 0001010101020202020303030304040404050505050606060607070707080808
	HEX 0001010101020202030303030404040505050506060606070707080808080909
	HEX 0001010102020203030303040404050505060606070707080808080909090A0A
	HEX 0001010102020203030304040405050606060707070808080909090A0A0A0B0B
	HEX 000101020202030303040405050506060607070808080909090A0A0B0B0B0C0C
	HEX 00010102020203030404040505060607070708080909090A0A0B0B0B0C0C0D0D
	HEX 0001010202030304040405050606070707080809090A0A0B0B0B0C0C0D0D0E0E
	HEX 00010102020303040405050606070708080809090A0A0B0B0C0C0D0D0E0E0F0F
	HEX 0101020203030404050506060707080809090A0A0B0B0C0C0D0D0E0E0F0F1010
	HEX 01010202030304040505060607070809090A0A0B0B0C0C0D0D0E0E0F0F101011
	HEX 010102020303040505060607070808090A0A0B0B0C0C0D0E0E0F0F1010111112
	HEX 0101020203040405050607070808090A0A0B0B0C0C0D0E0E0F0F101111121213
	HEX 0101020303040405060607080809090A0B0B0C0D0D0E0E0F1010111212131314
	HEX 01010203030405050607070809090A0B0B0C0C0D0E0E0F101011121213141415
	HEX 010102030304050606070808090A0A0B0C0C0D0E0E0F10111112131314151516
	HEX 010102030404050606070809090A0B0C0C0D0E0E0F1011111213131415161617
	HEX 0102020304050506070808090A0B0B0C0D0E0E0F101111121314141516171718
	HEX 0102020304050506070809090A0B0C0D0D0E0F10101112131414151617171819
	HEX 01020203040506070708090A0B0B0C0D0E0F0F1011121314141516171818191A
	HEX 01020303040506070808090A0B0C0D0E0E0F1011121313141516171818191A1B
	HEX 010203040405060708090A0B0B0C0D0E0F1011121213141516171819191A1B1C
	HEX 010203040505060708090A0B0C0D0E0F0F10111213141516171818191A1B1C1D
	HEX 010203040506070808090A0B0C0D0E0F10111213141516171718191A1B1C1D1E
	HEX 0102030405060708090A0B0C0D0E0F10101112131415161718191A1B1C1D1E1F

; ------ Screen addresses (missed out first half)
SCRTAB	HEX 00480049004A004B004C004D004E004F20482049204A204B204C204D204E204F
	HEX 40484049404A404B404C404D404E404F60486049604A604B604C604D604E604F
	HEX 80488049804A804B804C804D804E804FA048A049A04AA04BA04CA04DA04EA04F
	HEX C048C049C04AC04BC04CC04DC04EC04FE048E049E04AE04BE04CE04DE04EE04F
	HEX 0050005100520053005400550056005720502051205220532054205520562057
	HEX 4050405140524053405440554056405760506051605260536054605560566057
	HEX 80508051805280538054805580568057A050A051A052A053A054A055A056A057
	HEX C050C051C052C053C054C055C056C057E050E051E052E053E054E055E056E057

; ------ For each X position (0-15), all the X offsets needed!
XPOSNS	HEX 00000000000000000000000000000000
	HEX 100C0A08070605050404040303030202
	HEX 201713100E0C0B0A0908070606050404
	HEX 2F231D181513100F0D0C0B0A09080706
	HEX 3F2E26201C19161412100E0D0B0A0908
	HEX 4F3A3028231F1B19161412100E0D0B0A
	HEX 5F4639312A25211E1A181513110F0D0C
	HEX 6E514339312C26221F1C191614120F0E
	HEX 7E5D4C4138322C2723201C1917141210
	HEX 8E6856493F38312C2723201D1A171412
	HEX 9E745F51463E37312C2723201C191614
	HEX AD8069594D453C36302B27231F1C1816
	HEX BD8B7261544B423B352F2A26221E1A18
	HEX CD977C695B51474039332E2925211C1A
	HEX DDA2857162574D453D37312C28231F1C
	HEX ECAE8F79695E524A423B35302B26211E
	HEX FCBA98827064584F463F38332E282320

; ------ The four flashing colours (QuickIndex)
FOURCOLS	DB	BLUE,MAGENTA,GREEN,CYAN,WHITE,CYAN,GREEN,MAGENTA

; ------ Screen offsets for left/right/up/down in characters (QuickIndex)
MOVMTAB	DB	-1,1,-32,32

; Module name: Logical -> physical object translation table
;
; Usage: For each logical object, three physical objects are actually stored.
; This table gives, by indexing with the desired logical object, the smallest
; physical object appropriate (BB_GD_#nn). This has the advantage of not
; being restricted to any order of graphic layout in memory.

GRAPHINDEX	DB	3	;  1 palm tree
	DB	6	;  2 beech tree
	DB	9	;  3 pine tree
	DB	12	;  4 log
	DB	15	;  5 boulder
	DB	18	;  6 fence
	DB	21	;  7 timegate
	DB	24	;  8 100pts gate
	DB	27	;  9 250pts gate
	DB	30	; 10 500pts gate
	DB	33	; 11 Flag frame 1
	DB	36	; 12 Flag frame 2
	DB	39	; 13 rock
	DB	42	; 14 barrel
	DB	45	; 15 shrubbery
	DB	76	; 16 brick wall
	DB	70	; 17 crashing toggle set-off
	DB	73	; 18 water splash 
	DB	79	; 19 hand 1
	DB	82	; 20 hand 2
	DB	85	; 21 hand 3
	DB	88	; 22 "GO"
	DB	91	; 23 START banner

; ------ Each flag holds the reflect state for each buggy image
REFLAGS	DB	0,0,0,0,0,0,0	; buggies reflect flags

; The following tables MUST all start and end within the same page boundary,
; or the Q&D indexing won't work!

	RADIX	2

; ------ Bit positions (QuickIndex)
BITPOSNS	DB	10000000,11000000,11100000,11110000
	DB	11111000,11111100,11111110,11111111

BITPOSNS2	DB	11111111,01111111,00111111,00011111
	DB	00001111,00000111,00000011,00000001

	RADIX	10

; ------ Height of gates (in actual on-screen offsets) (QuickIndex)
GATEHEIGHT	DW	-500H,-400H,-300H,-280H,-200H,-180H,-140H,-100H
GATEHT2	DW	-6C0H,-580H,-400H,-360H,-260H,-200H,-1A0H,-160H

; ------ Half width of gates (in pixels) (QuickIndex)
GATEHWIDTH	DB	53,43,32,26,22,16,13,11
GATEHW2	DB	106,85,64,53,42,32,26,21

; ------ Actual heights of the gates (QuickIndex)
POLEPLUS	DB	40,32,24,20,16,12,10,8
POLEPLUS2	DB	54,44,32,27,22,16,13,11

; ------ This corresponds to HORIZ_R2 minus the values in entry 128 above
YREFPOSNS	DB	144,106,87,74,64,57,50,45,40,36,32,29,26,23,20,18

; ------ This will hold two values, a Y coord and a max horizon value for that Y
YCOORDS	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; holds Y and Ymax for horizonts
	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; ------ This holds the values of the centre of the track, 9-bit 2's complemented
OFFSETS	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	BOUNDARY 100H

; ------ Interrupt vectors (all the same)
VECTORS	REPEAT 81H
	DW	INTRPT

; ------ 32 bytes of workspace
WORKSP	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	; >32 bytes of free space

	SUBTTL "Initialized tables

STOREPC	EQU	$
	ORG	5B00H
;
; ------ Attribute address table (only used when PxxLIST is not used)
ATTRTAB

; WORKSPACE FOR VARIOUS COORDINATE AND MAINTENANCE TABLES
; -------------------------------------------------------
;
; The coordinate tables have the following contents:-
;
; P0XLIST:	primary X coordinates for all primitive points
; P0YLIST: primary Y coordinates for all primitive points
; P1XLIST: secondary X coordinates for all primitive points
; P1YLIST:	secondary Y coordinates for all primitive points
; P2XLIST: fractional X coordinates for all primitive points
; P2YLIST: fractional Y coordinates for all primitive points
; P3XLIST: not used
; P3YLIST: not used
; 
; All Y coordinate tables have byte-sized entries, the X tables are
; word-sized (15:59:17 on 22/02/88).
;
;
; ------ The current primitive points on-screen (we're currently sw. in betw)
P0XLIST	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P0YLIST	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P1XLIST	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P1YLIST	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P2XLIST	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P2YLIST	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P3XLIST	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DW	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
P3YLIST	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; ------ Different buggy frames' qualities
BB_TABLE
BB_FA	DB	20H+10,1FH,2DH,-10
	DW	EXPLOSION,4
BB_FB	DB	20H+12,1FH,2DH,-12
	DW	BUGGY6,6
BB_FC	DB	20H+12,1FH,2DH,-12
	DW	BUGGY5,6
BB_FD	DB	20H+10,1FH,2DH,-10
	DW	BUGGY4,5
BB_FE	DB	20H+14,1FH,2DH,-14
	DW	BUGGY3,7
BB_FF	DB	20H+12,1FH,2DH,-12
	DW	BUGGY2,6
BB_00	DB	20H-10,21H,2CH,10
	DW	BUGGY1,-5
BB_01	DB	20H-12,21H,2CH,12
	DW	BUGGY2,-6
BB_02	DB	20H-14,21H,2CH,14
	DW	BUGGY3,-7
BB_03	DB	20H-10,21H,2CH,10
	DW	BUGGY4,-5
BB_04	DB	20H-12,21H,2CH,12
	DW	BUGGY5,-6
BB_05	DB	20H-12,21H,2CH,12
	DW	BUGGY6,-6
BB_06	DB	20H-10,21H,2CH,10
	DW	EXPLOSION,-4

; ------ Dimensions of buggies' frames
BB_DIMS	DB	10*9	; no. of characters
	DW	BUGGY1	; 1st character's address
	DB	11*12
	DW	BUGGY2
	DB	11*14
	DW	BUGGY3
	DB	10*9
	DW	BUGGY4
	DB	12*11
	DW	BUGGY5
	DB	12*11
	DW	BUGGY6
	DB	10*7
	DW	EXPLOSION

; ------ Jump table for specialized 'non-bitmapped' objects
JUMPTABLE	DW	VERTLINE,HORIZLINE,WIDEPRIM,HORIZLINE
	DW	GRABPRAMSL,GRABPRAMSR
	DW	WATERL,WATERR,WATERBL,WATERBR
	DW	LVLINE1,RVLINE1,LVLINE,RVLINE
	DW	LVLINE1,RVLINE1,LVLINE,RVLINE
	DW	LVLINE1,RVLINE1,LVLINE,RVLINE
	DW	A_GOBACK
	DW	COLOURMAP
	DW	TURN2BLACK
	DW	TURN2YELLI

; ------ What to do if colliding with something
ACTIONTBLE	DW	A_CRASH,A_CRASH,A_CRASH
	DW	A_BOUNCE
	DW	A_CRASH,A_CRASH
	DW	A_TIME,A_100,A_250,A_500,A_FLAG,A_FLAG
	DW	A_ROCK
	DW	A_CRASH
	DW	A_SLOW
	DW	A_CRASH,A_CRASH,A_CRASH
	DW	A_GOBACK,A_GOBACK,A_GOBACK,A_GOBACK,A_GOBACK

; ------ Modify these addresses with the wall pattern to draw
WALPATS	DW	LINEMOD1,LINEMOD2,LINEMOD3,LINEMOD4,LINEMOD5
	DW	LINEMOD6,LINEMOD7,LINEMOD8,LINEMOD9,LINEMOD10
	DW	LINEMOD11,LINEMOD12,LINEMOD13

; ------ key/joystick addresses (initial)
CTRL_UP	HEX	FEDF01
CTRL_DOWN	HEX	FEBF02
CTRL_LEFT	HEX	FEFE04
CTRL_RIGHT	HEX	FEFE08
CTRL_FIRE	HEX	FE7F01

	SUBTTL "Uninitialized tables; text and variables

CLR_START			; start zeroing from here to CLR_END

; ------ Variables relevant to calculation of track
HORIZON	DB	0	; Global horizon 0-100 (50-150) (NG)
BUGGY	DW	0	;	Buggys distance from centre (IG)
P0LISTID	DB	0	; Primitive 0 list identification (P)
P1LISTID	DB	0	; Primitive 1 list identification (P)
FRACT_PTR	DW	0	; pointer to fraction table entries (32 bytes) (U)
MIDDLE	DW	0	; This is the middle of the screen (I)
TRACKFRAC	DB	0	; fraction 0-31 between current p's (P)
DFADDR	DW	0	; current address of df address in VDU routines (U)
SPEED	DB	0	; how fast the buggy is going (P)
TEMPSP	DW	0	; temporary store for stack pointer (U)
MILEAGE	DW	0	; actually the absolute primitive no. (P)
PRIMSLEFT	DB	0	; primitives left counter (P)
BUGGYFRAC	DW	0	; pointer to fraction table for automatic buggy motion (U)
LASTBFRAC	DB	0	; used in automotion, denotes last absolute fraction of movement (P)

; ------ Pointers to data related to track display and calculation
TRACK_PTR	DW	0	; pointer to page of track data (U)
SEG_PTR	DW	0	; pointer to track segments (U)
OBJ_PTR	DW	0	; pointer to objects on the track (U)
HORIZ_PTR	DW	0	; pointer to hill data (U)

; ------ Variables relevant to image and sprite drawing
VERTCLIP	DB	0	; vertical clip factor (from height) (P)
TIMER	DB	0,0	; 50Hz timer (P) and seconds (P)
TIMEFLAG	DB	0	; second counter has revolved (P)
FRAME	DB	0	; updates through main loop (P)
NOSPRITES	DB	0	; number of sprites (P)
BUGFRAME	DB	0	; What does the buggy look like (P)
BUGSTATE	DB	0	; Real left/right
TILT	DB	0	; Tilting information (P)
TILTDES	DB	0
TILTDELAY	DB	0
WALPATTERN	DB	0	; Wallpattern (P)
ORIGPAT	DB	0	; original pattern (P)
GRAPHIC	DB	0	; temp. storage for these two items (P)
MASK	DB	0	; mask storage (P)
LAYER	DB	0	; 'nearly' the inverse of the mask (P)
BCKCOLOUR	DB	0	; current colour (P)
TRACK_TYPE	DB	0	; Track type (offroad, north, etc.) (P)
CMAPDELAY	DB	0	; nz if a mapsegment is on-screen (P)
CRASHFLAG	DB	0	; z if no crash in progress (P)
HITFLAG	DB	0	; set if collision reports ON (P)
SCORE	DB	0,0,0	; score (U)
TGPICKED	DB	0	; how many timegates picked (P)
JUMPFLAG	DB	0	; used in jumping (P)
JUMPDELAY	DB	0	; used in jumping (P)
JUMPFACTOR	DB	0	; used in jumping (N)
JUMPOFFSET	DB	0	; used in jumping (P)
MOUNTVALUE	DB	0	; Position of mountains (M)
POLARISAT	DB	0	; Polarisation flag (N)

; ------ Variables for coordinate/display calculation
TEMPCOORDS			; 16-bit addressing of: (U)
TEMPY	DB	0	; store the X and Y coordinates of (P)
TEMPX	DB	0	; an object in print/question here (P)
LASTLXY			; 16-bit addressing of: (U)
LASTLY1	DB	0	; as TEMPX/Y but for last set (P)
LASTLX	DB	0	; last left X (P)
LASTLY2	DB	0	; last lower Y (P)
LASTRXY			; last left/right wall coords (P)
LASTRY1	DB	0	; last right upper Y (P)
LASTRX	DB	0	; last right X (P)
LASTRY2	DB	0	; last right lower Y (P)
LOWERY	DB	0	; lower end reference for walls (P)
LASTXYL	DW	0	; last X,Y access (u)
LASTDFL	DW	0	; last display-file access (U)
LASTPPOSL	DB	0	; last pixel position within display-file (P)
LASTXYR	DB	0,0	; last X,Y access (P,P)
LASTDFR	DW	0	; last display-file access (U)
LASTPPOSR	DB	0	; last pixel position within display-file (P)
PRIMITIVE	DB	0	; store current primitive here (P)
BOTDF	DW	0	; equvivalent to lower y, but df addr (U)
BOTY	DB	0	; height of 'what cannot be seen' (P)
TOPDF	DW	0	; eq. of top df addr calc'd Y (U)
TOPY	DB	0	; eq. of top Y
NEXTY	DB	0	; Y value of above primitive (P)
NEXTX	DB	0	; X value of above primtitive (P)
BACKPRIM	DB	0	; primitive number of next object (P)
XOFFSET	DW	0	; 'current' x-coordinate, 9-bit complemented (I)
LASTWIDTH	DB	0	; last width of a slope (P)
SLPWIDTH	DB	0	; current width of a slope (P)
SEED	DB	0,0,0,0	; 32-bit IEEE random seed (X)

; ------ Local variables & scratchpads
TEMPW1	DW	0	; temp. word 1 (normally -> p3ylist) (M) (L)
TEMPB1	DB	0	; temp. byte 1 (M) (L)
TEMPB2	DB	0	; temp. byte 2 (M) (L)
FLAGB1	DB	0	; flags whether pri 0 is inhabited (N/P)

CLR_END			; stop zeroing on previous byte

; ------ Variables and pointers which must be preserved during initialisation
MAP_DEST	DW	0	; attraddress of map data (U)
MAP_PTR	DW	0	; pointer to map data (U)

; ------ Text and messages related to the game
MSG_XTDTME	STRNG	DF_PSEUDO+40AH,"EXTENDED PLAY",END_TXT
MSG_START	STRNG	482CH,"OPTIONS",END_TXT
TXT_START	STRNG	4888H,"PLAY",END_TXT
	STRNG	48C8H,"RESET",END_TXT
	STRNG	5008H,"RE-ENTER MONITOR",END_TXT
	STRNG	5048H,"DEFINE PLAYING KEYS",END_TXT
	STRNG	5088H,"SAVE OBJECT TO TAPE",END_TXT
	DW	EOX
MSG_SELECT	STRNG	5025H,"SELECT YOUR COURSE BY",MORE_TXT
	STRNG	5066H,"MOVING THE JOYSTICK",MORE_TXT
	STRNG	50A8H,"THEN PRESS FIRE",END_TXT
TXT_SELECT	STRNG	4003H,"OFFROAD",END_TXT
	STRNG	4043H,"NORTH",END_TXT
	STRNG	4083H,"EAST",END_TXT
	STRNG	40C3H,"WEST",END_TXT
	STRNG	4803H,"SOUTH",END_TXT
	DW	EOX
MSG_OFFRD	STRNG	-67H,"OFFROAD",END_TXT
	DB	0C7H,13,14,END_TXT
MSG_NORTH	STRNG	3BH,"NORTH",END_TXT
	DB	025H,11,12,15,END_TXT
MSG_EAST	STRNG	07AH,"EAST",END_TXT
	DB	068H,11,12,15,END_TXT
MSG_WEST	STRNG	3CH,"WEST",END_TXT
	DB	023H,11,12,15,END_TXT
MSG_SOUTH	STRNG	-7EH,"SOUTH",END_TXT
	DB	0E1H,11,12,15,END_TXT
MSG_PANEL	DW	4010H
	DB	"TIME ",17,18,17,18,17,18,17,18,17,18,MORE_TXT
	DW	4035H
	DB	19,32,19,32,19,32,19,32,19,32,MORE_TXT
	STRNG	4072H,"LEG     SCORE",MORE_TXT
	DW	40D1H
	DB	"SPEED   ",20,21,20,21,20,21,MORE_TXT
	DW	40F4H
	DB	"kph  ",22,23,22,23,22,23,END_TXT

; ------ Widths of primitive points
WIDTHS	DW	1000000000000000B,1000000000000000B
	DW	1100000000000000B,1110000000000000B
	DW	1110000000000000B,1111000000000000B
	DW	1111000000000000B,1111100000000000B
	DW	1111111000000000B,1111111100000000B
	DW	1111111110000000B,1111111111000000B
	DW	1111111111110000B,1111111111111100B
	DW	1111111111111111B,1111111111111111B

; ------ Widths of poles
WIDTHS2	DW	1000000000111110B
	DW	1000000000111110B
	DW	1000000100111100B
	DW	1000000100111100B
	DW	1100001100011000B
	DW	1100001100011000B
	DW	1110001100001000B
	DW	1110001100001000B

; ------ Characters to create road map
RDCHRBASE

CHAR0	HEX 0000FFFFFFFF0000
CHAR1	HEX FFFFFFFF00000000
CHAR2	HEX 0080FFFFFFFF8000
CHAR3	HEX F0F0787878783C3C
CHAR4	HEX 033FFFFFFCC00000
CHAR5	HEX 0703010000000000
CHAR6	HEX 3C3E1F1F0F030000
CHAR7	HEX 3C3C1E1E0F0F0703
CHAR8	HEX 0000030F3F7FFCF0
CHAR9	HEX 3C7EFFE7E7FF7E3C
CHAR10	HEX 3C3C3C3C3C3C3C3C
CHAR11	HEX 0077427212720000
CHAR12	HEX 0076557655550000
CHAR13	HEX 00007F407F000000
CHAR14	HEX 080CFA01FA0C0800
CHAR15	HEX 0070202020200000
CHAR16	HEX 1038FE386C820000	; star
CHAR17	HEX 307C2F2F2F2F2F2F	; flag topleft
CHAR18	HEX 000000C0FCF0C000	; flag topright
CHAR19	HEX 6828282828283800	; flag botleft
CHAR20	HEX 007F5F5F5F5F5F5F	; gate topleft
CHAR21	HEX 00FEFAFAFAFAFAFA	; gate topright
CHAR22	HEX 5050505050507000	; gate botleft
CHAR23	HEX 0A0A0A0A0A0A0E00	; gate botright

; ------ Addresses of the different maps and messages for the tracks
MAPTABLE	DW	OFFRD_MAP,MSG_OFFRD
	DW	NORTH_MAP,MSG_NORTH
	DW	EAST_MAP,MSG_EAST
	DW	WEST_MAP,MSG_WEST
	DW	SOUTH_MAP,MSG_SOUTH

OFFRD_MAP	DB	0A8H
	CHAR	2,X,R
	CHAR	0,X,R
	CHAR	6,VH,D
	CHAR	3,VH,D
	CHAR	5,X,R
	CHAR	4,V,R
	CHAR	0,X,R
	CHAR	4,X,R
	CHAR	5,V,U
	CHAR	3,H,U
	CHAR	3,X,U
	CHAR	5,VH,L
	CHAR	4,H,L
	CHAR	4,VH,L
	CHAR	7,X,U
	CHAR	3,X,U
	CHAR	5,VH,L
	CHAR	4,H,L
	CHAR	4,V,L
	CHAR	7,H,D
	CHAR	3,H,D
	CHAR	5,V,L
	CHAR	4,X,L
	CHAR	4,V,L
	CHAR	1,X,L
	CHAR	1,X,L
	CHAR	4,X,L
	CHAR	4,VH,L
	CHAR	5,H,D
	CHAR	3,V,D
	CHAR	3,VH,D
	CHAR	5,X,R
	CHAR	4,V,R
	CHAR	0,X,R
	CHAR	0,X,R
	CHAR	4,X,R
	CHAR	5,V,U
	CHAR	3,H,U
	CHAR	6,H,R
	CHAR	0,X,R
	DB	-1

NORTH_MAP	DB	28H
	CHAR	9,X,R
	CHAR	6,VH,D
	CHAR	6,V,L
	CHAR	4,VH,L
	CHAR	1,H,L
	CHAR	4,H,L
	CHAR	0,X,L
	CHAR	0,X,L
	CHAR	9,X,U
	CHAR	5,VH,L
	CHAR	4,H,L
	CHAR	6,H,D
	CHAR	3,VH,D
	CHAR	3,V,D
	CHAR	10,X,D
	CHAR	6,X,R
	CHAR	9,X,D
	CHAR	5,X,R
	CHAR	4,V,R
	CHAR	6,V,U
	CHAR	10,H,U
	CHAR	6,H,R
	CHAR	4,H,R
	CHAR	5,VH,D
	CHAR	3,X,D
	CHAR	7,X,R
	CHAR	9,X,U
	CHAR	6,H,R
	CHAR	6,V,U
	CHAR	3,V,U
	CHAR	5,H,R
	CHAR	4,VH,R
	CHAR	6,VH,D
	CHAR	3,H,D
	CHAR	3,X,D
	CHAR	9,X,R
	CHAR	6,V,U
	CHAR	3,V,U
	CHAR	5,H,R
	CHAR	5,VH,D
	CHAR	3,X,D
	CHAR	6,X,R
	CHAR	6,V,U
	CHAR	3,V,U
	CHAR	5,H,R
	CHAR	5,VH,D
	CHAR	9,X,D
	DB	-1

SOUTH_MAP	DB	0C1H
	CHAR	9,X,U
	CHAR	6,H,R
	CHAR	6,VH,D
	CHAR	6,X,R
	CHAR	6,V,U
	CHAR	6,H,R
	CHAR	6,VH,D
	CHAR	6,X,R
	CHAR	6,V,U
	CHAR	6,H,R
	CHAR	9,X,R
	CHAR	4,H,R
	CHAR	7,V,U
	CHAR	6,H,R
	CHAR	6,VH,D
	CHAR	3,VH,D
	CHAR	5,X,R
	CHAR	4,V,R
	CHAR	0,X,R
	CHAR	9,X,R
	CHAR	0,X,R
	CHAR	6,V,U
	CHAR	6,VH,L
	CHAR	4,V,L
	CHAR	4,X,L
	CHAR	4,VH,L
	CHAR	7,X,U
	CHAR	6,H,R
	CHAR	9,X,R
	CHAR	4,X,R
	CHAR	4,V,R
	CHAR	6,V,U
	CHAR	7,VH,L
	CHAR	4,X,L
	CHAR	0,X,L
	CHAR	4,V,L
	CHAR	4,X,L
	CHAR	9,X,U
	CHAR	10,X,U
	CHAR	6,H,R
	CHAR	6,VH,D
	CHAR	6,X,R
	CHAR	8,VH,U
	CHAR	5,H,R
	CHAR	4,VH,R
	CHAR	0,X,R
	CHAR	9,X,R
	DB	-1

EAST_MAP	DB	67H
	CHAR	9,X,D
	CHAR	7,X,R
	CHAR	1,H,R
	CHAR	4,VH,R
	CHAR	6,VH,D
	CHAR	6,X,R
	CHAR	6,V,U
	CHAR	3,V,U
	CHAR	5,H,R
	CHAR	1,H,R
	CHAR	4,VH,R
	CHAR	6,VH,D
	CHAR	9,X,D
	CHAR	10,X,D
	CHAR	6,V,L
	CHAR	4,V,L
	CHAR	1,X,L
	CHAR	4,X,L
	CHAR	4,VH,L
	CHAR	4,H,L
	CHAR	9,X,L
	CHAR	4,V,L
	CHAR	4,X,L
	CHAR	6,X,U
	CHAR	6,VH,L
	CHAR	4,VH,L
	CHAR	7,X,U
	CHAR	6,VH,L
	CHAR	9,X,L
	CHAR	6,X,U
	CHAR	6,H,R
	CHAR	0,X,R
	CHAR	4,H,R
	CHAR	4,VH,R
	CHAR	6,VH,D
	CHAR	9,X,R
	CHAR	6,V,U
	CHAR	10,X,U
	CHAR	6,H,R
	CHAR	6,V,U
	CHAR	6,VH,L
	CHAR	4,VH,L
	CHAR	4,H,L
	CHAR	6,H,D
	CHAR	9,X,D
	DB	-1

WEST_MAP	DB	26H
	CHAR	9,X,R
	CHAR	0,X,R
	CHAR	8,V,D
	CHAR	5,X,R
	CHAR	1,X,R
	CHAR	4,V,R
	CHAR	9,X,R
	CHAR	8,VH,U
	CHAR	5,H,R
	CHAR	1,H,R
	CHAR	5,VH,D
	CHAR	3,X,D
	CHAR	3,H,D
	CHAR	5,V,L
	CHAR	4,X,L
	CHAR	9,X,L
	CHAR	1,H,L
	CHAR	4,H,L
	CHAR	8,X,D
	CHAR	3,X,D
	CHAR	9,X,L
	CHAR	6,X,U
	CHAR	6,VH,L
	CHAR	4,V,L
	CHAR	7,H,D
	CHAR	7,V,L
	CHAR	9,X,L
	CHAR	4,V,L
	CHAR	4,X,L
	CHAR	0,X,L
	CHAR	9,X,L
	DB	-1


; ROAD BLOCKS
; -----------
;
; These are the 'segments' that make out a whole track. Each block holds 32
; entries (nibble-mapped), which range from 0 to 14. 15 means end-of-block.
; To obtin negative numbers, the system puts the restriction that ALL the
; entries must be negated.
;

ROADBLOCKS	DW	ROAD_01,ROAD_02,ROAD_03,ROAD_04
	DW	ROAD_05,ROAD_06,ROAD_07,ROAD_08
	DW	ROAD_09,ROAD_10,ROAD_11

; The different road blocks (there are 32) have contents as follows:-
;
; Road Block	Description
; ----------	------------------------------------------------------------
; ROAD_01		Straight road
; ROAD_02		Right turn, factor 2
; ROAD_03		Right turn, factor 3
; ROAD_04		Right turn, factor 4
; ROAD_05		Right turn, factor 5
; ROAD_06		Right turn, factor 6
; ROAD_07		Right turn, factor 7

; ------ Road Blocks
ROAD_01	HEX	0000000000000000FF
ROAD_02	HEX	12321F
ROAD_03	HEX	1234321F
ROAD_04	HEX	123454321F
ROAD_05	HEX	12345654321F
ROAD_06	HEX	1234567654321F
ROAD_07	HEX	123456787654321F
ROAD_08	HEX	12345678987654321F
ROAD_09	HEX	2468ACA8642F
ROAD_10	HEX	3333333F
ROAD_11	HEX	123456789ABCCCCCCCCBA9876543210F

; road_block N,<speed>,<sign>
SEGMENTS	RDBLK	1,1,0

;	RDBLK	1,1,0
;	RDBLK	1,1,0
;	RDBLK	1,1,0
;	DW	0,0,0

	RDBLK	9,0,0
	RDBLK	9,0,1
	RDBLK	8,1,0
	RDBLK	8,1,1
	RDBLK	11,0,0
	RDBLK	1,1,0
	RDBLK	6,0,0
	RDBLK	6,0,1
	RDBLK	8,1,1
	RDBLK	1,0,0
	RDBLK	2,0,1

	DB	0

	DW	0
	DW	0	; end-of-table marker

	DW	0FFFFH,0FFFFH	; ahem... 

OBJDIST	EQU	60	; distance between objects in objtable
OBJTABLE	

QRT	DEFL	1	; starting off at 20

	DO	8

	IF	TRUE

	OBJCT	BANNER,QRT+4,0
	OBJCT	THICKLINE,QRT+4,0
	OBJCT	PTS100,QRT+10,-15
	OBJCT	PTS500,QRT+15,5

QRT	DEFL	QRT+15

	OBJCT	PALMTREE,QRT+3,-15
	OBJCT	PALMTREE,QRT+3,15

	OBJCT	BRICKWALL,QRT+5,0

	OBJCT	PALMTREE,QRT+7,-15
	OBJCT	PALMTREE,QRT+7,15

	OBJCT	PALMTREE,QRT+11,-15
	OBJCT	PALMTREE,QRT+11,15

	OBJCT	FENCE,QRT+13,8

	OBJCT	PALMTREE,QRT+15,-15
	OBJCT	PALMTREE,QRT+15,15

	OBJCT	PALMTREE,QRT+19,-15
	OBJCT	PALMTREE,QRT+19,15

QRT	=	QRT+20

	OBJCT	BARREL,QRT+3,9
	OBJCT	BARREL,QRT+3,-9

	IF	TRUE

	OBJCT	PATCHR,QRT+5,8
	OBJCT	PATCHL,QRT+5,-8

	OBJCT	PATCHR,QRT+6,8
	OBJCT	PATCHL,QRT+6,-8

	OBJCT	PATCHR,QRT+7,8
	OBJCT	PATCHL,QRT+7,-8

	OBJCT	PATCHR,QRT+8,8
	OBJCT	PATCHL,QRT+8,-8

	OBJCT	PATCHR,QRT+9,8
	OBJCT	PATCHL,QRT+9,-8

	OBJCT	PATCHR,QRT+10,8
	OBJCT	PATCHL,QRT+10,-8

	OBJCT	PATCHR,QRT+11,8
	OBJCT	PATCHL,QRT+11,-8

	OBJCT	BLACKPR,QRT+12,8
	OBJCT	BLACKPL,QRT+12,-8

	OBJCT	BLACKPR,QRT+13,8
	OBJCT	BLACKPL,QRT+13,-8

	ENDIF

	OBJCT	BARREL,QRT+14,9
	OBJCT	BARREL,QRT+14,-9

	OBJCT	MAPSEGMENT,QRT+15,0

	OBJCT	LOG,QRT+16,-3

QRT	=	QRT+16

	OBJCT	SNGLHLINE,QRT+5,0
	OBJCT	HLINE,QRT+5,0
	OBJCT	HLINE,QRT+6,0
	OBJCT ROCK,QRT+6,6
	OBJCT	HLINE,QRT+7,0
	OBJCT ROCK,QRT+7,-4
	OBJCT	HLINE,QRT+8,0
	OBJCT	ROCK,QRT+8,0
	OBJCT	HLINE,QRT+9,0
	OBJCT	ROCK,QRT+9,-12
	OBJCT	HLINE,QRT+10,0
	OBJCT	ROCK,QRT+10,9
	OBJCT	HLINE,QRT+11,0
	OBJCT	ROCK,QRT+11,-8
	OBJCT	HLINE,QRT+12,0
	OBJCT	BOULDER,QRT+12,15
	OBJCT	HLINE,QRT+13,0
	OBJCT	SNGLHLINE,QRT+14,0

	OBJCT	LOG,QRT+15,-8
	OBJCT	HASSELBLAD,QRT+22,-12
	OBJCT	HASSELBLAD,QRT+22,12
	OBJCT	TUNNRX,QRT+22,7
	OBJCT	TUNNLX,QRT+22,-7
	OBJCT	GOBLACK,QRT+23,0
	OBJCT	TUNNR,QRT+23,7
	OBJCT	TUNNL,QRT+23,-7
	OBJCT	TUNNR,QRT+24,7
	OBJCT	TUNNL,QRT+24,-7
	OBJCT	TUNNR,QRT+25,7
	OBJCT	TUNNL,QRT+25,-7
	OBJCT	TUNNR,QRT+26,7
	OBJCT	TUNNL,QRT+26,-7
	OBJCT	TUNNR,QRT+27,7
	OBJCT	TUNNL,QRT+27,-7
	OBJCT	TUNNR,QRT+28,7
	OBJCT	TUNNL,QRT+28,-7
	OBJCT	TUNNR,QRT+29,7
	OBJCT	TUNNL,QRT+29,-7

	OBJCT	GOYELLOW,QRT+31,0
	OBJCT	PTS250,QRT+31,7
	OBJCT	FLAG1,QRT+33,0

QRT	=	QRT+35

	OBJCT	HASSELBLAD,QRT,-13
	OBJCT	WALLLX,QRT+1,-8
	OBJCT	WALLL,QRT+2,-8
	OBJCT	WALLL,QRT+3,-8
	OBJCT	WALLL,QRT+4,-8
	OBJCT	WALLL,QRT+5,-8

	OBJCT	ROCK,QRT+6,8

	OBJCT	WALLL,QRT+6,-8
	OBJCT	WALLL,QRT+7,-8
	OBJCT	LOG,QRT+8,-2
	OBJCT	WALLL,QRT+8,-8
	OBJCT	FENCE,QRT+10,-12
	OBJCT	BOULDER,QRT+14,-4
	OBJCT	BOULDER,QRT+16,-0
	OBJCT	SHRUBBERY,QRT+20,8

QRT	=	QRT+20

	ENDIF

	LOOP

	DW	0FFFFH	; end-of-track-data marker (highest)

HORIZTBL	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

HORIZTBN	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	DB	0,0,0,0,-1,-1,-1,-2,-2,-2,-3,-3,-3,-3,-3,-3,-3,-3,-3,-3
	DB	-3,-3,-3,-3,-3,-3,-4,-4,-4,-4,-4
	DB	-4,-3,-2,-1,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,-6,-8,-8,-8,-8,-6,-3,0,3,3,3,3,3,3,3,0,0,0,0

	HILL	6	; giz' a hill
	HILL	-6

	DB	0,0,0,0,0,0,0,0,0,0

	HILL	-6
	HILL	6

	DB	1,2,3,3,3,3,3,3,3,3,3,2,1,-1,-2,-3,-3,-3,-3,-3,-3,-2,-1,0

	DB	0,0,0

;HORIZTBN	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; ------ Set down background mountains
MOUNTAINS
	DB	0,9,1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,0,18,31,19
	DB	32,0,33,0,0,20,0,21,0,22,0,0,0,0
	DB	0,9,1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,0,18,0,19
	DB	0,0,23,26,24,27,25,28,0,29,34,30,35,0,36,0
	DB	37,9,1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,0,18,0,19
	DB	0,0,0,0
	DB	0,9,1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,0,18,0,19
	DB	0,0,31,0,32,0,33,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	DB	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0


	FREE	START-$
	ORG	STOREPC	; back from the bottom of memory

	SUBTTL "Graphics data
; 
;
; GRAPHIC DATA START HERE
; -----------------------
;
; The buggies are listed as:-
; 
; Label	picture def     size
; -------- --------------- -----
; BUGGY1	straight        10x 9
; BUGGY2	turn 20 deg.    12x10
; BUGGY3   turn 40 deg.    14x10
; BUGGY4   straight down   10x 9
; BUGGY5   tilt 20 deg.    12x11
; BUGGY6	tilt 40 deg.    12x11
;
; ------ A character attribute value of 0 means that the character doesn't exist
BUGGY1	HEX 00
	HEX 00
	HEX B00000000001030303
	HEX B0C0FFFFE0E0E0F0FF
	HEX B000FFFF00000007FF
	HEX B000FFFF000000E0FF
	HEX B003FFFF0707070FFF
	HEX B00000000080C0C0C0
	HEX 00
	HEX 00
	HEX 00
	HEX B000010307063F7F60
	HEX 7802FCFC0408C8E868
	HEX B07FC0C0DCE4E6DFFF
	HEX B0FF1A151A171AFEFF
	HEX B0FF58A858E8587FFF
	HEX B0FE03033B2767FBFF
	HEX 78403F3F2010131716
	HEX B00080C0E060FCFE06
	HEX 00
	HEX 00
	HEX 684040404040404040
	HEX 782830313131313131
	HEX 50EADFB1E0A0E0A0F1
	HEX 60A850B0E0A0E3DC54
	HEX 60150A0D0705C73B2A
	HEX 5057FB8D070507058F
	HEX 78140C8C8C8C8C8C8C
	HEX 680202020202020202
	HEX 00
	HEX 00
	HEX 684040607FFFFFFFFF
	HEX 78313171F1F1F3F313
	HEX 50BED0B0D8A85FB160
	HEX 60F49793A2E2E322BF
	HEX 602FE9C94547C744FD
	HEX 507D0B0D1B15FA8D06
	HEX 788C8C8E8F8FCFCFC8
	HEX 68020206FEFFFFFFFF
	HEX 00
	HEX B0000F1E3C3F73677F
	HEX 78F0FF07F1FCFEFFFF
	HEX 78131393D3D373FBF7
	HEX 50A060A051BE50FFFF
	HEX 58A0C0C0C1C2C2FFFF
	HEX 58050303834343FFFF
	HEX 500506058A7D0AFFFF
	HEX 78C8C8C9CBCBCEDFEF
	HEX 780FFFE08F3F7FFFFF
	HEX B000F0783CFCCEE6FE
	HEX 307FFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78F0F0F0F0F0F0F0F0
	HEX 7A0000003F3F000000
	HEX 7A000000FFFF000000
	HEX 7A000000FFFF000000
	HEX 7A000000FCFC000000
	HEX 780F0F0F0F0F0F0F0F
	HEX 30FFFFFFFFFFFFFFFF
	HEX 70FEFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 28FFFFF3F3FFF3F3F3
	HEX 60FFFFE9E9E9390909
	HEX 58FFFF2023674F4E4B
	HEX 58FFFF04C4E6F272D2
	HEX 60FFFF9797979C9090
	HEX 30FFFFCFCFFFCFCFCF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFF7F7F7F7F
	HEX 70FFFFFFFFFFFFFFFF
	HEX B0F3F3F3F3E3E3E3E1
	HEX B00F010101000000FF
	HEX 584A4948C8484F30FC
	HEX 585292121312F20C3F
	HEX B0F0808080000000FF
	HEX B0CFCFCFCFC7C7C787
	HEX 30FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFEFEFEFE
	HEX B03F3F1F0F00000000
	HEX B0FFFFFFFF00000000
	HEX B0C0C0800000000000
	HEX B0FF00000000000000
	HEX B0FF00000000000000
	HEX B0FF00000000000000
	HEX B0FF00000000000000
	HEX B00303010000000000
	HEX B0FFFFFFFF00000000
	HEX B0FCFCF8F000000000
	HEX 00
	HEX FF

BUGGY2	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000000010307
	HEX F0000000000000F0FF
	HEX F000000000000000FF
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F0000000070F09090A
	HEX 78070F1BFF97373636
	HEX F00F00000080FF3F01
	HEX F0FF0000007CE2FF1F
	HEX F0FCFF0300000081FE
	HEX F008F8FC2C4CCC8687
	HEX F000000000000000C0
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 780A0C0C0C08080810
	HEX 7836363677777677B6
	HEX 60397D7FFFFFAF57AA
	HEX 700101E12127FF5BAF
	HEX F07E0606060CFCFC5C
	HEX 78C7C3C2AEB2BEBE96
	HEX 68E0FC1E0303030303
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 7A1113232321214143
	HEX 50371E1F1E2DCA0F8C
	HEX 6055AE55AE55BF64E4
	HEX 3257AF5FBF5FDF7DEB
	HEX 78ACDC6C5C6CFCBC9C
	HEX 799292928AAABABABB
	HEX 680303030303030303
	HEX F0000000C0FFFFFFFF
	HEX F00000000080C0C0E0
	HEX 00
	HEX F0000000000F3F3F7C
	HEX F000000000FFFFFF01
	HEX 784343838386860606
	HEX 504C24E4151F11121A
	HEX 604B4991123CE41EFF
	HEX 50AC44448C0F1818E8
	HEX 78FCACACACF8F87878
	HEX 79BCFCFCFDFDFFEFEF
	HEX 78FFFFFFDFBFAF2743
	HEX 78FFFFFFFFFFFBE2E6
	HEX F0E0E0E0E0E0E0E0E0
	HEX F00000000000010101
	HEX 7063FFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78050D0F0F00000707
	HEX 7A1ABBDEFF0100FCFF
	HEX 7BFFFFFFE7FF1F00C0
	HEX 7AD7D7DAFCF6FF0100
	HEX 787878F8B8B8F8F800
	HEX 78E2E7EFD8DF9FBFBF
	HEX 7840FFFF00FFFFFFFF
	HEX 787EFEFE3ECFF7FFFF
	HEX F0E0E0C0C0C0C0C0C0
	HEX F00101010101030303
	HEX 70FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78070000E0E0DFDFDF
	HEX 7AFF0F000000FFBD5F
	HEX 7AFFFF3F000080FF7D
	HEX 7AFCFFFF030000C07F
	HEX 7A00E0E1E101010181
	HEX 70BFBF3F3F7F7F7FFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78FFFFFFFFFFFFF7F7
	HEX F0C0C0808080808080
	HEX F00303030301010101
	HEX 70FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 60DFDFFF1818FFDEDE
	HEX 607555D55555D5F5CD
	HEX 58EAD58AE6D6A6C686
	HEX 608549A9A969696969
	HEX 60FF5AFD5ADF78F87F
	HEX 707FFF7FFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78F7F7F7F7F7F7F7FF
	HEX F08080000000000000
	HEX F00101000000000000
	HEX 70FFFFFFFFFF7F7F3F
	HEX 78FFFFFEFEFCFCFFFF
	HEX 70BFFFFE7E7CFFFFFC
	HEX F0850701000000F8FF
	HEX 58868686FC80413EFC
	HEX F0697F404080000000
	HEX F07999191B1B1E1E1E
	HEX 70FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78FFFFEFEFFFFFEFEF
	HEX 00
	HEX 00
	HEX F01F07000000000000
	HEX F0FFFF000000000000
	HEX F0F8F0000000000000
	HEX F00700000000000000
	HEX F0FF03000000000000
	HEX F0FFFF000000000000
	HEX F01CFCF80000000000
	HEX F0FFFFFF7F7F3F1F07
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78CECEEEFEFCFCFCF8
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F01F00000000000000
	HEX F0E000000000000000
	HEX 00
	HEX FF

BUGGY3	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000000000107
	HEX F00000000000008080
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000000010102
	HEX 780000205F871F3F3F
	HEX 681F1DFFFFFFFFFF1F
	HEX F0F8FF878080818386
	HEX F000C0F83F07E050B0
	HEX F000000080FC7F030F
	HEX F00000000060E0F030
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 7A0204080810397DFF
	HEX 60776F77EFD7AFD7AF
	HEX 680101010101018161
	HEX 30FDFF8B8F8E65FFFF
	HEX 785CEAF9F9F8F8F0F0
	HEX F01C70808080808080
	HEX F030181E1F0F0D0707
	HEX F000000080C0E0F8FC
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000101020204
	HEX 5087868A8AF1416030
	HEX 6056AF56AF56BFFEFD
	HEX 50FF7FB261A1E1E171
	HEX 20BB5DAA5DAA5DAAFD
	HEX 78F0E0E0E0C0C0C0C3
	HEX 78783F2F352A151A15
	HEX 780783FF7FAB55AB55
	HEX 683E0F070303030303
	HEX F00000000000008080
	HEX 00
	HEX 00
	HEX 00
	HEX F0000001070F1C337F
	HEX 780438F8F1D131E3E7
	HEX 5038384C868584868B
	HEX 60485162448BB18FE1
	HEX 50BE083C44021D3FFF
	HEX 589A9D7B6F7343C387
	HEX 79878F9F9FBF3F7F7E
	HEX 798ACDEAEDF6FDFEFD
	HEX 78AB55AB55AB55AB55
	HEX 6803030383E3BBB7B3
	HEX F080809EFFFFFFFFFF
	HEX F0000000E0F8FCFEFF
	HEX 00
	HEX F00000000101010303
	HEX 307FFFFFFFFFFFFFFF
	HEX 78E7C1C0C0C2C3C3C0
	HEX 7AF3FF7F0700C0FCFF
	HEX 7BFFFFE7FF7F0F0380
	HEX 50A1212123BDF97F0F
	HEX 508F878F9FAFCFDFFF
	HEX 79FCF8F8F0E0C08003
	HEX 797E7F3E3F1F1FFFFF
	HEX 79AF5FBF7FFEFCF8FE
	HEX 78F3E7E79F0F010102
	HEX 78FFFFFFFFFFFFFF41
	HEX 78FFFFFFF7B73373F3
	HEX F000808080C0C0C0C0
	HEX F00303030707070707
	HEX 30FFFFFFFFFFFFFFFF
	HEX 78C0C0F8FFFFFFFFFF
	HEX 7A1F030000E0B87F07
	HEX 7AF0FE3F07000000E0
	HEX 7A0300E0FFFF0F0000
	HEX 78FE3C080888880808
	HEX 78070E1F3F3F7F7FFF
	HEX 30817EFFFFFFFFFFFF
	HEX 78FF3FCFFBFBF7FFFF
	HEX 780488D0E0E0F0F0F8
	HEX 78437F7F7F7F7F7FFF
	HEX 78F3F3D3E3E7E7C1C9
	HEX F0C0C0C0C0C0808080
	HEX F00707070707030303
	HEX 70FFFFFFFFFFFFFFFF
	HEX 48FFFFFFFFFFFFFFFF
	HEX 68877F478FFEFFFFFF
	HEX 58BE69D4DC6C8C8C8C
	HEX 7800C0FCBFEBBDEBBD
	HEX 781010139DFFFBD7F7
	HEX 30FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 78FFFFFFFFFEFAFAFA
	HEX 30FFFFFFFFFF7F7D7C
	HEX 70FFFFFFFFFFFFFF7F
	HEX F0FDFDFFFEFEFCF0E0
	HEX F08000000000000000
	HEX F00100000000000000
	HEX F0FFFFFF7F3F1F0701
	HEX 70FFFFFFFFFFFFFFFF
	HEX 58FFFFFFFCFCFCFFFF
	HEX 588C8C78000041FFF0
	HEX 60A9A9A9A9C8886B1C
	HEX 60D7CBE7D3EFDBE3C3
	HEX 30FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 78F2F2F2F2E2E2E4E4
	HEX F07C7C7C7C7C7C7CF8
	HEX F00F00000000000000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F07F07000000000000
	HEX F0F8E0000000000000
	HEX F03F0F000000000000
	HEX F000E0F81F07000000
	HEX F0C3C3C7DEF8000000
	HEX 70FFFFFFFFFF7F7F7F
	HEX 30FFFFFFFFFFFFFEFF
	HEX 78E4E8E9F1DF9F1F3F
	HEX F0F8F8F8F0F0E0E0C0
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F03F1F0F0300000000
	HEX F0FFFFFFFF1F000000
	HEX F0FFFFFCF0C0000000
	HEX F08000000000000000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX FF

BUGGY4	HEX 00	; low buggy
	HEX 00
	HEX F00000000000000001
	HEX F0000000000000C0FF
	HEX F000000000000000FF
	HEX F000000000000000FF
	HEX F000000000000003FF
	HEX F00000000000000080
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000000010307
	HEX F00206060404FCFC08
	HEX 507FC0DFB1A0A0A0A0
	HEX F0FF000080878E8D9A
	HEX F0FF000001E171B159
	HEX 50FE03FB8D05050505
	HEX F040606020203F3F10
	HEX F0000000000080C0E0
	HEX 00
	HEX 00
	HEX 68063F7F6040404040
	HEX 7808D8F87838383131
	HEX 50B19E90F0B8D8BFF1
	HEX 60959A97FBBCE4E722
	HEX 60A959E9DF3D27E744
	HEX 508D79090F1D1BFD8F
	HEX 78101B1F1E1C1C8C8C
	HEX 6860FCFE0602020202
	HEX 00
	HEX 00
	HEX 684040404040404040
	HEX 783131313131313131
	HEX 50A0E0A060B15EA858
	HEX 58A3BFE0C0C0C1C2C2
	HEX 58C5FD070303834343
	HEX 50050705068D7A151A
	HEX 788C8C8C8C8C8C8C8C
	HEX 680202020202020202
	HEX 00
	HEX F0000F1E3C3F73677F
	HEX 7860FF07F1FCFEFFFF
	HEX 7871F1F0D0F070F0F0
	HEX 7AFFFF000000003F3F
	HEX 7AFFFF00000000FFFF
	HEX 7AFFFF00000000FFFF
	HEX 7AFFFF00000000FCFC
	HEX 788E8F0F0B0E0E0F0F
	HEX 7806FFE08F3F7FFFFF
	HEX F000F0783CFCCEE6FE
	HEX 307FFFFFFFFFFFFFFF
	HEX 78FFFFFFFFFFFFFFFF
	HEX 78F0F0F0F0FFFFFFFF
	HEX 7800000000FFEBFDEB
	HEX 7800000000FFEF5EED
	HEX 7800000000FFF77AB7
	HEX 7800000000FFD7BFD7
	HEX 780F0F0F0FFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 70FEFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 70FFFFF3F3FFFFF3F3
	HEX 60E9F90909FF010000
	HEX 584A4B4A4948C84F30
	HEX 5852D252921213F20C
	HEX 60979F9090FF800000
	HEX 70FFFFCFCFFFFFCFCF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX F0FFFFFFFF7F7F7F7F
	HEX 70FFFFFFFFFFFFFFFF
	HEX F0F3F3F1F0E0E0E0E0
	HEX F00000FFFF00000000
	HEX F00C03FFFF00000000
	HEX F030C0FFFF00000000
	HEX F00000FFFF00000000
	HEX F0CFCF8F0F07070707
	HEX 30FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFEFEFEFE
	HEX F03F3F1F0F00000000
	HEX F0FFFFFFFF00000000
	HEX F0C0C0800000000000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00303010000000000
	HEX F0FFFFFFFF00000000
	HEX F0FCFCF8F000000000
	HEX 00
	HEX FF
;
BUGGY5	HEX 00	; low tilt
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000000000003
	HEX F000000000000080E0
	HEX F0000000001038387E
	HEX 00
	HEX 00
	HEX 00
	HEX 0000
	HEX 00
	HEX 00
	HEX 00
	HEX F00000010102020404
	HEX 6877FE0F0301000001
	HEX 787B1E0D99FBF3E7CE
	HEX F0FFC3C0808000304C
	HEX F080E0F83E0F030000
	HEX F00000000080E0F83E
	HEX F00000000000000018
	HEX 0000
	HEX 00
	HEX F00000000000000001
	HEX F000000000003FF1C0
	HEX 680808101030F0F87E
	HEX 6801030206040C0818
	HEX 780E1F1D3B3F7B7FEF
	HEX 50838101030387E418
	HEX 707CD6AA55AB55EB77
	HEX F20F03000000C0F0FC
	HEX F0BCF8F8307078E8C8
	HEX 0000
	HEX 00
	HEX F00307070F0F1F1F3F
	HEX 30FFFFFFFFFFFFFFFF
	HEX 709FEFF6FFF7F3FBFF
	HEX 7891F3E7E74ECF9E9D
	HEX 50C183334E43818182
	HEX 60B83E3B30207C67CC
	HEX 601E0E84E43E1D1ED8
	HEX 500505090B1B9F674E
	HEX 78CE8F9990901E3F30
	HEX F00080E0603070F0E000
	HEX F00000000000000101
	HEX F03F7F7FFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 78FFFFFEFEFCFCF8F8
	HEX 783F7B1E0701000000
	HEX 7AFE7D9BFFF77F1F07
	HEX 58EBF90F0301605090
	HEX 50F8B1123CA3C0C081
	HEX 78EEDCBC78B8F8B9F9
	HEX 6830206040C0808000
	HEX F0301818181030206000
	HEX F0030307070F0F1F1F
	HEX 70FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 78F0F0E0F87E4FDFFF
	HEX 7A103C0F030080E0F8
	HEX 7A010000C0F03C0F03
	HEX 7AEF7F1F07010000C0
	HEX 7AFFBEDFF3FF7F1F07
	HEX 7873F2E6E7CFCE9F97
	HEX 6800000001C133FF1F
	HEX F040C080800000000000
	HEX F01F1F1F1F0F0F0707
	HEX 30FFFFFFFFFFFFFFFF
	HEX F0FFFEFEFCFCF9F9F3
	HEX F0FF7B71E1C3818000
	HEX 60CE8F8C152A49D392
	HEX 780080E078DEF77BBD
	HEX 7AF03C0F030080E078
	HEX 78010000C0C0000101
	HEX 78BC3F7F7FFFFFFFFF
	HEX 7807F9FEFFFFFFFFFF
	HEX F080C0E060F07030B000
	HEX F00100000000000000
	HEX F0FF7F1F0000000000
	HEX F0E3C7870301000000
	HEX F000000080C0F03C0F
	HEX 58A6A54D495830301C
	HEX 58FA59D2B225654BCA
	HEX 604E4F8F9F1F23E347
	HEX 780383E7FFEFCBFBFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX F0E0E0E0E0C0C0808000
	HEX 00
	HEX 00
	HEX 00
	HEX F00300000000000000
	HEX F0C7F13C0F03000000
	HEX F0DEF00000C0F03C0F
	HEX F007030306060C0C19
	HEX 30FFFFFF7F7FFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX F0FFFFFEFEFCFCF8F8
	HEX 0000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00300000000000000
	HEX F0F9F1010101000000
	HEX 70FFFFFFFFFFFFFF7F
	HEX 30FFFFFFFFFFFFFFFF
	HEX F0F0F0E0E0C0C08080
	HEX 0000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F07F1F070100000000
	HEX F0FFFEFCF800000000
	HEX 000000
	HEX FF

BUGGY6	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000000000001
	HEX 6800070F183060C080
	HEX 68387CEEC763371F1F
	HEX F000000103FDC1870C
	HEX F00040E0C08080C0E0
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000103070F1F
	HEX F0000F7FFFFFFFFFFF
	HEX F003068CFCFEFBFDFE
	HEX 680000000103078EFC
	HEX 783870E0C3870E1D3A
	HEX F2183060C070F8FCFE
	HEX F070381C0E07030100
	HEX F0000000000080C0E0
	HEX 00
	HEX 00
	HEX 00
	HEX F00000000103070F1F
	HEX F03F7FFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 38FFFEFEFEFCFFFEFE
	HEX 78F8F0E7CF9C3B75EB
	HEX 5076EAD5A858B0D123
	HEX 500202048D772BB1D8
	HEX 30003CEAD5AA55AAD5
	HEX F070381C0E87838180
	HEX F0000000000080C0E1
	HEX 00
	HEX F00000000101030303
	HEX F03F7FFFFFFFFFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 38FFFFFFFFFFFFF2F5
	HEX 78F9F1E0C080000001
	HEX 7AF7B7F7773B1C0F07
	HEX 501F0F0E1D3EE7C381
	HEX 60FC662319A472DA8D
	HEX 506B371E8CC76A749C
	HEX F2000000E0F0F8FCFD
	HEX F0733F1F123464C484
	HEX F08000000000000000
	HEX F00303010100000000
	HEX F0FFFFFFFFFF7F3F1F
	HEX F0FFFFFFFFFFFFFEFC
	HEX 78F8FDFFFFBF1F3D60
	HEX 7883C1E0F0F8ECBEEB
	HEX 7A83C1E070381C0E07
	HEX 7BFFFFFF733D1D0F07
	HEX 58D8743A1D0F07060C
	HEX 50EC7AF1E0C1C65D2A
	HEX 7AF7EA96EEDCB870E1
	HEX 780C1E3F3B3178FDC7
	HEX F000000080C0C0C080
	HEX 00
	HEX F00F03000000000000
	HEX F0F0C1030606060301
	HEX F0C080010000000180
	HEX 58458A93E64893A7CD
	HEX 7883C160F058ECFEDB
	HEX 7A83C1E070381C0E07
	HEX 7AFFF7F77B3D1E0F07
	HEX 50151B172B57AE5CB9
	HEX 68C3870E1C3870E0E0
	HEX 68830101010103060C
	HEX F00000808080000000
	HEX 00
	HEX 00
	HEX 00
	HEX F0C16131180C060301
	HEX 581B36E6E1B1504C87
	HEX 583D3A39DBB264C992
	HEX 7883C160F058FC56AF
	HEX 7A83C1E04000000001
	HEX 78F3E7C83B3F7FFFFF
	HEX 68E0F038ECF7FFFEFE
	HEX F0183060C080000000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F0DB6731180C060301
	HEX F0263BD10000000080
	HEX F06FBF1F0F1F1C1831
	HEX 7883C7AF1FAFCFFFFF
	HEX 70FFFFFFFFFFFFFFFF
	HEX 30FEFFFFFFFFFFFFFF
	HEX F00000808080800000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F0C060311F0E000000
	HEX F063C78F1F3F3F7F7F
	HEX 70FFFFFFFFFFFFFFFF
	HEX 30FFFFFFFFFFFFFFFF
	HEX F0FFFEFEFCF8F0E0C0
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F0FFFFFFFF7F7F3F1F
	HEX 30FFFFFFFFFFFFFFFF
	HEX F0FFFFFEFCF8F0E0C0
	HEX F08000000000000000
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX 00
	HEX F00F07030000000000
	HEX F0FFFFFEF800000000
	HEX F08000000000000000
	HEX 000000
	HEX FF

EXPLOSION	HEX B2000060380A272412
	HEX B200000000000040E0
	HEX B20000000010404030
	HEX 00
	HEX 320045452141A081C3
	HEX B20000800001838381
	HEX B20000000000002000
	HEX 00
	HEX B20000000000000102
	HEX B200000000032849A8
	HEX 321110105079515935
	HEX 328038D8F7AE493573
	HEX 32304C1A9D0E8EC345
	HEX B2010100008000C971
	HEX 324257A3E0E3EBEEE6
	HEX 3203C684604045E4E0
	HEX B2000000800814061F
	HEX B20000063CC87028E2
	HEX 32197112CE055FF048
	HEX B206405020C0800000
	HEX 32B7182AFC0A83092C
	HEX 32F1DBFDF7BB2E9FDE
	HEX 32A0D2E9FCFCBFFFE4
	HEX 32D9FC7EBEFE7BFFBF
	HEX 32847F6E37FFF7FFFF
	HEX 3270F0E2E1FBFFFEDF
	HEX 32FC70EFE68CBF7FFF
	HEX 328F1B3F6FDDF6F9FB
	HEX 32ED18543F50C19034
	HEX 00
	HEX B2EF030100090D0E87
	HEX 32EF773E1C97F7EFED
	HEX 32FFFBFFEFDBBAB7F2
	HEX 72FF87FB3FE7C0C0C2
	HEX 72CF9F037F8C1C1010
	HEX 72FFFFFF903E311008
	HEX 32FFFE8E3F7FBFEFF5
	HEX 32F7EEFCB8E9EFF637
	HEX B2F7C08000B060F2E1
	HEX 00
	HEX B2730D070203040204
	HEX 324EC2E7506B665B5C
	HEX 32FCC6C47CE6C1848C
	HEX 724664004070100032
	HEX 7200E3103589C34100
	HEX 7200D6681091868F03
	HEX 32363F2EF8D71F3321
	HEX 32F2C7E78AD7E6BA8A
	HEX B2CFB2E040C0206020
	HEX 00
	HEX 32B6182A1C0A83092C
	HEX 32F3DBFCF7BB2F9FDF
	HEX 72181430785ECF7D46
	HEX 7266C4C44400301800
	HEX 77940E301C12C3CB33
	HEX 723878614703020204
	HEX 727CA01A0F1317071F
	HEX 3270A8628F1B3FEF5D
	HEX 324AD248ED18543750
	HEX 00
	HEX B26F0301000F030900
	HEX 32EE773E1C97F7FF01
	HEX 72D4F498EEBAFB697F
	HEX 77F7117E74804F589C
	HEX 77BEDC7C7FFFFFFF7E
	HEX 77BEF0C4B8C199F718
	HEX 72B7AC3D539C2A3D6B
	HEX 32F4F9FBF76E7C3AE9
	HEX B2C19034F4C08000F0
	HEX FF
;
TIMES	HEX E0000007
	HEX 3C00003C
	HEX 3FFFFFFC
	HEX 1829C838
	HEX 1EEC9DF8
	HEX 1EED5C78
	HEX 1EEDDDF8
	HEX 1EEDDDF8
	HEX 1C688838
	HEX 3FFFFFFC
	HEX 3C00003C
	HEX E0000007

TIMEM	HEX E000000000E0
	HEX 7F0000001FC0
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 0E0423880E00
	HEX 0E0421080E00
	HEX 0F9E701CFE00
	HEX 0F9E729C3E00
	HEX 0F9E739C3E00
	HEX 0F9E739CFE00
	HEX 0F0C21080E00
	HEX 0F0C21080E00
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 7F0000001FC0
	HEX E000000000E0

TIMEL	HEX C0000000000006
	HEX 7800000000003C
	HEX 1FE00000000FF0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 07C0211F1007C0
	HEX 07C0210E1007C0
	HEX 07F9F38439FFC0
	HEX 07F9F38039FFC0
	HEX 07F9F391383FC0
	HEX 07F9F39B383FC0
	HEX 07F9F39F39FFC0
	HEX 07F9F39F39FFC0
	HEX 07F0E10E1007C0
	HEX 07F0E10E1007C0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 1FE00000000FF0
	HEX 7800000000003C
	HEX C0000000000006

P250S	HEX E0000007
	HEX 3C00003C
	HEX 3FFFFFFC
	HEX 1F9831F8
	HEX 1F4BE4F8
	HEX 1FD86EF8
	HEX 1FBF2EF8
	HEX 1F3B24F8
	HEX 1F0C71F8
	HEX 3FFFFFFC
	HEX 3C00003C
	HEX E0000007

P250M	HEX E000000000E0
	HEX 7F0000001FC0
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 0FE0C071FE00
	HEX 0FC04060FE00
	HEX 0FCC4FC47E00
	HEX 0FF8C0CE7E00
	HEX 0FE1E04E7E00
	HEX 0FC7FE447E00
	HEX 0FC04060FE00
	HEX 0FC040F1FE00
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 7F0000001FC0
	HEX E000000000E0

P250L	HEX C0000000000006
	HEX 7800000000003C
	HEX 1FE00000000FF0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 07FC0E01E1FFC0
	HEX 07F80601C0FFC0
	HEX 07F9E67F8C7FC0
	HEX 07FFC67F9E7FC0
	HEX 07FF0E039E7FC0
	HEX 07FC3F019E7FC0
	HEX 07F8FFF99E7FC0
	HEX 07F9FE798C7FC0
	HEX 07F80601C0FFC0
	HEX 07F80703E1FFC0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 1FE00000000FF0
	HEX 7800000000003C
	HEX C0000000000006

BARRELS	HEX 3FC0
	HEX C1B0
	HEX 7FE0
	HEX 7560
	HEX 6820
	HEX F570
	HEX 7FE0
	HEX 7560
	HEX 6820
	HEX 7560
	HEX EAF0
	HEX 7FE0
	HEX 7560
	HEX 6820
	HEX F570
	HEX 3FC0

BARRELM	HEX 1FF8
	HEX 7016
	HEX AAB9
	HEX F557
	HEX 7FFE
	HEX 7FFA
	HEX 7502
	HEX EAAB
	HEX F55F
	HEX 7FFE
	HEX 7FFA
	HEX 7502
	HEX EAAB
	HEX F55F
	HEX 7FFE
	HEX 7FFA
	HEX 7502
	HEX EAAB
	HEX F55F
	HEX 7FFE
	HEX 1FF8

BARRELL	HEX 0FFF00
	HEX 7006E0
	HEX AAAF10
	HEX F556F0
	HEX 7FFFE0
	HEX 7FFF60
	HEX 7AAA20
	HEX 7540A0
	HEX EFF570
	HEX FAAAF0
	HEX 7FFFE0
	HEX 7FFFA0
	HEX 755520
	HEX 6AAA60
	HEX 750020
	HEX EFD570
	HEX FAAAF0
	HEX 7FFFE0
	HEX 7FFFA0
	HEX 755520
	HEX 6AAA60
	HEX 750020
	HEX EFD570
	HEX FAAAF0
	HEX 7FFFE0
	HEX 0FFF00

STARTS	HEX F00000000000000F
	HEX 1FFFFE00003FFFF8
	HEX 00739FFFFFF9CE00
	HEX 002CCFFFFFF33400
	HEX 00167FFFFFFE6800
	HEX 001FFC43621FF800
	HEX 001CCBF6AB733800
	HEX 001CC9F6AB733800
	HEX 00133C76277CC800
	HEX 00133FB6AB7CC800
	HEX 001FFFB6AB7FF800
	HEX 0039DF76ABBB9C00
	HEX 007398EDADB9CE00
	HEX F09E7FFFFFFE790F
	HEX 3FFFFFFFFFFFFFFC
	HEX 00007FFFFFFE0000

STARTM	HEX F8000000000000000000F8
	HEX 3FF0000000000000007FE0
	HEX 003FFFFC000000FFFFE000
	HEX 000F3E3FFFFFFFFBE78000
	HEX 0007FE3FFFFFFFE3FF0000
	HEX 0004F3FFFFFFFFFE790000
	HEX 000279FFFFFFFFFCF20000
	HEX 0003FFF8418E107FFE0000
	HEX 0003CF30410410679E0000
	HEX 0003C713F32499C71E0000
	HEX 0003E793F32499CF3E0000
	HEX 00023CF8730439F9E60000
	HEX 00023CFC330439F9E20000
	HEX 00027CFF332499F9F20000
	HEX 0003FFFF332499FFFE0000
	HEX 00078F3E73249CE78F0000
	HEX 00079E306664CCE3CF0000
	HEX 000FFE70E666CCF3FF8000
	HEX F833E3FFFFFFFFFE3E60F8
	HEX 7FF3C7FFFFFFFFFF1E3FF0
	HEX 000FFFFFFFFFFFFFFFC000
	HEX 000000FFFFFFFFF8000000

STARTL	HEX FC00000000000000000000000FC0
	HEX 3FFE0000000000000000001FFF00
	HEX 03FFFFFC00000000000FFFFFF000
	HEX 0007E3F3FFFFFFFFFFF3F1F80000
	HEX 0001F1F8FFFFFFFFFFC7E3E00000
	HEX 0000FFF8FFFFFFFFFFC7FFC00000
	HEX 00008F8FFFFFFFFFFFFC7C400000
	HEX 000047C7FFFFFFFFFFF8F8800000
	HEX 000047C7F040E1C181F8F8800000
	HEX 00007FFFE040C0C181FFFF800000
	HEX 00003E3FE7F9CC99E7FF1F000000
	HEX 00003E3E27F9CC99CF1F1F000000
	HEX 00003E3E27F9CC99CF1F1F000000
	HEX 00003FFF33F9CC99CF3FFF000000
	HEX 000023E3F079C083CFF1F1000000
	HEX 000023E3F839C083CFF1F1000000
	HEX 000047E3FF39CC91CFF1F8800000
	HEX 00007FFFFF39CC99CFFFFF800000
	HEX 00007C7DFF39CC99CFEF8F800000
	HEX 0000F87C7E79CC99E78F87C00000
	HEX 0000F8F8E0739C9CE7C7C7C00000
	HEX 0001FFF9E0F39CCCE7E7FFE00000
	HEX FC063F1FFFFFFFFFFFFE3F180FC0
	HEX 7FFC7E3FFFFFFFFFFFFF1F8FFF80
	HEX 03FEFC7FFFFFFFFFFFFF8FDFF000
	HEX 0001FFFFFFFFFFFFFFFFFFE00000
	HEX 00000003FFFFFFFFFFF000000000

PINETREES	HEX 0080
	HEX 0140
	HEX 0120
	HEX 02D0
	HEX 07E0
	HEX 02A0
	HEX 05B0
	HEX 0FD8
	HEX 19AC
	HEX 02D0
	HEX 05C8
	HEX 0528
	HEX 0974
	HEX 0BAE
	HEX 1293
	HEX 17C8
	HEX 2AFC
	HEX 7756
	HEX CDEB
	HEX 3B68
	HEX 0EB4
	HEX 1BF4
	HEX 37AE
	HEX 09D3
	HEX 11E0
	HEX 01A0
	HEX 0180
	HEX 0180
	HEX 0180
	HEX 0180
	HEX 00C0
	HEX 04FE

PINETREEM	HEX 001000
	HEX 003000
	HEX 002800
	HEX 005400
	HEX 00BE00
	HEX 01FB00
	HEX 005400
	HEX 009A00
	HEX 013A00
	HEX 02FD00
	HEX 076E80
	HEX 0CD5C0
	HEX 00EA60
	HEX 015E00
	HEX 016D00
	HEX 027680
	HEX 02EB40
	HEX 04BBA0
	HEX 05F970
	HEX 095498
	HEX 0BBE40
	HEX 16B7A0
	HEX 3D6EE0
	HEX 725530
	HEX C5FD18
	HEX 0F7C80
	HEX 1A6A80
	HEX 07DE40
	HEX 0EAF40
	HEX 1D75A0
	HEX 33F6F0
	HEX 067B18
	HEX 0C5880
	HEX 005C00
	HEX 005000
	HEX 005000
	HEX 005000
	HEX 005000
	HEX 005000
	HEX 005000
	HEX 003000
	HEX 00BF00
	HEX 011FF0

PINETREEL	HEX 00020000
	HEX 00050000
	HEX 00050000
	HEX 00088000
	HEX 000A4000
	HEX 0017A000
	HEX 003FB000
	HEX 006D4000
	HEX 00174000
	HEX 00152000
	HEX 0027A000
	HEX 004F9000
	HEX 009AC800
	HEX 01FD6400
	HEX 03AF5E00
	HEX 002D2700
	HEX 006B9000
	HEX 004D9000
	HEX 005EC800
	HEX 00B5C800
	HEX 009EA400
	HEX 017DD200
	HEX 012AFF00
	HEX 02DF6B80
	HEX 027FE4C0
	HEX 05ED5400
	HEX 04EBB200
	HEX 09CF7900
	HEX 13DBAD00
	HEX 26959680
	HEX 593E91C0
	HEX E2FDC860
	HEX 07AAC800
	HEX 0D3FE400
	HEX 017EA400
	HEX 025A7200
	HEX 04DB7A00
	HEX 0BBB3D00
	HEX 1CBDA680
	HEX 316DD1C0
	HEX 038EB860
	HEX 060B8C00
	HEX 000BC000
	HEX 000B6000
	HEX 000B0000
	HEX 000B0000
	HEX 000B0000
	HEX 000B0000
	HEX 000B0000
	HEX 000B0000
	HEX 000B0000
	HEX 00170000
	HEX 00070000
	HEX 0027F800
	HEX 0043FFC0


PALMTREES	HEX 001E
	HEX 3868
	HEX 5CD0
	HEX 0EE0
	HEX 1BD8
	HEX 7EFE
	HEX ABAB
	HEX 85C1
	HEX 1FF0
	HEX 37B8
	HEX 6FCC
	HEX 7D6C
	HEX DAE4
	HEX B6C4
	HEX 34C0
	HEX 2A80
	HEX 0C80
	HEX 0A00
	HEX 1C00
	HEX 1000
	HEX 1C00
	HEX 0800
	HEX 3400
	HEX 3800
	HEX 2400
	HEX 3800
	HEX 3400
	HEX 2A00
	HEX 1C00
	HEX 1200
	HEX 1D00
	HEX 1E80

PALMTREEM	HEX 0003C0
	HEX 000E80
	HEX 0E1A00
	HEX 3FB800
	HEX 55F000
	HEX 02AF80
	HEX 1D7FE0
	HEX 7FBD70
	HEX D5E558
	HEX 80BC08
	HEX 077E00
	HEX 0EEB00
	HEX 1BB780
	HEX 37B980
	HEX 7F5DC0
	HEX 6F5540
	HEX EDCC40
	HEX 9E9C40
	HEX 9B4800
	HEX 9A9800
	HEX 133000
	HEX 139000
	HEX 042000
	HEX 0D0000
	HEX 0E0000
	HEX 0B0000
	HEX 0C0000
	HEX 0E0000
	HEX 110000
	HEX 1A0000
	HEX 1D0000
	HEX 1E0000
	HEX 110000
	HEX 1A0000
	HEX 1D0000
	HEX 1E0000
	HEX 108000
	HEX 0D0000
	HEX 0E8000
	HEX 0F4000
	HEX 084000
	HEX 06A000
	HEX 075000

PALMTREEL	HEX 00001E00
	HEX 00007400
	HEX 0F01C800
	HEX 3FC36000
	HEX 6DE7A000
	HEX 05774000
	HEX 015F8000
	HEX 0E967C00
	HEX 3FEEFF80
	HEX 6BFF6AC0
	HEX AAB36AE0
	HEX 828EC0A0
	HEX 0075E020
	HEX 01AF7000
	HEX 07FEB800
	HEX 0EC9BC00
	HEX 1F372C00
	HEX 3AE9AE00
	HEX 39D6CA00
	HEX 6BA9C300
	HEX 6772C100
	HEX E769C000
	HEX AE92C000
	HEX 8EE18000
	HEX 0AD38000
	HEX 09218000
	HEX 09C70000
	HEX 09A30000
	HEX 02C60000
	HEX 032C0000
	HEX 07480000
	HEX 07800000
	HEX 04C00000
	HEX 07000000
	HEX 06800000
	HEX 07400000
	HEX 08000000
	HEX 0D400000
	HEX 0E800000
	HEX 0F400000
	HEX 0F800000
	HEX 08400000
	HEX 0EA00000
	HEX 0F400000
	HEX 0FA00000
	HEX 0F800000
	HEX 08200000
	HEX 05500000
	HEX 06A00000
	HEX 07D00000
	HEX 07C80000
	HEX 04280000
	HEX 03540000
	HEX 03AA0000
	HEX 03F50000

BEECHTREES	HEX 03C0
	HEX 0CB8
	HEX 302C
	HEX 2556
	HEX 52BE
	HEX 45EE
	HEX 3ABB
	HEX 677D
	HEX 4BF9
	HEX A575
	HEX 8BFB
	HEX 95D5
	HEX 6FEA
	HEX 3FDC
	HEX 36FC
	HEX 31E8
	HEX 30D0
	HEX 10E0
	HEX 18C0
	HEX 18C0
	HEX 0880
	HEX 0C80
	HEX 0480
	HEX 0680
	HEX 0380
	HEX 0180
	HEX 0180
	HEX 0080
	HEX 0080
	HEX 0080
	HEX 0080
	HEX 01FC


BEECHTREEM	HEX 00F800
	HEX 038F00
	HEX 0E23C0
	HEX 190560
	HEX 142EE0
	HEX 315570
	HEX 22AFF0
	HEX 2975F0
	HEX 32BEF8
	HEX 3F57C8
	HEX 64FFA8
	HEX 41D7D8
	HEX AAFF88
	HEX 857F58
	HEX ABDE88
	HEX C57D58
	HEX 6FFEF0
	HEX 35DD60
	HEX 1FBFE0
	HEX 1BDFE0
	HEX 187CC0
	HEX 181BC0
	HEX 181F80
	HEX 0C1E00
	HEX 0C1C00
	HEX 0C1800
	HEX 0E1800
	HEX 061800
	HEX 063000
	HEX 073000
	HEX 033000
	HEX 03B000
	HEX 01F000
	HEX 00F000
	HEX 00F000
	HEX 007000
	HEX 007000
	HEX 007000
	HEX 003000
	HEX 003000
	HEX 003000
	HEX 003F00
	HEX 005FE0

BEECHTREEL	HEX 001F8000
	HEX 00F0F800
	HEX 03840E00
	HEX 06215700
	HEX 0D4AAB80
	HEX 18057580
	HEX 122BBBC0
	HEX 305D5EC0
	HEX 28ABABC0
	HEX 225F7FC0
	HEX 34EAEBE0
	HEX 1F5FFFA0
	HEX 39EEBF20
	HEX 627F7EA0
	HEX 48B6BE20
	HEX C15DFD20
	HEX A4FEFE60
	HEX 815FF520
	HEX 8AAFA8A0
	HEX A5FDF260
	HEX CAEFA840
	HEX 67DF72C0
	HEX 3AFBE980
	HEX 0FEFD700
	HEX 0EFBEF00
	HEX 0E1FFF00
	HEX 0A0F8E00
	HEX 0A039600
	HEX 0A03AC00
	HEX 0B03D800
	HEX 0703F000
	HEX 0503E000
	HEX 0503C000
	HEX 0503C000
	HEX 03838000
	HEX 02838000
	HEX 02C38000
	HEX 01C70000
	HEX 01670000
	HEX 00E70000
	HEX 00B70000
	HEX 005F0000
	HEX 002F0000
	HEX 001F0000
	HEX 00170000
	HEX 000F0000
	HEX 000F0000
	HEX 000F0000
	HEX 00070000
	HEX 00070000
	HEX 00070000
	HEX 00070000
	HEX 00070000
	HEX 000BF000
	HEX 0017FF00

BOULDERS	HEX 008000
	HEX 00F000
	HEX 015800
	HEX 013C00
	HEX 039780
	HEX 02EAC0
	HEX 065560
	HEX 0C7EE0
	HEX 192770
	HEX 120E90
	HEX 285070
	HEX 32ED28
	HEX 24451C
	HEX 28AC66
	HEX 6DC712
	HEX 489EAD
	HEX 454CFB
	HEX DB9EB7
	HEX 8D5DDF
	HEX EABAAF
	HEX 35D5DE
	HEX 1EEBFC
	HEX 0FFFF8
	HEX 01FF80

BOULDERM	HEX 00300000
	HEX 003E0000
	HEX 002B8000
	HEX 0065C000
	HEX 004BA000
	HEX 004D7000
	HEX 00DAFE00
	HEX 019DD700
	HEX 022EAB00
	HEX 068F5780
	HEX 053BE980
	HEX 0C6555C0
	HEX 095612C0
	HEX 0A453340
	HEX 180A2BA0
	HEX 112E5970
	HEX 120A0AB8
	HEX 311E59D8
	HEX 2678FAEC
	HEX 2535516C
	HEX 2E38FEB7
	HEX 2476F573
	HEX 66ACE7E9
	HEX C575D577
	HEX CE23EAAF
	HEX 6775DD57
	HEX 3BAB2ABE
	HEX 1F545F7E
	HEX 0AFAABFC
	HEX 07D5DFF8
	HEX 03FFFFC0
	HEX 007FFC00

BOULDERL	HEX 000C000000
	HEX 000B800000
	HEX 0009700000
	HEX 000AA80000
	HEX 0019580000
	HEX 0010AE0000
	HEX 0015770000
	HEX 0012AFF000
	HEX 00275F5800
	HEX 004BAAA800
	HEX 00A55DDE00
	HEX 010BFAAE00
	HEX 0195D74600
	HEX 022F1E8B00
	HEX 021C47D500
	HEX 05348AB980
	HEX 0410479D80
	HEX 0E94952EC0
	HEX 0801235760
	HEX 0949A2C8F0
	HEX 0D212A5170
	HEX 1917E0EE98
	HEX 11A352474C
	HEX 130EE6ABEC
	HEX 13A546C5C4
	HEX 370EAFAAEB
	HEX 229FEF65F7
	HEX 272E8FB7EF
	HEX 4295DF7757
	HEX 954EBEB2EF
	HEX 839F1D11DF
	HEX E92E3E4AFF
	HEX 70955D177F
	HEX 1A2E3A2AFE
	HEX 0D555557FC
	HEX 06AEAEBFF8
	HEX 07F75DFFF8
	HEX 03FFFFFFE0
	HEX 00FFFFFF80
	HEX 001FFFE000

LOGS	HEX 000380
	HEX 7E3D7E
	HEX B3D616
	HEX B4E381
	HEX 9A3AAB
	HEX B755F7
	HEX 7FFFFE

LOGM	HEX 00003800
	HEX 00004E00
	HEX 3FC1D1FC
	HEX 4AFE68E6
	HEX D54CB44B
	HEX B6222A05
	HEX B5178453
	HEX AEAAAAAB
	HEX 4DF557D6
	HEX 3FFFFFFC

LOGL	HEX 000001C000
	HEX 000002B000
	HEX 1FC01D4FF8
	HEX 6CFFF7C72E
	HEX 551F2B49C2
	HEX B6A381A093
	HEX AA12524425
	HEX 8AA8E82883
	HEX AB55755556
	HEX B6AAAAAAAB
	HEX 4F7D557D5E
	HEX 3FFFFFFFF8

FENCES	HEX 3C003C
	HEX FFFFFF
	HEX D75D75
	HEX AEBAEB
	HEX FFFFFF
	HEX 3C003C
	HEX 2C002C
	HEX FFFFFF
	HEX D75D75
	HEX AEBAEB
	HEX FFFFFF
	HEX 3C003C
	HEX 2C002C
	HEX FFFFFF
	HEX D75D75
	HEX AEBAEB
	HEX FFFFFF
	HEX 3C003C
	HEX 3C003C

FENCEM	HEX 1F8001F8
	HEX FFFFFFFF
	HEX EBEBF5F5
	HEX D7D7EBEB
	HEX AFAFD7D7
	HEX FFFFFFFF
	HEX 1F8001F8
	HEX 1B8001B8
	HEX 15800158
	HEX FFFFFFFF
	HEX EBEBF5F5
	HEX D7D7EBEB
	HEX AFAFD7D7
	HEX FFFFFFFF
	HEX 1F8001F8
	HEX 1B8001B8
	HEX 15800158
	HEX FFFFFFFF
	HEX EBEBF5F5
	HEX D7D7EBEB
	HEX AFAFD7D7
	HEX FFFFFFFF
	HEX 1F8001F8
	HEX 1F8001F8

FENCEL	HEX 0FE00007F0
	HEX 0AE0000570
	HEX FFFFFFFFFF
	HEX F57D5F57D5
	HEX EAFABEAFAB
	HEX D5F57D5F57
	HEX ABEAFABEAF
	HEX FFFFFFFFFF
	HEX 0FE00007F0
	HEX 0D600006B0
	HEX 0AE0000570
	HEX 0D600006B0
	HEX FFFFFFFFFF
	HEX F57D5F57D5
	HEX EAFABEAFAB
	HEX D5F57D5F57
	HEX ABEAFABEAF
	HEX FFFFFFFFFF
	HEX 0FE00007F0
	HEX 0D600006B0
	HEX 0AE0000570
	HEX 0D600006B0
	HEX FFFFFFFFFF
	HEX F57D5F57D5
	HEX EAFABEAFAB
	HEX D5F57D5F57
	HEX ABEAFABEAF
	HEX FFFFFFFFFF
	HEX 0FE00007F0
	HEX 0D600006B0
	HEX 0AE0000570
	HEX 0FE00007F0

ROCKS	HEX 0380
	HEX 0DC0
	HEX 32C0
	HEX FFC0

ROCKM	HEX 00C0
	HEX 0360
	HEX 0CB0
	HEX 3350
	HEX CAB0
	HEX 7FE0

ROCKL	HEX 001C
	HEX 0074
	HEX 01AA
	HEX 0616
	HEX 18AB
	HEX 6515
	HEX F1AB
	HEX 7FFE

SHRUBS	HEX 000400
	HEX 000C00
	HEX 001800
	HEX 001880
	HEX 001081
	HEX 043081
	HEX 053081
	HEX 093081
	HEX 4B7593
	HEX 4B7593
	HEX CB75B3
	HEX CB75B6
	HEX CBFF6E
	HEX C9BB6C
	HEX CDFECC
	HEX ED5FD8
	HEX 6DFFB2
	HEX 66FB76
	HEX 76DFEC
	HEX 37FEFA
	HEX 3F6FB4
	HEX 1BFF6C
	HEX 0DFFF8
	HEX 0FFFF0

SHRUBM	HEX 00004000
	HEX 0000C000
	HEX 00018000
	HEX 00018200
	HEX 00038200
	HEX 01030201
	HEX 03072201
	HEX 03072201
	HEX 02472603
	HEX 06472603
	HEX 064F2603
	HEX 46CF6E46
	HEX 46CB6E46
	HEX C6DB4E46
	HEX C6DBDCCE
	HEX C6DBDDCE
	HEX C6DEF99C
	HEX C6DAFB9C
	HEX C67EF33A
	HEX E37BF732
	HEX 636FEE72
	HEX 637FDCE6
	HEX 7337F5DE
	HEX 73BFBBFE
	HEX 3BBBDBBC
	HEX 39FFF774
	HEX 1DFDDFEC
	HEX 1EDEFE58
	HEX 0EFFF7D8
	HEX 076FFDF0
	HEX 03DFFBC0
	HEX 07FFFFE0

SHRUBL	HEX 0000060000
	HEX 00000C0000
	HEX 00000C0000
	HEX 0000180800
	HEX 0000180800
	HEX 0000180800
	HEX 0040380800
	HEX 00C0391801
	HEX 00C8311801
	HEX 0188311801
	HEX 0198333803
	HEX 0198B33883
	HEX 0198B23883
	HEX 0399F23887
	HEX 43B9F67186
	HEX 4331767186
	HEX C33374718E
	HEX C33374730E
	HEX C3337CE31C
	HEX E33B7CE71C
	HEX E33BEDE738
	HEX E3BBEDCE3A
	HEX E3BBEFCE72
	HEX 639BEF9C72
	HEX 719BFFBCE6
	HEX 71DBFF78E6
	HEX 71DBFEF1CC
	HEX 71DBFBE3DC
	HEX 39CFFFA7BC
	HEX 38EDFBEF7C
	HEX 3CEDFEDEFC
	HEX 1C6DBFBDE8
	HEX 1E77BB7FDC
	HEX 0F77DEDF98
	HEX 0FBAEAB9B0
	HEX 07BF7FB3B0
	HEX 03DDBFF760
	HEX 01EEFFEEC0
	HEX 00F9FFCF80
	HEX 03FFFFFFC0

BWALLS	HEX 000000
	HEX 03F800
	HEX 0FFE00
	HEX 0A0900
	HEX 3F5D80
	HEX 3FFFC0
	HEX 504140
	HEX BAEB30
	HEX FFFFE8
	HEX 820828
	HEX D75D64
	HEX FFFFFF
	HEX 904105
	HEX BAEBAD
	HEX FFFFFF
	HEX 820821
	HEX D75D75
	HEX FFFFFF
	HEX 7FFFFE

BWALLM	HEX 00000000
	HEX 007FE000
	HEX 03FFF800
	HEX 07A9AC00
	HEX 06828400
	HEX 1DD5D600
	HEX 1FFFFE00
	HEX 3A9A9B00
	HEX E82829C0
	HEX DD5D5D40
	HEX FFFFFFE0
	HEX A9A9A9B0
	HEX 82828298
	HEX D5D5D5DC
	HEX FFFFFFFF
	HEX 9A9A9A9B
	HEX A8282829
	HEX DD5D5D5D
	HEX FFFFFFFF
	HEX A9A9A9A9
	HEX 82828282
	HEX D5D5D5D5
	HEX FFFFFFFF
	HEX 7FFFFFFE

BWALLL	HEX 000FFC0000
	HEX 00301F8000
	HEX 00E52AC000
	HEX 0360586000
	HEX 02AAAAA000
	HEX 05755D7000
	HEX 0FFFFFF800
	HEX 0E0380F800
	HEX 14A5295C00
	HEX 6C0B02CC00
	HEX D555555700
	HEX AEABAAEB80
	HEX FFFFFFFFC0
	HEX 80E03C0760
	HEX 294A594AA0
	HEX 02C0B81670
	HEX 55555AAAB8
	HEX AAEABD575C
	HEX FFFFFFFFFF
	HEX 8E0380E03B
	HEX 94A5294A55
	HEX AC0B02C0B3
	HEX D555555555
	HEX AEABAAEABB
	HEX FFFFFFFFFF
	HEX C0701C0701
	HEX 94A5294A52
	HEX 8160581605
	HEX AAAAAAAAAA
	HEX D5755D5755
	HEX FFFFFFFFFF
	HEX 3FFFFFFFFF

FLAG1S	HEX 4000
	HEX F000
	HEX 5E00
	HEX 5F80
	HEX 4FE0
	HEX 4FF8
	HEX 4FFC
	HEX 4FFE
	HEX 4F1E
	HEX 5E07
	HEX F801
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 7FC0

FLAG1M	HEX 000000
	HEX 600000
	HEX FE0000
	HEX 6F8000
	HEX 6FF000
	HEX 67FC00
	HEX 67FF00
	HEX 67FF80
	HEX 67FFC0
	HEX 67FFC0
	HEX 67FFE0
	HEX 67E1E0
	HEX 6F8070
	HEX 6E0010
	HEX F80008
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 7FF800

FLAG1L	HEX 20000000
	HEX 50000000
	HEX FE000000
	HEX DF800000
	HEX 57F00000
	HEX 57FE0000
	HEX 53FFC000
	HEX 53FFF000
	HEX 53FFF800
	HEX 53FFFC00
	HEX 53FFFE00
	HEX 53FFFF00
	HEX 53FFFF00
	HEX 53FFFF80
	HEX 53FC1F80
	HEX 57E003C0
	HEX 578000C0
	HEX DF000060
	HEX FC000020
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 5F000000
	HEX 7FFF8000

FLAG2S	HEX 4000
	HEX F001
	HEX 5C03
	HEX 5F0E
	HEX 4FFE
	HEX 4FFC
	HEX 4FF8
	HEX 4FF0
	HEX 4FC0
	HEX 5F00
	HEX F800
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 4000
	HEX 7F80

FLAG2M	HEX 000000
	HEX 600000
	HEX FC0008
	HEX 6E0010
	HEX 6F8070
	HEX 67E1E0
	HEX 67FFE0
	HEX 67FFC0
	HEX 67FF80
	HEX 67FF00
	HEX 67FE00
	HEX 67F800
	HEX 6FC000
	HEX 6F0000
	HEX F80000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 600000
	HEX 7FF000

FLAG2L	HEX 20000000
	HEX 50000000
	HEX FC000020
	HEX DF000060
	HEX 578000C0
	HEX 57E003C0
	HEX 53FC1F80
	HEX 53FFFF80
	HEX 53FFFF00
	HEX 53FFFF00
	HEX 53FFFE00
	HEX 53FFFC00
	HEX 53FFF800
	HEX 53FFE000
	HEX 53FF8000
	HEX 57F80000
	HEX 57E00000
	HEX DF800000
	HEX FE000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 50000000
	HEX 5F000000
	HEX 7FFF8000

P100S	HEX E0000007
	HEX 3C00003C
	HEX 3FFFFFFC
	HEX 1FDC71F8
	HEX 1F9924F8
	HEX 1FDBAEF8
	HEX 1FDBAEF8
	HEX 1FD924F8
	HEX 1F8C71F8
	HEX 3FFFFFFC
	HEX 3C00003C
	HEX E0000007

P100M	HEX E000000000E0
	HEX 7F0000001FC0
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 0FF9E3E3FE00
	HEX 0FF1C1C1FE00
	HEX 0FF98888FE00
	HEX 0FF99C9CFE00
	HEX 0FF99C9CFE00
	HEX 0FF98888FE00
	HEX 0FF0C1C1FE00
	HEX 0FF0E3E3FE00
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 7F0000001FC0
	HEX E000000000E0

P100L	HEX C0000000000006
	HEX 7800000000003C
	HEX 1FE00000000FF0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 07FF3F0FC3FFC0
	HEX 07FE3E0781FFC0
	HEX 07FE3C6318FFC0
	HEX 07FF3CF33CFFC0
	HEX 07FF3CF33CFFC0
	HEX 07FF3CF33CFFC0
	HEX 07FF3CF33CFFC0
	HEX 07FF3C6318FFC0
	HEX 07FC0E0781FFC0
	HEX 07FC0F0FC3FFC0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 1FE00000000FF0
	HEX 7800000000003C
	HEX C0000000000006

P500S	HEX E0000007
	HEX 3C00003C
	HEX 3FFFFFFC
	HEX 1F0C71F8
	HEX 1F7924F8
	HEX 1F1BAEF8
	HEX 1FCBAEF8
	HEX 1FC924F8
	HEX 1F1C71F8
	HEX 3FFFFFFC
	HEX 3C00003C
	HEX E0000007

P500M	HEX E000000000E0
	HEX 7F0000001FC0
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 0FC071F1FE00
	HEX 0FC060E0FE00
	HEX 0FCFC4447E00
	HEX 0FC0CE4E7E00
	HEX 0FE04E4E7E00
	HEX 0FFE44447E00
	HEX 0FC060E0FE00
	HEX 0FC0F1F1FE00
	HEX 1FFFFFFFFF00
	HEX 1FFFFFFFFF00
	HEX 7F0000001FC0
	HEX E000000000E0

P500L	HEX C0000000000006
	HEX 7800000000003C
	HEX 1FE00000000FF0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 07F80787E1FFC0
	HEX 07F80703C0FFC0
	HEX 07F9FE318C7FC0
	HEX 07F9FE799E7FC0
	HEX 07F80E799E7FC0
	HEX 07FC06799E7FC0
	HEX 07FFE6799E7FC0
	HEX 07F9E6318C7FC0
	HEX 07F80703C0FFC0
	HEX 07FC0F87E1FFC0
	HEX 0FFFFFFFFFFFE0
	HEX 0FFFFFFFFFFFE0
	HEX 1FE00000000FF0
	HEX 7800000000003C
	HEX C0000000000006

; ------ Nice buggy-boy logo
LOGO	HEX 0000000000000003000000000000F0F90000000000000C9E0000000000000F7F
	HEX 00000000030F1FBF000000F08EC1E0F0000000011EFF3F1F08146281000080C0
	HEX 000000008040277F00000000000080C1000000000078F89800333F3E3F371717
	HEX F0F0FB7B3B3B71FD1C9C9E9E8ECECFCF0F7FFFE3F2707073031FBFB93C1CDCDC
	HEX C0C2EEEE870733F370787371717070B01E7EFFCFE7E7EEFF073F7F737B3939B9
	HEX 8084DCDCCECEE6E7E8E8E8E8E8E8E8681713030303010909DD9C9E8EDEFCF8E0
	HEX E7E7E7E3F77F7E387B39B9B8BD1F1F0EDECEEEEEEFC78703F1797938F7FFEF87
	HEX B0F3FFFF78783838FB7373717B3F3F3CBD9CDCDCDE8F0F07E3E3F171F0E0C000
	HEX 68E8ECFCFCFC7C7C020303000000000078E08000000000007E38000000000000
	HEX 1F0E0000000000008F07000000000000E381000000000000E080804020110A04
	HEX FE7F3F1E608000000301E01C03000000FFFEFC71C1010100FCF8E030503050E0
	HEX 4642424242424247474747474747474747424242420646464646464646464647
	HEX 4747474747474746464646464672727272727272727A4242427842427A727272
	HEX 72464672727272727272724242427A7842424242727272464672727272727272
	HEX 724242427A78427A7A42727272464672727272727272727272577A4242424242
	HEX 7A7A724646464646464646464646464747474747474747474646474242424242
	HEX 42424242424747474747474747474746

; ------ Hill character data
HILLDATA	HEX 00000000071D77FF
H02	HEX 000007FCAFFC7EFF
H03	HEX 073FFDCFE7FF5DFF
H04	HEX F01EE7B21ECFFB71
H05	HEX 000080C04060E0B1
H06	HEX 0000000001030EFF
H07	HEX 031F2CFF99BCFFFD
H08	HEX C070388CDCE676FE
H09	HEX 0103070F3F7FFFFF
H10	HEX BFFFFFFFFEFFFFFF
H11	HEX FFF1FFA7FEFFFFFF
H12	HEX F7FF1FCF77FDAFFF
H13	HEX FDF7FFFFFEFF3FFF
H14	HEX BBDF77F7DCEEFFFF
H15	HEX 37FFFFFFFF3FEFFF
H16	HEX 9FE677FFFDFEFFFF
H17	HEX 9BE9FCDFF37FBFFF
H18	HEX 0080E09CEF73FEFF
H19	HEX 0000000000E078FF
H20	HEX 00000001030D3FFF
H21	HEX 0F39EE73FDD7FEFF
H22	HEX 0080C0C060F0CCFF
H23	HEX 0000000000000306
H24	HEX 000000003CE7FED3
H25	HEX 0000000000C0782E
H26	HEX 0F1B3F2F7F7FFFFF
H27	HEX FDFEFFCFA7FDF7FF
H28	HEX F7DDCEFEF7FD3FFF
H29	HEX 0EF9ECFFDF6FBBFF
H30	HEX 00C0E0B0F8A8F4FF
;<24x08A>
	HEX 0000000217FF0000
	HEX 000038CE77FF0000
	HEX 0038DEEF7FFE0000
;<32x08A>
	HEX 000001031DFF0000
	HEX 000DDEEFFFFE0000
	HEX C0F6FB7D7DFF0000
	HEX 787EBEDFDFBE0000

	FREE	DF_PSEUDO-$
END	END

