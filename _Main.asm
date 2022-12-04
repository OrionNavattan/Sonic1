;  =========================================================================
; |           Sonic the Hedgehog Disassembly for Sega Mega Drive            |
;  =========================================================================

; Disassembly created by Hivebrain
; thanks to drx, Stealth, Esrael L.G. Neto and the Sonic Retro Github

; ===========================================================================

		opt	l.					; . is the local label symbol
		opt	ae-					; automatic evens are disabled by default
		opt	ws+					; allow statements to contain white-spaces
		opt	w+					; print warnings
		opt	m+					; do not expand macros - if enabled, this can break assembling

Main: 		group org(0)
		section MainProgram,Main

		include "Debugger.asm"
		include "Mega Drive.asm"
		include "Macros - More CPUs.asm"
		include "Macros.asm"
		include "sound\Sounds.asm"
		include "sound\Sound Equates.asm"
		include "Constants.asm"
		include "RAM Addresses.asm"
		include	"Start Positions.asm"
		include "Includes\Compatibility.asm"

		cpu	68000

EnableSRAM:	equ 0						; change to 1 to enable SRAM
BackupSRAM:	equ 0
AddressSRAM:	equ 0						; 0 = odd+even; 2 = even only; 3 = odd only

;	if ~def(Revision)					; bit-perfect check will automatically set this variable
;Revision:	equ 1
;	endc
	
FixBugs:	equ 1	; Set to 1 to fix various engine and gameplay bugs.

Optimize:	equ 1	; Apply a number of optimizations, including removing unnecessary branches and replacing jsr/rts and bsr/rts pairs with jmp and bra.

;RemoveGarbage equ 1 ; Removes many bits and pieces of unused and dead code

RestoreProtoDrums: equ 1

SpikeFix:	equ 1	; Set to 1 to modify spike behavior to match all later games.

RemoveSpeedCaps: equ 1 ; Self-explanatory.

SimultaneousPaletteFades: equ 1	; Modifies palette fades to be simultaneous on all three colors, as opposed to sequential.

ExtraLivesForPoints: equ 1 ; Extra lives will be awarded at 50000 point intervals for all regions.

BurntSprite: equ 1 ; Enables an unused burnt sprite for death from fire hazards, and changes harpoon death to use the spike sound.

RingChanges: equ 1 ; Scattered rings will have underwater physics, will no longer disappear at the wrap point of y-wrapped levels, 
; will bounce at the bottom of levels, have individual timers so that they will not accumulate if you are hurt multiple times in rapid succession,
; and flash briefly before disappearing.

SSVariableJump: equ 1 ; Enable variable jump height in the special stages

;SmoothSpecialStageRotation: equ 1 ; Enable smooth special stage rotation

RestoreProtoDrums: equ 1 ; Restores the higher pitched, gated-reverb sound of the drums as heard in the Sonic 1 prototype (you must also set this in the DAC driver).

LZMusic: equ 1 ; Adds an extra note to Labyrinth Zone's music that is in the demo version, but not in the final.

ResetOnFloor: equ 1


ZoneCount:	equ 6						; discrete zones are: GHZ, MZ, SYZ, LZ, SLZ, and SBZ

		include "File List.asm"
; ===========================================================================

ROM_Start:
Vectors:	
		dc.l v_stack_pointer&$FFFFFF			; Initial stack pointer value
		dc.l EntryPoint					; Start of program
		dc.l BusError					; Bus error
		dc.l AddressError				; Address error
		dc.l IllegalInstr				; Illegal instruction
		dc.l ZeroDivide					; Division by zero
		dc.l ChkInstr					; CHK exception
		dc.l TrapvInstr					; TRAPV exception
		dc.l PrivilegeViol				; Privilege violation
		dc.l Trace					; TRACE exception
		dc.l Line1010Emu				; Line-A emulator
		dc.l Line1111Emu				; Line-F emulator
		dcb.l 2,ErrorExcept				; Unused (reserved)
		dc.l ErrorExcept				; Format error
		dc.l ErrorExcept				; Uninitialized interrupt
		dcb.l 8,ErrorExcept				; Unused (reserved)
		dc.l ErrorExcept				; Spurious exception
		dc.l ErrorExcept					; IRQ level 1
		dc.l ErrorExcept					; IRQ level 2
		dc.l ErrorExcept					; IRQ level 3
		dc.l HBlank					; IRQ level 4 (horizontal retrace interrupt)
		dc.l ErrorExcept					; IRQ level 5
		dc.l VBlank					; IRQ level 6 (vertical retrace interrupt)
		dc.l ErrorExcept					; IRQ level 7
		dcb.l 16,ErrorExcept				; TRAP #00..#15 exceptions
		dcb.l 16,ErrorExcept				; Unused (reserved)

		dc.b "SEGA MEGA DRIVE "				; Hardware system ID (Console name)
		dc.b "(C)SEGA 1991.APR"				; Copyright holder and release date (generally year)
		dc.b "SONIC THE               HEDGEHOG                " ; Domestic name
		dc.b "SONIC THE               HEDGEHOG                " ; International name

;	if Revision=0
;		dc.b "GM 00001009-00"				; Serial/version number (Rev 0)
;	else
		dc.b "GM 00004049-01"				; Serial/version number (Rev non-0)
;	endc

Checksum: 	dc.w $0
		dc.b "J               "				; I/O support
ROM_Start_Ptr:	dc.l ROM_Start					; Start address of ROM
ROM_End_Ptr:	dc.l ROM_End-1					; End address of ROM
		dc.l $FF0000					; Start address of RAM
		dc.l $FFFFFF					; End address of RAM

	if EnableSRAM=1
		dc.b "RA", $A0+(BackupSRAM<<6)+(AddressSRAM<<3), $20
		dc.l $200000					; SRAM start
		dc.l $200FFF					; SRAM end
	else
		dc.l $20202020					; dummy values (SRAM disabled)
		dc.l $20202020					; SRAM start
		dc.l $20202020					; SRAM end
	endc

		dc.b "                                                    " ; Notes (unused, anything can be put in this space, but it has to be 52 bytes.)
		dc.b "JUE             "				; Region (Country code)
EndOfHeader:

; ===========================================================================
; Crash/Freeze the 68000. Unlike Sonic 2, Sonic 1 uses the 68000 for playing music, so it stops too

;ErrorTrap:
;		nop
;		nop
;		bra.s	ErrorTrap
; ===========================================================================

EntryPoint:
		disable_ints			; disable interrupts during setup; they will be reenabled by the Sega Screen
		lea SetupValues(pc),a0	; load setup array
		movem.l	(a0)+,a1-a3/a5/a6	; Z80 RAM start, work RAM start, Z80 bus request register, VDP data port, VDP control port
		movem.w (a0)+,d1/d2/d5	; VDP registers loop counter, first VDP register value ($8004), VDP register increment/value for Z80 stop and reset release ($100)
		moveq	#0,d4			; DMA fill and memory clearing value
		movea.l	d4,a4
		move.l	a4,usp			; clear user stack pointer

		tst.w	port_e_control_hi-z80_bus_request(a3)	; was this a soft reset?
		bne.w	.wait_DMA								; if so, skip the TMSS check
		
		move.b	console_version-z80_bus_request(a3),d3	; load hardware version
		asl.b	#4,d3									; get only hardware version ID (equ flag will be cleared if any of the first three bits are nonzero)
	
		beq.s	.wait_DMA								; if Model 1 VA4 or earlier (ID = 0), branch
		move.l	#'SEGA',tmss_sega-z80_bus_request(a3)	; satisfy the TMSS
		
	.wait_dma:	
		move.w	(a6),ccr		; get status register, resetting the 2nd write flag in the process
		bvs.s	.wait_dma		; if a DMA was in progress during a soft reset, wait until it is finished...
		
	.loop_vdp:
		move.w	d2,(a6)					; set VDP register
		add.w	d5,d2					; advance register ID
		move.b	(a0)+,d2				; load next register value 
		dbf	d1,.loop_vdp				; repeat for all registers ; final value loaded will be used later to initialize I/0 ports
		
		move.l	(a0)+,(a6)				; set DMA fill destination
		move.w	d4,(a5)					; set DMA fill value (0000), clearing the VRAM
		
		tst.w	port_e_control_hi-z80_bus_request(a3)		; was this a soft reset?
		bne.s	.clear_every_reset			; if so, skip clearing RAM addresses $FE00-$FFFF
		
		lea	(v_keep_after_reset).w,a4		; $FFFFFE00		
		move.w	#(($FFFFFFFF-v_keep_after_reset+1)/4)-1,d1	; repeat times
	.loop_ram1:
		move.l	d4,(a4)+
		dbf	d1,.loop_ram1				; clear RAM ($FE00-$FFFF)

	.clear_every_reset:
		move.w	#((v_keep_after_reset&$FFFF)/4)-1,d1	; set repeat times
	.loop_ram2:
		move.l	d4,(a2)+				; a2 = start of 68K RAM
		dbf	d1,.loop_ram2				; clear RAM ($0000-$FDFF)
		
		move.w	d5,(a3)									; stop the Z80 (we will clear the VSRAM and CRAM while waiting for it to stop)
		move.w	d5,z80_reset-z80_bus_request(a3)		; ensure Z80 reset is not set
	
		move.w	(a0)+,(a6) ; set VDP increment to 2
	
		move.l	(a0)+,(a6)		; set VDP to VSRAM write
		moveq	#(sizeof_vsram/4)-1,d1	; set repeat times
	.loop_vsram:	
		move.l	d4,(a5)		; clear 4 bytes of VSRAM
		dbf	d1,.loop_vsram	; repeat until entire VSRAM has been cleared	
		
		move.l	(a0)+,(a6)				; set VDP to CRAM write
		moveq	#(sizeof_pal_all/4)-1,d1	; set repeat times
	.loop_cram:	
		move.l	d4,(a5)		; clear two palette entries
		dbf	d1,.loop_cram	; repeat until entire CRAM has been cleared
		
	.waitz80:
		btst	d4,(a3)					; has the Z80 stopped?
		bne.s	.waitz80				; if not, branch
		
		move.w	#(Z80_Startup_end-Z80_Startup)-1,d1	; load size of Z80 startup program
	.load_z80:	
		move.b (a0)+,(a1)+ ; load Z80 startup program byte by byte into the Z80 RAM
		dbf d1,.load_z80
		
		move.w #(sizeof_z80_ram-(Z80_Startup_end-Z80_Startup))-1,d1	; size of remaining Z80 ram
	.clear_Z80_ram:	
		move.b d4,(a1)+ 	; clear the rest of Z80 RAM
		dbf	d1,.clear_Z80_ram

		move.w	d4,z80_reset-z80_bus_request(a3)		; reset Z80
		rept 4
		nop						; wait a little time to finish resetting
		endr
		
		move.w	d5,z80_reset-z80_bus_request(a3)		; release Z80 reset
		move.w	d4,(a3)									; start the Z80 
	
		moveq	#4-1,d1				; set number of PSG channels to mute 
	.psg_loop:
		move.b	(a0)+,psg_input-vdp_data_port(a6)		; set the PSG channel volume to null (no sound)
		dbf	d1,.psg_loop				; repeat for all channels	

	; Checksum check; delete everything from here to .init_joypads to remove
		tst.w	port_e_control_hi-z80_bus_request(a3)		; was this a soft reset?
		bne.w	.init_joypads		; if so,  skip the checksum check
	
		movea.l	#EndOfHeader,a1				; start	checking bytes after the header	($200)
		movea.l	#ROM_End_Ptr,a2				; stop at end of ROM
		move.l	(a2),d0

	.checksum_loop:
		add.w	(a1)+,d4	; add each word of the rom to d4
		cmp.l	a1,d0		; have we reached the end?
		bcc.s	.checksum_loop	; if not, branch
		
		movea.l	#Checksum,a1	; read checksum
		cmp.w	(a1),d4			; compare checksum in header to ROM
		beq.s	.init_joypads	; if they match, branch
		move.w	#cRed,(a5)		; set BG color to red
		;enable_display
		bra.s	* 				; stay here forever	
	
	.init_joypads:
		move.w	d2,port_1_control_hi-z80_bus_request(a3)				; initialise port 1
		move.w	d2,port_2_control_hi-z80_bus_request(a3)				; initialise port 2
		move.w	d2,port_e_control_hi-z80_bus_request(a3)				; initialise port e (last one)
		
		movem.l	-4(sp),d0-a6		; clear all registers
		
		move.b	(SetupVDP).l,d0	; get first entry of SetupVDP
		ori.w	#$8100,d0		; make it a valid command word ($8134)
		move.w	d0,(v_vdp_mode_buffer).w		; save to buffer for later use
		move.w	#vdp_hint_counter+(screen_height-1),(v_vdp_hint_counter).w ; horizontal interrupt every 224th scanline
		
		move.b	(console_version).l,d0
		andi.b	#console_region+console_speed,d0
		move.b	d0,(v_console_region).w			; get region setting
		
		bsr.w	DacDriverLoad		; load the DAC Driver
		move.b	#id_Sega,(v_gamemode).w			; set Game Mode to Sega Screen
		bra.s	MainGameLoop
		
SetupValues:
		dc.l	z80_ram
		dc.l	$FFFF0000	;ram_start 
		dc.l	z80_bus_request
		dc.l	vdp_data_port
		dc.l	vdp_control_port
		
		dc.w	SetupVDP_end-SetupVDP-1				; number of VDP registers to write
		dc.w	vdp_md_color		; $8004, normal color mode
		dc.w	vdp_mode_register2-vdp_mode_register1	; VDP Reg increment value & opposite initialisation flag for Z80
	SetupVDP:	
		dc.b	(vdp_enable_vint|vdp_enable_dma|vdp_ntsc_display|vdp_md_display)&$FF 
		dc.b	(vdp_fg_nametable+(vram_fg>>10))&$FF		; set foreground nametable address
		dc.b	(vdp_window_nametable+(vram_window>>10))&$FF	; set window nametable address
		dc.b	(vdp_bg_nametable+(vram_bg>>13))&$FF		; set background nametable address
		dc.b	(vdp_sprite_table+(vram_sprites>>9))&$FF		; set sprite table address
		dc.b	vdp_sprite_table2&$FF				; unused
		dc.b	(vdp_bg_color+0)&$FF			; set background colour (palette entry 0)
		dc.b	vdp_sms_hscroll&$FF				; unused
		dc.b 	vdp_sms_vscroll&$FF				; unused
		dc.b	(vdp_hint_counter+0)&$FF				; default horizontal interrupt register
		dc.b	(vdp_full_vscroll|vdp_full_hscroll)&$FF		; $8B00 ; full-screen vertical/horizontal scrolling
		dc.b	vdp_320px_screen_width&$FF			; $8C81 ; 40-cell display mode
		dc.b	(vdp_hscroll_table+(vram_hscroll>>10))&$FF	; set background hscroll address
		dc.b	vdp_nametable_hi&$FF				; unused
		dc.b 	(vdp_auto_inc+2)&$FF				; set VDP increment size
		dc.b	(vdp_plane_width_64|vdp_plane_height_32)&$FF	; $9001 ; 64x32 cell plane size
		dc.b	vdp_window_x_pos&$FF				; window horizontal position
		dc.b 	vdp_window_y_pos&$FF				; window vertical position

		dc.l 	(sizeof_vram/4)-1					; VDP $93/94 - DMA length
		dc.w 	0						; VDP $95/96 - DMA source
		dc.b 	vdp_dma_vram_fill&$FF			; VDP $97 - DMA fill VRAM
		
		dc.b	$40			; I/O port initialization value
		
	SetupVDP_end:	
	
		dc.l	$40000080	; DMA fill VRAM
		dc.w vdp_auto_inc+2				; VDP increment
		dc.l	$40000010				; VSRAM write mode
		dc.l	$C0000000				; CRAM write mode
		
		
	Z80_Startup:
		; Cut down from Sega's original: the 68K now zeros the Z80 RAM,
		; so this just handles clearing the registers.
		cpu	Z80
		obj 	0

		xor	a					; clear a
		ld	sp,.end				; set stack pointer to end of program (causing all of the pops to fill registers with 0)
		pop	ix					; clear all other registers
		pop	iy
		ld	i,a
		ld	r,a
		pop	de
		pop	hl
		pop	af
		
		
		ex	af,af				; swap af with af'
		exx						; swap bc, de, and hl
		pop	bc					; clear the shadow registers as well
		pop	de
		pop	hl
		pop	af
		ld	sp,hl				; clear stack pointer

		di						; disable interrupts
		jp *					; jump here to stay here forever

	.end:							; the space from here til end of Z80 RAM will be filled with 00's
		even						; align the Z80 start up code to the next even byte. Values below require alignment
		objend
		cpu 68000		
	Z80_Startup_end:
	
		dc.b	$9F,$BF,$DF,$FF				; PSG mute values (PSG 1 to 4)		

MainGameLoop:
		move.b	(v_gamemode).w,d0			; load Game Mode
		andi.w	#$1C,d0					; limit Game Mode value to $1C max (change to $7C to add more game modes)
		jsr	GameModeArray(pc,d0.w)			; jump to apt location in ROM
		bra.s	MainGameLoop				; loop indefinitely

; ---------------------------------------------------------------------------
; Main game mode array
; ---------------------------------------------------------------------------
gmptr:		macro
		id_\1:	equ *-GameModeArray
		bra.w	GM_\1
		endm

GameModeArray:
		gmptr Sega					; 0 - Sega screen (Includes\GM_Sega.asm)
		gmptr Title					; 4 - Title screen (Includes\GM_Title.asm)
		gmptr Demo					; 8 - Demo mode (Includes\GM_Level.asm - contains both demo and level)
		gmptr Level					; $C - Normal level (Includes\GM_Level.asm)
		gmptr Special					; $10 - Special stage (Includes\GM_Special.asm)
		gmptr Continue					; $14 - Continue screen (Includes\GM_Continue.asm)
		gmptr Ending					; $18 - End of game sequence (Includes\GM_Ending.asm)
		gmptr Credits					; $1C - Credits (Includes\GM_Credits.asm)
		rts
; ===========================================================================

;CheckSumError:
;		bsr.w	VDPSetupGame
;		move.l	#$C0000000,(vdp_control_port).l		; set VDP to CRAM write
;		moveq	#(sizeof_pal_all/2)-1,d7
;
;	.fillred:
;		move.w	#cRed,(vdp_data_port).l			; fill palette with red
;		dbf	d7,.fillred				; repeat $3F more times
;
;	.endlessloop:
;		bra.s	.endlessloop
; ===========================================================================


;		include	"Includes\Errors.asm"

		incfile	Art_Text

		include	"Includes\VBlank & HBlank.asm"
		include	"Includes\JoypadInit & ReadJoypads.asm"
;		include	"Includes\VDPSetupGame.asm"
		include	"Includes\ClearScreen.asm"
		include	"sound\PlaySound + DacDriverLoad.asm"
		include	"Includes\PauseGame.asm"
		include	"Includes\TilemapToVRAM.asm"

		include "Includes\Nemesis Decompression.asm"
		include "Includes\AddPLC, NewPLC, RunPLC, ProcessPLC & QuickPLC.asm"

		include "Includes\Enigma Decompression.asm"
		include "Includes\Kosinski Decompression.asm"

; ---------------------------------------------------------------------------
; Palette data & routines
; ---------------------------------------------------------------------------
		include "Includes\PaletteCycle.asm"
		incfile	Pal_TitleCyc
		incfile	Pal_GHZCyc
		incfile	Pal_LZCyc1
		incfile	Pal_LZCyc2
		incfile	Pal_LZCyc3
		incfile	Pal_SBZ3Cyc1
		incfile	Pal_MZCyc
		incfile	Pal_SLZCyc
		incfile	Pal_SYZCyc1
		incfile	Pal_SYZCyc2
		include_Pal_SBZCycList				; "Includes\PaletteCycle.asm"

		incfile	Pal_SBZCyc1
		incfile	Pal_SBZCyc2
		incfile	Pal_SBZCyc3
		incfile	Pal_SBZCyc4
		incfile	Pal_SBZCyc5
		incfile	Pal_SBZCyc6
		incfile	Pal_SBZCyc7
		incfile	Pal_SBZCyc8
		incfile	Pal_SBZCyc9
		incfile	Pal_SBZCyc10

	if SimultaneousPaletteFades = 1
		include "Includes\PaletteFadeIn, PaletteFadeOut, PaletteWhiteIn & PaletteWhiteOut (Simultaneous).asm"
	else
		include	"Includes\PaletteFadeIn, PaletteFadeOut, PaletteWhiteIn & PaletteWhiteOut.asm"
	endc
	
		include	"Includes\GM_Sega.asm"
		incfile	Pal_Sega1
		incfile	Pal_Sega2
		include "Includes\PalLoad & PalPointers.asm"
		incfile	Pal_SegaBG
		incfile	Pal_Title
		incfile	Pal_LevelSel
		incfile	Pal_Sonic
		incfile	Pal_GHZ
		incfile	Pal_LZ
		incfile	Pal_LZWater
		incfile	Pal_MZ
		incfile	Pal_SLZ
		incfile	Pal_SYZ
		incfile	Pal_SBZ1
		incfile	Pal_SBZ2
		incfile	Pal_Special
		incfile	Pal_SBZ3
		incfile	Pal_SBZ3Water
		incfile	Pal_LZSonWater
		incfile	Pal_SBZ3SonWat
		incfile	Pal_SSResult
		incfile	Pal_Continue
		incfile	Pal_Ending

		include "Includes\WaitForVBlank.asm"
		include "Objects\_RandomNumber.asm"
		include "Objects\_CalcSine & CalcAngle.asm"
Sine_Data:	incbin	"Misc Data\Sine & Cosine Waves.bin"	; values for a 256 degree sine wave
		incbin	"Misc Data\Sine & Cosine Waves.bin",,$80 ; contains duplicate data at the end!
		include_CalcAngle				; "Objects\_CalcSine & CalcAngle.asm"
Angle_Data:	incbin	"Misc Data\Angle Table.bin"

		include_Sega					; "Includes\GM_Sega.asm"
		include "Includes\Demo Pointers.asm"
		include "Includes\GM_Title.asm"

		include "Includes\GM_Level.asm"
		include "Includes\LZWaterFeatures.asm"

; ---------------------------------------------------------------------------
; Demo pointers and routines
; ---------------------------------------------------------------------------
		include "Includes\MoveSonicInDemo & DemoRecorder.asm"
		include_demo_pointers				; Includes\Demo Pointers.asm
;		incbin	"Demos\Unused.bin"			; unused demo data
		even

		include_Level_colptrs				; Includes\GM_Level.asm
		include "Includes\OscillateNumInit & OscillateNumDo.asm"
		include "Includes\SynchroAnimate.asm"
	
		include_Level_signpost				; Includes\GM_Level.asm

; ---------------------------------------------------------------------------
; Normal demo data
; ---------------------------------------------------------------------------
Demo_GHZ:	incbin	"Demos\Intro - GHZ.bin"
		even
Demo_MZ:	incbin	"Demos\Intro - MZ.bin"
		even
Demo_SYZ:	incbin	"Demos\Intro - SYZ.bin"
		even
Demo_SS:	incbin	"Demos\Intro - Special Stage.bin"
		even

		include "Includes\GM_Special.asm"

Pal_SSCyc1:	incbin	"Palettes\Cycle - Special Stage 1.bin"
		even
Pal_SSCyc2:	incbin	"Palettes\Cycle - Special Stage 2.bin"
		even

		include_Special_2				; Includes\GM_Special.asm

		include "Includes\GM_Continue.asm"

		include "Objects\Continue Screen Items.asm"	; ContScrItem
		include "Objects\Continue Screen Sonic.asm"	; ContSonic
		include "Objects\Continue Screen [Mappings].asm" ; Map_ContScr

		include "Includes\GM_Ending.asm"

		include "Objects\Ending Sonic.asm"		; EndSonic
		include "Objects\Ending Chaos Emeralds.asm"	; EndChaos
		include "Objects\Ending StH Text.asm"		; EndSTH

		include "Objects\Ending Sonic [Mappings].asm"	; Map_ESon
		include "Objects\Ending Chaos Emeralds [Mappings].asm" ; Map_ECha
		include "Objects\Ending StH Text [Mappings].asm" ; Map_ESth

		include "Includes\GM_Credits.asm"

		include "Objects\Ending Eggman Try Again.asm"	; EndEggman
		include "Objects\Ending Chaos Emeralds Try Again.asm" ; TryChaos
		include "Objects\Ending Eggman Try Again [Mappings].asm" ; Map_EEgg

; ---------------------------------------------------------------------------
; Ending demo data
; ---------------------------------------------------------------------------
Demo_EndGHZ1:	incbin	"Demos\Ending - GHZ1.bin"
		even
Demo_EndMZ:	incbin	"Demos\Ending - MZ.bin"
		even
Demo_EndSYZ:	incbin	"Demos\Ending - SYZ.bin"
		even
Demo_EndLZ:	incbin	"Demos\Ending - LZ.bin"
		even
Demo_EndSLZ:	incbin	"Demos\Ending - SLZ.bin"
		even
Demo_EndSBZ1:	incbin	"Demos\Ending - SBZ1.bin"
		even
Demo_EndSBZ2:	incbin	"Demos\Ending - SBZ2.bin"
		even
Demo_EndGHZ2:	incbin	"Demos\Ending - GHZ2.bin"
		even

		include	"Includes\LevelParameterLoad.asm"
		include	"Includes\DeformLayers.asm"
		include	"Includes\DrawTilesWhenMoving, DrawTilesAtStart & DrawChunks.asm"

		include "Includes\LevelDataLoad, LevelLayoutLoad & LevelHeaders.asm"
		include "Includes\DynamicLevelEvents.asm"

		include "Objects\GHZ Bridge.asm"		; Bridge

		include "Objects\_DetectPlatform.asm"
		include "Objects\_SlopeObject.asm"

		include "Objects\GHZ, MZ & SLZ Swinging Platforms, SBZ Ball on Chain.asm" ; SwingingPlatform
		
		include_Bridge_2				; Objects\GHZ Bridge.asm

		include "Objects\_ExitPlatform.asm"

		include_Bridge_3				; Objects\GHZ Bridge.asm
		include "Objects\GHZ Bridge [Mappings].asm"	; Map_Bri

		include_SwingingPlatform_1			; Objects\GHZ, MZ & SLZ Swinging Platforms, SBZ Ball on Chain.asm

		include "Objects\_MoveWithPlatform.asm"

		include_SwingingPlatform_2			; Objects\GHZ, MZ & SLZ Swinging Platforms, SBZ Ball on Chain.asm

		include "Objects\GHZ Boss Ball.asm"		; BossBall
		include_BossBall_2

		include_SwingingPlatform_3			; Objects\GHZ, MZ & SLZ Swinging Platforms, SBZ Ball on Chain.asm
		
		include "Objects\GHZ & MZ Swinging Platforms [Mappings].asm" ; Map_Swing_GHZ
		include "Objects\SLZ Swinging Platforms [Mappings].asm" ; Map_Swing_SLZ

		include "Objects\GHZ Spiked Helix Pole.asm"	; Helix
		include "Objects\GHZ Spiked Helix Pole [Mappings].asm" ; Map_Hel

		include "Objects\Platforms.asm"			; BasicPlatform
		include "Objects\Platforms [Mappings].asm"	; Map_Plat_Unused, Map_Plat_GHZ, Map_Plat_SYZ, Map_Plat_SLZ

		blankobj Obj19
		
		include "Objects\GHZ Giant Ball [Mappings].asm"	; Map_GBall

		include "Objects\GHZ Collapsing Ledge.asm"	; CollapseLedge
		include "Objects\MZ, SLZ & SBZ Collapsing Floors.asm" ; CollapseFloor
		include_CollapseLedge_2				; Objects\GHZ Collapsing Ledge.asm

		include_CollapseFloor_fragtiming

		include_SlopeObject_NoChk			; Objects\_SlopeObject.asm

Ledge_SlopeData:
		incbin	"Collision\GHZ Collapsing Ledge Heightmap.bin" ; used by CollapseLedge
		even

		include "Objects\GHZ Collapsing Ledge [Mappings].asm" ; Map_Ledge
		include "Objects\MZ, SLZ & SBZ Collapsing Floors [Mappings].asm" ; Map_CFlo

		include "Objects\GHZ Bridge Stump & SLZ Fireball Launcher.asm" ; Scenery
		include "Objects\SLZ Fireball Launcher [Mappings].asm" ; Map_Scen

		include "Objects\Unused Switch.asm"		; MagicSwitch
		include "Objects\Unused Switch [Mappings].asm"	; Map_Switch

		include "Objects\SBZ Door.asm"			; AutoDoor
		include "Objects\SBZ Door [Mappings].asm"	; Map_ADoor

		include "Objects\GHZ Walls.asm"			; EdgeWalls
		include_EdgeWalls_2

		include "Objects\Ball Hog.asm"			; BallHog
		include "Objects\Ball Hog Cannonball.asm"	; Cannonball

		include "Objects\Buzz Bomber Missile Vanishing.asm" ; MissileDissolve

		include "Objects\Explosions.asm"		; ExplosionItem & ExplosionBomb
		include_BallHog_animation
		include "Objects\Ball Hog [Mappings].asm"	; Map_Hog
		include "Objects\Buzz Bomber Missile Vanishing [Mappings].asm" ; Map_MisDissolve
		include "Objects\Explosions [Mappings].asm"	; Map_ExplodeItem & Map_ExplodeBomb

		include "Objects\Animals.asm"			; Animals
		include "Objects\Points.asm"			; Points
		include "Objects\Animals [Mappings].asm"	; Map_Animal1, Map_Animal2 & Map_Animal3
		include "Objects\Points [Mappings].asm"		; Map_Points

		include "Objects\Crabmeat.asm"			; Crabmeat
		include "Objects\Crabmeat [Mappings].asm"	; Map_Crab

		include "Objects\Buzz Bomber.asm"		; BuzzBomber
		include "Objects\Buzz Bomber Missile.asm"	; Missile
		include_BuzzBomber_animation
		include_Missile_animation
		include "Objects\Buzz Bomber [Mappings].asm"	; Map_Buzz
		include "Objects\Buzz Bomber Missile [Mappings].asm" ; Map_Missile

		include "Objects\Rings.asm"			; Rings
		include "Objects\_CollectRing.asm"
		include "Objects\Ring Loss.asm"			; RingLoss
		include "Objects\Giant Ring.asm"		; GiantRing
		include "Objects\Giant Ring Flash.asm"		; RingFlash
		include_Rings_animation
		include "Objects\Ring [Mappings].asm"		; Map_Ring
		include "Objects\Giant Ring [Mappings].asm"	; Map_GRing
		include "Objects\Giant Ring Flash [Mappings].asm" ; Map_Flash

		include "Objects\Monitors.asm"			; Monitor
		include "Objects\Monitor Contents.asm"		; PowerUp
		include_Monitor_2				; Objects\Monitors.asm
		include_Monitor_animation
		include "Objects\Monitors [Mappings].asm"	; Map_Monitor

		include "Objects\Title Screen Sonic.asm"	; TitleSonic
		include "Objects\Title Screen Press Start & TM.asm" ; PSBTM

		include_TitleSonic_animation
		include_PSBTM_animation

		include "Objects\_AnimateSprite.asm"

		include "Objects\Title Screen Press Start & TM [Mappings].asm" ; Map_PSB
		include "Objects\Title Screen Sonic [Mappings].asm" ; Map_TSon

		include "Objects\Chopper.asm"			; Chopper
		include "Objects\Chopper [Mappings].asm"	; Map_Chop

		include "Objects\Jaws.asm"			; Jaws
		include "Objects\Jaws [Mappings].asm"		; Map_Jaws

		include "Objects\Burrobot.asm"			; Burrobot
		include "Objects\Burrobot [Mappings].asm"	; Map_Burro

		include "Objects\MZ Grass Platforms.asm"	; LargeGrass
LGrass_Coll_Wide:
		incbin	"Collision\MZ Grass Platforms Heightmap (Wide).bin" ; used by LargeGrass
		even
LGrass_Coll_Narrow:
		incbin	"Collision\MZ Grass Platforms Heightmap (Narrow).bin" ; used by LargeGrass
		even
LGrass_Coll_Sloped:
		incbin	"Collision\MZ Grass Platforms Heightmap (Sloped).bin" ; used by LargeGrass
		even
		include "Objects\MZ Burning Grass.asm"		; GrassFire
		include "Objects\MZ Grass Platforms [Mappings].asm" ; Map_LGrass
		include "Objects\Fireballs [Mappings].asm"	; Map_Fire

		include "Objects\MZ Green Glass Blocks.asm"	; GlassBlock
		include "Objects\MZ Green Glass Blocks [Mappings].asm" ; Map_Glass

		include "Objects\MZ Chain Stompers.asm"		; ChainStomp
		;include "Objects\MZ Unused Sideways Stomper.asm" ; SideStomp
		include "Objects\MZ Chain Stompers [Mappings].asm" ; Map_CStom
		;include "Objects\MZ Unused Sideways Stomper [Mappings].asm" ; Map_SStom

		include "Objects\Button.asm"			; Button
		include "Objects\Button [Mappings].asm"		; Map_But

		include "Objects\MZ & LZ Pushable Blocks.asm"	; PushBlock
		include "Objects\MZ & LZ Pushable Blocks [Mappings].asm" ; Map_Push

		include "Objects\Title Cards.asm"		; TitleCard
		include "Objects\Game Over & Time Over.asm"	; GameOverCard
		include "Objects\Sonic Has Passed Title Card.asm" ; HasPassedCard

		include "Objects\Special Stage Results.asm"	; SSResult
		include "Objects\Special Stage Results Chaos Emeralds.asm" ; SSRChaos
		include "Objects\Title Cards [Mappings].asm"	; Map_Card
		include "Objects\Game Over & Time Over [Mappings].asm" ; Map_Over
		include "Objects\Title Cards Sonic Has Passed [Mappings].asm" ; Map_Has
		include "Objects\Special Stage Results [Mappings].asm" ; Map_SSR
		include "Objects\Special Stage Results Chaos Emeralds [Mappings].asm" ; Map_SSRC

		include "Objects\Spikes.asm"			; Spikes
		include "Objects\Spikes [Mappings].asm"		; Map_Spike

		include "Objects\GHZ Purple Rock.asm"		; PurpleRock
		include "Objects\GHZ Waterfall Sound.asm"	; WaterSound
		include "Objects\GHZ Purple Rock [Mappings].asm" ; Map_PRock

		include "Objects\GHZ & SLZ Smashable Walls & SmashObject.asm" ; SmashWall
		include "Objects\GHZ & SLZ Smashable Walls [Mappings].asm" ; Map_Smash

		include "Includes\ExecuteObjects & Object Pointers.asm"

NullObject:
		jmp	(DeleteObject).l ; It would be safer to have this instruction here, but instead it just falls through to ObjectFall

		include "Objects\_ObjectFall & SpeedToPos.asm"

		include "Objects\_DisplaySprite.asm"
		include "Objects\_DeleteObject & DeleteChild.asm"

		include "Includes\BuildSprites.asm"

		include "Objects\_CheckOffScreen.asm"

		include "Includes\ObjPosLoad.asm"
		include "Objects\_FindFreeObj & FindNextFreeObj.asm"

		include "Objects\Springs.asm"			; Springs
		include "Objects\Springs [Mappings].asm"	; Map_Spring

		include "Objects\Newtron.asm"			; Newtron
		include "Objects\Newtron [Mappings].asm"	; Map_Newt

		include "Objects\Roller.asm"			; Roller
		include "Objects\Roller [Mappings].asm"		; Map_Roll

		include_EdgeWalls_1				; Objects\GHZ Walls.asm
		include "Objects\GHZ Walls [Mappings].asm"	; Map_Edge

		include "Objects\MZ & SLZ Fireball Launchers.asm"
		include "Objects\Fireballs.asm"			; FireBall

		include "Objects\SBZ Flamethrower.asm"		; Flamethrower
		include "Objects\SBZ Flamethrower [Mappings].asm" ; Map_Flame

		include "Objects\MZ Purple Brick Block.asm"	; MarbleBrick
		include "Objects\MZ Purple Brick Block [Mappings].asm" ; Map_Brick

		include "Objects\SYZ Lamp.asm"			; SpinningLight
		include "Objects\SYZ Lamp [Mappings].asm"	; Map_Light

		include "Objects\SYZ Bumper.asm"		; Bumper
		include "Objects\SYZ Bumper [Mappings].asm"	; Map_Bump

		include "Objects\Signpost & HasPassedAct.asm"	; Signpost & HasPassedAct
		include "Objects\Signpost [Mappings].asm"	; Map_Sign

		include "Objects\MZ Lava Geyser Maker.asm"	; GeyserMaker
		include "Objects\MZ Lava Geyser.asm"		; LavaGeyser
		include "Objects\MZ Lava Wall.asm"		; LavaWall
		include "Objects\MZ Invisible Lava Tag.asm"	; LavaTag
		include "Objects\MZ Invisible Lava Tag [Mappings].asm" ; Map_LTag
		include_LavaGeyser_animation
		include_LavaWall_animation
		include "Objects\MZ Lava Geyser [Mappings].asm"	; Map_Geyser
		include "Objects\MZ Lava Wall [Mappings].asm"	; Map_LWall

		include "Objects\Moto Bug.asm"			; MotoBug
		include "Objects\_DespawnObject.asm"
		include_MotoBug_1
		include "Objects\Moto Bug [Mappings].asm"	; Map_Moto

		blankobj Obj4F

		include "Objects\Yadrin.asm"			; Yadrin
		include "Objects\Yadrin [Mappings].asm"		; Map_Yad

		include "Objects\_SolidObject.asm"

		include "Objects\MZ Smashable Green Block.asm"	; SmashBlock
		include "Objects\MZ Smashable Green Block [Mappings].asm" ; Map_Smab

		include "Objects\MZ, LZ & SBZ Moving Blocks.asm" ; MovingBlock
		include "Objects\MZ, LZ & SBZ Moving Blocks [Mappings].asm" ; Map_MBlock, Map_MBlockLZ

		include "Objects\Batbrain.asm"			; Batbrain
		include "Objects\Batbrain [Mappings].asm"	; Map_Bat

		include "Objects\SYZ & SLZ Floating Blocks, LZ Doors.asm" ; FloatingBlock
		include "Objects\SYZ & SLZ Floating Blocks, LZ Doors [Mappings].asm" ; Map_FBlock

		include "Objects\SYZ & LZ Spike Ball Chain.asm"	; SpikeBall
		include "Objects\SYZ & LZ Spike Ball Chain [Mappings].asm" ; Map_SBall, Map_SBall2

		include "Objects\SYZ Large Spike Balls.asm"	; BigSpikeBall
		include "Objects\SYZ & SBZ Large Spike Balls [Mappings].asm" ; Map_BBall

		include "Objects\SLZ Elevator.asm"		; Elevator
		include "Objects\SLZ Elevator [Mappings].asm"	; Map_Elev

		include "Objects\SLZ Circling Platform.asm"	; CirclingPlatform
		include "Objects\SLZ Circling Platform [Mappings].asm" ; Map_Circ

		include "Objects\SLZ Stairs.asm"		; Staircase
		include "Objects\SLZ Stairs [Mappings].asm"	; Map_Stair

		include "Objects\SLZ Pylon.asm"			; Pylon
		include "Objects\SLZ Pylon [Mappings].asm"	; Map_Pylon

		include "Objects\LZ Water Surface.asm"		; WaterSurface
		include "Objects\LZ Water Surface [Mappings].asm" ; Map_Surf

		include "Objects\LZ Pole.asm"			; Pole
		include "Objects\LZ Pole [Mappings].asm"	; Map_Pole

		include "Objects\LZ Flapping Door.asm"		; FlapDoor
		include "Objects\LZ Flapping Door [Mappings].asm" ; Map_Flap

		include "Objects\Invisible Solid Blocks.asm"	; Invisibarrier
		include "Objects\Invisible Solid Blocks [Mappings].asm" ; Map_Invis

		include "Objects\SLZ Fans.asm"			; Fan
		include "Objects\SLZ Fans [Mappings].asm"	; Map_Fan

		include "Objects\SLZ Seesaw.asm"		; Seesaw
See_DataSlope:	incbin	"Collision\SLZ Seesaw Heightmap (Sloped).bin" ; used by Seesaw
		even
See_DataFlat:	incbin	"Collision\SLZ Seesaw Heightmap (Flat).bin" ; used by Seesaw
		even
		include "Objects\SLZ Seesaw [Mappings].asm"	; Map_Seesaw
		include "Objects\SLZ Seesaw Spike Ball [Mappings].asm" ; Map_SSawBall

		include "Objects\Bomb Enemy.asm"		; Bomb
		include "Objects\Bomb Enemy [Mappings].asm"	; Map_Bomb

		include "Objects\Orbinaut.asm"			; Orbinaut
		include "Objects\Orbinaut [Mappings].asm"	; Map_Orb

		include "Objects\LZ Harpoon.asm"		; Harpoon
		include "Objects\LZ Harpoon [Mappings].asm"	; Map_Harp

		include "Objects\LZ Blocks.asm"			; LabyrinthBlock
		include "Objects\LZ Blocks [Mappings].asm"	; Map_LBlock

		include "Objects\LZ Gargoyle Head.asm"		; Gargoyle
		include "Objects\LZ Gargoyle Head [Mappings].asm" ; Map_Gar

		include "Objects\LZ Conveyor Belt Platforms.asm" ; LabyrinthConvey
		include "Objects\LZ Conveyor Belt Platforms [Mappings].asm" ; Map_LConv

		include "Objects\LZ Bubbles.asm"		; Bubble
		include "Objects\LZ Bubbles [Mappings].asm"	; Map_Bub

		include "Objects\LZ Waterfall.asm"		; Waterfall
		include "Objects\LZ Waterfall [Mappings].asm"	; Map_WFall

		include "Objects\Sonic.asm"			; SonicPlayer
		include "Objects\Sonic [Animations].asm"	; Ani_Sonic
		include_Sonic_1

		include "Objects\LZ Drowning Numbers.asm"	; DrownCount
		include "Objects\_ResumeMusic.asm"

		include_DrownCount_animation
		include "Objects\LZ Sonic's Drowning Face [Mappings].asm" ; Map_Drown

		include "Objects\Shield & Invincibility.asm"	; ShieldItem
		include "Objects\Unused Special Stage Warp.asm"	; VanishSonic
		include "Objects\LZ Water Splash.asm"		; Splash
		include_ShieldItem_animation
		include "Objects\Shield & Invincibility [Mappings].asm" ; Map_Shield
		include_VanishSonic_animation
		include "Objects\Unused Special Stage Warp [Mappings].asm" ; Map_Vanish
		include_Splash_animation
		include "Objects\LZ Water Splash [Mappings].asm" ; Map_Splash

		include_Sonic_2					; Objects\Sonic.asm
		include "Objects\_FindNearestTile, FindFloor & FindWall.asm"

		include	"Includes\ConvertCollisionArray.asm"

		include_Sonic_3					; Objects\Sonic.asm
		include "Objects\_FindFloorObj, FindWallRightObj, FindCeilingObj & FindWallLeftObj.asm"
		include_Sonic_4					; Objects\Sonic.asm
		include_FindWallRightObj			; Objects\_FindFloorObj, FindWallRightObj, FindCeilingObj & FindWallLeftObj.asm
		include_Sonic_5					; Objects\Sonic.asm
		include_FindCeilingObj				; Objects\_FindFloorObj, FindWallRightObj, FindCeilingObj & FindWallLeftObj.asm
		include_Sonic_6					; Objects\Sonic.asm
		include_FindWallLeftObj				; Objects\_FindFloorObj, FindWallRightObj, FindCeilingObj & FindWallLeftObj.asm

		include "Objects\SBZ Rotating Disc Junction.asm" ; Junction
		include "Objects\SBZ Rotating Disc Junction [Mappings].asm" ; Map_Jun

		include "Objects\SBZ Running Disc.asm"		; RunningDisc
		include "Objects\SBZ Running Disc [Mappings].asm" ; Map_Disc

		include "Objects\SBZ Conveyor Belt.asm"		; Conveyor
		include "Objects\SBZ Trapdoor & Spinning Platforms.asm" ; SpinPlatform
		include "Objects\SBZ Trapdoor & Spinning Platforms [Mappings].asm" ; Map_Trap, Map_Spin

		include "Objects\SBZ Saws.asm"			; Saws
		include "Objects\SBZ Saws [Mappings].asm"	; Map_Saw

		include "Objects\SBZ Stomper & Sliding Doors.asm" ; ScrapStomp
		include "Objects\SBZ Stomper & Sliding Doors [Mappings].asm" ; Map_Stomp

		include "Objects\SBZ Vanishing Platform.asm"	; VanishPlatform
		include "Objects\SBZ Vanishing Platform [Mappings].asm" ; Map_VanP

		include "Objects\SBZ Electric Orb.asm"		; Electro
		include "Objects\SBZ Electric Orb [Mappings].asm" ; Map_Elec

		include "Objects\SBZ Conveyor Belt Platforms.asm" ; SpinConvey

		include "Objects\SBZ Girder Block.asm"		; Girder
		include "Objects\SBZ Girder Block [Mappings].asm" ; Map_Gird

		include "Objects\SBZ Teleporter.asm"		; Teleport

		include "Objects\Caterkiller.asm"		; Caterkiller
		include "Objects\Caterkiller [Mappings].asm"	; Map_Cat

		include "Objects\Lamppost.asm"			; Lamppost
		include "Objects\Lamppost [Mappings].asm"	; Map_Lamp

		include "Objects\Hidden Bonus Points.asm"	; HiddenBonus
		include "Objects\Hidden Bonus Points [Mappings].asm" ; Map_Bonus

		include "Objects\Credits & Sonic Team Presents.asm" ; CreditsText
		include "Objects\Credits & Sonic Team Presents [Mappings].asm" ; Map_Cred

		include "Objects\GHZ Boss, BossExplode & BossMove.asm" ; BossGreenHill
		include_BossBall_1				; Objects\GHZ Boss Ball.asm; BossBall
		include "Objects\Bosses [Animations].asm"	; Ani_Bosses
		include "Objects\Bosses [Mappings].asm"		; Map_Bosses, Map_BossItems

		include "Objects\LZ Boss.asm"			; BossLabyrinth
		include "Objects\MZ Boss.asm"			; BossMarble
		include "Objects\MZ Boss Fire.asm"		; BossFire
		include "Objects\SLZ Boss.asm"			; BossStarLight
		include "Objects\SLZ Boss Spikeballs.asm"	; BossSpikeball
		include "Objects\SLZ Boss Spikeballs [Mappings].asm" ; Map_BSBall
		include "Objects\SYZ Boss.asm"			; BossSpringYard
		include "Objects\SYZ Blocks at Boss.asm"	; BossBlock
		include "Objects\SYZ Blocks at Boss [Mappings].asm" ; Map_BossBlock

		include "Objects\SBZ2 Blocks That Eggman Breaks.asm" ; FalseFloor
		include "Objects\SBZ2 Eggman.asm"		; ScrapEggman
		include "Objects\SBZ2 Eggman [Mappings].asm"	; Map_SEgg
		include_FalseFloor_1				; Objects\SBZ2 Blocks That Eggman Breaks.asm
		include "Objects\SBZ2 Blocks That Eggman Breaks [Mappings].asm" ; Map_FFloor

		include "Objects\FZ Boss.asm"			; BossFinal
		include "Objects\FZ Eggman in Damaged Ship [Mappings].asm" ; Map_FZDamaged
		include "Objects\FZ Eggman Ship Legs [Mappings].asm" ; Map_FZLegs

		include "Objects\FZ Cylinders.asm"		; EggmanCylinder
		include "Objects\FZ Cylinders [Mappings].asm"	; Map_EggCyl

		include "Objects\FZ Plasma Balls.asm"		; BossPlasma
		include "Objects\FZ Plasma Launcher [Mappings].asm" ; Map_PLaunch
		include_BossPlasma_animation
		include "Objects\FZ Plasma Balls [Mappings].asm" ; Map_Plasma

		include "Objects\Prison Capsule.asm"		; Prison
		include "Objects\Prison Capsule [Mappings].asm"	; Map_Pri

		include "Objects\_ReactToItem, HurtSonic & KillSonic.asm"

		include_Special_3				; Includes\GM_Special.asm
		include_Special_4				; Includes\GM_Special.asm

		include "Objects\Special Stage R [Mappings].asm" ; Map_SS_R
		include "Objects\Special Stage Breakable & Red-White Blocks [Mappings].asm" ; Map_SS_Glass
		include "Objects\Special Stage Up [Mappings].asm" ; Map_SS_Up
		include "Objects\Special Stage Down [Mappings].asm" ; Map_SS_Down
		include "Objects\Special Stage Chaos Emeralds [Mappings].asm" ; Map_SS_Chaos1, Map_SS_Chaos2 & Map_SS_Chaos3

		include "Objects\Special Stage Sonic.asm"	; SonicSpecial

		blankobj Obj10

		include "Includes\AnimateLevelGfx.asm"

		include "Objects\HUD.asm"			; HUD
		include "Objects\HUD Score, Time & Rings [Mappings].asm" ; Map_HUD

		include "Objects\_AddPoints.asm"

		include "Includes\HUD_Update, HUD_Base & ContScrCounter.asm"

; ---------------------------------------------------------------------------
; Uncompressed graphics	- HUD and lives counter
; ---------------------------------------------------------------------------
Art_Hud:	incbin	"Graphics\HUD Numbers.bin"		; 8x16 pixel numbers on HUD
		even
Art_LivesNums:	incbin	"Graphics\Lives Counter Numbers.bin"	; 8x8 pixel numbers on lives counter
		even

		include "Objects\_DebugMode.asm"

		include_levelheaders				; Includes\LevelDataLoad, LevelLayoutLoad & LevelHeaders.asm
		include "Pattern Load Cues.asm"

		align	$200,$FF
;		if Revision=0
;			incfile	Nem_SegaLogo
;	Eni_SegaLogo:	incbin	"Tilemaps\Sega Logo (REV00).eni" ; large Sega logo (mappings)
;			even
;		else
		dcb.b	$300,$FF
		incfile	Nem_SegaLogo

	Eni_SegaLogo:	incbin	"Tilemaps\Sega Logo.eni"	; large Sega logo (mappings)
		even
;		endc
Eni_Title:	incbin	"Tilemaps\Title Screen.eni"		; title screen foreground (mappings)
		even
		incfile	Nem_TitleFg
		incfile	Nem_TitleSonic
		incfile	Nem_TitleTM
Eni_JapNames:	incbin	"Tilemaps\Hidden Japanese Credits.eni"	; Japanese credits (mappings)
		even
		incfile	Nem_JapNames

		include "Objects\Sonic [Mappings].asm"		; Map_Sonic
		include "Objects\Sonic DPLCs.asm"		; SonicDynPLC

; ---------------------------------------------------------------------------
; Uncompressed graphics	- Sonic
; ---------------------------------------------------------------------------
Art_Sonic:	incbin	"Graphics\Sonic.bin"			; Sonic
		even
; ---------------------------------------------------------------------------
; Compressed graphics - various
; ---------------------------------------------------------------------------
;		if Revision=0
;			incfile	Nem_Smoke
;			incfile	Nem_SyzSparkle
;		endc
		incfile	Nem_Shield
		incfile	Nem_Stars
;		if Revision=0
;			incfile	Nem_LzSonic
;			incfile	Nem_UnkFire
;			incfile	Nem_Warp
;			incfile	Nem_Goggle
;		endc


		include "Objects\Special Stage Walls [Mappings].asm" ; Map_SSWalls

; ---------------------------------------------------------------------------
; Compressed graphics - special stage
; ---------------------------------------------------------------------------
		incfile	Nem_SSWalls
Eni_SSBg1:	incbin	"Tilemaps\SS Background 1.eni"		; special stage background (mappings)
		even
		incfile	Nem_SSBgFish
Eni_SSBg2:	incbin	"Tilemaps\SS Background 2.eni"		; special stage background (mappings)
		even
		incfile	Nem_SSBgCloud
		incfile	Nem_SSGOAL
		incfile	Nem_SSRBlock
		incfile	Nem_SS1UpBlock
		incfile	Nem_SSEmStars
		incfile	Nem_SSRedWhite
		incfile	Nem_SSZone1
		incfile	Nem_SSZone2
		incfile	Nem_SSZone3
		incfile	Nem_SSZone4
		incfile	Nem_SSZone5
		incfile	Nem_SSZone6
		incfile	Nem_SSUpDown
		incfile	Nem_SSEmerald
		incfile	Nem_SSGhost
		incfile	Nem_SSWBlock
		incfile	Nem_SSGlass
		incfile	Nem_ResultEm
; ---------------------------------------------------------------------------
; Compressed graphics - GHZ stuff
; ---------------------------------------------------------------------------
		incfile	Nem_Stalk
		incfile	Nem_Swing
		incfile	Nem_Bridge
		incfile	Nem_GhzUnkBlock
		incfile	Nem_Ball
		incfile	Nem_Spikes
		incfile	Nem_GhzUnkLog
		incfile	Nem_SpikePole
		incfile	Nem_PurpleRock
		incfile	Nem_GhzSmashWall
		incfile	Nem_GhzEdgeWall
; ---------------------------------------------------------------------------
; Compressed graphics - LZ stuff
; ---------------------------------------------------------------------------
		incfile	Nem_Water
		incfile	Nem_Splash
		incfile	Nem_LzSpikeBall
		incfile	Nem_FlapDoor
		incfile	Nem_Bubbles
		incfile	Nem_LzHalfBlock
		incfile	Nem_LzDoorV
		incfile	Nem_Harpoon
		incfile	Nem_LzPole
		incfile	Nem_LzDoorH
		incfile	Nem_LzWheel
		incfile	Nem_Gargoyle
		incfile	Nem_Sbz3HugeDoor
		incfile	Nem_LzPlatform
		incfile	Nem_Cork
		incfile	Nem_LzBlock
; ---------------------------------------------------------------------------
; Compressed graphics - MZ stuff
; ---------------------------------------------------------------------------
		incfile	Nem_MzMetal
		incfile	Nem_MzButton
		incfile	Nem_MzGlass
		incfile	Nem_MzUnkGrass
		incfile	Nem_Fireball
		incfile	Nem_Lava
		incfile	Nem_MzBlock
		incfile	Nem_MzUnkBlock
; ---------------------------------------------------------------------------
; Compressed graphics - SLZ stuff
; ---------------------------------------------------------------------------
		incfile	Nem_Seesaw
		incfile	Nem_SlzSpike
		incfile	Nem_Fan
		incfile	Nem_SlzWall
		incfile	Nem_Pylon
		incfile	Nem_SlzSwing
		incfile	Nem_SlzBlock
		incfile	Nem_SlzCannon
; ---------------------------------------------------------------------------
; Compressed graphics - SYZ stuff
; ---------------------------------------------------------------------------
		incfile	Nem_Bumper
		incfile	Nem_SmallSpike
		incfile	Nem_Button
		incfile	Nem_BigSpike
; ---------------------------------------------------------------------------
; Compressed graphics - SBZ stuff
; ---------------------------------------------------------------------------
		incfile	Nem_SbzDisc
		incfile	Nem_SbzJunction
		incfile	Nem_Cutter
		incfile	Nem_Stomper
		incfile	Nem_SpinPlatform
		incfile	Nem_TrapDoor
		incfile	Nem_SbzFloor
		incfile	Nem_Electric
		incfile	Nem_SbzBlock
		incfile	Nem_FlamePipe
		incfile	Nem_SbzDoorV
		incfile	Nem_SlideFloor
		incfile	Nem_SbzDoorH
		incfile	Nem_Girder
; ---------------------------------------------------------------------------
; Compressed graphics - enemies
; ---------------------------------------------------------------------------
		incfile	Nem_BallHog
		incfile	Nem_Crabmeat
		incfile	Nem_Buzz
		incfile	Nem_UnkExplode
		incfile	Nem_Burrobot
		incfile	Nem_Chopper
		incfile	Nem_Jaws
		incfile	Nem_Roller
		incfile	Nem_Motobug
		incfile	Nem_Newtron
		incfile	Nem_Yadrin
		incfile	Nem_Batbrain
		incfile	Nem_Splats
		incfile	Nem_Bomb
		incfile	Nem_Orbinaut
		incfile	Nem_Cater
; ---------------------------------------------------------------------------
; Compressed graphics - various
; ---------------------------------------------------------------------------
		incfile	Nem_TitleCard
		incfile	Nem_Hud
		incfile	Nem_Lives
		incfile	Nem_Ring
		incfile	Nem_Monitors
		incfile	Nem_Explode
		incfile	Nem_Points
		incfile	Nem_GameOver
		incfile	Nem_HSpring
		incfile	Nem_VSpring
		incfile	Nem_SignPost
		incfile	Nem_Lamp
		incfile	Nem_BigFlash
		incfile	Nem_Bonus
; ---------------------------------------------------------------------------
; Compressed graphics - continue screen
; ---------------------------------------------------------------------------
		incfile	Nem_ContSonic
		incfile	Nem_MiniSonic
; ---------------------------------------------------------------------------
; Compressed graphics - animals
; ---------------------------------------------------------------------------
		incfile	Nem_Rabbit
		incfile	Nem_Chicken
		incfile	Nem_BlackBird
		incfile	Nem_Seal
		incfile	Nem_Pig
		incfile	Nem_Flicky
		incfile	Nem_Squirrel
; ---------------------------------------------------------------------------
; Compressed graphics - primary patterns and block mappings
; ---------------------------------------------------------------------------
		incfile	Blk16_GHZ
		incfile	Nem_GHZ_1st
		incfile	Nem_GHZ_2nd
		incfile	Blk256_GHZ
		incfile	Blk16_LZ
		incfile	Nem_LZ
		incfile	Blk256_LZ
		incfile	Blk16_MZ
		incfile	Nem_MZ
		incfile	Blk256_MZ
		incfile	Blk16_SLZ
		incfile	Nem_SLZ
		incfile	Blk256_SLZ
		incfile	Blk16_SYZ
		incfile	Nem_SYZ
		incfile	Blk256_SYZ
		incfile	Blk16_SBZ
		incfile	Nem_SBZ
		incfile	Blk256_SBZ
; ---------------------------------------------------------------------------
; Compressed graphics - bosses and ending sequence
; ---------------------------------------------------------------------------
		incfile	Nem_Eggman
		incfile	Nem_Weapons
		incfile	Nem_Prison
		incfile	Nem_Sbz2Eggman
		incfile	Nem_FzBoss
		incfile	Nem_FzEggman
		incfile	Nem_Exhaust
		incfile	Nem_EndEm
		incfile	Nem_EndSonic
		incfile	Nem_TryAgain
;		if Revision=0
;			incfile	Nem_EndEggman
;		endc
		incfile	Kos_EndFlowers
		incfile	Nem_EndFlower
		incfile	Nem_CreditText
		incfile	Nem_EndStH


		;if Revision=0
		;	dcb.b $104,$FF				; why?
		;else
		dcb.b $40,$FF
		;endc
; ---------------------------------------------------------------------------
; Collision data
; ---------------------------------------------------------------------------
AngleMap:	incbin	"Collision\Angle Map.bin"
		even
CollArray1:	incbin	"Collision\Collision Array (Normal).bin"
		even
CollArray2:	incbin	"Collision\Collision Array (Rotated).bin"
		even
Col_GHZ:	incbin	"Collision\GHZ.bin"			; GHZ index
		even
Col_LZ:		incbin	"Collision\LZ.bin"			; LZ index
		even
Col_MZ:		incbin	"Collision\MZ.bin"			; MZ index
		even
Col_SLZ:	incbin	"Collision\SLZ.bin"			; SLZ index
		even
Col_SYZ:	incbin	"Collision\SYZ.bin"			; SYZ index
		even
Col_SBZ:	incbin	"Collision\SBZ.bin"			; SBZ index
		even
; ---------------------------------------------------------------------------
; Special Stage layouts
; ---------------------------------------------------------------------------
SS_1:		incbin	"Special Stage Layouts\1.eni"
		even
SS_2:		incbin	"Special Stage Layouts\2.eni"
		even
SS_3:		incbin	"Special Stage Layouts\3.eni"
		even
SS_4:		incbin	"Special Stage Layouts\4.eni"
		even
;		if Revision=0
;	SS_5:		incbin	"Special Stage Layouts\5 (REV00).eni"
;			even
;	SS_6:		incbin	"Special Stage Layouts\6 (REV00).eni"
;		else
	SS_5:	incbin	"Special Stage Layouts\5.eni"
		even
	SS_6:	incbin	"Special Stage Layouts\6.eni"
;		endc
		even
; ---------------------------------------------------------------------------
; Animated uncompressed graphics
; ---------------------------------------------------------------------------
Art_GhzWater:	incbin	"Graphics\GHZ Waterfall.bin"
		even
Art_GhzFlower1:	incbin	"Graphics\GHZ Flower Large.bin"
		even
Art_GhzFlower2:	incbin	"Graphics\GHZ Flower Small.bin"
		even
Art_MzLava1:	incbin	"Graphics\MZ Lava Surface.bin"
		even
Art_MzLava2:	incbin	"Graphics\MZ Lava.bin"
		even
Art_MzTorch:	incbin	"Graphics\MZ Background Torch.bin"
		even
Art_SbzSmoke:	incbin	"Graphics\SBZ Background Smoke.bin"
		even

; ---------------------------------------------------------------------------
; Level	layout index
; ---------------------------------------------------------------------------
Level_Index:	index offset(*)
		; GHZ
		ptr Level_GHZ1
		ptr Level_GHZ_bg
		ptr Level_GHZ1_unused
		
		ptr Level_GHZ2
		ptr Level_GHZ_bg
		ptr Level_GHZ2_unused
		
		ptr Level_GHZ3
		ptr Level_GHZ_bg
		ptr Level_GHZ3_unused
		
		ptr Level_GHZ4_unused
		ptr Level_GHZ4_unused
		ptr Level_GHZ4_unused
		
		; LZ
		ptr Level_LZ1
		ptr Level_LZ_bg
		ptr Level_LZ1_unused
		
		ptr Level_LZ2
		ptr Level_LZ_bg
		ptr Level_LZ2_unused
		
		ptr Level_LZ3
		ptr Level_LZ_bg
		ptr Level_LZ3_unused
		
		ptr Level_SBZ3
		ptr Level_LZ_bg
		ptr Level_SBZ3_unused
		
		; MZ
		ptr Level_MZ1
		ptr Level_MZ1bg
		ptr Level_MZ1
		
		ptr Level_MZ2
		ptr Level_MZ2bg
		ptr Level_MZ2_unused
		
		ptr Level_MZ3
		ptr Level_MZ3bg
		ptr Level_MZ3_unused
		
		ptr Level_MZ4_unused
		ptr Level_MZ4_unused
		ptr Level_MZ4_unused
		
		; SLZ
		ptr Level_SLZ1
		ptr Level_SLZ_bg
		ptr Level_SLZ_unused
		
		ptr Level_SLZ2
		ptr Level_SLZ_bg
		ptr Level_SLZ_unused
		
		ptr Level_SLZ3
		ptr Level_SLZ_bg
		ptr Level_SLZ_unused
		
		ptr Level_SLZ_unused
		ptr Level_SLZ_unused
		ptr Level_SLZ_unused
		
		; SYZ
		ptr Level_SYZ1
		ptr Level_SYZ_bg
		ptr Level_SYZ1_unused
		
		ptr Level_SYZ2
		ptr Level_SYZ_bg
		ptr Level_SYZ2_unused
		
		ptr Level_SYZ3
		ptr Level_SYZ_bg
		ptr Level_SYZ3_unused
		
		ptr Level_SYZ4_unused
		ptr Level_SYZ4_unused
		ptr Level_SYZ4_unused
		
		; SBZ
		ptr Level_SBZ1
		ptr Level_SBZ1bg
		ptr Level_SBZ1bg
		
		ptr Level_SBZ2
		ptr Level_SBZ2bg
		ptr Level_SBZ2bg
		
		; FZ
		ptr Level_SBZ2
		ptr Level_SBZ2bg
		ptr Level_SBZ2_unused
		
		ptr Level_SBZ4_unused
		ptr Level_SBZ4_unused
		ptr Level_SBZ4_unused
		zonewarning Level_Index,24
		
		; Ending
		ptr Level_End
		ptr Level_GHZ_bg
		ptr Level_End_unused
		
		ptr Level_End
		ptr Level_GHZ_bg
		ptr Level_End_unused
		
		ptr Level_End_unused
		ptr Level_End_unused
		ptr Level_End_unused
		
		ptr Level_End_unused
		ptr Level_End_unused
		ptr Level_End_unused

Level_GHZ1:	incbin	"Level Layouts\ghz1.bin"
		even
Level_GHZ1_unused:	dc.b 0,	0, 0, 0
Level_GHZ2:	incbin	"Level Layouts\ghz2.bin"
		even
Level_GHZ2_unused:	dc.b 0,	0, 0, 0
Level_GHZ3:	incbin	"Level Layouts\ghz3.bin"
		even
Level_GHZ_bg:	incbin	"Level Layouts\ghzbg.bin"
		even
Level_GHZ3_unused:	dc.b 0,	0, 0, 0
Level_GHZ4_unused:	dc.b 0,	0, 0, 0

Level_LZ1:	incbin	"Level Layouts\lz1.bin"
		even
Level_LZ_bg:	incbin	"Level Layouts\lzbg.bin"
		even
Level_LZ1_unused:	dc.b 0,	0, 0, 0
Level_LZ2:	incbin	"Level Layouts\lz2.bin"
		even
Level_LZ2_unused:	dc.b 0,	0, 0, 0
Level_LZ3:	incbin	"Level Layouts\lz3.bin"
		even
Level_LZ3_unused:	dc.b 0,	0, 0, 0
Level_SBZ3:	incbin	"Level Layouts\sbz3.bin"
		even
Level_SBZ3_unused:	dc.b 0,	0, 0, 0

Level_MZ1:	incbin	"Level Layouts\mz1.bin"
		even
Level_MZ1bg:	incbin	"Level Layouts\mz1bg.bin"
		even
Level_MZ2:	incbin	"Level Layouts\mz2.bin"
		even
Level_MZ2bg:	incbin	"Level Layouts\mz2bg.bin"
		even
Level_MZ2_unused:	dc.b 0,	0, 0, 0
Level_MZ3:	incbin	"Level Layouts\mz3.bin"
		even
Level_MZ3bg:	incbin	"Level Layouts\mz3bg.bin"
		even
Level_MZ3_unused:	dc.b 0,	0, 0, 0
Level_MZ4_unused:	dc.b 0,	0, 0, 0

Level_SLZ1:	incbin	"Level Layouts\slz1.bin"
		even
Level_SLZ_bg:	incbin	"Level Layouts\slzbg.bin"
		even
Level_SLZ2:	incbin	"Level Layouts\slz2.bin"
		even
Level_SLZ3:	incbin	"Level Layouts\slz3.bin"
		even
Level_SLZ_unused:	dc.b 0,	0, 0, 0

Level_SYZ1:	incbin	"Level Layouts\syz1.bin"
		even
Level_SYZ_bg:	;if Revision=0
;			incbin	"Level Layouts\syzbg.bin"
;		else
			incbin	"Level Layouts\syzbg (JP1).bin"
;		endc
		even
Level_SYZ1_unused:	dc.b 0,	0, 0, 0
Level_SYZ2:	incbin	"Level Layouts\syz2.bin"
		even
Level_SYZ2_unused:	dc.b 0,	0, 0, 0
Level_SYZ3:	incbin	"Level Layouts\syz3.bin"
		even
Level_SYZ3_unused:	dc.b 0,	0, 0, 0
Level_SYZ4_unused:	dc.b 0,	0, 0, 0

Level_SBZ1:	incbin	"Level Layouts\sbz1.bin"
		even
Level_SBZ1bg:	incbin	"Level Layouts\sbz1bg.bin"
		even
Level_SBZ2:	incbin	"Level Layouts\sbz2.bin"
		even
Level_SBZ2bg:	incbin	"Level Layouts\sbz2bg.bin"
		even
Level_SBZ2_unused:	dc.b 0,	0, 0, 0
Level_SBZ4_unused:	dc.b 0,	0, 0, 0
Level_End:	incbin	"Level Layouts\ending.bin"
		even
Level_End_unused:	dc.b 0,	0, 0, 0

		incfile	Art_BigRing

		align	$100,$FF

; ---------------------------------------------------------------------------
; Object position index
; ---------------------------------------------------------------------------
ObjPos_Index:	index offset(*)
		; GHZ
		ptr ObjPos_GHZ1
		ptr ObjPos_Null
		ptr ObjPos_GHZ2
		ptr ObjPos_Null
		ptr ObjPos_GHZ3
		ptr ObjPos_Null
		ptr ObjPos_GHZ1
		ptr ObjPos_Null
		; LZ
		ptr ObjPos_LZ1
		ptr ObjPos_Null
		ptr ObjPos_LZ2
		ptr ObjPos_Null
		ptr ObjPos_LZ3
		ptr ObjPos_Null
		ptr ObjPos_SBZ3
		ptr ObjPos_Null
		; MZ
		ptr ObjPos_MZ1
		ptr ObjPos_Null
		ptr ObjPos_MZ2
		ptr ObjPos_Null
		ptr ObjPos_MZ3
		ptr ObjPos_Null
		ptr ObjPos_MZ1
		ptr ObjPos_Null
		; SLZ
		ptr ObjPos_SLZ1
		ptr ObjPos_Null
		ptr ObjPos_SLZ2
		ptr ObjPos_Null
		ptr ObjPos_SLZ3
		ptr ObjPos_Null
		ptr ObjPos_SLZ1
		ptr ObjPos_Null
		; SYZ
		ptr ObjPos_SYZ1
		ptr ObjPos_Null
		ptr ObjPos_SYZ2
		ptr ObjPos_Null
		ptr ObjPos_SYZ3
		ptr ObjPos_Null
		ptr ObjPos_SYZ1
		ptr ObjPos_Null
		; SBZ
		ptr ObjPos_SBZ1
		ptr ObjPos_Null
		ptr ObjPos_SBZ2
		ptr ObjPos_Null
		ptr ObjPos_FZ
		ptr ObjPos_Null
		ptr ObjPos_SBZ1
		ptr ObjPos_Null
		zonewarning ObjPos_Index,$10
		; Ending
		ptr ObjPos_Ending
		ptr ObjPos_Null
		ptr ObjPos_Ending
		ptr ObjPos_Null
		ptr ObjPos_Ending
		ptr ObjPos_Null
		ptr ObjPos_Ending
		ptr ObjPos_Null
		; --- Put extra object data here. ---
ObjPosLZPlatform_Index:
		ptr ObjPos_LZ1pf1
		ptr ObjPos_LZ1pf2
		ptr ObjPos_LZ2pf1
		ptr ObjPos_LZ2pf2
		ptr ObjPos_LZ3pf1
		ptr ObjPos_LZ3pf2
		ptr ObjPos_LZ1pf1
		ptr ObjPos_LZ1pf2
ObjPosSBZPlatform_Index:
		ptr ObjPos_SBZ1pf1
		ptr ObjPos_SBZ1pf2
		ptr ObjPos_SBZ1pf3
		ptr ObjPos_SBZ1pf4
		ptr ObjPos_SBZ1pf5
		ptr ObjPos_SBZ1pf6
		ptr ObjPos_SBZ1pf1
		ptr ObjPos_SBZ1pf2
		endobj
		
		include "Object Subtypes.asm"
		include	"Object Placement\GHZ1.asm"
		include	"Object Placement\GHZ2.asm"
		include	"Object Placement\GHZ3.asm"
		include	"Object Placement\LZ1.asm"
		include	"Object Placement\LZ2.asm"
		include	"Object Placement\LZ3.asm"
		include	"Object Placement\SBZ3.asm"
		include	"Object Placement\LZ Platforms.asm"
		include	"Object Placement\MZ1.asm"
		include	"Object Placement\MZ2.asm"
		include	"Object Placement\MZ3.asm"
		include	"Object Placement\SLZ1.asm"
		include	"Object Placement\SLZ2.asm"
		include	"Object Placement\SLZ3.asm"
		include	"Object Placement\SYZ1.asm"
		include	"Object Placement\SYZ2.asm"
		include	"Object Placement\SYZ3.asm"
		include	"Object Placement\SBZ1.asm"
		include	"Object Placement\SBZ2.asm"
		include	"Object Placement\FZ.asm"
		include	"Object Placement\SBZ Platforms.asm"
		include	"Object Placement\Ending.asm"
ObjPos_Null:	endobj

		;if Revision=0
			;dcb.b $62A,$FF
		;else
			dcb.b $63C,$FF
		;endc
		;dcb.b ($10000-(*%$10000))-(ROM_End-SoundDriver),$FF

; ---------------------------------------------------------------------------
; Sound driver data
; ---------------------------------------------------------------------------
		include "sound/Sound Data.asm"
		
		even
; --------------------------------------------------------------
; Debugging modules
; --------------------------------------------------------------

   		include   "ErrorHandler.asm"

		even	

ROM_End:
		END
