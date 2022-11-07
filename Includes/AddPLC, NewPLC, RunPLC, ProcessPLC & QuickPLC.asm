; ---------------------------------------------------------------------------
; Subroutine to load pattern load cues (to queue pattern load requests)

; input:
;	d0.w = index of PLC list

;	uses d0.w

; usage:
;		moveq	#id_PLC_Explode,d0
;		jsr	(AddPLC).l
; ---------------------------------------------------------------------------

AddPLC:
		pushr	a1-a2					; save a1/a2 to stack
		lea	(PatternLoadCues).l,a1
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		lea	(a1,d0.w),a1				; jump to relevant PLC
		lea	(v_plc_buffer).w,a2			; PLC buffer space in RAM

	.findspace:
		tst.l	(a2)					; is first space available?
		beq.s	.copytoRAM				; if yes, branch
		addq.w	#sizeof_plc,a2				; if not, try next space
		bra.s	.findspace				; repeat until space is found (warning: there are $10 spaces, but it may overflow)
; ===========================================================================

.copytoRAM:
		move.w	(a1)+,d0				; get PLC item count
		bmi.s	.skip					; branch if -1 (i.e. 0 items)

	.loop:
		move.l	(a1)+,(a2)+
		move.w	(a1)+,(a2)+				; copy PLC to RAM
		dbf	d0,.loop				; repeat for all items in PLC

	.skip:
		popr	a1-a2					; restore a1/a2 from stack
		rts

; ---------------------------------------------------------------------------
; Subroutine to load pattern load cues (to queue pattern load requests) and
; clear the PLC buffer

; input:
;	d0.w = index of PLC list

;	uses d0.l
; ---------------------------------------------------------------------------

NewPLC:
		pushr	a1-a2					; save a1/a2 to stack
		lea	(PatternLoadCues).l,a1
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		lea	(a1,d0.w),a1				; jump to relevant PLC
		bsr.s	ClearPLC				; erase any data in PLC buffer space
		lea	(v_plc_buffer).w,a2
		move.w	(a1)+,d0				; get PLC item count
		bmi.s	.skip					; branch if -1 (i.e. 0 items)

	.loop:
		move.l	(a1)+,(a2)+
		move.w	(a1)+,(a2)+				; copy PLC to RAM
		dbf	d0,.loop				; repeat for all items in PLC

	.skip:
		popr	a1-a2					; restore a1/a2 from stack
		rts

; ---------------------------------------------------------------------------
; Subroutine to	clear the pattern load cue buffer

;	uses d0.l, a2
; ---------------------------------------------------------------------------

ClearPLC:
		lea	(v_plc_buffer).w,a2			; PLC buffer space in RAM
		moveq	#loops_to_clear_plc,d0

	.loop:
		clr.l	(a2)+
		dbf	d0,.loop				; clear RAM $F680-$F700
		rts

; ---------------------------------------------------------------------------
; Subroutine to	check the PLC buffer and begin decompression if it contains
; anything. ProcessPLC handles the actual decompression during VBlank

;	uses d0.l, d1.w, d2.w, d5.l, d6.l, d7.w, a0, a1, a3
; ---------------------------------------------------------------------------

RunPLC:
		tst.l	(v_plc_buffer).w			; does PLC buffer contain any items?
		beq.s	.exit					; if not, branch
		tst.w	(v_nem_tile_count).w
		bne.s	.exit
		movea.l	(v_plc_buffer).w,a0			; get pointer for Nemesis compressed graphics
		lea	(NemPCD_WriteRowToVDP).l,a3
		lea	(v_nem_gfx_buffer).w,a1
		move.w	(a0)+,d2				; get 1st word of Nemesis graphics header
		bpl.s	.normal_mode				; branch if 0-$7FFF
		adda.w	#NemPCD_WriteRowToVDP_XOR-NemPCD_WriteRowToVDP,a3 ; use XOR mode

	.normal_mode:
		andi.w	#$7FFF,d2				; clear highest bit
	if ~FixBugs
	; Vladikcomper:
	; This is done too early: this variable is also used as a flag 
	; to signal that there are PLCs to process, which means that as 
	; soon as this variable is set, PLC processing will occur during V-Int. 
	; If an interrupt occurs between here and the end of this function, 
	; then the PLC processor will begin without being fully
	; initialized, causing a crash. This can reliably be triggered by 
	; rolling past the signpost at the end of LZ1 and LZ2, where the 
	; downward shift of the screen, combined with the water level
	; being above scanline 96 and a hack to prevent the sound driver
	; from running on HBlank when it occurs above said scanline 
	; results in a high chance of the interrupt occurring while 
	; the PLC for the signpost is being processed.
	
	; This bug also exists in Sonic 2 and Sonic CD, although there
	; are no known ways to reliably trigger it in those games.
	; S3K eliminates this race condition by moving this instruction 
	; to the end of the function.

	
		move.w	d2,(v_nem_tile_count).w			; load tile count	
	endc			
		bsr.w	NemDec_BuildCodeTable
		move.b	(a0)+,d5				; get next byte of header
		asl.w	#8,d5					; move to high byte of word
		move.b	(a0)+,d5				; get final byte of header
		moveq	#$10,d6
		moveq	#0,d0
		move.l	a0,(v_plc_buffer).w			; save pointer to actual compressed data
		move.l	a3,(v_nem_mode_ptr).w			; save pointer to Nemesis normal/XOR code
		move.l	d0,(v_nem_repeat).w
		move.l	d0,(v_nem_pixel).w
		move.l	d0,(v_nem_d2).w
		move.l	d5,(v_nem_header).w
		move.l	d6,(v_nem_shift).w
	if FixBugs
	; See above.
		move.w	d2,(v_nem_tile_count).w			; load tile count	
	endc

	.exit:
		rts

; ---------------------------------------------------------------------------
; Subroutine to	decompress graphics listed in the PLC buffer

;	uses d0.l, d1.l, d2.l, d3.l, d4.l, d5.l, d6.l, d7.w, a0, a1, a3, a4, a5
; ---------------------------------------------------------------------------

nem_tile_count:	= 9

ProcessPLC:
		tst.w	(v_nem_tile_count).w			; has PLC execution begun?
		beq.w	ProcessPLC_Exit				; if not, branch
		move.w	#nem_tile_count,(v_nem_tile_count_frame).w ; 9 tiles per frame
		moveq	#0,d0
		move.w	(v_plc_buffer_dest).w,d0		; copy VRAM destination to d0
		addi.w	#nem_tile_count*sizeof_cell,(v_plc_buffer_dest).w ; update for next frame
		bra.s	ProcessPLC_Decompress

nem_tile_count:	= 3

ProcessPLC2:
		tst.w	(v_nem_tile_count).w			; has PLC execution begun?
		beq.s	ProcessPLC_Exit				; if not, branch
		move.w	#nem_tile_count,(v_nem_tile_count_frame).w ; 3 tiles per frame
		moveq	#0,d0
		move.w	(v_plc_buffer_dest).w,d0		; copy VRAM destination to d0
		addi.w	#nem_tile_count*sizeof_cell,(v_plc_buffer_dest).w ; update for next frame

ProcessPLC_Decompress:
		lea	(vdp_control_port).l,a4
		lsl.l	#2,d0
		lsr.w	#2,d0
		ori.w	#$4000,d0
		swap	d0					; convert VRAM address to VDP format
		move.l	d0,(a4)					; set address via control port
		subq.w	#4,a4					; a4 = vdp_data_port
		movea.l	(v_plc_buffer).w,a0			; load pointer for actual compressed data (sans header)
		movea.l	(v_nem_mode_ptr).w,a3
		move.l	(v_nem_repeat).w,d0
		move.l	(v_nem_pixel).w,d1
		move.l	(v_nem_d2).w,d2
		move.l	(v_nem_header).w,d5
		move.l	(v_nem_shift).w,d6
		lea	(v_nem_gfx_buffer).w,a1

	.loop_tile:
		movea.w	#8,a5
		bsr.w	NemPCD_NewRow
		subq.w	#1,(v_nem_tile_count).w			; decrement tile counter
		beq.s	ProcessPLC_Finish			; branch if 0
		subq.w	#1,(v_nem_tile_count_frame).w		; decrement tile per frame counter
		bne.s	.loop_tile				; branch if not 0

		move.l	a0,(v_plc_buffer).w			; save pointer to latest compressed data
		move.l	a3,(v_nem_mode_ptr).w
		move.l	d0,(v_nem_repeat).w
		move.l	d1,(v_nem_pixel).w
		move.l	d2,(v_nem_d2).w
		move.l	d5,(v_nem_header).w
		move.l	d6,(v_nem_shift).w

ProcessPLC_Exit:
		rts	
; ===========================================================================

ProcessPLC_Finish:
		lea	(v_plc_buffer).w,a0
		
	if FixBugs
		; Vladikcomper:
		; Shift the PLC buffer by the exact number of bytes in each cue (6)
		; using pairs of longword and word moves, and clear the last slot 
		; when finished, avoiding the bug described below. Also uses only 506
		; processor cycles instead of around 740.
		lea sizeof_plc(a0),a1 	 ; start of second slot in the queue
   		moveq   #(v_plc_buffer_end-v_plc_buffer-6)/6-1,d0 ; $E, number of loops needed to shift everything

	.loop:
		move.l  (a1)+,(a0)+		; shift the first 4 bytes...
		move.w  (a1)+,(a0)+		; ...then the final 2 bytes of the cue to the next slot
		dbf d0,.loop			; loop until entire buffer has been shifted
 
    	moveq   #0,d0
    	move.l  d0,(a0)+    ; clear the first longword... 
    	move.w  d0,(a0)+    ; ...and the final word of the last cue to prevent overcopying it
    	
	else
		; This shifts the PLC buffer using longword moves alone. However,
		; the total amount of data that needs to be shifted ($5A, the number 
		; of PLC slots minus one) is not divisible by a longword. 
		; Consequently, only $58 bytes are shifted; the final two bytes
		; (the VRAM offset of the $16th and final cue, are skipped.
		; Additionally, that $16th cue is not cleared, with the result that
		; if it is used, the part that isn't broken will get copied over 
		; until it fills the entire buffer, causing the PLC processor to get stuck in an 
		; infinite loop.
		
		moveq	#(v_plc_buffer_end-v_plc_buffer-6)/4-1,d0 ; $15

	.loop:
		move.l	sizeof_plc(a0),(a0)+	; shift contents of PLC buffer up 6 bytes
		dbf	d0,.loop					; repeat until everything has been shifted (but see the bug above)	
	endc
		rts

; ---------------------------------------------------------------------------
; Subroutine to	decompress graphics listed in a pattern load cue in a single
; frame

; input:
;	d0.w = index of PLC list

;	uses d0.l, d1.w, a1
; ---------------------------------------------------------------------------

QuickPLC:
		lea	(PatternLoadCues).l,a1			; load the PLC index
		add.w	d0,d0
		move.w	(a1,d0.w),d0
		lea	(a1,d0.w),a1
		move.w	(a1)+,d1				; get length of PLC

	.loop:
		movea.l	(a1)+,a0				; get compressed graphics pointer
		moveq	#0,d0
		move.w	(a1)+,d0				; get VRAM address
		lsl.l	#2,d0
		lsr.w	#2,d0
		ori.w	#$4000,d0
		swap	d0
		move.l	d0,(vdp_control_port).l			; converted VRAM address to VDP format
		bsr.w	NemDec					; decompress
		dbf	d1,.loop				; repeat for length of PLC
		rts
