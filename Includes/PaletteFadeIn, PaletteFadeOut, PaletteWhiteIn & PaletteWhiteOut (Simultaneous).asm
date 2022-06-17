; ---------------------------------------------------------------------------
; Subroutine to	fade in from black

;	uses d0, d1, d2, d3, d4, d5, d6, a0, a1
; ---------------------------------------------------------------------------

PaletteFadeIn:
		move.w	#palfade_all,(v_palfade_start).w	; set start position = 0; size = $40 ($3F)

PalFadeIn_Alt:							; start position and size are already set
		moveq	#0,d0
		lea	(v_pal_dry).w,a0
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		moveq	#cBlack,d1
		move.b	(v_palfade_size).w,d0

	@fill:
		move.w	d1,(a0)+
		dbf	d0,@fill			; fill pallete with black
		moveq	#$E,d4					; prepare maximum color check
		moveq	#$0,d6					; clear d6

	@mainloop:
		bsr.w	RunPLC 			; decompress gfx if PLC contains anything
		move.b	#id_VBlank_Fade,(v_vblank_routine).w
		bsr.w	WaitforVBlank			; wait for frame to end
		bchg	#$00,d6					; change delay counter
		beq		@mainloop				; if null, delay a frame
		bsr.s	FadeIn_FromBlack		; update palette
		subq.b	#$02,d4					; decrease color check
		bne		@mainloop				; if it has not reached zero, branch
		move.b	#id_VBlank_Fade,(v_vblank_routine).w			; wait for V-blank again (so colors transfer)
		bra		WaitforVBlank			

; ===========================================================================

FadeIn_FromBlack:				
		moveq	#0,d0
		lea	(v_pal_dry).w,a0			; current palette (starts as all black)
		lea	(v_pal_dry_next).w,a1		; target palette
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		adda.w	d0,a1
		move.b	(v_palfade_size).w,d0

	@addcolor:
		bsr.s	FadeIn_AddColor		; raise RGB levels (until they match target palette)
		dbf	d0,@addcolor				; repeat for size of palette
		cmpi.b	#id_LZ,(v_zone).w		; is level LZ or SBZ3?
		bne.s	@exit				; if not, branch
		moveq	#0,d0
		lea	(v_pal_water).w,a0
		lea	(v_pal_water_next).w,a1
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		adda.w	d0,a1
		move.b	(v_palfade_size).w,d0

	@addcolor_water:
		bsr.s	FadeIn_AddColor			; raise RGB levels for underwater palette
		dbf	d0,@addcolor_water			; repeat for size of palette

	@exit:
		rts	
		
; ===========================================================================

FadeIn_AddColor:				
		move.b	(a1),d5					; load blue
		move.w	(a1)+,d1				; load green and red
		move.b	d1,d2					; load red
		lsr.b	#$04,d1					; get only green
		andi.b	#cRed,d2				; get only red
		move.w	(a0),d3					; load current colour in buffer
		cmp.b	d5,d4					; is it time for blue to fade?
		bhi		@noblue					; if not, branch
		addi.w	#$0200,d3				; increase blue

	@noblue:
		cmp.b	d1,d4					; is it time for green to fade?
		bhi	@nogreen					; if not, branch
		addi.b	#$20,d3					; increase green
		
	@nogreen:
		cmp.b	d2,d4					; is it time for red to fade?
		bhi	@nored						; if not, branch
		addq.b	#$02,d3					; increase red
		
	@nored:
		move.w	d3,(a0)+				; save colour
		rts								; return

; ---------------------------------------------------------------------------
; Subroutine to fade out to black

;	uses d0, d1, d2, d3, d4, d5 d6, a0
; ---------------------------------------------------------------------------

PaletteFadeOut:
		move.w	#palfade_all,(v_palfade_start).w  ; set start position = 0; size = $40 ($3F)
		moveq	#$07,d4					; set repeat times
		moveq	#$00,d6					; clear d6

	@mainloop:
		bsr.w	RunPLC					; decompress gfx if PLC contains anything
		move.b	#id_VBlank_Fade,(v_vblank_routine).w
		bsr.w	WaitforVBlank			; wait for frame to end
		bchg	#$00,d6					; change delay counter
		beq		@mainloop				; if null, delay a frame
		bsr.s	FadeOut_ToBlack 		; update palette
		dbf	d4,@mainloop				; repeat until palette has faded completely to black
		rts	

; ===========================================================================

FadeOut_ToBlack:				
		moveq	#0,d0
		lea	(v_pal_dry).w,a0			; current palette
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		move.b	(v_palfade_size).w,d0

	@decolor:
		bsr.s	FadeOut_DecColor			; lower RGB levels
		dbf	d0,@decolor					; repeat for size of palette
		cmpi.b	#id_LZ,(v_zone).w		; is level LZ or SBZ3?
		bne.s	@exit				; if not, branch
		moveq	#0,d0
		lea	(v_pal_water).w,a0
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		move.b	(v_palfade_size).w,d0

	@decolor_water:
		bsr.s	FadeOut_DecColor
		dbf	d0,@decolor_water
		
	@exit:	
		rts	

; ===========================================================================

FadeOut_DecColor:				
		move.w	(a0),d5					; load color
		move.w	d5,d1					; copy to d1
		move.b	d1,d2					; load green and red
		move.b	d1,d3					; load red
		andi.w	#cBlue,d1				; get only blue
		beq	@noblue						; if blue is finished, branch
		subi.w	#$0200,d5				; decrease blue

	@noblue:
		andi.w	#cGreen,d2				; get only green (needs to be word)
		beq	@nogreen					; if green is finished, branch
		subi.b	#$20,d5					; decrease green

	@nogreen:
		andi.b	#cRed,d3				; get only red
		beq	@nored						; if red is finished, branch
		subq.b	#$2,d5					; decrease red

	@nored:
		move.w	d5,(a0)+				; save new color
		rts								; return

; ---------------------------------------------------------------------------
; Subroutine to	fade in from white (Special Stage)

;	uses d0, d1, d2, d3, d4, d5, d6, a0, a1
; ---------------------------------------------------------------------------

PaletteWhiteIn:
		move.w	#palfade_all,(v_palfade_start).w ; start position = 0; size = $40
		moveq	#0,d0
		lea	(v_pal_dry).w,a0
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		move.w	#cWhite,d1
		move.b	(v_palfade_size).w,d0

	@fill:
		move.w	d1,(a0)+
		dbf	d0,@fill 	; fill palette with white
        moveq	#$0E,d4 ; prepare maximum color check
        moveq 	#$00,d6 ; clear d6

	@mainloop:
		bsr.w   RunPLC
		move.b	#id_VBlank_Fade,(v_vblank_routine).w
		bsr.w	WaitForVBlank 			; wait for frame to end
		bchg 	#$00,d6 				; change delay counter
		beq		@mainloop				; if null, delay a frame
		bsr.s	WhiteIn_FromWhite		; update palette
		subq.b 	#$02,d4 				; decrease colour check
		bne		@mainloop				; if it has not reached zero, branch
		move.b	#id_VBlank_Fade,(v_vblank_routine).w ; wait for V-blank again (so colors transfer)
		bra 	WaitForVBlank  			; wait for frame to end

; ===========================================================================

WhiteIn_FromWhite:
		moveq	#0,d0
		lea	(v_pal_dry).w,a0
		lea	(v_pal_dry_next).w,a1
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		adda.w	d0,a1
		move.b	(v_palfade_size).w,d0

	@decolor:
		bsr.s	WhiteIn_DecColor 	; decrease color
		dbf	d0,@decolor				; repeat for size of palette
		
		cmpi.b	#id_LZ,(v_zone).w	; is level LZ or SBZ3?
		bne.s	@exit				; if not, branch
		
		moveq	#0,d0
		lea	(v_pal_water).w,a0
		lea	(v_pal_water_next).w,a1
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		adda.w	d0,a1
		move.b	(v_palfade_size).w,d0

	@decolor_water:
		bsr.s	WhiteIn_DecColor
		dbf	d0,@decolor_water

	@exit:
		rts	
		
; ===========================================================================

WhiteIn_DecColor:
        move.b (a1),d5 		; load blue
        move.w (a1)+,d1 	; load green and red
        move.b d1,d2 		; load red
        lsr.b #$04,d1 		; get only green
        andi.b #$0E,d2		; get only red
        move.w (a0),d3 		; load current colour in buffer
        cmp.b d5,d4 		; is it time for blue to fade?
        bls @noblue 		; if not, branch
        subi.w #$0200,d3 	; decrease blue

	@noblue:
        cmp.b d1,d4			 ; is it time for green to fade?
        bls @nogreen 		 ; if not, branch
        subi.b #$20,d3 		 ; decrease green

	@nogreen:
        cmp.b d2,d4 		; is it time for red to fade?
        bls @nored 			; if not, branch
        subq.b #$02,d3 		; decrease red

	@nored:
        move.w d3,(a0)+ 	; save color
        rts 				; return
        
; ---------------------------------------------------------------------------
; Subroutine to	fade to white (Special Stage)

;	uses d0, d1, d2, d3, d4, d5, a0, a1
; ---------------------------------------------------------------------------

PaletteWhiteOut:
		move.w	#palfade_all,(v_palfade_start).w ; start position = 0; size = $40
        moveq #$07,d4							 ; set repeat times
        moveq #$00,d6 							 ; clear d6

	@mainloop:
		bsr.w	RunPLC
		move.b	#id_VBlank_Fade,(v_vblank_routine).w
		bsr.w	WaitForVBlank
		bchg 	#$00,d6 ; MJ: change delay counter
        beq 	@mainloop ; MJ: if null, delay a frame
        bsr.s 	WhiteOut_ToWhite
		dbf		d4,@mainloop
		rts	
		
; ===========================================================================


WhiteOut_ToWhite:
		moveq	#0,d0
		lea	(v_pal_dry).w,a0			; current palette
		move.b	(v_pfade_start).w,d0
		adda.w	d0,a0
		move.b	(v_pfade_size).w,d0

	@addcolor:
		bsr.s	WhiteOut_AddColor		; current palette
		dbf	d0,@addcolor				; repeat for size of palette
		cmpi.b	#id_LZ,(v_zone).w		; is level LZ or SBZ3?
		bne.s	@exit				; if not, branch
		moveq	#0,d0
		lea	(v_pal_water).w,a0
		move.b	(v_palfade_start).w,d0
		adda.w	d0,a0
		move.b	(v_palfade_size).w,d0

	@addcolor_water:
		bsr.s	WhiteOut_AddColor
		dbf	d0,@addcolor_water
		
	@exit:	
		rts	
		
; ===========================================================================


WhiteOut_AddColor:
        move.w (a0),d5		; load color
        cmpi.w #cWhite,d5	; is color already white?
        beq.s  @nored		; if so, exit
        move.w d5,d1		; copy to d1
        move.b d1,d2 		; load green and red
        move.b d1,d3 		; load red
        andi.w #cBlue,d1 	; get only blue
        cmpi.w #cBlue,d1
        beq @noblue 		; if blue is finished, branch
        addi.w #$0200,d5 	; increase blue

	@noblue:
        andi.w #cGreen,d2 	; get only green (needs to be word)
        cmpi.w #cGreen,d2
        beq @nogreen 		; if green is finished, branch
        addi.b #$20,d5 		; increase green

	@nogreen:
        andi.b #cRed,d3 	; get only red
        cmpi.b #cRed,d3
        beq @nored 			; if red is finished, branch
        addq.b #$02,d5 		; increase red

	@nored:
        move.w d5,(a0)+ 	; save new color
        rts 				
        