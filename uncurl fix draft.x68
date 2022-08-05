Sonic_ResetOnFloor:
;		btst	#status_rolljump_bit,ost_status(a0)	; is Sonic jumping while rolling?
;		beq.s	@no_rolljump				; if not, branch
		btst    #bitDn,(v_joypad_hold).w            ; is down being pressed?
 		bne.w   @continuerolling        ; if yes, branch


	@part2:
	;@no_rolljump:
		bclr	#status_pushing_bit,ost_status(a0)
		bclr	#status_air_bit,ost_status(a0)
		bclr	#status_rolljump_bit,ost_status(a0)
		btst	#status_jump_bit,ost_status(a0)		; is Sonic jumping/rolling?
		beq.s	@no_jump				; if not, branch
		bclr	#status_jump_bit,ost_status(a0)
		move.b	#sonic_height,ost_height(a0)
		move.b	#sonic_width,ost_width(a0)
		move.b	#id_Walk,ost_anim(a0)			; use running/walking animation
		subq.w	#sonic_height-sonic_height_roll,ost_y_pos(a0)

	@no_jump:
		move.b	#0,ost_sonic_jump(a0)
		move.w	#0,(v_enemy_combo).w			; reset counter for points for breaking multiple enemies
		rts




;=====================================================================================================================
; (Hitaxas) Fix for awful uncurling frame when a character lands on the ground from a jump
; or if they walked off an object or edge of a floor while holding down.
; this "bug" is present in S1-S3K.
;=====================================================================================================================
	@continuerolling:
  		move.b  #id_Roll,ost_anim(a0)      ; play rolling animation
  	 	btst    #status_jump_bit,ost_status(a0)               ; was status set to rolling?
  		beq.s	Sonic_ResetOnFloor_Part3        ; if it was, branch
 		bset    #status_jump_bit,ost_status(a0)                   ; otherwise, set roll status
 	  	move.b	#sonic_height_roll,ost_height(a0)                ; adjust character's y_radius
    	move.b  #sonic_width_roll,ost_width(a0)                 ; same for x_radius
    	addq.w  #5,$C(a0)                       ; move character 5 pixels down, so they aren't floating
    	move.w  #SndID_Roll,d0
	   	jsr     (PlaySound).l                   ; play rolling sound - required because this is not part of the normal roll code                        
 