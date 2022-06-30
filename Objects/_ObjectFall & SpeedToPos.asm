; ---------------------------------------------------------------------------
; Subroutine to	make an	object fall downwards, increasingly fast
; (Also updates its position)

;	uses d0, d2, d3
; ---------------------------------------------------------------------------

; Both routines replaced with code backported from S3&K
; They only use d0 now!



ObjectFall:
;		move.l	ost_x_pos(a0),d2
;		move.l	ost_y_pos(a0),d3
;		move.w	ost_x_vel(a0),d0
;		ext.l	d0
;		asl.l	#8,d0					; d0 = ost_x_vel * $100
;		add.l	d0,d2					; add to x position
;		move.w	ost_y_vel(a0),d0
;		addi.w	#$38,ost_y_vel(a0)			; increase falling speed
;		ext.l	d0
;		asl.l	#8,d0					; d0 = last ost_y_vel * $100
;		add.l	d0,d3					; add to y position
;		move.l	d2,ost_x_pos(a0)
;		move.l	d3,ost_y_pos(a0)
;		rts


		move.w	ost_x_vel(a0),d0
		ext.l	d0
		lsl.l	#8,d0
		add.l	d0,ost_x_pos(a0)
		move.w	ost_y_vel(a0),d0
;<<<<<<< HEAD
;		addi.w	#$38,ost_y_vel(a0)	; increase vertical speed
;=======
		addi.w	#$38,ost_y_vel(a0)			; apply gravity
;>>>>>>> 5fb63516a8f1f7046d0ff464fd3d79dec8a8de76
		ext.l	d0
		lsl.l	#8,d0
		add.l	d0,ost_y_pos(a0)
		rts	


; ---------------------------------------------------------------------------
; Subroutine translating object	speed to update	object position

;	uses d0, d2, d3
; ---------------------------------------------------------------------------

SpeedToPos:
;		move.l	ost_x_pos(a0),d2
;		move.l	ost_y_pos(a0),d3
;		move.w	ost_x_vel(a0),d0			; load horizontal speed
;		ext.l	d0
;		asl.l	#8,d0					; multiply speed by $100
;		add.l	d0,d2					; add to x position
;		move.w	ost_y_vel(a0),d0			; load vertical speed
;		ext.l	d0
;		asl.l	#8,d0					; multiply by $100
;		add.l	d0,d3					; add to y position
;		move.l	d2,ost_x_pos(a0)			; update x position
;		move.l	d3,ost_y_pos(a0)			; update y position
;		rts

		move.w	ost_x_vel(a0),d0	; load horizontal speed
		ext.l	d0
		lsl.l	#8,d0		; multiply speed by $100
		add.l	d0,ost_x_pos(a0)	; add to x-axis	position
		move.w	ost_y_vel(a0),d0	; load vertical	speed
		ext.l	d0
		lsl.l	#8,d0		; multiply by $100
		add.l	d0,ost_y_pos(a0)	; add to y-axis	position
		rts	