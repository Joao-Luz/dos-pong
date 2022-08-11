; 
segment code
..start:
    mov 	ax,data
    mov 	ds,ax
    mov 	ax,stack
    mov 	ss,ax
    mov 	sp,stacktop

; set current mode to 'video' and check last mode
    mov  	ah,0fh
    int  	10h
    mov  	[last_mode],al

; change video mode to 640x480 16 color graphic mode
    mov    	al,12h
    mov    	ah,0
    int    	10h

	cli								; clear interrupt flag

	mov     ax, 0
	mov     es, ax
	mov     ax, [es:int9*4]			; load previous int9 interrupt address
	mov     [offset_dos], ax        ; save the last address to memory
	mov     ax, [es:int9*4+2]		; load previous int9 segment address
	mov     [cs_dos], ax		 	; save to memory

	mov     [es:int9*4+2], cs		; set new segment address to int9's table
	mov     word[es:int9*4],keyint	; set new interrupt address to table

	sti

main:
	mov		byte[color],white

	mov		ax,0
	push	ax
	mov		ax,430
	push	ax
	mov		ax,639
	push	ax
	mov		ax,430
	push	ax
	call 	line

	call	write_header
	call	write_name
	call	write_score
	call	write_computer
	call	write_current_speed
	call	write_speed
	call	draw_paddle

main_loop:
	cmp		byte[key],1
	je		exit

	call	update_ball

	mov		ax,0
	mov		al,byte[current_speed]	; select current speed
	mov		bh,0
	mov		bl,2
	mul		bl
	mov		bl,al
	mov		cx,0
	mov		dx,word[game_speed+bx]	; apply speed
	mov		al,0
	mov		ah,86h
	int		15h						; wait interrupt (cx:dx = time in microseconds)
	jmp		main_loop

exit:
	cli
	mov     ax,0
	mov     es,ax
	mov     ax,word[cs_dos]
	mov     [es:int9*4+2],ax		; restore previous segment
	mov     ax,[offset_dos]
	mov     [es:int9*4],ax 			; restore previous interrupt address
	mov     ah, 4ch

	mov  	ah,0   					; set video mode
    mov  	al,[last_mode]  		; to last mode
    int  	10h
    mov     ax,4c00h

	int     21h						; exit

update_ball:
	mov		ax,word[ball_x]
	add		ax,word[ball_vx]
	mov		word[ball_next_x],ax

	mov		ax,word[ball_y]
	add		ax,word[ball_vy]
	mov		word[ball_next_y],ax

	mov		byte[color],white

test_col_left:
	mov		bx,ball_r
	cmp		word[ball_next_x],bx
	jg		test_col_right
	mov		word[ball_next_x],ball_r
	call	invert_vx
	jmp		test_col_up
test_col_right:
	mov		bx,639
	sub		bx,ball_r
	cmp		word[ball_next_x],bx
	jb		test_col_up
	mov		word[ball_next_x],320
	mov		word[ball_next_y],215
	inc		byte[computer_score]
	call	write_score
	jmp		update_pos

test_col_up:
	mov		bx,429
	sub		bx,ball_r
	cmp		word[ball_next_y],bx
	jb		test_col_down
	mov		word[ball_next_y],bx
	call	invert_vy
	jmp		update_pos

test_col_down:
	mov		bx,ball_r
	cmp		word[ball_next_y],bx
	jg		test_col_paddle
	mov		word[ball_next_y],bx
	call	invert_vy
	jmp		update_pos

test_col_paddle:
	mov		bx,word[paddle_pos]
	sub		bx,ball_r
	cmp		word[ball_next_y],bx
	jl		update_pos
	add		bx,50
	cmp		word[ball_next_y],bx
	jg		update_pos
	mov		bx,600
	sub		bx,ball_r
	cmp		word[ball_x],bx
	jg		update_pos
	cmp		word[ball_next_x],bx
	jb		update_pos
	mov		word[ball_next_x],bx
	call	invert_vx
	inc		byte[player_score]
	call	write_score

update_pos:
	call	draw_ball

	mov		ax,word[ball_next_y]
	mov		word[ball_y],ax

	mov		ax,word[ball_next_x]
	mov		word[ball_x],ax

	ret

invert_vx:
	mov		ax,word[ball_vx]
	mov		bh,0
	mov		bl,-1
	imul	bl
	mov		word[ball_vx],ax
	ret

invert_vy:
	mov		ax,word[ball_vy]
	mov		bh,0
	mov		bl,-1
	imul	bl
	mov		word[ball_vy],ax
	ret

write_header:
	mov 	cx,58	; msg length
	mov 	bx,0	; msg offset
	mov		dh,1	; cursor line
	mov		dl,2	; cursor column
loop_write_header:
    call	cursor
    mov     al,[bx+msg_header]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_header
	ret

write_name:
	mov 	cx,14	; msg length
	mov 	bx,0	; msg offset
	mov		dh,2	; cursor line
	mov		dl,2	; cursor column
loop_write_name:
    call	cursor
    mov     al,[bx+msg_name]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_name
	ret

write_score:
	mov 	cx,5	; msg length
	mov 	bx,0	; msg offset
	mov		dh,2	; cursor line
	mov		dl,17	; cursor column

	mov		ax,0
	mov		al,byte[player_score]
	mov		bl,10
	div		bl
	add		al,'0'
	add		ah,'0'
	mov		byte[msg_score],al
	mov		byte[msg_score+1],ah

	mov		ax,0
	mov		al,byte[computer_score]
	mov		bl,10
	div		bl
	add		al,'0'
	add		ah,'0'
	mov		byte[msg_score+3],al
	mov		byte[msg_score+4],ah

	mov		bx,0
loop_write_score:
    call	cursor
    mov     al,[bx+msg_score]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_score
	ret

write_computer:
	mov 	cx,10	; msg length
	mov 	bx,0	; msg offset
	mov		dh,2	; cursor line
	mov		dl,23	; cursor column
loop_write_computer:
    call	cursor
    mov     al,[bx+msg_computer]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_computer
	ret

write_current_speed:
	mov 	cx,18	; msg length
	mov 	bx,0	; msg offset
	mov		dh,2	; cursor line
	mov		dl,57	; cursor column
loop_write_current_speed:
    call	cursor
    mov     al,[bx+msg_current_speed]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_current_speed
	ret

write_speed:
	mov 	cx,1	; msg length
	mov 	bx,0	; msg offset
	mov		dh,2	; cursor line
	mov		dl,75	; cursor column

	mov		al,byte[current_speed]
	add		al,'1'
	mov		byte[msg_speed],al
loop_write_speed:
    call	cursor
    mov     al,[bx+msg_speed]
    call	character	; display single character
    inc     bx          ; next character
    inc		dl          ; next column
    loop    loop_write_speed
	ret

draw_ball:
	mov		byte[color],black
	push	word[ball_x]
	push	word[ball_y]
	mov		ax,ball_r
	push	ax
	call	full_circle

	mov		byte[color],red
	push	word[ball_next_x]
	push	word[ball_next_y]
	mov		ax,ball_r
	push	ax
	call	full_circle

	ret

draw_paddle:
	mov		byte[color],black
	mov		ax,600
	push	ax
	mov		ax,0
	push	ax
	mov		ax,4
	push	ax
	mov		ax,429
	push	ax
	call	rectangle

	mov		byte[color],white
	mov		ax,600
	push	ax
	push	word[paddle_pos]
	mov		ax,4
	push	ax
	mov		ax,50
	push	ax
	call	rectangle

	ret

keyint:								; keyboard interrupt
	push    ax
	push    bx
	push	dx
	push    ds

	mov     ax,data					; load data segment
	mov     ds,ax

	mov		al,byte[color]
	mov		byte[prev_color],al
	mov		byte[color],white

	in      al,kb_data

	mov		bh,0
	mov		bl,al
	shr     bl,7
	and     al,7fh

	cmp		bl,1
	je		exit_int

	mov		byte[key],al

	cmp		al,4eh					; +
	je		increase_speed

	cmp		al,4ah					; -
	je		decrease_speed

	cmp		al,16h					; u
	je		move_up

	cmp		al,20h					; d
	je		move_down

	jmp 	exit_int

increase_speed:
	cmp		byte[current_speed],4
	je		exit_int
	inc		byte[current_speed]
	call	write_speed
	jmp		exit_int

decrease_speed:
	cmp		byte[current_speed],0
	je		exit_int
	dec		byte[current_speed]
	call	write_speed
	jmp		exit_int

move_up:
	cmp		word[paddle_pos],380
	je		exit_int
	add		word[paddle_pos],paddle_v
	call	draw_paddle
	jmp		exit_int

move_down:
	cmp		word[paddle_pos],0
	je		exit_int
	add		word[paddle_pos],-paddle_v
	call	draw_paddle
	jmp		exit_int

	exit_int:
	in      al, kb_ctl
	or      al, 80h
	out     kb_ctl, al
	and     al, 7fh
	out     kb_ctl, al
	mov     al, eoi
	out     pictrl, al

	mov		al,byte[prev_color]
	mov		byte[color],al

	pop     ds
	pop		dx
	pop     bx
	pop     ax

	iret

rectangle:
	push	bp
	mov		bp,sp
	pushf               ; push flags to stack
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+10]	; x
	mov		bx,[bp+8]	; y
	mov		cx,[bp+6]	; w
	mov		dx,[bp+4]	; h

rectangle_loop:
	mov		bx,[bp+8]
	push	ax
	push	bx
	push	ax
	add		bx,dx
	push	bx
	call	line

	inc		ax

	loop 	rectangle_loop

    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    pop		bp
    ret		8


cursor:
    pushf
    push 	ax
    push 	bx
    push	cx
    push	dx
    push	si
    push	di
    push	bp

    mov     ah,2
    mov     bh,0

    int     10h
    pop		bp
    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    ret
;------------------------------------------------------------------------------
;
;   function : character write in cursor position
;
;   color = color of character (defined in `color` variable)
;   al = character to be written
character:
    pushf
    push 	ax
    push 	bx
    push	cx
    push	dx
    push	si
    push	di
    push	bp
	
    mov    	ah,9
    mov    	bh,0
    mov    	cx,1
    mov    	bl,[color]

    int    	10h
    pop		bp
    pop		di
    pop		si
    pop		dx
    pop		cx
    pop		bx
    pop		ax
    popf
    ret
;------------------------------------------------------------------------------
;   function: plot pixel at (x, y)
;
;   push x (0 <= x <= 639)
;   push y (0 <= y <= 479 )
;   call plot_xy
;
; 	color = line color (defined in `color` variable)
plot_xy:
	push	bp
	mov		bp,sp
	pushf
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov    	ah,0ch
	mov    	al,[color]
	mov    	bh,0
	mov    	dx,479
	sub		dx,[bp+4]
	mov    	cx,[bp+6]

	int    	10h
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		4
;_____________________________________________________________________________
;   function : plot circle
;
;	push xc (xc-r>=0 and xc+r<=639)
;	push yc	(yc-r>=0 and yc+r<=479)
;	push r
;	call circle
;
; 	color = circle color (defined in `color` variable)
circle:
	push 	bp
	mov	 	bp,sp
	pushf				; push flags to stack
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]	; get xc
	mov		bx,[bp+6]	; get yc
	mov		cx,[bp+4]   ; get r

	mov 	dx,bx
	add		dx,cx       ; superior extreme
	push    ax
	push	dx
	call plot_xy

	mov		dx,bx
	sub		dx,cx       ; inferior extreme
	push    ax
	push	dx
	call plot_xy

	mov 	dx,ax
	add		dx,cx       ; right extreme
	push    dx
	push	bx
	call plot_xy

	mov		dx,ax
	sub		dx,cx       ; left extreme
	push    dx
	push	bx
	call plot_xy

	mov		di,cx
	sub		di,1	 	; di = r-1
	mov		dx,0  		; dx is the x variable. cx is the y variable

	; up here, the logic was inverted, 1-r => r-1
	; and the comparisons went from jl => jg, this guarantees
	; positive values for d

stay:				;loop
	mov		si,di
	cmp		si,0
	jg		inf     ; if d is less than 0, select superior pixel (don't jump)
	mov		si,dx	; jl is important because we are doing signed operations
	sal		si,1	; multiply by 2 (shift arithmetic left)
	add		si,3
	add		di,si   ; d = d+2*dx+3
	inc		dx
	jmp		plot
inf:
	mov		si,dx
	sub		si,cx  	; do x - y (dx-cx), store in di
	sal		si,1
	add		si,5
	add		di,si	; d=d+2*(dx-cx)+5
	inc		dx		; increment x (dx)
	dec		cx		; decrement y (cx)

plot:
	mov		si,dx
	add		si,ax
	push    si			; push x+xc axis to stack
	mov		si,cx
	add		si,bx
	push    si			; push y+yc axis to stack
	call plot_xy		; second octant
	mov		si,ax
	add		si,dx
	push    si			; push x+xc axis to stack
	mov		si,bx
	sub		si,cx
	push    si			; push yc-y axis to stack
	call plot_xy		; seventh octant
	mov		si,ax
	add		si,cx
	push    si			; push xc+y axis to stack
	mov		si,bx
	add		si,dx
	push    si			; push yc+x axis to stack
	call plot_xy		; second octant
	mov		si,ax
	add		si,cx
	push    si			; push xc+y axis to stack
	mov		si,bx
	sub		si,dx
	push    si			; push yc-x axis to stack
	call plot_xy		; eighth octant
	mov		si,ax
	sub		si,dx
	push    si			; push xc-x axis to stack
	mov		si,bx
	add		si,cx
	push    si			; push yc+y axis to stack
	call plot_xy		; third octant
	mov		si,ax
	sub		si,dx
	push    si			; push xc-x axis to stack
	mov		si,bx
	sub		si,cx
	push    si			; push yc-y axis to stack
	call plot_xy		; sixth octant
	mov		si,ax
	sub		si,cx
	push    si			; push xc-y axis to stack
	mov		si,bx
	sub		si,dx
	push    si			; push yc-x axis to stack
	call plot_xy		; fifth octant
	mov		si,ax
	sub		si,cx
	push    si			; push xc-y axis to stack
	mov		si,bx
	add		si,dx
	push    si			; push yc-x axis to stack
	call plot_xy		; fourth octant

	cmp		cx,dx
	jb		end_circle  ; if cx (y) is less than dx (x), finish
	jmp		stay		; if cx (y) is greater than dx (x), stay on loop


end_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6

;------------------------------------------------------------------------------
;   function: draw full circle
;
;	push xc (xc-r>=0 and xc+r<=639)
;	push yc	(yc-r>=0 and yc+r<=479)
;	push r
;	call circle
;
; 	color = circle color (defined in `color` variable)
full_circle:
	push 	bp
	mov	 	bp,sp
	pushf				; push flags to stack
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di

	mov		ax,[bp+8]   ; get xc
	mov		bx,[bp+6]   ; get yc
	mov		cx,[bp+4]   ; get r

	mov		si,bx
	sub		si,cx
	push    ax			; push xc to stack
	push	si			; push yc-r to stack
	mov		si,bx
	add		si,cx
	push	ax			; push xc to stack
	push	si			; push yc+r to stack
	call line


	mov		di,cx
	sub		di,1	 	; di=r-1
	mov		dx,0  		; dx is the x variable. cx is the y variable

	; up here, the logic was inverted, 1-r => r-1
	; and the comparisons went from jl => jg, this guarantees
	; positive values for d

stay_full:				;loop
	mov		si,di
	cmp		si,0
	jg		inf_full    ; if d is less than 0, select superior pixel (don't jump)
	mov		si,dx		; jl is important because we are doing signed operations
	sal		si,1		; multiply by 2 (shift arithmetic left)
	add		si,3
	add		di,si     	; d=d+2*dx+3
	inc		dx
	jmp		plot_full
inf_full:
	mov		si,dx
	sub		si,cx  	; do x - y (dx-cx), store in di
	sal		si,1
	add		si,5
	add		di,si	; d=d+2*(dx-cx)+5
	inc		dx		; increment x (dx)
	dec		cx		; decrement y (cx)

plot_full:
	mov		si,ax
	add		si,cx
	push	si
	mov		si,bx
	sub		si,dx
	push    si
	mov		si,ax
	add		si,cx
	push	si
	mov		si,bx
	add		si,dx
	push    si
	call 	line

	mov		si,ax
	add		si,dx
	push	si
	mov		si,bx
	sub		si,cx
	push    si
	mov		si,ax
	add		si,dx
	push	si
	mov		si,bx
	add		si,cx
	push    si
	call	line

	mov		si,ax
	sub		si,dx
	push	si
	mov		si,bx
	sub		si,cx
	push    si
	mov		si,ax
	sub		si,dx
	push	si
	mov		si,bx
	add		si,cx
	push    si
	call	line

	mov		si,ax
	sub		si,cx
	push	si
	mov		si,bx
	sub		si,dx
	push    si
	mov		si,ax
	sub		si,cx
	push	si
	mov		si,bx
	add		si,dx
	push    si
	call	line

	cmp		cx,dx
	jb		end_full_circle ; if cx (y) is less than dx (x), finish
	jmp		stay_full		; if cx (y) is greater than dx (x), stay on loop

end_full_circle:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		6

;------------------------------------------------------------------------------
;
;   function: plot line
;
; 	push x1 (0<=x1<=639)
;	push y1 (0<=y1<=479)
;	push x2 (0<=x2<=639)
;	push y2 (0<=y2<=479)
;	call line
line:
	push	bp
	mov		bp,sp
	pushf               ; push flags to stack
	push 	ax
	push 	bx
	push	cx
	push	dx
	push	si
	push	di
	mov		ax,[bp+10]	; get coordinate values
	mov		bx,[bp+8]
	mov		cx,[bp+6]
	mov		dx,[bp+4]
	cmp		ax,cx
	je		line2
	jb		line1
	xchg	ax,cx
	xchg	bx,dx
	jmp		line1
line2:					; deltax=0
		cmp		bx,dx	; subtract dx from bx
		jb		line3
		xchg	bx,dx   ; swap bx and dx values
line3:					; dx > bx
		push	ax
		push	bx
		call 	plot_xy
		cmp		bx,dx
		jne		line31
		jmp		end_line
line31:
	inc		bx
	jmp		line3

line1:		;deltax != 0
	; compare absolute values of deltax and deltay, knowing cx>ax
	push	cx
	sub		cx,ax
	mov		[deltax],cx
	pop		cx
	push	dx
	sub		dx,bx
	ja		line32
	neg		dx
line32:
	mov		[deltay],dx
	pop		dx

	push	ax
	mov		ax,[deltax]
	cmp		ax,[deltay]
	pop		ax
	jb		line5

	; cx > ax and deltax>deltay
	push	cx
	sub		cx,ax
	mov		[deltax],cx
	pop		cx
	push	dx
	sub		dx,bx
	mov		[deltay],dx
	pop		dx

	mov		si,ax
line4:
	push	ax
	push	dx
	push	si
	sub		si,ax		;(x-x1)
	mov		ax,[deltay]
	imul	si
	mov		si,[deltax]	; rounding
	shr		si,1
	cmp		dx,0		; if numerator (dx)>0, add. if < 0, subtract
	jl		ar1
	add		ax,si
	adc		dx,0
	jmp		arc1
ar1:
	sub		ax,si
	sbb		dx,0
arc1:
	idiv	word [deltax]
	add		ax,bx
	pop		si
	push	si
	push	ax
	call	plot_xy
	pop		dx
	pop		ax
	cmp		si,cx
	je		end_line
	inc		si
	jmp		line4

line5:		
	cmp		bx,dx
	jb 		line7
	xchg	ax,cx
	xchg	bx,dx
line7:
	push	cx
	sub		cx,ax
	mov		[deltax],cx
	pop		cx
	push	dx
	sub		dx,bx
	mov		[deltay],dx
	pop		dx
	mov		si,bx
line6:
	push	dx
	push	si
	push	ax
	sub		si,bx		;(y-y1)
	mov		ax,[deltax]
	imul	si
	mov		si,[deltay]	;arredondar
	shr		si,1
	cmp		dx,0		; if (dx)>0, add. if < 0, subtract
	jl		ar2
	add		ax,si
	adc		dx,0
	jmp		arc2
ar2:
	sub		ax,si
	sbb		dx,0
arc2:
	idiv	word [deltay]
	mov		di,ax
	pop		ax
	add		di,ax
	pop		si
	push	di
	push	si
	call	plot_xy
	pop		dx
	cmp		si,dx
	je		end_line
	inc		si
	jmp		line6
end_line:
	pop		di
	pop		si
	pop		dx
	pop		cx
	pop		bx
	pop		ax
	popf
	pop		bp
	ret		8

;------------------------------------------------------------------------------
segment data

	color				db		bright_white
	prev_color			db		bright_white

;	I R G B color
;	0 0 0 0 black
;	0 0 0 1 blue
;	0 0 1 0 green
;	0 0 1 1 cyan
;	0 1 0 0 red
;	0 1 0 1 magenta
;	0 1 1 0 brown
;	0 1 1 1 white
;	1 0 0 0 grey
;	1 0 0 1 light blue
;	1 0 1 0 light green
;	1 0 1 1 light cyan
;	1 1 0 0 pink
;	1 1 0 1 light magenta
;	1 1 1 0 yellow
;	1 1 1 1 strong white

	black		    equ		0
	blue		    equ		1
	green		    equ		2
	cyan		    equ		3
	red             equ		4
	magenta		    equ		5
	brown		    equ		6
	white		    equ		7
	grey		    equ		8
	light_blue	    equ		9
	light_green	    equ		10
	light_cyan	    equ		11
	pink		    equ		12
	light_magenta	equ		13
	yellow		    equ		14
	bright_white	equ		15
	last_mode		db		0
	column  		dw  	0
	deltax			dw		0
	deltay			dw		0
	mousex			dw		0
	mousey			dw		0
	mousepress		dw		0

	kb_data 		equ 	60h  	; keyboard input address
	kb_ctl  		equ 	61h		; interrupt reset address
	pictrl  		equ 	20h		
	eoi     		equ 	20h
	int9    		equ 	9h
	cs_dos  		dw  	1
	offset_dos  	dw 		1
	key				db		0

	msg_header		db		"Exercicio de Programacao de Sistemas Embarcados 1 - 2022/2" ; 58
	msg_name		db		"Joao Lucas Luz" ; 14
	msg_score		db		"00x00" ; 5
	msg_computer	db		"Computador" ; 10
	msg_current_speed		db		"Velocidade atual: " ; 18
	msg_speed		db		"0" ; 1

	paddle_v		equ		5
	paddle_pos		dw		0
	ball_x			dw		320
	ball_y			dw		215
	ball_next_x		dw		0
	ball_next_y		dw		0
	ball_vx			dw		3
	ball_vy			dw		3
	ball_r			equ		5
	computer_score	db		0
	player_score	db		0

	game_speed 		dw		40000, 30000, 20000, 10000, 5000
	current_speed	db		1

segment stack stack
		    		resb 	512
stacktop:
