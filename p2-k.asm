assume	ss:stos1, ds:dane
dane segment
	parsed_arguments		db 127 dup(?)
	RSA	db 16 dup(?)
	length_of_args			db 50 dup(0)
	table db 153 dup(0)
	beginoftable	db "+--[ RSA 1024]----+$"
	endoftable		db "+-----------------+$"
	LOG	db 	"Lack of arguments$"
	TMG	db 	"Too many arguments$"	
	FAG	db 	"Firt argument error$" 
	ID	db 	"Improper digit in the second argument$" 
	TMD	db	"too many digits in the second argument$"
	NOD	db	"not enough digits in the second argument$"
dane ends
program segment		
	start:	
		mov	ax, dane
		mov	ds, ax;  
		call parse_arguments
		;call check_argument
		;call get_key
		call show_arguments
		;call show_key
		;call fill_board ;bx - last pos
		;call change_signs
		;call show_board
		mov	ah,4ch ;end
		int	21h

	parse_arguments proc  ; PSP ->parsed_arguments
		push	di
		push	si
		push	es
		push	dx
		push	cx
		push	bx
		push 	ax
		mov	ah, 62h	;  Program Segment Prefix
	    int	21h		; adress in BX
	    mov	es, bx	;  PSP
		mov cl, es:[80h] ;  length of params 
	    cmp cl, 0h
			je lack_of_args  ;errors
		xor cl, cl ;lengh of arg
		mov	si, 82h ;the first sign of params
		mov bx, offset length_of_args	;index of first element
	    mov di, offset parsed_arguments	;index of first element
		call eat_whitespaces
		loop_parse:
			mov	al, es:[si]		; sign
			cmp al, ' '
				je first_white_space_in_row
			cmp al, 9h  ;tab
				je first_white_space_in_row		
			mov	byte ptr ds:[di], al  ;adding to array
			inc cl 
			jmp not_white_space ;omminting part below if not WS
			first_white_space_in_row:
				mov byte ptr ds:[bx], cl
				inc bx
				xor cl, cl
				mov al, '$'  ;end of arg
				mov	byte ptr ds:[di], al
				call eat_whitespaces  
				dec si ;i'll do it again below	
			not_white_space:
			inc di
			inc si
			cmp	al, 13d	
			jne	loop_parse ;repeat until ENTER
		dec cl
		mov byte ptr ds:[bx], cl ;last byte, 13d
		pop 	ax
		pop		bx
		pop		cx
		pop		dx
		pop		es
		pop		si
		pop		di
		ret
	parse_arguments endp		
	
	eat_whitespaces proc	
		dec si
		eat:
		inc si
		mov al, es:[si] ;next sign
		cmp al, ' '
			je eat
		cmp al, 9h  ;tab
			je eat
		ret	

	eat_whitespaces endp
	
	show_arguments proc
		push si
		push dx
		push ax
		mov si, offset parsed_arguments	 ;index of array
		print_loop:
			mov dl, ds:[si] ;actual character ready to ready to be printed
		
			cmp dl, 13d
				je end_of_loop ;stop if ENTER
			cmp dl, '$'
				je next_line
			mov ah, 2h
			int 21h;	
			jmp not_next_line ;ommiting code below
			next_line:
			mov dl, 10d  ;karetka
			mov ah, 02h   
			int 21h
			mov dl, 13d ;enter
			mov ah, 02h
			int 21h
			not_next_line:
			inc si
			jmp print_loop
		end_of_loop:

		pop ax
		pop dx
		pop si
		ret
	show_arguments endp
	
	show_key proc ;for debugging
		mov cx, 16
		mov si, offset RSA
		l2:
			mov dl, ds:[si]
			mov ah, 02h
			int 21h
			inc si
		
		loop l2
	
	show_key endp
	
	check_argument proc 
		push	si
		push	bx
		push 	ax
		mov bx, offset length_of_args
		mov al, ds:[bx] 
		cmp al, 1   ;length of the first arg
			jne first_arg_error
		inc bx
		mov al, ds:[bx]  ;lenght of then sec arg
		cmp al, 0
			je lack_of_args 
		cmp al, 32
			je OK_length
			jge too_many_dig_error
			jmp not_en_dig_error
		OK_length:
		inc bx 
		mov al, ds:[bx]
		inc bx
		cmp al,0  ;lenght of a futher argument
			jne too_many_args_error  ;if lnght of third arg != 0, then error
					
		mov si, offset parsed_arguments
		mov al, ds:[si]
		cmp al, '1'
			je no_problem_with_first_sign
		cmp al, '0'
			je no_problem_with_first_sign
		
		jmp first_arg_error 
		no_problem_with_first_sign:		
		inc si;
		inc si;

		check_loop:
			mov al, ds:[si] ;actual character ready to ready to be checked
			cmp al, 13d
				je end_of_check_loop ;stop if ENTER
			cmp al, '0'
				jb improp_dig_error
			cmp al, '9'
				jbe check_passed
			cmp al, 'a'
				jb improp_dig_error
			cmp al, 'f'
				jbe check_passed
			jmp improp_dig_error

			check_passed:
			inc si
			
			jmp check_loop
			end_of_check_loop:
		pop 	ax
		pop		bx
		pop		si
		ret
	check_argument endp
	
	get_key proc ;parsed_arguments -> RSA
		push si
		push di 
		push ax
		push dx
		push cx
		mov	si, offset parsed_arguments
		mov	di, offset RSA
		mov	cx, 16d
		inc si
		inc si
	l1:
		mov ah, ds:[si]
		inc si
		cmp	ah, '9'
		jbe	digit1  ;first digit
			sub	ah, 39d
		digit1:
			sub	ah, 48d
		
		mov al, ds:[si]
		inc si
		cmp	al, '9'
		jbe	digit2 ;second digit
			sub	al, 39d
		digit2:
			sub	al, 48d
		push cx
		mov cl, 04h
		shl	ah, cl
		pop cx
		add al, ah  ;sum
		mov byte ptr ds:[di], al
		inc di	
		loop l1
		pop cx
		pop dx
		pop ax
		pop di
		pop si
		ret
	get_key endp
	
	fill_board proc ;out in bx -  the last posiotion , 
		push si
		push di
		push ax
		push dx
		push cx
		mov dh, 0
		mov	si, offset parsed_arguments
		mov bh, ds:[si]
		cmp bh, '0' 
		je unmodified
		mov dh, 1
		unmodified:	
		mov si, offset RSA
		mov di, offset table
		mov bx, 76d ; (17*4 (row) + 8 (column)) position 
		mov cl, 16d
		byte_loop:
			mov al, ds:[si]
			inc si		
			mov ch, 4d		
			exact_pair:
				xor ah, ah
				mov dl, 100b
				div dl  ;modulo in AH
				cmp ah, 00b
					jne next1
					call UP ;move
					cmp dh, 0
					je unmodified1 
					call LEFT ;move
					unmodified1:
					call LEFT
					call updatecounter ;add
				next1:
				cmp ah, 01b
					jne next2
					call  UP
					cmp dh, 0   
					je unmodified2
					call RIGHT 
					unmodified2:
					call RIGHT
					call updatecounter
				next2:
				cmp ah, 10b
					jne next3
					call DOWN
					cmp dh, 0
					je unmodified3
					call LEFT 
					unmodified3:
					call LEFT
					call updatecounter
				next3:
				cmp ah, 11b
					jne next4
					call  DOWN
					cmp dh, 0
					je unmodified4
					call RIGHT 
					unmodified4:
					call RIGHT
					call updatecounter
				next4:
				dec ch
				cmp ch, 0d
				jne exact_pair
			dec cl
			cmp cl, 0d
			jne byte_loop	
		pop cx
		pop dx
		pop ax
		pop di
		pop si
	ret
	fill_board endp

	LEFT proc  ;  bx - position
			push ax
			push dx
			mov ax,bx
			mov dl, 17d
			div dl
			cmp ah, 0d
			je skip2
				dec bx
			skip2:
			pop dx
			pop ax
			ret
	LEFT endp

	RIGHT proc
			push ax
			push dx
			mov ax,bx
			mov dl, 17d 
			div dl ;is edge?
			cmp ah, 16d 
			je skip2
				inc bx
			skip2:
			pop dx
			pop ax
			ret
	RIGHT endp

	UP proc
			cmp bx, 17d
			jb skip3
				sub bx, 17d
			skip3:
			ret
	UP endp
	
	DOWN proc
			cmp bx, 135d
			jg skip4
				add bx, 17d
			skip4:
			ret
	DOWN endp
	
	updatecounter proc ;bx -position 
		push ax
		push si
		mov si, offset table
		add si, bx
		mov al, ds:[si]
		inc al
		mov byte ptr ds:[si], al
		pop si
		pop ax
		ret
	updatecounter endp
	
	show_board proc
		push cx
		push dx
		push si
		push ax			
		mov dx, offset beginoftable ;edge of the table
		mov ah, 9h
		int 21h
		mov dl, 10d
		mov ah, 02h
		int 21h
		mov dl, 13d
		mov ah, 02h
		int 21h
		mov si, offset table
		mov cl, 9d
		row_loop:
			mov dl, '|' ;edge
			mov ah, 02h
			int 21h
			mov ch, 17d
			column_loop:
				mov dl, ds:[si]
				inc si
				mov ah, 02h
				int 21h
				dec ch
				cmp ch, 0d
				jne column_loop
			mov dl, '|' ;edge
			mov ah, 02h
			int 21h
			mov dl, 10d
			mov ah, 02h
			int 21h
			mov dl, 13d
			mov ah, 02h
			int 21h
			dec cl
			cmp cl, 0d
			jne row_loop
		mov dx, offset endoftable ; edge of the table
		mov ah, 9h
		int 21h
		pop ax
		pop si
		pop dx
		pop cx
		ret
	show_board endp
	
	change_signs proc ;bx - pointer of the last element in array; table -> table
		push cx
		push si
		push ax	
		mov si, offset table
		mov cl, 9d
		row_loop:
			mov ch, 17d
			column_loop:		
				mov al, ds:[si]		
				cmp al, 0d
					jne s1
					mov ah, ' '
					jmp s15
				s1:
				cmp al, 1d
					jne s2
					mov ah, '.'
					jmp s15
				s2:
				cmp al, 2d
					jne s3
					mov ah, 'o'
					jmp s15
				s3:
				cmp al, 3d
					jne s4
					mov ah, '+'
					jmp s15
				s4:
				cmp al, 4d
					jne s5
					mov ah, '='
					jmp s15
				s5:
				cmp al, 5d
					jne s6
					mov ah, '*'
					jmp s15
				s6:
				cmp al, 6d
					jne s7
					mov ah, 'B'
					jmp s15
				s7:
				cmp al, 7d
					jne s8
					mov ah, 'O'
					jmp s15
				s8:
				cmp al, 8d
					jne s9
					mov ah, 'X'
					jmp s15
				s9:
				cmp al, 9d
					jne s10
					mov ah, '@'
					jmp s15
				s10:
				cmp al, 10d
					jne s11
					mov ah, '%'
					jmp s15
				s11:
				cmp al, 11d
					jne s12
					mov ah, '&'
					jmp s15
				s12:
				cmp al, 12d
					jne s13
					mov ah, '#'
					jmp s15
				s13:
				cmp al, 13d
					jne s14
					mov ah, '/'
					jmp s15
				s14:
					mov ah, '^'
					;jmp s15
				s15:
				mov byte ptr ds:[si], ah 
				inc si		
				dec ch
				cmp ch, 0d
				jne column_loop
			dec cl
			cmp cl, 0d
			jne row_loop
		mov si, offset table
		mov byte ptr ds:[si+76], 'S' ;the last and the first element
		mov byte ptr ds:[si+bx], 'E' ;
		pop ax
		pop si
		pop cx
		ret
	change_signs endp
				
;ERRORS			
	lack_of_args:
		mov dx, offset LOG
		mov ah, 9h
		int 21h
		mov	ah,4ch
		int	21h
		ret	
	first_arg_error:
		mov dx, offset FAG
		mov ah, 9h
		int 21h
		mov	ah,4ch
		int	21h
		ret
	improp_dig_error:
		mov dx, offset ID
		mov ah, 9h
		int 21h
		mov	ah,4ch
		int	21h
		ret
	not_en_dig_error:
		mov dx, offset NOD
		mov ah, 9h
		int 21h
		mov	ah,4ch
		int	21h
		ret	
	too_many_dig_error:
		mov dx, offset TMD
		mov ah, 9h
		int 21h
		mov	ah,4ch
		int	21h
		ret	
	too_many_args_error:
		mov dx, offset TMG
		mov ah, 9h
		int 21h
		mov	ah,4ch
		int	21h
		ret			
			
program ends
stos1 segment STACK
		dw	200	dup(?)
	top1 dw	?
stos1 ends
END start