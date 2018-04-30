;;; scheme.s
;;; Support for the Scheme compiler
;;; 
;;; Programmer: Mayer Goldberg, 2018

%define T_UNDEFINED 0
%define T_VOID 1
%define T_NIL 2
%define T_INTEGER 3
%define T_FRACTION 4
%define T_BOOL 5
%define T_CHAR 6
%define T_STRING 7
%define T_SYMBOL 8
%define T_CLOSURE 9
%define T_PAIR 10
%define T_VECTOR 11

%define CHAR_NUL 0
%define CHAR_TAB 9
%define CHAR_NEWLINE 10
%define CHAR_PAGE 12
%define CHAR_RETURN 13
%define CHAR_SPACE 32

%define TYPE_BITS 4
%define WORD_SIZE 64

%define MAKE_LITERAL(type, lit) ((lit << TYPE_BITS) | type)

%define MAKE_LITERAL_SYMBOL(sym) (((sym-start_of_data) << TYPE_BITS) | T_SYMBOL)

%macro TYPE 1
	and %1, ((1 << TYPE_BITS) - 1)
%endmacro

%macro DATA 1
	sar %1, TYPE_BITS
%endmacro
%define MAKE_LITERAL_SYMBOL(sym) (((sym-start_of_data)<< TYPE_BITS) | T_SYMBOL)

%macro DATA_UPPER 1
	shr %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro DATA_LOWER 1
	shl %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%macro Fraction_UPPER 1
	sar %1, (((WORD_SIZE - TYPE_BITS) >> 1) + TYPE_BITS)
%endmacro

%macro Fraction_LOWER 1
	sal %1, ((WORD_SIZE - TYPE_BITS) >> 1)
	DATA_UPPER %1
%endmacro

%define MAKE_LITERAL_PAIR(car, cdr) (((((car - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (cdr - start_of_data)) << TYPE_BITS) | T_PAIR)

%macro CAR 1
	DATA_UPPER %1
	add %1, start_of_data
	;mov %1, qword [%1]
%endmacro

%macro CDR 1
	DATA_LOWER %1
	add %1, start_of_data
	;mov %1, qword [%1]
%endmacro

;;; MAKE_MALLOC_LITERAL_PAIR target-addrees, car-addrees, cdr-addrees
%macro MAKE_MALLOC_LITERAL_PAIR 3
    push rax
    push rbx
    mov rax, %1
    mov qword [rax], %2
    sub qword [rax], start_of_data
    shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)
    mov rbx, %3
    sub rbx, start_of_data
    or qword [rax], rbx
    shl qword [rax], TYPE_BITS
    or qword [rax], T_PAIR
    pop rbx
    pop rax
%endmacro

;%define MAKE_LITERAL_FRACTION(numer, denom) (((((numer - start_of_data) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (denom - start_of_data)) << TYPE_BITS) | T_FRACTION)


;;; MAKE_LITERAL_CLOSURE target, env, code
%macro MAKE_LITERAL_CLOSURE 3
	push rax
	push rbx
	mov rax, %1
	mov qword [rax], %2
	sub qword [rax] , start_of_data
	shl qword [rax], ((WORD_SIZE - TYPE_BITS) >> 1)
	;lea rbx, [rax + 8 - start_of_data]
        lea rbx, [rax + 8]
        sub rbx, start_of_data
	or qword [rax], rbx
	shl qword [rax], TYPE_BITS
	or qword [rax], T_CLOSURE
	mov qword [rax + 8], %3
	pop rbx
	pop rax
%endmacro

%macro CLOSURE_ENV 1
	DATA_UPPER %1
	add %1, start_of_data
%endmacro

%macro CLOSURE_CODE 1
	DATA_LOWER %1
	add %1, start_of_data
	mov %1, qword [%1]
	
%endmacro

%macro MAKE_LITERAL_STRING 1+
	dq (((((%%LstrEnd - %%Lstr) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Lstr - start_of_data)) << TYPE_BITS) | T_STRING)
	%%Lstr:
	db %1
	%%LstrEnd:
%endmacro

%macro STRING_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro STRING_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

%define param(offset) qword [rbp + offset]
struc scmframe
.old_rbp: resq 1
.ret_addr: resq 1

.env: resq 1
.arg_count: resq 1
.A0: resq 1
.A1: resq 1
.A2: resq 1
.A3: resq 1
.A4: resq 1
.A5: resq 1
endstruc

%define old_rbp param(scmframe.old_rbp)
%define ret_addr param(scmframe.ret_addr)
%define env param(scmframe.env)
%define arg_count param(scmframe.arg_count)
%define A0 param(scmframe.A0)
%define A1 param(scmframe.A1)
%define A2 param(scmframe.A2)
%define A3 param(scmframe.A3)
%define A4 param(scmframe.A4)
%define A5 param(scmframe.A5)
%define An(n) qword [rbp + 8 * (n + 4)]

;;; STRING_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro STRING_REF 3
	push rax
	mov rax, %2
	STRING_ELEMENTS rax
	add rax, %3
	mov %1, byte [rax]
	pop rax
%endmacro

%macro MAKE_LITERAL_VECTOR 1+
	dq ((((((%%VecEnd - %%Vec) >> 3) << ((WORD_SIZE - TYPE_BITS) >> 1)) | (%%Vec - start_of_data)) << TYPE_BITS) | T_VECTOR)
	%%Vec:
	dq %1
	%%VecEnd:
%endmacro

%macro VECTOR_LENGTH 1
	DATA_UPPER %1
%endmacro

%macro VECTOR_ELEMENTS 1
	DATA_LOWER %1
	add %1, start_of_data
%endmacro

;;; VECTOR_REF dest, src, index
;;; dest cannot be RAX! (fix this!)
%macro VECTOR_REF 3
	mov %1, %2
	VECTOR_ELEMENTS %1
	lea %1, [%1 + %3*8]
	mov %1, qword [%1]
	mov %1, qword [%1]
%endmacro

%define SOB_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)
%define SOB_VOID MAKE_LITERAL(T_VOID, 0)
%define SOB_FALSE MAKE_LITERAL(T_BOOL, 0)
%define SOB_TRUE MAKE_LITERAL(T_BOOL, 1)
%define SOB_NIL MAKE_LITERAL(T_NIL, 0)

extern exit, printf, scanf
global main, write_sob, write_sob_if_not_void
section .text

gcd:
    push rbp
    mov rbp, rsp
    push rbx
    push rcx
    push rdx
    mov rbx, qword [rbp + 16]
    mov rcx, qword [rbp + 24]
    cmp rbx, 0
    jge .checkSecond
    mov rax, -1
    mul rbx
    mov rbx, rax
.checkSecond:
    cmp rcx, 0
    jge .loop
    mov rax, -1
    mul rcx
    mov rcx, rax

.loop:
    cmp rbx, 0
    je .done
    mov rdx, 0
    mov rax, rcx
    div rbx
    mov rcx, rbx
    mov rbx, rdx
    jmp .loop

.done:
    mov rax, rcx
    pop rdx
    pop rcx
    pop rbx
    leave
    ret
    

MAKE_SOB_STRING:
    push rbp
    mov rbp, rsp
    
    push rbx
    push rcx
    push rdx
    mov rbx, qword [rbp + 16] ;n
    mov rcx, qword [rbp + 24] ;char
    
    mov rdi, rbx
    push rbx
    push rcx
    call malloc
    pop rcx 
    pop rbx
    mov r10, rax
    
mov rsi, 0 ;counter
.mssloop:
    cmp rsi, rbx
    je .mssdone
    mov byte[rax], cl
    add rax, 1
    inc rsi
    jmp .mssloop
    

    
    
.mssdone:
    mov rdi, 8
    push rbx
    push rcx
    push rdx
    push r10
    call malloc
    pop r10
    pop rdx
    pop rcx 
    pop rbx
    mov qword[rax], rbx
    shl qword[rax], 34
    sub r10, start_of_data
    shl r10, 4
    or r10 , T_STRING
    or qword[rax], r10
    
    
    
    pop rdx
    pop rcx
    pop rbx
    leave
    ret
    
    
    
MAKE_SOB_VECTOR:
    push rbp
    mov rbp, rsp
    
    push rbx
    push rcx
    push rdx
    mov rbx, qword [rbp + 16] ;n
    mov rcx, qword [rbp + 24] ;obj
    
    mov rax, rbx
    mov r15, 8
    mul r15
    
    mov rdi, rax
    push rbx
    push rcx
    call malloc
    pop rcx 
    pop rbx
    mov r10, rax
    
mov rsi, 0 ;counter
.msvloop:
    cmp rsi, rbx
    je .msvdone
    mov qword[rax],rcx
    add rax, 8
    inc rsi
    jmp .msvloop
    

    
    
.msvdone:
    mov rdi, 8
    push rbx
    push rcx
    push rdx
    push r10
    call malloc
    pop r10
    pop rdx
    pop rcx 
    pop rbx
    mov qword[rax], rbx
    shl qword[rax], 34
    sub r10, start_of_data
    shl r10, 4
    or r10 , T_VECTOR
    or qword[rax], r10
    
    
    
    pop rdx
    pop rcx
    pop rbx
    leave
    ret
    
    
MAKE_VECTOR:
    push rbp
    mov rbp, rsp
    
    push rbx
    push rcx
    push rdx
    mov rbx, qword [rbp + 16] ;n
    mov rcx, 24 ;first obj
    
    mov rax, rbx
    mov r15, 8
    mul r15
    
    mov rdi, rax
    push rbx
    push rcx
    call malloc
    pop rcx 
    pop rbx
    mov r10, rax
    
mov rsi, 0 ;counter
.mvloop:
    cmp rsi, rbx
    je .mvdone
    mov r11, qword[rbp+rcx]
    mov qword[rax],r11
    add rcx, 8
    add rax, 8
    inc rsi
    jmp .mvloop
    

    
    
.mvdone:
    mov rdi, 8
    push rbx
    push rcx
    push rdx
    push r10
    call malloc
    pop r10
    pop rdx
    pop rcx 
    pop rbx
    mov qword[rax], rbx
    shl qword[rax], 34
    sub r10, start_of_data
    shl r10, 4
    or r10 , T_VECTOR
    or qword[rax], r10
    
    
    
    pop rdx
    pop rcx
    pop rbx
    leave
    ret
    


write_sob_undefined:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .undefined
	call printf

	leave
	ret

section .data
.undefined:
	db "#<undefined>", 0

write_sob_integer:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	sar rsi, TYPE_BITS
	mov rdi, .int_format_string
	mov rax, 0
	call printf

	leave
	ret

section .data
.int_format_string:
	db "%ld", 0

write_sob_char:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	DATA rsi

	cmp rsi, CHAR_NUL
	je .Lnul

	cmp rsi, CHAR_TAB
	je .Ltab

	cmp rsi, CHAR_NEWLINE
	je .Lnewline

	cmp rsi, CHAR_PAGE
	je .Lpage

	cmp rsi, CHAR_RETURN
	je .Lreturn

	cmp rsi, CHAR_SPACE
	je .Lspace
	jg .Lregular

	mov rdi, .special
	jmp .done	

.Lnul:
	mov rdi, .nul
	jmp .done

.Ltab:
	mov rdi, .tab
	jmp .done

.Lnewline:
	mov rdi, .newline
	jmp .done

.Lpage:
	mov rdi, .page
	jmp .done

.Lreturn:
	mov rdi, .return
	jmp .done

.Lspace:
	mov rdi, .space
	jmp .done

.Lregular:
	mov rdi, .regular
	jmp .done

.done:
	mov rax, 0
	call printf

	leave
	ret

section .data
.space:
	db "#\space", 0
.newline:
	db "#\newline", 0
.return:
	db "#\return", 0
.tab:
	db "#\tab", 0
.page:
	db "#\page", 0
.nul:
	db "#\nul", 0
.special:
	db "#\x%01x", 0
.regular:
	db "#\%c", 0

write_sob_void:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .void
	call printf

	leave
	ret

section .data
.void:
	db "#<void>", 0
	
write_sob_bool:
	push rbp
	mov rbp, rsp

	mov rax, qword [rbp + 8 + 1*8]
	cmp rax, SOB_FALSE
	je .sobFalse
	
	mov rdi, .true
	jmp .continue

.sobFalse:
	mov rdi, .false

.continue:
	mov rax, 0
	call printf	

	leave
	ret

section .data			
.false:
	db "#f", 0
.true:
	db "#t", 0

write_sob_nil:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .nil
	call printf

	leave
	ret

section .data
.nil:
	db "()", 0

write_sob_string:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .double_quote
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	cmp rbx, CHAR_TAB
	je .ch_tab
	cmp rbx, CHAR_NEWLINE
	je .ch_newline
	cmp rbx, CHAR_PAGE
	je .ch_page
	cmp rbx, CHAR_RETURN
	je .ch_return
	cmp rbx, CHAR_SPACE
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	
.ch_tab:
	mov rdi, .fs_tab
	mov rsi, rbx
	jmp .printf
	
.ch_page:
	mov rdi, .fs_page
	mov rsi, rbx
	jmp .printf
	
.ch_return:
	mov rdi, .fs_return
	mov rsi, rbx
	jmp .printf

.ch_newline:
	mov rdi, .fs_newline
	mov rsi, rbx

.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .double_quote
	call printf

	leave
	ret
section .data
.double_quote:
	db '"', 0
.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%01x;", 0	
.fs_tab:
	db "\t", 0
.fs_page:
	db "\f", 0
.fs_return:
	db "\r", 0
.fs_newline:
	db "\n", 0

write_sob_pair:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .open_paren
	call printf
	mov rax, qword [rbp + 8 + 1*8]
	CAR rax
	push qword[rax]
	call write_sob
	add rsp, 1*8
	mov rax, qword [rbp + 8 + 1*8]
	CDR rax
	push qword[rax]
	call write_sob_pair_on_cdr
	add rsp, 1*8
	mov rdi, .close_paren
	mov rax, 0
	call printf

	leave
	ret

section .data
.open_paren:
	db "(", 0
.close_paren:
	db ")", 0

write_sob_pair_on_cdr:
	push rbp
	mov rbp, rsp

	mov rbx, qword [rbp + 8 + 1*8]
	mov rax, rbx
	TYPE rbx
	cmp rbx, T_NIL
	je .done
	cmp rbx, T_PAIR
	je .cdrIsPair
	push rax
	mov rax, 0
	mov rdi, .dot
	call printf
	call write_sob
	add rsp, 1*8
	jmp .done

.cdrIsPair:
	mov rbx, rax
	CDR rbx
	push qword[rbx]
	CAR rax
	push qword[rax]
	mov rax, 0
	mov rdi, .space
	call printf
	call write_sob
	add rsp, 1*8
	call write_sob_pair_on_cdr
	add rsp, 1*8

.done:
	leave
	ret

section .data
.space:
	db " ", 0
.dot:
	db " . ", 0

write_sob_vector:
	push rbp
	mov rbp, rsp

	mov rax, 0
	mov rdi, .fs_open_vector
	call printf

	mov rax, qword [rbp + 8 + 1*8]
	mov rcx, rax
	VECTOR_LENGTH rcx
	cmp rcx, 0
	je .done
	VECTOR_ELEMENTS rax

	push rcx
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8

.loop:
	cmp rcx, 0
	je .done

	push rcx
	push rax
	mov rax, 0
	mov rdi, .fs_space
	call printf
	
	pop rax
	push rax
	mov rax, qword [rax]
	push qword [rax]
	call write_sob
	add rsp, 1*8
	pop rax
	pop rcx
	dec rcx
	add rax, 8
	jmp .loop

.done:
	mov rax, 0
	mov rdi, .fs_close_vector
	call printf

	leave
	ret

section	.data
.fs_open_vector:
	db "#(", 0
.fs_close_vector:
	db ")", 0
.fs_space:
	db " ", 0


	
	
write_sob_symbol:
	push rbp
	mov rbp, rsp

	
	
        

	mov rax, qword [rbp + 8 + 1*8]
	DATA rax
	add rax, start_of_data
	mov rax, qword[rax]
	mov rcx, rax
	STRING_LENGTH rcx
	STRING_ELEMENTS rax

.loop:
	cmp rcx, 0
	je .done
	mov bl, byte [rax]
	and rbx, 0xff

	
	jl .ch_hex
	
	mov rdi, .fs_simple_char
	mov rsi, rbx
	jmp .printf
	
.ch_hex:
	mov rdi, .fs_hex_char
	mov rsi, rbx
	jmp .printf
	


.printf:
	push rax
	push rcx
	mov rax, 0
	call printf
	pop rcx
	pop rax

	dec rcx
	inc rax
	jmp .loop

.done:
	

	leave
	ret
section .data

.fs_simple_char:
	db "%c", 0
.fs_hex_char:
	db "\x%01x;", 0	


write_sob_fraction:
	push rbp
	mov rbp, rsp
	
	mov rsi, qword [rbp + 8 + 1*8]
	mov rdx, rsi
	Fraction_UPPER rsi
	Fraction_LOWER rdx
	mov rdi, .frac_format_string
	mov rax, 0
	call printf

	leave
	ret
section .data
.frac_format_string:
	db "%ld/%ld", 0
	
%define MAKE_LITERAL_FRACTION(up, down) ((up<<34)| (down << TYPE_BITS) | T_FRACTION)

write_sob_closure:
	push rbp
	mov rbp, rsp

	mov rsi, qword [rbp + 8 + 1*8]
	mov rdx, rsi
	CLOSURE_ENV rsi
	CLOSURE_CODE rdx
	mov rdi, .closure
	mov rax, 0
	call printf

	leave
	ret
section .data
.closure:
	db "#<closure [env:%p, code:%p]>", 0

write_sob:
	mov rax, qword [rsp + 1*8]
	TYPE rax
	jmp qword [.jmp_table + rax * 8]

section .data
.jmp_table:
	dq write_sob_undefined, write_sob_void, write_sob_nil
	dq write_sob_integer, write_sob_fraction, write_sob_bool
	dq write_sob_char, write_sob_string, write_sob_symbol
	dq write_sob_closure, write_sob_pair, write_sob_vector

section .text
write_sob_if_not_void:
	mov rax, qword [rsp + 1*8]
	cmp rax, SOB_VOID
	je .continue

	push rax
	call write_sob
	add rsp, 1*8
	mov rax, 0
	mov rdi, .newline
	call printf
	
.continue:
	ret
section .data
.newline:
	db CHAR_NEWLINE, 0