	NAME EMU
BOOT	SEGMENT AT 0FFFFH
STR	LABEL	WORD
BOOT	ENDS
code segment at 01000h
assume	cs:code
START:

	MOV	CX,127

	MOV	DX,100H
	MOV	AX,800H
	XOR BX,BX
C1:
	OUT 	DX,AX
	ADD	DX,2
	INC AX
	NOP
	LOOP	C1
	MOV	DX,01FEH
	MOV	AX,87fH
	OUT	DX,AX
c2:	mov	dx,0140h
	mov	ax,400h
	out	dx,ax
	NOP
	NOP
	NOP
	CLI
	MOV	AX,1
	OUT	0AAH,AX
	mov	ax,04000h
	mov	di,0
	mov	es,ax
WR:	mov	di,0
	mov	cx,4096
	mov	ax,0ffffh
	rep	stosw
	NOP
	NOP
	JMP WR
	push	es
	pop	ds
c3:	mov	si,0
	mov	cx,4096
c4:	mov	ax,1234h
	lodsw
	cmp ax,0ffffh
	je	c5
	mov	al,'!'
	out	0,al
	NOP
	NOP
	NOP
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
c5:	loop	c4
	JMP	C3
code	ends
	end
