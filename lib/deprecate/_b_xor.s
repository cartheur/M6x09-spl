;****************************************
;  b_xor  -  bitwise xor
;
b_xor
    jsr get_tb
    jsr pop
    eor tb              ;  al ^ bl
    sta ta
    txa             
    eor tb+1            ;  ah ^ bh
    tax
    lda ta
    jmp push 
	
b_xor
    PULU	D			; take TOS into D
	PULU	Y			; take next TOS into Y
	EORR	Y,D			; exclustive- OR the two
	PSHU	D			; push the result
	RTS
	

