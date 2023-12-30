;****************************************
;  b_or  -  bitwise or
;
b_or
    jsr get_tb
    jsr pop
    ora tb              ;  al | bl
    sta ta
    txa        
    ora tb+1            ;  ah | bh
    tax
    lda ta
    jmp push 


b_or
    PULU	D			; take TOS into D
	PULU	Y			; take next TOS into Y
	ORR		Y,D			; logical OR the two
	PSHU	D			; push the result
	RTS
	

