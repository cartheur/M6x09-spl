;**********************************************
;  get_ab
;
get_ab 
    jsr get_tb
    jmp get_ta
	
get_ab
	LEAX	ta
	PULU	D
	STD		0,X
	PULU	D
	STD		2,X
	RTS

