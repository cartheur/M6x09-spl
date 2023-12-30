;****************************************
;  add  -  ( a b -- a+b )
;
add
	PULU	X		; get B operand
	PULU	Y		; get A operand	
	ADDR	X,Y		; add them together
	PSHU	Y		; push answer
	RTS

