;****************************************
;  abs  -  absolute value
;
abs
	LDD		0,U			; examine TOS
	TSTD				; check sign of TOS
	BPL		absx		; already positive, no further action required
	NEGD				; 2s complement of D
	STD		0,U
absx	
	RTS
	


