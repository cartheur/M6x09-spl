;*****************************************
;  add1  -  add 1 to value in ta
;	
; this seems to be a 32b add operation!	
	
add1
	LEAX	ta
	LDD		0,x
	ADD		#1
	STD		0,x
	LDD		2,x
	ADC		#0
	STD		2,x
	RTS
	
	
	

