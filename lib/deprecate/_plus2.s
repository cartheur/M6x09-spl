;****************************************
;  plus2  -  ( a -- a+2 )
;
plus2 
	LDD		0,U		; get TOS value
	ADDD	#2		; add 2 to it
	STD		0,U		; replace TOS with incremented value
	RTS


