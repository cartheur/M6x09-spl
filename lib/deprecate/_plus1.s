;****************************************
;  plus1  -  ( a -- a+1 )
;
plus1
	LDD		0,U		; get TOS value
	ADDD	#1		; add 1 to it
	STD		0,U		; replace TOS with incremented value
	RTS
