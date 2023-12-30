;****************************************
;  accept -  get a line of text
;            in the input buffer
;            ( addr -- len )
;
;  HD6309 SBC ROM 1V4 specific!
;
accept
	LDX		0,U    	; copy the TOS value (address of text buffer) into X
	PSHS	U		; save the user stack pointer register (needed by ROM function)
	LEAU	0,X		; put the buffer address into U 
	JSR		W_GETLNZ	; call MON 1V4 function to get line of text input
						; on entry: U = address of text buffer
						; on exit: B = index of last character in buffer
						;          A = last character in buffer
						;          V = 0  CR received
						;          V = 1  CANCEL or >120 characters received
    BVC     xaccept	; if V is clear, a line of text was entered
	CLRB			; cancel or too-long a line, so return null-string
xaccept
	CLRA			
	INCB	
	STA    	B,U
	EXG		A,B 	; form length of string (incl. terminator) from 0:B --> D
	PULS	U     	; restore the user stack pointer
	STD		0,U  	; replace TOS with length of string
	RTS
	


