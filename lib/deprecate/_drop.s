;****************************************
;  drop  -  drop the top stack item
;
drop 
    jmp pop        ;  pop the stack
	
	
drop
	LEAU	2,U		; pop the stack
	RTS

