;****************************************
;  btest  -  ( n b -- 0|1 )
;  test if bit b of n is set (ie, return
;  the bit value)
;
btest 
    jsr get_ab
    lda tb
    and #$0f        ;  only low nibble
    beq btest_done
    tax
btest_loop 
    lsr ta+1
    ror ta
    dex
    bne btest_loop
btest_done 
    lda ta
    and #1
    jsr push
	
btest
	LEAX	ta
	PULU	D
	STD		0,X
	PULU	D	
	ANDD	0x000F
	TFR		D,Y		; Y contains index into bit #
	LDW		0,Y		; load bitmask into W
	LDD		0,X		; get first operand again
	ANDR	D,W		; AND operand with mask
	BEQ		btestx	; if zero, push it
	LDD		0xFFFF	; one, so load -1
	
	
	
	

