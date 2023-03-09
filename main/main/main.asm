.include "m328pdef.inc"

	.def	mask 	= r16		; mask register
	.def	ledR 	= r17		; led register
	.def	loopCt	= r18		; delay loop count
	.def    motorouterloop=r23  ;motor outer loop
	.def    motorinnerloop=r22	; motor loop
	.def    delay1seciloop =r21		; 1 sec delay inner loop
	.def    delay1secoloop =r20		; 1 sec delay outer loop 
	.def	iLoopRl = r24		; inner loop register low
	.def	iLoopRh = r25		; inner loop register high

	.equ	iVal 	= 200		; inner loop value of delay
	.equ    oval    = 40        ; outer loop value of delay

	.equ    dval	= 10		; delay 1 sec inner loop counter
	.equ    dval2   = 5		; delay 1 sec outer loop counter

	.equ    imotor  = 250       ;inner loop of motor
	.equ    omotor  = 20         ;outer loop of motor

	.cseg
	.org	0x00
	ldi	r16,LOW(RAMEND)		; initialize
	out	SPL,r16			; stack pointer
	ldi	r16,HIGH(RAMEND)	; to RAMEND
	out	SPH,r16			; "

	clr	ledR			; clear led register
	ldi	mask,(1<<PINB4)		; load 00000001 into mask register
	out	DDRB,mask		; set PINB0 to output


	ldi r19,0b00011111        ;define pinb0,pinb1,pinb2,pinb3(mortors) ,pinb4(magnet) as outputs
	out	DDRB,r19
	CBI DDRD, pd6   ; set PD6 as input (ir sensor input)
	CBI DDRD, pd7   ; switch as input
	; SBI DDRB, pb4	; electromagnet

	ldi loopCt,oval ; outer loop of delay
	ldi motorouterloop,omotor ;motor outer loop count


start:	
	switch:
		SBIS PIND, 7 ; when switch is pressed skip the bellow
		rjmp switch

		ir_logic:
			SBIS PIND, 6 ; skip bellow statement if ir out is logic 1
			rjmp reverse_loop
			
			;forward
			sbi portb, pb4 ; magnet on
			rcall delay1sec	; long delay
	
			rcall forward	;motor
			rcall delay1sec	;long delay
			;return to the initial position
			cbi portb,pb4	;magnet off
			rcall delay1sec	;long delay
			rcall reverse	;motor

			rjmp switch


;;;;;;;;;;;;;;;;;;;;;; reverse motor ;;;;;;;;;;;;
reverse_loop:
			sbi portb, pb4 ; magnet on
			rcall delay1sec	;long delay
			rcall reverse ;motor
			rcall delay1sec	;long delay
			cbi portb, pb4	;magnet off
			rcall delay1sec	;long delay
			rcall forward ;motor
	
	rjmp switch








										;;;     delay function    ;;;;;
delay10ms:
	
	ldi	iLoopRl,LOW(iVal)	; intialize inner loop count in inner
	ldi	iLoopRh,HIGH(iVal)	; loop high and low registers

iLoop:	dec	iLoopRl		; decrement inner loop registers
	brne	iLoop			; branch to iLoop if iLoop registers != 0

	dec	loopCt			; decrement outer loop register
	brne	delay10ms		; branch to oLoop if outer loop register != 0

	ldi loopCt,oval ; outer loop of delay
	nop
	ret				; return from subroutine




										;;;     forward function    ;;;;;

forward:
	ldi	motorinnerloop, imotor	; intialize inner loop count of motor
	

motorLoop:	
	;Motor forward a circle
	;ldi r19,0b00000001   ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb0

	rcall delay10ms

	;ldi r19,0b00000010    ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb1

	rcall delay10ms

	;ldi r19,0b00000100    ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb2

	rcall delay10ms

	;ldi r19,0b00001000    ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb3

	rcall delay10ms


	dec	motorinnerloop		; decrement inner motor loop registers
	brne	motorLoop			; branch to motorLoop if iLoop registers != 0

	dec	motorouterloop	; decrement outer loop register
	brne	forward		; branch to oLoop if outer loop register != 0

	ldi motorouterloop,omotor ;motor outer loop count
	ret				; return from subroutine


											;;;     reverse function    ;;;;;


reverse:
ldi	motorinnerloop,imotor	; intialize inner loop count of motor
	

motorLoop2:	
	;Motor forward a circle
	;ldi r19,0b00001000   ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb3

	rcall delay10ms

	;ldi r19,0b00000100    ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb2

	rcall delay10ms

	;ldi r19,0b00000010    ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb1

	rcall delay10ms

	;ldi r19,0b00000001    ; stepper mortor control
	;out	PORTB,r19
	sbi portb,pb0

	rcall delay10ms


	dec	motorinnerloop		; decrement inner motor loop registers
	brne	motorLoop2			; branch to motorLoop if iLoop registers != 0

	dec	motorouterloop	; decrement outer loop register
	brne	reverse		; branch to oLoop if outer loop register != 0

	ldi motorouterloop,omotor ;motor outer loop count
	ret				; return from subroutine


							;;;;; long delay function ;;;;;;;

delay1sec:	
		ldi	delay1seciloop,dval	; intialize inner loop count in inner

longdelayLoop:
	rcall delay10ms 
	dec	delay1seciloop		; decrement inner loop registers
	brne	longdelayLoop			; branch to iLoop if iLoop registers != 0

	dec	delay1secoloop			; decrement outer loop register
	brne	delay1sec		; branch if outer loop register != 0

	ldi delay1seciloop,dval2 ; outer loop of delay
	
	ret				; return from subroutine