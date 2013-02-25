	
#include <stdio.h>
#include <stdlib.h>
#include "cisc.h"

int main()
{
	START_MACHINE;
	
	/* test functions section */
	void print_heap(){
		int i;
		printf("\n");
		printf("printing heap\n");
		for (i=ADDR(0); i>=0; i--){
			printf("\t element %d: %d\n", i, ADDR(i));
		}
	}
	
	void print_stack(){
        int i;
        printf("printing stack, FP: %d SP: %d\n", (int)(FP), (int)(SP));
        for(i=SP+5; i>=0; --i){
			if(SP == i){
				printf("SP ");
			}
			if(FP == i){
				printf("FP ");
			}
			printf("\telement %d: %d \n", i, STACK(i));
        }
	}
	
	/* end of test functions section */
	
	JUMP(CONTINUE);

	#include "scheme.lib"
	#include "char.lib"
	#include "io.lib"
	#include "math.lib"
	#include "string.lib"
	#include "system.lib"
	
		/* error_no-such-type */
ERROR_NST:
	PUSH(69);
	PUSH(114);
	PUSH(114);
	PUSH(111);
	PUSH(114);
	PUSH(58);
	PUSH(32);
	PUSH(110);
	PUSH(111);
	PUSH(32);
	PUSH(115);
	PUSH(117);
	PUSH(99);
	PUSH(104);
	PUSH(32);
	PUSH(116);
	PUSH(121);
	PUSH(112);
	PUSH(101);
	PUSH(IMM(19));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(20));
	PUSH(R0);
	CALL(WRITE_SOB_STRING);
return 1;	/* error_undefined-variable */
ERROR_UNDEFINED_VAR:
	PUSH(69);
	PUSH(114);
	PUSH(114);
	PUSH(111);
	PUSH(114);
	PUSH(58);
	PUSH(32);
	PUSH(118);
	PUSH(97);
	PUSH(114);
	PUSH(105);
	PUSH(97);
	PUSH(98);
	PUSH(108);
	PUSH(101);
	PUSH(32);
	PUSH(IMM(16));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(17));
	PUSH(R0);
	CALL(WRITE_SOB_STRING);
	DROP(IMM(1));
	PUSH(R1);
	CALL(WRITE_SOB);
	DROP(IMM(1));
	PUSH(32);
	PUSH(105);
	PUSH(115);
	PUSH(32);
	PUSH(117);
	PUSH(110);
	PUSH(100);
	PUSH(101);
	PUSH(102);
	PUSH(105);
	PUSH(110);
	PUSH(101);
	PUSH(100);
	PUSH(46);
	PUSH(IMM(14));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(15));
	PUSH(R0);
	CALL(WRITE_SOB_STRING);
return 1;
CONTINUE:
	
	/* Initialize stack with default values */
	
	/* Void */
	MOV(ADDR(10), IMM(T_VOID));
	
	/* Nil (Empty List) */
	MOV(ADDR(11), IMM(T_NIL));
	
	/* False (Boolean) */
	MOV(ADDR(12), IMM(T_BOOL));
	MOV(ADDR(13), IMM(0));
	
	/* True (Boolean) */
	MOV(ADDR(14), IMM(T_BOOL));
	MOV(ADDR(15), IMM(1));
	
	/* Increase address */
	ADD(ADDR(0), IMM(15))
;
	
	/* create constants table*/
		/* Allocate memory and create the SOB string: "a" */
	PUSH(97);
	PUSH(IMM(1));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(2));
	/* Allocate memory and create the SOB symbol: "a" */
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_SYMBOL);
	MOV(INDD(R0,1), IMM(0));
	/* Allocate memory and create the SOB string: "b" */
	PUSH(98);
	PUSH(IMM(1));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(2));
	/* Allocate memory and create the SOB symbol: "b" */
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_SYMBOL);
	MOV(INDD(R0,1), IMM(0));
	/* Allocate memory and create the SOB string: "c" */
	PUSH(99);
	PUSH(IMM(1));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(2));
	/* Allocate memory and create the SOB symbol: "c" */
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_SYMBOL);
	MOV(INDD(R0,1), IMM(0));
	PUSH(11);
	PUSH(29);
	CALL(MAKE_SOB_PAIR);
	DROP(IMM(2));
	PUSH(31);
	PUSH(24);
	CALL(MAKE_SOB_PAIR);
	DROP(IMM(2));
	PUSH(34);
	PUSH(19);
	CALL(MAKE_SOB_PAIR);
	DROP(IMM(2));
MOV(IND(1),40);
	/* Create bucket for symbol : a */
	PUSH(IMM(3));
	CALL(MALLOC);
	MOV(IND(20),R0);
	MOV(IND(R0),16);
	MOV(INDD(R0,1),43);
	MOV(INDD(R0,2),IMM(0));
	/* Create bucket for symbol : b */
	PUSH(IMM(3));
	CALL(MALLOC);
	MOV(IND(25),R0);
	MOV(IND(R0),21);
	MOV(INDD(R0,1),46);
	MOV(INDD(R0,2),IMM(0));
	/* Create bucket for symbol : c */
	PUSH(IMM(3));
	CALL(MALLOC);
	MOV(IND(30),R0);
	MOV(IND(R0),26);
	MOV(INDD(R0,1),0);
	MOV(INDD(R0,2),IMM(0));

	/* END of initialization */
	
	/* Fake Env */
	PUSH(IMM(0));
	PUSH(IMM(T_NIL));
	PUSH(LABEL(END));
	PUSH(FP);
	MOV(FP,SP);

	/* code generation */
		MOV(R0,IMM(37));

END:
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
	
	print_heap();
		
	STOP_MACHINE;

	return 0;
}