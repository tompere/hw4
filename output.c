	
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
	
	/* create symbol table based on constants */
		/* Allocate memory and create the SOB string: "abc" */
	PUSH(97);
	PUSH(98);
	PUSH(99);
	PUSH(IMM(3));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(4));
	/* Allocate memory and create the SOB symbol: "abc" */
	PUSH(IMM(2));
	CALL(MALLOC);
	DROP(1);
	MOV(IND(R0), T_SYMBOL);
	MOV(INDD(R0,1), IMM(0));
	/* Allocate memory and create the SOB string: "tom" */
	PUSH(116);
	PUSH(111);
	PUSH(109);
	PUSH(IMM(3));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(4));
	PUSH(IMM(3));
	CALL(MALLOC);
	MOV(IND(22),R0);
	MOV(IND(R0),16);
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
	
END:
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
		
	STOP_MACHINE;

	return 0;
}