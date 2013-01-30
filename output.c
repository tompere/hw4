	
#include <stdio.h>
#include <stdlib.h>
#include "cisc.h"

int main()
{
	START_MACHINE;
	
	void print_heap(){
		int i;
		printf("\n");
		printf("printing heap\n");
		for (i=ADDR(0); i>=0; i--){
			printf("\t element %d: %d\n", i, ADDR(i));
		}
	}
	
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
		/* Allocate memory and create the SOB string: "abcdefghijklmnopqrstuvwxyz" */
	PUSH(97);
	PUSH(98);
	PUSH(99);
	PUSH(100);
	PUSH(101);
	PUSH(102);
	PUSH(103);
	PUSH(104);
	PUSH(105);
	PUSH(106);
	PUSH(107);
	PUSH(108);
	PUSH(109);
	PUSH(110);
	PUSH(111);
	PUSH(112);
	PUSH(113);
	PUSH(114);
	PUSH(115);
	PUSH(116);
	PUSH(117);
	PUSH(118);
	PUSH(119);
	PUSH(120);
	PUSH(121);
	PUSH(122);
	PUSH(IMM(26));
	CALL(MAKE_SOB_STRING);
	DROP(IMM(27));
	/* Allocate memory and create the SOB integer: 2013 */
	PUSH(IMM(2013));
	CALL(MAKE_SOB_INTEGER);
	DROP(IMM(1));
	/* Allocate memory and create the SOB char: f */
	PUSH(IMM(102));
	CALL(MAKE_SOB_CHAR);
	DROP(IMM(1));

	
	/* END of initialization */
	
	/* Fake Env */
	PUSH(IMM(0));
	PUSH(IMM(T_NIL));
	PUSH(LABEL(END));
	PUSH(FP);
	MOV(FP,SP);

	/* code generation */
		/* applic_3342065*/
	/* applic_3342065 - B3 */
	MOV(R0,IMM(46));
	PUSH(R0);
	/* applic_3342065 - B2 */
	MOV(R0,IMM(44));
	PUSH(R0);
	/* applic_3342065 - B1 */
	MOV(R0,IMM(16));
	PUSH(R0);
	/* pushing number of operands to stack */
	PUSH(IMM(3));
	/* generate applic's operator code */
	/* Part A : lambda-simple 3342066*/
	PUSH(IMM(3));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R0),IMM(T_CLOSURE));
	MOV(R1,R0);
	PUSH(IMM(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(INDD(R1,1),R0);
	MOV(R2,IMM(0));
	MOV(R3,IMM(1));
LOOP_3342066:
	CMP(R2,IMM(0));
	JUMP_GE(END_LOOP_3342066);
	MOV(R4,FPARG(0));
	MOV(INDD(R0,R3),INDD(R4,R2));
	ADD(R2,IMM(1));
	ADD(R3,IMM(1));
	JUMP(LOOP_3342066);
END_LOOP_3342066:
	MOV(R2,R0);
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R2),R0);
	MOV(R4,IMM(0));
	MOV(R5,IMM(2));
LOOP_PARAMS_3342066:
	CMP(R4,FPARG(1));
	JUMP_GE(END_LOOP_PARAMS_3342066);
	MOV(INDD(R0,R4),FPARG(R5));
	ADD(R5,IMM(1));
	ADD(R4,IMM(1));
	JUMP(LOOP_PARAMS_3342066);
END_LOOP_PARAMS_3342066:
	MOV(INDD(R1,2),LABEL(L_CLOS_CODE_3342066));
	MOV(R0,R1);
	JUMP(L_CLOS_EXIT_3342066);
	/* Part B : lambda-simple 3342066*/
L_CLOS_CODE_3342066:
	PUSH(FP);
	MOV(FP,SP);
	/* pvar_z */
	MOV(R0,FPARG(4));
	POP(FP);
RETURN;
L_CLOS_EXIT_3342066:
	/* final stage of the procedure */
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	MOV(R6,STARG(0));
	ADD(R6,IMM(2));
	DROP(R6);

END:
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
	
	print_heap();
	
	STOP_MACHINE;

	return 0;
}