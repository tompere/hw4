	
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
	
	/* create symbol table based on constants */
	
	
	/* END of initialization */
	
	/* Fake Env */
	PUSH(IMM(0));
	PUSH(IMM(T_NIL));
	PUSH(LABEL(END));
	PUSH(FP);
	MOV(FP,SP);

	/* code generation */
		/* applic_3342061*/
	/* applic_3342061 - B1 */
	/* Part A : lambda-simple 3342064*/
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
LOOP_3342064:
	CMP(R2,IMM(0));
	JUMP_GE(END_LOOP_3342064);
	MOV(R4,FPARG(0));
	MOV(INDD(R0,R3),INDD(R4,R2));
	ADD(R2,IMM(1));
	ADD(R3,IMM(1));
	JUMP(LOOP_3342064);
END_LOOP_3342064:
	MOV(R2,R0);
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R2),R0);
	MOV(R4,IMM(0));
	MOV(R5,IMM(2));
LOOP_PARAMS_3342064:
	CMP(R4,FPARG(1));
	JUMP_GE(END_LOOP_PARAMS_3342064);
	MOV(INDD(R0,R4),FPARG(R5));
	ADD(R5,IMM(1));
	ADD(R4,IMM(1));
	JUMP(LOOP_PARAMS_3342064);
END_LOOP_PARAMS_3342064:
	MOV(INDD(R1,2),LABEL(L_CLOS_CODE_3342064));
	MOV(R0,R1);
	JUMP(L_CLOS_EXIT_3342064);
	/* Part B : lambda-simple 3342064*/
L_CLOS_CODE_3342064:
	PUSH(FP);
	MOV(FP,SP);
	/* Test if_3342065 */
	/* pvar_n */
	MOV(R0,FPARG(2));
	CMP(INDD(R0,1),IMM(0));
	JUMP_EQ(DIF_LABEL_3342065);
	/* Do-if-true if_3342065 */
	MOV(R0, IMM(14));
	JUMP(END_IF_3342065);
	DIF_LABEL_3342065:
	/* Do-if-false if_3342065 */
	MOV(R0, IMM(12));
	END_IF_3342065:
	POP(FP);
RETURN;
L_CLOS_EXIT_3342064:
	PUSH(R0);
	/* pushing number of operands to stack */
	PUSH(IMM(1));
	/* generate applic's operator code */
	/* Part A : lambda-simple 3342062*/
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
LOOP_3342062:
	CMP(R2,IMM(0));
	JUMP_GE(END_LOOP_3342062);
	MOV(R4,FPARG(0));
	MOV(INDD(R0,R3),INDD(R4,R2));
	ADD(R2,IMM(1));
	ADD(R3,IMM(1));
	JUMP(LOOP_3342062);
END_LOOP_3342062:
	MOV(R2,R0);
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R2),R0);
	MOV(R4,IMM(0));
	MOV(R5,IMM(2));
LOOP_PARAMS_3342062:
	CMP(R4,FPARG(1));
	JUMP_GE(END_LOOP_PARAMS_3342062);
	MOV(INDD(R0,R4),FPARG(R5));
	ADD(R5,IMM(1));
	ADD(R4,IMM(1));
	JUMP(LOOP_PARAMS_3342062);
END_LOOP_PARAMS_3342062:
	MOV(INDD(R1,2),LABEL(L_CLOS_CODE_3342062));
	MOV(R0,R1);
	JUMP(L_CLOS_EXIT_3342062);
	/* Part B : lambda-simple 3342062*/
L_CLOS_CODE_3342062:
	PUSH(FP);
	MOV(FP,SP);
	/* tc_applic_3342063*/
	/* applic_3342063 - B1 */
	MOV(R0, IMM(14));
	PUSH(R0);
	/* pushing number of operands to stack */
	PUSH(IMM(1));
	/* generate tc_applic's operator code */
	/* pvar_x */
	MOV(R0,FPARG(2));
	/* final stage of the procedure */
	CMP(INDD(R0,0),IMM(T_CLOSURE));
	JUMP_NE(ERROR_NST);
	PUSH(FPARG(-1));
	MOV(FP,FPARG(-2));
	/********************************************/
	JUMPA(INDD(R0,2));
	POP(FP);
RETURN;
L_CLOS_EXIT_3342062:
	/* final stage of the procedure */
	CMP(INDD(R0,0),IMM(T_CLOSURE));
	JUMP_NE(ERROR_NST);
	PUSH(INDD(R0,1));
	CALLA(INDD(R0,2));
	MOV(R6,STARG(0));
	ADD(R6,IMM(2));
	DROP(R6);

END:
	PUSH(R0);
	CALL(WRITE_SOB);
	DROP(IMM(1));
		
	STOP_MACHINE;

	return 0;
}