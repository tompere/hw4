	
#include <stdio.h>
#include <stdlib.h>
#include "cisc.h"

int main()
{
	START_MACHINE;
	
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
	
	/* create symbol table based on constants */
		/* Allocate memory and create the SOB char c */
	MOV(R1,IMM(99));
	PUSH(R1);
	CALL(MAKE_SOB_CHAR);
	MOV(ADDR(16),R0);
	DROP(IMM(1));
	/* Allocate memory and create the SOB integer 9 */
	MOV(R1,IMM(9));
	PUSH(R1);
	CALL(MAKE_SOB_INTEGER);
	MOV(ADDR(17),R0);
	DROP(IMM(1));
	/* Allocate memory and create the SOB integer 10 */
	MOV(R1,IMM(10));
	PUSH(R1);
	CALL(MAKE_SOB_INTEGER);
	MOV(ADDR(18),R0);
	DROP(IMM(1));

	
	/* increase address */
	/* === IMPORTANT : MAKE SURE INITIAL NUMBER STARTS FROM 15 (which is hard-coded) ==== */
	ADD(ADDR(0), IMM(18))
;
	/* END of initialization */
	
	/* Fake Env */
	PUSH(IMM(0));
	PUSH(IMM(T_NIL));
	PUSH(LABEL(END));
	PUSH(FP);
	MOV(FP,SP);

	/* code generation */
		/* applic_3342061*/
	/* applic_3342061 - B3 */
	MOV(R0,ADDR(18));
	PUSH(R0);
	/* applic_3342061 - B2 */
	MOV(R0,ADDR(17));
	PUSH(R0);
	/* applic_3342061 - B1 */
	MOV(R0,ADDR(16));
	PUSH(R0);
	/* pushing number of operands to stack */
	PUSH(IMM(3));
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
	/* pvar_x */
	MOV(R0,FPARG(2));
	POP(FP);
RETURN;
L_CLOS_EXIT_3342062:
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
	
	STOP_MACHINE;

	return 0;
}