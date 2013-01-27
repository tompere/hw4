	
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
	
	/* increase address */
	ADD(ADDR(0), IMM(15));
	/* END of initialization */
	
	/* Fake Env */
	PUSH(IMM(0));
	PUSH(IMM(T_NIL));
	PUSH(LABEL(END));
	PUSH(FP);
	MOV(FP,SP);
	
	/* Internal function to check if a type is a SOB */
	
	/*IS_SOB_TYPE:
		MOV(R0, STARG(0));
		MOV(R0, IND(R0));
		CMP(R0, IMM(T_VOID));
		JUMP_EQ(TRUE_SOB_TYPE);
		CMP(R0, IMM(T_NIL));
		JUMP_EQ(TRUE_SOB_TYPE); 
		CMP(R0, IMM(T_BOOL));
		JUMP_EQ(TRUE_SOB_TYPE); 
		CMP(R0, IMM(T_CHAR));
		JUMP_EQ(TRUE_SOB_TYPE); 
		CMP(R0, IMM(T_INTEGER));
		JUMP_EQ(TRUE_SOB_TYPE);
		CMP(R0, IMM(T_STRING));
		JUMP_EQ(TRUE_SOB_TYPE);
		CMP(R0, IMM(T_SYMBOL));
		JUMP_EQ(TRUE_SOB_TYPE);
		CMP(R0, IMM(T_PAIR));
		JUMP_EQ(TRUE_SOB_TYPE);
		CMP(R0, IMM(T_VECTOR));
		JUMP_EQ(TRUE_SOB_TYPE);
		CMP(R0, IMM(T_CLOSURE));
		JUMP_EQ(TRUE_SOB_TYPE);
		MOV(R0, IMM(0));
		JUMP(EXIT_IS_SOB_TYPE);
		TRUE_SOB_TYPE:
			MOV(R0, IMM(1));
		EXIT_IS_SOB_TYPE:
			POP(FP);
			RETURN;*/
	/* End of IS_SOB_TYPE */
	
		/* applic_3342067*/
	/* applic_3342067 - B7 */
	MOV(R0, IMM(12));
	PUSH(R0);
	/* applic_3342067 - B6 */
	MOV(R0, IMM(14));
	PUSH(R0);
	/* applic_3342067 - B5 */
	MOV(R0, IMM(14));
	PUSH(R0);
	/* applic_3342067 - B4 */
	MOV(R0, IMM(14));
	PUSH(R0);
	/* applic_3342067 - B3 */
	MOV(R0, IMM(12));
	PUSH(R0);
	/* applic_3342067 - B2 */
	MOV(R0, IMM(12));
	PUSH(R0);
	/* applic_3342067 - B1 */
	MOV(R0, IMM(12));
	PUSH(R0);
	/* pushing number of operands to stack */
	PUSH(IMM(7));
	/* generate applic's operator code */
	/* Part A : lambda-opt 3342068*/
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
LOOP_3342068:
	CMP(R2,IMM(0));
	JUMP_GE(END_LOOP_3342068);
	MOV(R4,FPARG(0));
	MOV(INDD(R0,R3),INDD(R4,R2));
	ADD(R2,IMM(1));
	ADD(R3,IMM(1));
	JUMP(LOOP_3342068);
END_LOOP_3342068:
	MOV(R2,R0);
	PUSH(FPARG(1));
	CALL(MALLOC);
	DROP(IMM(1));
	MOV(IND(R2),R0);
	MOV(R4,IMM(0));
	MOV(R5,IMM(2));
LOOP_PARAMS_3342068:
	CMP(R4,FPARG(1));
	JUMP_GE(END_LOOP_PARAMS_3342068);
	MOV(INDD(R0,R4),FPARG(R5));
	ADD(R5,IMM(1));
	ADD(R4,IMM(1));
	JUMP(LOOP_PARAMS_3342068);
END_LOOP_PARAMS_3342068:
	MOV(INDD(R1,2),LABEL(L_CLOS_CODE_3342068));
	MOV(R0,R1);
	JUMP(L_CLOS_EXIT_3342068);
	/* Part B : lambda-opt 3342068*/
L_CLOS_CODE_3342068:
	PUSH(FP);
	MOV(FP,SP);
	/* stack adjustment for lambda-opt : make a list (based on pairs) for each FPARG(i) */
	MOV(R7,IMM(FPARG(1)));
	ADD(R7,IMM(1));
	PUSH(IMM(11));
	PUSH(FPARG(R7));
	CALL(MAKE_SOB_PAIR);
	DROP(IMM(2));
	SUB(R7,IMM(1));
LOOP_PARAMS_OPT_3342068:
	CMP(R7,IMM(5));
	JUMP_LT(END_LOOP_PARAMS_OPT_3342068);
	PUSH(FPARG(R7));
	PUSH(R0);
	CALL(MAKE_SOB_PAIR);
	DROP(IMM(2));
	SUB(R7,IMM(1));
	JUMP(LOOP_PARAMS_OPT_3342068);
	END_LOOP_PARAMS_OPT_3342068:
	MOV(FPARG(5),R0);
	/* lambda-opt body */
	/* pvar_rest */
	MOV(R0,FPARG(5));
	POP(FP);
	RETURN;
L_CLOS_EXIT_3342068:
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