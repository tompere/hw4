	
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
	CALL(MAKE_SOB_VOID);
	MOV(ADDR(10), IND(R0));
	
	/* Nil (Empty List) */
	CALL(MAKE_SOB_NIL);
	MOV(ADDR(11), IND(R0));
	
	/* False (Boolean) */
	PUSH(IMM(0));
	CALL(MAKE_SOB_BOOL);
	MOV(ADDR(12), IND(R0));
	MOV(ADDR(13), INDD(R0,1));
	DROP(IMM(1));
	
	/* True (Boolean) */
	PUSH(IMM(1));
	CALL(MAKE_SOB_BOOL);
	MOV(ADDR(14), IND(R0));
	MOV(ADDR(15), INDD(R0,1));
	DROP(IMM(1));
	
	/* END of initialization */
	
	MOV(R1,IMM(2));
	ADD(R1,IMM(3));
	printf("===> %d",IND(R1));
	
	STOP_MACHINE;
	
	IS_SOB_TYPE:
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
		JUMP(L_IS_SOB_BOOL_EXIT);
		TRUE_SOB_TYPE:
			MOV(R0, IMM(1));
		L_IS_SOB_BOOL_EXIT:
			POP(FP);
			RETURN;

	return 0;
}