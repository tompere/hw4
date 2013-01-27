	
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
	
	/* Internal function to check if a type is a SOB */
	
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
		JUMP(EXIT_IS_SOB_TYPE);
		TRUE_SOB_TYPE:
			MOV(R0, IMM(1));
		EXIT_IS_SOB_TYPE:
			POP(FP);
			RETURN;
	/* End of IS_SOB_TYPE */
	
		/* applic_3342061*/
	/* applic_3342061 - B4 */
	MOV(R0, IMM(12));
	PUSH(R0);
	/* applic_3342061 - B3 */
	MOV(R0, IMM(14));
	PUSH(R0);
	/* applic_3342061 - B2 */
	MOV(R0, IMM(12));
	PUSH(R0);
	/* applic_3342061 - B1 */
	MOV(R0, IMM(14));
	PUSH(R0);
	PUSH(4);
	PUSH(INDD(R0,1))
	CALL(INDD(R0,2))
	MOV(IND(R1),IMM(2));
	ADD(IND(R1),IMM(4));
	MOV(SP,IND(R1));

	PUSH(R0);
	CALL(WRITE_SOB);
	
	STOP_MACHINE;

	return 0;
}