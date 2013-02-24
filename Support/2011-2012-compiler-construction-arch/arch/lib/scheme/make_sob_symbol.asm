/* scheme/make_sob_symbol.asm
 * places in R0 a symbol pointer
 * 
 */

 MAKE_SOB_BOOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(2));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_SYMBOL);
  MOV(INDD(R0,1), IMM(0));
  POP(FP);
  RETURN;

