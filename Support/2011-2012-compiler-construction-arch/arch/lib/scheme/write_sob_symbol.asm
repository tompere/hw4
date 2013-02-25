/* scheme/write_sob_symbol.asm
 * Take a pointer to a Scheme symbol object, and 
 * prints (to stdout) the character representation
 * of that symbol object.
 * 
 * Programmer: 
 */

 WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  MOV(R0, FPARG(0));
  MOV(R0, INDD(R0, 1));
  MOV(R0, IND(R0));
  PUSH(R0);
  CALL(WRITE_SOB);
  DROP(1);
  POP(FP);
  RETURN;