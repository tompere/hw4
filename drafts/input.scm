void print_stack(char* comment){
	int i;
	printf("printing stack, FP: %d SP: %d %s\n", (int)(FP), (int)(SP), comment);
	for(i=SP+5; i>=0; --i){
	if(SP == i){
	printf("SP ");
	}
	if(FP == i){
	printf("FP");
	}
	printf("\telement %d: ", i);
	SHOW(" ", STACK(i));
	}
}