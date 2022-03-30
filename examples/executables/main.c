#include <stdio.h>

extern "C" double add();

extern "C" double print_number(double n) {
	printf("%f\n", n);	
}

extern "C" double print_text(const char* t) {
	printf("%s\n", t);	
}

//int main() {
//	printf("Res: %f", add());
//}