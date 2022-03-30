#include <stdio.h>

extern "C" double add();

extern "C" double print(double n) {
	printf("%f\n", n);
	
}

//int main() {
//	printf("Res: %f", add());
//}