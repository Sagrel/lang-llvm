#include <stdio.h>

extern "C" double add();

int main() {
	printf("Res: %f", add());
}