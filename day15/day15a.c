#include <stdint.h>
#include <stdio.h>

int64_t generateNext(int64_t input, int64_t factor) {
	return (input * factor) % 2147483647;
}

int match(int64_t val1, int64_t val2) {
	int64_t v1 = val1 & 0xffff;
	int64_t v2 = val2 & 0xffff;
	return v1 == v2;
}

int main() {
	int loops = 40 * 1000 * 1000;
	
	int64_t val1 = 722;
	int64_t val2 = 354;

	int nbOfMatches = 0;

	for(int i = 0; i < loops; i++) {
		val1 = generateNext(val1, 16807);
		val2 = generateNext(val2, 48271);

		if(match(val1, val2)) {
			nbOfMatches++;		
		}
	}
	printf("%i", nbOfMatches);
}
