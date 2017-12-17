#include <stdint.h>
#include <stdio.h>

int64_t generateNext(int64_t input, int64_t factor, int mod) {
	int64_t newValue = input;
	do {
		newValue = (newValue * factor) % 2147483647;
	} while((newValue % mod) != 0);
	return newValue;
}

int match(int64_t val1, int64_t val2) {
	int64_t v1 = val1 & 0xffff;
	int64_t v2 = val2 & 0xffff;
	return v1 == v2;
}

int main() {
	int loops = 5 * 1000 * 1000;
	
	int64_t val1 = 722;
	int64_t val2 = 354;

	int nbOfMatches = 0;

	for(int i = 0; i < loops; i++) {
		val1 = generateNext(val1, 16807, 4);
		val2 = generateNext(val2, 48271, 8);

		if(match(val1, val2)) {
			nbOfMatches++;		
		}
	}
	printf("%i", nbOfMatches);
}
