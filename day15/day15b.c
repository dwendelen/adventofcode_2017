// Copyright (c) 2017 Daan Wendelen
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// * Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
// 
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
// 
// * Neither the name of the copyright holder nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
