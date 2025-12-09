#include <iostream>
#include <cstdlib>

// https://stackoverflow.com/a/12089637
template<typename T, typename U>
T modulo(T x, U y) {
	T z = x % y;
	return z < 0 ? z + y : z;
}

int main(int argc, char **argv) {
	(void)argv;
	// whatever, as long as it works!
	bool any_click = argc > 1;

	std::string line;
	long long cur = 50;
	long long clicks = 0;
	while (std::getline(std::cin, line)) {
		bool left = line.at(0) == 'L';
		long long num = atoll(line.c_str() + 1);
		if (any_click) {
			// the idea:
			// - we now that N / 100 = minimum amount of clicks
			// - question is: +1 depending on current position
			clicks += num / 100;
			num = modulo(num, 100);
			num = left ? -num : num;
			// we want to +1 as soon as we hit 0
			// the case cur == 0 is annoying, so handle it specially
			if (cur == 0) {
				cur = num;
			} else {
				cur = cur + num;
				clicks += cur >= 100 || cur <= 0;
			}
			cur = modulo(cur, 100);
		} else {
			// do modulo now to avoid signed integer overflow
			// (unsigned overflow would also be bad)
			num = modulo(num, 100);
			num = left ? -num : num;
			cur = modulo(cur + num, 100);
			clicks += cur == 0;
		}
	}
	std::cout << clicks << std::endl;
	return 0;
}
