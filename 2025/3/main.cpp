#include <iostream>
#include <cstdlib>

int main(int argc, char **argv) {
	(void)argv;
	// you know the deal by now
	int loooong = argc > 1;

	std::string line;
	long long sum = 0;

	while (std::getline(std::cin, line)) {
		// dumb but not even worth spending time on
		long long max = 0;
		if (loooong) {
			// how very unpredictable...
			// actually, I'll admit: I expected we would need an arbitrary amount of digits.
			// this is a bit more difficult:
			// let's be clever: the MSD matters most, so
			// just scan for the highest, left-most digits.
			size_t start_i = 0;
			for (int n = 12; n > 0; n--) {
				int highest = 0;
				size_t highest_i = start_i;
				for (size_t i = start_i; i <= line.size() - n; i++) {
					int d = line[i] - '0';
					if (highest < d) {
						highest = d;
						highest_i = i;
					}
				}
				max = (max * 10) + highest;
				start_i = highest_i + 1;
			}
		} else {
			for (size_t i = 0; i < line.size() - 1; i++) {
				long long hi = (line[i] - '0') * 10;
				for (size_t k = i + 1; k < line.size(); k++) {
					max = std::max(max, hi + (line[k] - '0'));
				}
			}
		}
		sum += max;
	}

	std::cout << sum << std::endl;

	return 0;
}
