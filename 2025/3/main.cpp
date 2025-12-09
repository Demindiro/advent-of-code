#include <iostream>
#include <cstdlib>

int main(int argc, char **argv) {
	(void)argv;
	(void)argc;

	std::string line;
	long long sum = 0;

	while (std::getline(std::cin, line)) {
		// dumb but not even worth spending time on
		int max = 0;
		for (size_t i = 0; i < line.size() - 1; i++) {
			int hi = (line[i] - '0') * 10;
			for (size_t k = i + 1; k < line.size(); k++) {
				max = std::max(max, hi + (line[k] - '0'));
			}
		}
		sum += max;
	}

	std::cout << sum << std::endl;

	return 0;
}
