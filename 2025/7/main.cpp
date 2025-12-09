#include <iostream>
#include <sstream>
#include <vector>
#include <set>

int main(int argc, char **argv) {
	(void)argc;
	(void)argv;

	std::string line;
	std::vector<std::vector<bool> > splitters;
	std::set<size_t> beams, next_beams;

	if (!std::getline(std::cin, line))
		abort();

	size_t start = line.find('S');
	if (start == std::string::npos)
		abort();
	beams.insert(start);
	//
	// skip every other line, which is empty anyway
	if (!std::getline(std::cin, line))
		abort();

	long long count = 0;
	while (std::getline(std::cin, line)) {
		for (const auto x : beams) {
			if (line[x] == '^') {
				if (x > 0)
					next_beams.insert(x - 1);
				if (x < line.size())
					next_beams.insert(x + 1);
				++count;
			} else {
				next_beams.insert(x);
			}
		}
		std::swap(beams, next_beams);
		next_beams.clear();
		if (!std::getline(std::cin, line))
			abort();
	}

	std::cout << count << std::endl;

	return 0;
}
