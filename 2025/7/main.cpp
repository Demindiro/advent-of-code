#include <iostream>
#include <sstream>
#include <vector>
#include <set>

int main(int argc, char **argv) {
	(void)argc;
	(void)argv;

	std::string line;
	std::set<size_t> beams, next_beams;
	std::vector<bool> splitters;

	if (!std::getline(std::cin, line))
		abort();

	size_t width = line.size();

	size_t start = line.find('S');
	if (start == std::string::npos)
		abort();
	beams.insert(start);
	//
	// skip every other line, which is empty anyway
	if (!std::getline(std::cin, line))
		abort();

	long long count = 0;
	size_t height = 0;
	while (std::getline(std::cin, line)) {
		++height;
		for (const auto x : line)
			splitters.push_back(x == '^');
		for (const auto x : beams) {
			auto insert = [&](size_t i) {
				next_beams.insert(i);
			};
			if (line[x] == '^') {
				if (x > 0)
					insert(x - 1);
				if (x < width - 1)
					insert(x + 1);
				++count;
			} else {
				insert(x);
			}
		}
		std::swap(beams, next_beams);
		next_beams.clear();
		if (!std::getline(std::cin, line))
			abort();
	}

	std::vector timelines(width, 0), next_timelines(width, 0);
	for (const auto x : beams)
		timelines[x] = 1;
	for (size_t y = height; y > 0; ) {
		--y;
		for (size_t x = 0; x < width; ++x) {
			if (splitters[y * width + x]) {
				if (x > 0)
					next_timelines[x] += timelines[x - 1];
				if (x < width - 1)
					next_timelines[x] += timelines[x + 1];
			} else {
				next_timelines[x] += timelines[x];
			}
		}
		std::swap(timelines, next_timelines);
		for (auto &x : next_timelines)
			x = 0;
	}

	std::cout << count << std::endl;
	std::cout << timelines[start] << std::endl;

	return 0;
}
