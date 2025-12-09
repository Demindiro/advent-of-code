#include <numeric>
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

	std::vector<long long> timelines(line.size(), 0), next_timelines(line.size(), 0);

	size_t start = line.find('S');
	if (start == std::string::npos)
		abort();
	beams.insert(start);
	timelines[start] = 1;
	//
	// skip every other line, which is empty anyway
	if (!std::getline(std::cin, line))
		abort();

	long long count = 0;
	while (std::getline(std::cin, line)) {
		for (const auto x : beams) {
			auto insert = [&](size_t i) {
				next_beams.insert(i);
				next_timelines[i] += timelines[x];
			};
			if (line[x] == '^') {
				if (x > 0)
					insert(x - 1);
				if (x < line.size())
					insert(x + 1);
				++count;
			} else {
				insert(x);
			}
		}
		std::swap(beams, next_beams);
		std::swap(timelines, next_timelines);
		next_beams.clear();
		for (auto &x : next_timelines)
			x = 0;
		if (!std::getline(std::cin, line))
			abort();
	}

	std::cout << count << std::endl;
	std::cout << std::accumulate(timelines.begin(), timelines.end(), 0LL) << std::endl;

	return 0;
}
