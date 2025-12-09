#include <queue>
#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <set>

struct Pos {
	int x, y;

	Pos operator-(const Pos &rhs) const {
		return { x - rhs.x, y - rhs.y };
	}

	long long area(void) const {
		return (long long)x * y;
	}
};

int main(int argc, char **argv) {
	std::string line;
	std::vector<Pos> tiles;

	while (std::getline(std::cin, line)) {
		std::stringstream ss(line);
		Pos p;
		char c; // ..... ok then
		ss >> p.x >> c >> p.y;
		std::cout << p.x << "," << p.y << std::endl;
		tiles.push_back(p);
	}

	// fuck it bruteforce
	long long area = 0;
	for (size_t i = 0; i < tiles.size(); i++) {
		for (size_t k = i + 1; k < tiles.size(); k++) {
			Pos d = tiles[i] - tiles[k];
			++d.x, ++d.y;
			area = std::max(area, d.area());
		}
	}

	std::cout << area << std::endl;

	return 0;
}
