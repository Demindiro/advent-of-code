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

	std::pair<Pos, Pos> mix(const Pos &with) const {
		return { { x, with.y }, { with.x, y } };
	}

	bool operator<(const Pos &rhs) const {
		if (x == rhs.x) {
			return y < rhs.y;
		}
		return x < rhs.x;
	}

	bool operator==(const Pos &rhs) const {
		return x == rhs.x && y == rhs.y;
	}
};

int main(int argc, char **argv) {
	std::string line;
	std::vector<Pos> tiles;
	std::set<Pos> tiles_set;

	while (std::getline(std::cin, line)) {
		std::stringstream ss(line);
		Pos p;
		char c; // ..... ok then
		ss >> p.x >> c >> p.y;
		std::cout << p.x << "," << p.y << std::endl;
		tiles.push_back(p);
		tiles_set.insert(p);
	}

	// fuck it bruteforce
	long long area = 0;
	for (size_t i = 0; i < tiles.size(); i++) {
		for (size_t k = i + 1; k < tiles.size(); k++) {
			const auto [a, b] = tiles[i].mix(tiles[k]);
			if (tiles_set.find(a) == tiles_set.end() && tiles_set.find(b) == tiles_set.end())
				continue;
			Pos d = tiles[i] - tiles[k];
			++d.x, ++d.y;
			area = std::max(area, d.area());
		}
	}

	std::cout << area << std::endl;

	return 0;
}
