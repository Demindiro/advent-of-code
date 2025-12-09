#include <queue>
#include <iostream>
#include <sstream>
#include <vector>
#include <map>
#include <set>

struct Pos {
	int x, y, z;

	long long length_squared(void) const {
		return (long long)x * x + (long long)y * y + (long long)z * z;
	}

	long long distance_squared(const Pos &to) const {
		return (*this - to).length_squared();
	}

	Pos operator-(const Pos &rhs) const {
		return { x - rhs.x, y - rhs.y, z - rhs.z };
	}
};

struct Connection {
	long long len2;
	size_t a, b;

	bool operator<(const Connection &rhs) const {
		// reverse so we get smallest at top of heap
		return len2 >= rhs.len2;
	}
};

class Bfs {
	std::vector<size_t> bfs, bfs_next;

public:
	std::set<size_t> visited;
	std::map<size_t, std::set<size_t>> edges;

	size_t count(size_t root) {
		bfs.clear(), bfs_next.clear();
		size_t n = visited.size();

		auto insert = [&](size_t a) {
			if (visited.find(a) == visited.end()) {
				bfs_next.push_back(a);
				visited.insert(a);
			}
		};
		insert(root);
		while (!bfs_next.empty()) {
			std::swap(bfs, bfs_next);
			bfs_next.clear();
			for (const auto a : bfs) {
				for (const auto b : edges[a])
					insert(b);
			}
		}

		return visited.size() - n;
	}

	void connect(size_t a, size_t b) {
		edges[a].insert(b);
		edges[b].insert(a);
	}
};

int main(int argc, char **argv) {
	if (argc < 2) {
		std::cerr << "usafe: aoc8 <pair count>" << std::endl;
		return 1;
	}
	// part 2: just use some huge number
	size_t pair_count = atoll(argv[1]);

	std::string line;
	std::vector<Pos> boxes;
	std::priority_queue<Connection> wires;
	Bfs bfs;
	std::priority_queue<long long> largest;

	while (std::getline(std::cin, line)) {
		std::stringstream ss(line);
		Pos p;
		char c; // ..... ok then
		ss >> p.x >> c >> p.y >> c >> p.z;
		std::cout << p.x << "," << p.y << "," << p.z;
		std::cout << std::endl;
		boxes.push_back(p);
	}

	for (size_t i = 0; i < boxes.size(); i++) {
		for (size_t k = i + 1; k < boxes.size(); k++) {
			wires.push({ boxes[i].distance_squared(boxes[k]), i, k });
		}
	}

	size_t pairs = 0;
	while (pairs < pair_count) {
		if (wires.empty()) {
			std::cerr << "error: not enough pairs" << std::endl;
			return 1;
		}
		const Connection &n = wires.top();
		Pos a = boxes[n.a], b = boxes[n.b];
		if (0) {
			std::cout << n.len2;
			std::cout << " ";
			std::cout << a.x << "," << a.y << "," << a.z;
			std::cout << " ";
			std::cout << b.x << "," << b.y << "," << b.z;
			std::cout << std::endl;
		}
		pairs += 1;
		bfs.connect(n.a, n.b);
		wires.pop();
		bfs.visited.clear();
		if (bfs.count(n.a) == boxes.size()) {
			std::cout << a.x << " * " << b.x << " = " << (long long)a.x * b.x << std::endl;
			break;
		}
	}
	bfs.visited.clear();

	std::cout << pair_count << std::endl;
	std::cout << std::endl;
	for (const auto &[x, y] : bfs.edges) {
		for (const auto t : bfs.visited)
			std::cout << t << " ";
		std::cout << std::endl;
		largest.push(bfs.count(x));
	}

	long long a, b, c;
	a = largest.top(), largest.pop();
	b = largest.top(), largest.pop();
	c = largest.top(), largest.pop();
	std::cout << a << " " << b << " " << c << std::endl;
	std::cout << a * b * c << std::endl;

	return 0;
}
