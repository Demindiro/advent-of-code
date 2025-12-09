#include <iostream>
#include <vector>

class Grid {
public:
	std::vector<bool> cells;
	size_t width, height;

	bool in_bounds(ssize_t x, ssize_t y) const {
		return x >= 0 && y >= 0 && (size_t)x < width && (size_t)y < height;
	}

	bool get(ssize_t x, ssize_t y) const {
		return in_bounds(x, y) && cells[y * width + x];
	}

	size_t neighbors(ssize_t x, ssize_t y) const {
		size_t n = 0;
		for (ssize_t dy = -1; dy <= 1; dy++) {
			for (ssize_t dx = -1; dx <= 1; dx++) {
				n += (dx != 0 || dy != 0) && get(x + dx, y + dy);
			}
		}
		return n;
	}

	size_t remove_some(Grid &out) const {
		size_t n = 0;
		out.cells.clear();
		for (size_t y = 0; y < height; y++) {
			for (size_t x = 0; x < width; x++) {
				bool present = get(x, y), can_remove = neighbors(x, y) < 4;
				n += present && can_remove;
				out.cells.push_back(present && !can_remove);
			}
		}
		out.width = width;
		out.height = height;
		return n;
	}
};

int main(int argc, char **argv) {
	(void)argv;
	bool repeat = argc > 1;

	Grid grid, next_grid;

	std::string line;
	while (std::getline(std::cin, line)) {
		size_t n = grid.cells.size();
		for (char c : line)
			grid.cells.push_back(c == '@');
		grid.width = grid.cells.size() - n;
	}
	grid.height = grid.cells.size() / grid.width;

	size_t sum = 0;
	size_t n;
	do {
		n = grid.remove_some(next_grid);
		sum += n;
		std::swap(grid, next_grid);
	} while (repeat && n != 0);
	std::cout << sum << std::endl;

	return 0;
}
