#include <iostream>
#include <sstream>
#include <vector>

template<typename T>
void read_numbers(std::vector<T> &out, std::stringstream &ss) {
	while (1) {
		long long x;
		ss >> x;
		if (ss.fail())
			break;
		std::cout << " " << x;
		out.push_back(x);
	}
	std::cout << std::endl;
}

template<typename T>
void read_column_numbers(std::vector<T> &out, const std::string &line) {
	std::stringstream ss;
	ss.str(line);
	read_numbers(out, ss);
}

void read_column_ops(std::vector<char> &out, const std::string &line) {
	for (char c : line) {
		if (c == ' ')
			continue;
		out.push_back(c);
	}
}

int main(int argc, char **argv) {
	(void)argv;
	(void)argc;

	std::vector<std::vector<long long> > nums;
	std::vector<char> ops;
	std::string line, next_line;
	if (!std::getline(std::cin, line)) 
		abort();
	while (std::getline(std::cin, next_line)) {
		nums.push_back({});
		read_column_numbers(nums.back(), line);
		std::swap(line, next_line);
		next_line.clear();
	}
	read_column_ops(ops, line);

	long long sum = 0;
	for (size_t i = 0; i < ops.size(); i++) {
		long long x;
		switch (ops[i]) {
		case '+':
			x = 0;
			for (const auto &v : nums)
				x += v[i];
			break;
		case '*':
			x = 1;
			for (const auto &v : nums)
				x *= v[i];
			break;
		default: abort();
		}
		sum += x;
	}

	std::cout << sum << std::endl;

	return 0;
}
