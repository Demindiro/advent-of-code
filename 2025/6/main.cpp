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
	bool fuckyou = argc > 1;

	std::vector<std::vector<long long> > nums;
	std::vector<char> ops;

	if (fuckyou) {
		std::vector<std::string> lines = { "" };
		while (std::getline(std::cin, lines.back())) 
			lines.push_back("");
		lines.pop_back();
		if (lines.empty())
			abort();
		read_column_ops(ops, lines.back());
		lines.pop_back();
		size_t cursor = 0;
		while (cursor < lines[0].size()) {
			nums.push_back({});
			std::vector<long long> &v = nums.back();
			for ( ; cursor < lines[0].size(); ++cursor) {
				bool all_space = true;
				long long x = 0;
				for (const auto &l : lines) {
					if (l[cursor] == ' ')
						continue;
					x = x * 10 + (l[cursor] - '0');
					all_space = false;
				}
				if (all_space)
					break;
				v.push_back(x);
			}
			++cursor;
		}
	} else {
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
	}

	for (const auto &v : nums) {
		for (const auto x : v)
			std::cout << x << " ";
		std::cout << std::endl;
	}
	for (const auto &x : ops)
		std::cout << x << " ";
	std::cout << std::endl;

	long long sum = 0;
	for (size_t i = 0; i < ops.size(); i++) {
		std::cout << i << std::endl;
		long long x;
		switch (ops[i]) {
		case '+':
			x = 0;
			if (fuckyou) {
				for (const auto v : nums[i])
					x += v;
			} else {
				for (const auto &v : nums)
					x += v[i];
			}
			break;
		case '*':
			x = 1;
			if (fuckyou) {
				for (const auto v : nums[i])
					x *= v;
			} else {
				for (const auto &v : nums)
					x *= v[i];
			}
			break;
		default: abort();
		}
		sum += x;
	}

	std::cout << sum << std::endl;

	return 0;
}
