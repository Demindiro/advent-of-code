#include <iostream>
#include <cstdlib>

// https://stackoverflow.com/a/12089637
template<typename T, typename U>
T modulo(T x, U y) {
	T z = x % y;
	return z < 0 ? z + y : z;
}

int main() {
	std::string line;
	long long cur = 50;
	long long clicks = 0;
	while (std::getline(std::cin, line)) {
		bool left = line.at(0) == 'L';
		long long num = atoll(line.c_str() + 1);
		// do modulo now to avoid signed integer overflow
		// (unsigned overflow would also be bad)
		num = modulo(num, 100);
		num = left ? -num : num;
		cur = modulo(cur + num, 100);
		clicks += cur == 0;
	}
	std::cout << clicks << std::endl;
	return 0;
}
