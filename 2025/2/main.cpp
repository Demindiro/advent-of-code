#include <iostream>
#include <cstdlib>
#include <set>

template<typename T>
int num_digits(T x) {
	int n = 0;
	do {
		x /= 10;
		n++;
	} while (x != 0);
	return n;
}

/**
 * Cut off the lower N digits.
 */
template<typename T>
T cut(T x, int n) {
	while (n-- > 0)
		x /= 10;
	return x;
}

/**
 * Cut off the lower half of digits.
 * "Rounded down" for numbers with an odd amount of digits.
 *
 * e.g. `123456` -> `123`, `123` -> `1`.
 */
template<typename T>
T half(T x) {
	return cut(x, (num_digits(x) + 1) / 2);
}

template<typename T>
T ipow(T base, int n) {
	T acc = 1;
	while (n > 0) {
		if (n & 1)
			acc *= base;
		base *= base;
		n >>= 1;
	}
	return acc;
}

/**
 * Join two numbers together
 *
 * e.g. `123` `456` -> `123456`
 */
template<typename T>
T join(T x, T y) {
	int n = num_digits(y);
	return x * ipow(10LL, n) + y;
}

int main(int argc, char **argv) {
	// again: lololol
	(void)argv;
	int at_least_twice = argc > 1;

	// in summary:
	// - sequence repeated *exactly twice*
	// - (part 2) *at least* twice. Who could have seen this coming?

	std::string line;
	long long sum = 0;

	while (std::getline(std::cin, line, ',')) {
		size_t split_at = line.find('-');
		if (split_at == std::string::npos)
			abort();
		std::string s_lo = line.substr(0, split_at);
		std::string s_hi = line.substr(split_at + 1);

		long long lo = atoll(s_lo.c_str()), hi = atoll(s_hi.c_str());
		std::cout << lo << " - " << hi << std::endl;

		if (at_least_twice) {
			// this is totally dumb but idc
			// the essential idea: bruteforce!
			//
			// This took an embarassingly long amount of time to come up with
			// as I tried to be too clever and missed a ton of obvious bugs.
			int n_digits = num_digits(hi);
			// set to avoid counting duplicates
			std::set<long long> dedup;
			// *at least* twice, so / 2
			for (int n = 1; n <= n_digits / 2; n++) {
				long long start = ipow(10LL, n - 1), end = ipow(10LL, n);
				std::cout << "--  " << start << " " << end << std::endl;
				for (long long x = start; x < end; x++) {
					long long xx = join(x, x);
					for (int nn = n * 2; nn <= n_digits; nn += n) {
						if (xx >= lo && xx <= hi && dedup.find(xx) == dedup.end()) {
							dedup.insert(xx);
							std::cout << "  " << xx << std::endl;
							sum += xx;
						}
						xx = join(xx, x);
					}
				}
			}
		} else {
			// IDs with odd number of digits are always valid, so skip those
			int n = num_digits(lo);
			n += n % 2;
			for ( ; n <= num_digits(hi); n += 2) {
				long long lo_m = std::max(cut(lo, n / 2), ipow(10LL, n / 2 - 1)),
						  hi_m = std::min(cut(hi, n / 2), ipow(10LL, n / 2) - 1);
				std::cout << ": " << lo_m << " --- " << hi_m << std::endl;
				for (long long x = lo_m; x <= hi_m; x++) {
					long long xx = join(x, x);
					// TODO better calculation of ranges,
					// but I'm wasting too much time already
					if (xx < lo || xx > hi)
						continue;
					std::cout << "  " << xx << std::endl;
					sum += xx;
				}
			}
		}
	}

	std::cout << sum << std::endl;

	return 0;
}
