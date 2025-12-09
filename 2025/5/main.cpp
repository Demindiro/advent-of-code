#include <iostream>
#include <map>
#include <vector>

// the definition of std::map::lower_bound is fucking insane, so provide
// a version that actually makes fucking sense.
//
// returns `map.end()` if no key is smaller.
template<typename T, typename U>
typename std::map<T, U>::iterator last_lower_incl(std::map<T, U> &map, const T &key) {
	auto it = map.lower_bound(key);
	if (it == map.end()) {
		if (!map.empty())
			return --it;
		return it;
	}
	if (key < it->first) {
		if (it == map.begin())
			return map.end();
		return --it;
	}
	return it;
}

// TODO study C++ templates...
template<typename T, typename U>
typename std::map<T, U>::const_iterator last_lower_incl(const std::map<T, U> &map, const T &key) {
	auto it = map.lower_bound(key);
	if (it == map.end()) {
		if (!map.empty())
			return --it;
		return it;
	}
	if (key < it->first) {
		if (it == map.begin())
			return map.end();
		return --it;
	}
	return it;
}

template<typename T>
class RangeSet {
public:
	// format: start as key and end as value
	std::map<T, T> ranges;

	void insert(T start, T end) {
		// situations:
		//
		// |----|    |----|
		//    |xxxxxxxx|
		//
		// |----|    |----|
		//       |xxxxx|
		//
		// |----|    |----|
		//    |xxxx|
		//
		// |----|    |----|
		//       |xx|
		//
		// approach:
		// - remove all ranges between start and end
		// - if a range on the high end overlaps, remove it and extend end appropiately
		// - if a range on the low end overlaps, update it. Otherwise insert
		//
		// to simplify things, make the range [incl;excl)
		// so we can simply do excl == incl when checking
		++end;
		for (T i = start + 1; ; ) {
			auto it = ranges.upper_bound(i);
			if (it == ranges.end() || end < it->first)
				break;
			i = it->second;
			end = std::max(end, i);
			ranges.erase(it);
		}

		auto it = last_lower_incl(ranges, start);
		if (it != ranges.end() && start <= it->second) {
			it->second = std::max(end, it->second);
		} else {
			ranges[start] = end;
		}
	}

	bool has(const T &t) const {
		auto it = last_lower_incl(ranges, t);
		return it != ranges.end() && t < it->second;
	}
};

int main(int argc, char **argv) {
	(void)argv;
	bool all = argc > 1;

	std::string line;
	RangeSet<long long> ranges;
	std::vector<std::pair<long long, long long>> check;

	while (std::getline(std::cin, line)) {
		size_t split_at = line.find('-');
		if (split_at == std::string::npos)
			break;
		std::string s_lo = line.substr(0, split_at);
		std::string s_hi = line.substr(split_at + 1);
		long long lo = atoll(s_lo.c_str()), hi = atoll(s_hi.c_str());
		ranges.insert(lo, hi);
		check.push_back({ lo, hi });

		std::cout << "+ " << lo << " - " << hi << std::endl;
		for (const auto [x, y] : ranges.ranges) {
			std::cout << x << " - " << y - 1 << std::endl;
		}
		for (const auto [x, y] : check) {
			std::cout << "? " << x << " - " << y << std::endl;
			if (!ranges.has(x) || !ranges.has(y))
				abort();
		}
	}

	long long sum = 0;
	if (all) {
		for (const auto [x, y] : ranges.ranges) {
			sum += y - x;
		}
	} else {
		while (std::getline(std::cin, line)) {
			long long x = atoll(line.c_str());
			sum += ranges.has(x);
		}
	}
	std::cout << sum << std::endl;

	return 0;
}
