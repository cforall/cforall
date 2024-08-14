#include "Symbol.hpp"

#include <unordered_map>

// This class (and the member-like helpers) are the implementation details
// of the Symbol class. It is just does reference counting to share strings.
// It is designed for maintainablity, but also to optimize the copy cases.
class SymbolData {
public:
	std::string const value;
	// As long as we are using a pointer sized counter,
	// overflow should never be an issue.
	std::size_t refCount;

	SymbolData(std::string const & value, std::size_t refCount) :
		value(value), refCount(refCount)
	{}
};

namespace {
	template<typename T>
	struct IndirectHash {
		std::hash<T> hasher;
		std::size_t operator()(T const * const & val) const {
			return hasher(*val);
		}
	};

	template<typename T>
	struct IndirectEqualTo {
		std::equal_to<T> equals;
		bool operator()(T const * const & lhs, T const * const & rhs) const {
			return equals(*lhs, *rhs);
		}
	};
} // namespace

using SymbolMap = std::unordered_map<std::string const *, SymbolData *,
	IndirectHash<std::string>, IndirectEqualTo<std::string>>;

/// Get the storage for the map of symbol data.
// Note: This is basically static memory, but wrapping the data in a function
// causes the constructor to run with the proper timing.
static SymbolMap & getAllSymbols() {
	static SymbolMap allSymbols;
	return allSymbols;
}

/// Clear out all unsed SymbolData.
static void eraseUnused() {
	SymbolMap & allSymbols = getAllSymbols();

	auto it = allSymbols.begin();
	while (it != allSymbols.end()) {
		if (it->second->refCount) {
			++it;
		} else {
			delete it->second;
			it = allSymbols.erase(it);
		}
	}
}

/// Convert a string to a new or existing SymbolData.
static SymbolData * getSymbolData(std::string const & str) {
	SymbolMap & allSymbols = getAllSymbols();

	// If there is an existing symbol, re-use it.
	auto it = allSymbols.find(&str);
	if (it != allSymbols.end()) {
		SymbolData * data = it->second;
		++data->refCount;
		return data;
	}

	// If these is no existing symbol, create the required data.
	SymbolData * data = new SymbolData(str, 1);
	allSymbols.insert(std::make_pair(&data->value, data));
	return data;
}

/// Reduce reference count and free the SymbolData as needed.
static void decSymbolData(SymbolData * data) {
	// First comes the actual decrement.
	--data->refCount;

	// If it has hit zero, remove it from the map.
	if (0 == data->refCount) {
		eraseUnused();
	}
}

Symbol::Symbol() : data(getSymbolData(std::string())) {}

Symbol::Symbol(std::string const & str) : data(getSymbolData(str)) {}

Symbol::Symbol(char const * str) : data(getSymbolData(str)) {}

Symbol::Symbol(Symbol const & other) : data(other.data) {
	++data->refCount;
}

Symbol::Symbol(Symbol && other) : data(other.data) {
	++data->refCount;
}

Symbol::~Symbol() {
	decSymbolData(data);
}

Symbol & Symbol::operator=(std::string const & str) {
	SymbolData * dat = getSymbolData(str);
	decSymbolData(data);
	data = dat;
	return *this;
}

Symbol & Symbol::operator=(char const * str) {
	return this->operator=(std::string(str));
}

Symbol & Symbol::operator=(Symbol const & other) {
	++other.data->refCount;
	decSymbolData(data);
	data = other.data;
	return *this;
}

Symbol & Symbol::operator=(Symbol && other) {
	Symbol const & ref = other;
	return this->operator=(ref);
}

bool Symbol::operator==(Symbol const & other) const {
	return data == other.data;
}

bool Symbol::operator!=(Symbol const & other) const {
	return data != other.data;
}

std::string const & Symbol::str() const {
	return data->value;
}

char const * Symbol::c_str() const {
	return data->value.c_str();
}
