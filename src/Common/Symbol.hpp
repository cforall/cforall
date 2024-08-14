#pragma once

#include <string>

// Internal Type:
class SymbolData;

/// A Symbol is an interned string, where the characters are 'hidden'.
class Symbol final {
	SymbolData * data;
public:
	/// Create a Symbol for the empty string.
	Symbol();

	Symbol(std::string const &);
	Symbol(char const *);
	Symbol(Symbol const &);
	Symbol(Symbol &&);

	~Symbol();

	Symbol & operator=(std::string const &);
	Symbol & operator=(char const *);
	Symbol & operator=(Symbol const &);
	Symbol & operator=(Symbol &&);

	bool operator==(Symbol const &) const;
	bool operator!=(Symbol const &) const;

	std::string const & str() const;
	char const * c_str() const;
};
