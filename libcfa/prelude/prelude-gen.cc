//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// prelude-gen.cc --
//
// Author           : Rob Schluntz and Thierry Delisle
// Created On       : Sat Feb 16 08:44:58 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Sun Jan 12 20:28:33 2025
// Update Count     : 53
//

#include <algorithm>
#include <array>
#include <iostream>
#include <string>
#include <vector>
using namespace std;

static struct{
	const string name;
	bool isFloat;
	bool hasComparison;		// CANNOT COMPARE COMPLEX NUMBERS!!!
} basicTypes[] = {
	{ "char",					false, true,  },
	{ "signed char",			false, true,  },
	{ "unsigned char",			false, true,  },
	{ "signed short",			false, true,  },
	{ "unsigned short",			false, true,  },
	{ "signed int",				false, true,  },
	{ "unsigned int",			false, true,  },
	{ "signed long int",		false, true,  },
	{ "unsigned long int",		false, true,  },
	{ "signed long long int",	false, true,  },
	{ "unsigned long long int",	false, true,  },
	{ "float",					true , true,  },
	{ "double",					true,  true,  },
	{ "long double",			true,  true,  },
	{ "float _Complex",			true,  false, },
	{ "double _Complex",		true,  false, },
	{ "long double _Complex",	true,  false, },
#if defined(__SIZEOF_INT128__)
	{ "__int128",				false, true,  },
	{ "unsigned __int128",		false, true,  },
#endif
#if defined(__i386__) || defined(__ia64__) || defined(__x86_64__)
	{ "__float80",				true,  true,  },
	{ "__float128",				true,  true,  },
	{ "_Float128",				true,  true,  },
	{ "_Float128 _Complex",		true,  false, },
//	{ "_Float128x",				true,  true,  },		// add declarations if type supported
//	{ "_Float128x _Complex",	true,  false, },
#endif
};

struct {
	const string name;
	bool assignment = false;
	bool floatCompat = true;
	bool isComparison = false;
	bool isEqual = false;
} arithmeticOperators[] = {
	{ "?++",	true,  true,  false, false },
	{ "?--",	true,  true,  false, false },
	{ "++?",	true,  true,  false, false },
	{ "--?",	true,  true,  false, false },
	{ "+?",		false, true,  false, false },
	{ "-?",		false, true,  false, false },
	{ "~?",		false, false, false, false },
	{ "!?",		false, true,  false, true  },
	{ "?*?",	false, true,  false, false },
	{ "?/?",	false, true,  false, false },
	{ "?%?",	false, false, false, false },
	{ "?+?",	false, true,  false, false },
	{ "?-?",	false, true,  false, false },
	{ "?<<?",	false, false, false, false },
	{ "?>>?",	false, false, false, false },
	{ "?<?",	false, true,  true,  false },
	{ "?<=?",	false, true,  true,  true  },
	{ "?>?",	false, true,  true,  false },
	{ "?>=?",	false, true,  true,  true  },
	{ "?==?",	false, true,  false, true  },
	{ "?!=?",	false, true,  false, true  },
	{ "?&?",	false, false, false, false },
	{ "?^?",	false, false, false, false },
	{ "?|?",	false, false, false, false },
	{ "?=?",	true,  true,  false, false },
	{ "?+=?",	true,  true,  false, false },
	{ "?-=?",	true,  true,  false, false },
	{ "?*=?",	true,  true,  false, false },
	{ "?/=?",	true,  true,  false, false },
	{ "?%=?",	true,  false, false, false },
	{ "?<<=?",	true,  false, false, false },
	{ "?>>=?",	true,  false, false, false },
	{ "?&=?",	true,  false, false, false },
	{ "?|=?",	true,  false, false, false },
	{ "?^=?",	true,  false, false, false },
};

enum ArgType { Normal, PtrDiff, CommPtrDiff };

struct {
	const string name;
	bool assignment = false;
	string diffReturn;
	ArgType diffArg2 = Normal;
	string sized;
} pointerOperators[] = {
	{ "?++",	true,  "",				Normal,			" | sized(DT)" },
	{ "?--",	true,  "",				Normal,			" | sized(DT)" },
	{ "++?",	true,  "",				Normal,			" | sized(DT)" },
	{ "--?",	true,  "",				Normal,			" | sized(DT)" },
	{ "!?",		false, "int",			Normal,			"" },
	{ "?<?",	false, "signed int",	Normal,			"" },
	{ "?<=?",	false, "signed int",	Normal,			"" },
	{ "?>?",	false, "signed int",	Normal,			"" },
	{ "?>=?",	false, "signed int",	Normal,			"" },
	{ "?==?",	false, "signed int",	Normal,			"" },
	{ "?!=?",	false, "signed int",	Normal,			"" },
	{ "?=?",	true,  "",				Normal,			"" }, // void * LHS, zero_t RHS ???
//	{ "*?",		false, "&",				Normal,			" | sized(DT)" }, // & ???
	{ "*?",		false, "&",				Normal,			"" }, // & ???

	{ "?-?",	false, "ptrdiff_t",		Normal,			" | sized(DT)" },
	{ "?-?",	false, "",				PtrDiff,		" | sized(DT)" },
	{ "?-=?",	true,  "",				PtrDiff,		" | sized(DT)" },

	{ "?+?",	false, "",				CommPtrDiff,	" | sized(DT)" },
	{ "?[?]",	false, "&",				CommPtrDiff,	" | sized(DT)" }, // & ???
	{ "?+=?",	true,  "",				PtrDiff,		" | sized(DT)" },
};

template<size_t N>
string mask2string(unsigned int mask, array<string, N> names) {
	string result = "";
	int i = 0;
	for(auto name : names) {
		if(mask & (1 << i)) {
			result += name;
		} else {
			result.append(name.size(), ' ');
		}
		i++;
	}
	return result;
}

template <typename... T>
constexpr auto make_array(T&&... values) ->
    std::array<
        typename std::decay<typename std::common_type<T...>::type>::type,
        sizeof...(T)>
{
    return std::array<
        typename std::decay<
            typename std::common_type<T...>::type>::type,
        sizeof...(T)>{{std::forward<T>(values)...}};
}

int main() {
	cout << "# 2 \"prelude.cfa\"  // needed for error messages from this file" << endl;
	cout << "forall( T * ) trait sized {};" << endl;

	cout << "//////////////////////////" << endl;
	cout << "// Arithmetic Operators //" << endl;
	cout << "//////////////////////////" << endl;
	cout << endl;

	cout << "signed int ?==?( zero_t, zero_t ),	?!=?( zero_t, zero_t );" << endl;
	cout << "signed int ?==?( one_t, one_t ),	?!=?( one_t, one_t );" << endl;
	cout << "signed int ?==?( _Bool, _Bool ),	?!=?( _Bool, _Bool );" << endl;
	cout << "signed int !?( _Bool );" << endl;

	for (auto op : arithmeticOperators) {
		for (auto type : basicTypes ) {
			auto operands = count(op.name.begin(), op.name.end(), '?');
			if (! op.floatCompat && type.isFloat) continue;
			if (op.isComparison && ! type.hasComparison) continue;
			if (op.assignment) {
				const char * qualifiers[] = { "", "volatile " };
				for (auto q : qualifiers){
					cout << type.name << " " << op.name << "(";
					cout << q << type.name << " &";
					for (int i = 1; i < operands; ++i) {
						cout << ", " << type.name;
					}
					cout << ");" << endl;
				}
			} else {
				if (op.isComparison || op.isEqual) cout << "signed int";
				else cout << type.name;
				cout << " " << op.name << "(";
				for (int i = 0; i < operands; ++i) {
					cout << type.name;
					if ((i+1) != operands) cout << ", ";
				}
				cout << ");" << endl;
			}
		}
		cout << endl;
	}
	cout << endl;

	cout << "/////////////////////////////" << endl;
	cout << "// Arithmetic Constructors //" << endl;
	cout << "/////////////////////////////" << endl;
	cout << endl;

	auto otype = [](const std::string & type, bool do_volatile = false) {
		cout << "void ?{} (" << type << " &);" << endl;
		cout << "void ?{} (" << type << " &, " << type << ");" << endl;
		cout << type << " ?=? (" << type << " &, " << type << ")";
		if ( do_volatile ) {
			cout << ", ?=?(volatile " << type << " &, " << type << ")";
		}
		cout << ";" << endl;
		cout << "void ^?{}( " << type << " & );" << endl;
	};

	otype("zero_t");
	cout << endl;
	otype("one_t");
	cout << endl;
	otype("_Bool", true);
	cout << endl;

	for (auto type : basicTypes) {
		cout << "void ?{}(" << type.name << " &);" << endl;
		cout << "void ?{}(" << type.name << " &, " << type.name << ");" << endl;
		cout << "void ?{}(" << type.name << " &, zero_t);" << endl;
		cout << "void ?{}(" << type.name << " &, one_t);" << endl;
		cout << "void ^?{}(" << type.name << " &);" << endl;
		cout << endl;
	}
	cout << endl;

	cout << "//////////////////////////" << endl;
	cout << "// Pointer Constructors //" << endl;
	cout << "//////////////////////////" << endl;
	cout << endl;

	cout << "forall(ftype FT) void ?{}( FT *&, FT * );" << endl;
	cout << "forall(ftype FT) void ?{}( FT * volatile &, FT * );" << endl;

	// generate qualifiers
	vector<string> qualifiersSingle;
	vector<pair<const string, const string>> qualifiersPair;
	const unsigned int NQ = 2;
	for(unsigned int lhs = 0; lhs < (1<<NQ); lhs++) {
		// for parameter of default constructor and destructor
		qualifiersSingle.push_back(mask2string(lhs, make_array("const "s, "volatile "s)));

		// for first and second parameters of copy constructors
		for(unsigned int rhs = 0; rhs < (1<<NQ); rhs++) {
			if((lhs & rhs) == rhs) {
				qualifiersPair.push_back({
					mask2string(lhs, make_array("const "s, "volatile "s)),
					mask2string(rhs, make_array("const "s, "volatile "s))
				});
			}
		}
	}

	for (auto type : { "  DT", "void" }) {
		for (auto cvq : qualifiersPair) {
			for (auto is_vol : { "        ", "volatile" }) {
				cout << "forall(DT &) void  ?{}(" << cvq.first << type << " * " << is_vol << " &, " << cvq.second << "DT *);" << endl;
			}
		}
	}
	for (auto cvq : qualifiersSingle) {
		for (auto is_vol : { "        ", "volatile" }) {
			cout << "void  ?{}(" << cvq << "void" << " * " << is_vol << " &);" << endl;
		}
		for (auto is_vol : { "        ", "volatile" }) {
			cout << "void ^?{}(" << cvq << "void" << " * " << is_vol << " &);" << endl;
		}
	}

	for (auto cvq : qualifiersSingle) {
		for (auto is_vol : { "        ", "volatile" }) {
			cout << "forall(DT &) void  ?{}(" << cvq << "  DT" << " * " << is_vol << " &);" << endl;
		}
		for (auto is_vol : { "        ", "volatile" }) {
			cout << "forall(DT &) void ^?{}(" << cvq << "  DT" << " * " << is_vol << " &);" << endl;
		}
	}

	{
		auto type = "  DT";
		for (auto is_vol : { "        ", "volatile" }) {
			for (auto cvq : qualifiersSingle) {
				cout << "forall(DT &) void ?{}( " << cvq << type << " * " << is_vol << " &, zero_t);" << endl;
			}
		}
	}

	cout << endl;

	cout << "forall(ftype FT) void	?{}( FT *	   &, zero_t );" << endl;
	cout << "forall(ftype FT) FT *			?=?( FT *	   &, zero_t );" << endl;
	cout << "forall(ftype FT) FT *			?=?( FT * volatile &, zero_t );" << endl;
	cout << "forall(ftype FT) void	?{}( FT *	   & );" << endl;
	cout << "forall(ftype FT) void	^?{}( FT *	   & );" << endl;
	cout << endl;

	cout << "///////////////////////" << endl;
	cout << "// Pointer Operators //" << endl;
	cout << "///////////////////////" << endl;

	cout << "forall(ftype FT) FT *			?=?( FT *&, FT * );" << endl;
	cout << "forall(ftype FT) FT *			?=?( FT * volatile &, FT * );" << endl;
	cout << "forall(ftype FT) int !?( FT * );" << endl;
	cout << "forall(ftype FT) signed int ?==?( FT *, FT * );" << endl;
	cout << "forall(ftype FT) signed int ?!=?( FT *, FT * );" << endl;
	cout << "forall(ftype FT) FT &		 *?( FT * );" << endl;

	for (auto op : pointerOperators) {
		auto forall = [&op]() {
			cout << "forall(DT &" << op.sized << ") ";
		};
		for (auto type : { "DT"/*, "void"*/ } ) {
			auto operands = count(op.name.begin(), op.name.end(), '?');
			if (op.assignment) {
				// const char * qualifiers[] = { "", "volatile ", "const ", "const volatile " };
				switch(op.diffArg2) {
					case Normal:
						if (operands == 1) {
							for (auto q : qualifiersSingle){
								for (auto q2 : { "        ", "volatile" }) {
									forall();
									cout << q << type << " * " << op.name << "(";
									cout << q << type << " * " << q2 << " &";
									cout << ");" << endl;
								}
							}
						} else {
							for (auto q : qualifiersPair){
								for (auto q2 : { "        ", "volatile" }) {
									forall();
									cout << q.first << type << " * " << op.name << "(";
									cout << q.first << type << " * " << q2 << " &";

									for (int i = 1; i < operands; ++i) {
										cout << ", " << q.second << type << " *";
									}
									cout << ");" << endl;
								}
							}
						}
						break;
					case PtrDiff:
						for (auto q : qualifiersSingle){
							for (auto q2 : { "        ", "volatile" }) {
								forall();
								cout << q << type << " * " << op.name << "(";
								cout << q << type << " * " << q2 << " &";

								for (int i = 1; i < operands; ++i) {
									cout << ", ptrdiff_t";
								}
								cout << ");" << endl;
							}
						}
						break;
					default:
						abort();
					}
			} else {
				auto name_and_arg1 = [&op, &type](const std::string & q) {
					if (op.diffReturn == "&") cout << q << type << " &"; // -- qualifiers
					else if (op.diffReturn != "") cout << op.diffReturn;
					else cout << q << type << " *";
					cout << " " << op.name << "(";
				};
				switch(op.diffArg2) {
					case Normal:
						for (auto q : qualifiersSingle) {
							forall();
							name_and_arg1( q );
							for (int i = 0; i < operands; ++i) {
								cout << q << type << " *";
								if ((i+1) != operands) cout << ", ";
							}
							cout << ");" << endl;
						}
						break;
					case CommPtrDiff:
						for (auto q : qualifiersSingle) {
							forall();
							name_and_arg1( q );
							cout << "ptrdiff_t, " << q << type << " *);" << endl;
						}
						// fallthrough
					case PtrDiff:
						for (auto q : qualifiersSingle) {
							forall();
							name_and_arg1( q );
							cout << q << type << " *, ptrdiff_t);" << endl;
						}
						break;
				}
			}
		}
		cout << endl;
	}
	cout << endl;

	for (auto is_vol : { "        ", "volatile" }) {
		for (auto cvq : qualifiersPair) {
				cout << "forall(DT &) " << cvq.first << "void * ?=?( " << cvq.first << "void * " << is_vol << " &, " << cvq.second << "DT *);" << endl;
		}
		for (auto cvq : qualifiersSingle) {
			cout << "forall(DT &) " << cvq <<   "  DT * ?=?( " << cvq << "  DT * " << is_vol << " &, zero_t);" << endl;
		}
	}
	cout << endl;
}

// Local Variables: //
// tab-width: 4 //
// End: //
