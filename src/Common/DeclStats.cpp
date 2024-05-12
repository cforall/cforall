//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclStats.cpp -- Print statistics about a translation unit's declarations.
//
// Author           : Andrew Beach
// Created On       : Fri Oct  1 14:26:00 2021
// Last Modified By : Andrew Beach
// Last Modified On : Wed Oct  8 11:24:00 2021
// Update Count     : 0
//

#include "DeclStats.hpp"

#include "AST/LinkageSpec.hpp"
#include "AST/Pass.hpp"
#include "AST/Print.hpp"
#include "Common/VectorMap.hpp"

#include <iostream>
#include <map>
#include <sstream>
#include <unordered_map>
#include <unordered_set>

// Everything but printDeclStats at the bottom is hidden.
namespace {

template<typename T>
void sum( T & l, const T & r ) { l += r; }

void sum( VectorMap<unsigned> & l, const VectorMap<unsigned> & r ) {
	l.reserve( r.size() );
	for ( unsigned i = 0 ; i < r.size() ; ++i ) {
		l[i] += r[i];
	}
}

template<typename KeyT>
void sum( std::map<KeyT, unsigned> & l, const std::map<KeyT, unsigned> & r ) {
	for ( const auto & entry : r ) {
		l[ entry.first ] += entry.second;
	}
}

template<typename KeyT>
void sum( std::unordered_map<KeyT, unsigned> & l,
		const std::unordered_map<KeyT, unsigned> & r ) {
	for ( const auto & entry : r ) {
		l[ entry.first ] += entry.second;
	}
}

/// Stores statistics on a single group of arguments or return values.
struct ArgPackStats {
	/// Count of decls with each number of elements.
	VectorMap<unsigned> n;
	/// Count of decls with each number of basic type elements.
	VectorMap<unsigned> n_basic;
	/// Count of decls with each number of generic type elements.
	VectorMap<unsigned> n_generic;
	/// Count of decls with each number of polymorphic elements.
	VectorMap<unsigned> n_poly;
	/// Count of decls with each number of non-generic compound types.
	VectorMap<unsigned> n_compound;
	/// Count of decls with each percentage of basic type elements.
	std::map<unsigned, unsigned> p_basic;
	/// Count of decls with each percentage of generic type elements.
	std::map<unsigned, unsigned> p_generic;
	/// Count of decls with each percentage of polymorphic elements.
	std::map<unsigned, unsigned> p_poly;
	/// Count of decls with each percentage of non-generic compound type elements.
	std::map<unsigned, unsigned> p_compound;
	/// Count of decls with each number of distinct types in the pack.
	VectorMap<unsigned> n_types;
	/// Count of decls with each percentage of new types in lists.
	/// Types used in the parameter list that recur in the return list are not considered to be new.
	std::map<unsigned, unsigned> p_new;

	ArgPackStats& operator+=( const ArgPackStats& other ) {
		sum(n, other.n);
		sum(n_basic, other.n_basic);
		sum(n_generic, other.n_generic);
		sum(n_poly, other.n_poly);
		sum(n_compound, other.n_compound);
		sum(p_basic, other.p_basic);
		sum(p_generic, other.p_generic);
		sum(p_poly, other.p_poly);
		sum(p_compound, other.p_compound);
		sum(n_types, other.n_types);
		sum(p_new, other.p_new);

		return *this;
	}
};

/// Collected statistics on a group of declarations.
struct Stats {
	/// Total number of declarations in these statistics.
	unsigned n_decls;
	/// Count of declarations with each number of assertion parameters.
	VectorMap<unsigned> n_type_params;
	/// Count of generic types with each number of type parameters.
	VectorMap<unsigned> n_generic_params;
	/// Count of maximum nesting depth of types.
	VectorMap<unsigned> n_generic_nesting;
	/// Count of declarations with each name.
	std::unordered_map<std::string, unsigned> by_name;
	/// Count of uses of each basic type.
	std::unordered_map<std::string, unsigned> basic_type_names;
	/// Count of uses of each generic type name (includes "*", "[]", "(*)", "[,]").
	std::unordered_map<std::string, unsigned> generic_type_names;
	/// Count of uses of each non-generic aggregate type.
	std::unordered_map<std::string, unsigned> compound_type_names;
	/// Count of decls using each basic type.
	std::unordered_map<std::string, unsigned> basic_type_decls;
	/// Count of decls using each generic type (includes "*", "[]", "(*)", "[,]").
	std::unordered_map<std::string, unsigned> generic_type_decls;
	/// Count of decls using each compound type.
	std::unordered_map<std::string, unsigned> compound_type_decls;
	/// Stats for the parameter lists.
	ArgPackStats params;
	/// Stats for the return lists.
	ArgPackStats returns;

	/// Count of declarations with each number of assertions.
	std::map<unsigned, unsigned> n_assns;
	/// Stats for the assertions' parameters.
	ArgPackStats assn_params;
	/// Stats for the assertions' return types.
	ArgPackStats assn_returns;

	Stats& operator+=( const Stats& other ) {
		sum( n_decls, other.n_decls );
		sum( n_type_params, other.n_type_params );
		sum( n_generic_params, other.n_generic_params );
		sum( n_generic_nesting, other.n_generic_nesting );
		sum( by_name, other.by_name );
		sum( basic_type_names, other.basic_type_names );
		sum( generic_type_names, other.generic_type_names );
		sum( compound_type_names, other.compound_type_names );
		sum( basic_type_decls, other.basic_type_decls );
		sum( generic_type_decls, other.generic_type_decls );
		sum( compound_type_decls, other.compound_type_decls );
		sum( params, other.params );
		sum( returns, other.returns );
		sum( n_assns, other.n_assns );
		sum( assn_params, other.assn_params );
		sum( assn_returns, other.assn_returns );

		return *this;
	}

};

void update_max( unsigned & max, unsigned value ) {
	if ( max < value ) max = value;
}

// Where all unnamed specs are counted as one named spec group.
constexpr unsigned num_named_specs = 8;

unsigned linkage_index( ast::Linkage::Spec spec ) {
	switch ( spec.val ) {
	case ast::Linkage::Intrinsic.val:  return 0;
	case ast::Linkage::C.val:          return 1;
	case ast::Linkage::Cforall.val:    return 2;
	case ast::Linkage::AutoGen.val:    return 3;
	case ast::Linkage::Compiler.val:   return 4;
	case ast::Linkage::BuiltinCFA.val: return 5;
	case ast::Linkage::BuiltinC.val:   return 6;
	default:                           return 7;
	}
}

struct DeclStats : public ast::WithShortCircuiting {
	/// Stores separate stats per linkage.
	Stats by_linkage[num_named_specs];
	/// Stores manglenames already seen to avoid double-counting.
	std::unordered_set<std::string> seen_names;
	/// Overall stats.
	Stats total;
	/// Count of expressions with (depth, fanout)
	std::map<std::pair<unsigned, unsigned>, unsigned> exprs_by_fanout_at_depth;

	/// Count that we have seen a named type.
	void countType(
			const std::string & name, unsigned & n,
			std::unordered_map<std::string, unsigned> & names,
			std::unordered_map<std::string, unsigned> & decls,
			std::unordered_set<std::string> & elSeen ) {
		++n;
		++names[ name ];
		if ( elSeen.insert( name ).second ) {
			++decls[ name ];
		}
	}

	/// Perform type analysis on a subtype.
	void analyzeSubtype( const ast::Type * type, Stats & stats,
			std::unordered_set<std::string> & elSeen, unsigned & n_poly,
			bool & seen_poly, unsigned & max_depth, unsigned depth ) {
		// This kind of gets in the way of grouping arguments.
		unsigned ignored = 0;
		analyzeType(
			type, stats, elSeen, ignored, ignored, n_poly, ignored,
			seen_poly, max_depth, depth + 1 );
	}

	/// Perform type analysis on each subtype.
	void analyzeSubtypes(
			const std::vector<ast::ptr<ast::Type>> & types, Stats & stats,
			std::unordered_set<std::string> & elSeen, unsigned & n_poly,
			bool & seen_poly, unsigned & max_depth, unsigned depth ) {
		for ( const auto & type : types ) {
			analyzeSubtype( type, stats, elSeen, n_poly, seen_poly, max_depth, depth );
		}
	}

	/// Perform sub-type analysis on each subtype in an argument pack.
	void analyzeSubPack(
			const std::vector<ast::ptr<ast::Type>> & types, Stats & stats,
			std::unordered_set<std::string> & elSeen, unsigned & n_poly,
			bool & seen_poly, unsigned & max_depth, unsigned depth,
			unsigned & n_subs ) {
		// ... and count voids?
		for ( const auto & type : types ) {
			if ( type.as<ast::VoidType>() ) {
				++n_subs;
			}
			// Could this be in `else`?
			analyzeSubtype( type, stats, elSeen, n_poly, seen_poly, max_depth, depth );
		}
	}

	/// Analyze and gather stats from a single type.
	void analyzeType( const ast::ptr<ast::Type> & type, Stats & stats,
			std::unordered_set<std::string> & elSeen,
			unsigned & n_basic, unsigned & n_generic, unsigned & n_poly,
			unsigned & n_agg, bool & seen_poly,
			unsigned & max_depth, unsigned depth ) {
		// Almost a visit, except it is only types.
		if ( const ast::BasicType * t = type.as<ast::BasicType>() ) {
			const std::string name = ast::BasicType::typeNames[ t->kind ];
			countType( name, n_basic, stats.basic_type_names, stats.basic_type_decls, elSeen );
			update_max( max_depth, depth );
		} else if ( auto t = type.as<ast::PointerType>() ) {
			static const std::string name = "*";
			countType( name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen );
			analyzeSubtype( t->base, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			++stats.n_generic_params.at( 1 );
		} else if ( auto t = type.as<ast::ArrayType>() ) {
			static const std::string name = "[]";
			countType( name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen );
			analyzeSubtype( t->base, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			++stats.n_generic_params.at( 1 );
		} else if ( auto t = type.as<ast::ReferenceType>() ) {
			static const std::string name = "&";
			countType( name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen );
			analyzeSubtype( t->base, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			++stats.n_generic_params.at( 1 );
		} else if ( auto t = type.as<ast::FunctionType>() ) {
			static const std::string name = "(*)";
			countType( name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen );
			unsigned n_subs = 0;
			analyzeSubPack( t->returns, stats, elSeen, n_poly, seen_poly, max_depth, depth, n_subs );
			analyzeSubPack( t->params, stats, elSeen, n_poly, seen_poly, max_depth, depth, n_subs );
			++stats.n_generic_params.at( n_subs );
		} else if ( auto t = type.as<ast::TypeInstType>() ) {
			if ( !seen_poly ) {
				++n_poly;
				seen_poly = true;
			}
			countType( t->name, n_agg, stats.compound_type_names,
					stats.compound_type_decls, elSeen );
			update_max( max_depth, depth );
		} else if ( auto t = type.as<ast::BaseInstType>() ) {
			auto & params = t->params;
			if ( params.empty() ) {
				countType( t->name, n_agg, stats.compound_type_names,
						stats.compound_type_decls, elSeen );
				update_max( max_depth, depth );
			} else {
				countType( t->name, n_generic, stats.generic_type_names,
						stats.generic_type_decls, elSeen );
				++stats.n_generic_params.at( params.size() );
			}
		} else if ( auto t = type.as<ast::TupleType>() ) {
			static const std::string name = "[,]";
			countType( name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen);
			analyzeSubtypes( t->types, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			++stats.n_generic_params.at( t->size() );
		} else if ( type.as<ast::VarArgsType>() ) {
			static const std::string name = "...";
			countType( name, n_agg, stats.compound_type_names, stats.compound_type_decls, elSeen );
			update_max( max_depth, depth );
		} else if ( type.as<ast::ZeroType>() ) {
			static const std::string name = "0";
			countType( name, n_basic, stats.basic_type_names, stats.basic_type_decls, elSeen );
			update_max( max_depth, depth );
		} else if ( type.as<ast::OneType>() ) {
			static const std::string name = "1";
			countType( name, n_basic, stats.basic_type_names, stats.basic_type_decls, elSeen );
			update_max( max_depth, depth );
		}
	}

	/// Update an ArgPackStats based on the list of types it repersents.
	void analyzeArgPack(
			const std::vector<ast::ptr<ast::Type>> & types,
			Stats & stats,
			ArgPackStats & packStats,
			// What are these two used for?
			std::unordered_set<std::string> & seen,
			std::unordered_set<std::string> & elSeen ) {
		std::unordered_set<std::string> type_names;
		unsigned n = 0;
		unsigned n_basic = 0;
		unsigned n_generic = 0;
		unsigned n_poly = 0;
		unsigned n_compound = 0;
		unsigned n_new = 0;

		for ( auto & type : types ) {
			n += type->size();

			std::stringstream ss;
			ast::print( ss, type );
			type_names.insert( ss.str() );
			if ( seen.insert( ss.str() ).second ) {
				++n_new;
			}

			bool seen_poly = false;
			unsigned max_depth = 0;
			analyzeType(
				type, stats, elSeen, n_basic, n_generic, n_poly, n_compound,
				seen_poly, max_depth, 0
			);
			++stats.n_generic_nesting.at( max_depth );
		}

		++packStats.n.at( n );
		++packStats.n_basic.at( n_basic );
		++packStats.n_generic.at( n_generic );
		++packStats.n_poly.at( n_poly );
		++packStats.n_compound.at( n_compound );
		if ( n > 0 ) {
			++packStats.p_basic[ n_basic * 100 / n ];
			++packStats.p_generic[ n_generic * 100 / n ];
			++packStats.p_poly[ n_poly * 100 / n ];
			++packStats.p_compound[ n_compound * 100 / n ];
			if ( n > 1 ) {
				++packStats.p_new[ (n_new - 1) * 100 / (n - 1) ];
			}
		}
		++packStats.n_types.at( types.size() );
	}

	/// Perform type analysis on a function type, storing information in the
	/// given ArgPackStats.
	void analyzeFunctionType( const ast::FunctionType * type, Stats& stats,
			ArgPackStats Stats::* param_pack,
			ArgPackStats Stats::* return_pack ) {
		// I still don't know what these are for.
		std::unordered_set<std::string> seen;
		std::unordered_set<std::string> elSeen;
		analyzeArgPack( type->params, stats, stats.*param_pack, seen, elSeen );
		analyzeArgPack( type->returns, stats, stats.*return_pack, seen, elSeen );
	}

	/// If the assertion is a function, return the function type.
	static const ast::FunctionType * getAssertionFunctionType(
			const ast::ptr<ast::DeclWithType> & assertion ) {
		if ( auto * assertionObject = assertion.as<ast::ObjectDecl>() ) {
			if ( auto * ptrTy = assertionObject->type.as<ast::PointerType>() ) {
				return ptrTy->base.as<ast::FunctionType>();
			} else {
				return assertionObject->type.as<ast::FunctionType>();
			}
		} else if ( auto * assertionDecl = assertion.as<ast::FunctionDecl>() ) {
			return assertionDecl->type;
		}
		return nullptr;
	}

	void analyzeFunctionDecl( const ast::FunctionDecl * decl ) {
		Stats & stats = by_linkage[ linkage_index( decl->linkage ) ];

		++stats.n_decls;
		const ast::FunctionType * type = decl->type.get();
		const ast::FunctionType::ForallList & forall = type->forall;
		++stats.n_type_params.at( forall.size() );
		unsigned num_assertions = 0;
		for ( const ast::ptr<ast::TypeInstType> & instType : forall ) {
			num_assertions += instType->base->assertions.size();
			for ( const auto & assertion : instType->base->assertions ) {
				if ( auto assertionType = getAssertionFunctionType( assertion ) ) {
					analyzeFunctionType( assertionType, stats,
							&Stats::assn_params, &Stats::assn_returns );
				}
			}
		}
		++stats.n_assns[ num_assertions ];
		++stats.by_name[ decl->name ];
		analyzeFunctionType( type, stats, &Stats::params, &Stats::returns );
	}

	void analyzeUntypedExpr( const ast::UntypedExpr * expr, unsigned depth ) {
		unsigned fanout = expr->args.size();
		++exprs_by_fanout_at_depth[ std::make_pair( depth, fanout ) ];

		for ( const ast::ptr<ast::Expr> & arg : expr->args ) {
			if ( const auto * untyped = arg.as<ast::UntypedExpr>() ) {
				analyzeUntypedExpr( untyped, depth + 1 );
			}
		}
	}

public:
	void previsit( const ast::UntypedExpr * expr ) {
		visit_children = false;
		analyzeUntypedExpr( expr, 0 );
	}

	void previsit( const ast::FunctionDecl * decl ) {
		const std::string & mangleName = decl->mangleName;
		const std::string & indexName = mangleName.empty() ? decl->name : mangleName;
		if ( seen_names.insert( indexName ).second ) {
			analyzeFunctionDecl( decl );
		}
	}

private:

	// Trying to avoid duplication by templates.
	// I couldn't do it in all cases.
	template<typename T, typename U>
	using getter = std::function<U(T const &)>;

	/// Print a single role, for every linkage and the totals.
	void printRow( const std::string & name, getter<Stats, unsigned> extract ) {
		std::cout << "\"" << name << "\",";
		for ( const Stats & stats : by_linkage ) {
			std::cout << "," << extract( stats );
		}
		std::cout << "," << extract( total ) << std::endl;
	}

	/// Print every row in a group of maps.
	template<typename Func>
	void printAllMap( const std::string & name, Func && extract ) {
		// Get all rows from the total stats.
		for ( auto & entry : extract( total ) ) {
			auto & key = entry.first;
			std::cout << "\"" << name << "\"," << key;
			for ( const auto & stats : by_linkage ) {
				const auto & map = extract( stats );
				auto it = map.find( key );
				if ( map.end() == it ) {
					std::cout << ",0";
				} else {
					std::cout << "," << it->second;
				}
			}
			std::cout << "," << entry.second << std::endl;
		}
	}

	/// Accumalate information, then print every row in the remaining maps.
	template<typename Func>
	void printAllSparseHisto( const std::string & name, Func && extract ) {
		std::map<unsigned, unsigned> histos[num_named_specs];
		std::map<unsigned, unsigned> histo_total;

		// Collect all data into the histograms.
		for ( const auto & entry : extract( total ) ) {
			++histo_total[ entry.second ];
		}

		for ( unsigned i = 0 ; i < num_named_specs ; ++i ) {
			for ( const auto & entry : extract( by_linkage[i] ) ) {
				++histos[ i ][ entry.second ];
			}
		}

		// Get all rows from the total stats.
		for ( const auto & entry : histo_total ) {
			const unsigned & key = entry.first;
			std::cout << "\"" << name << "\"," << key;
			for ( unsigned i = 0 ; i < num_named_specs ; ++i ) {
				auto it = histos[i].find( key );
				if ( histos[i].end() == it ) {
					std::cout << ",0";
				} else {
					std::cout << "," << it->second;
				}
			}
			std::cout << "," << entry.second << std::endl;
		}
	}

	void printAllPack( const std::string & name, ArgPackStats Stats::* field ) {
		printAllMap("n_basic_" + name, [&field](const Stats& stats) { return (stats.*field).n_basic; });
		printAllMap("n_generic_" + name, [&field](const Stats& stats) { return (stats.*field).n_generic; });
		printAllMap("n_poly_" + name, [&field](const Stats& stats) { return (stats.*field).n_poly; });
		printAllMap("n_compound_" + name, [&field](const Stats& stats) { return (stats.*field).n_compound; });
		printAllMap("n_" + name, [&field](const Stats& stats) { return (stats.*field).n; });
		printAllMap("%_basic_" + name, [&field](const Stats& stats) { return (stats.*field).p_basic; });
		printAllMap("%_generic_" + name, [&field](const Stats& stats) { return (stats.*field).p_generic; });
		printAllMap("%_poly_" + name, [&field](const Stats& stats) { return (stats.*field).p_poly; });
		printAllMap("%_compound_" + name, [&field](const Stats& stats) { return (stats.*field).p_compound; });
		printAllMap("n_distinct_types_" + name, [&field](const Stats& stats) { return (stats.*field).n_types; });
		printAllMap("%_new_types_in_" + name, [&field](const Stats& stats) { return (stats.*field).p_new; });
	}

	static void printPairMap (
			const std::string & name,
			const std::map<std::pair<unsigned, unsigned>, unsigned> & map ) {
		for ( const auto & entry : map ) {
			const auto & key = entry.first;
			std::cout << "\"" << name << "\"," << key.first << ','
				<< key.second << ',' << entry.second << std::endl;
		}
	}

public:
	void print() {
		for ( auto & stats : by_linkage ) {
			total += stats;
		}

		std::cout << ",,\"intrinsic\",\"Cforall\",\"C\",\"autogen\",\"compiler\",\"builtinCFA\",\"builtinC\",\"other\",\"TOTAL\"" << std::endl;

		printAllMap("n_type_params", [](const Stats& stats) { return stats.n_type_params; });
		printAllMap("n_generic_params", [](const Stats& stats) { return stats.n_generic_params; });
		printAllMap("n_generic_nesting", [](const Stats& stats) { return stats.n_generic_nesting; });
		printRow("n_decls", [](const Stats& stats) { return stats.n_decls; });
		printRow("unique_names", [](const Stats& stats) { return stats.by_name.size(); });
		printAllSparseHisto("overloads", [](const Stats& stats) { return stats.by_name; });
		printRow("basic_type_names", [](const Stats& stats) { return stats.basic_type_names.size(); });
		printAllSparseHisto("basic_type_uses", [](const Stats& stats) { return stats.basic_type_names; });
		printAllSparseHisto("decls_using_basic_type", [](const Stats& stats) { return stats.basic_type_decls; });
		printRow("generic_type_names", [](const Stats& stats) { return stats.generic_type_names.size(); });
		printAllSparseHisto("generic_type_uses", [](const Stats& stats) { return stats.generic_type_names; });
		printAllSparseHisto("decls_using_generic_type", [](const Stats& stats) { return stats.generic_type_decls; });
		printRow("compound_type_names", [](const Stats& stats) { return stats.compound_type_names.size(); });
		printAllSparseHisto("compound_type_uses", [](const Stats& stats) { return stats.compound_type_names; });
		printAllSparseHisto("decls_using_compound_type", [](const Stats& stats) { return stats.compound_type_decls; });
		printAllPack("params", &Stats::params);
		printAllPack("returns", &Stats::returns);
		printAllMap("n_assns", [](const Stats& stats) { return stats.n_assns; });
		printAllPack("assn_params", &Stats::assn_params);
		printAllPack("assn_returns", &Stats::assn_returns);
		std::cout << std::endl;

		printPairMap( "exprs by depth+fanout", exprs_by_fanout_at_depth );
	}
};

} // namespace

void printDeclStats( ast::TranslationUnit & translationUnit ) {
	ast::Pass<DeclStats> stats;
	accept_all( translationUnit, stats );
	stats.core.print();
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
