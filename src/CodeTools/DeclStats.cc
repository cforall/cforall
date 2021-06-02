//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// DeclStats.cc --
//
// Author           : Aaron Moss
// Created On       : Wed Jan 31 16:40:00 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Dec 13 23:39:33 2019
// Update Count     : 2
//

#include "DeclStats.h"

#include <iostream>                // for operator<<, basic_ostream, cout
#include <map>                     // for map
#include <string>                  // for string, operator+, operator<<, cha...
#include <unordered_map>           // for unordered_map
#include <unordered_set>           // for unordered_set
#include <utility>                 // for pair, make_pair

#include "Common/PassVisitor.h"
#include "Common/VectorMap.h"      // for VectorMap
#include "GenPoly/GenPoly.h"       // for hasPolyBase
#include "SynTree/LinkageSpec.h"   // for ::NoOfSpecs, Spec
#include "SynTree/Declaration.h"   // for FunctionDecl, TypeDecl, Declaration
#include "SynTree/Expression.h"    // for UntypedExpr, Expression
#include "SynTree/Statement.h"     // for CompoundStmt
#include "SynTree/Type.h"          // for Type, FunctionType, PointerType
#include "SynTree/Visitor.h"       // for maybeAccept, Visitor, acceptAll

namespace CodeTools {

	struct DeclStats : public WithShortCircuiting {
		template<typename T>
		static void sum(T& a, const T& b) { a += b; }

		static void sum(VectorMap<unsigned>& a, const VectorMap<unsigned>& b) {
			a.reserve( b.size() );
			for ( unsigned i = 0; i < b.size(); ++i ) {
				a[i] += b[i];
			}
		}

		template<typename K>
		static void sum(std::map<K, unsigned>& a, const std::map<K, unsigned>& b) {
			for ( const auto& entry : b ) {
				a[ entry.first ] += entry.second;
			}
		}

		template<typename K>
		static void sum(std::unordered_map<K, unsigned>& a, const std::unordered_map<K, unsigned>& b) {
			for ( const auto& entry : b ) {
				a[ entry.first ] += entry.second;
			}
		}

		struct ArgPackStats {
			VectorMap<unsigned> n;                   ///< Count of decls with each number of elements
			VectorMap<unsigned> n_basic;             ///< Count of decls with each number of basic type elements
			VectorMap<unsigned> n_generic;           ///< Count of decls with each number of generic type elements
			VectorMap<unsigned> n_poly;              ///< Count of decls with each number of polymorphic elements
			VectorMap<unsigned> n_compound;          ///< Count of decls with each number of non-generic compound types
			std::map<unsigned, unsigned> p_basic;    ///< Count of decls with each percentage of basic type elements
			std::map<unsigned, unsigned> p_generic;  ///< Count of decls with each percentage of generic type elements
			std::map<unsigned, unsigned> p_poly;     ///< Count of decls with each percentage of polymorphic elements
			std::map<unsigned, unsigned> p_compound; ///< Count of decls with each percentage of non-generic compound type elements
			VectorMap<unsigned> n_types;             ///< Count of decls with each number of distinct types in the pack
			/// Count of decls with each percentage of new types in lists.
			/// Types used in the parameter list that recur in the return list are not considered to be new.
			std::map<unsigned, unsigned> p_new;

			ArgPackStats& operator+= (const ArgPackStats& o) {
				sum(n, o.n);
				sum(n_basic, o.n_basic);
				sum(n_generic, o.n_generic);
				sum(n_poly, o.n_poly);
				sum(n_compound, o.n_compound);
				sum(p_basic, o.p_basic);
				sum(p_generic, o.p_generic);
				sum(p_poly, o.p_poly);
				sum(p_compound, o.p_compound);
				sum(n_types, o.n_types);
				sum(p_new, o.p_new);

				return *this;
			}
		};

		struct Stats {
			unsigned n_decls;     ///< Total number of declarations
			/// Count of declarations with each number of assertion parameters
			VectorMap<unsigned> n_type_params;
			/// Count of generic types with each number of type parameters
			VectorMap<unsigned> n_generic_params;
			/// Count of maximum nesting depth of types
			VectorMap<unsigned> n_generic_nesting;
			/// Count of declarations with each name
			std::unordered_map<std::string, unsigned> by_name;
			/// Count of uses of each basic type
			std::unordered_map<std::string, unsigned> basic_type_names;
			/// Count of uses of each generic type name (includes "*", "[]", "(*)", "[,]")
			std::unordered_map<std::string, unsigned> generic_type_names;
			/// Count of uses of each non-generic aggregate type
			std::unordered_map<std::string, unsigned> compound_type_names;
			/// Count of decls using each basic type
			std::unordered_map<std::string, unsigned> basic_type_decls;
			/// Count of decls using each generic type (includes "*", "[]", "(*)", "[,]")
			std::unordered_map<std::string, unsigned> generic_type_decls;
			/// Count of decls using each compound type
			std::unordered_map<std::string, unsigned> compound_type_decls;
			/// Stats for the parameter list
			ArgPackStats params;
			/// Stats for the return list
			ArgPackStats returns;

			/// Count of declarations with each number of assertions
			std::map<unsigned, unsigned> n_assns;
			/// Stats for the assertions' parameters
			ArgPackStats assn_params;
			/// Stats for the assertions' return types
			ArgPackStats assn_returns;

			Stats() : n_decls(0), n_type_params(), n_generic_params(), n_generic_nesting(),
				by_name(), basic_type_names(), generic_type_names(), compound_type_names(),
				basic_type_decls(), generic_type_decls(), compound_type_decls(), params(),
				returns(), n_assns(), assn_params(), assn_returns() {}

		public:
			Stats& operator+= (const Stats& o) {
				sum( n_decls, o.n_decls );
				sum( n_type_params, o.n_type_params );
				sum( n_generic_params, o.n_generic_params );
				sum( n_generic_nesting, o.n_generic_nesting );
				sum( by_name, o.by_name );
				sum( basic_type_names, o.basic_type_names );
				sum( generic_type_names, o.generic_type_names );
				sum( compound_type_names, o.compound_type_names );
				sum( basic_type_decls, o.basic_type_decls );
				sum( generic_type_decls, o.generic_type_decls );
				sum( compound_type_decls, o.compound_type_decls );
				sum( params, o.params );
				sum( returns, o.returns );
				sum( n_assns, o.n_assns );
				sum( assn_params, o.assn_params );
				sum( assn_returns, o.assn_returns );

				return *this;
			}
		};

		/// number of counting bins for linkages
		static const unsigned n_named_specs = 8;
		/// map from total number of specs to bins
		static const unsigned ind_for_linkage[16];

		Stats for_linkage[n_named_specs];            ///< Stores separate stats per linkage
		std::unordered_set<std::string> seen_names;  ///< Stores manglenames already seen to avoid double-counting
		Stats total;
		/// Count of expressions with (depth, fanout)
		std::map<std::pair<unsigned, unsigned>, unsigned> exprs_by_fanout_at_depth;

		void countType( const std::string& name, unsigned& n, std::unordered_map<std::string,
				unsigned>& names, std::unordered_map<std::string, unsigned>& decls,
				std::unordered_set<std::string>& elSeen ) {
			++n;
			++names[ name ];
			if ( elSeen.insert( name ).second ) { ++decls[ name ]; }
		}

		void update_max( unsigned& max, unsigned crnt ) {
			if ( crnt > max ) max = crnt;
		}

		void analyzeSubtype( Type* ty, Stats& stats, std::unordered_set<std::string>& elSeen,
				unsigned& n_poly, bool& seen_poly, unsigned& max_depth, unsigned depth ) {
			unsigned x;
			analyzeType( ty, stats, elSeen, x, x, n_poly, x, seen_poly, max_depth, depth + 1 );
		}

		void analyzeSubtypes( std::list<DeclarationWithType*>& tys, Stats& stats,
				std::unordered_set<std::string>& elSeen, unsigned& n_poly, bool& seen_poly,
				unsigned& max_depth, unsigned depth, unsigned& n_subs ) {
			for ( DeclarationWithType* dwt : tys ) {
				Type* ty = dwt->get_type();
				n_subs += (unsigned)( dynamic_cast<VoidType*>(ty) != nullptr );
				analyzeSubtype( ty, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			}
		}

		void analyzeSubtypes( std::list<Expression*>& tys, Stats& stats,
				std::unordered_set<std::string>& elSeen, unsigned& n_poly, bool& seen_poly,
				unsigned& max_depth, unsigned depth ) {
			for ( Expression* expr : tys ) {
				TypeExpr* texpr = dynamic_cast<TypeExpr*>(expr);
				if ( ! texpr ) continue;
				Type* ty = texpr->get_type();
				analyzeSubtype( ty, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			}
		}

		void analyzeSubtypes( std::list<Type*>& tys, Stats& stats,
				std::unordered_set<std::string>& elSeen, unsigned& n_poly, bool& seen_poly,
				unsigned& max_depth, unsigned depth ) {
			for ( Type* ty : tys ) {
				analyzeSubtype( ty, stats, elSeen, n_poly, seen_poly, max_depth, depth );
			}
		}

		void analyzeType( Type* ty, Stats& stats, std::unordered_set<std::string>& elSeen,
				unsigned& n_basic, unsigned& n_generic, unsigned& n_poly, unsigned& n_agg,
				bool& seen_poly, unsigned& max_depth, unsigned depth = 0 ) {
			if ( BasicType* bt = dynamic_cast<BasicType*>(ty) ) {
				std::string name = BasicType::typeNames[ bt->get_kind() ];
				countType( name, n_basic, stats.basic_type_names, stats.basic_type_decls, elSeen );
				update_max( max_depth, depth );
			} else if ( PointerType* pt = dynamic_cast<PointerType*>(ty) ) {
				std::string name = "*";
				countType(
					name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen);
				analyzeSubtype(
					pt->get_base(), stats, elSeen, n_poly, seen_poly, max_depth, depth );
				++stats.n_generic_params.at( 1 );
			} else if ( ArrayType* at = dynamic_cast<ArrayType*>(ty) ) {
				std::string name = "[]";
				countType(
					name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen);
				analyzeSubtype(
					at->get_base(), stats, elSeen, n_poly, seen_poly, max_depth, depth );
				++stats.n_generic_params.at( 1 );
			} else if ( ReferenceType* rt = dynamic_cast<ReferenceType*>(ty) ) {
				std::string name = "&";
				countType(
					name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen);
				analyzeSubtype(
					rt->get_base(), stats, elSeen, n_poly, seen_poly, max_depth, depth );
				++stats.n_generic_params.at( 1 );
			} else if ( FunctionType* ft = dynamic_cast<FunctionType*>(ty) ) {
				std::string name = "(*)";
				countType(
					name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen);
				unsigned n_subs = 0;
				analyzeSubtypes(
					ft->get_returnVals(), stats, elSeen, n_poly, seen_poly, max_depth, depth,
					n_subs );
				analyzeSubtypes(
					ft->get_parameters(), stats, elSeen, n_poly, seen_poly, max_depth, depth,
					n_subs );
				++stats.n_generic_params.at( n_subs );
			} else if ( TypeInstType* vt = dynamic_cast<TypeInstType*>(ty) ) {
				if ( ! seen_poly ) {
					++n_poly;
					seen_poly = true;
				}
				countType(
					vt->get_name(), n_agg, stats.compound_type_names, stats.compound_type_decls,
					elSeen );
				update_max( max_depth, depth );
			} else if ( ReferenceToType* st = dynamic_cast<ReferenceToType*>(ty) ) {
				std::list<Expression*>& params = st->get_parameters();
				if ( params.empty() ) {
					countType(
						st->get_name(), n_agg, stats.compound_type_names,
						stats.compound_type_decls, elSeen );
					update_max( max_depth, depth );
				} else {
					countType(
						st->get_name(), n_generic, stats.generic_type_names,
						stats.generic_type_decls, elSeen);
					analyzeSubtypes( params, stats, elSeen, n_poly, seen_poly, max_depth, depth );
					++stats.n_generic_params.at( params.size() );
				}
			} else if ( TupleType* tt = dynamic_cast<TupleType*>(ty) ) {
				std::string name = "[,]";
				countType(
					name, n_generic, stats.generic_type_names, stats.generic_type_decls, elSeen);
				analyzeSubtypes(
					tt->get_types(), stats, elSeen, n_poly, seen_poly, max_depth, depth );
				++stats.n_generic_params.at( tt->size() );
			} else if ( dynamic_cast<VarArgsType*>(ty) ) {
				std::string name = "...";
				countType(
					name, n_agg, stats.compound_type_names, stats.compound_type_decls, elSeen );
				update_max( max_depth, depth );
			} else if ( dynamic_cast<ZeroType*>(ty) ) {
				std::string name = "0";
				countType( name, n_basic, stats.basic_type_names, stats.basic_type_decls, elSeen );
				update_max( max_depth, depth );
			} else if ( dynamic_cast<OneType*>(ty) ) {
				std::string name = "1";
				countType( name, n_basic, stats.basic_type_names, stats.basic_type_decls, elSeen );
				update_max( max_depth, depth );
			}
		}

		/// Update arg pack stats based on a declaration list
		void analyze( Stats& stats, std::unordered_set<std::string>& seen,
				std::unordered_set<std::string>& elSeen, ArgPackStats& pstats,
				std::list<DeclarationWithType*>& decls ) {
			std::unordered_set<std::string> types;
			unsigned n = 0;                 ///< number of args/returns
			unsigned n_basic = 0;           ///< number of basic types
			unsigned n_generic = 0;         ///< number of generic types (includes "*", "&", "[]", "(*)", "[,]")
			unsigned n_poly = 0;            ///< number of polymorphic types
			unsigned n_agg = 0;             ///< number of non-generic aggregate types
			unsigned n_new = 0;             ///< number of new types

			for ( auto decl : decls ) {
				Type* dt = decl->get_type();

				n += dt->size();

				std::stringstream ss;
				dt->print( ss );
				types.insert( ss.str() );
				if ( seen.insert( ss.str() ).second ) { ++n_new; }

				bool seen_poly = false;
				unsigned max_depth = 0;
				analyzeType(
					dt, stats, elSeen, n_basic, n_generic, n_poly, n_agg, seen_poly, max_depth );
				++stats.n_generic_nesting.at( max_depth );
			}

			++pstats.n.at( n );
			++pstats.n_basic.at( n_basic );
			++pstats.n_generic.at( n_generic );
			++pstats.n_poly.at( n_poly );
			++pstats.n_compound.at( n_agg );
			if ( n > 0 ) {
				++pstats.p_basic[ n_basic*100/n ];
				++pstats.p_generic[ n_generic*100/n ];
				++pstats.p_poly[ n_poly*100/n ];
				++pstats.p_compound[ n_agg*100/n ];
				if ( n > 1 ) ++pstats.p_new[ (n_new-1)*100/(n-1) ];
			}
			++pstats.n_types.at( types.size() );
		}

		void analyzeFunc( FunctionType* fnTy, Stats& stats, ArgPackStats& params, ArgPackStats& returns ) {
			std::unordered_set<std::string> seen;
			std::unordered_set<std::string> elSeen;
			analyze( stats, seen, elSeen, params, fnTy->get_parameters() );
			analyze( stats, seen, elSeen, returns, fnTy->get_returnVals() );
		}

		void analyzeExpr( UntypedExpr *expr, unsigned depth ) {
			auto& args = expr->get_args();
			unsigned fanout = args.size();

			++exprs_by_fanout_at_depth[ std::make_pair(depth, fanout) ];
			for ( Expression* arg : args ) {
				if ( UntypedExpr *uearg = dynamic_cast<UntypedExpr*>(arg) ) {
					analyzeExpr( uearg, depth+1 );
				}
			}
		}

	public:
		void previsit( FunctionDecl *decl ) {
			// skip if already seen declaration for this function
			const std::string& mangleName = decl->get_mangleName().empty() ? decl->name : decl->get_mangleName();
			if ( seen_names.insert( mangleName ).second ) {
				Stats& stats = for_linkage[ ind_for_linkage[ decl->linkage ] ];

				++stats.n_decls;
				FunctionType* fnTy = decl->type;
				const Type::ForallList& forall = fnTy->forall;
				++stats.n_type_params.at( forall.size() );
				unsigned n_assns = 0;
				for ( TypeDecl* fdecl : forall ) {
					n_assns += fdecl->assertions.size();
					for ( DeclarationWithType* assn : fdecl->assertions ) {
						FunctionType *assnTy = nullptr;
						if ( ObjectDecl *assnObj = dynamic_cast<ObjectDecl*>(assn) ) {
							if ( PointerType *ptrTy = dynamic_cast<PointerType*>(assnObj->get_type()) ) {
								assnTy = dynamic_cast<FunctionType*>(ptrTy->base);
							} else assnTy = dynamic_cast<FunctionType*>(assnObj->get_type());
						} else if ( FunctionDecl *assnDecl = dynamic_cast<FunctionDecl*>(assn) ) {
							assnTy = assnDecl->type;
						}
						if ( assnTy ) analyzeFunc( assnTy, stats, stats.assn_params, stats.assn_returns );
					}
				}
				++stats.n_assns[ n_assns ];
				++stats.by_name[ decl->name ];
				analyzeFunc( fnTy, stats, stats.params, stats.returns );
			}
		}

		void previsit( UntypedExpr *expr ) {
			visit_children = false;
			analyzeExpr( expr, 0 );
		}

	private:
		template<typename F>
		void printAll( const std::string& name, F extract ) {
			std::cout << "\"" << name << "\",";
			for ( const auto& stats : for_linkage ) {
				std::cout << "," << extract(stats);
			}
			std::cout << "," << extract(total) << std::endl;
		}

		template<typename F>
		void printAllMap( const std::string& name, F extract ) {
			for ( const auto& entry : extract(total) ) {
				const auto& key = entry.first;
				std::cout << "\"" << name << "\"," << key;
				for ( const auto& stats : for_linkage ) {
					const auto& map = extract(stats);
					auto it = map.find( key );
					if ( it == map.end() ) std::cout << ",0";
					else std::cout << "," << it->second;
				}
				std::cout  << "," << entry.second << std::endl;
			}
		}

		template<typename F>
		void printAllHisto( const std::string& name, F extract ) {
			VectorMap<unsigned> histos[n_named_specs];
			VectorMap<unsigned> thisto;

			for ( const auto& entry : extract(total) ) { ++thisto.at( entry.second ); }

			for ( unsigned i = 0; i < n_named_specs; ++i ) {
				// can't be a higher count in one of the sub-histograms than the total
				histos[i].reserve( thisto.size() );

				for ( const auto& entry : extract(for_linkage[i]) ) { ++histos[i][entry.second]; }
			}

			for ( unsigned i = 0; i < thisto.size(); ++i ) {
				std::cout << "\"" << name << "\"," << i;
				for ( const auto& histo : histos ) {
					std::cout << "," << histo[i];
				}
				std::cout << "," << thisto[i] << std::endl;
			}
		}

		template<typename F>
		void printAllSparseHisto( const std::string& name, F extract ) {
			std::map<unsigned, unsigned> histos[n_named_specs];
			std::map<unsigned, unsigned> thisto;

			for ( const auto& entry : extract(total) ) { ++thisto[ entry.second ]; }

			for ( unsigned i = 0; i < n_named_specs; ++i ) {
				for ( const auto& entry : extract(for_linkage[i]) ) { ++histos[i][entry.second]; }
			}

			for ( const auto& entry : thisto ) {
				const auto& key = entry.first;
				std::cout << "\"" << name << "\"," << key;
				for ( unsigned i = 0; i < n_named_specs; ++i ) {
					auto it = histos[i].find( key );
					if ( it == histos[i].end() ) std::cout << ",0";
					else std::cout << "," << it->second;
				}
				std::cout << "," << entry.second << std::endl;
			}
		}

		template<typename F>
		void printAllPack( const std::string& name, F extract ) {
			printAllMap("n_basic_" + name, [&extract](const Stats& stats) { return extract(stats).n_basic; });
			printAllMap("n_generic_" + name, [&extract](const Stats& stats) { return extract(stats).n_generic; });
			printAllMap("n_poly_" + name, [&extract](const Stats& stats) { return extract(stats).n_poly; });
			printAllMap("n_compound_" + name, [&extract](const Stats& stats) { return extract(stats).n_compound; });
			printAllMap("n_" + name, [&extract](const Stats& stats) { return extract(stats).n; });
			printAllMap("%_basic_" + name, [&extract](const Stats& stats) { return extract(stats).p_basic; });
			printAllMap("%_generic_" + name, [&extract](const Stats& stats) { return extract(stats).p_generic; });
			printAllMap("%_poly_" + name, [&extract](const Stats& stats) { return extract(stats).p_poly; });
			printAllMap("%_compound_" + name, [&extract](const Stats& stats) { return extract(stats).p_compound; });
			printAllMap("n_distinct_types_" + name, [&extract](const Stats& stats) { return extract(stats).n_types; });
			printAllMap("%_new_types_in_" + name, [&extract](const Stats& stats) { return extract(stats).p_new; });
		}

		void printPairMap( const std::string& name,
		                   const std::map<std::pair<unsigned, unsigned>, unsigned>& map ) {
			for ( const auto& entry : map ) {
				const auto& key = entry.first;
				std::cout << "\"" << name << "\"," << key.first << "," << key.second << ","
				          << entry.second << std::endl;
			}
		}

	public:
		void print() {
			for ( auto& stats : for_linkage ) {
				total += stats;
			}

			std::cout << ",,\"intrinsic\",\"Cforall\",\"C\",\"autogen\",\"compiler\",\"builtinCFA\",\"builtinC\",\"other\",\"TOTAL\"" << std::endl;

			printAllMap("n_type_params", [](const Stats& stats) { return stats.n_type_params; });
			printAllMap("n_generic_params", [](const Stats& stats) { return stats.n_generic_params; });
			printAllMap("n_generic_nesting", [](const Stats& stats) { return stats.n_generic_nesting; });
			printAll("n_decls", [](const Stats& stats) { return stats.n_decls; });
			printAll("unique_names", [](const Stats& stats) { return stats.by_name.size(); });
			printAllSparseHisto("overloads", [](const Stats& stats) { return stats.by_name; });
			printAll("basic_type_names", [](const Stats& stats) { return stats.basic_type_names.size(); });
			printAllSparseHisto("basic_type_uses", [](const Stats& stats) { return stats.basic_type_names; });
			printAllSparseHisto("decls_using_basic_type", [](const Stats& stats) { return stats.basic_type_decls; });
			printAll("generic_type_names", [](const Stats& stats) { return stats.generic_type_names.size(); });
			printAllSparseHisto("generic_type_uses", [](const Stats& stats) { return stats.generic_type_names; });
			printAllSparseHisto("decls_using_generic_type", [](const Stats& stats) { return stats.generic_type_decls; });
			printAll("compound_type_names", [](const Stats& stats) { return stats.compound_type_names.size(); });
			printAllSparseHisto("compound_type_uses", [](const Stats& stats) { return stats.compound_type_names; });
			printAllSparseHisto("decls_using_compound_type", [](const Stats& stats) { return stats.compound_type_decls; });
			printAllPack("params", [](const Stats& stats) { return stats.params; });
			printAllPack("returns", [](const Stats& stats) { return stats.returns; });
			printAllMap("n_assns", [](const Stats& stats) { return stats.n_assns; });
			printAllPack("assn_params", [](const Stats& stats) { return stats.assn_params; });
			printAllPack("assn_returns", [](const Stats& stats) { return stats.assn_returns; });
			std::cout << std::endl;

			printPairMap("exprs_by_depth+fanout", exprs_by_fanout_at_depth);
		}
	};

	const unsigned DeclStats::ind_for_linkage[]
		= { 7, 7, 2, 1,   7, 7, 7, 3,   4, 7, 6, 5,   7, 7, 7, 0 };

	void printDeclStats( std::list< Declaration * > &translationUnit ) {
		PassVisitor<DeclStats> stats;
		acceptAll( translationUnit, stats );
		stats.pass.print();
	}

} // namespace CodeTools

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
