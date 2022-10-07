//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// InitTweak.cc --
//
// Author           : Rob Schluntz
// Created On       : Fri May 13 11:26:36 2016
// Last Modified By : Andrew Beach
// Last Modified On : Wed Sep 22  9:50:00 2022
// Update Count     : 21
//

#include <algorithm>               // for find, all_of
#include <cassert>                 // for assertf, assert, strict_dynamic_cast
#include <iostream>                // for ostream, cerr, endl
#include <iterator>                // for back_insert_iterator, back_inserter
#include <memory>                  // for __shared_ptr
#include <vector>

#include "AST/Expr.hpp"
#include "AST/Init.hpp"
#include "AST/Inspect.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/Type.hpp"
#include "CodeGen/OperatorTable.h" // for isConstructor, isDestructor, isCto...
#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Common/utility.h"        // for toString, deleteAll, maybeClone
#include "GenPoly/GenPoly.h"       // for getFunctionType
#include "InitTweak.h"
#include "ResolvExpr/typeops.h"    // for typesCompatibleIgnoreQualifiers
#include "SymTab/Autogen.h"
#include "SymTab/Indexer.h"        // for Indexer
#include "SynTree/LinkageSpec.h"   // for Spec, isBuiltin, Intrinsic
#include "SynTree/Attribute.h"     // for Attribute
#include "SynTree/Constant.h"      // for Constant
#include "SynTree/Declaration.h"   // for ObjectDecl, DeclarationWithType
#include "SynTree/Expression.h"    // for Expression, UntypedExpr, Applicati...
#include "SynTree/Initializer.h"   // for Initializer, ListInit, Designation
#include "SynTree/Label.h"         // for Label
#include "SynTree/Statement.h"     // for CompoundStmt, ExprStmt, BranchStmt
#include "SynTree/Type.h"          // for FunctionType, ArrayType, PointerType
#include "SynTree/Visitor.h"       // for Visitor, maybeAccept
#include "Tuples/Tuples.h"         // for Tuples::isTtype

namespace InitTweak {
	namespace {
		struct HasDesignations : public WithShortCircuiting {
			bool hasDesignations = false;

			void previsit( BaseSyntaxNode * ) {
				// short circuit if we already know there are designations
				if ( hasDesignations ) visit_children = false;
			}

			void previsit( Designation * des ) {
				// short circuit if we already know there are designations
				if ( hasDesignations ) visit_children = false;
				else if ( ! des->get_designators().empty() ) {
					hasDesignations = true;
					visit_children = false;
				}
			}
		};

		struct InitDepthChecker : public WithGuards {
			bool depthOkay = true;
			Type * type;
			int curDepth = 0, maxDepth = 0;
			InitDepthChecker( Type * type ) : type( type ) {
				Type * t = type;
				while ( ArrayType * at = dynamic_cast< ArrayType * >( t ) ) {
					maxDepth++;
					t = at->get_base();
				}
				maxDepth++;
			}
			void previsit( ListInit * ) {
				curDepth++;
				GuardAction( [this]() { curDepth--; } );
				if ( curDepth > maxDepth ) depthOkay = false;
			}
		};

		struct HasDesignations_new : public ast::WithShortCircuiting {
			bool result = false;

			void previsit( const ast::Node * ) {
				// short circuit if we already know there are designations
				if ( result ) visit_children = false;
			}

			void previsit( const ast::Designation * des ) {
				// short circuit if we already know there are designations
				if ( result ) visit_children = false;
				else if ( ! des->designators.empty() ) {
					result = true;
					visit_children = false;
				}
			}
		};

		struct InitDepthChecker_new : public ast::WithGuards {
			bool result = true;
			const ast::Type * type;
			int curDepth = 0, maxDepth = 0;
			InitDepthChecker_new( const ast::Type * type ) : type( type ) {
				const ast::Type * t = type;
				while ( auto at = dynamic_cast< const ast::ArrayType * >( t ) ) {
					maxDepth++;
					t = at->base;
				}
				maxDepth++;
			}
			void previsit( ListInit * ) {
				curDepth++;
				GuardAction( [this]() { curDepth--; } );
				if ( curDepth > maxDepth ) result = false;
			}
		};

		struct InitFlattener_old : public WithShortCircuiting {
			void previsit( SingleInit * singleInit ) {
				visit_children = false;
				argList.push_back( singleInit->value->clone() );
			}
			std::list< Expression * > argList;
		};

		struct InitFlattener_new : public ast::WithShortCircuiting {
			std::vector< ast::ptr< ast::Expr > > argList;

			void previsit( const ast::SingleInit * singleInit ) {
				visit_children = false;
				argList.emplace_back( singleInit->value );
			}
		};

	} // anonymous namespace

	std::list< Expression * > makeInitList( Initializer * init ) {
		PassVisitor<InitFlattener_old> flattener;
		maybeAccept( init, flattener );
		return flattener.pass.argList;
	}

	bool isDesignated( Initializer * init ) {
		PassVisitor<HasDesignations> finder;
		maybeAccept( init, finder );
		return finder.pass.hasDesignations;
	}

	bool checkInitDepth( ObjectDecl * objDecl ) {
		PassVisitor<InitDepthChecker> checker( objDecl->type );
		maybeAccept( objDecl->init, checker );
		return checker.pass.depthOkay;
	}

	bool isDesignated( const ast::Init * init ) {
		ast::Pass<HasDesignations_new> finder;
		maybe_accept( init, finder );
		return finder.core.result;
	}

	bool checkInitDepth( const ast::ObjectDecl * objDecl ) {
		ast::Pass<InitDepthChecker_new> checker( objDecl->type );
		maybe_accept( objDecl->init.get(), checker );
		return checker.core.result;
	}

std::vector< ast::ptr< ast::Expr > > makeInitList( const ast::Init * init ) {
	ast::Pass< InitFlattener_new > flattener;
	maybe_accept( init, flattener );
	return std::move( flattener.core.argList );
}

	class InitExpander_old::ExpanderImpl {
	public:
		virtual ~ExpanderImpl() = default;
		virtual std::list< Expression * > next( std::list< Expression * > & indices ) = 0;
		virtual Statement * buildListInit( UntypedExpr * callExpr, std::list< Expression * > & indices ) = 0;
	};

	class InitImpl_old : public InitExpander_old::ExpanderImpl {
	public:
		InitImpl_old( Initializer * init ) : init( init ) {}
		virtual ~InitImpl_old() = default;

		virtual std::list< Expression * > next( __attribute((unused)) std::list< Expression * > & indices ) {
			// this is wrong, but just a placeholder for now
			// if ( ! flattened ) flatten( indices );
			// return ! inits.empty() ? makeInitList( inits.front() ) : std::list< Expression * >();
			return makeInitList( init );
		}

		virtual Statement * buildListInit( UntypedExpr * callExpr, std::list< Expression * > & indices );
	private:
		Initializer * init;
	};

	class ExprImpl_old : public InitExpander_old::ExpanderImpl {
	public:
		ExprImpl_old( Expression * expr ) : arg( expr ) {}
		virtual ~ExprImpl_old() { delete arg; }

		virtual std::list< Expression * > next( std::list< Expression * > & indices ) {
			std::list< Expression * > ret;
			Expression * expr = maybeClone( arg );
			if ( expr ) {
				for ( std::list< Expression * >::reverse_iterator it = indices.rbegin(); it != indices.rend(); ++it ) {
					// go through indices and layer on subscript exprs ?[?]
					++it;
					UntypedExpr * subscriptExpr = new UntypedExpr( new NameExpr( "?[?]") );
					subscriptExpr->get_args().push_back( expr );
					subscriptExpr->get_args().push_back( (*it)->clone() );
					expr = subscriptExpr;
				}
				ret.push_back( expr );
			}
			return ret;
		}

		virtual Statement * buildListInit( UntypedExpr * callExpr, std::list< Expression * > & indices );
	private:
		Expression * arg;
	};

	InitExpander_old::InitExpander_old( Initializer * init ) : expander( new InitImpl_old( init ) ) {}

	InitExpander_old::InitExpander_old( Expression * expr ) : expander( new ExprImpl_old( expr ) ) {}

	std::list< Expression * > InitExpander_old::operator*() {
		return cur;
	}

	InitExpander_old & InitExpander_old::operator++() {
		cur = expander->next( indices );
		return *this;
	}

	// use array indices list to build switch statement
	void InitExpander_old::addArrayIndex( Expression * index, Expression * dimension ) {
		indices.push_back( index );
		indices.push_back( dimension );
	}

	void InitExpander_old::clearArrayIndices() {
		deleteAll( indices );
		indices.clear();
	}

	bool InitExpander_old::addReference() {
		bool added = false;
		for ( Expression *& expr : cur ) {
			expr = new AddressExpr( expr );
			added = true;
		}
		return added;
	}

	namespace {
		/// given index i, dimension d, initializer init, and callExpr f, generates
		///   if (i < d) f(..., init)
		///   ++i;
		/// so that only elements within the range of the array are constructed
		template< typename OutIterator >
		void buildCallExpr( UntypedExpr * callExpr, Expression * index, Expression * dimension, Initializer * init, OutIterator out ) {
			UntypedExpr * cond = new UntypedExpr( new NameExpr( "?<?") );
			cond->get_args().push_back( index->clone() );
			cond->get_args().push_back( dimension->clone() );

			std::list< Expression * > args = makeInitList( init );
			callExpr->get_args().splice( callExpr->get_args().end(), args );

			*out++ = new IfStmt( cond, new ExprStmt( callExpr ), nullptr );

			UntypedExpr * increment = new UntypedExpr( new NameExpr( "++?" ) );
			increment->get_args().push_back( index->clone() );
			*out++ = new ExprStmt( increment );
		}

		template< typename OutIterator >
		void build( UntypedExpr * callExpr, InitExpander_old::IndexList::iterator idx, InitExpander_old::IndexList::iterator idxEnd, Initializer * init, OutIterator out ) {
			if ( idx == idxEnd ) return;
			Expression * index = *idx++;
			assert( idx != idxEnd );
			Expression * dimension = *idx++;

			// xxx - may want to eventually issue a warning here if we can detect
			// that the number of elements exceeds to dimension of the array
			if ( idx == idxEnd ) {
				if ( ListInit * listInit = dynamic_cast< ListInit * >( init ) ) {
					for ( Initializer * init : *listInit ) {
						buildCallExpr( callExpr->clone(), index, dimension, init, out );
					}
				} else {
					buildCallExpr( callExpr->clone(), index, dimension, init, out );
				}
			} else {
				std::list< Statement * > branches;

				unsigned long cond = 0;
				ListInit * listInit = dynamic_cast< ListInit * >( init );
				if ( ! listInit ) {
					// xxx - this shouldn't be an error, but need a way to
					// terminate without creating output, so should catch this error
					SemanticError( init->location, "unbalanced list initializers" );
				}

				static UniqueName targetLabel( "L__autogen__" );
				Label switchLabel( targetLabel.newName(), 0, std::list< Attribute * >{ new Attribute("unused") } );
				for ( Initializer * init : *listInit ) {
					Expression * condition;
					// check for designations
					// if ( init-> ) {
						condition = new ConstantExpr( Constant::from_ulong( cond ) );
						++cond;
					// } else {
					// 	condition = // ... take designation
					// 	cond = // ... take designation+1
					// }
					std::list< Statement * > stmts;
					build( callExpr, idx, idxEnd, init, back_inserter( stmts ) );
					stmts.push_back( new BranchStmt( switchLabel, BranchStmt::Break ) );
					CaseStmt * caseStmt = new CaseStmt( condition, stmts );
					branches.push_back( caseStmt );
				}
				*out++ = new SwitchStmt( index->clone(), branches );
				*out++ = new NullStmt( { switchLabel } );
			}
		}
	}

	// if array came with an initializer list: initialize each element
	// may have more initializers than elements in the array - need to check at each index that
	// we haven't exceeded size.
	// may have fewer initializers than elements in the array - need to default construct
	// remaining elements.
	// To accomplish this, generate switch statement, consuming all of expander's elements
	Statement * InitImpl_old::buildListInit( UntypedExpr * dst, std::list< Expression * > & indices ) {
		if ( ! init ) return nullptr;
		CompoundStmt * block = new CompoundStmt();
		build( dst, indices.begin(), indices.end(), init, back_inserter( block->get_kids() ) );
		if ( block->get_kids().empty() ) {
			delete block;
			return nullptr;
		} else {
			init = nullptr; // init was consumed in creating the list init
			return block;
		}
	}

	Statement * ExprImpl_old::buildListInit( UntypedExpr *, std::list< Expression * > & ) {
		return nullptr;
	}

	Statement * InitExpander_old::buildListInit( UntypedExpr * dst ) {
		return expander->buildListInit( dst, indices );
	}

class InitExpander_new::ExpanderImpl {
public:
	virtual ~ExpanderImpl() = default;
	virtual std::vector< ast::ptr< ast::Expr > > next( IndexList & indices ) = 0;
	virtual ast::ptr< ast::Stmt > buildListInit(
		ast::UntypedExpr * callExpr, IndexList & indices ) = 0;
};

namespace {
	template< typename Out >
	void buildCallExpr(
		ast::UntypedExpr * callExpr, const ast::Expr * index, const ast::Expr * dimension,
		const ast::Init * init, Out & out
	) {
		const CodeLocation & loc = init->location;

		auto cond = new ast::UntypedExpr{
			loc, new ast::NameExpr{ loc, "?<?" }, { index, dimension } };

		std::vector< ast::ptr< ast::Expr > > args = makeInitList( init );
		splice( callExpr->args, args );

		out.emplace_back( new ast::IfStmt{ loc, cond, new ast::ExprStmt{ loc, callExpr } } );

		out.emplace_back( new ast::ExprStmt{
			loc, new ast::UntypedExpr{ loc, new ast::NameExpr{ loc, "++?" }, { index } } } );
	}

	template< typename Out >
	void build(
		ast::UntypedExpr * callExpr, const InitExpander_new::IndexList & indices,
		const ast::Init * init, Out & out
	) {
		if ( indices.empty() ) return;

		unsigned idx = 0;

		const ast::Expr * index = indices[idx++];
		assert( idx != indices.size() );
		const ast::Expr * dimension = indices[idx++];

		if ( idx == indices.size() ) {
			if ( auto listInit = dynamic_cast< const ast::ListInit * >( init ) ) {
				for ( const ast::Init * init : *listInit ) {
					buildCallExpr( shallowCopy(callExpr), index, dimension, init, out );
				}
			} else {
				buildCallExpr( shallowCopy(callExpr), index, dimension, init, out );
			}
		} else {
			const CodeLocation & loc = init->location;

			unsigned long cond = 0;
			auto listInit = dynamic_cast< const ast::ListInit * >( init );
			if ( ! listInit ) { SemanticError( loc, "unbalanced list initializers" ); }

			static UniqueName targetLabel( "L__autogen__" );
			ast::Label switchLabel{
				loc, targetLabel.newName(), { new ast::Attribute{ "unused" } } };

			std::vector< ast::ptr< ast::CaseClause > > branches;
			for ( const ast::Init * init : *listInit ) {
				auto condition = ast::ConstantExpr::from_ulong( loc, cond );
				++cond;

				std::vector< ast::ptr< ast::Stmt > > stmts;
				build( callExpr, indices, init, stmts );
				stmts.emplace_back(
					new ast::BranchStmt{ loc, ast::BranchStmt::Break, switchLabel } );
				branches.emplace_back( new ast::CaseClause{ loc, condition, std::move( stmts ) } );
			}
			out.emplace_back( new ast::SwitchStmt{ loc, index, std::move( branches ) } );
			out.emplace_back( new ast::NullStmt{ loc, { switchLabel } } );
		}
	}

	class InitImpl_new final : public InitExpander_new::ExpanderImpl {
		ast::ptr< ast::Init > init;
	public:
		InitImpl_new( const ast::Init * i ) : init( i ) {}

		std::vector< ast::ptr< ast::Expr > > next( InitExpander_new::IndexList & ) override {
			return makeInitList( init );
		}

		ast::ptr< ast::Stmt > buildListInit(
			ast::UntypedExpr * callExpr, InitExpander_new::IndexList & indices
		) override {
			// If array came with an initializer list, initialize each element. We may have more
			// initializers than elements of the array; need to check at each index that we have
			// not exceeded size. We may have fewer initializers than elements in the array; need
			// to default-construct remaining elements. To accomplish this, generate switch
			// statement consuming all of expander's elements

			if ( ! init ) return {};

			std::list< ast::ptr< ast::Stmt > > stmts;
			build( callExpr, indices, init, stmts );
			if ( stmts.empty() ) {
				return {};
			} else {
				auto block = new ast::CompoundStmt{ init->location, std::move( stmts ) };
				init = nullptr;  // consumed in creating the list init
				return block;
			}
		}
	};

	class ExprImpl_new final : public InitExpander_new::ExpanderImpl {
		ast::ptr< ast::Expr > arg;
	public:
		ExprImpl_new( const ast::Expr * a ) : arg( a ) {}

		std::vector< ast::ptr< ast::Expr > > next(
			InitExpander_new::IndexList & indices
		) override {
			if ( ! arg ) return {};

			const CodeLocation & loc = arg->location;
			const ast::Expr * expr = arg;
			for ( auto it = indices.rbegin(); it != indices.rend(); ++it ) {
				// go through indices and layer on subscript exprs ?[?]
				++it;
				expr = new ast::UntypedExpr{
					loc, new ast::NameExpr{ loc, "?[?]" }, { expr, *it } };
			}
			return { expr };
		}

		ast::ptr< ast::Stmt > buildListInit(
			ast::UntypedExpr *, InitExpander_new::IndexList &
		) override {
			return {};
		}
	};
} // anonymous namespace

InitExpander_new::InitExpander_new( const ast::Init * init )
: expander( new InitImpl_new{ init } ), crnt(), indices() {}

InitExpander_new::InitExpander_new( const ast::Expr * expr )
: expander( new ExprImpl_new{ expr } ), crnt(), indices() {}

std::vector< ast::ptr< ast::Expr > > InitExpander_new::operator* () { return crnt; }

InitExpander_new & InitExpander_new::operator++ () {
	crnt = expander->next( indices );
	return *this;
}

/// builds statement which has the same semantics as a C-style list initializer (for array
/// initializers) using callExpr as the base expression to perform initialization
ast::ptr< ast::Stmt > InitExpander_new::buildListInit( ast::UntypedExpr * callExpr ) {
	return expander->buildListInit( callExpr, indices );
}

void InitExpander_new::addArrayIndex( const ast::Expr * index, const ast::Expr * dimension ) {
	indices.emplace_back( index );
	indices.emplace_back( dimension );
}

void InitExpander_new::clearArrayIndices() { indices.clear(); }

bool InitExpander_new::addReference() {
	for ( ast::ptr< ast::Expr > & expr : crnt ) {
		expr = new ast::AddressExpr{ expr };
	}
	return ! crnt.empty();
}

	Type * getTypeofThis( FunctionType * ftype ) {
		assertf( ftype, "getTypeofThis: nullptr ftype" );
		ObjectDecl * thisParam = getParamThis( ftype );
		ReferenceType * refType = strict_dynamic_cast< ReferenceType * >( thisParam->type );
		return refType->base;
	}

	const ast::Type * getTypeofThis( const ast::FunctionType * ftype ) {
		assertf( ftype, "getTypeofThis: nullptr ftype" );
		const std::vector<ast::ptr<ast::Type>> & params = ftype->params;
		assertf( !params.empty(), "getTypeofThis: ftype with 0 parameters: %s",
				toString( ftype ).c_str() );
		const ast::ReferenceType * refType =
			params.front().strict_as<ast::ReferenceType>();
		return refType->base;
	}

	ObjectDecl * getParamThis( FunctionType * ftype ) {
		assertf( ftype, "getParamThis: nullptr ftype" );
		auto & params = ftype->parameters;
		assertf( ! params.empty(), "getParamThis: ftype with 0 parameters: %s", toString( ftype ).c_str() );
		return strict_dynamic_cast< ObjectDecl * >( params.front() );
	}

	const ast::ObjectDecl * getParamThis(const ast::FunctionDecl * func) {
		assertf( func, "getParamThis: nullptr ftype" );
		auto & params = func->params;
		assertf( ! params.empty(), "getParamThis: ftype with 0 parameters: %s", toString( func ).c_str());
		return params.front().strict_as<ast::ObjectDecl>();
	}

	bool tryConstruct( DeclarationWithType * dwt ) {
		ObjectDecl * objDecl = dynamic_cast< ObjectDecl * >( dwt );
		if ( ! objDecl ) return false;
		return (objDecl->get_init() == nullptr ||
				( objDecl->get_init() != nullptr && objDecl->get_init()->get_maybeConstructed() ))
			&& ! objDecl->get_storageClasses().is_extern
			&& isConstructable( objDecl->type );
	}

	bool isConstructable( Type * type ) {
		return ! dynamic_cast< VarArgsType * >( type ) && ! dynamic_cast< ReferenceType * >( type ) && ! dynamic_cast< FunctionType * >( type ) && ! Tuples::isTtype( type );
	}

	bool tryConstruct( const ast::DeclWithType * dwt ) {
		auto objDecl = dynamic_cast< const ast::ObjectDecl * >( dwt );
		if ( ! objDecl ) return false;
		return (objDecl->init == nullptr ||
				( objDecl->init != nullptr && objDecl->init->maybeConstructed ))
			&& ! objDecl->storage.is_extern
			&& isConstructable( objDecl->type );
	}

	bool isConstructable( const ast::Type * type ) {
		return ! dynamic_cast< const ast::VarArgsType * >( type ) && ! dynamic_cast< const ast::ReferenceType * >( type )
		&& ! dynamic_cast< const ast::FunctionType * >( type ) && ! Tuples::isTtype( type );
	}

	struct CallFinder_old {
		CallFinder_old( const std::list< std::string > & names ) : names( names ) {}

		void postvisit( ApplicationExpr * appExpr ) {
			handleCallExpr( appExpr );
		}

		void postvisit( UntypedExpr * untypedExpr ) {
			handleCallExpr( untypedExpr );
		}

		std::list< Expression * > * matches;
	private:
		const std::list< std::string > names;

		template< typename CallExpr >
		void handleCallExpr( CallExpr * expr ) {
			std::string fname = getFunctionName( expr );
			if ( std::find( names.begin(), names.end(), fname ) != names.end() ) {
				matches->push_back( expr );
			}
		}
	};

	struct CallFinder_new final {
		std::vector< const ast::Expr * > matches;
		const std::vector< std::string > names;

		CallFinder_new( std::vector< std::string > && ns ) : matches(), names( std::move(ns) ) {}

		void handleCallExpr( const ast::Expr * expr ) {
			std::string fname = getFunctionName( expr );
			if ( std::find( names.begin(), names.end(), fname ) != names.end() ) {
				matches.emplace_back( expr );
			}
		}

		void postvisit( const ast::ApplicationExpr * expr ) { handleCallExpr( expr ); }
		void postvisit( const ast::UntypedExpr *     expr ) { handleCallExpr( expr ); }
	};

	void collectCtorDtorCalls( Statement * stmt, std::list< Expression * > & matches ) {
		static PassVisitor<CallFinder_old> finder( std::list< std::string >{ "?{}", "^?{}" } );
		finder.pass.matches = &matches;
		maybeAccept( stmt, finder );
	}

	std::vector< const ast::Expr * > collectCtorDtorCalls( const ast::Stmt * stmt ) {
		ast::Pass< CallFinder_new > finder{ std::vector< std::string >{ "?{}", "^?{}" } };
		maybe_accept( stmt, finder );
		return std::move( finder.core.matches );
	}

	Expression * getCtorDtorCall( Statement * stmt ) {
		std::list< Expression * > matches;
		collectCtorDtorCalls( stmt, matches );
		assertf( matches.size() <= 1, "%zd constructor/destructors found in %s", matches.size(), toString( stmt ).c_str() );
		return matches.size() == 1 ? matches.front() : nullptr;
	}

	namespace {
		DeclarationWithType * getCalledFunction( Expression * expr );

		template<typename CallExpr>
		DeclarationWithType * handleDerefCalledFunction( CallExpr * expr ) {
			// (*f)(x) => should get "f"
			std::string name = getFunctionName( expr );
			assertf( name == "*?", "Unexpected untyped expression: %s", name.c_str() );
			assertf( ! expr->get_args().empty(), "Cannot get called function from dereference with no arguments" );
			return getCalledFunction( expr->get_args().front() );
		}

		DeclarationWithType * getCalledFunction( Expression * expr ) {
			assert( expr );
			if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( expr ) ) {
				return varExpr->var;
			} else if ( MemberExpr * memberExpr = dynamic_cast< MemberExpr * >( expr ) ) {
				return memberExpr->member;
			} else if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( expr ) ) {
				return getCalledFunction( castExpr->arg );
			} else if ( UntypedExpr * untypedExpr = dynamic_cast< UntypedExpr * >( expr ) ) {
				return handleDerefCalledFunction( untypedExpr );
			} else if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * > ( expr ) ) {
				return handleDerefCalledFunction( appExpr );
			} else if ( AddressExpr * addrExpr = dynamic_cast< AddressExpr * >( expr ) ) {
				return getCalledFunction( addrExpr->arg );
			} else if ( CommaExpr * commaExpr = dynamic_cast< CommaExpr * >( expr ) ) {
				return getCalledFunction( commaExpr->arg2 );
			}
			return nullptr;
		}

		DeclarationWithType * getFunctionCore( const Expression * expr ) {
			if ( const auto * appExpr = dynamic_cast< const ApplicationExpr * >( expr ) ) {
				return getCalledFunction( appExpr->function );
			} else if ( const auto * untyped = dynamic_cast< const UntypedExpr * >( expr ) ) {
				return getCalledFunction( untyped->function );
			}
			assertf( false, "getFunction with unknown expression: %s", toString( expr ).c_str() );
		}
	}

	DeclarationWithType * getFunction( Expression * expr ) {
		return getFunctionCore( expr );
	}

	const DeclarationWithType * getFunction( const Expression * expr ) {
		return getFunctionCore( expr );
	}

	ApplicationExpr * isIntrinsicCallExpr( Expression * expr ) {
		ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * >( expr );
		if ( ! appExpr ) return nullptr;
		DeclarationWithType * function = getCalledFunction( appExpr->get_function() );
		assertf( function, "getCalledFunction returned nullptr: %s", toString( appExpr->get_function() ).c_str() );
		// check for Intrinsic only - don't want to remove all overridable ctor/dtors because autogenerated ctor/dtor
		// will call all member dtors, and some members may have a user defined dtor.
		return function->get_linkage() == LinkageSpec::Intrinsic ? appExpr : nullptr;
	}

	namespace {
		template <typename Predicate>
		bool allofCtorDtor( Statement * stmt, const Predicate & pred ) {
			std::list< Expression * > callExprs;
			collectCtorDtorCalls( stmt, callExprs );
			return std::all_of( callExprs.begin(), callExprs.end(), pred);
		}

		template <typename Predicate>
		bool allofCtorDtor( const ast::Stmt * stmt, const Predicate & pred ) {
			std::vector< const ast::Expr * > callExprs = collectCtorDtorCalls( stmt );
			return std::all_of( callExprs.begin(), callExprs.end(), pred );
		}
	}

	bool isIntrinsicSingleArgCallStmt( Statement * stmt ) {
		return allofCtorDtor( stmt, []( Expression * callExpr ){
			if ( ApplicationExpr * appExpr = isIntrinsicCallExpr( callExpr ) ) {
				FunctionType *funcType = GenPoly::getFunctionType( appExpr->function->result );
				assert( funcType );
				return funcType->get_parameters().size() == 1;
			}
			return false;
		});
	}

	bool isIntrinsicSingleArgCallStmt( const ast::Stmt * stmt ) {
		return allofCtorDtor( stmt, []( const ast::Expr * callExpr ){
			if ( const ast::ApplicationExpr * appExpr = isIntrinsicCallExpr( callExpr ) ) {
				const ast::FunctionType * funcType =
					GenPoly::getFunctionType( appExpr->func->result );
				assert( funcType );
				return funcType->params.size() == 1;
			}
			return false;
		});
	}

	bool isIntrinsicCallStmt( Statement * stmt ) {
		return allofCtorDtor( stmt, []( Expression * callExpr ) {
			return isIntrinsicCallExpr( callExpr );
		});
	}

	namespace {
		template<typename CallExpr>
		Expression *& callArg( CallExpr * callExpr, unsigned int pos ) {
			if ( pos >= callExpr->get_args().size() ) assertf( false, "getCallArg for argument that doesn't exist: (%u); %s.", pos, toString( callExpr ).c_str() );
			for ( Expression *& arg : callExpr->get_args() ) {
				if ( pos == 0 ) return arg;
				pos--;
			}
			assert( false );
		}
	}

	Expression *& getCallArg( Expression * callExpr, unsigned int pos ) {
		if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * >( callExpr ) ) {
			return callArg( appExpr, pos );
		} else if ( UntypedExpr * untypedExpr = dynamic_cast< UntypedExpr * >( callExpr ) ) {
			return callArg( untypedExpr, pos );
		} else if ( TupleAssignExpr * tupleExpr = dynamic_cast< TupleAssignExpr * > ( callExpr ) ) {
			std::list< Statement * > & stmts = tupleExpr->get_stmtExpr()->get_statements()->get_kids();
			assertf( ! stmts.empty(), "TupleAssignExpr somehow has no statements." );
			ExprStmt * stmt = strict_dynamic_cast< ExprStmt * >( stmts.back() );
			TupleExpr * tuple = strict_dynamic_cast< TupleExpr * >( stmt->get_expr() );
			assertf( ! tuple->get_exprs().empty(), "TupleAssignExpr somehow has empty tuple expr." );
			return getCallArg( tuple->get_exprs().front(), pos );
		} else if ( ImplicitCopyCtorExpr * copyCtor = dynamic_cast< ImplicitCopyCtorExpr * >( callExpr ) ) {
			return getCallArg( copyCtor->callExpr, pos );
		} else {
			assertf( false, "Unexpected expression type passed to getCallArg: %s", toString( callExpr ).c_str() );
		}
	}

	namespace {
		std::string funcName( Expression * func );

		template<typename CallExpr>
		std::string handleDerefName( CallExpr * expr ) {
			// (*f)(x) => should get name "f"
			std::string name = getFunctionName( expr );
			assertf( name == "*?", "Unexpected untyped expression: %s", name.c_str() );
			assertf( ! expr->get_args().empty(), "Cannot get function name from dereference with no arguments" );
			return funcName( expr->get_args().front() );
		}

		std::string funcName( Expression * func ) {
			if ( NameExpr * nameExpr = dynamic_cast< NameExpr * >( func ) ) {
				return nameExpr->get_name();
			} else if ( VariableExpr * varExpr = dynamic_cast< VariableExpr * >( func ) ) {
				return varExpr->get_var()->get_name();
			} else if ( CastExpr * castExpr = dynamic_cast< CastExpr * >( func ) ) {
				return funcName( castExpr->get_arg() );
			} else if ( MemberExpr * memberExpr = dynamic_cast< MemberExpr * >( func ) ) {
				return memberExpr->get_member()->get_name();
			} else if ( UntypedMemberExpr * memberExpr = dynamic_cast< UntypedMemberExpr * > ( func ) ) {
				return funcName( memberExpr->get_member() );
			} else if ( UntypedExpr * untypedExpr = dynamic_cast< UntypedExpr * >( func ) ) {
				return handleDerefName( untypedExpr );
			} else if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * >( func ) ) {
				return handleDerefName( appExpr );
			} else if ( ConstructorExpr * ctorExpr = dynamic_cast< ConstructorExpr * >( func ) ) {
				return funcName( getCallArg( ctorExpr->get_callExpr(), 0 ) );
			} else {
				assertf( false, "Unexpected expression type being called as a function in call expression: %s", toString( func ).c_str() );
			}
		}
	}

	std::string getFunctionName( Expression * expr ) {
		// there's some unforunate overlap here with getCalledFunction. Ideally this would be able to use getCalledFunction and
		// return the name of the DeclarationWithType, but this needs to work for NameExpr and UntypedMemberExpr, where getCalledFunction
		// can't possibly do anything reasonable.
		if ( ApplicationExpr * appExpr = dynamic_cast< ApplicationExpr * >( expr ) ) {
			return funcName( appExpr->get_function() );
		} else if ( UntypedExpr * untypedExpr = dynamic_cast< UntypedExpr * > ( expr ) ) {
			return funcName( untypedExpr->get_function() );
		} else {
			std::cerr << expr << std::endl;
			assertf( false, "Unexpected expression type passed to getFunctionName" );
		}
	}

	Type * getPointerBase( Type * type ) {
		if ( PointerType * ptrType = dynamic_cast< PointerType * >( type ) ) {
			return ptrType->get_base();
		} else if ( ArrayType * arrayType = dynamic_cast< ArrayType * >( type ) ) {
			return arrayType->get_base();
		} else if ( ReferenceType * refType = dynamic_cast< ReferenceType * >( type ) ) {
			return refType->get_base();
		} else {
			return nullptr;
		}
	}

	Type * isPointerType( Type * type ) {
		return getPointerBase( type ) ? type : nullptr;
	}

	ApplicationExpr * createBitwiseAssignment( Expression * dst, Expression * src ) {
		static FunctionDecl * assign = nullptr;
		if ( ! assign ) {
			// temporary? Generate a fake assignment operator to represent bitwise assignments.
			// This operator could easily exist as a real function, but it's tricky because nothing should resolve to this function.
			TypeDecl * td = new TypeDecl( "T", noStorageClasses, nullptr, TypeDecl::Dtype, true );
			assign = new FunctionDecl( "?=?", noStorageClasses, LinkageSpec::Intrinsic, SymTab::genAssignType( new TypeInstType( noQualifiers, td->name, td ) ), nullptr );
		}
		if ( dynamic_cast< ReferenceType * >( dst->result ) ) {
			for (int depth = dst->result->referenceDepth(); depth > 0; depth--) {
				dst = new AddressExpr( dst );
			}
		} else {
			dst = new CastExpr( dst, new ReferenceType( noQualifiers, dst->result->clone() ) );
		}
		if ( dynamic_cast< ReferenceType * >( src->result ) ) {
			for (int depth = src->result->referenceDepth(); depth > 0; depth--) {
				src = new AddressExpr( src );
			}
		}
		return new ApplicationExpr( VariableExpr::functionPointer( assign ), { dst, src } );
	}

	// looks like some other such codegen uses UntypedExpr and does not create fake function. should revisit afterwards
	// following passes may accidentally resolve this expression if returned as untyped...
	ast::Expr * createBitwiseAssignment (const ast::Expr * dst, const ast::Expr * src) {
		static ast::ptr<ast::FunctionDecl> assign = nullptr;
		if (!assign) {
			auto td = new ast::TypeDecl(CodeLocation(), "T", {}, nullptr, ast::TypeDecl::Dtype, true);
			assign = new ast::FunctionDecl(CodeLocation(), "?=?", {},
			{ new ast::ObjectDecl(CodeLocation(), "_dst", new ast::ReferenceType(new ast::TypeInstType("T", td))),
			  new ast::ObjectDecl(CodeLocation(), "_src", new ast::TypeInstType("T", td))},
			{ new ast::ObjectDecl(CodeLocation(), "_ret", new ast::TypeInstType("T", td))}, nullptr, {}, ast::Linkage::Intrinsic);
		}
		if (dst->result.as<ast::ReferenceType>()) {
			for (int depth = dst->result->referenceDepth(); depth > 0; depth--) {
				dst = new ast::AddressExpr(dst);
			}
		}
		else {
			dst = new ast::CastExpr(dst, new ast::ReferenceType(dst->result, {}));
		}
		if (src->result.as<ast::ReferenceType>()) {
			for (int depth = src->result->referenceDepth(); depth > 0; depth--) {
				src = new ast::AddressExpr(src);
			}
		}
		return new ast::ApplicationExpr(dst->location, ast::VariableExpr::functionPointer(dst->location, assign), {dst, src});
	}

	struct ConstExprChecker : public WithShortCircuiting {
		// most expressions are not const expr
		void previsit( Expression * ) { isConstExpr = false; visit_children = false; }

		void previsit( AddressExpr *addressExpr ) {
			visit_children = false;

			// address of a variable or member expression is constexpr
			Expression * arg = addressExpr->get_arg();
			if ( ! dynamic_cast< NameExpr * >( arg) && ! dynamic_cast< VariableExpr * >( arg ) && ! dynamic_cast< MemberExpr * >( arg ) && ! dynamic_cast< UntypedMemberExpr * >( arg ) ) isConstExpr = false;
		}

		// these expressions may be const expr, depending on their children
		void previsit( SizeofExpr * ) {}
		void previsit( AlignofExpr * ) {}
		void previsit( UntypedOffsetofExpr * ) {}
		void previsit( OffsetofExpr * ) {}
		void previsit( OffsetPackExpr * ) {}
		void previsit( CommaExpr * ) {}
		void previsit( LogicalExpr * ) {}
		void previsit( ConditionalExpr * ) {}
		void previsit( CastExpr * ) {}
		void previsit( ConstantExpr * ) {}

		void previsit( VariableExpr * varExpr ) {
			visit_children = false;

			if ( EnumInstType * inst = dynamic_cast< EnumInstType * >( varExpr->result ) ) {
				long long int value;
				if ( inst->baseEnum->valueOf( varExpr->var, value ) ) {
					// enumerators are const expr
					return;
				}
			}
			isConstExpr = false;
		}

		bool isConstExpr = true;
	};

	struct ConstExprChecker_new : public ast::WithShortCircuiting {
		// most expressions are not const expr
		void previsit( const ast::Expr * ) { result = false; visit_children = false; }

		void previsit( const ast::AddressExpr *addressExpr ) {
			visit_children = false;
			const ast::Expr * arg = addressExpr->arg;

			// address of a variable or member expression is constexpr
			if ( ! dynamic_cast< const ast::NameExpr * >( arg )
			&& ! dynamic_cast< const ast::VariableExpr * >( arg )
			&& ! dynamic_cast< const ast::MemberExpr * >( arg )
			&& ! dynamic_cast< const ast::UntypedMemberExpr * >( arg ) ) result = false;
		}

		// these expressions may be const expr, depending on their children
		void previsit( const ast::SizeofExpr * ) {}
		void previsit( const ast::AlignofExpr * ) {}
		void previsit( const ast::UntypedOffsetofExpr * ) {}
		void previsit( const ast::OffsetofExpr * ) {}
		void previsit( const ast::OffsetPackExpr * ) {}
		void previsit( const ast::CommaExpr * ) {}
		void previsit( const ast::LogicalExpr * ) {}
		void previsit( const ast::ConditionalExpr * ) {}
		void previsit( const ast::CastExpr * ) {}
		void previsit( const ast::ConstantExpr * ) {}

		void previsit( const ast::VariableExpr * varExpr ) {
			visit_children = false;

			if ( auto inst = varExpr->result.as<ast::EnumInstType>() ) {
				long long int value;
				if ( inst->base->valueOf( varExpr->var, value ) ) {
					// enumerators are const expr
					return;
				}
			}
			result = false;
		}

		bool result = true;
	};

	bool isConstExpr( Expression * expr ) {
		if ( expr ) {
			PassVisitor<ConstExprChecker> checker;
			expr->accept( checker );
			return checker.pass.isConstExpr;
		}
		return true;
	}

	bool isConstExpr( Initializer * init ) {
		if ( init ) {
			PassVisitor<ConstExprChecker> checker;
			init->accept( checker );
			return checker.pass.isConstExpr;
		} // if
		// for all intents and purposes, no initializer means const expr
		return true;
	}

	bool isConstExpr( const ast::Expr * expr ) {
		if ( expr ) {
			ast::Pass<ConstExprChecker_new> checker;
			expr->accept( checker );
			return checker.core.result;
		}
		return true;
	}

	bool isConstExpr( const ast::Init * init ) {
		if ( init ) {
			ast::Pass<ConstExprChecker_new> checker;
			init->accept( checker );
			return checker.core.result;
		} // if
		// for all intents and purposes, no initializer means const expr
		return true;
	}

	const FunctionDecl * isCopyFunction( const Declaration * decl, const std::string & fname ) {
		const FunctionDecl * function = dynamic_cast< const FunctionDecl * >( decl );
		if ( ! function ) return nullptr;
		if ( function->name != fname ) return nullptr;
		FunctionType * ftype = function->type;
		if ( ftype->parameters.size() != 2 ) return nullptr;

		Type * t1 = getPointerBase( ftype->get_parameters().front()->get_type() );
		Type * t2 = ftype->parameters.back()->get_type();
		assert( t1 );

		if ( ResolvExpr::typesCompatibleIgnoreQualifiers( t1, t2, SymTab::Indexer() ) ) {
			return function;
		} else {
			return nullptr;
		}
	}

bool isAssignment( const ast::FunctionDecl * decl ) {
	return CodeGen::isAssignment( decl->name ) && isCopyFunction( decl );
}

bool isDestructor( const ast::FunctionDecl * decl ) {
	return CodeGen::isDestructor( decl->name );
}

bool isDefaultConstructor( const ast::FunctionDecl * decl ) {
	return CodeGen::isConstructor( decl->name ) && 1 == decl->params.size();
}

bool isCopyConstructor( const ast::FunctionDecl * decl ) {
	return CodeGen::isConstructor( decl->name ) && 2 == decl->params.size();
}

bool isCopyFunction( const ast::FunctionDecl * decl ) {
	const ast::FunctionType * ftype = decl->type;
	if ( ftype->params.size() != 2 ) return false;

	const ast::Type * t1 = ast::getPointerBase( ftype->params.front() );
	if ( ! t1 ) return false;
	const ast::Type * t2 = ftype->params.back();

	return ResolvExpr::typesCompatibleIgnoreQualifiers( t1, t2, ast::SymbolTable() );
}


	const FunctionDecl * isAssignment( const Declaration * decl ) {
		return isCopyFunction( decl, "?=?" );
	}
	const FunctionDecl * isDestructor( const Declaration * decl ) {
		if ( CodeGen::isDestructor( decl->name ) ) {
			return dynamic_cast< const FunctionDecl * >( decl );
		}
		return nullptr;
	}
	const FunctionDecl * isDefaultConstructor( const Declaration * decl ) {
		if ( CodeGen::isConstructor( decl->name ) ) {
			if ( const FunctionDecl * func = dynamic_cast< const FunctionDecl * >( decl ) ) {
				if ( func->type->parameters.size() == 1 ) {
					return func;
				}
			}
		}
		return nullptr;
	}
	const FunctionDecl * isCopyConstructor( const Declaration * decl ) {
		return isCopyFunction( decl, "?{}" );
	}

	#if defined( __x86_64 ) || defined( __i386 ) // assembler comment to prevent assembler warning message
		#define ASM_COMMENT "#"
	#else // defined( __ARM_ARCH )
		#define ASM_COMMENT "//"
	#endif
	static const char * const data_section =  ".data" ASM_COMMENT;
	static const char * const tlsd_section = ".tdata" ASM_COMMENT;
	void addDataSectionAttribute( ObjectDecl * objDecl ) {
		const bool is_tls = objDecl->get_storageClasses().is_threadlocal_any();
		const char * section = is_tls ? tlsd_section : data_section;
		objDecl->attributes.push_back(new Attribute("section", {
			new ConstantExpr( Constant::from_string( section ) )
		}));
	}

	void addDataSectionAttribute( ast::ObjectDecl * objDecl ) {
		const bool is_tls = objDecl->storage.is_threadlocal_any();
		const char * section = is_tls ? tlsd_section : data_section;
		objDecl->attributes.push_back(new ast::Attribute("section", {
			ast::ConstantExpr::from_string(objDecl->location, section)
		}));
	}

}
