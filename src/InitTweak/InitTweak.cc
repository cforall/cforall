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
#include "Common/SemanticError.h"  // for SemanticError
#include "Common/UniqueName.h"     // for UniqueName
#include "Common/utility.h"        // for toString, deleteAll, maybeClone
#include "GenPoly/GenPoly.h"       // for getFunctionType
#include "InitTweak.h"
#include "ResolvExpr/Unify.h"      // for typesCompatibleIgnoreQualifiers
#include "Tuples/Tuples.h"         // for Tuples::isTtype

namespace InitTweak {
	namespace {
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

		struct InitDepthChecker_new {
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
			void previsit( ast::ListInit const * ) {
				curDepth++;
				if ( curDepth > maxDepth ) result = false;
			}
			void postvisit( ast::ListInit const * ) {
				curDepth--;
			}
		};

		struct InitFlattener_new : public ast::WithShortCircuiting {
			std::vector< ast::ptr< ast::Expr > > argList;

			void previsit( const ast::SingleInit * singleInit ) {
				visit_children = false;
				argList.emplace_back( singleInit->value );
			}
		};

	} // anonymous namespace

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

	const ast::Type * getTypeofThis( const ast::FunctionType * ftype ) {
		assertf( ftype, "getTypeofThis: nullptr ftype" );
		const std::vector<ast::ptr<ast::Type>> & params = ftype->params;
		assertf( !params.empty(), "getTypeofThis: ftype with 0 parameters: %s",
				toString( ftype ).c_str() );
		const ast::ReferenceType * refType =
			params.front().strict_as<ast::ReferenceType>();
		return refType->base;
	}

	const ast::ObjectDecl * getParamThis(const ast::FunctionDecl * func) {
		assertf( func, "getParamThis: nullptr ftype" );
		auto & params = func->params;
		assertf( ! params.empty(), "getParamThis: ftype with 0 parameters: %s", toString( func ).c_str());
		return params.front().strict_as<ast::ObjectDecl>();
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

	std::vector< const ast::Expr * > collectCtorDtorCalls( const ast::Stmt * stmt ) {
		ast::Pass< CallFinder_new > finder{ std::vector< std::string >{ "?{}", "^?{}" } };
		maybe_accept( stmt, finder );
		return std::move( finder.core.matches );
	}

	namespace {
		template <typename Predicate>
		bool allofCtorDtor( const ast::Stmt * stmt, const Predicate & pred ) {
			std::vector< const ast::Expr * > callExprs = collectCtorDtorCalls( stmt );
			return std::all_of( callExprs.begin(), callExprs.end(), pred );
		}
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

	// looks like some other such codegen uses UntypedExpr and does not create fake function. should revisit afterwards
	// following passes may accidentally resolve this expression if returned as untyped...
	ast::Expr * createBitwiseAssignment (const ast::Expr * dst, const ast::Expr * src) {
		static ast::ptr<ast::FunctionDecl> assign = nullptr;
		if (!assign) {
			auto td = new ast::TypeDecl(CodeLocation(), "T", {}, nullptr, ast::TypeDecl::Dtype, true);
			assign = new ast::FunctionDecl(CodeLocation(), "?=?", {td},
			{ new ast::ObjectDecl(CodeLocation(), "_dst", new ast::ReferenceType(new ast::TypeInstType("T", td))),
			  new ast::ObjectDecl(CodeLocation(), "_src", new ast::TypeInstType("T", td))},
			{ new ast::ObjectDecl(CodeLocation(), "_ret", new ast::TypeInstType("T", td))}, nullptr, {}, ast::Linkage::Intrinsic);
		}
		if (dst->result.as<ast::ReferenceType>()) {
			for (int depth = dst->result->referenceDepth(); depth > 0; depth--) {
				dst = new ast::AddressExpr(dst);
			}
		} else {
			dst = new ast::CastExpr(dst, new ast::ReferenceType(dst->result, {}));
		}
		if (src->result.as<ast::ReferenceType>()) {
			for (int depth = src->result->referenceDepth(); depth > 0; depth--) {
				src = new ast::AddressExpr(src);
			}
		}
		auto var = ast::VariableExpr::functionPointer(dst->location, assign);
		auto app = new ast::ApplicationExpr(dst->location, var, {dst, src});
		// Skip the resolver, just set the result to the correct type.
		app->result = ast::deepCopy( src->result );
		return app;
	}

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

	return ResolvExpr::typesCompatibleIgnoreQualifiers( t1, t2 );
}

	#if defined( __x86_64 ) || defined( __i386 ) // assembler comment to prevent assembler warning message
		#define ASM_COMMENT "#"
	#else // defined( __ARM_ARCH )
		#define ASM_COMMENT "//"
	#endif
	static const char * const data_section =  ".data" ASM_COMMENT;
	static const char * const tlsd_section = ".tdata" ASM_COMMENT;

	void addDataSectionAttribute( ast::ObjectDecl * objDecl ) {
		const bool is_tls = objDecl->storage.is_threadlocal_any();
		const char * section = is_tls ? tlsd_section : data_section;
		objDecl->attributes.push_back(new ast::Attribute("section", {
			ast::ConstantExpr::from_string(objDecl->location, section)
		}));
	}

}
