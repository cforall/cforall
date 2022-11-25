//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// FindOpenVars.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 09:42:48 2015
// Last Modified By : Andrew
// Last Modified On : Fri Jul 12 14:18:00 2019
// Update Count     : 4
//

#include "FindOpenVars.h"

#include <list>                   // for _List_const_iterator, list<>::const...
#include <map>                    // for map<>::mapped_type

#include "AST/Pass.hpp"
#include "AST/Type.hpp"
#include "Common/PassVisitor.h"
#include "SynTree/Declaration.h"  // for TypeDecl, DeclarationWithType (ptr ...
#include "SynTree/Type.h"         // for Type, Type::ForallList, ArrayType

namespace ResolvExpr {
	struct FindOpenVars_old : public WithGuards {
		FindOpenVars_old( OpenVarSet &openVars, OpenVarSet &closedVars, AssertionSet &needAssertions, AssertionSet &haveAssertions, bool firstIsOpen );

		void previsit( const PointerType * pointerType );
		void previsit( const ArrayType * arrayType );
		void previsit( const FunctionType * functionType );
		void previsit( const TupleType * tupleType );

		void common_action( const Type *type );

		OpenVarSet &openVars, &closedVars;
		AssertionSet &needAssertions, &haveAssertions;
		bool nextIsOpen;
	};

	void findOpenVars( const Type *type, OpenVarSet &openVars, OpenVarSet &closedVars, AssertionSet &needAssertions, AssertionSet &haveAssertions, bool firstIsOpen ) {
		PassVisitor<FindOpenVars_old> finder( openVars, closedVars, needAssertions, haveAssertions, firstIsOpen );
		type->accept( finder );
	}

	FindOpenVars_old::FindOpenVars_old( OpenVarSet &openVars, OpenVarSet &closedVars, AssertionSet &needAssertions, AssertionSet &haveAssertions, bool firstIsOpen )
		: openVars( openVars ), closedVars( closedVars ), needAssertions( needAssertions ), haveAssertions( haveAssertions ), nextIsOpen( firstIsOpen )	{
	}

	void FindOpenVars_old::common_action( const Type * type ) {
		if ( nextIsOpen ) {
			for ( Type::ForallList::const_iterator i = type->forall.begin(); i != type->forall.end(); ++i ) {
				openVars[ (*i)->get_name() ] = TypeDecl::Data{ (*i) };
				for ( std::list< DeclarationWithType* >::const_iterator assert = (*i)->get_assertions().begin(); assert != (*i)->get_assertions().end(); ++assert ) {
					needAssertions[ *assert ].isUsed = false;
				}
///       cloneAll( (*i)->get_assertions(), needAssertions );
///       needAssertions.insert( needAssertions.end(), (*i)->get_assertions().begin(), (*i)->get_assertions().end() );
			}
		} else {
			for ( Type::ForallList::const_iterator i = type->forall.begin(); i != type->forall.end(); ++i ) {
				closedVars[ (*i)->get_name() ] = TypeDecl::Data{ (*i) };
				for ( std::list< DeclarationWithType* >::const_iterator assert = (*i)->get_assertions().begin(); assert != (*i)->get_assertions().end(); ++assert ) {
					haveAssertions[ *assert ].isUsed = false;
				}
///       cloneAll( (*i)->get_assertions(), haveAssertions );
///       haveAssertions.insert( haveAssertions.end(), (*i)->get_assertions().begin(), (*i)->get_assertions().end() );
			} // for
		} // if
///   std::cerr << "type is ";
///   type->print( std::cerr );
///   std::cerr << std::endl << "need is" << std::endl;
///   printAssertionSet( needAssertions, std::cerr );
///   std::cerr << std::endl << "have is" << std::endl;
///   printAssertionSet( haveAssertions, std::cerr );
	}

	void FindOpenVars_old::previsit(const PointerType * pointerType) {
		common_action( pointerType );
	}

	void FindOpenVars_old::previsit(const ArrayType * arrayType) {
		common_action( arrayType );
	}

	void FindOpenVars_old::previsit(const FunctionType * functionType) {
		common_action( functionType );
		nextIsOpen = ! nextIsOpen;
		GuardAction( [this](){ nextIsOpen = ! nextIsOpen; } );
	}

	void FindOpenVars_old::previsit(const TupleType * tupleType) {
		common_action( tupleType );
	}

	namespace {
		struct FindOpenVars_new final : public ast::WithGuards {
			ast::OpenVarSet & open;
			ast::OpenVarSet & closed;
			ast::AssertionSet & need;
			ast::AssertionSet & have;
			bool nextIsOpen;

			FindOpenVars_new(
				ast::OpenVarSet & o, ast::OpenVarSet & c, ast::AssertionSet & n,
				ast::AssertionSet & h, FirstMode firstIsOpen )
			: open( o ), closed( c ), need( n ), have( h ), nextIsOpen( firstIsOpen ) {}

			void previsit( const ast::FunctionType * type ) {
				// mark open/closed variables
				if ( nextIsOpen ) {
					for ( auto & decl : type->forall ) {
						open[ *decl ] = ast::TypeData{ decl->base };
					}
					for ( auto & assert : type->assertions ) {
						need[ assert ].isUsed = false;
					}
				} else {
					for ( auto & decl : type->forall ) {
						closed[ *decl ] = ast::TypeData{ decl->base };
					}
					for ( auto & assert : type->assertions ) {
						have[ assert ].isUsed = false;
					}
				}

				// flip open variables for contained function types
				nextIsOpen = ! nextIsOpen;
				GuardAction( [this](){ nextIsOpen = ! nextIsOpen; } );
			}

		};
	}

	void findOpenVars(
			const ast::Type * type, ast::OpenVarSet & open, ast::OpenVarSet & closed,
			ast::AssertionSet & need, ast::AssertionSet & have, FirstMode firstIsOpen ) {
		ast::Pass< FindOpenVars_new > finder{ open, closed, need, have, firstIsOpen };
		type->accept( finder );
	}
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
