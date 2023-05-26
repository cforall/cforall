//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Explode.cc --
//
// Author           : Rob Schluntz
// Created On       : Wed Nov 9 13:12:24 2016
// Last Modified By : Andrew Beach
// Last Modified On : Wed Jun 12 16:40:00 2016
// Update Count     : 3
//

#include "Explode.h"
#include <list>                  // for list

#include "AST/Pass.hpp"          // for Pass
#include "SynTree/Mutator.h"     // for Mutator
#include "Common/PassVisitor.h"  // for PassVisitor

namespace Tuples {
	namespace {
		// remove one level of reference from a reference type -- may be useful elsewhere.
		Type * getReferenceBase( Type * t ) {
			if ( ReferenceType * refType = dynamic_cast<ReferenceType *>( t ) ) {
				return refType->get_base();
			} else {
				// for the moment, I want to know immediately if a non-reference type is ever passed in here.
				assertf( false, "getReferenceBase for non-ref: %s", toString( refType ).c_str() );
				return nullptr;
			}
		}

		struct CastExploder {
			bool castAdded = false;
			bool foundUniqueExpr = false;
			Expression * applyCast( Expression * expr, bool first = true ) {
				if ( TupleExpr * tupleExpr = dynamic_cast< TupleExpr * >( expr ) ){
					foundUniqueExpr = true;
					std::list< Expression * > exprs;
					for ( Expression *& expr : tupleExpr->get_exprs() ) {
						// move cast into tuple exprs
						exprs.push_back( applyCast( expr, false ) );
					}
					// want the top-level expression to be cast to reference type, but not nested
					// tuple expressions
					if ( first ) {
						castAdded = true;
						Expression * tupleExpr = new TupleExpr( exprs );
						return new CastExpr( tupleExpr, new ReferenceType( Type::Qualifiers(), tupleExpr->result->clone() ) );
					} else {
						return new TupleExpr( exprs );
					}
				}
				if ( dynamic_cast<ReferenceType*>( expr->result ) ) {
					// don't need to cast reference type to another reference type
					return expr->clone();
				} else {
					// anything else should be cast to reference as normal
					castAdded = true;
					return new CastExpr( expr->clone(), new ReferenceType( Type::Qualifiers(), expr->result->clone() ) );
				}
			}

			Expression * postmutate( UniqueExpr * uniqueExpr ) {
				// move cast into unique expr so that the unique expr has type T& rather than
				// type T. In particular, this transformation helps with generating the
				// correct code for reference-cast member tuple expressions, since the result
				// should now be a tuple of references rather than a reference to a tuple.
				// Still, this code is a bit awkward, and could use some improvement.
				UniqueExpr * newUniqueExpr = new UniqueExpr( applyCast( uniqueExpr->get_expr() ), uniqueExpr->get_id() );
				delete uniqueExpr;
				if ( castAdded ) {
					// if a cast was added by applyCast, then unique expr now has one more layer of reference
					// than it had coming into this function. To ensure types still match correctly, need to cast
					//  to reference base so that outer expressions are still correct.
					castAdded = false;
					Type * toType = getReferenceBase( newUniqueExpr->result );
					return new CastExpr( newUniqueExpr, toType->clone() );
				}
				return newUniqueExpr;
			}


			Expression * postmutate( TupleIndexExpr * tupleExpr ) {
				// tuple index expr needs to be rebuilt to ensure that the type of the
				// field is consistent with the type of the tuple expr, since the field
				// may have changed from type T to T&.
				Expression * expr = tupleExpr->get_tuple();
				tupleExpr->set_tuple( nullptr );
				TupleIndexExpr * ret = new TupleIndexExpr( expr, tupleExpr->get_index() );
				delete tupleExpr;
				return ret;
			}
		};
	} // namespace

	Expression * distributeReference( Expression * expr ) {
		PassVisitor<CastExploder> exploder;
		expr = expr->acceptMutator( exploder );
		if ( ! exploder.pass.foundUniqueExpr ) {
			// if a UniqueExpr was found, then the cast has already been added inside the UniqueExpr as appropriate
			expr = new CastExpr( expr, new ReferenceType( Type::Qualifiers(), expr->result->clone() ) );
		}
		return expr;
	}

namespace {

// Remove one level of reference from a reference type.
const ast::Type * getReferenceBase( const ast::Type * t ) {
	if ( const ast::ReferenceType * ref = dynamic_cast< const ast::ReferenceType * >( t ) ) {
		return ref->base;
	} else {
		assertf( false, "getReferenceBase for non-ref: %s", toString( t ).c_str() );
		return nullptr;
	}
}

struct CastExploderCore {
	bool castAdded = false;
	bool foundUniqueExpr = false;
	const ast::Expr * applyCast( const ast::Expr * expr, bool first = true ) {
		// On tuple push the cast down.
		if ( const ast::TupleExpr * tupleExpr = dynamic_cast< const ast::TupleExpr * >( expr ) ) {
			foundUniqueExpr = true;
			std::vector< ast::ptr< ast::Expr > > exprs;
			for ( const ast::Expr * expr : tupleExpr->exprs ) {
				exprs.emplace_back( applyCast( expr, false ) );
			}
			if ( first ) {
				castAdded = true;
				const ast::Expr * tuple = new ast::TupleExpr{
					tupleExpr->location, std::move( exprs ) };
				return new ast::CastExpr{ tuple, new ast::ReferenceType{ tuple->result } };
			} else {
				return new ast::TupleExpr( tupleExpr->location, std::move( exprs ) );
			}
		}
		if ( dynamic_cast< const ast::ReferenceType * >( expr->result.get() ) ) {
			return expr;
		} else {
			castAdded = true;
			return new ast::CastExpr{ expr, new ast::ReferenceType{ expr->result } };
		}
	}

	const ast::Expr * postvisit( const ast::UniqueExpr * node ) {
		// move cast into unique expr so that the unique expr has type T& rather than
		// type T. In particular, this transformation helps with generating the
		// correct code for reference-cast member tuple expressions, since the result
		// should now be a tuple of references rather than a reference to a tuple.
		// Still, this code is a bit awkward, and could use some improvement.
		const ast::UniqueExpr * newNode = new ast::UniqueExpr( node->location,
				applyCast( node->expr ), node->id );
		if ( castAdded ) {
			// if a cast was added by applyCast, then unique expr now has one more layer of reference
			// than it had coming into this function. To ensure types still match correctly, need to cast
			//  to reference base so that outer expressions are still correct.
			castAdded = false;
			const ast::Type * newType = getReferenceBase( newNode->result );
			return new ast::CastExpr{ newNode->location, newNode, newType };
		}
		return newNode;
	}

	const ast::Expr * postvisit( const ast::TupleIndexExpr * tupleExpr ) {
		// tuple index expr needs to be rebuilt to ensure that the type of the
		// field is consistent with the type of the tuple expr, since the field
		// may have changed from type T to T&.
		return new ast::TupleIndexExpr( tupleExpr->location, tupleExpr->tuple, tupleExpr->index );
	}
};

} // namespace

const ast::Expr * distributeReference( const ast::Expr * expr ) {
	ast::Pass<CastExploderCore> exploder;
	expr = expr->accept( exploder );
	if ( ! exploder.core.foundUniqueExpr ) {
		expr = new ast::CastExpr{ expr, new ast::ReferenceType{ expr->result } };
	}
	return expr;
}

} // namespace Tuples

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
