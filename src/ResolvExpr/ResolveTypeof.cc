//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolveTypeof.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 12:12:20 2015
// Last Modified By : Andrew Beach
// Last Modified On : Wed Mar 16 16:09:00 2022
// Update Count     : 4
//

#include "ResolveTypeof.h"

#include <cassert>  // for assert

#include "AST/CVQualifiers.hpp"
#include "AST/Node.hpp"
#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"
#include "AST/Type.hpp"
#include "AST/TypeEnvironment.hpp"
#include "Common/utility.h"       // for copy
#include "InitTweak/InitTweak.h"  // for isConstExpr
#include "RenameVars.h"
#include "Resolver.h"  // for resolveInVoidContext
#include "SymTab/Mangler.h"

namespace ResolvExpr {

namespace {
struct ResolveTypeof : public ast::WithShortCircuiting {
    const ResolveContext & context;

		ResolveTypeof( const ResolveContext & context ) :
			context( context ) {}

		void previsit( const ast::TypeofType * ) { visit_children = false; }

        const ast::Type * postvisit( const ast::TypeofType * typeofType ) {
        // pass on null expression
            if ( ! typeofType->expr ) return typeofType;

            ast::ptr< ast::Type > newType;
            if ( auto tyExpr = typeofType->expr.as< ast::TypeExpr >() ) {
            // typeof wrapping type
            newType = tyExpr->type;
        } else {
            // typeof wrapping expression
            ast::TypeEnvironment dummy;
            ast::ptr< ast::Expr > newExpr =
                resolveInVoidContext( typeofType->expr, context, dummy );
            assert( newExpr->result && ! newExpr->result->isVoid() );
            newType = newExpr->result;
        }

        // clear qualifiers for base, combine with typeoftype quals regardless
        if ( typeofType->kind == ast::TypeofType::Basetypeof ) {
            // replace basetypeof(<enum>) by int
				if ( newType.as< ast::EnumInstType >() ) {
					newType = new ast::BasicType{
						ast::BasicType::SignedInt, newType->qualifiers, copy(newType->attributes) };
            }
				reset_qualifiers(
					newType,
					( newType->qualifiers & ~ast::CV::EquivQualifiers ) | typeofType->qualifiers );
        } else {
				add_qualifiers( newType, typeofType->qualifiers );
        }

        return newType.release();
    }
};
} // anonymous namespace

const ast::Type * resolveTypeof( const ast::Type * type , const ResolveContext & context ) {
	ast::Pass< ResolveTypeof > mutator( context );
	return type->accept( mutator );
}

struct FixArrayDimension {
	const ResolveContext & context;
	FixArrayDimension(const ResolveContext & context) : context( context ) {}

	const ast::ArrayType * previsit (const ast::ArrayType * arrayType) {
		if (!arrayType->dimension) return arrayType;
		auto mutType = mutate(arrayType);
		auto globalSizeType = context.global.sizeType;
		ast::ptr<ast::Type> sizetype = globalSizeType ? globalSizeType : new ast::BasicType(ast::BasicType::LongUnsignedInt);
		mutType->dimension = findSingleExpression(arrayType->dimension, sizetype, context );

		if (InitTweak::isConstExpr(mutType->dimension)) {
			mutType->isVarLen = ast::LengthFlag::FixedLen;
		}
		else {
			mutType->isVarLen = ast::LengthFlag::VariableLen;
		}
		return mutType;
	}
};

const ast::Type * fixArrayType( const ast::Type * type, const ResolveContext & context ) {
	ast::Pass<FixArrayDimension> visitor(context);
	return type->accept(visitor);
}

const ast::ObjectDecl * fixObjectType( const ast::ObjectDecl * decl , const ResolveContext & context ) {
    if (decl->isTypeFixed) {
        return decl;
    }

    auto mutDecl = mutate(decl);
    fixObjectInit(decl, context);
    {
        auto resolvedType = resolveTypeof(decl->type, context);
        resolvedType = fixArrayType(resolvedType, context);
        mutDecl->type = resolvedType;
    }

    // Do not mangle unnamed variables.
    if (!mutDecl->name.empty()) {
        mutDecl->mangleName = Mangle::mangle(mutDecl);
    }

    mutDecl->type = renameTyVars(mutDecl->type, RenameMode::GEN_EXPR_ID);
    mutDecl->isTypeFixed = true;
    return mutDecl;
}

const ast::ObjectDecl *fixObjectInit(const ast::ObjectDecl *decl,
                                     const ResolveContext &context) {
    if (decl->isTypeFixed) {
        return decl;
    }

    auto mutDecl = mutate(decl);

    if ( auto mutListInit = mutDecl->init.as<ast::ListInit>() ) {
        // std::list<ast::Designation *> newDesignations;        

        for ( size_t k = 0; k < mutListInit->designations.size(); k++ ) {
            const ast::Designation *des = mutListInit->designations[k].get();
            // Desination here
            ast::Designation * newDesignation = new ast::Designation(des->location);
            std::deque<ast::ptr<ast::Expr>> newDesignators;

            for ( ast::ptr<ast::Expr> designator : des->designators ) {
                // Stupid flag variable for development, to be removed
                // bool mutated = false;
                if ( const ast::NameExpr * designatorName = designator.as<ast::NameExpr>() ) {
                    auto candidates = context.symtab.lookupId(designatorName->name);
                    // Does not work for the overloading case currently
                    // assert( candidates.size() == 1 );
                    if ( candidates.size() != 1 ) return mutDecl;
                    auto candidate = candidates.at(0);
                    if ( const ast::EnumInstType * enumInst = dynamic_cast<const ast::EnumInstType *>(candidate.id->get_type())) {
                        // determine that is an enumInst, swap it with its const value
                        assert( candidates.size() == 1 );
                        const ast::EnumDecl * baseEnum = enumInst->base;
                        // Need to iterate over all enum value to find the initializer to swap
                        for ( size_t m = 0; m < baseEnum->members.size(); ++m ) {
                            const ast::ObjectDecl * mem = baseEnum->members.at(m).as<const ast::ObjectDecl>();
                            if ( baseEnum->members.at(m)->name == designatorName->name ) {
                                assert(mem);
                                newDesignators.push_back( ast::ConstantExpr::from_int(designator->location, m) );
                                // mutated = true;
                                break;
                            }
                        }
                    } else {
                        newDesignators.push_back( des->designators.at(0) );
                    }
                } else {
                    newDesignators.push_back( des->designators.at(0) );
                }
            }            
            
            newDesignation->designators = newDesignators;
            mutListInit = ast::mutate_field_index(mutListInit, &ast::ListInit::designations, k, newDesignation);
            
        }
    }
    return mutDecl;
}

}  // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
