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
#include "Common/PassVisitor.h"   // for PassVisitor
#include "Common/utility.h"       // for copy
#include "InitTweak/InitTweak.h"  // for isConstExpr
#include "RenameVars.h"
#include "Resolver.h"  // for resolveInVoidContext
#include "SymTab/Mangler.h"
#include "SynTree/Expression.h"  // for Expression
#include "SynTree/Mutator.h"     // for Mutator
#include "SynTree/Type.h"        // for TypeofType, Type

namespace SymTab {
class Indexer;
}  // namespace SymTab

namespace ResolvExpr {
namespace {
#if 0
		void
		printAlts( const AltList &list, std::ostream &os, int indent = 0 )
		{
			for ( AltList::const_iterator i = list.begin(); i != list.end(); ++i ) {
				i->print( os, indent );
				os << std::endl;
			}
		}
#endif
	}

class ResolveTypeof_old : public WithShortCircuiting {
   public:
		ResolveTypeof_old( const SymTab::Indexer &indexer ) : indexer( indexer ) {}
		void premutate( TypeofType *typeofType );
		Type * postmutate( TypeofType *typeofType );

   private:
    const SymTab::Indexer &indexer;
};

	Type * resolveTypeof( Type *type, const SymTab::Indexer &indexer ) {
		PassVisitor<ResolveTypeof_old> mutator( indexer );
		return type->acceptMutator( mutator );
	}

	void ResolveTypeof_old::premutate( TypeofType * ) {
		visit_children = false;
	}

    Type * ResolveTypeof_old::postmutate( TypeofType *typeofType ) {
#if 0
		std::cerr << "resolving typeof: ";
		typeofType->print( std::cerr );
		std::cerr << std::endl;
#endif
    // pass on null expression
		if ( ! typeofType->expr ) return typeofType;

    bool isBasetypeof = typeofType->is_basetypeof;
    auto oldQuals = typeofType->get_qualifiers().val;

    Type* newType;
		if ( TypeExpr* tyExpr = dynamic_cast<TypeExpr*>(typeofType->expr) ) {
        // typeof wrapping type
        newType = tyExpr->type;
        tyExpr->type = nullptr;
        delete tyExpr;
    } else {
        // typeof wrapping expression
			Expression * newExpr = resolveInVoidContext( typeofType->expr, indexer );
			assert( newExpr->result && ! newExpr->result->isVoid() );
        newType = newExpr->result;
        newExpr->result = nullptr;
        delete typeofType;
        delete newExpr;
    }

    // clear qualifiers for base, combine with typeoftype quals in any case
    if ( isBasetypeof ) {
			// replace basetypeof(<enum>) by int
			if ( dynamic_cast<EnumInstType*>(newType) ) {
				Type* newerType =
					new BasicType{ newType->get_qualifiers(), BasicType::SignedInt,
					newType->attributes };
				delete newType;
				newType = newerType;
			}
			newType->get_qualifiers().val
				= ( newType->get_qualifiers().val & ~Type::Qualifiers::Mask ) | oldQuals;
		} else {
        newType->get_qualifiers().val |= oldQuals;
    }

    return newType;
}

namespace {
struct ResolveTypeof_new : public ast::WithShortCircuiting {
    const ResolveContext & context;

		ResolveTypeof_new( const ResolveContext & context ) :
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
	ast::Pass< ResolveTypeof_new > mutator( context );
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
            ast::Designation * newDesination = new ast::Designation(des->location);

            if (des->designators.size() == 0) continue;

            // The designator I want to replace
            const ast::Expr * designator = des->designators.at(0);

            if ( const ast::NameExpr * designatorName = dynamic_cast<const ast::NameExpr *>(designator) ) {
                auto candidates = context.symtab.lookupId(designatorName->name);
                // Does not work for the overloading case currently
                // assert( candidates.size() == 1 );
                if ( candidates.size() != 1 ) return mutDecl;
                auto candidate = candidates.at(0);
                if ( const ast::EnumInstType * enumInst = dynamic_cast<const ast::EnumInstType *>(candidate.id->get_type()) ) {
                    // determine that is an enumInst, swap it with its const value
                    assert( candidates.size() == 1 );
                    const ast::EnumDecl * baseEnum = enumInst->base;
                    // Need to iterate over all enum value to find the initializer to swap
                    for ( size_t m = 0; m < baseEnum->members.size(); ++m ) {
                        if ( baseEnum->members.at(m)->name == designatorName->name ) {
                            const ast::ObjectDecl * mem = baseEnum->members.at(m).as<const ast::ObjectDecl>();
                            assert(mem);
                            if ( mem->init ) {
                                const ast::SingleInit * memInit = mem->init.as<const ast::SingleInit>();
                                ast::Expr * initValue = shallowCopy( memInit->value.get() );
                                newDesination->designators.push_back( initValue );
                            } else {
                                SemanticError(des->location, "TODO: Enum Array Designation with no explicit value is not implemented");
                            }
                        }
                    }
                    if ( newDesination->designators.size() == 0 ) {
                        SemanticError(des->location, "Resolution Error: Resolving array designation as Enum Instance value, but cannot find a desgination value");
                    }
                } else {
                    newDesination->designators.push_back( des->designators.at(0) );
                }
            } else {
                newDesination->designators.push_back( des->designators.at(0) );
            }
            mutListInit = ast::mutate_field_index(mutListInit, &ast::ListInit::designations, k, newDesination);
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
