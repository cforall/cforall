#include "AST/Decl.hpp"
#include "AST/Pass.hpp"
#include "AST/Stmt.hpp"
#include "AST/Inspect.hpp"
#include "Common/utility.h"
#include "ReplacePseudoFunc.hpp"

namespace Validate {

namespace {

struct ReplacePseudoFuncCore {
    ast::Expr const * postvisit( ast::ApplicationExpr const * decl );
};
}

ast::Expr const * ReplacePseudoFuncCore::postvisit( ast::ApplicationExpr const * expr) {
    auto fname = ast::getFunctionName( expr );
    if ( fname == "posE" ) {
        // std::cerr << "Found App in ReplacePseudoFunc" << std::endl;
        if ( expr->args.size() != 1 ) {
            SemanticError( expr, "Position Expression only take one parameter" );
        }
        const ast::VariableExpr * arg = expr->args.front().as<const ast::VariableExpr>();
        if ( !arg ) {
            SemanticError( expr, "Unimplement Pseudo Function Cases" );
        }
        const ast::ObjectDecl * argAsVar = arg->var.as<const ast::ObjectDecl>();
        const std::string referredName = argAsVar->name;
        const ast::EnumInstType * argType = argAsVar->type.as<const ast::EnumInstType>();
        if ( !argType ) {
            SemanticError( argAsVar, "Position can only be used on an enumeration instance" );
        }
        const ast::EnumDecl * base = argType->base;
        for ( size_t i = 0; i < base->members.size(); i++ ) {
            if ( base->members[i]->name == referredName ) {
                return ast::ConstantExpr::from_int( expr->location, i );
            }
        }
    }
    return expr;
}



void replacePseudoFunc( ast::TranslationUnit & translationUnit ) {
    ast::Pass<ReplacePseudoFuncCore>::run( translationUnit );
}

}