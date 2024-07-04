#include "TranslateEnumRange.hpp"

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"

namespace ControlStruct {

struct addInitType {
    const ast::Stmt * postvisit( const ast::ForStmt * stmt );
};

struct addInit {
    const ast::Stmt * postvisit( const ast::ForStmt * stmt );
};

struct translateEnumRangeCore {
    const ast::Stmt * postvisit( const ast::ForStmt * stmt );
};

const ast::Stmt* addInitType::postvisit( const ast::ForStmt * stmt ) {
    if ( stmt->range_over ) {
        auto typeExpr = stmt->range_over.strict_as<ast::TypeExpr>();
        auto type = typeExpr->type;

        auto objInit = stmt->inits.front();
        assert( stmt->inits.size() == 1 );

        if (auto declStmt = objInit.as<ast::DeclStmt>()) {
            auto decl = declStmt->decl;
            if ( auto objDecl = decl.as<ast::ObjectDecl>()) {
                if ( !objDecl->type && type ) {
                    auto objDeclWithType = ast::mutate_field( objDecl, &ast::ObjectDecl::type, type );
                    auto declWithType = ast::mutate_field( declStmt, &ast::DeclStmt::decl, objDeclWithType );
                    stmt = ast::mutate_field_index( stmt, &ast::ForStmt::inits, 0, declWithType );
                }
            }
        }
    }
    return stmt;
}

const ast::Stmt* addInit::postvisit( const ast::ForStmt * stmt ) {
    if ( stmt->range_over ) {
        auto inits = stmt->inits;
        auto init = stmt->inits.front();

        if (auto declStmt = init.as<ast::DeclStmt>()) {
            auto decl = declStmt->decl;
            if ( auto objDecl = decl.as<ast::ObjectDecl>()) {
                if ( !objDecl->init ) {
                    auto location = stmt->location;
                    ast::SingleInit * newInit = new ast::SingleInit( location, 
                        stmt->is_inc?
                        ast::UntypedExpr::createCall( location, "lowerBound", {} ):
                        ast::UntypedExpr::createCall( location, "upperBound", {} ),
                        ast::ConstructFlag::MaybeConstruct
                    );
                    auto objDeclWithInit = ast::mutate_field( objDecl, &ast::ObjectDecl::init, newInit );
                    auto declWithInit = ast::mutate_field( declStmt, &ast::DeclStmt::decl, objDeclWithInit );
                    stmt = ast::mutate_field_index( stmt, &ast::ForStmt::inits, 0, declWithInit );
                }
            }
        }
    }
    return stmt;
}

const ast::Stmt* translateEnumRangeCore::postvisit( const ast::ForStmt * stmt ) {
    if ( !stmt->range_over ) return stmt;
    auto location = stmt->location;
    auto declStmt = stmt->inits.front().strict_as<ast::DeclStmt>();
    auto initDecl = declStmt->decl.strict_as<ast::ObjectDecl>();
    auto indexName = initDecl->name;

    // Both inc and dec check if the current posn less than the number of enumerator
    // for dec, it keeps call pred until it passes 0 (the first enumerator) and underflow,
    // it wraps around and become unsigned max
    ast::UntypedExpr * condition = ast::UntypedExpr::createCall( location,
        "?<=?",
        {   new ast::NameExpr( location, indexName ),
            ast::UntypedExpr::createCall( location, "upperBound", {} )  });
    auto increment = ast::UntypedExpr::createCall( location, 
        stmt->is_inc? "succ": "pred",
        { new ast::NameExpr( location, indexName ) });
    auto assig = ast::UntypedExpr::createAssign( location, new ast::NameExpr( location, indexName ), increment );
    auto mut = ast::mutate_field( stmt, &ast::ForStmt::cond, condition );
    mut = ast::mutate_field(stmt, &ast::ForStmt::inc, assig );
    return mut;
}

void translateEnumRange( ast::TranslationUnit & translationUnit ) {
    ast::Pass<addInit>::run( translationUnit );
    ast::Pass<translateEnumRangeCore>::run( translationUnit );
}
}
