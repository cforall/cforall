#include "TranslateEnumRange.hpp"

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"

namespace ControlStruct {

namespace {

struct TranslateEnumRangeCore {
	const ast::Stmt * postvisit( const ast::ForStmt * stmt );
};

const ast::Stmt * TranslateEnumRangeCore::postvisit( const ast::ForStmt * stmt ) {
	if ( !stmt->range_over ) return stmt;
	auto mutStmt = ast::mutate( stmt );
	auto & location = mutStmt->location;

	if ( auto declStmt = mutStmt->inits.front().as<ast::DeclStmt>() ) {
		if ( auto objDecl = declStmt->decl.as<ast::ObjectDecl>() ) {
			if ( !objDecl->init ) {
				ast::SingleInit * newInit = new ast::SingleInit( location,
					ast::UntypedExpr::createCall( location,
						mutStmt->is_inc ? "lowerBound" : "upperBound", {} ),
					ast::ConstructFlag::MaybeConstruct
				);
				auto objDeclWithInit = ast::mutate_field( objDecl, &ast::ObjectDecl::init, newInit );
				auto declWithInit = ast::mutate_field( declStmt, &ast::DeclStmt::decl, objDeclWithInit );
				mutStmt->inits[0] = declWithInit;
			}
		}
	}

	auto declStmt = mutStmt->inits.front().strict_as<ast::DeclStmt>();
	auto initDecl = declStmt->decl.strict_as<ast::ObjectDecl>();
	auto indexName = initDecl->name;

	// Both inc and dec check if the current posn less than the number of enumerator
	// for dec, it keeps call pred until it passes 0 (the first enumerator) and underflow,
	// it wraps around and become unsigned max
	ast::UntypedExpr * condition = ast::UntypedExpr::createCall( location,
		mutStmt->is_inc ? "?<=?" : "?>=?",
		{
			new ast::NameExpr( location, indexName ),
			ast::UntypedExpr::createCall( location,
				mutStmt->is_inc ? "upperBound" : "lowerBound", {} )
		} );
	auto increment = ast::UntypedExpr::createCall( location,
		mutStmt->is_inc ? "succ_unsafe" : "pred_unsafe",
		{ new ast::NameExpr( location, indexName ) } );
	auto assig = ast::UntypedExpr::createAssign( location,
		new ast::NameExpr( location, indexName ), increment );
	mutStmt->cond = condition;
	mutStmt->inc = assig;
	return mutStmt;
}

} // namespace

void translateEnumRange( ast::TranslationUnit & translationUnit ) {
	ast::Pass<TranslateEnumRangeCore>::run( translationUnit );
}

} // namespace ControlStruct
