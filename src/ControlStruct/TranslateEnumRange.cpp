#include "TranslateEnumRange.hpp"

#include "AST/Pass.hpp"
#include "AST/TranslationUnit.hpp"

namespace ControlStruct {

namespace {

struct TranslateEnumRangeCore {
	const ast::Stmt * postvisit( const ast::ForeachStmt * stmt );
};

const ast::Stmt * TranslateEnumRangeCore::postvisit( const ast::ForeachStmt * stmt ) {
	auto & location = stmt->location;

	assert( stmt->inits.size() == 1 );
	ast::DeclStmt const * initialize = stmt->inits.front().strict_as<ast::DeclStmt>();

	auto objDecl = initialize->decl.strict_as<ast::ObjectDecl>();
	if ( !objDecl->init ) {
		ast::SingleInit * init = new ast::SingleInit( location,
			ast::UntypedExpr::createCall( location,
				stmt->isIncreasing ? "lowerBound" : "upperBound", {} ),
			ast::ConstructFlag::MaybeConstruct
		);
		objDecl = ast::mutate_field( objDecl, &ast::ObjectDecl::init, init );
		initialize = ast::mutate_field( initialize, &ast::DeclStmt::decl, objDecl );
	}

	auto indexName = objDecl->name;

	// Both inc and dec check if the current posn less than the number of enumerator
	// for dec, it keeps call pred until it passes 0 (the first enumerator) and underflow,
	// it wraps around and become unsigned max
	ast::UntypedExpr * condition = ast::UntypedExpr::createCall( location,
		stmt->isIncreasing ? "?<=?" : "?>=?",
		{
			new ast::NameExpr( location, indexName ),
			ast::UntypedExpr::createCall( location,
				stmt->isIncreasing ? "upperBound" : "lowerBound", {} )
		} );
	ast::UntypedExpr * increment = ast::UntypedExpr::createAssign( location,
		new ast::NameExpr( location, indexName ),
		ast::UntypedExpr::createCall( location,
			stmt->isIncreasing ? "succ_unsafe" : "pred_unsafe",
			{ new ast::NameExpr( location, indexName ) } ) );

	return new ast::ForStmt(
		stmt->location,
		{ initialize },
		condition,
		increment,
		stmt->body,
		stmt->else_,
		copy( stmt->labels )
	);
}

} // namespace

void translateEnumRange( ast::TranslationUnit & translationUnit ) {
	ast::Pass<TranslateEnumRangeCore>::run( translationUnit );
}

} // namespace ControlStruct
