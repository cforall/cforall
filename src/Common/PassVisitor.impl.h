#pragma once
// IWYU pragma: private, include "PassVisitor.h"

#define VISIT_START( node )                                     \
	__attribute__((unused))                                   \
	ChildrenGuard children_guard( get_visit_children_ptr() ); \
	__attribute__((unused))                                   \
	guard_value_impl guard( at_cleanup_impl(pass, 0) );       \
	call_previsit( node );                                    \

#define VISIT_END( node )                       \
	call_postvisit( node );                   \

#define MUTATE_START( node )                                    \
	__attribute__((unused))                                   \
	ChildrenGuard children_guard( get_visit_children_ptr() ); \
	__attribute__((unused))                                   \
	guard_value_impl guard( at_cleanup_impl(pass, 0) );       \
	call_premutate( node );                                   \

#define MUTATE_END( type, node )                \
	auto __return = call_postmutate< type * >( node ); \
	assert( __return ); \
	return __return;


template<typename T>
static inline bool empty( T * ptr ) {
	return !ptr || ptr->empty();
}

typedef std::list< Statement   * > StmtList_t;
typedef std::list< Declaration * > DeclList_t;

template<typename iterator_t>
static inline void splice( iterator_t it, DeclList_t * decls ) {
	std::transform(
		decls->begin(),
		decls->end(),
		it,
		[](Declaration * decl) -> auto {
			return new DeclStmt( decl );
		}
	);
	decls->clear();
}

template< typename pass_type >
inline void acceptAll( std::list< Declaration* > &decls, PassVisitor< pass_type >& visitor ) {
	DeclList_t* beforeDecls = visitor.get_beforeDecls();
	DeclList_t* afterDecls  = visitor.get_afterDecls();
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( std::list< Declaration* >::iterator i = decls.begin(); ; ++i ) {


		// splice in new declarations after previous decl
		if ( !empty( afterDecls ) ) { decls.splice( i, *afterDecls ); }

		if ( i == decls.end() ) break;

		try {
			// run visitor on declaration
			maybeAccept_impl( *i, visitor );
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		}

		// splice in new declarations before current decl
		if ( !empty( beforeDecls ) ) { decls.splice( i, *beforeDecls ); }
	}
	pass_visitor_stats.depth--;
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

template< typename pass_type >
inline void acceptAll( const std::list< const Declaration * > & decls, PassVisitor< pass_type >& visitor ) {
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( const Declaration * decl : decls ) {
		try {
			// run visitor on declaration
			maybeAccept_impl( decl, visitor );
		}
		catch( SemanticErrorException &e ) {
			errors.append( e );
		}
	}
	pass_visitor_stats.depth--;
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

template< typename pass_type >
inline void mutateAll( std::list< Declaration* > &decls, PassVisitor< pass_type >& mutator ) {
	DeclList_t* beforeDecls = mutator.get_beforeDecls();
	DeclList_t* afterDecls  = mutator.get_afterDecls();
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( std::list< Declaration* >::iterator i = decls.begin(); ; ++i ) {
		// splice in new declarations after previous decl
		if ( !empty( afterDecls ) ) { decls.splice( i, *afterDecls ); }

		if ( i == decls.end() ) break;
		try {
			// run mutator on declaration
			maybeMutate_impl( *i, mutator );
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		}

		// splice in new declarations before current decl
		if ( !empty( beforeDecls ) ) { decls.splice( i, *beforeDecls ); }
	}
	pass_visitor_stats.depth--;
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

template< typename TreeType, typename pass_type >
inline void maybeAccept_impl( TreeType * tree, PassVisitor< pass_type > & visitor ) {
	if ( ! visitor.get_visit_children() ) return;
	if ( tree ) {
		tree->accept( visitor );
	}
}

template< typename TreeType, typename pass_type >
inline void maybeAccept_impl( const TreeType * tree, PassVisitor< pass_type > & visitor ) {
	if ( ! visitor.get_visit_children() ) return;
	if ( tree ) {
		tree->accept( visitor );
	}
}

template< typename Container, typename pass_type >
inline void maybeAccept_impl( Container & container, PassVisitor< pass_type > & visitor ) {
	if ( ! visitor.get_visit_children() ) return;
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( typename Container::iterator i = container.begin(); i != container.end(); ++i ) {
		try {
			if ( *i ) {
				(*i)->accept( visitor );
			}
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		}
	}
	pass_visitor_stats.depth--;
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

template< typename Container, typename pass_type >
inline void maybeAccept_impl( const Container & container, PassVisitor< pass_type > & visitor ) {
	if ( ! visitor.get_visit_children() ) return;
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( const auto & i : container ) {
		try {
			if ( i ) {
				i->accept( visitor );
			}
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		}
	}
	pass_visitor_stats.depth--;
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

template< typename TreeType, typename pass_type >
inline void maybeMutate_impl( TreeType *& tree, PassVisitor< pass_type > & mutator ) {
	if ( ! mutator.get_visit_children() ) return;

	if ( tree ) {
		tree = strict_dynamic_cast< TreeType * >( tree->acceptMutator( mutator ) );
	}
}

template< typename Container, typename pass_type >
inline void maybeMutate_impl( Container & container, PassVisitor< pass_type > & mutator ) {

	if ( ! mutator.get_visit_children() ) return;
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( typename Container::iterator i = container.begin(); i != container.end(); ++i ) {
		try {
			if ( *i ) {
				*i = dynamic_cast< typename Container::value_type >( (*i)->acceptMutator( mutator ) );
				assert( *i );
			} // if
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		} // try
	} // for
	pass_visitor_stats.depth--;
	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
}

template< typename pass_type >
template< typename func_t >
void PassVisitor< pass_type >::handleStatementList( std::list< Statement * > & statements, func_t func ) {
	if ( ! get_visit_children() ) return;
	SemanticErrorException errors;

	// don't want statements from outer CompoundStmts to be added to this CompoundStmt
	ValueGuardPtr< StmtList_t > oldBeforeStmts( get_beforeStmts() );
	ValueGuardPtr< StmtList_t > oldAfterStmts ( get_afterStmts () );
	ValueGuardPtr< DeclList_t > oldBeforeDecls( get_beforeDecls() );
	ValueGuardPtr< DeclList_t > oldAfterDecls ( get_afterDecls () );

	StmtList_t* beforeStmts = get_beforeStmts();
	StmtList_t* afterStmts  = get_afterStmts();
	DeclList_t* beforeDecls = get_beforeDecls();
	DeclList_t* afterDecls  = get_afterDecls();

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( std::list< Statement* >::iterator i = statements.begin(); i != statements.end(); ++i ) {

		if ( !empty( afterDecls ) ) { splice( std::inserter( statements, i ), afterDecls ); }
		if ( !empty( afterStmts ) ) { statements.splice( i, *afterStmts ); }

		try {
			func( *i );
			assert( *i );
			assert(( empty( beforeStmts ) && empty( afterStmts ))
			    || ( empty( beforeDecls ) && empty( afterDecls )) );

		} catch ( SemanticErrorException &e ) {
			errors.append( e );
		}

		if ( !empty( beforeDecls ) ) { splice( std::inserter( statements, i ), beforeDecls ); }
		if ( !empty( beforeStmts ) ) { statements.splice( i, *beforeStmts ); }
	}
	pass_visitor_stats.depth--;

	if ( !empty( afterDecls ) ) { splice( std::back_inserter( statements ), afterDecls); }
	if ( !empty( afterStmts ) ) { statements.splice( statements.end(), *afterStmts ); }
	if ( !errors.isEmpty() ) { throw errors; }
}

template< typename pass_type >
void PassVisitor< pass_type >::visitStatementList( std::list< Statement * > & statements ) {
	handleStatementList( statements, [this]( Statement * stmt) {
		maybeAccept_impl( stmt, *this );
	});
}

template< typename pass_type >
void PassVisitor< pass_type >::visitStatementList( const std::list< Statement * > & statements ) {
	if ( ! get_visit_children() ) return;
	SemanticErrorException errors;

	pass_visitor_stats.depth++;
	pass_visitor_stats.max->push(pass_visitor_stats.depth);
	pass_visitor_stats.avg->push(pass_visitor_stats.depth);
	for ( const Statement * i : statements ) {
		try {
			maybeAccept_impl( i, *this );
		} catch ( SemanticErrorException &e ) {
			errors.append( e );
		}
	}
	pass_visitor_stats.depth--;
	if ( !errors.isEmpty() ) { throw errors; }
}

template< typename pass_type >
void PassVisitor< pass_type >::mutateStatementList( std::list< Statement * > & statements ) {
	handleStatementList( statements, [this]( Statement *& stmt) {
		maybeMutate_impl( stmt, *this );
	});
}


template< typename pass_type >
template< typename func_t >
Statement * PassVisitor< pass_type >::handleStatement( Statement * stmt, func_t func ) {
	if ( ! get_visit_children() ) return stmt;

	// don't want statements from outer CompoundStmts to be added to this CompoundStmt
	ValueGuardPtr< typename std::remove_pointer<decltype(get_env_ptr())>::type >  oldEnv( get_env_ptr() );
	ValueGuardPtr< DeclList_t >          oldBeforeDecls( get_beforeDecls() );
	ValueGuardPtr< DeclList_t >          oldAfterDecls ( get_afterDecls () );
	ValueGuardPtr< StmtList_t >          oldBeforeStmts( get_beforeStmts() );
	ValueGuardPtr< StmtList_t >          oldAfterStmts ( get_afterStmts () );

	Statement *newStmt = func( stmt );

	StmtList_t* beforeStmts = get_beforeStmts();
	StmtList_t* afterStmts  = get_afterStmts();
	DeclList_t* beforeDecls = get_beforeDecls();
	DeclList_t* afterDecls  = get_afterDecls();

	if( empty(beforeStmts) && empty(afterStmts) && empty(beforeDecls) && empty(afterDecls) ) { return newStmt; }
	assert(( empty( beforeStmts ) && empty( afterStmts ))
	    || ( empty( beforeDecls ) && empty( afterDecls )) );

	CompoundStmt *compound = new CompoundStmt();
	if( !empty(beforeDecls) ) { splice( std::back_inserter( compound->get_kids() ), beforeDecls ); }
	if( !empty(beforeStmts) ) { compound->get_kids().splice( compound->get_kids().end(), *beforeStmts ); }
	compound->get_kids().push_back( newStmt );
	if( !empty(afterDecls) ) { splice( std::back_inserter( compound->get_kids() ), afterDecls ); }
	if( !empty(afterStmts) ) { compound->get_kids().splice( compound->get_kids().end(), *afterStmts ); }
	return compound;
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::visitStatement( Statement * stmt ) {
	return handleStatement( stmt, [this]( Statement * stmt ) {
		maybeAccept_impl( stmt, *this );
		return stmt;
	});
}

template< typename pass_type >
void PassVisitor< pass_type >::visitStatement( const Statement * stmt ) {
	if ( ! get_visit_children() ) return;

	// don't want statements from outer CompoundStmts to be added to this CompoundStmt
	ValueGuardPtr< typename std::remove_pointer<decltype(get_env_ptr())>::type >  oldEnv( get_env_ptr() );

	maybeAccept_impl( stmt, *this );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutateStatement( Statement * stmt ) {
	return handleStatement( stmt, [this]( Statement * stmt ) {
		maybeMutate_impl( stmt, *this );
		return stmt;
	});
}

template< typename pass_type >
template< typename func_t >
Expression * PassVisitor< pass_type >::handleExpression( Expression * expr, func_t func ) {
	if ( ! get_visit_children() ) return expr;
	if( !expr ) return nullptr;

	auto env_ptr = get_env_ptr();
	if ( env_ptr && expr->get_env() ) {
		*env_ptr = expr->get_env();
	}

	// should env be moved onto the result of the mutate?
	return func( expr );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::visitExpression( Expression * expr ) {
	return handleExpression(expr, [this]( Expression * expr ) {
		maybeAccept_impl( expr, *this );
		return expr;
	});
}

template< typename pass_type >
void PassVisitor< pass_type >::visitExpression( const Expression * expr ) {
	if ( ! get_visit_children() ) return;
	if( !expr ) return;

	auto env_ptr = get_env_ptr();
	if ( env_ptr && expr->get_env() ) {
		*env_ptr = expr->get_env();
	}

	maybeAccept_impl( expr, *this );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutateExpression( Expression * expr ) {
	return handleExpression(expr, [this]( Expression * expr ) {
		maybeMutate_impl( expr, *this );
		return expr;
	});
}

template< typename TreeType, typename VisitorType >
inline void indexerScopedAccept( TreeType * tree, VisitorType & visitor ) {
	if ( ! visitor.get_visit_children() ) return;
	auto guard = makeFuncGuard(
		[&visitor]() { visitor.indexerScopeEnter(); },
		[&visitor]() { visitor.indexerScopeLeave(); }
	);
	maybeAccept_impl( tree, visitor );
}

template< typename TreeType, typename VisitorType >
inline void indexerScopedAccept( const TreeType * tree, VisitorType & visitor ) {
	if ( ! visitor.get_visit_children() ) return;
	auto guard = makeFuncGuard(
		[&visitor]() { visitor.indexerScopeEnter(); },
		[&visitor]() { visitor.indexerScopeLeave(); }
	);
	maybeAccept_impl( tree, visitor );
}

template< typename TreeType, typename MutatorType >
inline void indexerScopedMutate( TreeType *& tree, MutatorType & mutator ) {
	if ( ! mutator.get_visit_children() ) return;
	auto guard = makeFuncGuard(
		[&mutator]() { mutator.indexerScopeEnter(); },
		[&mutator]() { mutator.indexerScopeLeave(); }
	);
	maybeMutate_impl( tree, mutator );
}

//------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//========================================================================================================================================================================
//========================================================================================================================================================================
//========================================================================================================================================================================
//========================================================================================================================================================================
//========================================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------------------------------

// A NOTE ON THE ORDER OF TRAVERSAL
//
// Types and typedefs have their base types visited before they are added to the type table.  This is ok, since there is
// no such thing as a recursive type or typedef.
//
//             typedef struct { T *x; } T; // never allowed
//
// for structs/unions, it is possible to have recursion, so the decl should be added as if it's incomplete to begin, the
// members are traversed, and then the complete type should be added (assuming the type is completed by this particular
// declaration).
//
//             struct T { struct T *x; }; // allowed
//
// It is important to add the complete type to the symbol table *after* the members/base has been traversed, since that
// traversal may modify the definition of the type and these modifications should be visible when the symbol table is
// queried later in this pass.
//
// TODO: figure out whether recursive contexts are sensible/possible/reasonable.

//--------------------------------------------------------------------------
// ObjectDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( ObjectDecl * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->type         , *this );
	maybeAccept_impl   ( node->init         , *this );
	maybeAccept_impl   ( node->bitfieldWidth, *this );
	maybeAccept_impl   ( node->attributes   , *this );

	indexerAddId( node );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ObjectDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->type         , *this );
	maybeAccept_impl( node->init         , *this );
	maybeAccept_impl( node->bitfieldWidth, *this );
	maybeAccept_impl( node->attributes   , *this );

	VISIT_END( node );
}

template< typename pass_type >
DeclarationWithType * PassVisitor< pass_type >::mutate( ObjectDecl * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->type         , *this );
	maybeMutate_impl   ( node->init         , *this );
	maybeMutate_impl   ( node->bitfieldWidth, *this );
	maybeMutate_impl   ( node->attributes   , *this );

	indexerAddId( node );

	MUTATE_END( DeclarationWithType, node );
}

//--------------------------------------------------------------------------
// FunctionDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( FunctionDecl * node ) {
	VISIT_START( node );

	indexerAddId( node );

	maybeAccept_impl( node->withExprs, *this );
	{
		// with clause introduces a level of scope (for the with expression members).
		// with clause exprs are added to the indexer before parameters so that parameters
		// shadow with exprs and not the other way around.
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		indexerAddWith( node->withExprs, node );
		{
			auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
			// implicit add __func__ identifier as specified in the C manual 6.4.2.2
			static ObjectDecl func(
				"__func__", noStorageClasses, LinkageSpec::C, nullptr,
				new ArrayType( Type::Qualifiers(), new BasicType( Type::Qualifiers( Type::Const ), BasicType::Char ), nullptr, true, false ),
				nullptr
			);
			indexerAddId( &func );
			maybeAccept_impl( node->type, *this );
			// First remember that we are now within a function.
			ValueGuard< bool > oldInFunction( inFunction );
			inFunction = true;
			// The function body needs to have the same scope as parameters.
			// A CompoundStmt will not enter a new scope if atFunctionTop is true.
			ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
			atFunctionTop = true;
			maybeAccept_impl( node->statements, *this );
			maybeAccept_impl( node->attributes, *this );
		}
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const FunctionDecl * node ) {
	VISIT_START( node );

	indexerAddId( node );

	maybeAccept_impl( node->withExprs, *this );
	{
		// with clause introduces a level of scope (for the with expression members).
		// with clause exprs are added to the indexer before parameters so that parameters
		// shadow with exprs and not the other way around.
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		indexerAddWith( node->withExprs, node );
		{
			auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
			// implicit add __func__ identifier as specified in the C manual 6.4.2.2
			static ObjectDecl func(
				"__func__", noStorageClasses, LinkageSpec::C, nullptr,
				new ArrayType( Type::Qualifiers(), new BasicType( Type::Qualifiers( Type::Const ), BasicType::Char ), nullptr, true, false ),
				nullptr
			);
			indexerAddId( &func );
			maybeAccept_impl( node->type, *this );
			// First remember that we are now within a function.
			ValueGuard< bool > oldInFunction( inFunction );
			inFunction = true;
			// The function body needs to have the same scope as parameters.
			// A CompoundStmt will not enter a new scope if atFunctionTop is true.
			ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
			atFunctionTop = true;
			maybeAccept_impl( node->statements, *this );
			maybeAccept_impl( node->attributes, *this );
		}
	}

	VISIT_END( node );
}

template< typename pass_type >
DeclarationWithType * PassVisitor< pass_type >::mutate( FunctionDecl * node ) {
	MUTATE_START( node );

	indexerAddId( node );

	{
		// with clause introduces a level of scope (for the with expression members).
		// with clause exprs are added to the indexer before parameters so that parameters
		// shadow with exprs and not the other way around.
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		indexerAddWith( node->withExprs, node );
		{
			auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
			// implicit add __func__ identifier as specified in the C manual 6.4.2.2
			static ObjectDecl func(
				"__func__", noStorageClasses, LinkageSpec::C, nullptr,
				new ArrayType( Type::Qualifiers(), new BasicType( Type::Qualifiers( Type::Const ), BasicType::Char ), nullptr, true, false ),
				nullptr
			);
			indexerAddId( &func );
			maybeMutate_impl( node->type, *this );
			maybeMutate_impl( node->attributes, *this );
			// First remember that we are now within a function.
			ValueGuard< bool > oldInFunction( inFunction );
			inFunction = true;
			// The function body needs to have the same scope as parameters.
			// A CompoundStmt will not enter a new scope if atFunctionTop is true.
			ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
			atFunctionTop = true;
			maybeMutate_impl( node->statements, *this );
		}
	}

	MUTATE_END( DeclarationWithType, node );
}

//--------------------------------------------------------------------------
// StructDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( StructDecl * node ) {
	VISIT_START( node );

	// make up a forward declaration and add it before processing the members
	// needs to be on the heap because addStruct saves the pointer
	indexerAddStructFwd( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->parameters, *this );
		maybeAccept_impl( node->members   , *this );
		maybeAccept_impl( node->attributes, *this );
	}

	// this addition replaces the forward declaration
	indexerAddStruct( node );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const StructDecl * node ) {
	VISIT_START( node );

	// make up a forward declaration and add it before processing the members
	// needs to be on the heap because addStruct saves the pointer
	indexerAddStructFwd( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->parameters, *this );
		maybeAccept_impl( node->members   , *this );
		maybeAccept_impl( node->attributes, *this );
	}

	// this addition replaces the forward declaration
	indexerAddStruct( node );

	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( StructDecl * node ) {
	MUTATE_START( node );

	// make up a forward declaration and add it before processing the members
	// needs to be on the heap because addStruct saves the pointer
	indexerAddStructFwd( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->parameters, *this );
		maybeMutate_impl( node->members   , *this );
		maybeMutate_impl( node->attributes, *this );
	}

	// this addition replaces the forward declaration
	indexerAddStruct( node );

	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// UnionDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( UnionDecl * node ) {
	VISIT_START( node );

	// make up a forward declaration and add it before processing the members
	indexerAddUnionFwd( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->parameters, *this );
		maybeAccept_impl( node->members   , *this );
		maybeAccept_impl( node->attributes, *this );
	}

	indexerAddUnion( node );

	VISIT_END( node );
}
template< typename pass_type >
void PassVisitor< pass_type >::visit( const UnionDecl * node ) {
	VISIT_START( node );

	// make up a forward declaration and add it before processing the members
	indexerAddUnionFwd( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->parameters, *this );
		maybeAccept_impl( node->members   , *this );
		maybeAccept_impl( node->attributes, *this );
	}

	indexerAddUnion( node );

	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( UnionDecl * node ) {
	MUTATE_START( node );

	// make up a forward declaration and add it before processing the members
	indexerAddUnionFwd( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->parameters, *this );
		maybeMutate_impl( node->members   , *this );
		maybeMutate_impl( node->attributes, *this );
	}

	indexerAddUnion( node );

	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// EnumDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( EnumDecl * node ) {
	VISIT_START( node );

	indexerAddEnum( node );

	// unlike structs, traits, and unions, enums inject their members into the global scope
	// if ( node->base ) maybeAccept_impl( node->base, *this ); // Need this? Maybe not?
	maybeAccept_impl( node->parameters, *this );
	maybeAccept_impl( node->members   , *this );
	maybeAccept_impl( node->attributes, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const EnumDecl * node ) {
	VISIT_START( node );

	indexerAddEnum( node );

	// unlike structs, traits, and unions, enums inject their members into the global scope
	maybeAccept_impl( node->parameters, *this );
	maybeAccept_impl( node->members   , *this );
	maybeAccept_impl( node->attributes, *this );

	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( EnumDecl * node ) {
	MUTATE_START( node );

	indexerAddEnum( node );

	// unlike structs, traits, and unions, enums inject their members into the global scope
	maybeMutate_impl( node->parameters, *this );
	maybeMutate_impl( node->members   , *this );
	maybeMutate_impl( node->attributes, *this );

	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// TraitDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( TraitDecl * node ) {
	VISIT_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->parameters, *this );
		maybeAccept_impl( node->members   , *this );
		maybeAccept_impl( node->attributes, *this );
	}

	indexerAddTrait( node );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TraitDecl * node ) {
	VISIT_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->parameters, *this );
		maybeAccept_impl( node->members   , *this );
		maybeAccept_impl( node->attributes, *this );
	}

	indexerAddTrait( node );

	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( TraitDecl * node ) {
	MUTATE_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->parameters, *this );
		maybeMutate_impl( node->members   , *this );
		maybeMutate_impl( node->attributes, *this );
	}

	indexerAddTrait( node );

	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// TypeDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( TypeDecl * node ) {
	VISIT_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->base      , *this );
	}

	// see A NOTE ON THE ORDER OF TRAVERSAL, above
	// note that assertions come after the type is added to the symtab, since they are not part of the type proper
	// and may depend on the type itself
	indexerAddType( node );

	maybeAccept_impl( node->assertions, *this );

	indexerScopedAccept( node->init, *this );

	VISIT_END( node );
}


template< typename pass_type >
void PassVisitor< pass_type >::visit( const TypeDecl * node ) {
	VISIT_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->base      , *this );
	}

	// see A NOTE ON THE ORDER OF TRAVERSAL, above
	// note that assertions come after the type is added to the symtab, since they are not part of the type proper
	// and may depend on the type itself
	indexerAddType( node );

	maybeAccept_impl( node->assertions, *this );

	indexerScopedAccept( node->init, *this );

	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( TypeDecl * node ) {
	MUTATE_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->base      , *this );
	}

	// see A NOTE ON THE ORDER OF TRAVERSAL, above
	// note that assertions come after the type is added to the symtab, since they are not part of the type proper
	// and may depend on the type itself
	indexerAddType( node );

	maybeMutate_impl( node->assertions, *this );

	indexerScopedMutate( node->init, *this );

	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// TypedefDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( TypedefDecl * node ) {
	VISIT_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->base      , *this );
	}

	indexerAddType( node );

	maybeAccept_impl( node->assertions, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TypedefDecl * node ) {
	VISIT_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->base      , *this );
	}

	indexerAddType( node );

	maybeAccept_impl( node->assertions, *this );

	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( TypedefDecl * node ) {
	MUTATE_START( node );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->base      , *this );
	}

	indexerAddType( node );

	maybeMutate_impl( node->assertions, *this );

	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// AsmDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( AsmDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->stmt, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const AsmDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->stmt, *this );

	VISIT_END( node );
}

template< typename pass_type >
AsmDecl * PassVisitor< pass_type >::mutate( AsmDecl * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->stmt, *this );

	MUTATE_END( AsmDecl, node );
}

//--------------------------------------------------------------------------
// DirectiveDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( DirectiveDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->stmt, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const DirectiveDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->stmt, *this );

	VISIT_END( node );
}

template< typename pass_type >
DirectiveDecl * PassVisitor< pass_type >::mutate( DirectiveDecl * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->stmt, *this );

	MUTATE_END( DirectiveDecl, node );
}

//--------------------------------------------------------------------------
// StaticAssertDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( StaticAssertDecl * node ) {
	VISIT_START( node );

	node->condition = visitExpression( node->condition );
	maybeAccept_impl( node->message, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const StaticAssertDecl * node ) {
	VISIT_START( node );

	visitExpression( node->condition );
	maybeAccept_impl( node->message, *this );

	VISIT_END( node );
}

template< typename pass_type >
StaticAssertDecl * PassVisitor< pass_type >::mutate( StaticAssertDecl * node ) {
	MUTATE_START( node );

	node->condition = mutateExpression( node->condition );
	maybeMutate_impl( node->message, *this );

	MUTATE_END( StaticAssertDecl, node );
}

//--------------------------------------------------------------------------
// InlineValueDecl
template< typename pass_type >
void PassVisitor< pass_type >::visit( InlineValueDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->type, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const InlineValueDecl * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->type, *this );

	VISIT_END( node );
}

template< typename pass_type >
DeclarationWithType * PassVisitor< pass_type >::mutate( InlineValueDecl * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->type, *this );

	MUTATE_END( DeclarationWithType, node );
}

//--------------------------------------------------------------------------
// CompoundStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( CompoundStmt * node ) {
	VISIT_START( node );
	{
		// Do not enter a new scope if atFunctionTop is true, don't leave one either.
		ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
		auto guard1 = makeFuncGuard( [this, go = !atFunctionTop]() { if ( go ) indexerScopeEnter(); }, [this, go = !atFunctionTop]() { if ( go ) indexerScopeLeave(); } );
		auto guard2 = makeFuncGuard( [this]() { call_beginScope();   }, [this]() { call_endScope();     } );
		atFunctionTop = false;
		visitStatementList( node->kids );
	}
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const CompoundStmt * node ) {
	VISIT_START( node );
	{
		// Do not enter a new scope if atFunctionTop is true, don't leave one either.
		ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
		auto guard1 = makeFuncGuard( [this, go = !atFunctionTop]() { if ( go ) indexerScopeEnter(); }, [this, go = !atFunctionTop]() { if ( go ) indexerScopeLeave(); } );
		auto guard2 = makeFuncGuard( [this]() { call_beginScope();   }, [this]() { call_endScope();     } );
		atFunctionTop = false;
		visitStatementList( node->kids );
	}
	VISIT_END( node );
}

template< typename pass_type >
CompoundStmt * PassVisitor< pass_type >::mutate( CompoundStmt * node ) {
	MUTATE_START( node );
	{
		// Do not enter a new scope if atFunctionTop is true, don't leave one either.
		ValueGuard< bool > oldAtFunctionTop( atFunctionTop );
		auto guard1 = makeFuncGuard( [this, go = !atFunctionTop]() { if ( go ) indexerScopeEnter(); }, [this, go = !atFunctionTop]() { if ( go ) indexerScopeLeave(); } );
		auto guard2 = makeFuncGuard( [this]() { call_beginScope();   }, [this]() { call_endScope();     } );
		atFunctionTop = false;
		mutateStatementList( node->kids );
	}
	MUTATE_END( CompoundStmt, node );
}

//--------------------------------------------------------------------------
// ExprStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( ExprStmt * node ) {
	VISIT_START( node );

	visitExpression( node->expr );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ExprStmt * node ) {
	VISIT_START( node );

	visitExpression( node->expr );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( ExprStmt * node ) {
	MUTATE_START( node );

	node->expr = mutateExpression( node->expr );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// AsmStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( AsmStmt * node ) {
	VISIT_START( node )

	maybeAccept_impl( node->instruction, *this );
	maybeAccept_impl( node->output, *this );
	maybeAccept_impl( node->input, *this );
	maybeAccept_impl( node->clobber, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const AsmStmt * node ) {
	VISIT_START( node )

	maybeAccept_impl( node->instruction, *this );
	maybeAccept_impl( node->output, *this );
	maybeAccept_impl( node->input, *this );
	maybeAccept_impl( node->clobber, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( AsmStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->instruction, *this );
	maybeMutate_impl( node->output, *this );
	maybeMutate_impl( node->input, *this );
	maybeMutate_impl( node->clobber, *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// AsmStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( DirectiveStmt * node ) {
	VISIT_START( node )

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const DirectiveStmt * node ) {
	VISIT_START( node )

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( DirectiveStmt * node ) {
	MUTATE_START( node );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// IfStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( IfStmt * node ) {
	VISIT_START( node );
	{
		// if statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->initialization, *this );
		visitExpression ( node->condition );
		node->then = visitStatement( node->then );
		node->else_ = visitStatement( node->else_ );
	}
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const IfStmt * node ) {
	VISIT_START( node );
	{
		// if statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->initialization, *this );
		visitExpression ( node->condition );
		visitStatement  ( node->then );
		visitStatement  ( node->else_ );
	}
	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( IfStmt * node ) {
	MUTATE_START( node );
	{
		// if statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->initialization, *this );
		node->condition = mutateExpression( node->condition );
		node->then  = mutateStatement ( node->then  );
		node->else_  = mutateStatement ( node->else_  );
	}
	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// WhileDoStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( WhileDoStmt * node ) {
	VISIT_START( node );

	{
		// while statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->initialization, *this );
		visitExpression ( node->condition );
		node->body = visitStatement( node->body );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const WhileDoStmt * node ) {
	VISIT_START( node );

	{
		// while statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->initialization, *this );
		visitExpression ( node->condition );
		visitStatement  ( node->body );
	}

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( WhileDoStmt * node ) {
	MUTATE_START( node );

	{
		// while statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->initialization, *this );
		node->condition = mutateExpression( node->condition );
		node->body      = mutateStatement ( node->body      );
	}


	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// ForStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( ForStmt * node ) {
	VISIT_START( node );
	{
		// for statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->initialization, *this );
		visitExpression( node->condition );
		visitExpression( node->increment );
		node->body = visitStatement( node->body );
	}
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ForStmt * node ) {
	VISIT_START( node );
	{
		// for statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->initialization, *this );
		visitExpression( node->condition );
		visitExpression( node->increment );
		visitStatement ( node->body );
	}
	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( ForStmt * node ) {
	MUTATE_START( node );
	{
		// for statements introduce a level of scope (for the initialization)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->initialization, *this );
		node->condition = mutateExpression( node->condition );
		node->increment = mutateExpression( node->increment );
		node->body      = mutateStatement ( node->body      );
	}
	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// SwitchStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( SwitchStmt * node ) {
	VISIT_START( node );

	visitExpression   ( node->condition  );
	visitStatementList( node->statements );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const SwitchStmt * node ) {
	VISIT_START( node );

	visitExpression   ( node->condition  );
	visitStatementList( node->statements );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( SwitchStmt * node ) {
	MUTATE_START( node );

	node->condition = mutateExpression( node->condition );
	mutateStatementList( node->statements );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// CaseStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( CaseStmt * node ) {
	VISIT_START( node );

	visitExpression   ( node->condition );
	visitStatementList( node->stmts     );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const CaseStmt * node ) {
	VISIT_START( node );

	visitExpression   ( node->condition );
	visitStatementList( node->stmts     );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( CaseStmt * node ) {
	MUTATE_START( node );

	node->condition = mutateExpression( node->condition );
	mutateStatementList( node->stmts );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// BranchStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( BranchStmt * node ) {
	VISIT_START( node );
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const BranchStmt * node ) {
	VISIT_START( node );
	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( BranchStmt * node ) {
	MUTATE_START( node );
	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// ReturnStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( ReturnStmt * node ) {
	VISIT_START( node );

	visitExpression( node->expr );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ReturnStmt * node ) {
	VISIT_START( node );

	visitExpression( node->expr );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( ReturnStmt * node ) {
	MUTATE_START( node );

	node->expr = mutateExpression( node->expr );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// ThrowStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( ThrowStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->expr, *this );
	maybeAccept_impl( node->target, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ThrowStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->expr, *this );
	maybeAccept_impl( node->target, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( ThrowStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->expr, *this );
	maybeMutate_impl( node->target, *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// TryStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( TryStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->block       , *this );
	maybeAccept_impl( node->handlers    , *this );
	maybeAccept_impl( node->finallyBlock, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TryStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->block       , *this );
	maybeAccept_impl( node->handlers    , *this );
	maybeAccept_impl( node->finallyBlock, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( TryStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->block       , *this );
	maybeMutate_impl( node->handlers    , *this );
	maybeMutate_impl( node->finallyBlock, *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// CatchStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( CatchStmt * node ) {
	VISIT_START( node );
	{
		// catch statements introduce a level of scope (for the caught exception)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->decl, *this );
		node->cond = visitExpression( node->cond );
		node->body = visitStatement ( node->body );
	}
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const CatchStmt * node ) {
	VISIT_START( node );
	{
		// catch statements introduce a level of scope (for the caught exception)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->decl, *this );
		visitExpression ( node->cond );
		visitStatement  ( node->body );
	}
	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( CatchStmt * node ) {
	MUTATE_START( node );
	{
		// catch statements introduce a level of scope (for the caught exception)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->decl, *this );
		node->cond = mutateExpression( node->cond );
		node->body = mutateStatement ( node->body );
	}
	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// FinallyStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( FinallyStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->block, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const FinallyStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->block, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( FinallyStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->block, *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// SuspendStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( SuspendStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->then  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const SuspendStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->then  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( SuspendStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->then  , *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// WaitForStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( WaitForStmt * node ) {
	VISIT_START( node );

	for( auto & clause : node->clauses ) {
		maybeAccept_impl( clause.target.function, *this );
		maybeAccept_impl( clause.target.arguments, *this );

		maybeAccept_impl( clause.statement, *this );
		maybeAccept_impl( clause.condition, *this );
	}

	maybeAccept_impl( node->timeout.time, *this );
	maybeAccept_impl( node->timeout.statement, *this );
	maybeAccept_impl( node->timeout.condition, *this );
	maybeAccept_impl( node->orelse.statement, *this );
	maybeAccept_impl( node->orelse.condition, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const WaitForStmt * node ) {
	VISIT_START( node );

	for( auto & clause : node->clauses ) {
		maybeAccept_impl( clause.target.function, *this );
		maybeAccept_impl( clause.target.arguments, *this );

		maybeAccept_impl( clause.statement, *this );
		maybeAccept_impl( clause.condition, *this );
	}

	maybeAccept_impl( node->timeout.time, *this );
	maybeAccept_impl( node->timeout.statement, *this );
	maybeAccept_impl( node->timeout.condition, *this );
	maybeAccept_impl( node->orelse.statement, *this );
	maybeAccept_impl( node->orelse.condition, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( WaitForStmt * node ) {
	MUTATE_START( node );

	for( auto & clause : node->clauses ) {
		maybeMutate_impl( clause.target.function, *this );
		maybeMutate_impl( clause.target.arguments, *this );

		maybeMutate_impl( clause.statement, *this );
		maybeMutate_impl( clause.condition, *this );
	}

	maybeMutate_impl( node->timeout.time, *this );
	maybeMutate_impl( node->timeout.statement, *this );
	maybeMutate_impl( node->timeout.condition, *this );
	maybeMutate_impl( node->orelse.statement, *this );
	maybeMutate_impl( node->orelse.condition, *this );

	MUTATE_END( Statement, node );
}



//--------------------------------------------------------------------------
// WithStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( WithStmt * node ) {
	VISIT_START( node );
	maybeAccept_impl( node->exprs, *this );
	{
		// catch statements introduce a level of scope (for the caught exception)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		indexerAddWith( node->exprs, node );
		maybeAccept_impl( node->stmt, *this );
	}
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const WithStmt * node ) {
	VISIT_START( node );
	maybeAccept_impl( node->exprs, *this );
	{
		// catch statements introduce a level of scope (for the caught exception)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		indexerAddWith( node->exprs, node );
		maybeAccept_impl( node->stmt, *this );
	}
	VISIT_END( node );
}

template< typename pass_type >
Declaration * PassVisitor< pass_type >::mutate( WithStmt * node ) {
	MUTATE_START( node );
	maybeMutate_impl( node->exprs, *this );
	{
		// catch statements introduce a level of scope (for the caught exception)
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		indexerAddWith( node->exprs, node );
		maybeMutate_impl( node->stmt, *this );
	}
	MUTATE_END( Declaration, node );
}

//--------------------------------------------------------------------------
// NullStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( NullStmt * node ) {
	VISIT_START( node );
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const NullStmt * node ) {
	VISIT_START( node );
	VISIT_END( node );
}

template< typename pass_type >
NullStmt * PassVisitor< pass_type >::mutate( NullStmt * node ) {
	MUTATE_START( node );
	MUTATE_END( NullStmt, node );
}

//--------------------------------------------------------------------------
// DeclStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( DeclStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->decl, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const DeclStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->decl, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( DeclStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->decl, *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// ImplicitCtorDtorStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( ImplicitCtorDtorStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->callStmt, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ImplicitCtorDtorStmt * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->callStmt, *this );

	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( ImplicitCtorDtorStmt * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->callStmt, *this );

	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// MutexStmt
template< typename pass_type >
void PassVisitor< pass_type >::visit( MutexStmt * node ) {
	VISIT_START( node );
	// mutex statements introduce a level of scope (for the initialization)
	maybeAccept_impl( node->mutexObjs, *this );
	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		node->stmt = visitStatement( node->stmt );
	}
	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const MutexStmt * node ) {
	VISIT_START( node );
	maybeAccept_impl( node->mutexObjs, *this );
	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		visitStatement( node->stmt );
	}
	VISIT_END( node );
}

template< typename pass_type >
Statement * PassVisitor< pass_type >::mutate( MutexStmt * node ) {
	MUTATE_START( node );
	maybeMutate_impl( node->mutexObjs, *this );
	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		node->stmt = mutateStatement( node->stmt );
	}
	MUTATE_END( Statement, node );
}

//--------------------------------------------------------------------------
// ApplicationExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( ApplicationExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result  , *this );
	maybeAccept_impl   ( node->function, *this );
	maybeAccept_impl   ( node->args    , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ApplicationExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result  , *this );
	maybeAccept_impl   ( node->function, *this );
	maybeAccept_impl   ( node->args    , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( ApplicationExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env     , *this );
	indexerScopedMutate( node->result  , *this );
	maybeMutate_impl   ( node->function, *this );
	maybeMutate_impl   ( node->args    , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// UntypedExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( UntypedExpr * node ) {
	VISIT_START( node );

	// maybeAccept_impl( node->get_env(), *this );
	indexerScopedAccept( node->result, *this );

	for ( auto expr : node->args ) {
		visitExpression( expr );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UntypedExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	for ( auto expr : node->args ) {
		visitExpression( expr );
	}

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( UntypedExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );

	for ( auto& expr : node->args ) {
		expr = mutateExpression( expr );
	}

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// NameExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( NameExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const NameExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( NameExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// QualifiedNameExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( QualifiedNameExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl( node->type_decl, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const QualifiedNameExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl( node->type_decl, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( QualifiedNameExpr * node ) {
	MUTATE_START( node );

    indexerScopedMutate( node->env   , *this );
    indexerScopedMutate( node->result, *this );
	maybeMutate_impl( node->type_decl, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// CastExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( CastExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg   , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const CastExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg   , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( CastExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg   , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// KeywordCastExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( KeywordCastExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl        ( node->arg   , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const KeywordCastExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg   , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( KeywordCastExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg   , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// VirtualCastExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( VirtualCastExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const VirtualCastExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( VirtualCastExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg   , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// AddressExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( AddressExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg   , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const AddressExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg   , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( AddressExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg   , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// LabelAddressExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( LabelAddressExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const LabelAddressExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( LabelAddressExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// UntypedMemberExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( UntypedMemberExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result   , *this );
	maybeAccept_impl   ( node->aggregate, *this );
	maybeAccept_impl   ( node->member   , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UntypedMemberExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result   , *this );
	maybeAccept_impl   ( node->aggregate, *this );
	maybeAccept_impl   ( node->member   , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( UntypedMemberExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env      , *this );
	indexerScopedMutate( node->result   , *this );
	maybeMutate_impl   ( node->aggregate, *this );
	maybeMutate_impl   ( node->member   , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// MemberExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( MemberExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result   , *this );
	maybeAccept_impl   ( node->aggregate, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const MemberExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result   , *this );
	maybeAccept_impl   ( node->aggregate, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( MemberExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env      , *this );
	indexerScopedMutate( node->result   , *this );
	maybeMutate_impl   ( node->aggregate, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// VariableExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( VariableExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const VariableExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( VariableExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// ConstantExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( ConstantExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result   , *this );
	maybeAccept_impl   ( &node->constant, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ConstantExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result   , *this );
	maybeAccept_impl   ( &node->constant, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( ConstantExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	Constant * ptr = &node->constant;
	maybeMutate_impl( ptr, *this );
	node->constant = *ptr;

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// SizeofExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( SizeofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	if ( node->get_isType() ) {
		maybeAccept_impl( node->type, *this );
	} else {
		maybeAccept_impl( node->expr, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const SizeofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	if ( node->get_isType() ) {
		maybeAccept_impl( node->type, *this );
	} else {
		maybeAccept_impl( node->expr, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( SizeofExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	if ( node->get_isType() ) {
		maybeMutate_impl( node->type, *this );
	} else {
		maybeMutate_impl( node->expr, *this );
	}

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// AlignofExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( AlignofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	if ( node->get_isType() ) {
		maybeAccept_impl( node->type, *this );
	} else {
		maybeAccept_impl( node->expr, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const AlignofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	if ( node->get_isType() ) {
		maybeAccept_impl( node->type, *this );
	} else {
		maybeAccept_impl( node->expr, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( AlignofExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	if ( node->get_isType() ) {
		maybeMutate_impl( node->type, *this );
	} else {
		maybeMutate_impl( node->expr, *this );
	}

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// UntypedOffsetofExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( UntypedOffsetofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UntypedOffsetofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( UntypedOffsetofExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->type  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// OffsetofExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( OffsetofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const OffsetofExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( OffsetofExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->type  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// OffsetPackExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( OffsetPackExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const OffsetPackExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( OffsetPackExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->type  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// LogicalExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( LogicalExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg1  , *this );
	maybeAccept_impl   ( node->arg2  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const LogicalExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg1  , *this );
	maybeAccept_impl   ( node->arg2  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( LogicalExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg1  , *this );
	maybeMutate_impl   ( node->arg2  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// ConditionalExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( ConditionalExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl        ( node->arg1  , *this );
	maybeAccept_impl        ( node->arg2  , *this );
	maybeAccept_impl        ( node->arg3  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ConditionalExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg1  , *this );
	maybeAccept_impl   ( node->arg2  , *this );
	maybeAccept_impl   ( node->arg3  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( ConditionalExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg1  , *this );
	maybeMutate_impl   ( node->arg2  , *this );
	maybeMutate_impl   ( node->arg3  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// CommaExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( CommaExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg1  , *this );
	maybeAccept_impl   ( node->arg2  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const CommaExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->arg1  , *this );
	maybeAccept_impl   ( node->arg2  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( CommaExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->arg1  , *this );
	maybeMutate_impl   ( node->arg2  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// TypeExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( TypeExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TypeExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->type, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( TypeExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->type  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// DimensionExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( DimensionExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const DimensionExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( DimensionExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// AsmExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( AsmExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result    , *this );
	maybeAccept_impl   ( node->constraint, *this );
	maybeAccept_impl   ( node->operand   , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const AsmExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result    , *this );
	maybeAccept_impl   ( node->constraint, *this );
	maybeAccept_impl   ( node->operand   , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( AsmExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env       , *this );
	indexerScopedMutate( node->result    , *this );
	maybeMutate_impl   ( node->constraint, *this );
	maybeMutate_impl   ( node->operand   , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// ImplicitCopyCtorExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( ImplicitCopyCtorExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result    , *this );
	maybeAccept_impl   ( node->callExpr  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ImplicitCopyCtorExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result    , *this );
	maybeAccept_impl   ( node->callExpr  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( ImplicitCopyCtorExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env       , *this );
	indexerScopedMutate( node->result    , *this );
	maybeMutate_impl   ( node->callExpr  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// ConstructorExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( ConstructorExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result  , *this );
	maybeAccept_impl   ( node->callExpr, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ConstructorExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result  , *this );
	maybeAccept_impl   ( node->callExpr, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( ConstructorExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env     , *this );
	indexerScopedMutate( node->result  , *this );
	maybeMutate_impl   ( node->callExpr, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// CompoundLiteralExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( CompoundLiteralExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result     , *this );
	maybeAccept_impl   ( node->initializer, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const CompoundLiteralExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result     , *this );
	maybeAccept_impl   ( node->initializer, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( CompoundLiteralExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env        , *this );
	indexerScopedMutate( node->result     , *this );
	maybeMutate_impl     ( node->initializer, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// RangeExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( RangeExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->low   , *this );
	maybeAccept_impl   ( node->high  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const RangeExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->low   , *this );
	maybeAccept_impl   ( node->high  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( RangeExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->low   , *this );
	maybeMutate_impl   ( node->high  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// UntypedTupleExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( UntypedTupleExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->exprs , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UntypedTupleExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->exprs , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( UntypedTupleExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->exprs , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// TupleExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( TupleExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->exprs , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TupleExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->exprs , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( TupleExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->exprs , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// TupleIndexExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( TupleIndexExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->tuple , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TupleIndexExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->tuple , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( TupleIndexExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->tuple , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// TupleAssignExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( TupleAssignExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result  , *this );
	maybeAccept_impl   ( node->stmtExpr, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TupleAssignExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result  , *this );
	maybeAccept_impl( node->stmtExpr, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( TupleAssignExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env     , *this );
	indexerScopedMutate( node->result  , *this );
	maybeMutate_impl   ( node->stmtExpr, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// StmtExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( StmtExpr * node ) {
	VISIT_START( node );

	// don't want statements from outer CompoundStmts to be added to this StmtExpr
	ValueGuardPtr< typename std::remove_pointer<decltype(get_env_ptr())>::type >  oldEnv( get_env_ptr() );
	ValueGuardPtr< std::list< Statement* > > oldBeforeStmts( get_beforeStmts() );
	ValueGuardPtr< std::list< Statement* > > oldAfterStmts ( get_afterStmts () );

	indexerScopedAccept( node->result     , *this );
	maybeAccept_impl   ( node->statements , *this );
	maybeAccept_impl   ( node->returnDecls, *this );
	maybeAccept_impl   ( node->dtors      , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const StmtExpr * node ) {
	VISIT_START( node );

	// don't want statements from outer CompoundStmts to be added to this StmtExpr
	ValueGuardPtr< typename std::remove_pointer<decltype(get_env_ptr())>::type >  oldEnv( get_env_ptr() );
	ValueGuardPtr< std::list< Statement* > > oldBeforeStmts( get_beforeStmts() );
	ValueGuardPtr< std::list< Statement* > > oldAfterStmts ( get_afterStmts () );

	indexerScopedAccept( node->result     , *this );
	maybeAccept_impl   ( node->statements , *this );
	maybeAccept_impl   ( node->returnDecls, *this );
	maybeAccept_impl   ( node->dtors      , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( StmtExpr * node ) {
	MUTATE_START( node );

	// don't want statements from outer CompoundStmts to be added to this StmtExpr
	ValueGuardPtr< typename std::remove_pointer<decltype(get_env_ptr())>::type >  oldEnv( get_env_ptr() );
	ValueGuardPtr< std::list< Statement* > > oldBeforeStmts( get_beforeStmts() );
	ValueGuardPtr< std::list< Statement* > > oldAfterStmts ( get_afterStmts () );

	indexerScopedMutate( node->result     , *this );
	maybeMutate_impl   ( node->statements , *this );
	maybeMutate_impl   ( node->returnDecls, *this );
	maybeMutate_impl   ( node->dtors      , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// UniqueExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( UniqueExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr  , *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UniqueExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr  , *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( UniqueExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->expr  , *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// UntypedInitExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( UntypedInitExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr  , *this );
	// not currently visiting initAlts, but this doesn't matter since this node is only used in the resolver.

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UntypedInitExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr  , *this );
	// not currently visiting initAlts, but this doesn't matter since this node is only used in the resolver.

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( UntypedInitExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->expr  , *this );
	// not currently visiting initAlts, but this doesn't matter since this node is only used in the resolver.

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// InitExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( InitExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr  , *this );
	maybeAccept_impl   ( node->designation, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const InitExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr  , *this );
	maybeAccept_impl   ( node->designation, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( InitExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env   , *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl   ( node->expr  , *this );
	maybeMutate_impl   ( node->designation, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// DeletedExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( DeletedExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr, *this );
	// don't visit deleteStmt, because it is a pointer to somewhere else in the tree.

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const DeletedExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr, *this );
	// don't visit deleteStmt, because it is a pointer to somewhere else in the tree.

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( DeletedExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env, *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl( node->expr, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// DefaultArgExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( DefaultArgExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const DefaultArgExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl   ( node->expr, *this );

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( DefaultArgExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env, *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl( node->expr, *this );

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// GenericExpr
template< typename pass_type >
void PassVisitor< pass_type >::visit( GenericExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl( node->control, *this );
	for ( GenericExpr::Association & assoc : node->associations ) {
		indexerScopedAccept( assoc.type, *this );
		maybeAccept_impl( assoc.expr, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const GenericExpr * node ) {
	VISIT_START( node );

	indexerScopedAccept( node->result, *this );
	maybeAccept_impl( node->control, *this );
	for ( const GenericExpr::Association & assoc : node->associations ) {
		indexerScopedAccept( assoc.type, *this );
		maybeAccept_impl( assoc.expr, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
Expression * PassVisitor< pass_type >::mutate( GenericExpr * node ) {
	MUTATE_START( node );

	indexerScopedMutate( node->env, *this );
	indexerScopedMutate( node->result, *this );
	maybeMutate_impl( node->control, *this );
	for ( GenericExpr::Association & assoc : node->associations ) {
		indexerScopedMutate( assoc.type, *this );
		maybeMutate_impl( assoc.expr, *this );
	}

	MUTATE_END( Expression, node );
}

//--------------------------------------------------------------------------
// VoidType
template< typename pass_type >
void PassVisitor< pass_type >::visit( VoidType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const VoidType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( VoidType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// BasicType
template< typename pass_type >
void PassVisitor< pass_type >::visit( BasicType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const BasicType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( BasicType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// PointerType
template< typename pass_type >
void PassVisitor< pass_type >::visit( PointerType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->dimension, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const PointerType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->dimension, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( PointerType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->dimension, *this );
	maybeMutate_impl( node->base, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// ArrayType
template< typename pass_type >
void PassVisitor< pass_type >::visit( ArrayType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->dimension, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ArrayType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->dimension, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( ArrayType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->dimension, *this );
	maybeMutate_impl( node->base, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// ReferenceType
template< typename pass_type >
void PassVisitor< pass_type >::visit( ReferenceType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ReferenceType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( ReferenceType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->base, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// QualifiedType
template< typename pass_type >
void PassVisitor< pass_type >::visit( QualifiedType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->parent, *this );
	maybeAccept_impl( node->child, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const QualifiedType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->parent, *this );
	maybeAccept_impl( node->child, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( QualifiedType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->parent, *this );
	maybeMutate_impl( node->child, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// FunctionType
template< typename pass_type >
void PassVisitor< pass_type >::visit( FunctionType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->returnVals, *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const FunctionType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->returnVals, *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( FunctionType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->returnVals, *this );
	maybeMutate_impl( node->parameters, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// StructInstType
template< typename pass_type >
void PassVisitor< pass_type >::visit( StructInstType * node ) {
	VISIT_START( node );

	indexerAddStruct( node->name );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->forall    , *this );
		maybeAccept_impl( node->parameters, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const StructInstType * node ) {
	VISIT_START( node );

	indexerAddStruct( node->name );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->forall    , *this );
		maybeAccept_impl( node->parameters, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( StructInstType * node ) {
	MUTATE_START( node );

	indexerAddStruct( node->name );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->forall    , *this );
		maybeMutate_impl( node->parameters, *this );
	}

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// UnionInstType
template< typename pass_type >
void PassVisitor< pass_type >::visit( UnionInstType * node ) {
	VISIT_START( node );

	indexerAddUnion( node->name );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->forall    , *this );
		maybeAccept_impl( node->parameters, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const UnionInstType * node ) {
	VISIT_START( node );

	indexerAddUnion( node->name );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeAccept_impl( node->forall    , *this );
		maybeAccept_impl( node->parameters, *this );
	}

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( UnionInstType * node ) {
	MUTATE_START( node );

	indexerAddUnion( node->name );

	{
		auto guard = makeFuncGuard( [this]() { indexerScopeEnter(); }, [this]() { indexerScopeLeave(); } );
		maybeMutate_impl( node->forall    , *this );
		maybeMutate_impl( node->parameters, *this );
	}

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// EnumInstType
template< typename pass_type >
void PassVisitor< pass_type >::visit( EnumInstType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const EnumInstType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( EnumInstType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->parameters, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// TraitInstType
template< typename pass_type >
void PassVisitor< pass_type >::visit( TraitInstType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall    , *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TraitInstType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall    , *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( TraitInstType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall    , *this );
	maybeMutate_impl( node->parameters, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// TypeInstType
template< typename pass_type >
void PassVisitor< pass_type >::visit( TypeInstType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall    , *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TypeInstType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall    , *this );
	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( TypeInstType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall    , *this );
	maybeMutate_impl( node->parameters, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// TupleType
template< typename pass_type >
void PassVisitor< pass_type >::visit( TupleType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->types, *this );
	maybeAccept_impl( node->members, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TupleType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->types, *this );
	maybeAccept_impl( node->members, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( TupleType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->types, *this );
	maybeMutate_impl( node->members, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// TypeofType
template< typename pass_type >
void PassVisitor< pass_type >::visit( TypeofType * node ) {
	VISIT_START( node );

	assert( node->expr );
	maybeAccept_impl( node->expr, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const TypeofType * node ) {
	VISIT_START( node );

	assert( node->expr );
	maybeAccept_impl( node->expr, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( TypeofType * node ) {
	MUTATE_START( node );

	assert( node->expr );
	maybeMutate_impl( node->expr, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// VTableType
template< typename pass_type >
void PassVisitor< pass_type >::visit( VTableType * node ) {
	VISIT_START( node );

	// Forall qualifiers should be on base type, not here
	// maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const VTableType * node ) {
	VISIT_START( node );

	// Forall qualifiers should be on base type, not here
	// maybeAccept_impl( node->forall, *this );
	maybeAccept_impl( node->base, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( VTableType * node ) {
	MUTATE_START( node );

	// Forall qualifiers should be on base type, not here
	// maybeMutate_impl( node->forall, *this );
	maybeMutate_impl( node->base, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// AttrType
template< typename pass_type >
void PassVisitor< pass_type >::visit( AttrType * node ) {
	VISIT_START( node );

	if ( node->isType ) {
		assert( node->type );
		maybeAccept_impl( node->type, *this );
	} else {
		assert( node->expr );
		maybeAccept_impl( node->expr, *this );
	} // if

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const AttrType * node ) {
	VISIT_START( node );

	if ( node->isType ) {
		assert( node->type );
		maybeAccept_impl( node->type, *this );
	} else {
		assert( node->expr );
		maybeAccept_impl( node->expr, *this );
	} // if

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( AttrType * node ) {
	MUTATE_START( node );

	if ( node->isType ) {
		assert( node->type );
		maybeMutate_impl( node->type, *this );
	} else {
		assert( node->expr );
		maybeMutate_impl( node->expr, *this );
	} // if

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// VarArgsType
template< typename pass_type >
void PassVisitor< pass_type >::visit( VarArgsType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const VarArgsType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( VarArgsType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// ZeroType
template< typename pass_type >
void PassVisitor< pass_type >::visit( ZeroType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ZeroType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( ZeroType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// OneType
template< typename pass_type >
void PassVisitor< pass_type >::visit( OneType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const OneType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( OneType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// GlobalScopeType
template< typename pass_type >
void PassVisitor< pass_type >::visit( GlobalScopeType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const GlobalScopeType * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->forall, *this );

	VISIT_END( node );
}

template< typename pass_type >
Type * PassVisitor< pass_type >::mutate( GlobalScopeType * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->forall, *this );

	MUTATE_END( Type, node );
}

//--------------------------------------------------------------------------
// Designation
template< typename pass_type >
void PassVisitor< pass_type >::visit( Designation * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->designators, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const Designation * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->designators, *this );

	VISIT_END( node );
}

template< typename pass_type >
Designation * PassVisitor< pass_type >::mutate( Designation * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->designators, *this );

	MUTATE_END( Designation, node );
}

//--------------------------------------------------------------------------
// SingleInit
template< typename pass_type >
void PassVisitor< pass_type >::visit( SingleInit * node ) {
	VISIT_START( node );

	visitExpression( node->value );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const SingleInit * node ) {
	VISIT_START( node );

	visitExpression( node->value );

	VISIT_END( node );
}

template< typename pass_type >
Initializer * PassVisitor< pass_type >::mutate( SingleInit * node ) {
	MUTATE_START( node );

	node->value = mutateExpression( node->value );

	MUTATE_END( Initializer, node );
}

//--------------------------------------------------------------------------
// ListInit
template< typename pass_type >
void PassVisitor< pass_type >::visit( ListInit * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->designations, *this );
	maybeAccept_impl( node->initializers, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ListInit * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->designations, *this );
	maybeAccept_impl( node->initializers, *this );

	VISIT_END( node );
}

template< typename pass_type >
Initializer * PassVisitor< pass_type >::mutate( ListInit * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->designations, *this );
	maybeMutate_impl( node->initializers, *this );

	MUTATE_END( Initializer, node );
}

//--------------------------------------------------------------------------
// ConstructorInit
template< typename pass_type >
void PassVisitor< pass_type >::visit( ConstructorInit * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->ctor, *this );
	maybeAccept_impl( node->dtor, *this );
	maybeAccept_impl( node->init, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const ConstructorInit * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->ctor, *this );
	maybeAccept_impl( node->dtor, *this );
	maybeAccept_impl( node->init, *this );

	VISIT_END( node );
}

template< typename pass_type >
Initializer * PassVisitor< pass_type >::mutate( ConstructorInit * node ) {
	MUTATE_START( node );

	maybeMutate_impl( node->ctor, *this );
	maybeMutate_impl( node->dtor, *this );
	maybeMutate_impl( node->init, *this );

	MUTATE_END( Initializer, node );
}

//--------------------------------------------------------------------------
// Constant
template< typename pass_type >
void PassVisitor< pass_type >::visit( Constant * node ) {
	VISIT_START( node );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const Constant * node ) {
	VISIT_START( node );

	VISIT_END( node );
}

template< typename pass_type >
Constant * PassVisitor< pass_type >::mutate( Constant * node  )  {
	MUTATE_START( node );

	MUTATE_END( Constant, node );
}

//--------------------------------------------------------------------------
// Attribute
template< typename pass_type >
void PassVisitor< pass_type >::visit( Attribute * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
void PassVisitor< pass_type >::visit( const Attribute * node ) {
	VISIT_START( node );

	maybeAccept_impl( node->parameters, *this );

	VISIT_END( node );
}

template< typename pass_type >
Attribute * PassVisitor< pass_type >::mutate( Attribute * node  )  {
	MUTATE_START( node );

	maybeMutate_impl( node->parameters, *this );

	MUTATE_END( Attribute, node );
}

//--------------------------------------------------------------------------
// TypeSubstitution
template< typename pass_type >
TypeSubstitution * PassVisitor< pass_type >::mutate( TypeSubstitution * node ) {
	MUTATE_START( node );

	for ( auto & p : node->typeEnv ) {
		indexerScopedMutate( p.second, *this );
	}
	for ( auto & p : node->varEnv ) {
		indexerScopedMutate( p.second, *this );
	}

	MUTATE_END( TypeSubstitution, node );
}

#undef VISIT_START
#undef VISIT_END

#undef MUTATE_START
#undef MUTATE_END
