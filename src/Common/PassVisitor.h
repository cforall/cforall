#pragma once

// IWYU pragma: private, include "Common/PassVisitor.h"

#include <stack>
#include <type_traits>

#include "Common/Stats.h"
#include "Common/utility.h"

#include "SynTree/Mutator.h"
#include "SynTree/Visitor.h"

#include "SymTab/Indexer.h"

#include "SynTree/Attribute.h"
#include "SynTree/Initializer.h"
#include "SynTree/Statement.h"
#include "SynTree/Type.h"
#include "SynTree/Declaration.h"
#include "SynTree/Expression.h"
#include "SynTree/Constant.h"

class TypeSubstitution;

#include "PassVisitor.proto.h"

//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// Templated visitor type
// To use declare a PassVisitor< YOUR VISITOR TYPE >
// The visitor type should specify the previsit/postvisit/premutate/postmutate for types that are desired.
// Note: previsit/postvisit/premutate/postmutate must be **public** members
//
// Several additional features are available through inheritance
// | WithTypeSubstitution - provides polymorphic TypeSubstitution * env for the current expression
// | WithStmtsToAdd       - provides the ability to insert statements before or after the current statement by adding new statements into
//                          stmtsToAddBefore or stmtsToAddAfter respectively.
// | WithShortCircuiting  - provides the ability to skip visiting child nodes; set visit_children to false in pre{visit,mutate} to skip visiting children
// | WithGuards           - provides the ability to save/restore data like a LIFO stack; to save, call GuardValue with the variable to save, the variable
//                          will automatically be restored to its previous value after the corresponding postvisit/postmutate teminates.
//-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
template< typename pass_type >
class PassVisitor final : public Visitor, public Mutator {
public:

	template< typename... Args >
	PassVisitor(Args &&... args)
		: pass( std::forward<Args>( args )... )
	{
		typedef PassVisitor<pass_type> this_t;
		this_t * const * visitor = visitor_impl(pass, 0);
		if(visitor) {
			*const_cast<this_t **>( visitor ) = this;
		}
	}

	virtual ~PassVisitor() = default;

	pass_type pass;

	virtual void visit( ObjectDecl * objectDecl ) override final;
	virtual void visit( const ObjectDecl * objectDecl ) override final;
	virtual void visit( FunctionDecl * functionDecl ) override final;
	virtual void visit( const FunctionDecl * functionDecl ) override final;
	virtual void visit( StructDecl * aggregateDecl ) override final;
	virtual void visit( const StructDecl * aggregateDecl ) override final;
	virtual void visit( UnionDecl * aggregateDecl ) override final;
	virtual void visit( const UnionDecl * aggregateDecl ) override final;
	virtual void visit( EnumDecl * aggregateDecl ) override final;
	virtual void visit( const EnumDecl * aggregateDecl ) override final;
	virtual void visit( TraitDecl * aggregateDecl ) override final;
	virtual void visit( const TraitDecl * aggregateDecl ) override final;
	virtual void visit( TypeDecl * typeDecl ) override final;
	virtual void visit( const TypeDecl * typeDecl ) override final;
	virtual void visit( TypedefDecl * typeDecl ) override final;
	virtual void visit( const TypedefDecl * typeDecl ) override final;
	virtual void visit( AsmDecl * asmDecl ) override final;
	virtual void visit( const AsmDecl * asmDecl ) override final;
	virtual void visit( DirectiveDecl * directiveDecl ) override final;
	virtual void visit( const DirectiveDecl * directiveDecl ) override final;
	virtual void visit( StaticAssertDecl * assertDecl ) override final;
	virtual void visit( const StaticAssertDecl * assertDecl ) override final;

	virtual void visit( CompoundStmt * compoundStmt ) override final;
	virtual void visit( const CompoundStmt * compoundStmt ) override final;
	virtual void visit( ExprStmt * exprStmt ) override final;
	virtual void visit( const ExprStmt * exprStmt ) override final;
	virtual void visit( AsmStmt * asmStmt ) override final;
	virtual void visit( const AsmStmt * asmStmt ) override final;
	virtual void visit( DirectiveStmt * dirStmt ) override final;
	virtual void visit( const DirectiveStmt * dirStmt ) override final;
	virtual void visit( IfStmt * ifStmt ) override final;
	virtual void visit( const IfStmt * ifStmt ) override final;
	virtual void visit( WhileStmt * whileStmt ) override final;
	virtual void visit( const WhileStmt * whileStmt ) override final;
	virtual void visit( ForStmt * forStmt ) override final;
	virtual void visit( const ForStmt * forStmt ) override final;
	virtual void visit( SwitchStmt * switchStmt ) override final;
	virtual void visit( const SwitchStmt * switchStmt ) override final;
	virtual void visit( CaseStmt * caseStmt ) override final;
	virtual void visit( const CaseStmt * caseStmt ) override final;
	virtual void visit( BranchStmt * branchStmt ) override final;
	virtual void visit( const BranchStmt * branchStmt ) override final;
	virtual void visit( ReturnStmt * returnStmt ) override final;
	virtual void visit( const ReturnStmt * returnStmt ) override final;
	virtual void visit( ThrowStmt * throwStmt ) override final;
	virtual void visit( const ThrowStmt * throwStmt ) override final;
	virtual void visit( TryStmt * tryStmt ) override final;
	virtual void visit( const TryStmt * tryStmt ) override final;
	virtual void visit( CatchStmt * catchStmt ) override final;
	virtual void visit( const CatchStmt * catchStmt ) override final;
	virtual void visit( FinallyStmt * finallyStmt ) override final;
	virtual void visit( const FinallyStmt * finallyStmt ) override final;
	virtual void visit( SuspendStmt * suspendStmt ) override final;
	virtual void visit( const SuspendStmt * suspendStmt ) override final;
	virtual void visit( WaitForStmt * waitforStmt ) override final;
	virtual void visit( const WaitForStmt * waitforStmt ) override final;
	virtual void visit( WithStmt * withStmt ) override final;
	virtual void visit( const WithStmt * withStmt ) override final;
	virtual void visit( NullStmt * nullStmt ) override final;
	virtual void visit( const NullStmt * nullStmt ) override final;
	virtual void visit( DeclStmt * declStmt ) override final;
	virtual void visit( const DeclStmt * declStmt ) override final;
	virtual void visit( ImplicitCtorDtorStmt * impCtorDtorStmt ) override final;
	virtual void visit( const ImplicitCtorDtorStmt * impCtorDtorStmt ) override final;

	virtual void visit( ApplicationExpr * applicationExpr ) override final;
	virtual void visit( const ApplicationExpr * applicationExpr ) override final;
	virtual void visit( UntypedExpr * untypedExpr ) override final;
	virtual void visit( const UntypedExpr * untypedExpr ) override final;
	virtual void visit( NameExpr * nameExpr ) override final;
	virtual void visit( const NameExpr * nameExpr ) override final;
	virtual void visit( CastExpr * castExpr ) override final;
	virtual void visit( const CastExpr * castExpr ) override final;
	virtual void visit( KeywordCastExpr * castExpr ) override final;
	virtual void visit( const KeywordCastExpr * castExpr ) override final;
	virtual void visit( VirtualCastExpr * castExpr ) override final;
	virtual void visit( const VirtualCastExpr * castExpr ) override final;
	virtual void visit( AddressExpr * addressExpr ) override final;
	virtual void visit( const AddressExpr * addressExpr ) override final;
	virtual void visit( LabelAddressExpr * labAddressExpr ) override final;
	virtual void visit( const LabelAddressExpr * labAddressExpr ) override final;
	virtual void visit( UntypedMemberExpr * memberExpr ) override final;
	virtual void visit( const UntypedMemberExpr * memberExpr ) override final;
	virtual void visit( MemberExpr * memberExpr ) override final;
	virtual void visit( const MemberExpr * memberExpr ) override final;
	virtual void visit( VariableExpr * variableExpr ) override final;
	virtual void visit( const VariableExpr * variableExpr ) override final;
	virtual void visit( ConstantExpr * constantExpr ) override final;
	virtual void visit( const ConstantExpr * constantExpr ) override final;
	virtual void visit( SizeofExpr * sizeofExpr ) override final;
	virtual void visit( const SizeofExpr * sizeofExpr ) override final;
	virtual void visit( AlignofExpr * alignofExpr ) override final;
	virtual void visit( const AlignofExpr * alignofExpr ) override final;
	virtual void visit( UntypedOffsetofExpr * offsetofExpr ) override final;
	virtual void visit( const UntypedOffsetofExpr * offsetofExpr ) override final;
	virtual void visit( OffsetofExpr * offsetofExpr ) override final;
	virtual void visit( const OffsetofExpr * offsetofExpr ) override final;
	virtual void visit( OffsetPackExpr * offsetPackExpr ) override final;
	virtual void visit( const OffsetPackExpr * offsetPackExpr ) override final;
	virtual void visit( LogicalExpr * logicalExpr ) override final;
	virtual void visit( const LogicalExpr * logicalExpr ) override final;
	virtual void visit( ConditionalExpr * conditionalExpr ) override final;
	virtual void visit( const ConditionalExpr * conditionalExpr ) override final;
	virtual void visit( CommaExpr * commaExpr ) override final;
	virtual void visit( const CommaExpr * commaExpr ) override final;
	virtual void visit( TypeExpr * typeExpr ) override final;
	virtual void visit( const TypeExpr * typeExpr ) override final;
	virtual void visit( AsmExpr * asmExpr ) override final;
	virtual void visit( const AsmExpr * asmExpr ) override final;
	virtual void visit( ImplicitCopyCtorExpr * impCpCtorExpr ) override final;
	virtual void visit( const ImplicitCopyCtorExpr * impCpCtorExpr ) override final;
	virtual void visit( ConstructorExpr *  ctorExpr ) override final;
	virtual void visit( const ConstructorExpr *  ctorExpr ) override final;
	virtual void visit( CompoundLiteralExpr * compLitExpr ) override final;
	virtual void visit( const CompoundLiteralExpr * compLitExpr ) override final;
	virtual void visit( RangeExpr * rangeExpr ) override final;
	virtual void visit( const RangeExpr * rangeExpr ) override final;
	virtual void visit( UntypedTupleExpr * tupleExpr ) override final;
	virtual void visit( const UntypedTupleExpr * tupleExpr ) override final;
	virtual void visit( TupleExpr * tupleExpr ) override final;
	virtual void visit( const TupleExpr * tupleExpr ) override final;
	virtual void visit( TupleIndexExpr * tupleExpr ) override final;
	virtual void visit( const TupleIndexExpr * tupleExpr ) override final;
	virtual void visit( TupleAssignExpr * assignExpr ) override final;
	virtual void visit( const TupleAssignExpr * assignExpr ) override final;
	virtual void visit( StmtExpr *  stmtExpr ) override final;
	virtual void visit( const StmtExpr *  stmtExpr ) override final;
	virtual void visit( UniqueExpr *  uniqueExpr ) override final;
	virtual void visit( const UniqueExpr *  uniqueExpr ) override final;
	virtual void visit( UntypedInitExpr *  initExpr ) override final;
	virtual void visit( const UntypedInitExpr *  initExpr ) override final;
	virtual void visit( InitExpr *  initExpr ) override final;
	virtual void visit( const InitExpr *  initExpr ) override final;
	virtual void visit( DeletedExpr *  delExpr ) override final;
	virtual void visit( const DeletedExpr *  delExpr ) override final;
	virtual void visit( DefaultArgExpr * argExpr ) override final;
	virtual void visit( const DefaultArgExpr * argExpr ) override final;
	virtual void visit( GenericExpr * genExpr ) override final;
	virtual void visit( const GenericExpr * genExpr ) override final;

	virtual void visit( VoidType * basicType ) override final;
	virtual void visit( const VoidType * basicType ) override final;
	virtual void visit( BasicType * basicType ) override final;
	virtual void visit( const BasicType * basicType ) override final;
	virtual void visit( PointerType * pointerType ) override final;
	virtual void visit( const PointerType * pointerType ) override final;
	virtual void visit( ArrayType * arrayType ) override final;
	virtual void visit( const ArrayType * arrayType ) override final;
	virtual void visit( ReferenceType * referenceType ) override final;
	virtual void visit( const ReferenceType * referenceType ) override final;
	virtual void visit( QualifiedType * qualType ) override final;
	virtual void visit( const QualifiedType * qualType ) override final;
	virtual void visit( FunctionType * functionType ) override final;
	virtual void visit( const FunctionType * functionType ) override final;
	virtual void visit( StructInstType * aggregateUseType ) override final;
	virtual void visit( const StructInstType * aggregateUseType ) override final;
	virtual void visit( UnionInstType * aggregateUseType ) override final;
	virtual void visit( const UnionInstType * aggregateUseType ) override final;
	virtual void visit( EnumInstType * aggregateUseType ) override final;
	virtual void visit( const EnumInstType * aggregateUseType ) override final;
	virtual void visit( TraitInstType * aggregateUseType ) override final;
	virtual void visit( const TraitInstType * aggregateUseType ) override final;
	virtual void visit( TypeInstType * aggregateUseType ) override final;
	virtual void visit( const TypeInstType * aggregateUseType ) override final;
	virtual void visit( TupleType * tupleType ) override final;
	virtual void visit( const TupleType * tupleType ) override final;
	virtual void visit( TypeofType * typeofType ) override final;
	virtual void visit( const TypeofType * typeofType ) override final;
	virtual void visit( AttrType * attrType ) override final;
	virtual void visit( const AttrType * attrType ) override final;
	virtual void visit( VarArgsType * varArgsType ) override final;
	virtual void visit( const VarArgsType * varArgsType ) override final;
	virtual void visit( ZeroType * zeroType ) override final;
	virtual void visit( const ZeroType * zeroType ) override final;
	virtual void visit( OneType * oneType ) override final;
	virtual void visit( const OneType * oneType ) override final;
	virtual void visit( GlobalScopeType * globalType ) override final;
	virtual void visit( const GlobalScopeType * globalType ) override final;

	virtual void visit( Designation * designation ) override final;
	virtual void visit( const Designation * designation ) override final;
	virtual void visit( SingleInit * singleInit ) override final;
	virtual void visit( const SingleInit * singleInit ) override final;
	virtual void visit( ListInit * listInit ) override final;
	virtual void visit( const ListInit * listInit ) override final;
	virtual void visit( ConstructorInit * ctorInit ) override final;
	virtual void visit( const ConstructorInit * ctorInit ) override final;

	virtual void visit( Constant * constant ) override final;
	virtual void visit( const Constant * constant ) override final;

	virtual void visit( Attribute * attribute ) override final;
	virtual void visit( const Attribute * attribute ) override final;

	virtual DeclarationWithType * mutate( ObjectDecl * objectDecl ) override final;
	virtual DeclarationWithType * mutate( FunctionDecl * functionDecl ) override final;
	virtual Declaration * mutate( StructDecl * aggregateDecl ) override final;
	virtual Declaration * mutate( UnionDecl * aggregateDecl ) override final;
	virtual Declaration * mutate( EnumDecl * aggregateDecl ) override final;
	virtual Declaration * mutate( TraitDecl * aggregateDecl ) override final;
	virtual Declaration * mutate( TypeDecl * typeDecl ) override final;
	virtual Declaration * mutate( TypedefDecl * typeDecl ) override final;
	virtual AsmDecl * mutate( AsmDecl * asmDecl ) override final;
	virtual DirectiveDecl * mutate( DirectiveDecl * directiveDecl ) override final;
	virtual StaticAssertDecl * mutate( StaticAssertDecl * assertDecl ) override final;

	virtual CompoundStmt * mutate( CompoundStmt * compoundStmt ) override final;
	virtual Statement * mutate( ExprStmt * exprStmt ) override final;
	virtual Statement * mutate( AsmStmt * asmStmt ) override final;
	virtual Statement * mutate( DirectiveStmt * dirStmt ) override final;
	virtual Statement * mutate( IfStmt * ifStmt ) override final;
	virtual Statement * mutate( WhileStmt * whileStmt ) override final;
	virtual Statement * mutate( ForStmt * forStmt ) override final;
	virtual Statement * mutate( SwitchStmt * switchStmt ) override final;
	virtual Statement * mutate( CaseStmt * caseStmt ) override final;
	virtual Statement * mutate( BranchStmt * branchStmt ) override final;
	virtual Statement * mutate( ReturnStmt * returnStmt ) override final;
	virtual Statement * mutate( ThrowStmt * throwStmt ) override final;
	virtual Statement * mutate( TryStmt * tryStmt ) override final;
	virtual Statement * mutate( CatchStmt * catchStmt ) override final;
	virtual Statement * mutate( FinallyStmt * finallyStmt ) override final;
	virtual Statement * mutate( SuspendStmt * suspendStmt ) override final;
	virtual Statement * mutate( WaitForStmt * waitforStmt ) override final;
	virtual Declaration * mutate( WithStmt * withStmt ) override final;
	virtual NullStmt * mutate( NullStmt * nullStmt ) override final;
	virtual Statement * mutate( DeclStmt * declStmt ) override final;
	virtual Statement * mutate( ImplicitCtorDtorStmt * impCtorDtorStmt ) override final;

	virtual Expression * mutate( ApplicationExpr * applicationExpr ) override final;
	virtual Expression * mutate( UntypedExpr * untypedExpr ) override final;
	virtual Expression * mutate( NameExpr * nameExpr ) override final;
	virtual Expression * mutate( AddressExpr * addrExpr ) override final;
	virtual Expression * mutate( LabelAddressExpr * labAddressExpr ) override final;
	virtual Expression * mutate( CastExpr * castExpr ) override final;
	virtual Expression * mutate( KeywordCastExpr * castExpr ) override final;
	virtual Expression * mutate( VirtualCastExpr * castExpr ) override final;
	virtual Expression * mutate( UntypedMemberExpr * memberExpr ) override final;
	virtual Expression * mutate( MemberExpr * memberExpr ) override final;
	virtual Expression * mutate( VariableExpr * variableExpr ) override final;
	virtual Expression * mutate( ConstantExpr * constantExpr ) override final;
	virtual Expression * mutate( SizeofExpr * sizeofExpr ) override final;
	virtual Expression * mutate( AlignofExpr * alignofExpr ) override final;
	virtual Expression * mutate( UntypedOffsetofExpr * offsetofExpr ) override final;
	virtual Expression * mutate( OffsetofExpr * offsetofExpr ) override final;
	virtual Expression * mutate( OffsetPackExpr * offsetPackExpr ) override final;
	virtual Expression * mutate( LogicalExpr * logicalExpr ) override final;
	virtual Expression * mutate( ConditionalExpr * conditionalExpr ) override final;
	virtual Expression * mutate( CommaExpr * commaExpr ) override final;
	virtual Expression * mutate( TypeExpr * typeExpr ) override final;
	virtual Expression * mutate( AsmExpr * asmExpr ) override final;
	virtual Expression * mutate( ImplicitCopyCtorExpr * impCpCtorExpr ) override final;
	virtual Expression * mutate( ConstructorExpr * ctorExpr ) override final;
	virtual Expression * mutate( CompoundLiteralExpr * compLitExpr ) override final;
	virtual Expression * mutate( RangeExpr * rangeExpr ) override final;
	virtual Expression * mutate( UntypedTupleExpr * tupleExpr ) override final;
	virtual Expression * mutate( TupleExpr * tupleExpr ) override final;
	virtual Expression * mutate( TupleIndexExpr * tupleExpr ) override final;
	virtual Expression * mutate( TupleAssignExpr * assignExpr ) override final;
	virtual Expression * mutate( StmtExpr *  stmtExpr ) override final;
	virtual Expression * mutate( UniqueExpr *  uniqueExpr ) override final;
	virtual Expression * mutate( UntypedInitExpr *  initExpr ) override final;
	virtual Expression * mutate( InitExpr *  initExpr ) override final;
	virtual Expression * mutate( DeletedExpr *  delExpr ) override final;
	virtual Expression * mutate( DefaultArgExpr * argExpr ) override final;
	virtual Expression * mutate( GenericExpr * genExpr ) override final;

	virtual Type * mutate( VoidType * basicType ) override final;
	virtual Type * mutate( BasicType * basicType ) override final;
	virtual Type * mutate( PointerType * pointerType ) override final;
	virtual Type * mutate( ArrayType * arrayType ) override final;
	virtual Type * mutate( ReferenceType * referenceType ) override final;
	virtual Type * mutate( QualifiedType * qualType ) override final;
	virtual Type * mutate( FunctionType * functionType ) override final;
	virtual Type * mutate( StructInstType * aggregateUseType ) override final;
	virtual Type * mutate( UnionInstType * aggregateUseType ) override final;
	virtual Type * mutate( EnumInstType * aggregateUseType ) override final;
	virtual Type * mutate( TraitInstType * aggregateUseType ) override final;
	virtual Type * mutate( TypeInstType * aggregateUseType ) override final;
	virtual Type * mutate( TupleType * tupleType ) override final;
	virtual Type * mutate( TypeofType * typeofType ) override final;
	virtual Type * mutate( AttrType * attrType ) override final;
	virtual Type * mutate( VarArgsType * varArgsType ) override final;
	virtual Type * mutate( ZeroType * zeroType ) override final;
	virtual Type * mutate( OneType * oneType ) override final;
	virtual Type * mutate( GlobalScopeType * globalType ) override final;

	virtual Designation * mutate( Designation * designation ) override final;
	virtual Initializer * mutate( SingleInit * singleInit ) override final;
	virtual Initializer * mutate( ListInit * listInit ) override final;
	virtual Initializer * mutate( ConstructorInit * ctorInit ) override final;

	virtual Constant * mutate( Constant * constant ) override final;

	virtual Attribute * mutate( Attribute * attribute ) override final;

	virtual TypeSubstitution * mutate( TypeSubstitution * sub ) final;

	bool isInFunction() const {
		return inFunction;
	}

private:
	bool inFunction = false;
	bool atFunctionTop = false;

	template<typename pass_t> friend void acceptAll( std::list< Declaration* > &decls, PassVisitor< pass_t >& visitor );
	template<typename pass_t> friend void acceptAll( const std::list< const Declaration * > &decls, PassVisitor< pass_t >& visitor );
	template<typename pass_t> friend void mutateAll( std::list< Declaration* > &decls, PassVisitor< pass_t >& visitor );
	template< typename TreeType, typename pass_t > friend void maybeAccept_impl( TreeType * tree, PassVisitor< pass_t > & visitor );
	template< typename TreeType, typename pass_t > friend void maybeAccept_impl( const TreeType * tree, PassVisitor< pass_t > & visitor );
	template< typename TreeType, typename pass_t > friend void maybeMutate_impl( TreeType *& tree, PassVisitor< pass_t > & mutator );
	template< typename Container, typename pass_t > friend void maybeAccept_impl( Container & container, PassVisitor< pass_t > & visitor );
	template< typename Container, typename pass_t > friend void maybeAccept_impl( const Container & container, PassVisitor< pass_t > & visitor );
	template< typename Container, typename pass_t > friend void maybeMutate_impl( Container & container, PassVisitor< pass_t > & mutator );

	template<typename node_type> void call_previsit ( node_type * node ) { previsit_impl ( pass, node, 0 ); }
	template<typename node_type> void call_previsit ( const node_type * node ) { previsit_impl ( pass, node, 0 ); }
	template<typename node_type> void call_postvisit( node_type * node ) { postvisit_impl( pass, node, 0 ); }
	template<typename node_type> void call_postvisit( const node_type * node ) { postvisit_impl( pass, node, 0 ); }

	template<typename node_type> void call_premutate ( node_type * node ) { premutate_impl( pass, node, 0 ); }
	template<typename return_type, typename node_type> return_type call_postmutate ( node_type * node ) { return postmutate_impl<return_type>( pass, node, 0 ); }

	void call_beginScope() { begin_scope_impl( pass, 0 ); }
	void call_endScope  () { end_scope_impl  ( pass, 0 ); }

	void set_env( TypeSubstitution * env ) { set_env_impl( pass, env, 0); }

	template< typename func_t >
	void handleStatementList( std::list< Statement * > & statements, func_t func );
	void visitStatementList ( std::list< Statement* > &statements );
	void mutateStatementList( std::list< Statement* > &statements );
	void visitStatementList ( const std::list< Statement * > & statements );

	template< typename func_t >
	Statement * handleStatement( Statement * stmt, func_t func );
	Statement * visitStatement ( Statement * stmt );
	Statement * mutateStatement( Statement * stmt );
	void visitStatement ( const Statement * stmt );

	template< typename func_t >
	Expression * handleExpression( Expression * expr, func_t func );
	Expression * visitExpression ( Expression * expr );
	Expression * mutateExpression( Expression * expr );
	void visitExpression ( const Expression * expr );


	auto			 		get_env_ptr    () -> decltype(env_impl( pass, 0)) { return env_impl( pass, 0); }
	std::list< Statement* > * 	get_beforeStmts() { return stmtsToAddBefore_impl( pass, 0); }
	std::list< Statement* > * 	get_afterStmts () { return stmtsToAddAfter_impl ( pass, 0); }
	std::list< Declaration* > * 	get_beforeDecls() { return declsToAddBefore_impl( pass, 0); }
	std::list< Declaration* > * 	get_afterDecls () { return declsToAddAfter_impl ( pass, 0); }

	bool       get_visit_children    () { bool_ref * ptr = visit_children_impl(pass, 0); return ptr ? *ptr : true; }
	bool_ref * get_visit_children_ptr() { return visit_children_impl(pass, 0); }

	void indexerScopeEnter  ()                                    { indexer_impl_enterScope  ( pass, 0       ); }
	void indexerScopeLeave  ()                                    { indexer_impl_leaveScope  ( pass, 0       ); }
	void indexerAddId       ( const DeclarationWithType * node  ) { indexer_impl_addId       ( pass, 0, node ); }
	void indexerAddType     ( const NamedTypeDecl       * node  ) { indexer_impl_addType     ( pass, 0, node ); }
	void indexerAddStruct   ( const std::string         & id    ) { indexer_impl_addStruct   ( pass, 0, id   ); }
	void indexerAddStruct   ( const StructDecl          * node  ) { indexer_impl_addStruct   ( pass, 0, node ); }
	void indexerAddStructFwd( const StructDecl          * node  ) { indexer_impl_addStructFwd( pass, 0, node ); }
	void indexerAddEnum     ( const EnumDecl            * node  ) { indexer_impl_addEnum     ( pass, 0, node ); }
	void indexerAddUnion    ( const std::string         & id    ) { indexer_impl_addUnion    ( pass, 0, id   ); }
	void indexerAddUnion    ( const UnionDecl           * node  ) { indexer_impl_addUnion    ( pass, 0, node ); }
	void indexerAddUnionFwd ( const UnionDecl           * node  ) { indexer_impl_addUnionFwd ( pass, 0, node ); }
	void indexerAddTrait    ( const TraitDecl           * node  ) { indexer_impl_addTrait    ( pass, 0, node ); }
	void indexerAddWith     ( const std::list< Expression * > & exprs, const Declaration * withStmt ) { indexer_impl_addWith( pass, 0, exprs, withStmt ); }


	template< typename TreeType, typename VisitorType >
	friend inline void indexerScopedAccept( TreeType * tree, VisitorType & visitor );

	template< typename TreeType, typename VisitorType >
	friend inline void indexerScopedAccept( const TreeType * tree, VisitorType & visitor );

	template< typename TreeType, typename VisitorType >
	friend inline void indexerScopedMutate( TreeType *& tree, VisitorType & visitor );
};

template<typename pass_type, typename T>
void GuardValue( pass_type * pass, T& val ) {
	pass->at_cleanup( [ val ]( void * newVal ) {
		* static_cast< T * >( newVal ) = val;
	}, static_cast< void * >( & val ) );
}

class WithTypeSubstitution {
protected:
	WithTypeSubstitution() = default;
	~WithTypeSubstitution() = default;

public:
	TypeSubstitution * env = nullptr;
};

class WithConstTypeSubstitution {
protected:
	WithConstTypeSubstitution() = default;
	~WithConstTypeSubstitution() = default;

public:
	const TypeSubstitution * env = nullptr;
};

class WithStmtsToAdd {
protected:
	WithStmtsToAdd() = default;
	~WithStmtsToAdd() = default;

public:
	std::list< Statement* > stmtsToAddBefore;
	std::list< Statement* > stmtsToAddAfter;
};

class WithDeclsToAdd {
protected:
	WithDeclsToAdd() = default;
	~WithDeclsToAdd() {
		assert( declsToAddBefore.empty() );
	}

public:
	std::list< Declaration* > declsToAddBefore;
	std::list< Declaration* > declsToAddAfter;
};

class WithShortCircuiting {
protected:
	WithShortCircuiting() = default;
	~WithShortCircuiting() = default;

public:
	bool_ref visit_children;
};

class WithGuards {
protected:
	WithGuards() = default;
	~WithGuards() = default;

public:
	at_cleanup_t at_cleanup;

	template< typename T >
	void GuardValue( T& val ) {
		at_cleanup( [ val ]( void * newVal ) {
			* static_cast< T * >( newVal ) = val;
		}, static_cast< void * >( & val ) );
	}

	template< typename T >
	void GuardScope( T& val ) {
		val.beginScope();
		at_cleanup( []( void * val ) {
			static_cast< T * >( val )->endScope();
		}, static_cast< void * >( & val ) );
	}

	template< typename Func >
	void GuardAction( Func func ) {
		at_cleanup( [func](__attribute__((unused)) void *) { func(); }, nullptr );
	}
};

template<typename pass_type>
class WithVisitorRef {
protected:
	WithVisitorRef() {}
	~WithVisitorRef() {}

public:
	PassVisitor<pass_type> * const visitor = nullptr;

	bool isInFunction() const {
		return visitor->isInFunction();
	}
};

class WithIndexer {
protected:
	WithIndexer() {}
	~WithIndexer() {}

public:
	SymTab::Indexer indexer;
};

#include "Common/Stats.h"

extern struct PassVisitorStats {
	size_t depth = 0;
	Stats::Counters::MaxCounter<double> * max = nullptr;
	Stats::Counters::AverageCounter<double> * avg = nullptr;
} pass_visitor_stats;

#include "SynTree/TypeSubstitution.h"
#include "PassVisitor.impl.h"
