//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Mutator.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 12 18:35:36 2021
// Update Count     : 18
//
#pragma once

#include <cassert>                 // for assert

#include "Common/SemanticError.h"  // for SemanticError
#include "SynTree/SynTree.h"       // for AST nodes

class Mutator {
  protected:
	Mutator() = default;
	virtual ~Mutator() = default;
  public:
	virtual DeclarationWithType * mutate( ObjectDecl * objectDecl ) = 0;
	virtual DeclarationWithType * mutate( FunctionDecl * functionDecl ) = 0;
	virtual Declaration * mutate( StructDecl * aggregateDecl ) = 0;
	virtual Declaration * mutate( UnionDecl * aggregateDecl ) = 0;
	virtual Declaration * mutate( EnumDecl * aggregateDecl ) = 0;
	virtual Declaration * mutate( TraitDecl * aggregateDecl ) = 0;
	virtual Declaration * mutate( TypeDecl * typeDecl ) = 0;
	virtual Declaration * mutate( TypedefDecl * typeDecl ) = 0;
	virtual AsmDecl * mutate( AsmDecl * asmDecl ) = 0;
	virtual DirectiveDecl * mutate( DirectiveDecl * directiveDecl ) = 0;
	virtual StaticAssertDecl * mutate( StaticAssertDecl * assertDecl ) = 0;

	virtual CompoundStmt * mutate( CompoundStmt * compoundStmt ) = 0;
	virtual Statement * mutate( ExprStmt * exprStmt ) = 0;
	virtual Statement * mutate( AsmStmt * asmStmt ) = 0;
	virtual Statement * mutate( DirectiveStmt * dirStmt ) = 0;
	virtual Statement * mutate( IfStmt * ifStmt ) = 0;
	virtual Statement * mutate( WhileStmt * whileStmt ) = 0;
	virtual Statement * mutate( ForStmt * forStmt ) = 0;
	virtual Statement * mutate( SwitchStmt * switchStmt ) = 0;
	virtual Statement * mutate( CaseStmt * caseStmt ) = 0;
	virtual Statement * mutate( BranchStmt * branchStmt ) = 0;
	virtual Statement * mutate( ReturnStmt * returnStmt ) = 0;
	virtual Statement * mutate( ThrowStmt * throwStmt ) = 0;
	virtual Statement * mutate( TryStmt * tryStmt ) = 0;
	virtual Statement * mutate( CatchStmt * catchStmt ) = 0;
	virtual Statement * mutate( FinallyStmt * catchStmt ) = 0;
	virtual Statement * mutate( SuspendStmt * suspendStmt ) = 0;
	virtual Statement * mutate( WaitForStmt * waitforStmt ) = 0;
	virtual Declaration * mutate( WithStmt * withStmt ) = 0;
	virtual NullStmt * mutate( NullStmt * nullStmt ) = 0;
	virtual Statement * mutate( DeclStmt * declStmt ) = 0;
	virtual Statement * mutate( ImplicitCtorDtorStmt * impCtorDtorStmt ) = 0;

	virtual Expression * mutate( ApplicationExpr * applicationExpr ) = 0;
	virtual Expression * mutate( UntypedExpr * untypedExpr ) = 0;
	virtual Expression * mutate( NameExpr * nameExpr ) = 0;
	virtual Expression * mutate( AddressExpr * addrExpr ) = 0;
	virtual Expression * mutate( LabelAddressExpr * labAddressExpr ) = 0;
	virtual Expression * mutate( CastExpr * castExpr ) = 0;
	virtual Expression * mutate( KeywordCastExpr * castExpr ) = 0;
	virtual Expression * mutate( VirtualCastExpr * castExpr ) = 0;
	virtual Expression * mutate( UntypedMemberExpr * memberExpr ) = 0;
	virtual Expression * mutate( MemberExpr * memberExpr ) = 0;
	virtual Expression * mutate( VariableExpr * variableExpr ) = 0;
	virtual Expression * mutate( ConstantExpr * constantExpr ) = 0;
	virtual Expression * mutate( SizeofExpr * sizeofExpr ) = 0;
	virtual Expression * mutate( AlignofExpr * alignofExpr ) = 0;
	virtual Expression * mutate( UntypedOffsetofExpr * offsetofExpr ) = 0;
	virtual Expression * mutate( OffsetofExpr * offsetofExpr ) = 0;
	virtual Expression * mutate( OffsetPackExpr * offsetPackExpr ) = 0;
	virtual Expression * mutate( LogicalExpr * logicalExpr ) = 0;
	virtual Expression * mutate( ConditionalExpr * conditionalExpr ) = 0;
	virtual Expression * mutate( CommaExpr * commaExpr ) = 0;
	virtual Expression * mutate( TypeExpr * typeExpr ) = 0;
	virtual Expression * mutate( DimensionExpr * dimensionExpr ) = 0;
	virtual Expression * mutate( AsmExpr * asmExpr ) = 0;
	virtual Expression * mutate( ImplicitCopyCtorExpr * impCpCtorExpr ) = 0;
	virtual Expression * mutate( ConstructorExpr * ctorExpr ) = 0;
	virtual Expression * mutate( CompoundLiteralExpr * compLitExpr ) = 0;
	virtual Expression * mutate( RangeExpr * rangeExpr ) = 0;
	virtual Expression * mutate( UntypedTupleExpr * tupleExpr ) = 0;
	virtual Expression * mutate( TupleExpr * tupleExpr ) = 0;
	virtual Expression * mutate( TupleIndexExpr * tupleExpr ) = 0;
	virtual Expression * mutate( TupleAssignExpr * assignExpr ) = 0;
	virtual Expression * mutate( StmtExpr  * stmtExpr ) = 0;
	virtual Expression * mutate( UniqueExpr  * uniqueExpr ) = 0;
	virtual Expression * mutate( UntypedInitExpr  * initExpr ) = 0;
	virtual Expression * mutate( InitExpr  * initExpr ) = 0;
	virtual Expression * mutate( DeletedExpr * delExpr ) = 0;
	virtual Expression * mutate( DefaultArgExpr * argExpr ) = 0;
	virtual Expression * mutate( GenericExpr * genExpr ) = 0;

	virtual Type * mutate( VoidType * basicType ) = 0;
	virtual Type * mutate( BasicType * basicType ) = 0;
	virtual Type * mutate( PointerType * pointerType ) = 0;
	virtual Type * mutate( ArrayType * arrayType ) = 0;
	virtual Type * mutate( ReferenceType * refType ) = 0;
	virtual Type * mutate( QualifiedType * qualType ) = 0;
	virtual Type * mutate( FunctionType * functionType ) = 0;
	virtual Type * mutate( StructInstType * aggregateUseType ) = 0;
	virtual Type * mutate( UnionInstType * aggregateUseType ) = 0;
	virtual Type * mutate( EnumInstType * aggregateUseType ) = 0;
	virtual Type * mutate( TraitInstType * aggregateUseType ) = 0;
	virtual Type * mutate( TypeInstType * aggregateUseType ) = 0;
	virtual Type * mutate( TupleType * tupleType ) = 0;
	virtual Type * mutate( TypeofType * typeofType ) = 0;
	virtual Type * mutate( VTableType * vtableType ) = 0;
	virtual Type * mutate( AttrType * attrType ) = 0;
	virtual Type * mutate( VarArgsType * varArgsType ) = 0;
	virtual Type * mutate( ZeroType * zeroType ) = 0;
	virtual Type * mutate( OneType * oneType ) = 0;
	virtual Type * mutate( GlobalScopeType * globalType ) = 0;

	virtual Designation * mutate( Designation * designation ) = 0 ;
	virtual Initializer * mutate( SingleInit * singleInit ) = 0 ;
	virtual Initializer * mutate( ListInit * listInit ) = 0 ;
	virtual Initializer * mutate( ConstructorInit * ctorInit ) = 0 ;

	virtual Constant * mutate( Constant * constant ) = 0;

	virtual Attribute * mutate( Attribute * attribute ) = 0;

	virtual TypeSubstitution * mutate( TypeSubstitution * sub ) = 0;
};

template< typename TreeType, typename MutatorType >
inline TreeType *maybeMutate( TreeType *tree, MutatorType &mutator ) {
	if ( tree ) {
		TreeType *newnode = dynamic_cast< TreeType * >( tree->acceptMutator( mutator ) );
		assert( newnode );
		return newnode;
	} else {
		return 0;
	} // if
}

template< typename Container, typename MutatorType >
inline void mutateAll( Container &container, MutatorType &mutator ) {
	SemanticErrorException errors;
	for ( typename Container::iterator i = container.begin(); i != container.end(); ++i ) {
		try {
			if ( *i ) {
///		    *i = (*i)->acceptMutator( mutator );
				*i = dynamic_cast< typename Container::value_type >( (*i)->acceptMutator( mutator ) );
				assert( *i );
			} // if
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		} // try
	} // for
	if ( ! errors.isEmpty() ) {
		throw errors;
	} // if
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
