//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Visitor.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 09:26:57 2022
// Update Count     : 17
//

#pragma once

#include "Common/SemanticError.h"  // for SemanticError
#include "SynTree.h"               // for AST nodes

class Visitor {
  protected:
	Visitor() = default;
	virtual ~Visitor() = default;
  public:
	// visit: Default implementation of all functions visits the children
	// of the given syntax node, but performs no other action.

	virtual void visit( ObjectDecl * node ) { visit( const_cast<const ObjectDecl *>(node) ); }
	virtual void visit( const ObjectDecl * objectDecl ) = 0;
	virtual void visit( FunctionDecl * node ) { visit( const_cast<const FunctionDecl *>(node) ); }
	virtual void visit( const FunctionDecl * functionDecl ) = 0;
	virtual void visit( StructDecl * node ) { visit( const_cast<const StructDecl *>(node) ); }
	virtual void visit( const StructDecl * aggregateDecl ) = 0;
	virtual void visit( UnionDecl * node ) { visit( const_cast<const UnionDecl *>(node) ); }
	virtual void visit( const UnionDecl * aggregateDecl ) = 0;
	virtual void visit( EnumDecl * node ) { visit( const_cast<const EnumDecl *>(node) ); }
	virtual void visit( const EnumDecl * aggregateDecl ) = 0;
	virtual void visit( TraitDecl * node ) { visit( const_cast<const TraitDecl *>(node) ); }
	virtual void visit( const TraitDecl * aggregateDecl ) = 0;
	virtual void visit( TypeDecl * node ) { visit( const_cast<const TypeDecl *>(node) ); }
	virtual void visit( const TypeDecl * typeDecl ) = 0;
	virtual void visit( TypedefDecl * node ) { visit( const_cast<const TypedefDecl *>(node) ); }
	virtual void visit( const TypedefDecl * typeDecl ) = 0;
	virtual void visit( AsmDecl * node ) { visit( const_cast<const AsmDecl *>(node) ); }
	virtual void visit( const AsmDecl * asmDecl ) = 0;
	virtual void visit( DirectiveDecl * node ) { visit( const_cast<const DirectiveDecl *>(node) ); }
	virtual void visit( const DirectiveDecl * directiveDecl ) = 0;
	virtual void visit( StaticAssertDecl * node ) { visit( const_cast<const StaticAssertDecl *>(node) ); }
	virtual void visit( const StaticAssertDecl * assertDecl ) = 0;

	virtual void visit( CompoundStmt * node ) { visit( const_cast<const CompoundStmt *>(node) ); }
	virtual void visit( const CompoundStmt * compoundStmt ) = 0;
	virtual void visit( ExprStmt * node ) { visit( const_cast<const ExprStmt *>(node) ); }
	virtual void visit( const ExprStmt * exprStmt ) = 0;
	virtual void visit( AsmStmt * node ) { visit( const_cast<const AsmStmt *>(node) ); }
	virtual void visit( const AsmStmt * asmStmt ) = 0;
	virtual void visit( DirectiveStmt * node ) { visit( const_cast<const DirectiveStmt *>(node) ); }
	virtual void visit( const DirectiveStmt * directiveStmt ) = 0;
	virtual void visit( IfStmt * node ) { visit( const_cast<const IfStmt *>(node) ); }
	virtual void visit( const IfStmt * ifStmt ) = 0;
	virtual void visit( WhileDoStmt * node ) { visit( const_cast<const WhileDoStmt *>(node) ); }
	virtual void visit( const WhileDoStmt * whileDoStmt ) = 0;
	virtual void visit( ForStmt * node ) { visit( const_cast<const ForStmt *>(node) ); }
	virtual void visit( const ForStmt * forStmt ) = 0;
	virtual void visit( SwitchStmt * node ) { visit( const_cast<const SwitchStmt *>(node) ); }
	virtual void visit( const SwitchStmt * switchStmt ) = 0;
	virtual void visit( CaseStmt * node ) { visit( const_cast<const CaseStmt *>(node) ); }
	virtual void visit( const CaseStmt * caseStmt ) = 0;
	virtual void visit( BranchStmt * node ) { visit( const_cast<const BranchStmt *>(node) ); }
	virtual void visit( const BranchStmt * branchStmt ) = 0;
	virtual void visit( ReturnStmt * node ) { visit( const_cast<const ReturnStmt *>(node) ); }
	virtual void visit( const ReturnStmt * returnStmt ) = 0;
	virtual void visit( ThrowStmt * node ) { visit( const_cast<const ThrowStmt *>(node) ); }
	virtual void visit( const ThrowStmt * throwStmt ) = 0;
	virtual void visit( TryStmt * node ) { visit( const_cast<const TryStmt *>(node) ); }
	virtual void visit( const TryStmt * tryStmt ) = 0;
	virtual void visit( CatchStmt * node ) { visit( const_cast<const CatchStmt *>(node) ); }
	virtual void visit( const CatchStmt * catchStmt ) = 0;
	virtual void visit( FinallyStmt * node ) { visit( const_cast<const FinallyStmt *>(node) ); }
	virtual void visit( const FinallyStmt * finallyStmt ) = 0;
	virtual void visit( SuspendStmt * node ) { visit( const_cast<const SuspendStmt *>(node) ); }
	virtual void visit( const SuspendStmt * suspendStmt ) = 0;
	virtual void visit( WaitForStmt * node ) { visit( const_cast<const WaitForStmt *>(node) ); }
	virtual void visit( const WaitForStmt * waitforStmt ) = 0;
	virtual void visit( WithStmt * node ) { visit( const_cast<const WithStmt *>(node) ); }
	virtual void visit( const WithStmt * withStmt ) = 0;
	virtual void visit( NullStmt * node ) { visit( const_cast<const NullStmt *>(node) ); }
	virtual void visit( const NullStmt * nullStmt ) = 0;
	virtual void visit( DeclStmt * node ) { visit( const_cast<const DeclStmt *>(node) ); }
	virtual void visit( const DeclStmt * declStmt ) = 0;
	virtual void visit( ImplicitCtorDtorStmt * node ) { visit( const_cast<const ImplicitCtorDtorStmt *>(node) ); }
	virtual void visit( const ImplicitCtorDtorStmt * impCtorDtorStmt ) = 0;
	virtual void visit( MutexStmt * node ) { visit( const_cast<const MutexStmt *>(node) ); }
	virtual void visit( const MutexStmt * mutexStmt ) = 0;

	virtual void visit( ApplicationExpr * node ) { visit( const_cast<const ApplicationExpr *>(node) ); }
	virtual void visit( const ApplicationExpr * applicationExpr ) = 0;
	virtual void visit( UntypedExpr * node ) { visit( const_cast<const UntypedExpr *>(node) ); }
	virtual void visit( const UntypedExpr * untypedExpr ) = 0;
	virtual void visit( NameExpr * node ) { visit( const_cast<const NameExpr *>(node) ); }
	virtual void visit( const NameExpr * nameExpr ) = 0;
	virtual void visit( CastExpr * node ) { visit( const_cast<const CastExpr *>(node) ); }
	virtual void visit( const CastExpr * castExpr ) = 0;
	virtual void visit( KeywordCastExpr * node ) { visit( const_cast<const KeywordCastExpr *>(node) ); }
	virtual void visit( const KeywordCastExpr * castExpr ) = 0;
	virtual void visit( VirtualCastExpr * node ) { visit( const_cast<const VirtualCastExpr *>(node) ); }
	virtual void visit( const VirtualCastExpr * castExpr ) = 0;
	virtual void visit( AddressExpr * node ) { visit( const_cast<const AddressExpr *>(node) ); }
	virtual void visit( const AddressExpr * addressExpr ) = 0;
	virtual void visit( LabelAddressExpr * node ) { visit( const_cast<const LabelAddressExpr *>(node) ); }
	virtual void visit( const LabelAddressExpr * labAddressExpr ) = 0;
	virtual void visit( UntypedMemberExpr * node ) { visit( const_cast<const UntypedMemberExpr *>(node) ); }
	virtual void visit( const UntypedMemberExpr * memberExpr ) = 0;
	virtual void visit( MemberExpr * node ) { visit( const_cast<const MemberExpr *>(node) ); }
	virtual void visit( const MemberExpr * memberExpr ) = 0;
	virtual void visit( VariableExpr * node ) { visit( const_cast<const VariableExpr *>(node) ); }
	virtual void visit( const VariableExpr * variableExpr ) = 0;
	virtual void visit( ConstantExpr * node ) { visit( const_cast<const ConstantExpr *>(node) ); }
	virtual void visit( const ConstantExpr * constantExpr ) = 0;
	virtual void visit( SizeofExpr * node ) { visit( const_cast<const SizeofExpr *>(node) ); }
	virtual void visit( const SizeofExpr * sizeofExpr ) = 0;
	virtual void visit( AlignofExpr * node ) { visit( const_cast<const AlignofExpr *>(node) ); }
	virtual void visit( const AlignofExpr * alignofExpr ) = 0;
	virtual void visit( UntypedOffsetofExpr * node ) { visit( const_cast<const UntypedOffsetofExpr *>(node) ); }
	virtual void visit( const UntypedOffsetofExpr * offsetofExpr ) = 0;
	virtual void visit( OffsetofExpr * node ) { visit( const_cast<const OffsetofExpr *>(node) ); }
	virtual void visit( const OffsetofExpr * offsetofExpr ) = 0;
	virtual void visit( OffsetPackExpr * node ) { visit( const_cast<const OffsetPackExpr *>(node) ); }
	virtual void visit( const OffsetPackExpr * offsetPackExpr ) = 0;
	virtual void visit( LogicalExpr * node ) { visit( const_cast<const LogicalExpr *>(node) ); }
	virtual void visit( const LogicalExpr * logicalExpr ) = 0;
	virtual void visit( ConditionalExpr * node ) { visit( const_cast<const ConditionalExpr *>(node) ); }
	virtual void visit( const ConditionalExpr * conditionalExpr ) = 0;
	virtual void visit( CommaExpr * node ) { visit( const_cast<const CommaExpr *>(node) ); }
	virtual void visit( const CommaExpr * commaExpr ) = 0;
	virtual void visit( TypeExpr * node ) { visit( const_cast<const TypeExpr *>(node) ); }
	virtual void visit( const TypeExpr * typeExpr ) = 0;
	virtual void visit( DimensionExpr * node ) { visit( const_cast<const DimensionExpr *>(node) ); }
	virtual void visit( const DimensionExpr * typeExpr ) = 0;
	virtual void visit( AsmExpr * node ) { visit( const_cast<const AsmExpr *>(node) ); }
	virtual void visit( const AsmExpr * asmExpr ) = 0;
	virtual void visit( ImplicitCopyCtorExpr * node ) { visit( const_cast<const ImplicitCopyCtorExpr *>(node) ); }
	virtual void visit( const ImplicitCopyCtorExpr * impCpCtorExpr ) = 0;
	virtual void visit( ConstructorExpr * node ) { visit( const_cast<const ConstructorExpr *>(node) ); }
	virtual void visit( const ConstructorExpr *  ctorExpr ) = 0;
	virtual void visit( CompoundLiteralExpr * node ) { visit( const_cast<const CompoundLiteralExpr *>(node) ); }
	virtual void visit( const CompoundLiteralExpr * compLitExpr ) = 0;
	virtual void visit( RangeExpr * node ) { visit( const_cast<const RangeExpr *>(node) ); }
	virtual void visit( const RangeExpr * rangeExpr ) = 0;
	virtual void visit( UntypedTupleExpr * node ) { visit( const_cast<const UntypedTupleExpr *>(node) ); }
	virtual void visit( const UntypedTupleExpr * tupleExpr ) = 0;
	virtual void visit( TupleExpr * node ) { visit( const_cast<const TupleExpr *>(node) ); }
	virtual void visit( const TupleExpr * tupleExpr ) = 0;
	virtual void visit( TupleIndexExpr * node ) { visit( const_cast<const TupleIndexExpr *>(node) ); }
	virtual void visit( const TupleIndexExpr * tupleExpr ) = 0;
	virtual void visit( TupleAssignExpr * node ) { visit( const_cast<const TupleAssignExpr *>(node) ); }
	virtual void visit( const TupleAssignExpr * assignExpr ) = 0;
	virtual void visit( StmtExpr * node ) { visit( const_cast<const StmtExpr *>(node) ); }
	virtual void visit( const StmtExpr *  stmtExpr ) = 0;
	virtual void visit( UniqueExpr * node ) { visit( const_cast<const UniqueExpr *>(node) ); }
	virtual void visit( const UniqueExpr *  uniqueExpr ) = 0;
	virtual void visit( UntypedInitExpr * node ) { visit( const_cast<const UntypedInitExpr *>(node) ); }
	virtual void visit( const UntypedInitExpr *  initExpr ) = 0;
	virtual void visit( InitExpr * node ) { visit( const_cast<const InitExpr *>(node) ); }
	virtual void visit( const InitExpr *  initExpr ) = 0;
	virtual void visit( DeletedExpr * node ) { visit( const_cast<const DeletedExpr *>(node) ); }
	virtual void visit( const DeletedExpr * delExpr ) = 0;
	virtual void visit( DefaultArgExpr * node ) { visit( const_cast<const DefaultArgExpr *>(node) ); }
	virtual void visit( const DefaultArgExpr * argExpr ) = 0;
	virtual void visit( GenericExpr * node ) { visit( const_cast<const GenericExpr *>(node) ); }
	virtual void visit( const GenericExpr * genExpr ) = 0;

	virtual void visit( VoidType * node ) { visit( const_cast<const VoidType *>(node) ); }
	virtual void visit( const VoidType * basicType ) = 0;
	virtual void visit( BasicType * node ) { visit( const_cast<const BasicType *>(node) ); }
	virtual void visit( const BasicType * basicType ) = 0;
	virtual void visit( PointerType * node ) { visit( const_cast<const PointerType *>(node) ); }
	virtual void visit( const PointerType * pointerType ) = 0;
	virtual void visit( ArrayType * node ) { visit( const_cast<const ArrayType *>(node) ); }
	virtual void visit( const ArrayType * arrayType ) = 0;
	virtual void visit( ReferenceType * node ) { visit( const_cast<const ReferenceType *>(node) ); }
	virtual void visit( const ReferenceType * refType ) = 0;
	virtual void visit( QualifiedType * node ) { visit( const_cast<const QualifiedType *>(node) ); }
	virtual void visit( const QualifiedType * qualType ) = 0;
	virtual void visit( FunctionType * node ) { visit( const_cast<const FunctionType *>(node) ); }
	virtual void visit( const FunctionType * functionType ) = 0;
	virtual void visit( StructInstType * node ) { visit( const_cast<const StructInstType *>(node) ); }
	virtual void visit( const StructInstType * aggregateUseType ) = 0;
	virtual void visit( UnionInstType * node ) { visit( const_cast<const UnionInstType *>(node) ); }
	virtual void visit( const UnionInstType * aggregateUseType ) = 0;
	virtual void visit( EnumInstType * node ) { visit( const_cast<const EnumInstType *>(node) ); }
	virtual void visit( const EnumInstType * aggregateUseType ) = 0;
	virtual void visit( TraitInstType * node ) { visit( const_cast<const TraitInstType *>(node) ); }
	virtual void visit( const TraitInstType * aggregateUseType ) = 0;
	virtual void visit( TypeInstType * node ) { visit( const_cast<const TypeInstType *>(node) ); }
	virtual void visit( const TypeInstType * aggregateUseType ) = 0;
	virtual void visit( TupleType * node ) { visit( const_cast<const TupleType *>(node) ); }
	virtual void visit( const TupleType * tupleType ) = 0;
	virtual void visit( TypeofType * node ) { visit( const_cast<const TypeofType *>(node) ); }
	virtual void visit( const TypeofType * typeofType ) = 0;
	virtual void visit( VTableType * node ) { visit( const_cast<const VTableType *>(node) ); }
	virtual void visit( const VTableType * vtableType ) = 0;
	virtual void visit( AttrType * node ) { visit( const_cast<const AttrType *>(node) ); }
	virtual void visit( const AttrType * attrType ) = 0;
	virtual void visit( VarArgsType * node ) { visit( const_cast<const VarArgsType *>(node) ); }
	virtual void visit( const VarArgsType * varArgsType ) = 0;
	virtual void visit( ZeroType * node ) { visit( const_cast<const ZeroType *>(node) ); }
	virtual void visit( const ZeroType * zeroType ) = 0;
	virtual void visit( OneType * node ) { visit( const_cast<const OneType *>(node) ); }
	virtual void visit( const OneType * oneType ) = 0;
	virtual void visit( GlobalScopeType * node ) { visit( const_cast<const GlobalScopeType *>(node) ); }
	virtual void visit( const GlobalScopeType * globalType ) = 0;

	virtual void visit( Designation * node ) { visit( const_cast<const Designation *>(node) ); }
	virtual void visit( const Designation * designation ) = 0;
	virtual void visit( SingleInit * node ) { visit( const_cast<const SingleInit *>(node) ); }
	virtual void visit( const SingleInit * singleInit ) = 0;
	virtual void visit( ListInit * node ) { visit( const_cast<const ListInit *>(node) ); }
	virtual void visit( const ListInit * listInit ) = 0;
	virtual void visit( ConstructorInit * node ) { visit( const_cast<const ConstructorInit *>(node) ); }
	virtual void visit( const ConstructorInit * ctorInit ) = 0;

	virtual void visit( Constant * node ) { visit( const_cast<const Constant *>(node) ); }
	virtual void visit( const Constant * constant ) = 0;

	virtual void visit( Attribute * node ) { visit( const_cast<const Attribute *>(node) ); }
	virtual void visit( const Attribute * attribute ) = 0;
};

template< typename TreeType, typename VisitorType >
inline void maybeAccept( TreeType * tree, VisitorType & visitor ) {
	if ( tree ) {
		tree->accept( visitor );
	}
}

template< typename TreeType, typename VisitorType >
inline void maybeAccept( const TreeType * tree, VisitorType & visitor ) {
	if ( tree ) {
		tree->accept( visitor );
	}
}

template< typename Container, typename VisitorType >
inline void acceptAll( Container & container, VisitorType & visitor ) {
	SemanticErrorException errors;
	for ( auto * i : container ) {
		try {
			if ( i ) {
				i->accept( visitor );
			}
		} catch( SemanticErrorException & e ) {
			errors.append( e );
		}
	}
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

template< typename Container, typename VisitorType >
inline void acceptAll( const Container & container, VisitorType & visitor ) {
	SemanticErrorException errors;
	for ( const auto * i : container ) {
		try {
			if ( i ) {
				i->accept( visitor );
			}
		} catch( SemanticErrorException &e ) {
			errors.append( e );
		}
	}
	if ( ! errors.isEmpty() ) {
		throw errors;
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
