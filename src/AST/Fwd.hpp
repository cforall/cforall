//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Fwd.hpp -- Forward declarations of AST Types.
//
// Author           : Andrew Beach
// Created On       : Wed May  8 16:05:00 2019
// Last Modified By : Peter A. Buhr
// Last Modified On : Tue Feb  1 09:08:33 2022
// Update Count     : 5
//

#pragma once

template<typename> struct bitfield;

#include "AST/Node.hpp"

namespace ast {

class ParseNode;

class Decl;
class DeclWithType;
class ObjectDecl;
class FunctionDecl;
class AggregateDecl;
class StructDecl;
class UnionDecl;
class EnumDecl;
class TraitDecl;
class NamedTypeDecl;
class TypeDecl;
class TypedefDecl;
class AsmDecl;
class DirectiveDecl;
class StaticAssertDecl;
class InlineMemberDecl;

class Stmt;
class CompoundStmt;
class ExprStmt;
class AsmStmt;
class DirectiveStmt;
class IfStmt;
class WhileDoStmt;
class ForStmt;
class SwitchStmt;
class CaseClause;
class BranchStmt;
class ReturnStmt;
class ThrowStmt;
class TryStmt;
class CatchClause;
class FinallyClause;
class SuspendStmt;
class WhenClause;
class WaitForStmt;
class WaitForClause;
class WaitUntilStmt;
class WithStmt;
class DeclStmt;
class NullStmt;
class ImplicitCtorDtorStmt;
class MutexStmt;
class CorunStmt;
class CoforStmt;

class Expr;
class ApplicationExpr;
class UntypedExpr;
class NameExpr;
class QualifiedNameExpr;
class AddressExpr;
class LabelAddressExpr;
class CastExpr;
class KeywordCastExpr;
class VirtualCastExpr;
class MemberExpr;
class UntypedMemberExpr;
class VariableExpr;
class ConstantExpr;
class SizeofExpr;
class AlignofExpr;
class UntypedOffsetofExpr;
class OffsetofExpr;
class OffsetPackExpr;
class EnumPosExpr;
class LogicalExpr;
class ConditionalExpr;
class CommaExpr;
class TypeExpr;
class DimensionExpr;
class AsmExpr;
class ImplicitCopyCtorExpr;
class ConstructorExpr;
class CompoundLiteralExpr;
class RangeExpr;
class UntypedTupleExpr;
class TupleExpr;
class TupleIndexExpr;
class TupleAssignExpr;
class StmtExpr;
class UniqueExpr;
class UntypedInitExpr;
class InitExpr;
class DeletedExpr;
class DefaultArgExpr;
class GenericExpr;

class Type;
class VoidType;
class BasicType;
class PointerType;
class ArrayType;
class ReferenceType;
class QualifiedType;
class FunctionType;
class BaseInstType;
template<typename decl_t> class SueInstType;
using StructInstType = SueInstType<StructDecl>;
using UnionInstType = SueInstType<UnionDecl>;
using EnumInstType = SueInstType<EnumDecl>;
class TraitInstType;
class TypeInstType;
class TupleType;
class TypeofType;
class VTableType;
class VarArgsType;
class ZeroType;
class OneType;
class GlobalScopeType;

class Designation;
class Init;
class SingleInit;
class ListInit;
class ConstructorInit;

class Label;

class Attribute;

class SymbolTable;
class TypeEnvironment;
class TypeSubstitution;

typedef unsigned int UniqueId;

class TranslationUnit;
class TranslationGlobal;

// For the following types, only use the using type.
namespace CV {
	struct qualifier_flags;
	using Qualifiers = bitfield<qualifier_flags>;
}
namespace Function {
	struct spec_flags;
	using Specs = bitfield<spec_flags>;
}
namespace Storage {
	struct class_flags;
	using Classes = bitfield<class_flags>;
}
namespace Linkage {
	struct spec_flags;
	using Spec = bitfield<spec_flags>;
}

}
