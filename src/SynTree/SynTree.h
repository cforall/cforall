//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// SynTree.h --
//
// Author           : Richard C. Bilson
// Created On       : Mon May 18 07:44:20 2015
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Mar 12 18:56:44 2021
// Update Count     : 13
//

#pragma once

#include <string>
#include <list>
#include <map>
#include <iostream>

class BaseSyntaxNode;

class Declaration;
class DeclarationWithType;
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

class Statement;
class CompoundStmt;
class ExprStmt;
class AsmStmt;
class DirectiveStmt;
class IfStmt;
class WhileStmt;
class ForStmt;
class SwitchStmt;
class CaseStmt;
class BranchStmt;
class ReturnStmt;
class ThrowStmt;
class TryStmt;
class CatchStmt;
class FinallyStmt;
class SuspendStmt;
class WaitForStmt;
class WithStmt;
class NullStmt;
class DeclStmt;
class NullStmt;
class ImplicitCtorDtorStmt;

class Expression;
class ApplicationExpr;
class UntypedExpr;
class NameExpr;
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
class LogicalExpr;
class ConditionalExpr;
class CommaExpr;
class TypeExpr;
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
class ReferenceToType;
class StructInstType;
class UnionInstType;
class EnumInstType;
class TraitInstType;
class TypeInstType;
class TupleType;
class TypeofType;
class AttrType;
class VarArgsType;
class ZeroType;
class OneType;
class GlobalScopeType;

class Designation;
class Initializer;
class SingleInit;
class ListInit;
class ConstructorInit;

//template <class T>	// emulate a union with templates?
class Constant;

// typedef std::string Label;
class Label;
typedef unsigned int UniqueId;

class TypeSubstitution;

// gcc attribute
class Attribute;

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
