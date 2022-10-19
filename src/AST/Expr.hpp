//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Expr.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Last Modified By : Peter A. Buhr
// Created On       : Fri May 10 10:30:00 2019
// Update Count     : 7
//

#pragma once

#include <cassert>
#include <deque>
#include <map>
#include <string>
#include <utility>        // for move
#include <vector>
#include <optional>

#include "Fwd.hpp"        // for UniqueId
#include "Label.hpp"
#include "Decl.hpp"
#include "ParseNode.hpp"
#include "Visitor.hpp"

// Must be included in *all* AST classes; should be #undef'd at the end of the file
#define MUTATE_FRIEND \
    template<typename node_t> friend node_t * mutate(const node_t * node); \
	template<typename node_t> friend node_t * shallowCopy(const node_t * node);


class ConverterOldToNew;
class ConverterNewToOld;

namespace ast {

/// Contains the ID of a declaration and a type that is derived from that declaration,
/// but subject to decay-to-pointer and type parameter renaming
struct ParamEntry {
	UniqueId decl;
	readonly<Decl> declptr;
	ptr<Type> actualType;
	ptr<Type> formalType;
	ptr<Expr> expr;

	ParamEntry() : decl( 0 ), declptr( nullptr ), actualType( nullptr ), formalType( nullptr ), expr( nullptr ) {}
	ParamEntry(
		UniqueId id, const Decl * declptr, const Type * actual, const Type * formal,
		const Expr * e )
	: decl( id ), declptr( declptr ), actualType( actual ), formalType( formal ), expr( e ) {}
};

/// Pre-resolution list of parameters to infer
using ResnSlots = std::vector<UniqueId>;
/// Post-resolution map of inferred parameters
using InferredParams = std::map< UniqueId, ParamEntry >;

/// Base node for expressions
class Expr : public ParseNode {
public:
	/*
	 * NOTE: the union approach is incorrect until the case of
	 * partial resolution in InferMatcher is eliminated.
	 * it is reverted to allow unresolved and resolved parameters
	 * to coexist in an expression node.
	 */
	struct InferUnion {
		// mode is now unused
		enum { Empty, Slots, Params } mode;
		struct data_t {
			// char def;
			ResnSlots * resnSlots;
			InferredParams * inferParams;

			data_t(): resnSlots(nullptr), inferParams(nullptr) {}
			data_t(const data_t &other) = delete;
			~data_t() {
				delete resnSlots;
				delete inferParams;
			}
		} data;

		/// initializes from other InferUnion
		void init_from( const InferUnion& o ) {
			if (o.data.resnSlots) {
				data.resnSlots = new ResnSlots(*o.data.resnSlots);
			}
			if (o.data.inferParams) {
				data.inferParams = new InferredParams(*o.data.inferParams);
			}
		}

		/// initializes from other InferUnion (move semantics)
		void init_from( InferUnion&& o ) {
			data.resnSlots = o.data.resnSlots;
			data.inferParams = o.data.inferParams;
			o.data.resnSlots = nullptr;
			o.data.inferParams = nullptr;
		}

		InferUnion() : mode(Empty), data() {}
		InferUnion( const InferUnion& o ) : mode( o.mode ), data() { init_from( o ); }
		InferUnion( InferUnion&& o ) : mode( o.mode ), data() { init_from( std::move(o) ); }
		InferUnion& operator= ( const InferUnion& ) = delete;
		InferUnion& operator= ( InferUnion&& ) = delete;

		bool hasSlots() const { return data.resnSlots; }
		bool hasParams() const { return data.inferParams; }

		ResnSlots& resnSlots() {
			if (!data.resnSlots) {
				data.resnSlots = new ResnSlots();
			}
			return *data.resnSlots;
		}

		const ResnSlots& resnSlots() const {
			if (data.resnSlots) {
				return *data.resnSlots;
			}
			assertf(false, "Mode was not already resnSlots");
			abort();
		}

		InferredParams& inferParams() {
			if (!data.inferParams) {
				data.inferParams = new InferredParams();
			}
			return *data.inferParams;
		}

		const InferredParams& inferParams() const {
			if (data.inferParams) {
				return *data.inferParams;
			}
			assertf(false, "Mode was not already Params");
			abort();
		}

		void set_inferParams( InferredParams * ps ) {
			delete data.resnSlots;
			data.resnSlots = nullptr;
			delete data.inferParams;
			data.inferParams = ps;
		}

		/// splices other InferUnion into this one. Will fail if one union is in `Slots` mode
		/// and the other is in `Params`.
		void splice( InferUnion && o ) {
			if (o.data.resnSlots) {
				if (data.resnSlots) {
					data.resnSlots->insert(
						data.resnSlots->end(), o.data.resnSlots->begin(), o.data.resnSlots->end() );
					delete o.data.resnSlots;
				}
				else {
					data.resnSlots = o.data.resnSlots;
				}
				o.data.resnSlots = nullptr;
			}

			if (o.data.inferParams) {
				if (data.inferParams) {
					for ( const auto & p : *o.data.inferParams ) {
						(*data.inferParams)[p.first] = std::move(p.second);
					}
					delete o.data.inferParams;
				}
				else {
					data.inferParams = o.data.inferParams;
				}
				o.data.inferParams = nullptr;
			}
		}
	};

	ptr<Type> result;
	ptr<TypeSubstitution> env;
	InferUnion inferred;
	bool extension = false;

	Expr( const CodeLocation & loc, const Type * res = nullptr )
	: ParseNode( loc ), result( res ), env(), inferred() {}

	Expr * set_extension( bool ex ) { extension = ex; return this; }
	virtual bool get_lvalue() const;

	virtual const Expr * accept( Visitor & v ) const override = 0;
private:
	Expr * clone() const override = 0;
	MUTATE_FRIEND
};

/// The application of a function to a set of parameters.
/// Post-resolver form of `UntypedExpr`
class ApplicationExpr final : public Expr {
public:
	ptr<Expr> func;
	std::vector<ptr<Expr>> args;

	ApplicationExpr( const CodeLocation & loc, const Expr * f, std::vector<ptr<Expr>> && as = {} );

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ApplicationExpr * clone() const override { return new ApplicationExpr{ *this }; }
	MUTATE_FRIEND
};

/// The application of a function to a set of parameters, pre-overload resolution.
class UntypedExpr final : public Expr {
public:
	ptr<Expr> func;
	std::vector<ptr<Expr>> args;

	UntypedExpr( const CodeLocation & loc, const Expr * f, std::vector<ptr<Expr>> && as = {} )
	: Expr( loc ), func( f ), args( std::move(as) ) {}

	bool get_lvalue() const final;

	/// Creates a new dereference expression
	static UntypedExpr * createDeref( const CodeLocation & loc, const Expr * arg );
	/// Creates a new assignment expression
	static UntypedExpr * createAssign( const CodeLocation & loc, const Expr * lhs, const Expr * rhs );
	/// Creates a new call of a variable.
	static UntypedExpr * createCall( const CodeLocation & loc,
		const std::string & name, std::vector<ptr<Expr>> && args );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	UntypedExpr * clone() const override { return new UntypedExpr{ *this }; }
	MUTATE_FRIEND
};

/// A name whose name is as-yet undetermined.
/// May also be used to avoid name mangling in codegen phase.
class NameExpr final : public Expr {
public:
	std::string name;

	NameExpr( const CodeLocation & loc, const std::string & n ) : Expr( loc ), name( n ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	NameExpr * clone() const override { return new NameExpr{ *this }; }
	MUTATE_FRIEND
};

class QualifiedNameExpr final : public Expr {
public:
	ptr<Decl> type_decl;
	std::string name;

	QualifiedNameExpr( const CodeLocation & loc, const Decl * d, const std::string & n ) 
	: Expr( loc ), type_decl( d ), name( n ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	QualifiedNameExpr * clone() const override { return new QualifiedNameExpr{ *this }; }
	MUTATE_FRIEND
};

/// A reference to a named variable.
class VariableExpr final : public Expr {
public:
	readonly<DeclWithType> var;

	VariableExpr( const CodeLocation & loc );
	VariableExpr( const CodeLocation & loc, const DeclWithType * v );

	bool get_lvalue() const final;

	/// generates a function pointer for a given function
	static VariableExpr * functionPointer( const CodeLocation & loc, const FunctionDecl * decl );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	VariableExpr * clone() const override { return new VariableExpr{ *this }; }
	MUTATE_FRIEND
};

/// Address-of expression `&e`
class AddressExpr final : public Expr {
public:
	ptr<Expr> arg;

	AddressExpr( const CodeLocation & loc, const Expr * a );

	/// Generate AddressExpr wrapping given expression at same location
	AddressExpr( const Expr * a ) : AddressExpr( a->location, a ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	AddressExpr * clone() const override { return new AddressExpr{ *this }; }
	MUTATE_FRIEND
};

/// GCC &&label
/// https://gcc.gnu.org/onlinedocs/gcc-3.4.2/gcc/Labels-as-Values.html
class LabelAddressExpr final : public Expr {
public:
	Label arg;

	LabelAddressExpr( const CodeLocation & loc, Label && a );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	LabelAddressExpr * clone() const override { return new LabelAddressExpr{ *this }; }
	MUTATE_FRIEND
};

/// Inidicates whether the cast is introduced by the CFA type system.
/// GeneratedCast for casts that the resolver introduces to force a return type
/// ExplicitCast for casts from user code
/// ExplicitCast for casts from desugaring advanced CFA features into simpler CFA
/// example
///   int * p;     // declaration
///   (float *) p; // use, with subject cast
/// subject cast being GeneratedCast means we are considering an interpretation with a type mismatch
/// subject cast being ExplicitCast means someone in charge wants it that way
enum GeneratedFlag { ExplicitCast, GeneratedCast };

/// A type cast, e.g. `(int)e`
class CastExpr final : public Expr {
public:
	ptr<Expr> arg;
	GeneratedFlag isGenerated;

	CastExpr( const CodeLocation & loc, const Expr * a, const Type * to,
		GeneratedFlag g = GeneratedCast ) : Expr( loc, to ), arg( a ), isGenerated( g ) {}
	/// Cast-to-void
	CastExpr( const CodeLocation & loc, const Expr * a, GeneratedFlag g = GeneratedCast );

	/// Wrap a cast expression around an existing expression (always generated)
	CastExpr( const Expr * a, const Type * to ) : CastExpr( a->location, a, to, GeneratedCast ) {}

	/// Wrap a cast-to-void expression around an existing expression (always generated)
	CastExpr( const Expr * a ) : CastExpr( a->location, a, GeneratedCast ) {}

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	CastExpr * clone() const override { return new CastExpr{ *this }; }
	MUTATE_FRIEND
};

/// A cast to "keyword types", e.g. `(thread &)t`
class KeywordCastExpr final : public Expr {
public:
	ptr<Expr> arg;
	struct Concrete {
		std::string field;
		std::string getter;

		Concrete() = default;
		Concrete(const Concrete &) = default;
	};
	ast::AggregateDecl::Aggregate target;
	Concrete concrete_target;


	KeywordCastExpr( const CodeLocation & loc, const Expr * a, ast::AggregateDecl::Aggregate t )
	: Expr( loc ), arg( a ), target( t ) {}

	KeywordCastExpr( const CodeLocation & loc, const Expr * a, ast::AggregateDecl::Aggregate t, const Concrete & ct )
	: Expr( loc ), arg( a ), target( t ), concrete_target( ct ) {}

	/// Get a name for the target type
	const char * targetString() const;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	KeywordCastExpr * clone() const override { return new KeywordCastExpr{ *this }; }
	MUTATE_FRIEND
};

/// A virtual dynamic cast, e.g. `(virtual exception)e`
class VirtualCastExpr final : public Expr {
public:
	ptr<Expr> arg;

	VirtualCastExpr( const CodeLocation & loc, const Expr * a, const Type * to )
	: Expr( loc, to ), arg( a ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	VirtualCastExpr * clone() const override { return new VirtualCastExpr{ *this }; }
	MUTATE_FRIEND
};

/// A member selection operation before expression resolution, e.g. `q.p`
class UntypedMemberExpr final : public Expr {
public:
	ptr<Expr> member;
	ptr<Expr> aggregate;

	UntypedMemberExpr( const CodeLocation & loc, const Expr * mem, const Expr * agg )
	: Expr( loc ), member( mem ), aggregate( agg ) { assert( aggregate ); }

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	UntypedMemberExpr * clone() const override { return new UntypedMemberExpr{ *this }; }
	MUTATE_FRIEND
};

/// A member selection operation after expression resolution, e.g. `q.p`
class MemberExpr final : public Expr {
public:
	readonly<DeclWithType> member;
	ptr<Expr> aggregate;

	MemberExpr( const CodeLocation & loc, const DeclWithType * mem, const Expr * agg );

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	MemberExpr * clone() const override { return new MemberExpr{ *this }; }
	MUTATE_FRIEND

	// Custructor overload meant only for AST conversion
	enum NoOpConstruction { NoOpConstructionChosen };
	MemberExpr( const CodeLocation & loc, const DeclWithType * mem, const Expr * agg,
	    NoOpConstruction overloadSelector );
	friend class ::ConverterOldToNew;
	friend class ::ConverterNewToOld;
};

/// A compile-time constant.
/// Mostly carries C-source text from parse to code-gen, without interpretation.  E.g. strings keep their outer quotes and never have backslashes interpreted.
/// Integer constants get special treatment, e.g. for verifying array operations, when an integer constant occurs as the length of an array.
class ConstantExpr final : public Expr {
public:
	// Representation of this constant, as it occurs in .cfa source and .cfa.cc result.
	std::string rep;

	ConstantExpr(
		const CodeLocation & loc, const Type * ty, const std::string & r,
			std::optional<unsigned long long> i )
	: Expr( loc, ty ), rep( r ), ival( i ), underlyer(ty) {}

	/// Gets the integer value of this constant, if one is appropriate to its type.
	/// Throws a SemanticError if the type is not appropriate for value-as-integer.
	/// Suffers an assertion failure the type is appropriate but no integer value was supplied to the constructor.
	long long int intValue() const;

	/// Generates a boolean constant of the given bool.
	static ConstantExpr * from_bool( const CodeLocation & loc, bool b );
	/// Generates an integer constant of the given int.
	static ConstantExpr * from_int( const CodeLocation & loc, int i );
	/// Generates an integer constant of the given unsigned long int.
	static ConstantExpr * from_ulong( const CodeLocation & loc, unsigned long i );
	/// Generates a string constant from the given string (char type, unquoted string).
	static ConstantExpr * from_string( const CodeLocation & loc, const std::string & string );
	/// Generates a null pointer value for the given type. void * if omitted.
	static ConstantExpr * null( const CodeLocation & loc, const Type * ptrType = nullptr );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ConstantExpr * clone() const override { return new ConstantExpr{ *this }; }
	MUTATE_FRIEND

	std::optional<unsigned long long> ival;

	// Intended only for legacy support of roundtripping the old AST.
	// Captures the very-locally inferred type, before the resolver modifies the type of this ConstantExpression.
	// In the old AST it's constExpr->constant.type
	ptr<Type> underlyer;
	friend class ::ConverterOldToNew;
	friend class ::ConverterNewToOld;
};

/// sizeof expression, e.g. `sizeof(int)`, `sizeof 3+4`
class SizeofExpr final : public Expr {
public:
	ptr<Expr> expr;
	ptr<Type> type;

	SizeofExpr( const CodeLocation & loc, const Expr * e );
	SizeofExpr( const CodeLocation & loc, const Type * t );
	// deliberately no disambiguating overload for nullptr_t

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	SizeofExpr * clone() const override { return new SizeofExpr{ *this }; }
	MUTATE_FRIEND
};

/// alignof expression, e.g. `alignof(int)`, `alignof 3+4`
class AlignofExpr final : public Expr {
public:
	ptr<Expr> expr;
	ptr<Type> type;

	AlignofExpr( const CodeLocation & loc, const Expr * e );
	AlignofExpr( const CodeLocation & loc, const Type * t );
	// deliberately no disambiguating overload for nullptr_t

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	AlignofExpr * clone() const override { return new AlignofExpr{ *this }; }
	MUTATE_FRIEND
};

/// offsetof expression before resolver determines field, e.g. `offsetof(MyStruct, myfield)`
class UntypedOffsetofExpr final : public Expr {
public:
	ptr<Type> type;
	std::string member;

	UntypedOffsetofExpr( const CodeLocation & loc, const Type * ty, const std::string & mem )
	: Expr( loc ), type( ty ), member( mem ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	UntypedOffsetofExpr * clone() const override { return new UntypedOffsetofExpr{ *this }; }
	MUTATE_FRIEND
};

/// offsetof expression after resolver determines field, e.g. `offsetof(MyStruct, myfield)`
class OffsetofExpr final : public Expr {
public:
	ptr<Type> type;
	readonly<DeclWithType> member;

	OffsetofExpr( const CodeLocation & loc, const Type * ty, const DeclWithType * mem );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	OffsetofExpr * clone() const override { return new OffsetofExpr{ *this }; }
	MUTATE_FRIEND
};

/// a pack of field-offsets for a generic type
class OffsetPackExpr final : public Expr {
public:
	ptr<StructInstType> type;

	OffsetPackExpr( const CodeLocation & loc, const StructInstType * ty );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	OffsetPackExpr * clone() const override { return new OffsetPackExpr{ *this }; }
	MUTATE_FRIEND
};

/// Variants of short-circuiting logical expression
enum LogicalFlag { OrExpr, AndExpr };

/// Short-circuiting boolean expression (`&&` or `||`)
class LogicalExpr final : public Expr {
public:
	ptr<Expr> arg1;
	ptr<Expr> arg2;
	LogicalFlag isAnd;

	LogicalExpr( const CodeLocation & loc, const Expr * a1, const Expr * a2, LogicalFlag ia );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	LogicalExpr * clone() const override { return new LogicalExpr{ *this }; }
	MUTATE_FRIEND
};

/// Three-argument conditional e.g. `p ? a : b`
class ConditionalExpr final : public Expr {
public:
	ptr<Expr> arg1;
	ptr<Expr> arg2;
	ptr<Expr> arg3;

	ConditionalExpr( const CodeLocation & loc, const Expr * a1, const Expr * a2, const Expr * a3 )
	: Expr( loc ), arg1( a1 ), arg2( a2 ), arg3( a3 ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ConditionalExpr * clone() const override { return new ConditionalExpr{ *this }; }
	MUTATE_FRIEND
};

/// Comma expression e.g. `( a , b )`
class CommaExpr final : public Expr {
public:
	ptr<Expr> arg1;
	ptr<Expr> arg2;

	CommaExpr( const CodeLocation & loc, const Expr * a1, const Expr * a2 )
	: Expr( loc ), arg1( a1 ), arg2( a2 ) {
		this->result = a2->result;
	}

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	CommaExpr * clone() const override { return new CommaExpr{ *this }; }
	MUTATE_FRIEND
};

/// A type used as an expression (e.g. a type generator parameter)
class TypeExpr final : public Expr {
public:
	ptr<Type> type;

	TypeExpr( const CodeLocation & loc, const Type * t ) : Expr(loc), type(t) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TypeExpr * clone() const override { return new TypeExpr{ *this }; }
	MUTATE_FRIEND
};

class DimensionExpr final : public Expr {
public:
	std::string name;

	DimensionExpr( const CodeLocation & loc, std::string name )
	: Expr( loc ), name( name ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	DimensionExpr * clone() const override { return new DimensionExpr{ *this }; }
	MUTATE_FRIEND
};

/// A GCC "asm constraint operand" used in an asm statement, e.g. `[output] "=f" (result)`.
/// https://gcc.gnu.org/onlinedocs/gcc-4.7.1/gcc/Machine-Constraints.html#Machine-Constraints
class AsmExpr final : public Expr {
public:
	std::string inout;
	ptr<Expr> constraint;
	ptr<Expr> operand;

	AsmExpr( const CodeLocation & loc, const std::string & io, const Expr * con, const Expr * op )
	: Expr( loc ), inout( io ), constraint( con ), operand( op ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	AsmExpr * clone() const override { return new AsmExpr{ *this }; }
	MUTATE_FRIEND
};

/// The application of a function to a set of parameters, along with a set of copy constructor
/// calls, one for each argument
class ImplicitCopyCtorExpr final : public Expr {
public:
	ptr<ApplicationExpr> callExpr;

	ImplicitCopyCtorExpr( const CodeLocation& loc, const ApplicationExpr * call )
	: Expr( loc, call->result ), callExpr(call) { assert( call ); assert(call->result); }

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ImplicitCopyCtorExpr * clone() const override { return new ImplicitCopyCtorExpr{ *this }; }
	MUTATE_FRIEND
};

/// Constructor in expression context, e.g. `int * x = alloc() { 42 };`
class ConstructorExpr final : public Expr {
public:
	ptr<Expr> callExpr;

	ConstructorExpr( const CodeLocation & loc, const Expr * call );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ConstructorExpr * clone() const override { return new ConstructorExpr{ *this }; }
	MUTATE_FRIEND
};

/// A C99 compound literal, e.g. `(MyType){ a, b, c }`
class CompoundLiteralExpr final : public Expr {
public:
	ptr<Init> init;

	CompoundLiteralExpr( const CodeLocation & loc, const Type * t, const Init * i );

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	CompoundLiteralExpr * clone() const override { return new CompoundLiteralExpr{ *this }; }
	MUTATE_FRIEND
};

/// A range, e.g. `3 ... 5` or `1~10`
class RangeExpr final : public Expr {
public:
	ptr<Expr> low;
	ptr<Expr> high;

	RangeExpr( const CodeLocation & loc, const Expr * l, const Expr * h )
	: Expr( loc ), low( l ), high( h ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	RangeExpr * clone() const override { return new RangeExpr{ *this }; }
	MUTATE_FRIEND
};

/// A tuple expression before resolution, e.g. `[a, b, c]`
class UntypedTupleExpr final : public Expr {
public:
	std::vector<ptr<Expr>> exprs;

	UntypedTupleExpr( const CodeLocation & loc, std::vector<ptr<Expr>> && xs )
	: Expr( loc ), exprs( std::move(xs) ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	UntypedTupleExpr * clone() const override { return new UntypedTupleExpr{ *this }; }
	MUTATE_FRIEND
};

/// A tuple expression after resolution, e.g. `[a, b, c]`
class TupleExpr final : public Expr {
public:
	std::vector<ptr<Expr>> exprs;

	TupleExpr( const CodeLocation & loc, std::vector<ptr<Expr>> && xs );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TupleExpr * clone() const override { return new TupleExpr{ *this }; }
	MUTATE_FRIEND
};

/// An element selection operation on a tuple value, e.g. `t.3` after analysis
class TupleIndexExpr final : public Expr {
public:
	ptr<Expr> tuple;
	unsigned index;

	TupleIndexExpr( const CodeLocation & loc, const Expr * t, unsigned i );

	bool get_lvalue() const final;

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	TupleIndexExpr * clone() const override { return new TupleIndexExpr{ *this }; }
	MUTATE_FRIEND
};

/// A multiple- or mass-assignment operation, or a tuple ctor/dtor expression.
/// multiple-assignment: both sides of the assignment have tuple type,
///     e.g. `[a, b, c] = [d, e, f];`
/// mass-assignment: left-hand side has tuple type and right-hand side does not:
///     e.g. `[a, b, c] = 42;`
class TupleAssignExpr final : public Expr {
public:
	ptr<StmtExpr> stmtExpr;

	TupleAssignExpr(
		const CodeLocation & loc, std::vector<ptr<Expr>> && assigns,
		std::vector<ptr<ObjectDecl>> && tempDecls );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }

	friend class ::ConverterOldToNew;

private:
	TupleAssignExpr * clone() const override { return new TupleAssignExpr{ *this }; }
    TupleAssignExpr( const CodeLocation & loc, const Type * result, const StmtExpr * s );

	MUTATE_FRIEND
};

/// A GCC "statement expression", e.g. `({ int x = 5; x })`
class StmtExpr final : public Expr {
public:
	ptr<CompoundStmt> stmts;
	std::vector<ptr<ObjectDecl>> returnDecls;  ///< return variable(s) for statement expression
	std::vector<ptr<Expr>> dtors;              ///< destructor(s) for return variable(s)

	readonly<ExprStmt> resultExpr;

	StmtExpr( const CodeLocation & loc, const CompoundStmt * ss );

	/// Set the result type of this StmtExpr based on its body
	void computeResult();

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	StmtExpr * clone() const override { return new StmtExpr{ *this }; }
	MUTATE_FRIEND
};

/// An expression which must only be evaluated once
class UniqueExpr final : public Expr {
	static unsigned long long nextId;
public:
	ptr<Expr> expr;
	readonly<ObjectDecl> object;
	ptr<VariableExpr> var;
	unsigned long long id;

	UniqueExpr( const CodeLocation & loc, const Expr * e, unsigned long long i = -1ull );

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	UniqueExpr * clone() const override { return new UniqueExpr{ *this }; }
	MUTATE_FRIEND
};

/// One option for resolving an initializer expression
struct InitAlternative {
	ptr<Type> type;
	ptr<Designation> designation;

	InitAlternative() = default;
	InitAlternative( const Type * ty, const Designation * des ) : type( ty ), designation( des ) {}
};

/// Pre-resolution initializer expression
class UntypedInitExpr final : public Expr {
public:
	ptr<Expr> expr;
	std::deque<InitAlternative> initAlts;

	UntypedInitExpr( const CodeLocation & loc, const Expr * e, std::deque<InitAlternative> && as )
	: Expr( loc ), expr( e ), initAlts( std::move(as) ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	UntypedInitExpr * clone() const override { return new UntypedInitExpr{ *this }; }
	MUTATE_FRIEND
};

/// Post-resolution initializer expression
class InitExpr final : public Expr {
public:
	ptr<Expr> expr;
	ptr<Designation> designation;

	InitExpr( const CodeLocation & loc, const Expr * e, const Designation * des )
	: Expr( loc, e->result ), expr( e ), designation( des ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	InitExpr * clone() const override { return new InitExpr{ *this }; }
	MUTATE_FRIEND
};

/// Expression containing a deleted identifier.
/// Internal to resolver.
class DeletedExpr final : public Expr {
public:
	ptr<Expr> expr;
	readonly<Decl> deleteStmt;

	DeletedExpr( const CodeLocation & loc, const Expr * e, const Decl * del )
	: Expr( loc, e->result ), expr( e ), deleteStmt( del ) { assert( expr->result ); }

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	DeletedExpr * clone() const override { return new DeletedExpr{ *this }; }
	MUTATE_FRIEND
};

/// Use of a default argument.
/// Internal to resolver.
class DefaultArgExpr final : public Expr {
public:
	ptr<Expr> expr;

	DefaultArgExpr( const CodeLocation & loc, const Expr * e )
	: Expr( loc, e->result ), expr( e ) { assert( e->result ); }

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	DefaultArgExpr * clone() const override { return new DefaultArgExpr{ *this }; }
	MUTATE_FRIEND
};

/// C11 _Generic expression
class GenericExpr final : public Expr {
public:
	/// One arm of the _Generic expr
	struct Association {
		ptr<Type> type;
		ptr<Expr> expr;

		Association() = default;
		// default case
		Association( const Expr * e ) : type(), expr( e ) {}
		// non-default case
		Association( const Type * t, const Expr * e ) : type( t ), expr( e ) {}
	};

	ptr<Expr> control;
	std::vector<Association> associations;

	GenericExpr( const CodeLocation & loc, const Expr * ctrl, std::vector<Association> && assns )
	: Expr( loc ), control( ctrl ), associations( std::move(assns) ) {}

	const Expr * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	GenericExpr * clone() const override { return new GenericExpr{ *this }; }
	MUTATE_FRIEND
};


}

#undef MUTATE_FRIEND

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
