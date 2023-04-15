//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Autogen.cc --
//
// Author           : Rob Schluntz
// Created On       : Thu Mar 03 15:45:56 2016
// Last Modified By : Peter A. Buhr
// Last Modified On : Fri Apr 14 15:03:00 2023
// Update Count     : 64
//

#include "Autogen.h"

#include <algorithm>               // for count_if
#include <cassert>                 // for strict_dynamic_cast, assert, assertf
#include <iterator>                // for back_insert_iterator, back_inserter
#include <list>                    // for list, _List_iterator, list<>::iter...
#include <set>                     // for set, _Rb_tree_const_iterator
#include <utility>                 // for pair
#include <vector>                  // for vector

#include "AST/Decl.hpp"
#include "CodeGen/OperatorTable.h" // for isCtorDtor, isCtorDtorAssign
#include "Common/PassVisitor.h"    // for PassVisitor
#include "Common/ScopedMap.h"      // for ScopedMap<>::const_iterator, Scope...
#include "Common/utility.h"        // for cloneAll, operator+
#include "GenPoly/ScopedSet.h"     // for ScopedSet, ScopedSet<>::iterator
#include "InitTweak/GenInit.h"     // for fixReturnStatements
#include "ResolvExpr/Resolver.h"   // for resolveDecl
#include "SymTab/Mangler.h"        // for Mangler
#include "SynTree/Attribute.h"     // For Attribute
#include "SynTree/Mutator.h"       // for maybeMutate
#include "SynTree/Statement.h"     // for CompoundStmt, ReturnStmt, ExprStmt
#include "SynTree/Type.h"          // for FunctionType, Type, TypeInstType
#include "SynTree/Visitor.h"       // for maybeAccept, Visitor, acceptAll
#include "CompilationState.h"

class Attribute;

namespace SymTab {
	/// Data used to generate functions generically. Specifically, the name of the generated function and a function which generates the routine protoype
	struct FuncData {
		typedef FunctionType * (*TypeGen)( Type *, bool );
		FuncData( const std::string & fname, const TypeGen & genType ) : fname( fname ), genType( genType ) {}
		std::string fname;
		TypeGen genType;
	};

	struct AutogenerateRoutines final : public WithDeclsToAdd, public WithVisitorRef<AutogenerateRoutines>, public WithGuards, public WithShortCircuiting, public WithIndexer {
		AutogenerateRoutines();

		void previsit( EnumDecl * enumDecl );
		void previsit( StructDecl * structDecl );
		void previsit( UnionDecl * structDecl );
		void previsit( TypeDecl * typeDecl );
		void previsit( TraitDecl * traitDecl );
		void previsit( FunctionDecl * functionDecl );

		void previsit( CompoundStmt * compoundStmt );

	  private:

		GenPoly::ScopedSet< std::string > structsDone;
		unsigned int functionNesting = 0;     // current level of nested functions

		std::vector< FuncData > data;
	};

	/// generates routines for tuple types.
	struct AutogenTupleRoutines : public WithDeclsToAdd, public WithVisitorRef<AutogenTupleRoutines>, public WithGuards, public WithShortCircuiting {
		void previsit( FunctionDecl * functionDecl );

		void postvisit( TupleType * tupleType );

		void previsit( CompoundStmt * compoundStmt );

	  private:
		unsigned int functionNesting = 0;     // current level of nested functions
		GenPoly::ScopedSet< std::string > seenTuples;
	};

	void autogenerateRoutines( std::list< Declaration * > &translationUnit ) {
		PassVisitor<AutogenerateRoutines> generator;
		acceptAll( translationUnit, generator );

		// needs to be done separately because AutogenerateRoutines skips types that appear as function arguments, etc.
		// AutogenTupleRoutines tupleGenerator;
		// acceptAll( translationUnit, tupleGenerator );
	}

	//=============================================================================================
	// FuncGenerator definitions
	//=============================================================================================
	class FuncGenerator {
	public:
		std::list< Declaration * > definitions, forwards;

		FuncGenerator( Type * type, const std::vector< FuncData > & data, unsigned int functionNesting, SymTab::Indexer & indexer ) : type( type ), data( data ), functionNesting( functionNesting ), indexer( indexer ) {}

		virtual bool shouldAutogen() const = 0;
		void genStandardFuncs();
		virtual void genFieldCtors() = 0;
	protected:
		Type * type;
		const std::vector< FuncData > & data;
		unsigned int functionNesting;
		SymTab::Indexer & indexer;

		virtual void genFuncBody( FunctionDecl * dcl ) = 0;
		virtual bool isConcurrentType() const = 0;

		void resolve( FunctionDecl * dcl );
		void generatePrototypes( std::list< FunctionDecl * > & newFuncs );
	};

	class StructFuncGenerator : public FuncGenerator {
		StructDecl * aggregateDecl;
	public:
		StructFuncGenerator( StructDecl * aggregateDecl, StructInstType * refType, const std::vector< FuncData > & data,  unsigned int functionNesting, SymTab::Indexer & indexer ) : FuncGenerator( refType, data, functionNesting, indexer ), aggregateDecl( aggregateDecl) {}

		virtual bool shouldAutogen() const override;
		virtual bool isConcurrentType() const override;

		virtual void genFuncBody( FunctionDecl * dcl ) override;
		virtual void genFieldCtors() override;

	private:
		/// generates a single struct member operation (constructor call, destructor call, assignment call)
		void makeMemberOp( ObjectDecl * dstParam, Expression * src, DeclarationWithType * field, FunctionDecl * func, bool forward = true );

		/// generates the body of a struct function by iterating the struct members (via parameters) - generates default ctor, copy ctor, assignment, and dtor bodies, but NOT field ctor bodies
		template<typename Iterator>
		void makeFunctionBody( Iterator member, Iterator end, FunctionDecl * func, bool forward = true );

		/// generate the body of a constructor which takes parameters that match fields, e.g.
		/// void ?{}(A *, int) and void?{}(A *, int, int) for a struct A which has two int fields.
		template<typename Iterator>
		void makeFieldCtorBody( Iterator member, Iterator end, FunctionDecl * func );
	};

	class UnionFuncGenerator : public FuncGenerator {
		UnionDecl * aggregateDecl;
	public:
		UnionFuncGenerator( UnionDecl * aggregateDecl, UnionInstType * refType, const std::vector< FuncData > & data,  unsigned int functionNesting, SymTab::Indexer & indexer ) : FuncGenerator( refType, data, functionNesting, indexer ), aggregateDecl( aggregateDecl) {}

		virtual bool shouldAutogen() const override;
		virtual bool isConcurrentType() const override;

		virtual void genFuncBody( FunctionDecl * dcl ) override;
		virtual void genFieldCtors() override;

	private:
		/// generates a single struct member operation (constructor call, destructor call, assignment call)
		template<typename OutputIterator>
		void makeMemberOp( ObjectDecl * srcParam, ObjectDecl * dstParam, OutputIterator out );

		/// generates the body of a struct function by iterating the struct members (via parameters) - generates default ctor, copy ctor, assignment, and dtor bodies, but NOT field ctor bodies
		template<typename Iterator>
		void makeFunctionBody( Iterator member, Iterator end, FunctionDecl * func, bool forward = true );

		/// generate the body of a constructor which takes parameters that match fields, e.g.
		/// void ?{}(A *, int) and void?{}(A *, int, int) for a struct A which has two int fields.
		template<typename Iterator>
		void makeFieldCtorBody( Iterator member, Iterator end, FunctionDecl * func );
	};

	class EnumFuncGenerator : public FuncGenerator {
	public:
		EnumFuncGenerator( EnumInstType * refType, const std::vector< FuncData > & data,  unsigned int functionNesting, SymTab::Indexer & indexer ) : FuncGenerator( refType, data, functionNesting, indexer ) {}

		virtual bool shouldAutogen() const override;
		virtual bool isConcurrentType() const override;

		virtual void genFuncBody( FunctionDecl * dcl ) override;
		virtual void genFieldCtors() override;

	private:
	};

	class TypeFuncGenerator : public FuncGenerator {
		TypeDecl * typeDecl;
	public:
		TypeFuncGenerator( TypeDecl * typeDecl, TypeInstType * refType, const std::vector<FuncData> & data, unsigned int functionNesting, SymTab::Indexer & indexer ) : FuncGenerator( refType, data, functionNesting, indexer ), typeDecl( typeDecl ) {}

		virtual bool shouldAutogen() const override;
		virtual void genFuncBody( FunctionDecl * dcl ) override;
		virtual bool isConcurrentType() const override;
		virtual void genFieldCtors() override;
	};

	//=============================================================================================
	// helper functions
	//=============================================================================================
	void generateFunctions( FuncGenerator & gen, std::list< Declaration * > & declsToAdd ) {
		if ( ! gen.shouldAutogen() ) return;

		// generate each of the functions based on the supplied FuncData objects
		gen.genStandardFuncs();
		gen.genFieldCtors();

		declsToAdd.splice( declsToAdd.end(), gen.forwards );
		declsToAdd.splice( declsToAdd.end(), gen.definitions );
	}

	bool isUnnamedBitfield( ObjectDecl * obj ) {
		return obj != nullptr && obj->name == "" && obj->bitfieldWidth != nullptr;
	}

	/// inserts a forward declaration for functionDecl into declsToAdd
	void addForwardDecl( FunctionDecl * functionDecl, std::list< Declaration * > & declsToAdd ) {
		FunctionDecl * decl = functionDecl->clone();
		delete decl->statements;
		decl->statements = nullptr;
		declsToAdd.push_back( decl );
		decl->fixUniqueId();
	}

	const std::list< TypeDecl * > getGenericParams( Type * t ) {
		std::list< TypeDecl * > * ret = nullptr;
		if ( StructInstType * inst = dynamic_cast< StructInstType * > ( t ) ) {
			ret = inst->get_baseParameters();
		} else if ( UnionInstType * inst = dynamic_cast< UnionInstType * >( t ) ) {
			ret = inst->get_baseParameters();
		}
		return ret ? *ret : std::list< TypeDecl * >();
	}

	/// given type T, generate type of default ctor/dtor, i.e. function type void (*) (T *)
	FunctionType * genDefaultType( Type * paramType, bool maybePolymorphic ) {
		FunctionType *ftype = new FunctionType( Type::Qualifiers(), false );
		if ( maybePolymorphic ) {
			// only copy in
			const auto & typeParams = getGenericParams( paramType );
			cloneAll( typeParams, ftype->forall );
		}
		ObjectDecl *dstParam = new ObjectDecl( "_dst", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, new ReferenceType( Type::Qualifiers(), paramType->clone() ), nullptr );
		ftype->parameters.push_back( dstParam );
		return ftype;
	}

	/// given type T, generate type of copy ctor, i.e. function type void (*) (T *, T)
	FunctionType * genCopyType( Type * paramType, bool maybePolymorphic ) {
		FunctionType *ftype = genDefaultType( paramType, maybePolymorphic );
		ObjectDecl *srcParam = new ObjectDecl( "_src", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, paramType->clone(), nullptr );
		ftype->parameters.push_back( srcParam );
		return ftype;
	}

	/// given type T, generate type of assignment, i.e. function type T (*) (T *, T)
	FunctionType * genAssignType( Type * paramType, bool maybePolymorphic ) {
		FunctionType *ftype = genCopyType( paramType, maybePolymorphic );
		ObjectDecl *returnVal = new ObjectDecl( "_ret", Type::StorageClasses(), LinkageSpec::Cforall, nullptr, paramType->clone(), nullptr );
		ftype->returnVals.push_back( returnVal );
		return ftype;
	}

	/// generate a function decl from a name and type. Nesting depth determines whether
	/// the declaration is static or not; optional paramter determines if declaration is intrinsic
	FunctionDecl * genFunc( const std::string & fname, FunctionType * ftype, unsigned int functionNesting, bool isIntrinsic = false  ) {
		// Routines at global scope marked "static" to prevent multiple definitions in separate translation units
		// because each unit generates copies of the default routines for each aggregate.
		Type::StorageClasses scs = functionNesting > 0 ? Type::StorageClasses() : Type::StorageClasses( Type::Static );
		LinkageSpec::Spec spec = isIntrinsic ? LinkageSpec::Intrinsic : LinkageSpec::AutoGen;
		FunctionDecl * decl = new FunctionDecl( fname, scs, spec, ftype, new CompoundStmt(),
												std::list< Attribute * >(), Type::FuncSpecifiers( Type::Inline ) );
		decl->fixUniqueId();
		return decl;
	}

	Type * declToType( Declaration * decl ) {
		if ( DeclarationWithType * dwt = dynamic_cast< DeclarationWithType * >( decl ) ) {
			return dwt->get_type();
		}
		return nullptr;
	}

	Type * declToTypeDeclBase( Declaration * decl ) {
		if ( TypeDecl * td = dynamic_cast< TypeDecl * >( decl ) ) {
			return td->base;
		}
		return nullptr;
	}

	//=============================================================================================
	// FuncGenerator member definitions
	//=============================================================================================
	void FuncGenerator::genStandardFuncs() {
		std::list< FunctionDecl * > newFuncs;
		generatePrototypes( newFuncs );

		for ( FunctionDecl * dcl : newFuncs ) {
			genFuncBody( dcl );
			if ( CodeGen::isAssignment( dcl->name ) ) {
				// assignment needs to return a value
				FunctionType * assignType = dcl->type;
				assert( assignType->parameters.size() == 2 );
				assert( assignType->returnVals.size() == 1 );
				ObjectDecl * dstParam = strict_dynamic_cast< ObjectDecl * >( assignType->parameters.front() );
				dcl->statements->push_back( new ReturnStmt( new VariableExpr( dstParam ) ) );
			}
			resolve( dcl );
		}
	}

	void FuncGenerator::generatePrototypes( std::list< FunctionDecl * > & newFuncs ) {
		bool concurrent_type = isConcurrentType();
		for ( const FuncData & d : data ) {
			// generate a function (?{}, ?=?, ^?{}) based on the current FuncData.
			FunctionType * ftype = d.genType( type, true );

			// destructor for concurrent type must be mutex
			if ( concurrent_type && CodeGen::isDestructor( d.fname ) ) {
				ftype->parameters.front()->get_type()->set_mutex( true );
			}

			newFuncs.push_back( genFunc( d.fname, ftype, functionNesting ) );
		}
	}

	void FuncGenerator::resolve( FunctionDecl * dcl ) {
		try {
			if (!useNewAST) // attempt to delay resolver call
				ResolvExpr::resolveDecl( dcl, indexer );
			if ( functionNesting == 0 ) {
				// forward declare if top-level struct, so that
				// type is complete as soon as its body ends
				// Note: this is necessary if we want structs which contain
				// generic (otype) structs as members.
				addForwardDecl( dcl, forwards );
			}
			definitions.push_back( dcl );
			indexer.addId( dcl );
		} catch ( SemanticErrorException & ) {
			// okay if decl does not resolve - that means the function should not be generated
			// delete dcl;
			delete dcl->statements;
			dcl->statements = nullptr;
			dcl->isDeleted = true;
			definitions.push_back( dcl );
			indexer.addId( dcl );
		}
	}

	bool StructFuncGenerator::shouldAutogen() const {
		// Builtins do not use autogeneration.
		return ! aggregateDecl->linkage.is_builtin;
	}
	bool StructFuncGenerator::isConcurrentType() const { return aggregateDecl->is_thread() || aggregateDecl->is_monitor(); }

	void StructFuncGenerator::genFuncBody( FunctionDecl * dcl ) {
		// generate appropriate calls to member ctor, assignment
		// destructor needs to do everything in reverse, so pass "forward" based on whether the function is a destructor
		if ( ! CodeGen::isDestructor( dcl->name ) ) {
			makeFunctionBody( aggregateDecl->members.begin(), aggregateDecl->members.end(), dcl );
		} else {
			makeFunctionBody( aggregateDecl->members.rbegin(), aggregateDecl->members.rend(), dcl, false );
		}
	}

	void StructFuncGenerator::genFieldCtors() {
		// field ctors are only generated if default constructor and copy constructor are both generated
		unsigned numCtors = std::count_if( definitions.begin(), definitions.end(), [](Declaration * dcl) { return CodeGen::isConstructor( dcl->name ); } );

		// Field constructors are only generated if default and copy constructor
		// are generated, since they need access to both
		if ( numCtors != 2 ) return;

		// create constructors which take each member type as a parameter.
		// for example, for struct A { int x, y; }; generate
		//   void ?{}(A *, int) and void ?{}(A *, int, int)
		FunctionType * memCtorType = genDefaultType( type );
		for ( Declaration * member : aggregateDecl->members ) {
			DeclarationWithType * field = strict_dynamic_cast<DeclarationWithType *>( member );
			if ( isUnnamedBitfield( dynamic_cast< ObjectDecl * > ( field ) ) ) {
				// don't make a function whose parameter is an unnamed bitfield
				continue;
			}
			// do not carry over field's attributes to parameter type
			Type * paramType = field->get_type()->clone();
			deleteAll( paramType->attributes );
			paramType->attributes.clear();
			// add a parameter corresponding to this field
			ObjectDecl * param = new ObjectDecl( field->name, Type::StorageClasses(), LinkageSpec::Cforall, nullptr, paramType, nullptr );
			cloneAll_if( field->attributes, param->attributes, [](Attribute * attr) { return attr->isValidOnFuncParam(); } );
			memCtorType->parameters.push_back( param );
			FunctionDecl * ctor = genFunc( "?{}", memCtorType->clone(), functionNesting );
			makeFieldCtorBody( aggregateDecl->members.begin(), aggregateDecl->members.end(), ctor );
			resolve( ctor );
		}
		delete memCtorType;
	}

	void StructFuncGenerator::makeMemberOp( ObjectDecl * dstParam, Expression * src, DeclarationWithType * field, FunctionDecl * func, bool forward ) {
		InitTweak::InitExpander_old srcParam( src );

		// assign to destination
		Expression *dstselect = new MemberExpr( field, new CastExpr( new VariableExpr( dstParam ), strict_dynamic_cast< ReferenceType* >( dstParam->get_type() )->base->clone() ) );
		genImplicitCall( srcParam, dstselect, func->name, back_inserter( func->statements->kids ), field, forward );
	}

	template<typename Iterator>
	void StructFuncGenerator::makeFunctionBody( Iterator member, Iterator end, FunctionDecl * func, bool forward ) {
		for ( ; member != end; ++member ) {
			if ( DeclarationWithType *field = dynamic_cast< DeclarationWithType * >( *member ) ) { // otherwise some form of type declaration, e.g. Aggregate
				// query the type qualifiers of this field and skip assigning it if it is marked const.
				// If it is an array type, we need to strip off the array layers to find its qualifiers.
				Type * type = field->get_type();
				while ( ArrayType * at = dynamic_cast< ArrayType * >( type ) ) {
					type = at->get_base();
				}

				if ( type->get_const() && CodeGen::isAssignment( func->name ) ) {
					// don't assign const members, but do construct/destruct
					continue;
				}

				assert( ! func->get_functionType()->get_parameters().empty() );
				ObjectDecl * dstParam = dynamic_cast<ObjectDecl*>( func->get_functionType()->get_parameters().front() );
				ObjectDecl * srcParam = nullptr;
				if ( func->get_functionType()->get_parameters().size() == 2 ) {
					srcParam = dynamic_cast<ObjectDecl*>( func->get_functionType()->get_parameters().back() );
				}

				// srcParam may be NULL, in which case we have default ctor/dtor
				assert( dstParam );

				Expression *srcselect = srcParam ? new MemberExpr( field, new VariableExpr( srcParam ) ) : nullptr;
				makeMemberOp( dstParam, srcselect, field, func, forward );
			} // if
		} // for
	} // makeFunctionBody

	template<typename Iterator>
	void StructFuncGenerator::makeFieldCtorBody( Iterator member, Iterator end, FunctionDecl * func ) {
		FunctionType * ftype = func->type;
		std::list<DeclarationWithType*> & params = ftype->parameters;
		assert( params.size() >= 2 );  // should not call this function for default ctor, etc.

		// skip 'this' parameter
		ObjectDecl * dstParam = dynamic_cast<ObjectDecl*>( params.front() );
		assert( dstParam );
		std::list<DeclarationWithType*>::iterator parameter = params.begin()+1;
		for ( ; member != end; ++member ) {
			if ( DeclarationWithType * field = dynamic_cast<DeclarationWithType*>( *member ) ) {
				if ( isUnnamedBitfield( dynamic_cast< ObjectDecl * > ( field ) ) ) {
					// don't make a function whose parameter is an unnamed bitfield
					continue;
				} else if ( parameter != params.end() ) {
					// matching parameter, initialize field with copy ctor
					Expression *srcselect = new VariableExpr(*parameter);
					makeMemberOp( dstParam, srcselect, field, func );
					++parameter;
				} else {
					// no matching parameter, initialize field with default ctor
					makeMemberOp( dstParam, nullptr, field, func );
				}
			}
		}
	}

	bool UnionFuncGenerator::shouldAutogen() const {
		// Builtins do not use autogeneration.
		return ! aggregateDecl->linkage.is_builtin;
	}

	// xxx - is this right?
	bool UnionFuncGenerator::isConcurrentType() const { return false; };

	/// generate a single union assignment expression (using memcpy)
	template< typename OutputIterator >
	void UnionFuncGenerator::makeMemberOp( ObjectDecl * srcParam, ObjectDecl * dstParam, OutputIterator out ) {
		UntypedExpr *copy = new UntypedExpr( new NameExpr( "__builtin_memcpy" ) );
		copy->args.push_back( new AddressExpr( new VariableExpr( dstParam ) ) );
		copy->args.push_back( new AddressExpr( new VariableExpr( srcParam ) ) );
		copy->args.push_back( new SizeofExpr( srcParam->get_type()->clone() ) );
		*out++ = new ExprStmt( copy );
	}

	/// generates the body of a union assignment/copy constructor/field constructor
	void UnionFuncGenerator::genFuncBody( FunctionDecl * funcDecl ) {
		FunctionType * ftype = funcDecl->type;
		if ( InitTweak::isCopyConstructor( funcDecl ) || InitTweak::isAssignment( funcDecl ) ) {
			assert( ftype->parameters.size() == 2 );
			ObjectDecl * dstParam = strict_dynamic_cast< ObjectDecl * >( ftype->parameters.front() );
			ObjectDecl * srcParam = strict_dynamic_cast< ObjectDecl * >( ftype->parameters.back() );
			makeMemberOp( srcParam, dstParam, back_inserter( funcDecl->statements->kids ) );
		} else {
			// default ctor/dtor body is empty - add unused attribute to parameter to silence warnings
			assert( ftype->parameters.size() == 1 );
			ObjectDecl * dstParam = strict_dynamic_cast< ObjectDecl * >( ftype->parameters.front() );
			dstParam->attributes.push_back( new Attribute( "unused" ) );
		}
	}

	/// generate the body of a constructor which takes parameters that match fields, e.g.
	/// void ?{}(A *, int) and void?{}(A *, int, int) for a struct A which has two int fields.
	void UnionFuncGenerator::genFieldCtors() {
		// field ctors are only generated if default constructor and copy constructor are both generated
		unsigned numCtors = std::count_if( definitions.begin(), definitions.end(), [](Declaration * dcl) { return CodeGen::isConstructor( dcl->get_name() ); } );

		// Field constructors are only generated if default and copy constructor
		// are generated, since they need access to both
		if ( numCtors != 2 ) return;

		// create a constructor which takes the first member type as a parameter.
		// for example, for Union A { int x; double y; }; generate
		// void ?{}(A *, int)
		// This is to mimic C's behaviour which initializes the first member of the union.
		FunctionType * memCtorType = genDefaultType( type );
		for ( Declaration * member : aggregateDecl->members ) {
			DeclarationWithType * field = strict_dynamic_cast<DeclarationWithType *>( member );
			if ( isUnnamedBitfield( dynamic_cast< ObjectDecl * > ( field ) ) ) {
				// don't make a function whose parameter is an unnamed bitfield
				break;
			}
			// do not carry over field's attributes to parameter type
			Type * paramType = field->get_type()->clone();
			deleteAll( paramType->attributes );
			paramType->attributes.clear();
			// add a parameter corresponding to this field
			memCtorType->parameters.push_back( new ObjectDecl( field->name, Type::StorageClasses(), LinkageSpec::Cforall, nullptr, paramType, nullptr ) );
			FunctionDecl * ctor = genFunc( "?{}", memCtorType->clone(), functionNesting );
			ObjectDecl * srcParam = strict_dynamic_cast<ObjectDecl *>( ctor->type->parameters.back() );
			srcParam->fixUniqueId();
			ObjectDecl * dstParam = InitTweak::getParamThis( ctor->type );
			makeMemberOp( srcParam, dstParam, back_inserter( ctor->statements->kids ) );
			resolve( ctor );
			// only generate one field ctor for unions
			break;
		}
		delete memCtorType;
	}

	void EnumFuncGenerator::genFuncBody( FunctionDecl * funcDecl ) {
		// xxx - Temporary: make these functions intrinsic so they codegen as C assignment.
		// Really they're something of a cross between instrinsic and autogen, so should
		// probably make a new linkage type
		funcDecl->linkage = LinkageSpec::Intrinsic;
		FunctionType * ftype = funcDecl->type;
		if ( InitTweak::isCopyConstructor( funcDecl ) || InitTweak::isAssignment( funcDecl ) ) {
			assert( ftype->parameters.size() == 2 );
			ObjectDecl * dstParam = strict_dynamic_cast< ObjectDecl * >( ftype->parameters.front() );
			ObjectDecl * srcParam = strict_dynamic_cast< ObjectDecl * >( ftype->parameters.back() );

			// enum copy construct and assignment is just C-style assignment.
			// this looks like a bad recursive call, but code gen will turn it into
			// a C-style assignment.
			// This happens before function pointer type conversion, so need to do it manually here
			ApplicationExpr * callExpr = new ApplicationExpr( VariableExpr::functionPointer( funcDecl ) );
			callExpr->get_args().push_back( new VariableExpr( dstParam ) );
			callExpr->get_args().push_back( new VariableExpr( srcParam ) );
			funcDecl->statements->push_back( new ExprStmt( callExpr ) );
		} else {
			// default ctor/dtor body is empty - add unused attribute to parameter to silence warnings
			assert( ftype->parameters.size() == 1 );
			ObjectDecl * dstParam = strict_dynamic_cast< ObjectDecl * >( ftype->parameters.front() );
			dstParam->attributes.push_back( new Attribute( "unused" ) );
		}
	}

	bool EnumFuncGenerator::shouldAutogen() const { return true; }
	bool EnumFuncGenerator::isConcurrentType() const { return false; }
	// enums do not have field constructors
	void EnumFuncGenerator::genFieldCtors() {}

	bool TypeFuncGenerator::shouldAutogen() const { return true; };

	void TypeFuncGenerator::genFuncBody( FunctionDecl * dcl ) {
		FunctionType * ftype = dcl->type;
		assertf( ftype->parameters.size() == 1 || ftype->parameters.size() == 2, "Incorrect number of parameters in autogenerated typedecl function: %zd", ftype->parameters.size() );
		DeclarationWithType * dst = ftype->parameters.front();
		DeclarationWithType * src = ftype->parameters.size() == 2 ? ftype->parameters.back() : nullptr;
		// generate appropriate calls to member ctor, assignment
		UntypedExpr * expr = new UntypedExpr( new NameExpr( dcl->name ) );
		expr->args.push_back( new CastExpr( new VariableExpr( dst ), new ReferenceType( Type::Qualifiers(), typeDecl->base->clone() ) ) );
		if ( src ) expr->args.push_back( new CastExpr( new VariableExpr( src ), typeDecl->base->clone() ) );
		dcl->statements->kids.push_back( new ExprStmt( expr ) );
	};

	// xxx - should reach in and determine if base type is concurrent?
	bool TypeFuncGenerator::isConcurrentType() const { return false; };

	// opaque types do not have field constructors
	void TypeFuncGenerator::genFieldCtors() {};

	//=============================================================================================
	// Visitor definitions
	//=============================================================================================
	AutogenerateRoutines::AutogenerateRoutines() {
		// the order here determines the order that these functions are generated.
		// assignment should come last since it uses copy constructor in return.
		data.emplace_back( "?{}", genDefaultType );
		data.emplace_back( "?{}", genCopyType );
		data.emplace_back( "^?{}", genDefaultType );
		data.emplace_back( "?=?", genAssignType );
	}

	void AutogenerateRoutines::previsit( EnumDecl * enumDecl ) {
		// must visit children (enum constants) to add them to the indexer
		if ( enumDecl->has_body() ) {
			EnumInstType enumInst( Type::Qualifiers(), enumDecl->get_name() );
			enumInst.set_baseEnum( enumDecl );
			EnumFuncGenerator gen( &enumInst, data, functionNesting, indexer );
			generateFunctions( gen, declsToAddAfter );
		}
	}

	void AutogenerateRoutines::previsit( StructDecl * structDecl ) {
		visit_children = false;
		if ( structDecl->has_body() ) {
			StructInstType structInst( Type::Qualifiers(), structDecl->name );
			structInst.set_baseStruct( structDecl );
			for ( TypeDecl * typeDecl : structDecl->parameters ) {
				structInst.parameters.push_back( new TypeExpr( new TypeInstType( Type::Qualifiers(), typeDecl->name, typeDecl ) ) );
			}
			StructFuncGenerator gen( structDecl, &structInst, data, functionNesting, indexer );
			generateFunctions( gen, declsToAddAfter );
		} // if
	}

	void AutogenerateRoutines::previsit( UnionDecl * unionDecl ) {
		visit_children = false;
		if ( unionDecl->has_body()  ) {
			UnionInstType unionInst( Type::Qualifiers(), unionDecl->get_name() );
			unionInst.set_baseUnion( unionDecl );
			for ( TypeDecl * typeDecl : unionDecl->get_parameters() ) {
				unionInst.get_parameters().push_back( new TypeExpr( new TypeInstType( Type::Qualifiers(), typeDecl->get_name(), typeDecl ) ) );
			}
			UnionFuncGenerator gen( unionDecl, &unionInst, data, functionNesting, indexer );
			generateFunctions( gen, declsToAddAfter );
		} // if
	}

	// generate ctor/dtors/assign for typedecls, e.g., otype T = int *;
	void AutogenerateRoutines::previsit( TypeDecl * typeDecl ) {
		if ( ! typeDecl->base ) return;

		TypeInstType refType( Type::Qualifiers(), typeDecl->name, typeDecl );
		TypeFuncGenerator gen( typeDecl, &refType, data, functionNesting, indexer );
		generateFunctions( gen, declsToAddAfter );

	}

	void AutogenerateRoutines::previsit( TraitDecl * ) {
		// ensure that we don't add assignment ops for types defined as part of the trait
		visit_children = false;
	}

	void AutogenerateRoutines::previsit( FunctionDecl * ) {
		// Track whether we're currently in a function.
		// Can ignore function type idiosyncrasies, because function type can never
		// declare a new type.
		functionNesting += 1;
		GuardAction( [this]()  { functionNesting -= 1; } );
	}

	void AutogenerateRoutines::previsit( CompoundStmt * ) {
		GuardScope( structsDone );
	}

	void makeTupleFunctionBody( FunctionDecl * function ) {
		FunctionType * ftype = function->get_functionType();
		assertf( ftype->get_parameters().size() == 1 || ftype->get_parameters().size() == 2, "too many parameters in generated tuple function" );

		UntypedExpr * untyped = new UntypedExpr( new NameExpr( function->get_name() ) );

		/// xxx - &* is used to make this easier for later passes to handle
		untyped->get_args().push_back( new AddressExpr( UntypedExpr::createDeref( new VariableExpr( ftype->get_parameters().front() ) ) ) );
		if ( ftype->get_parameters().size() == 2 ) {
			untyped->get_args().push_back( new VariableExpr( ftype->get_parameters().back() ) );
		}
		function->get_statements()->get_kids().push_back( new ExprStmt( untyped ) );
		function->get_statements()->get_kids().push_back( new ReturnStmt( UntypedExpr::createDeref( new VariableExpr( ftype->get_parameters().front() ) ) ) );
	}

	void AutogenTupleRoutines::postvisit( TupleType * tupleType ) {
		std::string mangleName = SymTab::Mangler::mangleType( tupleType );
		if ( seenTuples.find( mangleName ) != seenTuples.end() ) return;
		seenTuples.insert( mangleName );

		// T ?=?(T *, T);
		FunctionType *assignType = genAssignType( tupleType );

		// void ?{}(T *); void ^?{}(T *);
		FunctionType *ctorType = genDefaultType( tupleType );
		FunctionType *dtorType = genDefaultType( tupleType );

		// void ?{}(T *, T);
		FunctionType *copyCtorType = genCopyType( tupleType );

		std::set< TypeDecl* > done;
		std::list< TypeDecl * > typeParams;
		for ( Type * t : *tupleType ) {
			if ( TypeInstType * ty = dynamic_cast< TypeInstType * >( t ) ) {
				if ( ! done.count( ty->get_baseType() ) ) {
					TypeDecl * newDecl = new TypeDecl( ty->get_baseType()->get_name(), Type::StorageClasses(), nullptr, TypeDecl::Dtype, true );
					TypeInstType * inst = new TypeInstType( Type::Qualifiers(), newDecl->get_name(), newDecl );
					newDecl->get_assertions().push_back( new FunctionDecl( "?=?", Type::StorageClasses(), LinkageSpec::Cforall, genAssignType( inst ), nullptr,
																		   std::list< Attribute * >(), Type::FuncSpecifiers( Type::Inline ) ) );
					newDecl->get_assertions().push_back( new FunctionDecl( "?{}", Type::StorageClasses(), LinkageSpec::Cforall, genDefaultType( inst ), nullptr,
																		   std::list< Attribute * >(), Type::FuncSpecifiers( Type::Inline ) ) );
					newDecl->get_assertions().push_back( new FunctionDecl( "?{}", Type::StorageClasses(), LinkageSpec::Cforall, genCopyType( inst ), nullptr,
																		   std::list< Attribute * >(), Type::FuncSpecifiers( Type::Inline ) ) );
					newDecl->get_assertions().push_back( new FunctionDecl( "^?{}", Type::StorageClasses(), LinkageSpec::Cforall, genDefaultType( inst ), nullptr,
																		   std::list< Attribute * >(), Type::FuncSpecifiers( Type::Inline ) ) );
					typeParams.push_back( newDecl );
					done.insert( ty->get_baseType() );
				}
			}
		}
		cloneAll( typeParams, ctorType->get_forall() );
		cloneAll( typeParams, dtorType->get_forall() );
		cloneAll( typeParams, copyCtorType->get_forall() );
		cloneAll( typeParams, assignType->get_forall() );

		FunctionDecl *assignDecl = genFunc( "?=?", assignType, functionNesting );
		FunctionDecl *ctorDecl = genFunc( "?{}", ctorType, functionNesting );
		FunctionDecl *copyCtorDecl = genFunc( "?{}", copyCtorType, functionNesting );
		FunctionDecl *dtorDecl = genFunc( "^?{}", dtorType, functionNesting );

		makeTupleFunctionBody( assignDecl );
		makeTupleFunctionBody( ctorDecl );
		makeTupleFunctionBody( copyCtorDecl );
		makeTupleFunctionBody( dtorDecl );

		declsToAddBefore.push_back( ctorDecl );
		declsToAddBefore.push_back( copyCtorDecl );
		declsToAddBefore.push_back( dtorDecl );
		declsToAddBefore.push_back( assignDecl ); // assignment should come last since it uses copy constructor in return
	}

	void AutogenTupleRoutines::previsit( FunctionDecl *functionDecl ) {
		visit_children = false;
		maybeAccept( functionDecl->type, *visitor );
		functionNesting += 1;
		maybeAccept( functionDecl->statements, *visitor );
		functionNesting -= 1;
	}

	void AutogenTupleRoutines::previsit( CompoundStmt * ) {
		GuardScope( seenTuples );
	}
} // SymTab

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
