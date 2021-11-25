//
// Cforall Version 1.0.0 Copyright (C) 2016 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Keywords.cc --
//
// Author           : Thierry Delisle
// Created On       : Mon Mar 13 12:41:22 2017
// Last Modified By :
// Last Modified On :
// Update Count     : 10
//

#include "Concurrency/Keywords.h"

#include <cassert>                        // for assert
#include <string>                         // for string, operator==

#include <iostream>

#include "Common/Examine.h"               // for isMainFor
#include "Common/PassVisitor.h"           // for PassVisitor
#include "Common/SemanticError.h"         // for SemanticError
#include "Common/utility.h"               // for deleteAll, map_range
#include "CodeGen/OperatorTable.h"        // for isConstructor
#include "ControlStruct/LabelGenerator.h" // for LebelGenerator
#include "InitTweak/InitTweak.h"          // for getPointerBase
#include "SynTree/LinkageSpec.h"          // for Cforall
#include "SynTree/Constant.h"             // for Constant
#include "SynTree/Declaration.h"          // for StructDecl, FunctionDecl, ObjectDecl
#include "SynTree/Expression.h"           // for VariableExpr, ConstantExpr, Untype...
#include "SynTree/Initializer.h"          // for SingleInit, ListInit, Initializer ...
#include "SynTree/Label.h"                // for Label
#include "SynTree/Statement.h"            // for CompoundStmt, DeclStmt, ExprStmt
#include "SynTree/Type.h"                 // for StructInstType, Type, PointerType
#include "SynTree/Visitor.h"              // for Visitor, acceptAll
#include "Virtual/Tables.h"

class Attribute;

namespace Concurrency {
	inline static std::string getTypeIdName( std::string const & exception_name ) {
		return exception_name.empty() ? std::string() : Virtual::typeIdType( exception_name );
	}
	inline static std::string getVTableName( std::string const & exception_name ) {
		return exception_name.empty() ? std::string() : Virtual::vtableTypeName( exception_name );
	}

	// Only detects threads constructed with the keyword thread.
	inline static bool isThread( DeclarationWithType * decl ) {
		Type * baseType = decl->get_type()->stripDeclarator();
		StructInstType * instType = dynamic_cast<StructInstType *>( baseType );
		if ( nullptr == instType ) { return false; }
		return instType->baseStruct->is_thread();
	}

	//=============================================================================================
	// Pass declarations
	//=============================================================================================

	//-----------------------------------------------------------------------------
	//Handles sue type declarations :
	// sue MyType {                             struct MyType {
	// 	int data;                                  int data;
	// 	a_struct_t more_data;                      a_struct_t more_data;
	//                                =>             NewField_t newField;
	// };                                        };
	//                                           static inline NewField_t * getter_name( MyType * this ) { return &this->newField; }
	//
	class ConcurrentSueKeyword : public WithDeclsToAdd {
	  public:

		ConcurrentSueKeyword( std::string&& type_name, std::string&& field_name,
			std::string&& getter_name, std::string&& context_error, std::string&& exception_name,
			bool needs_main, AggregateDecl::Aggregate cast_target ) :
		  type_name( type_name ), field_name( field_name ), getter_name( getter_name ),
		  context_error( context_error ), exception_name( exception_name ),
		  typeid_name( getTypeIdName( exception_name ) ),
		  vtable_name( getVTableName( exception_name ) ),
		  needs_main( needs_main ), cast_target( cast_target ) {}

		virtual ~ConcurrentSueKeyword() {}

		Declaration * postmutate( StructDecl * decl );
		DeclarationWithType * postmutate( FunctionDecl * decl );

		void handle( StructDecl * );
		void addTypeId( StructDecl * );
		void addVtableForward( StructDecl * );
		FunctionDecl * forwardDeclare( StructDecl * );
		ObjectDecl * addField( StructDecl * );
		void addRoutines( ObjectDecl *, FunctionDecl * );
		void addLockUnlockRoutines( StructDecl * );

		virtual bool is_target( StructDecl * decl ) = 0;

		Expression * postmutate( KeywordCastExpr * cast );

	  private:
		const std::string type_name;
		const std::string field_name;
		const std::string getter_name;
		const std::string context_error;
		const std::string exception_name;
		const std::string typeid_name;
		const std::string vtable_name;
		bool needs_main;
		AggregateDecl::Aggregate cast_target;

		StructDecl   * type_decl = nullptr;
		FunctionDecl * dtor_decl = nullptr;
		StructDecl * except_decl = nullptr;
		StructDecl * typeid_decl = nullptr;
		StructDecl * vtable_decl = nullptr;
	};


	//-----------------------------------------------------------------------------
	//Handles thread type declarations :
	// thread Mythread {                         struct MyThread {
	// 	int data;                                  int data;
	// 	a_struct_t more_data;                      a_struct_t more_data;
	//                                =>             thread$ __thrd_d;
	// };                                        };
	//                                           static inline thread$ * get_thread( MyThread * this ) { return &this->__thrd_d; }
	//
	class ThreadKeyword final : public ConcurrentSueKeyword {
	  public:

	  	ThreadKeyword() : ConcurrentSueKeyword(
			"thread$",
			"__thrd",
			"get_thread",
			"thread keyword requires threads to be in scope, add #include <thread.hfa>\n",
			"ThreadCancelled",
			true,
			AggregateDecl::Thread
		)
		{}

		virtual ~ThreadKeyword() {}

		virtual bool is_target( StructDecl * decl ) override final { return decl->is_thread(); }

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< ThreadKeyword > impl;
			mutateAll( translationUnit, impl );
		}
	};

	//-----------------------------------------------------------------------------
	//Handles coroutine type declarations :
	// coroutine MyCoroutine {                   struct MyCoroutine {
	// 	int data;                                  int data;
	// 	a_struct_t more_data;                      a_struct_t more_data;
	//                                =>             coroutine$ __cor_d;
	// };                                        };
	//                                           static inline coroutine$ * get_coroutine( MyCoroutine * this ) { return &this->__cor_d; }
	//
	class CoroutineKeyword final : public ConcurrentSueKeyword {
	  public:

	  	CoroutineKeyword() : ConcurrentSueKeyword(
			"coroutine$",
			"__cor",
			"get_coroutine",
			"coroutine keyword requires coroutines to be in scope, add #include <coroutine.hfa>\n",
			"CoroutineCancelled",
			true,
			AggregateDecl::Coroutine
		)
		{}

		virtual ~CoroutineKeyword() {}

		virtual bool is_target( StructDecl * decl ) override final { return decl->is_coroutine(); }

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< CoroutineKeyword > impl;
			mutateAll( translationUnit, impl );
		}
	};



	//-----------------------------------------------------------------------------
	//Handles monitor type declarations :
	// monitor MyMonitor {                       struct MyMonitor {
	// 	int data;                                  int data;
	// 	a_struct_t more_data;                      a_struct_t more_data;
	//                                =>             monitor$ __mon_d;
	// };                                        };
	//                                           static inline monitor$ * get_coroutine( MyMonitor * this ) { return &this->__cor_d; }
	//
	class MonitorKeyword final : public ConcurrentSueKeyword {
	  public:

	  	MonitorKeyword() : ConcurrentSueKeyword(
			"monitor$",
			"__mon",
			"get_monitor",
			"monitor keyword requires monitors to be in scope, add #include <monitor.hfa>\n",
			"",
			false,
			AggregateDecl::Monitor
		)
		{}

		virtual ~MonitorKeyword() {}

		virtual bool is_target( StructDecl * decl ) override final { return decl->is_monitor(); }

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< MonitorKeyword > impl;
			mutateAll( translationUnit, impl );
		}
	};

	//-----------------------------------------------------------------------------
	//Handles generator type declarations :
	// generator MyGenerator {                   struct MyGenerator {
	// 	int data;                                  int data;
	// 	a_struct_t more_data;                      a_struct_t more_data;
	//                                =>             int __gen_next;
	// };                                        };
	//
	class GeneratorKeyword final : public ConcurrentSueKeyword {
	  public:

	  	GeneratorKeyword() : ConcurrentSueKeyword(
			"generator$",
			"__generator_state",
			"get_generator",
			"Unable to find builtin type generator$\n",
			"",
			true,
			AggregateDecl::Generator
		)
		{}

		virtual ~GeneratorKeyword() {}

		virtual bool is_target( StructDecl * decl ) override final { return decl->is_generator(); }

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< GeneratorKeyword > impl;
			mutateAll( translationUnit, impl );
		}
	};


	//-----------------------------------------------------------------------------
	class SuspendKeyword final : public WithStmtsToAdd, public WithGuards {
	public:
		SuspendKeyword() = default;
		virtual ~SuspendKeyword() = default;

		void  premutate( FunctionDecl * );
		DeclarationWithType * postmutate( FunctionDecl * );

		Statement * postmutate( SuspendStmt * );

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< SuspendKeyword > impl;
			mutateAll( translationUnit, impl );
		}

	private:
		bool is_real_suspend( FunctionDecl * );

		Statement * make_generator_suspend( SuspendStmt * );
		Statement * make_coroutine_suspend( SuspendStmt * );

		struct LabelPair {
			Label obj;
			int   idx;
		};

		LabelPair make_label() {
			labels.push_back( gen.newLabel("generator") );
			return { labels.back(), int(labels.size()) };
		}

		DeclarationWithType * in_generator = nullptr;
		FunctionDecl * decl_suspend = nullptr;
		std::vector<Label> labels;
		ControlStruct::LabelGenerator & gen = *ControlStruct::LabelGenerator::getGenerator();
	};

	//-----------------------------------------------------------------------------
	//Handles mutex routines definitions :
	// void foo( A * mutex a, B * mutex b,  int i ) {                  void foo( A * a, B * b,  int i ) {
	// 	                                                                 monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
	// 	                                                                 monitor_guard_t __guard = { __monitors, 2 };
	//    /*Some code*/                                       =>           /*Some code*/
	// }                                                               }
	//
	class MutexKeyword final {
	  public:

		void postvisit( FunctionDecl * decl );
		void postvisit(   StructDecl * decl );
		Statement * postmutate( MutexStmt * stmt );

		std::list<DeclarationWithType*> findMutexArgs( FunctionDecl*, bool & first );
		void validate( DeclarationWithType * );
		void addDtorStatements( FunctionDecl* func, CompoundStmt *, const std::list<DeclarationWithType * > &);
		void addStatements( FunctionDecl* func, CompoundStmt *, const std::list<DeclarationWithType * > &);
		void addStatements( CompoundStmt * body, const std::list<Expression * > & args );
		void addThreadDtorStatements( FunctionDecl* func, CompoundStmt * body, const std::list<DeclarationWithType * > & args );

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< MutexKeyword > impl;
			acceptAll( translationUnit, impl );
			mutateAll( translationUnit, impl );
		}

	  private:
	  	StructDecl* monitor_decl = nullptr;
		StructDecl* guard_decl = nullptr;
		StructDecl* dtor_guard_decl = nullptr;
		StructDecl* thread_guard_decl = nullptr;
		StructDecl* lock_guard_decl = nullptr;

		static std::unique_ptr< Type > generic_func;
	};

	std::unique_ptr< Type > MutexKeyword::generic_func = std::unique_ptr< Type >(
		new FunctionType(
			noQualifiers,
			true
		)
	);

	//-----------------------------------------------------------------------------
	//Handles mutex routines definitions :
	// void foo( A * mutex a, B * mutex b,  int i ) {                  void foo( A * a, B * b,  int i ) {
	// 	                                                                 monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
	// 	                                                                 monitor_guard_t __guard = { __monitors, 2 };
	//    /*Some code*/                                       =>           /*Some code*/
	// }                                                               }
	//
	class ThreadStarter final {
	  public:

		void postvisit( FunctionDecl * decl );
		void previsit ( StructDecl   * decl );

		void addStartStatement( FunctionDecl * decl, DeclarationWithType * param );

		static void implement( std::list< Declaration * > & translationUnit ) {
			PassVisitor< ThreadStarter > impl;
			acceptAll( translationUnit, impl );
		}

	  private :
		bool thread_ctor_seen = false;
		StructDecl * thread_decl = nullptr;
	};

	//=============================================================================================
	// General entry routine
	//=============================================================================================
	void applyKeywords( std::list< Declaration * > & translationUnit ) {
		ThreadKeyword	::implement( translationUnit );
		CoroutineKeyword	::implement( translationUnit );
		MonitorKeyword	::implement( translationUnit );
		GeneratorKeyword  ::implement( translationUnit );
		SuspendKeyword    ::implement( translationUnit );
	}

	void implementMutexFuncs( std::list< Declaration * > & translationUnit ) {
		MutexKeyword	::implement( translationUnit );
	}

	void implementThreadStarter( std::list< Declaration * > & translationUnit ) {
		ThreadStarter	::implement( translationUnit );
	}

	//=============================================================================================
	// Generic keyword implementation
	//=============================================================================================
	void fixupGenerics(FunctionType * func, StructDecl * decl) {
		cloneAll(decl->parameters, func->forall);
		for ( TypeDecl * td : func->forall ) {
			strict_dynamic_cast<StructInstType*>(
				func->parameters.front()->get_type()->stripReferences()
			)->parameters.push_back(
				new TypeExpr( new TypeInstType( noQualifiers, td->name, td ) )
			);
		}
	}

	Declaration * ConcurrentSueKeyword::postmutate(StructDecl * decl) {
		if( decl->name == type_name && decl->body ) {
			assert( !type_decl );
			type_decl = decl;
		}
		else if ( is_target(decl) ) {
			handle( decl );
		}
		else if ( !except_decl && exception_name == decl->name && decl->body ) {
			except_decl = decl;
		}
		else if ( !typeid_decl && typeid_name == decl->name && decl->body ) {
			typeid_decl = decl;
		}
		else if ( !vtable_decl && vtable_name == decl->name && decl->body ) {
			vtable_decl = decl;
		}
		// Might be able to get ride of is target.
		assert( is_target(decl) == (cast_target == decl->kind) );
		return decl;
	}

	DeclarationWithType * ConcurrentSueKeyword::postmutate( FunctionDecl * decl ) {
		if ( type_decl && isDestructorFor( decl, type_decl ) )
			dtor_decl = decl;
		else if ( vtable_name.empty() || !decl->has_body() )
			;
		else if ( auto param = isMainFor( decl, cast_target ) ) {
			// This should never trigger.
			assert( vtable_decl );
			// Should be safe because of isMainFor.
			StructInstType * struct_type = static_cast<StructInstType *>(
				static_cast<ReferenceType *>( param->get_type() )->base );
			assert( struct_type );

			std::list< Expression * > poly_args = { new TypeExpr( struct_type->clone() ) };
			ObjectDecl * vtable_object = Virtual::makeVtableInstance(
				"_default_vtable_object_declaration",
				vtable_decl->makeInst( poly_args ), struct_type, nullptr );
			declsToAddAfter.push_back( vtable_object );
			declsToAddAfter.push_back(
				new ObjectDecl(
					Virtual::concurrentDefaultVTableName(),
					noStorageClasses,
					LinkageSpec::Cforall,
					/* bitfieldWidth */ nullptr,
					new ReferenceType( Type::Const, vtable_object->type->clone() ),
					new SingleInit( new VariableExpr( vtable_object ) )
				)
			);
			declsToAddAfter.push_back( Virtual::makeGetExceptionFunction(
				vtable_object, except_decl->makeInst( std::move( poly_args ) )
			) );
		}

		return decl;
	}

	Expression * ConcurrentSueKeyword::postmutate( KeywordCastExpr * cast ) {
		if ( cast_target == cast->target ) {
			// convert (thread &)t to (thread$ &)*get_thread(t), etc.
			if( !type_decl ) SemanticError( cast, context_error );
			if( !dtor_decl ) SemanticError( cast, context_error );
			assert( cast->result == nullptr );
			cast->set_result( new ReferenceType( noQualifiers, new StructInstType( noQualifiers, type_decl ) ) );
			cast->concrete_target.field  = field_name;
			cast->concrete_target.getter = getter_name;
		}
		return cast;
	}

	void ConcurrentSueKeyword::handle( StructDecl * decl ) {
		if( ! decl->body ) return;

		if( !type_decl ) SemanticError( decl, context_error );
		if( !dtor_decl ) SemanticError( decl, context_error );

		if ( !exception_name.empty() ) {
			if( !typeid_decl ) SemanticError( decl, context_error );
			if( !vtable_decl ) SemanticError( decl, context_error );

			addTypeId( decl );
			addVtableForward( decl );
		}
		FunctionDecl * func = forwardDeclare( decl );
		ObjectDecl * field = addField( decl );

		// add get_.* routine
		addRoutines( field, func );
		// add lock/unlock routines to monitors for use by mutex stmt
		addLockUnlockRoutines( decl );
	}

	void ConcurrentSueKeyword::addTypeId( StructDecl * decl ) {
		assert( typeid_decl );
		StructInstType typeid_type( Type::Const, typeid_decl );
		typeid_type.parameters.push_back( new TypeExpr(
			new StructInstType( noQualifiers, decl )
			) );
		declsToAddBefore.push_back( Virtual::makeTypeIdInstance( &typeid_type ) );
	}

	void ConcurrentSueKeyword::addVtableForward( StructDecl * decl ) {
		assert( vtable_decl );
		std::list< Expression * > poly_args = {
			new TypeExpr( new StructInstType( noQualifiers, decl ) ),
		};
		declsToAddBefore.push_back( Virtual::makeGetExceptionForward(
			vtable_decl->makeInst( poly_args ),
			except_decl->makeInst( poly_args )
		) );
		ObjectDecl * vtable_object = Virtual::makeVtableForward(
			"_default_vtable_object_declaration",
			vtable_decl->makeInst( move( poly_args ) ) );
		declsToAddBefore.push_back( vtable_object );
		declsToAddBefore.push_back(
			new ObjectDecl(
				Virtual::concurrentDefaultVTableName(),
				Type::StorageClasses( Type::Extern ),
				LinkageSpec::Cforall,
				/* bitfieldWidth */ nullptr,
				new ReferenceType( Type::Const, vtable_object->type->clone() ),
				/* init */ nullptr
			)
		);
	}

	FunctionDecl * ConcurrentSueKeyword::forwardDeclare( StructDecl * decl ) {

		StructDecl * forward = decl->clone();
		forward->set_body( false );
		deleteAll( forward->get_members() );
		forward->get_members().clear();

		FunctionType * get_type = new FunctionType( noQualifiers, false );
		ObjectDecl * this_decl = new ObjectDecl(
			"this",
			noStorageClasses,
			LinkageSpec::Cforall,
			nullptr,
			new ReferenceType(
				noQualifiers,
				new StructInstType(
					noQualifiers,
					decl
				)
			),
			nullptr
		);

		get_type->get_parameters().push_back( this_decl->clone() );
		get_type->get_returnVals().push_back(
			new ObjectDecl(
				"ret",
				noStorageClasses,
				LinkageSpec::Cforall,
				nullptr,
				new PointerType(
					noQualifiers,
					new StructInstType(
						noQualifiers,
						type_decl
					)
				),
				nullptr
			)
		);
		fixupGenerics(get_type, decl);

		FunctionDecl * get_decl = new FunctionDecl(
			getter_name,
			Type::Static,
			LinkageSpec::Cforall,
			get_type,
			nullptr,
			{ new Attribute("const") },
			Type::Inline
		);

		FunctionDecl * main_decl = nullptr;

		if( needs_main ) {
			FunctionType * main_type = new FunctionType( noQualifiers, false );

			main_type->get_parameters().push_back( this_decl->clone() );

			main_decl = new FunctionDecl(
				"main",
				noStorageClasses,
				LinkageSpec::Cforall,
				main_type,
				nullptr
			);
			fixupGenerics(main_type, decl);
		}

		delete this_decl;

		declsToAddBefore.push_back( forward );
		if( needs_main ) declsToAddBefore.push_back( main_decl );
		declsToAddBefore.push_back( get_decl );

		return get_decl;
	}

	ObjectDecl * ConcurrentSueKeyword::addField( StructDecl * decl ) {
		ObjectDecl * field = new ObjectDecl(
			field_name,
			noStorageClasses,
			LinkageSpec::Cforall,
			nullptr,
			new StructInstType(
				noQualifiers,
				type_decl
			),
			nullptr
		);

		decl->get_members().push_back( field );

		return field;
	}

	// This function adds the get_.* routine body for coroutines, monitors etc
	// 		after their corresponding struct has been made
	void ConcurrentSueKeyword::addRoutines( ObjectDecl * field, FunctionDecl * func ) {
		CompoundStmt * statement = new CompoundStmt();
		statement->push_back(
			new ReturnStmt(
				new AddressExpr(
					new MemberExpr(
						field,
						new CastExpr(
							new VariableExpr( func->get_functionType()->get_parameters().front() ),
							func->get_functionType()->get_parameters().front()->get_type()->stripReferences()->clone(),
							false
						)
					)
				)
			)
		);

		FunctionDecl * get_decl = func->clone();

		get_decl->set_statements( statement );

		declsToAddAfter.push_back( get_decl );
	}

	// Generates lock/unlock routines for monitors to be used by mutex stmts
	void ConcurrentSueKeyword::addLockUnlockRoutines( StructDecl * decl ) {
		// this routine will be called for all ConcurrentSueKeyword children so only continue if we are a monitor
		if ( !decl->is_monitor() ) return;

		FunctionType * lock_fn_type = new FunctionType( noQualifiers, false );
		FunctionType * unlock_fn_type = new FunctionType( noQualifiers, false );

		// create this ptr parameter for both routines
		ObjectDecl * this_decl = new ObjectDecl(
			"this",
			noStorageClasses,
			LinkageSpec::Cforall,
			nullptr,
			new ReferenceType(
				noQualifiers,
				new StructInstType(
					noQualifiers,
					decl
				)
			),
			nullptr
		);

		lock_fn_type->get_parameters().push_back( this_decl->clone() );
		unlock_fn_type->get_parameters().push_back( this_decl->clone() );
		fixupGenerics(lock_fn_type, decl);
		fixupGenerics(unlock_fn_type, decl);

		delete this_decl;


		//////////////////////////////////////////////////////////////////////
		// The following generates this lock routine for all monitors
		/*
			void lock (monitor_t & this) {
				lock(get_monitor(this));
			}	
		*/
		FunctionDecl * lock_decl = new FunctionDecl(
			"lock",
			Type::Static,
			LinkageSpec::Cforall,
			lock_fn_type,
			nullptr,
			{ },
			Type::Inline
		);

		UntypedExpr * get_monitor_lock =  new UntypedExpr (
			new NameExpr( "get_monitor" ),
			{ new VariableExpr( lock_fn_type->get_parameters().front() ) }
		);

		CompoundStmt * lock_statement = new CompoundStmt();
		lock_statement->push_back(
			new ExprStmt( 
				new UntypedExpr (
					new NameExpr( "lock" ),
					{
						get_monitor_lock
					}
				)
			)
		);
		lock_decl->set_statements( lock_statement );

		//////////////////////////////////////////////////////////////////
		// The following generates this routine for all monitors
		/*
			void unlock (monitor_t & this) {
				unlock(get_monitor(this));
			}	
		*/
		FunctionDecl * unlock_decl = new FunctionDecl(
			"unlock",
			Type::Static,
			LinkageSpec::Cforall,
			unlock_fn_type,
			nullptr,
			{ },
			Type::Inline
		);

		CompoundStmt * unlock_statement = new CompoundStmt();

		UntypedExpr * get_monitor_unlock =  new UntypedExpr (
			new NameExpr( "get_monitor" ),
			{ new VariableExpr( unlock_fn_type->get_parameters().front() ) }
		);

		unlock_statement->push_back(
			new ExprStmt( 
				new UntypedExpr(
					new NameExpr( "unlock" ),
					{
						get_monitor_unlock
					}
				)
			)
		);
		unlock_decl->set_statements( unlock_statement );
		
		// pushes routines to declsToAddAfter to add at a later time
		declsToAddAfter.push_back( lock_decl );
		declsToAddAfter.push_back( unlock_decl );
	}

	//=============================================================================================
	// Suspend keyword implementation
	//=============================================================================================
	bool SuspendKeyword::is_real_suspend( FunctionDecl * func ) {
		if(isMangled(func->linkage)) return false; // the real suspend isn't mangled
		if(func->name != "__cfactx_suspend") return false; // the real suspend has a specific name
		if(func->type->parameters.size() != 0) return false; // Too many parameters
		if(func->type->returnVals.size() != 0) return false; // Too many return values

		return true;
	}

	void SuspendKeyword::premutate( FunctionDecl * func ) {
		GuardValue(in_generator);
		in_generator = nullptr;

		// Is this the real suspend?
		if(is_real_suspend(func)) {
			decl_suspend = decl_suspend ? decl_suspend : func;
			return;
		}

		// Is this the main of a generator?
		auto param = isMainFor( func, AggregateDecl::Aggregate::Generator );
		if(!param) return;

		if(func->type->returnVals.size() != 0) SemanticError(func->location, "Generator main must return void");

		in_generator = param;
		GuardValue(labels);
		labels.clear();
	}

	DeclarationWithType * SuspendKeyword::postmutate( FunctionDecl * func ) {
		if( !func->statements ) return func; // Not the actual definition, don't do anything
		if( !in_generator     ) return func; // Not in a generator, don't do anything
		if( labels.empty()    ) return func; // Generator has no states, nothing to do, could throw a warning

		// This is a generator main, we need to add the following code to the top
		// static void * __generator_labels[] = {&&s0, &&s1, ...};
		// goto * __generator_labels[gen.__generator_state];
		const auto & loc = func->location;

		const auto first_label = gen.newLabel("generator");

		// for each label add to declaration
		std::list<Initializer*> inits = { new SingleInit( new LabelAddressExpr( first_label ) ) };
		for(const auto & label : labels) {
			inits.push_back(
				new SingleInit(
					new LabelAddressExpr( label )
				)
			);
		}
		auto init = new ListInit(std::move(inits), noDesignators, true);
		labels.clear();

		// create decl
		auto decl = new ObjectDecl(
			"__generator_labels",
			Type::StorageClasses( Type::Static ),
			LinkageSpec::AutoGen,
			nullptr,
			new ArrayType(
				Type::Qualifiers(),
				new PointerType(
					Type::Qualifiers(),
					new VoidType( Type::Qualifiers() )
				),
				nullptr,
				false, false
			),
			init
		);

		// create the goto
		assert(in_generator);

		auto go_decl = new ObjectDecl(
			"__generator_label",
			noStorageClasses,
			LinkageSpec::AutoGen,
			nullptr,
			new PointerType(
				Type::Qualifiers(),
				new VoidType( Type::Qualifiers() )
			),
			new SingleInit(
				new UntypedExpr(
					new NameExpr("?[?]"),
					{
						new NameExpr("__generator_labels"),
						new UntypedMemberExpr(
							new NameExpr("__generator_state"),
							new VariableExpr( in_generator )
						)
					}
				)
			)
		);
		go_decl->location = loc;

		auto go = new BranchStmt(
			new VariableExpr( go_decl ),
			BranchStmt::Goto
		);
		go->location = loc;
		go->computedTarget->location = loc;

		auto noop = new NullStmt({ first_label });
		noop->location = loc;

		// wrap everything in a nice compound
		auto body = new CompoundStmt({
			new DeclStmt( decl ),
			new DeclStmt( go_decl ),
			go,
			noop,
			func->statements
		});
		body->location   = loc;
		func->statements = body;

		return func;
	}

	Statement * SuspendKeyword::postmutate( SuspendStmt * stmt ) {
		SuspendStmt::Type type = stmt->type;
		if(type == SuspendStmt::None) {
			// This suspend has a implicit target, find it
			type = in_generator ? SuspendStmt::Generator : SuspendStmt::Coroutine;
		}

		// Check that the target makes sense
		if(!in_generator && type == SuspendStmt::Generator) SemanticError( stmt->location, "'suspend generator' must be used inside main of generator type.");

		// Act appropriately
		switch(type) {
			case SuspendStmt::Generator: return make_generator_suspend(stmt);
			case SuspendStmt::Coroutine: return make_coroutine_suspend(stmt);
			default: abort();
		}
	}

	Statement * SuspendKeyword::make_generator_suspend( SuspendStmt * stmt ) {
		assert(in_generator);
		// Target code is :
		//   gen.__generator_state = X;
		//   { THEN }
		//   return;
		//   __gen_X:;

		// Save the location and delete the old statement, we only need the location from this point on
		auto loc = stmt->location;

		// Build the label and get its index
		auto label = make_label();

		// Create the context saving statement
		auto save = new ExprStmt( new UntypedExpr(
			new NameExpr( "?=?" ),
			{
				new UntypedMemberExpr(
					new NameExpr("__generator_state"),
					new VariableExpr( in_generator )
				),
				new ConstantExpr(
					Constant::from_int( label.idx )
				)
			}
		));
		assert(save->expr);
		save->location = loc;
		stmtsToAddBefore.push_back( save );

		// if we have a then add it here
		auto then = stmt->then;
		stmt->then = nullptr;
		delete stmt;
		if(then) stmtsToAddBefore.push_back( then );

		// Create the return statement
		auto ret = new ReturnStmt( nullptr );
		ret->location = loc;
		stmtsToAddBefore.push_back( ret );

		// Create the null statement with the created label
		auto noop = new NullStmt({ label.obj });
		noop->location = loc;

		// Return the null statement to take the place of the previous statement
		return noop;
	}

	Statement * SuspendKeyword::make_coroutine_suspend( SuspendStmt * stmt ) {
		if(stmt->then) SemanticError( stmt->location, "Compound statement following coroutines is not implemented.");

		// Save the location and delete the old statement, we only need the location from this point on
		auto loc = stmt->location;
		delete stmt;

		// Create the call expression
		if(!decl_suspend) SemanticError( loc, "suspend keyword applied to coroutines requires coroutines to be in scope, add #include <coroutine.hfa>\n");
		auto expr = new UntypedExpr( VariableExpr::functionPointer( decl_suspend ) );
		expr->location = loc;

		// Change this statement into a regular expr
		assert(expr);
		auto nstmt = new ExprStmt( expr );
		nstmt->location = loc;
		return nstmt;
	}


	//=============================================================================================
	// Mutex keyword implementation
	//=============================================================================================

	void MutexKeyword::postvisit(FunctionDecl* decl) {

		bool first = false;
		std::list<DeclarationWithType*> mutexArgs = findMutexArgs( decl, first );
		bool const isDtor = CodeGen::isDestructor( decl->name );

		// Is this function relevant to monitors
		if( mutexArgs.empty() ) {
			// If this is the destructor for a monitor it must be mutex
			if(isDtor) {
				// This reflects MutexKeyword::validate, except does not produce an error.
				Type* ty = decl->get_functionType()->get_parameters().front()->get_type();

				// If it's a copy, it's not a mutex
				ReferenceType* rty = dynamic_cast< ReferenceType * >( ty );
				if( ! rty ) return;

				// If we are not pointing directly to a type, it's not a mutex
				Type* base = rty->get_base();
				if( dynamic_cast< ReferenceType * >( base ) ) return;
				if( dynamic_cast< PointerType * >( base ) ) return;

				// Check if its a struct
				StructInstType * baseStruct = dynamic_cast< StructInstType * >( base );
				if( !baseStruct ) return;

				// Check if its a monitor
				if(baseStruct->baseStruct->is_monitor() || baseStruct->baseStruct->is_thread())
					SemanticError( decl, "destructors for structures declared as \"monitor\" must use mutex parameters\n" );
			}
			return;
		}

		// Monitors can't be constructed with mutual exclusion
		if( CodeGen::isConstructor(decl->name) && !first ) SemanticError( decl, "constructors cannot have mutex parameters" );

		// It makes no sense to have multiple mutex parameters for the destructor
		if( isDtor && mutexArgs.size() != 1 ) SemanticError( decl, "destructors can only have 1 mutex argument" );

		// Make sure all the mutex arguments are monitors
		for(auto arg : mutexArgs) {
			validate( arg );
		}

		// Check if we need to instrument the body
		CompoundStmt* body = decl->get_statements();
		if( ! body ) return;

		// Do we have the required headers
		if( !monitor_decl || !guard_decl || !dtor_guard_decl )
			SemanticError( decl, "mutex keyword requires monitors to be in scope, add #include <monitor.hfa>\n" );

		// Instrument the body
		if ( isDtor && isThread( mutexArgs.front() ) ) {
			if( !thread_guard_decl ) {
				SemanticError( decl, "thread destructor requires threads to be in scope, add #include <thread.hfa>\n" );
			}
			addThreadDtorStatements( decl, body, mutexArgs );
		}
		else if ( isDtor ) {
			addDtorStatements( decl, body, mutexArgs );
		}
		else {
			addStatements( decl, body, mutexArgs );
		}
	}

	void MutexKeyword::postvisit(StructDecl* decl) {

		if( decl->name == "monitor$" && decl->body ) {
			assert( !monitor_decl );
			monitor_decl = decl;
		}
		else if( decl->name == "monitor_guard_t" && decl->body ) {
			assert( !guard_decl );
			guard_decl = decl;
		}
		else if( decl->name == "monitor_dtor_guard_t" && decl->body ) {
			assert( !dtor_guard_decl );
			dtor_guard_decl = decl;
		}
		else if( decl->name == "thread_dtor_guard_t" && decl->body ) {
			assert( !thread_guard_decl );
			thread_guard_decl = decl;
		} 
		else if ( decl->name == "__mutex_stmt_lock_guard" && decl->body ) {
			assert( !lock_guard_decl );
			lock_guard_decl = decl;
		}
	}

	Statement * MutexKeyword::postmutate( MutexStmt * stmt ) {
		std::list<Statement *> stmtsForCtor;
		stmtsForCtor.push_back(stmt->stmt);
		CompoundStmt * body = new CompoundStmt( stmtsForCtor );
		addStatements( body, stmt->mutexObjs);
		return body;
	}

	std::list<DeclarationWithType*> MutexKeyword::findMutexArgs( FunctionDecl* decl, bool & first ) {
		std::list<DeclarationWithType*> mutexArgs;

		bool once = true;
		for( auto arg : decl->get_functionType()->get_parameters()) {
			//Find mutex arguments
			Type* ty = arg->get_type();
			if( ! ty->get_mutex() ) continue;

			if(once) {first = true;}
			once = false;

			//Append it to the list
			mutexArgs.push_back( arg );
		}

		return mutexArgs;
	}

	void MutexKeyword::validate( DeclarationWithType * arg ) {
		Type* ty = arg->get_type();

		//Makes sure it's not a copy
		ReferenceType* rty = dynamic_cast< ReferenceType * >( ty );
		if( ! rty ) SemanticError( arg, "Mutex argument must be of reference type " );

		//Make sure the we are pointing directly to a type
		Type* base = rty->get_base();
		if( dynamic_cast< ReferenceType * >( base ) ) SemanticError( arg, "Mutex argument have exactly one level of indirection " );
		if( dynamic_cast< PointerType * >( base ) ) SemanticError( arg, "Mutex argument have exactly one level of indirection " );

		//Make sure that typed isn't mutex
		if( base->get_mutex() ) SemanticError( arg, "mutex keyword may only appear once per argument " );
	}

	void MutexKeyword::addDtorStatements( FunctionDecl* func, CompoundStmt * body, const std::list<DeclarationWithType * > & args ) {
		Type * arg_type = args.front()->get_type()->clone();
		arg_type->set_mutex( false );

		ObjectDecl * monitors = new ObjectDecl(
			"__monitor",
			noStorageClasses,
			LinkageSpec::Cforall,
			nullptr,
			new PointerType(
				noQualifiers,
				new StructInstType(
					noQualifiers,
					monitor_decl
				)
			),
			new SingleInit( new UntypedExpr(
				new NameExpr( "get_monitor" ),
				{  new CastExpr( new VariableExpr( args.front() ), arg_type, false ) }
			))
		);

		assert(generic_func);

		//in reverse order :
		// monitor_dtor_guard_t __guard = { __monitors, func };
		body->push_front(
			new DeclStmt( new ObjectDecl(
				"__guard",
				noStorageClasses,
				LinkageSpec::Cforall,
				nullptr,
				new StructInstType(
					noQualifiers,
					dtor_guard_decl
				),
				new ListInit(
					{
						new SingleInit( new AddressExpr( new VariableExpr( monitors ) ) ),
						new SingleInit( new CastExpr( new VariableExpr( func ), generic_func->clone(), false ) ),
						new SingleInit( new ConstantExpr( Constant::from_bool( false ) ) )
					},
					noDesignators,
					true
				)
			))
		);

		//monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
		body->push_front( new DeclStmt( monitors ) );
	}

	void MutexKeyword::addThreadDtorStatements(
			FunctionDecl*, CompoundStmt * body,
			const std::list<DeclarationWithType * > & args ) {
		assert( args.size() == 1 );
		DeclarationWithType * arg = args.front();
		Type * arg_type = arg->get_type()->clone();
		assert( arg_type->get_mutex() );
		arg_type->set_mutex( false );

		// thread_dtor_guard_t __guard = { this, intptr( 0 ) };
		body->push_front(
			new DeclStmt( new ObjectDecl(
				"__guard",
				noStorageClasses,
				LinkageSpec::Cforall,
				nullptr,
				new StructInstType(
					noQualifiers,
					thread_guard_decl
				),
				new ListInit(
					{
						new SingleInit( new CastExpr( new VariableExpr( arg ), arg_type ) ),
						new SingleInit( new UntypedExpr(
							new NameExpr( "intptr" ), {
								new ConstantExpr( Constant::from_int( 0 ) ),
							}
						) ),
					},
					noDesignators,
					true
				)
			))
		);
	}

	void MutexKeyword::addStatements( CompoundStmt * body, const std::list<Expression * > & args ) {
		ObjectDecl * monitors = new ObjectDecl(
			"__monitors",
			noStorageClasses,
			LinkageSpec::Cforall,
			nullptr,
			new ArrayType(
				noQualifiers,
				new PointerType(
					noQualifiers,
					//new TypeofType( noQualifiers, args.front()->clone() )
					new TypeofType( noQualifiers, new UntypedExpr(
							new NameExpr( "__get_type" ),
							{ args.front()->clone() }
						) 
					)
				),
				new ConstantExpr( Constant::from_ulong( args.size() ) ),
				false,
				false
			),
			new ListInit(
				map_range < std::list<Initializer*> > ( args, [](Expression * var ){
					return new SingleInit( new UntypedExpr(
							new NameExpr( "__get_ptr" ),
							{ var }
					) );
					//return new SingleInit( new AddressExpr( var ) );
				})
			)
		);

		StructInstType * lock_guard_struct = new StructInstType( noQualifiers, lock_guard_decl );
		TypeExpr * lock_type_expr = new TypeExpr( 
			new TypeofType( noQualifiers, new UntypedExpr(
				new NameExpr( "__get_type" ),
				{ args.front()->clone() }
				) 
			) 
		);

		lock_guard_struct->parameters.push_back( lock_type_expr ) ;

		// in reverse order :
		// monitor_guard_t __guard = { __monitors, # };
		body->push_front(
			new DeclStmt( new ObjectDecl(
				"__guard",
				noStorageClasses,
				LinkageSpec::Cforall,
				nullptr,
				lock_guard_struct,
				new ListInit(
					{
						new SingleInit( new VariableExpr( monitors ) ),
						new SingleInit( new ConstantExpr( Constant::from_ulong( args.size() ) ) )
					},
					noDesignators,
					true
				)
			))
		);

		//monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
		body->push_front( new DeclStmt( monitors) );
	}

	void MutexKeyword::addStatements( FunctionDecl* func, CompoundStmt * body, const std::list<DeclarationWithType * > & args ) {
		ObjectDecl * monitors = new ObjectDecl(
			"__monitors",
			noStorageClasses,
			LinkageSpec::Cforall,
			nullptr,
			new ArrayType(
				noQualifiers,
				new PointerType(
					noQualifiers,
					new StructInstType(
						noQualifiers,
						monitor_decl
					)
				),
				new ConstantExpr( Constant::from_ulong( args.size() ) ),
				false,
				false
			),
			new ListInit(
				map_range < std::list<Initializer*> > ( args, [](DeclarationWithType * var ){
					Type * type = var->get_type()->clone();
					type->set_mutex( false );
					return new SingleInit( new UntypedExpr(
						new NameExpr( "get_monitor" ),
						{  new CastExpr( new VariableExpr( var ), type, false ) }
					) );
				})
			)
		);

		assert(generic_func);

		// in reverse order :
		// monitor_guard_t __guard = { __monitors, #, func };
		body->push_front(
			new DeclStmt( new ObjectDecl(
				"__guard",
				noStorageClasses,
				LinkageSpec::Cforall,
				nullptr,
				new StructInstType(
					noQualifiers,
					guard_decl
				),
				new ListInit(
					{
						new SingleInit( new VariableExpr( monitors ) ),
						new SingleInit( new ConstantExpr( Constant::from_ulong( args.size() ) ) ),
						new SingleInit( new CastExpr( new VariableExpr( func ), generic_func->clone(), false ) )
					},
					noDesignators,
					true
				)
			))
		);

		//monitor$ * __monitors[] = { get_monitor(a), get_monitor(b) };
		body->push_front( new DeclStmt( monitors) );
	}

	//=============================================================================================
	// General entry routine
	//=============================================================================================
	void ThreadStarter::previsit( StructDecl * decl ) {
		if( decl->name == "thread$" && decl->body ) {
			assert( !thread_decl );
			thread_decl = decl;
		}
	}

	void ThreadStarter::postvisit(FunctionDecl * decl) {
		if( ! CodeGen::isConstructor(decl->name) ) return;

		Type * typeof_this = InitTweak::getTypeofThis(decl->type);
		StructInstType * ctored_type = dynamic_cast< StructInstType * >( typeof_this );
		if( ctored_type && ctored_type->baseStruct == thread_decl ) {
			thread_ctor_seen = true;
		}

		DeclarationWithType * param = decl->get_functionType()->get_parameters().front();
		auto type  = dynamic_cast< StructInstType * >( InitTweak::getPointerBase( param->get_type() ) );
		if( type && type->get_baseStruct()->is_thread() ) {
			if( !thread_decl || !thread_ctor_seen ) {
				SemanticError( type->get_baseStruct()->location, "thread keyword requires threads to be in scope, add #include <thread.hfa>");
			}

			addStartStatement( decl, param );
		}
	}

	void ThreadStarter::addStartStatement( FunctionDecl * decl, DeclarationWithType * param ) {
		CompoundStmt * stmt = decl->get_statements();

		if( ! stmt ) return;

		stmt->push_back(
			new ExprStmt(
				new UntypedExpr(
					new NameExpr( "__thrd_start" ),
					{ new VariableExpr( param ), new NameExpr("main") }
				)
			)
		);
	}
};

// Local Variables: //
// mode: c //
// tab-width: 4 //
// End: //

