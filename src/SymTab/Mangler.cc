//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Mangler.cc --
//
// Author           : Richard C. Bilson
// Created On       : Sun May 17 21:40:29 2015
// Last Modified By : Andrew Beach
// Last Modified On : Fri Oct 21 16:18:00 2022
// Update Count     : 75
//
#include "Mangler.h"

#include <algorithm>                     // for copy, transform
#include <cassert>                       // for assert, assertf
#include <functional>                    // for const_mem_fun_t, mem_fun
#include <iterator>                      // for ostream_iterator, back_insert_ite...
#include <list>                          // for _List_iterator, list, _List_const...
#include <string>                        // for string, char_traits, operator<<

#include "CodeGen/OperatorTable.h"       // for OperatorInfo, operatorLookup
#include "Common/PassVisitor.h"
#include "Common/SemanticError.h"        // for SemanticError
#include "Common/utility.h"              // for toString
#include "ResolvExpr/TypeEnvironment.h"  // for TypeEnvironment
#include "SynTree/LinkageSpec.h"         // for Spec, isOverridable, AutoGen, Int...
#include "SynTree/Declaration.h"         // for TypeDecl, DeclarationWithType
#include "SynTree/Expression.h"          // for TypeExpr, Expression, operator<<
#include "SynTree/Type.h"                // for Type, ReferenceToType, Type::Fora...

#include "AST/Pass.hpp"

namespace SymTab {
	namespace Mangler {
		namespace {
			/// Mangles names to a unique C identifier
			struct Mangler_old : public WithShortCircuiting, public WithVisitorRef<Mangler_old>, public WithGuards {
				Mangler_old( bool mangleOverridable, bool typeMode, bool mangleGenericParams );
				Mangler_old( const Mangler_old & ) = delete;

				void previsit( const BaseSyntaxNode * ) { visit_children = false; }

				void postvisit( const ObjectDecl * declaration );
				void postvisit( const FunctionDecl * declaration );
				void postvisit( const TypeDecl * declaration );

				void postvisit( const VoidType * voidType );
				void postvisit( const BasicType * basicType );
				void postvisit( const PointerType * pointerType );
				void postvisit( const ArrayType * arrayType );
				void postvisit( const ReferenceType * refType );
				void postvisit( const FunctionType * functionType );
				void postvisit( const StructInstType * aggregateUseType );
				void postvisit( const UnionInstType * aggregateUseType );
				void postvisit( const EnumInstType * aggregateUseType );
				void postvisit( const TypeInstType * aggregateUseType );
				void postvisit( const TraitInstType * inst );
				void postvisit( const TupleType * tupleType );
				void postvisit( const VarArgsType * varArgsType );
				void postvisit( const ZeroType * zeroType );
				void postvisit( const OneType * oneType );
				void postvisit( const QualifiedType * qualType );

				std::string get_mangleName() { return mangleName; }
			  private:
				std::string mangleName;         ///< Mangled name being constructed
				typedef std::map< std::string, std::pair< int, int > > VarMapType;
				VarMapType varNums;             ///< Map of type variables to indices
				int nextVarNum;                 ///< Next type variable index
				bool isTopLevel;                ///< Is the Mangler at the top level
				bool mangleOverridable;         ///< Specially mangle overridable built-in methods
				bool typeMode;                  ///< Produce a unique mangled name for a type
				bool mangleGenericParams;       ///< Include generic parameters in name mangling if true
				bool inFunctionType = false;    ///< Include type qualifiers if false.
				bool inQualifiedType = false;   ///< Add start/end delimiters around qualified type

			  public:
				Mangler_old( bool mangleOverridable, bool typeMode, bool mangleGenericParams,
					int nextVarNum, const VarMapType& varNums );

			  private:
				void mangleDecl( const DeclarationWithType * declaration );
				void mangleRef( const ReferenceToType * refType, std::string prefix );

				void printQualifiers( const Type *type );
			}; // Mangler_old
		} // namespace

		std::string mangle( const BaseSyntaxNode * decl, bool mangleOverridable, bool typeMode, bool mangleGenericParams ) {
			PassVisitor<Mangler_old> mangler( mangleOverridable, typeMode, mangleGenericParams );
			maybeAccept( decl, mangler );
			return mangler.pass.get_mangleName();
		}

		std::string mangleType( const Type * ty ) {
			PassVisitor<Mangler_old> mangler( false, true, true );
			maybeAccept( ty, mangler );
			return mangler.pass.get_mangleName();
		}

		std::string mangleConcrete( const Type * ty ) {
			PassVisitor<Mangler_old> mangler( false, false, false );
			maybeAccept( ty, mangler );
			return mangler.pass.get_mangleName();
		}

		namespace {
			Mangler_old::Mangler_old( bool mangleOverridable, bool typeMode, bool mangleGenericParams )
				: nextVarNum( 0 ), isTopLevel( true ),
				mangleOverridable( mangleOverridable ), typeMode( typeMode ),
				mangleGenericParams( mangleGenericParams ) {}

			Mangler_old::Mangler_old( bool mangleOverridable, bool typeMode, bool mangleGenericParams,
				int nextVarNum, const VarMapType& varNums )
				: varNums( varNums ), nextVarNum( nextVarNum ), isTopLevel( false ),
				mangleOverridable( mangleOverridable ), typeMode( typeMode ),
				mangleGenericParams( mangleGenericParams ) {}

			void Mangler_old::mangleDecl( const DeclarationWithType * declaration ) {
				bool wasTopLevel = isTopLevel;
				if ( isTopLevel ) {
					varNums.clear();
					nextVarNum = 0;
					isTopLevel = false;
				} // if
				mangleName += Encoding::manglePrefix;
				const CodeGen::OperatorInfo * opInfo = CodeGen::operatorLookup( declaration->get_name() );
				if ( opInfo ) {
					mangleName += std::to_string( opInfo->outputName.size() ) + opInfo->outputName;
				} else {
					mangleName += std::to_string( declaration->name.size() ) + declaration->name;
				} // if
				maybeAccept( declaration->get_type(), *visitor );
				if ( mangleOverridable && LinkageSpec::isOverridable( declaration->get_linkage() ) ) {
					// want to be able to override autogenerated and intrinsic routines,
					// so they need a different name mangling
					if ( declaration->get_linkage() == LinkageSpec::AutoGen ) {
						mangleName += Encoding::autogen;
					} else if ( declaration->get_linkage() == LinkageSpec::Intrinsic ) {
						mangleName += Encoding::intrinsic;
					} else {
						// if we add another kind of overridable function, this has to change
						assert( false && "unknown overrideable linkage" );
					} // if
				}
				isTopLevel = wasTopLevel;
			}

			void Mangler_old::postvisit( const ObjectDecl * declaration ) {
				mangleDecl( declaration );
			}

			void Mangler_old::postvisit( const FunctionDecl * declaration ) {
				mangleDecl( declaration );
			}

			void Mangler_old::postvisit( const VoidType * voidType ) {
				printQualifiers( voidType );
				mangleName += Encoding::void_t;
			}

			void Mangler_old::postvisit( const BasicType * basicType ) {
				printQualifiers( basicType );
				assertf( basicType->kind < BasicType::NUMBER_OF_BASIC_TYPES, "Unhandled basic type: %d", basicType->kind );
				mangleName += Encoding::basicTypes[ basicType->kind ];
			}

			void Mangler_old::postvisit( const PointerType * pointerType ) {
				printQualifiers( pointerType );
				// mangle void (*f)() and void f() to the same name to prevent overloading on functions and function pointers
				if ( ! dynamic_cast<FunctionType *>( pointerType->base ) ) mangleName += Encoding::pointer;
				maybeAccept( pointerType->base, *visitor );
			}

			void Mangler_old::postvisit( const ArrayType * arrayType ) {
				// TODO: encode dimension
				printQualifiers( arrayType );
				mangleName += Encoding::array + "0";
				maybeAccept( arrayType->base, *visitor );
			}

			void Mangler_old::postvisit( const ReferenceType * refType ) {
				// don't print prefix (e.g. 'R') for reference types so that references and non-references do not overload.
				// Further, do not print the qualifiers for a reference type (but do run printQualifers because of TypeDecls, etc.),
				// by pretending every reference type is a function parameter.
				GuardValue( inFunctionType );
				inFunctionType = true;
				printQualifiers( refType );
				maybeAccept( refType->base, *visitor );
			}

			namespace {
				inline std::list< Type* > getTypes( const std::list< DeclarationWithType* > decls ) {
					std::list< Type* > ret;
					std::transform( decls.begin(), decls.end(), std::back_inserter( ret ),
									std::mem_fun( &DeclarationWithType::get_type ) );
					return ret;
				}
			}

			void Mangler_old::postvisit( const FunctionType * functionType ) {
				printQualifiers( functionType );
				mangleName += Encoding::function;
				// turn on inFunctionType so that printQualifiers does not print most qualifiers for function parameters,
				// since qualifiers on outermost parameter type do not differentiate function types, e.g.,
				// void (*)(const int) and void (*)(int) are the same type, but void (*)(const int *) and void (*)(int *) are different
				GuardValue( inFunctionType );
				inFunctionType = true;
				std::list< Type* > returnTypes = getTypes( functionType->returnVals );
				if (returnTypes.empty()) mangleName += Encoding::void_t;
				else acceptAll( returnTypes, *visitor );
				mangleName += "_";
				std::list< Type* > paramTypes = getTypes( functionType->parameters );
				acceptAll( paramTypes, *visitor );
				mangleName += "_";
			}

			void Mangler_old::mangleRef( const ReferenceToType * refType, std::string prefix ) {
				printQualifiers( refType );

				mangleName += prefix + std::to_string( refType->name.length() ) + refType->name;

				if ( mangleGenericParams ) {
					const std::list< Expression* > & params = refType->parameters;
					if ( ! params.empty() ) {
						mangleName += "_";
						for ( const Expression * param : params ) {
							const TypeExpr * paramType = dynamic_cast< const TypeExpr * >( param );
							assertf(paramType, "Aggregate parameters should be type expressions: %s", toCString(param));
							maybeAccept( paramType->type, *visitor );
						}
						mangleName += "_";
					}
				}
			}

			void Mangler_old::postvisit( const StructInstType * aggregateUseType ) {
				mangleRef( aggregateUseType, Encoding::struct_t );
			}

			void Mangler_old::postvisit( const UnionInstType * aggregateUseType ) {
				mangleRef( aggregateUseType, Encoding::union_t );
			}

			void Mangler_old::postvisit( const EnumInstType * aggregateUseType ) {
				mangleRef( aggregateUseType, Encoding::enum_t );
			}

			void Mangler_old::postvisit( const TypeInstType * typeInst ) {
				VarMapType::iterator varNum = varNums.find( typeInst->get_name() );
				if ( varNum == varNums.end() ) {
					mangleRef( typeInst, Encoding::type );
				} else {
					printQualifiers( typeInst );
					// Note: Can't use name here, since type variable names do not actually disambiguate a function, e.g.
					//   forall(dtype T) void f(T);
					//   forall(dtype S) void f(S);
					// are equivalent and should mangle the same way. This is accomplished by numbering the type variables when they
					// are first found and prefixing with the appropriate encoding for the type class.
					assertf( varNum->second.second < TypeDecl::NUMBER_OF_KINDS, "Unhandled type variable kind: %d", varNum->second.second );
					mangleName += Encoding::typeVariables[varNum->second.second] + std::to_string( varNum->second.first );
				} // if
			}

			void Mangler_old::postvisit( const TraitInstType * inst ) {
				printQualifiers( inst );
				mangleName += std::to_string( inst->name.size() ) + inst->name;
			}

			void Mangler_old::postvisit( const TupleType * tupleType ) {
				printQualifiers( tupleType );
				mangleName += Encoding::tuple + std::to_string( tupleType->types.size() );
				acceptAll( tupleType->types, *visitor );
			}

			void Mangler_old::postvisit( const VarArgsType * varArgsType ) {
				printQualifiers( varArgsType );
				static const std::string vargs = "__builtin_va_list";
				mangleName += Encoding::type + std::to_string( vargs.size() ) + vargs;
			}

			void Mangler_old::postvisit( const ZeroType * ) {
				mangleName += Encoding::zero;
			}

			void Mangler_old::postvisit( const OneType * ) {
				mangleName += Encoding::one;
			}

			void Mangler_old::postvisit( const QualifiedType * qualType ) {
				bool inqual = inQualifiedType;
				if (! inqual ) {
					// N marks the start of a qualified type
					inQualifiedType = true;
					mangleName += Encoding::qualifiedTypeStart;
				}
				maybeAccept( qualType->parent, *visitor );
				maybeAccept( qualType->child, *visitor );
				if ( ! inqual ) {
					// E marks the end of a qualified type
					inQualifiedType = false;
					mangleName += Encoding::qualifiedTypeEnd;
				}
			}

			void Mangler_old::postvisit( const TypeDecl * decl ) {
				// TODO: is there any case where mangling a TypeDecl makes sense? If so, this code needs to be
				// fixed to ensure that two TypeDecls mangle to the same name when they are the same type and vice versa.
				// Note: The current scheme may already work correctly for this case, I have not thought about this deeply
				// and the case has not yet come up in practice. Alternatively, if not then this code can be removed
				// aside from the assert false.
				assertf( false, "Mangler_old should not visit typedecl: %s", toCString(decl));
				assertf( decl->kind < TypeDecl::NUMBER_OF_KINDS, "Unhandled type variable kind: %d", decl->kind );
				mangleName += Encoding::typeVariables[ decl->kind ] + std::to_string( decl->name.length() ) + decl->name;
			}

			__attribute__((unused)) void printVarMap( const std::map< std::string, std::pair< int, int > > &varMap, std::ostream &os ) {
				for ( std::map< std::string, std::pair< int, int > >::const_iterator i = varMap.begin(); i != varMap.end(); ++i ) {
					os << i->first << "(" << i->second.first << "/" << i->second.second << ")" << std::endl;
				} // for
			}

			void Mangler_old::printQualifiers( const Type * type ) {
				// skip if not including qualifiers
				if ( typeMode ) return;
				if ( ! type->forall.empty() ) {
					std::list< std::string > assertionNames;
					int dcount = 0, fcount = 0, vcount = 0, acount = 0;
					mangleName += Encoding::forall;
					for ( const TypeDecl * i : type->forall ) {
						switch ( i->kind ) {
						  case TypeDecl::Dtype:
							dcount++;
							break;
						  case TypeDecl::Ftype:
							fcount++;
							break;
						  case TypeDecl::Ttype:
							vcount++;
							break;
						  default:
							assertf( false, "unimplemented kind for type variable %s", SymTab::Mangler::Encoding::typeVariables[i->kind].c_str() );
						} // switch
						varNums[ i->name ] = std::make_pair( nextVarNum, (int)i->kind );
						for ( const DeclarationWithType * assert : i->assertions ) {
							PassVisitor<Mangler_old> sub_mangler(
								mangleOverridable, typeMode, mangleGenericParams, nextVarNum, varNums );
							assert->accept( sub_mangler );
							assertionNames.push_back( sub_mangler.pass.get_mangleName() );
							acount++;
						} // for
					} // for
					mangleName += std::to_string( dcount ) + "_" + std::to_string( fcount ) + "_" + std::to_string( vcount ) + "_" + std::to_string( acount ) + "_";
					for(const auto & a : assertionNames) mangleName += a;
//					std::copy( assertionNames.begin(), assertionNames.end(), std::ostream_iterator< std::string >( mangleName, "" ) );
					mangleName += "_";
				} // if
				if ( ! inFunctionType ) {
					// these qualifiers do not distinguish the outermost type of a function parameter
					if ( type->get_const() ) {
						mangleName += Encoding::qualifiers.at(Type::Const);
					} // if
					if ( type->get_volatile() ) {
						mangleName += Encoding::qualifiers.at(Type::Volatile);
					} // if
					// Removed due to restrict not affecting function compatibility in GCC
					// if ( type->get_isRestrict() ) {
					// 	mangleName += "E";
					// } // if
					if ( type->get_atomic() ) {
						mangleName += Encoding::qualifiers.at(Type::Atomic);
					} // if
				}
				if ( type->get_mutex() ) {
					mangleName += Encoding::qualifiers.at(Type::Mutex);
				} // if
				if ( inFunctionType ) {
					// turn off inFunctionType so that types can be differentiated for nested qualifiers
					GuardValue( inFunctionType );
					inFunctionType = false;
				}
			}
		} // namespace
	} // namespace Mangler
} // namespace SymTab

namespace Mangle {
	namespace {
		/// Mangles names to a unique C identifier
		struct Mangler_new : public ast::WithShortCircuiting, public ast::WithVisitorRef<Mangler_new>, public ast::WithGuards {
			Mangler_new( Mangle::Mode mode );
			Mangler_new( const Mangler_new & ) = delete;

			void previsit( const ast::Node * ) { visit_children = false; }

			void postvisit( const ast::ObjectDecl * declaration );
			void postvisit( const ast::FunctionDecl * declaration );
			void postvisit( const ast::TypeDecl * declaration );

			void postvisit( const ast::VoidType * voidType );
			void postvisit( const ast::BasicType * basicType );
			void postvisit( const ast::PointerType * pointerType );
			void postvisit( const ast::ArrayType * arrayType );
			void postvisit( const ast::ReferenceType * refType );
			void postvisit( const ast::FunctionType * functionType );
			void postvisit( const ast::StructInstType * aggregateUseType );
			void postvisit( const ast::UnionInstType * aggregateUseType );
			void postvisit( const ast::EnumInstType * aggregateUseType );
			void postvisit( const ast::TypeInstType * aggregateUseType );
			void postvisit( const ast::TraitInstType * inst );
			void postvisit( const ast::TupleType * tupleType );
			void postvisit( const ast::VarArgsType * varArgsType );
			void postvisit( const ast::ZeroType * zeroType );
			void postvisit( const ast::OneType * oneType );
			void postvisit( const ast::QualifiedType * qualType );

			/// The result is the current constructed mangled name.
			std::string result() const { return mangleName; }
		  private:
			std::string mangleName;         ///< Mangled name being constructed
			typedef std::map< std::string, std::pair< int, int > > VarMapType;
			VarMapType varNums;             ///< Map of type variables to indices
			int nextVarNum;                 ///< Next type variable index
			bool isTopLevel;                ///< Is the Mangler at the top level
			bool mangleOverridable;         ///< Specially mangle overridable built-in methods
			bool typeMode;                  ///< Produce a unique mangled name for a type
			bool mangleGenericParams;       ///< Include generic parameters in name mangling if true
			bool inFunctionType = false;    ///< Include type qualifiers if false.
			bool inQualifiedType = false;   ///< Add start/end delimiters around qualified type

		  private:
			Mangler_new( bool mangleOverridable, bool typeMode, bool mangleGenericParams,
				int nextVarNum, const VarMapType& varNums );
			friend class ast::Pass<Mangler_new>;

		  private:
			void mangleDecl( const ast::DeclWithType *declaration );
			void mangleRef( const ast::BaseInstType *refType, const std::string & prefix );

			void printQualifiers( const ast::Type *type );
		}; // Mangler_new
	} // namespace

	std::string mangle( const ast::Node * decl, Mangle::Mode mode ) {
		return ast::Pass<Mangler_new>::read( decl, mode );
	}

	namespace {
		Mangler_new::Mangler_new( Mangle::Mode mode )
			: nextVarNum( 0 ), isTopLevel( true ),
			mangleOverridable  ( ! mode.no_overrideable   ),
			typeMode           (   mode.type              ),
			mangleGenericParams( ! mode.no_generic_params ) {}

		Mangler_new::Mangler_new( bool mangleOverridable, bool typeMode, bool mangleGenericParams,
			int nextVarNum, const VarMapType& varNums )
			: varNums( varNums ), nextVarNum( nextVarNum ), isTopLevel( false ),
			mangleOverridable( mangleOverridable ), typeMode( typeMode ),
			mangleGenericParams( mangleGenericParams ) {}

		void Mangler_new::mangleDecl( const ast::DeclWithType * decl ) {
			bool wasTopLevel = isTopLevel;
			if ( isTopLevel ) {
				varNums.clear();
				nextVarNum = 0;
				isTopLevel = false;
			} // if
			mangleName += Encoding::manglePrefix;
			const CodeGen::OperatorInfo * opInfo = CodeGen::operatorLookup( decl->name );
			if ( opInfo ) {
				mangleName += std::to_string( opInfo->outputName.size() ) + opInfo->outputName;
			} else {
				mangleName += std::to_string( decl->name.size() ) + decl->name;
			} // if
			maybeAccept( decl->get_type(), *visitor );
			if ( mangleOverridable && decl->linkage.is_overrideable ) {
				// want to be able to override autogenerated and intrinsic routines,
				// so they need a different name mangling
				if ( decl->linkage == ast::Linkage::AutoGen ) {
					mangleName += Encoding::autogen;
				} else if ( decl->linkage == ast::Linkage::Intrinsic ) {
					mangleName += Encoding::intrinsic;
				} else {
					// if we add another kind of overridable function, this has to change
					assert( false && "unknown overrideable linkage" );
				} // if
			}
			isTopLevel = wasTopLevel;
		}

		void Mangler_new::postvisit( const ast::ObjectDecl * decl ) {
			mangleDecl( decl );
		}

		void Mangler_new::postvisit( const ast::FunctionDecl * decl ) {
			mangleDecl( decl );
		}

		void Mangler_new::postvisit( const ast::VoidType * voidType ) {
			printQualifiers( voidType );
			mangleName += Encoding::void_t;
		}

		void Mangler_new::postvisit( const ast::BasicType * basicType ) {
			printQualifiers( basicType );
			assertf( basicType->kind < ast::BasicType::NUMBER_OF_BASIC_TYPES, "Unhandled basic type: %d", basicType->kind );
			mangleName += Encoding::basicTypes[ basicType->kind ];
		}

		void Mangler_new::postvisit( const ast::PointerType * pointerType ) {
			printQualifiers( pointerType );
			// mangle void (*f)() and void f() to the same name to prevent overloading on functions and function pointers
			if ( ! pointerType->base.as<ast::FunctionType>() ) mangleName += Encoding::pointer;
			maybe_accept( pointerType->base.get(), *visitor );
		}

		void Mangler_new::postvisit( const ast::ArrayType * arrayType ) {
			// TODO: encode dimension
			printQualifiers( arrayType );
			mangleName += Encoding::array + "0";
			maybeAccept( arrayType->base.get(), *visitor );
		}

		void Mangler_new::postvisit( const ast::ReferenceType * refType ) {
			// don't print prefix (e.g. 'R') for reference types so that references and non-references do not overload.
			// Further, do not print the qualifiers for a reference type (but do run printQualifers because of TypeDecls, etc.),
			// by pretending every reference type is a function parameter.
			GuardValue( inFunctionType );
			inFunctionType = true;
			printQualifiers( refType );
			maybeAccept( refType->base.get(), *visitor );
		}

		void Mangler_new::postvisit( const ast::FunctionType * functionType ) {
			printQualifiers( functionType );
			mangleName += Encoding::function;
			// turn on inFunctionType so that printQualifiers does not print most qualifiers for function parameters,
			// since qualifiers on outermost parameter type do not differentiate function types, e.g.,
			// void (*)(const int) and void (*)(int) are the same type, but void (*)(const int *) and void (*)(int *) are different
			GuardValue( inFunctionType );
			inFunctionType = true;
			if (functionType->returns.empty()) mangleName += Encoding::void_t;
			else accept_each( functionType->returns, *visitor );
			mangleName += "_";
			accept_each( functionType->params, *visitor );
			mangleName += "_";
		}

		void Mangler_new::mangleRef(
				const ast::BaseInstType * refType, const std::string & prefix ) {
			printQualifiers( refType );

			mangleName += prefix + std::to_string( refType->name.length() ) + refType->name;

			if ( mangleGenericParams && ! refType->params.empty() ) {
				mangleName += "_";
				for ( const ast::Expr * param : refType->params ) {
					auto paramType = dynamic_cast< const ast::TypeExpr * >( param );
					assertf(paramType, "Aggregate parameters should be type expressions: %s", toCString(param));
					maybeAccept( paramType->type.get(), *visitor );
				}
				mangleName += "_";
			}
		}

		void Mangler_new::postvisit( const ast::StructInstType * aggregateUseType ) {
			mangleRef( aggregateUseType, Encoding::struct_t );
		}

		void Mangler_new::postvisit( const ast::UnionInstType * aggregateUseType ) {
			mangleRef( aggregateUseType, Encoding::union_t );
		}

		void Mangler_new::postvisit( const ast::EnumInstType * aggregateUseType ) {
			mangleRef( aggregateUseType, Encoding::enum_t );
		}

		void Mangler_new::postvisit( const ast::TypeInstType * typeInst ) {
			VarMapType::iterator varNum = varNums.find( typeInst->name );
			if ( varNum == varNums.end() ) {
				mangleRef( typeInst, Encoding::type );
			} else {
				printQualifiers( typeInst );
				// Note: Can't use name here, since type variable names do not actually disambiguate a function, e.g.
				//   forall(dtype T) void f(T);
				//   forall(dtype S) void f(S);
				// are equivalent and should mangle the same way. This is accomplished by numbering the type variables when they
				// are first found and prefixing with the appropriate encoding for the type class.
				assertf( varNum->second.second < TypeDecl::NUMBER_OF_KINDS, "Unhandled type variable kind: %d", varNum->second.second );
				mangleName += Encoding::typeVariables[varNum->second.second] + std::to_string( varNum->second.first );
			} // if
		}

		void Mangler_new::postvisit( const ast::TraitInstType * inst ) {
			printQualifiers( inst );
			mangleName += std::to_string( inst->name.size() ) + inst->name;
		}

		void Mangler_new::postvisit( const ast::TupleType * tupleType ) {
			printQualifiers( tupleType );
			mangleName += Encoding::tuple + std::to_string( tupleType->types.size() );
			accept_each( tupleType->types, *visitor );
		}

		void Mangler_new::postvisit( const ast::VarArgsType * varArgsType ) {
			printQualifiers( varArgsType );
			static const std::string vargs = "__builtin_va_list";
			mangleName += Encoding::type + std::to_string( vargs.size() ) + vargs;
		}

		void Mangler_new::postvisit( const ast::ZeroType * ) {
			mangleName += Encoding::zero;
		}

		void Mangler_new::postvisit( const ast::OneType * ) {
			mangleName += Encoding::one;
		}

		void Mangler_new::postvisit( const ast::QualifiedType * qualType ) {
			bool inqual = inQualifiedType;
			if (! inqual ) {
				// N marks the start of a qualified type
				inQualifiedType = true;
				mangleName += Encoding::qualifiedTypeStart;
			}
			maybeAccept( qualType->parent.get(), *visitor );
			maybeAccept( qualType->child.get(), *visitor );
			if ( ! inqual ) {
				// E marks the end of a qualified type
				inQualifiedType = false;
				mangleName += Encoding::qualifiedTypeEnd;
			}
		}

		void Mangler_new::postvisit( const ast::TypeDecl * decl ) {
			// TODO: is there any case where mangling a TypeDecl makes sense? If so, this code needs to be
			// fixed to ensure that two TypeDecls mangle to the same name when they are the same type and vice versa.
			// Note: The current scheme may already work correctly for this case, I have not thought about this deeply
			// and the case has not yet come up in practice. Alternatively, if not then this code can be removed
			// aside from the assert false.
			assertf(false, "Mangler_new should not visit typedecl: %s", toCString(decl));
			assertf( decl->kind < ast::TypeDecl::Kind::NUMBER_OF_KINDS, "Unhandled type variable kind: %d", decl->kind );
			mangleName += Encoding::typeVariables[ decl->kind ] + std::to_string( decl->name.length() ) + decl->name;
		}

		// For debugging:
		__attribute__((unused)) void printVarMap( const std::map< std::string, std::pair< int, int > > &varMap, std::ostream &os ) {
			for ( std::map< std::string, std::pair< int, int > >::const_iterator i = varMap.begin(); i != varMap.end(); ++i ) {
				os << i->first << "(" << i->second.first << "/" << i->second.second << ")" << std::endl;
			} // for
		}

		void Mangler_new::printQualifiers( const ast::Type * type ) {
			// skip if not including qualifiers
			if ( typeMode ) return;
			auto funcType = dynamic_cast<const ast::FunctionType *>( type );
			if ( funcType && !funcType->forall.empty() ) {
				std::list< std::string > assertionNames;
				int dcount = 0, fcount = 0, vcount = 0, acount = 0;
				mangleName += Encoding::forall;
				for ( auto & decl : funcType->forall ) {
					switch ( decl->kind ) {
					case ast::TypeDecl::Dtype:
						dcount++;
						break;
					case ast::TypeDecl::Ftype:
						fcount++;
						break;
					case ast::TypeDecl::Ttype:
						vcount++;
						break;
					default:
						assertf( false, "unimplemented kind for type variable %s", SymTab::Mangler::Encoding::typeVariables[decl->kind].c_str() );
					} // switch
					varNums[ decl->name ] = std::make_pair( nextVarNum, (int)decl->kind );
				} // for
				for ( auto & assert : funcType->assertions ) {
					assertionNames.push_back( ast::Pass<Mangler_new>::read(
						assert->var.get(),
						mangleOverridable, typeMode, mangleGenericParams, nextVarNum, varNums ) );
					acount++;
				} // for
				mangleName += std::to_string( dcount ) + "_" + std::to_string( fcount ) + "_" + std::to_string( vcount ) + "_" + std::to_string( acount ) + "_";
				for ( const auto & a : assertionNames ) mangleName += a;
				mangleName += "_";
			} // if
			if ( ! inFunctionType ) {
				// these qualifiers do not distinguish the outermost type of a function parameter
				if ( type->is_const() ) {
					mangleName += Encoding::qualifiers.at(Type::Const);
				} // if
				if ( type->is_volatile() ) {
					mangleName += Encoding::qualifiers.at(Type::Volatile);
				} // if
				// Removed due to restrict not affecting function compatibility in GCC
				// if ( type->get_isRestrict() ) {
				// 	mangleName += "E";
				// } // if
				if ( type->is_atomic() ) {
					mangleName += Encoding::qualifiers.at(Type::Atomic);
				} // if
			}
			if ( type->is_mutex() ) {
				mangleName += Encoding::qualifiers.at(Type::Mutex);
			} // if
			if ( inFunctionType ) {
				// turn off inFunctionType so that types can be differentiated for nested qualifiers
				GuardValue( inFunctionType );
				inFunctionType = false;
			}
		}
	}	// namespace
} // namespace Mangle

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
