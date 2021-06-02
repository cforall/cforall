//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolvProtoDump.cc -- Translates CFA resolver instances into resolv-proto instances
//
// Author           : Aaron Moss
// Created On       : Tue Sep 11 09:04:00 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Sat Feb 15 13:50:11 2020
// Update Count     : 3
//

#include <algorithm>
#include <cctype>
#include <iostream>
#include <memory>
#include <list>
#include <set>
#include <sstream>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "Common/PassVisitor.h"
#include "Common/utility.h"
#include "CodeGen/OperatorTable.h"
#include "SynTree/Declaration.h"
#include "SynTree/Expression.h"
#include "SynTree/Initializer.h"
#include "SynTree/Statement.h"
#include "SynTree/Type.h"

namespace CodeTools {

	/// Visitor for dumping resolver prototype output
	class ProtoDump : public WithShortCircuiting, public WithVisitorRef<ProtoDump> {
		std::set<std::string> decls;             ///< Declarations in this scope
		std::vector<std::string> exprs;          ///< Expressions in this scope
		std::vector<ProtoDump> subs;             ///< Sub-scopes
		std::unordered_set<std::string> closed;  ///< Closed type variables
		const ProtoDump* parent;                 ///< Outer lexical scope
		std::unique_ptr<Type> rtnType;           ///< Return type for this scope

	public:
		/// Default constructor for root ProtoDump
		ProtoDump() : decls(), exprs(), subs(), closed(), parent(nullptr), rtnType(nullptr) {}

		/// Child constructor
		ProtoDump(const ProtoDump* p, Type* r)
			: decls(), exprs(), subs(), closed(p->closed), parent(p), rtnType(r) {}

		// Fix copy issues
		ProtoDump(const ProtoDump& o)
			: decls(o.decls), exprs(o.exprs), subs(o.subs), closed(o.closed), parent(o.parent),
			  rtnType(maybeClone(o.rtnType.get())) {}
		ProtoDump( ProtoDump&& ) = default;

		ProtoDump& operator= (const ProtoDump& o) {
			if ( this == &o ) return *this;

			decls = o.decls;
			exprs = o.exprs;
			subs = o.subs;
			closed = o.closed;
			parent = o.parent;
			rtnType.reset( maybeClone(o.rtnType.get()) );

			return *this;
		}
		ProtoDump& operator= (ProtoDump&&) = delete;

	private:
		/// checks if this declaration is contained in the scope or one of its parents
		bool hasDecl( const std::string& s ) const {
			if ( decls.count( s ) ) return true;
			if ( parent ) return parent->hasDecl( s );
			return false;
		}

		/// adds a new declaration to this scope, providing it does not already exist
		void addDecl( const std::string& s ) {
			if ( ! hasDecl( s ) ) decls.insert( s );
		}

		/// adds a new expression to this scope
		void addExpr( const std::string& s ) {
			if ( ! s.empty() ) { exprs.emplace_back( s ); }
		}

		/// adds a new subscope to this scope, returning a reference
		void addSub( PassVisitor<ProtoDump>&& sub ) {
			subs.emplace_back( std::move(sub.pass) );
		}

		/// Whether lists should be separated, terminated, or preceded by their separator
		enum septype { separated, terminated, preceded };

		/// builds space-separated list of types
		template<typename V>
		static void build( V& visitor, const std::list< Type* >& tys, std::stringstream& ss,
				septype mode = separated ) {
			if ( tys.empty() ) return;

			if ( mode == preceded ) { ss << ' '; }

			auto it = tys.begin();
			(*it)->accept( visitor );

			while ( ++it != tys.end() ) {
				ss << ' ';
				(*it)->accept( visitor );
			}

			if ( mode == terminated ) { ss << ' '; }
		}

		/// builds list of types wrapped as tuple type
		template<typename V>
		static void buildAsTuple( V& visitor, const std::list< Type* >& tys,
				std::stringstream& ss ) {
			switch ( tys.size() ) {
				case 0: ss << "#void"; break;
				case 1: tys.front()->accept( visitor ); break;
				default:
					ss << "#$" << tys.size() << '<';
					build( visitor, tys, ss );
					ss << '>';
					break;
			}
		}

		/// gets types from DWT list
		static std::list< Type* > from_decls( const std::list< DeclarationWithType* >& decls ) {
			std::list< Type* > tys;
			for ( auto decl : decls ) { tys.emplace_back( decl->get_type() ); }
			return tys;
		}

		/// gets types from TypeExpr list
		static std::list< Type* > from_exprs( const std::list< Expression* >& exprs ) {
			std::list< Type* > tys;
			for ( auto expr : exprs ) {
				if ( TypeExpr* tyExpr = dynamic_cast<TypeExpr*>(expr) ) {
					tys.emplace_back( tyExpr->type );
				}
			}
			return tys;
		}

		/// builds prefixes for rp_name
		static std::string new_prefix( const std::string& old, const char* added ) {
			if ( old.empty() ) return std::string{"$"} + added;
			return old + added;
		}

		/// shortens operator names
		static void op_name( const std::string& name, std::stringstream& ss ) {
			if ( name.compare( 0, 10, "_operator_" ) == 0 ) {
				ss << name.substr(10);
			} else if ( name.compare( "_constructor" ) == 0
					|| name.compare( "_destructor" ) == 0 ) {
				ss << name.substr(1);
			} else if ( name.compare( 0, 11, "__operator_" ) == 0 ) {
				ss << name.substr(11);
			} else {
				ss << name;
			}
		}

		/// replaces operators with resolv-proto names
		static void rp_name( const std::string& name, std::stringstream& ss,
				std::string&& pre = "" ) {
			// safety check for anonymous names
			if ( name.empty() ) {
				ss << new_prefix(pre, "anon");
				return;
			}

			// replace operator names
			const CodeGen::OperatorInfo * opInfo = CodeGen::operatorLookup( name );
			if ( opInfo ) {
				ss << new_prefix(pre, "");
				op_name( opInfo->outputName, ss );
				return;
			}

			// replace retval names
			if ( name.compare( 0, 8, "_retval_" ) == 0 ) {
				ss << new_prefix(pre, "rtn_");
				op_name( name.substr(8), ss );
				return;
			}

			// default to just name, with first character in lowercase
			ss << pre
			   << (char)std::tolower( static_cast<unsigned char>(name[0]) )
			   << (name.c_str() + 1);
		}

		/// ensures type inst names are uppercase
		static void ti_name( const std::string& name, std::stringstream& ss ) {
			// replace built-in wide character types with named types
			if ( name == "char16_t" || name == "char32_t" || name == "wchar_t" ) {
				ss << "#" << name;
				return;
			}

			// strip leading underscore
			unsigned i = 0;
			while ( i < name.size() && name[i] == '_' ) { ++i; }
			if ( i == name.size() ) {
				ss << "Anon";
				return;
			}

			std::string stripped = name.substr(i);
			// strip trailing "_generic_" from autogen names (avoids some user-generation issues)
			char generic[] = "_generic_"; size_t n_generic = sizeof(generic) - 1;
			if ( stripped.size() >= n_generic
					&& stripped.substr( stripped.size() - n_generic ) == generic ) {
				stripped.resize( stripped.size() - n_generic );
			}

			// uppercase first character
			ss << (char)std::toupper( static_cast<unsigned char>(stripped[0]) )
			   << (stripped.c_str() + 1);
		}

		/// Visitor for printing types
		struct TypePrinter : public WithShortCircuiting, WithVisitorRef<TypePrinter>, WithGuards {
			std::stringstream& ss;                          ///< Output to print to
			const std::unordered_set<std::string>& closed;  ///< Closed type variables
			unsigned depth;                                 ///< Depth of nesting from root type

			TypePrinter( const std::unordered_set<std::string>& closed, std::stringstream& ss )
				: ss(ss), closed(closed), depth(0) {}

			// basic type represented as integer type
			// TODO maybe hard-code conversion graph and make named type
			void previsit( BasicType* bt ) { ss << (int)bt->get_kind(); }

			// pointers (except function pointers) represented as generic type
			void previsit( PointerType* pt ) {
				if ( ! dynamic_cast<FunctionType*>(pt->base) ) { ss << "#$ptr<"; ++depth; }
			}
			void postvisit( PointerType* pt ) {
				if ( ! dynamic_cast<FunctionType*>(pt->base) ) { --depth; ss << '>'; }
			}

			// arrays represented as generic pointers
			void previsit( ArrayType* at ) {
				ss << "#$ptr<";
				++depth;
				at->base->accept( *visitor );
				--depth;
				ss << '>';
				visit_children = false;
			}

			// ignore top-level reference types, they're mostly transparent to resolution
			void previsit( ReferenceType* ) {
				if ( depth > 0 ) { ss << "#$ref<"; }
				++depth;
			}
			void postvisit( ReferenceType* ) {
				--depth;
				if ( depth > 0 ) { ss << '>'; }
			}

			// print function types using prototype syntax
			void previsit( FunctionType* ft ) {
				ss << '[';
				++depth;
				build( *visitor, from_decls( ft->returnVals ), ss, preceded );
				ss << " : ";
				build( *visitor, from_decls( ft->parameters ), ss, terminated );
				--depth;
				ss << ']';
				visit_children = false;
			}

		private:
			// prints aggregate type name as NamedType with optional paramters
			void handleAggregate( ReferenceToType* at ) {
				ss << '#' << at->name;
				if ( ! at->parameters.empty() ) {
					ss << '<';
					++depth;
					build( *visitor, from_exprs( at->parameters ), ss );
					--depth;
					ss << '>';
				}
				visit_children = false;
			}

		public:
			// handle aggregate types using NamedType
			void previsit( StructInstType* st ) { handleAggregate( st ); }
			void previsit( UnionInstType* ut ) { handleAggregate( ut ); }

			// replace enums with int
			void previsit( EnumInstType* ) { ss << (int)BasicType::SignedInt; }

			void previsit( TypeInstType* vt ) {
				// print closed variables as named types
				if ( closed.count( vt->name ) ) { ss << '#' << vt->name; }
				// otherwise make sure first letter is capitalized
				else { ti_name( vt->name, ss ); }
			}

			// flattens empty and singleton tuples
			void previsit( TupleType* tt ) {
				++depth;
				buildAsTuple( *visitor, tt->types, ss );
				--depth;
				visit_children = false;
			}

			// TODO support variable args for functions
			void previsit( VarArgsType* ) {
				// only include varargs for top level (argument type)
				if ( depth == 0 ) { ss << "#$varargs"; }
			}

			// replace 0 and 1 with int
			// TODO support 0 and 1 with their proper type names and conversions
			void previsit( ZeroType* ) { ss << (int)BasicType::SignedInt; }
			void previsit( OneType* ) { ss << (int)BasicType::SignedInt; }

			// only print void type if not at top level
			void previsit( VoidType* ) {
				if ( depth > 0 ) { ss << "#void"; }
			}
		};

		/// builds description of function
		void build( const std::string& name, FunctionType* fnTy, std::stringstream& ss ) {
			PassVisitor<TypePrinter> printTy{ closed, ss };
			// print return values
			build( printTy, from_decls( fnTy->returnVals ), ss, terminated );
			// print name
			rp_name( name, ss );
			// print parameters
			build( printTy, from_decls( fnTy->parameters ), ss, preceded );
			// print assertions
			for ( TypeDecl* tyvar : fnTy->forall ) {
				for ( DeclarationWithType* assn : tyvar->assertions ) {
					ss << " | ";
					build( assn->name, assn->get_type(), ss );
				}
			}
		}

		/// builds description of a variable (falls back to function if function type)
		void build( const std::string& name, Type* ty, std::stringstream& ss ) {
			// ignore top-level references
			Type *norefs = ty->stripReferences();

			// fall back to function declaration if function type
			if ( PointerType* pTy = dynamic_cast< PointerType* >(norefs) ) {
				if ( FunctionType* fnTy = dynamic_cast< FunctionType* >(pTy->base) ) {
					build( name, fnTy, ss );
					return;
				}
			} else if ( FunctionType* fnTy = dynamic_cast< FunctionType* >(norefs) ) {
				build( name, fnTy, ss );
				return;
			}

			// print variable declaration in prototype syntax
			PassVisitor<TypePrinter> printTy{ closed, ss };
			norefs->accept( printTy );
			ss << " &";
			rp_name( name, ss );
		}

		/// builds description of a field access
		void build( const std::string& name, AggregateDecl* agg, Type* ty, std::stringstream& ss ) {
			// ignore top-level references
			Type *norefs = ty->stripReferences();

			// print access as new field name
			PassVisitor<TypePrinter> printTy{ closed, ss };
			norefs->accept( printTy );
			ss << ' ';
			rp_name( name, ss, "$field_" );
			ss << " #" << agg->name;
			// handle type parameters
			if ( ! agg->parameters.empty() ) {
				ss << '<';
				auto it = agg->parameters.begin();
				while (true) {
					ti_name( (*it)->name, ss );
					if ( ++it == agg->parameters.end() ) break;
					ss << ' ';
				}
				ss << '>';
			}
		}

		/// Visitor for printing expressions
		struct ExprPrinter : WithShortCircuiting, WithVisitorRef<ExprPrinter> {
			// TODO change interface to generate multiple expression candidates
			const std::unordered_set<std::string>& closed;  ///< set of closed type vars
			std::stringstream& ss;                          ///< Output to print to

			ExprPrinter( const std::unordered_set<std::string>& closed, std::stringstream& ss )
				: closed(closed), ss(ss) {}

			/// Names handled as name expressions
			void previsit( NameExpr* expr ) {
				ss << '&';
				rp_name( expr->name, ss );
			}

			/// Handle already-resolved variables as type constants
			void previsit( VariableExpr* expr ) {
				PassVisitor<TypePrinter> tyPrinter{ closed, ss };
				expr->var->get_type()->accept( tyPrinter );
				visit_children = false;
			}

			/// Calls handled as calls
			void previsit( UntypedExpr* expr ) {
				// TODO handle name extraction more generally
				NameExpr* name = dynamic_cast<NameExpr*>(expr->function);

				// fall back on just resolving call to function name
				// TODO incorporate function type into resolv-proto
				if ( ! name ) {
					expr->function->accept( *visitor );
					visit_children = false;
					return;
				}

				rp_name( name->name, ss );
				if ( expr->args.empty() ) {
					ss << "()";
				} else {
					ss << "( ";
					auto it = expr->args.begin();
					while (true) {
						(*it)->accept( *visitor );
						if ( ++it == expr->args.end() ) break;
						ss << ' ';
					}
					ss << " )";
				}
				visit_children = false;
			}

			/// Already-resolved calls reduced to their type constant
			void previsit( ApplicationExpr* expr ) {
				PassVisitor<TypePrinter> tyPrinter{ closed, ss };
				expr->result->accept( tyPrinter );
				visit_children = false;
			}

			/// Address-of handled as operator
			void previsit( AddressExpr* expr ) {
				ss << "$addr( ";
				expr->arg->accept( *visitor );
				ss << " )";
				visit_children = false;
			}

			/// Casts replaced with result type
			/// TODO put cast target functions in, and add second expression for target
			void previsit( CastExpr* cast ) {
				PassVisitor<TypePrinter> tyPrinter{ closed, ss };
				cast->result->accept( tyPrinter );
				visit_children = false;
			}

			/// Member access handled as function from aggregate to member
			void previsit( UntypedMemberExpr* expr ) {
				// TODO handle name extraction more generally
				NameExpr* name = dynamic_cast<NameExpr*>(expr->member);

				// fall back on just resolving call to member name
				// TODO incorporate function type into resolv-proto
				if ( ! name ) {
					expr->member->accept( *visitor );
					visit_children = false;
					return;
				}

				rp_name( name->name, ss, "$field_" );
				ss << "( ";
				expr->aggregate->accept( *visitor );
				ss << " )";
				visit_children = false;
			}

			/// Constant expression replaced by its type
			void previsit( ConstantExpr* expr ) {
				PassVisitor<TypePrinter> tyPrinter{ closed, ss };
				expr->constant.get_type()->accept( tyPrinter );
				visit_children = false;
			}

			/// sizeof( ... ), alignof( ... ), offsetof( ... ) replaced by unsigned long constant
			/// TODO extra expression to resolve argument
			void previsit( SizeofExpr* ) {
				ss << (int)BasicType::LongUnsignedInt;
				visit_children = false;
			}
			void previsit( AlignofExpr* ) {
				ss << (int)BasicType::LongUnsignedInt;
				visit_children = false;
			}
			void previsit( UntypedOffsetofExpr* ) {
				ss << (int)BasicType::LongUnsignedInt;
				visit_children = false;
			}

			/// Logical expressions represented as operators
			void previsit( LogicalExpr* expr ) {
				ss << '$' << ( expr->get_isAnd() ? "and" : "or" ) << "( ";
				expr->arg1->accept( *visitor );
				ss << ' ';
				expr->arg2->accept( *visitor );
				ss << " )";
				visit_children = false;
			}

			/// Conditional expression represented as operator
			void previsit( ConditionalExpr* expr ) {
				ss << "$if( ";
				expr->arg1->accept( *visitor );
				ss << ' ';
				expr->arg2->accept( *visitor );
				ss << ' ';
				expr->arg3->accept( *visitor );
				ss << " )";
				visit_children = false;
			}

			/// Comma expression represented as operator
			void previsit( CommaExpr* expr ) {
				ss << "$seq( ";
				expr->arg1->accept( *visitor );
				ss << ' ';
				expr->arg2->accept( *visitor );
				ss << " )";
				visit_children = false;
			}

			// TODO handle ignored ImplicitCopyCtorExpr and below
		};

		void build( Initializer* init, std::stringstream& ss ) {
			if ( SingleInit* si = dynamic_cast<SingleInit*>(init) ) {
				PassVisitor<ExprPrinter> exprPrinter{ closed, ss };
				si->value->accept( exprPrinter );
				ss << ' ';
			} else if ( ListInit* li = dynamic_cast<ListInit*>(init) ) {
				for ( Initializer* it : li->initializers ) {
					build( it, ss );
				}
			}
		}

		/// Adds an object initializer to the list of expressions
		void build( const std::string& name, Initializer* init, std::stringstream& ss ) {
			ss << "$constructor( &";
			rp_name( name, ss );
			ss << ' ';
			build( init, ss );
			ss << ')';
		}

		/// Adds a return expression to the list of expressions
		void build( Type* rtnType, Expression* expr, std::stringstream& ss ) {
			ss << "$constructor( ";
			PassVisitor<TypePrinter> tyPrinter{ closed, ss };
			rtnType->accept( tyPrinter );
			ss << ' ';
			PassVisitor<ExprPrinter> exprPrinter{ closed, ss };
			expr->accept( exprPrinter );
			ss << " )";
		}

		/// Adds all named declarations in a list to the local scope
		void addAll( const std::list<DeclarationWithType*>& decls ) {
			for ( auto decl : decls ) {
				// skip anonymous decls
				if ( decl->name.empty() ) continue;

				// handle objects
				if ( ObjectDecl* obj = dynamic_cast< ObjectDecl* >( decl ) ) {
					previsit( obj );
				}
			}
		}

		/// encode field access as function
		void addAggregateFields( AggregateDecl* agg ) {
			// make field names functions
			for ( Declaration* member : agg->members ) {
				if ( ObjectDecl* obj = dynamic_cast< ObjectDecl* >(member) ) {
					std::stringstream ss;
					build( obj->name, agg, obj->type, ss );
					addDecl( ss.str() );
				}
			}

			visit_children = false;
		}

	public:
		void previsit( ObjectDecl *obj ) {
			// add variable as declaration
			std::stringstream ss;
			build( obj->name, obj->type, ss );
			addDecl( ss.str() );

			// add initializer as expression if applicable
			if ( obj->init ) {
				std::stringstream ss;
				build( obj->name, obj->init, ss );
				addExpr( ss.str() );
			}
		}

		void previsit( FunctionDecl *decl ) {
			// skip decls with ftype parameters
			for ( TypeDecl* tyvar : decl->type->forall ) {
				if ( tyvar->get_kind() == TypeDecl::Ftype ) {
					visit_children = false;
					return;
				}
			}

			// add function as declaration
			std::stringstream ss;
			build( decl->name, decl->type, ss );
			addDecl( ss.str() );

			// add body if available
			if ( decl->statements ) {
				std::list<Type*> rtns = from_decls( decl->type->returnVals );
				Type* rtn = nullptr;
				if ( rtns.size() == 1 ) {
					if ( ! dynamic_cast<VoidType*>(rtns.front()) ) rtn = rtns.front()->clone();
				} else if ( rtns.size() > 1 ) {
					rtn = new TupleType{ Type::Qualifiers{}, rtns };
				}
				PassVisitor<ProtoDump> body{ this, rtn };

				for ( TypeDecl* tyvar : decl->type->forall ) {
					// add set of "closed" types to body so that it can print them as NamedType
					body.pass.closed.insert( tyvar->name );

					// add assertions to local scope as declarations as well
					for ( DeclarationWithType* assn : tyvar->assertions ) {
						assn->accept( body );
					}
				}

				// add named parameters and returns to local scope
				body.pass.addAll( decl->type->returnVals );
				body.pass.addAll( decl->type->parameters );

				// add contents of function to new scope
				decl->statements->accept( body );

				// store sub-scope
				addSub( std::move(body) );
			}

			visit_children = false;
		}

		void previsit( StructDecl* sd ) { addAggregateFields(sd); }
		void previsit( UnionDecl* ud ) { addAggregateFields(ud); }

		void previsit( EnumDecl* ed ) {
			std::unique_ptr<Type> eType =
				std::make_unique<BasicType>( Type::Qualifiers{}, BasicType::SignedInt );

			// add field names directly to enclosing scope
			for ( Declaration* member : ed->members ) {
				if ( ObjectDecl* obj = dynamic_cast< ObjectDecl* >(member) ) {
					previsit(obj);
				}
			}

			visit_children = false;
		}

		void previsit( ReturnStmt* stmt ) {
			// do nothing for void-returning functions or statements returning nothing
			if ( ! rtnType || ! stmt->expr ) return;

			// otherwise construct the return type from the expression
			std::stringstream ss;
			build( rtnType.get(), stmt->expr, ss );
			addExpr( ss.str() );
			visit_children = false;
		}

		void previsit( AsmStmt* ) {
			// skip asm statements
			visit_children = false;
		}

		void previsit( Expression* expr ) {
			std::stringstream ss;
			PassVisitor<ExprPrinter> exPrinter{ closed, ss };
			expr->accept( exPrinter );
			addExpr( ss.str() );
			visit_children = false;
		}

		/// Print non-prelude global declarations for resolv proto
		void printGlobals() const {
			std::cout << "#$ptr<T> $addr T" << std::endl;  // &?
			int i = (int)BasicType::SignedInt;
			std::cout << i << " $and " << i << ' ' << i << std::endl;  // ?&&?
			std::cout << i << " $or " << i << ' ' << i << std::endl;  // ?||?
			std::cout << "T $if " << i << " T T" << std::endl; // ternary operator
			std::cout << "T $seq X T" << std::endl;  // ?,?
		}

	public:
		/// Prints this ProtoDump instance
		void print(unsigned indent = 0) const {
			// print globals at root level
			if ( ! parent ) printGlobals();
			// print decls
			std::string tab( indent, '\t' );
			for ( const std::string& d : decls ) {
				std::cout << tab << d << std::endl;
			}
			// print divider
			std::cout << '\n' << tab << "%%\n" << std::endl;
			// print top-level expressions
			for ( const std::string& e : exprs ) {
				std::cout << tab << e << std::endl;
			}
			// print child scopes
			++indent;
			for ( const ProtoDump & s : subs ) {
				std::cout << tab << '{' << std::endl;
				s.print( indent );
				std::cout << tab << '}' << std::endl;
			}
		}
	};

	void dumpAsResolvProto( std::list< Declaration * > &translationUnit ) {
		PassVisitor<ProtoDump> dump;
		acceptAll( translationUnit, dump );
		dump.pass.print();
	}

}  // namespace CodeTools

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
