//
// Cforall Version 1.0.0 Copyright (C) 2018 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Demangle.cc -- Convert a mangled name into a human readable name.
//
// Author           : Rob Schluntz
// Created On       : Thu Jul 19 12:52:41 2018
// Last Modified By : Peter A. Buhr
// Last Modified On : Mon Jan 11 21:28:27 2021
// Update Count     : 11
//

#include <algorithm>
#include <sstream>

#include "CodeGen/GenType.h"
#include "Common/PassVisitor.h"
#include "Common/utility.h"								// isPrefix
#include "Mangler.h"
#include "SynTree/Type.h"
#include "SynTree/Declaration.h"

#define DEBUG
#ifdef DEBUG
#define PRINT(x) x
#else
#define PRINT(x) {}
#endif

namespace {
	struct GenType : public WithVisitorRef<GenType>, public WithShortCircuiting {
		std::string typeString;
		GenType( const std::string &typeString );

		void previsit( BaseSyntaxNode * );
		void postvisit( BaseSyntaxNode * );

		void postvisit( FunctionType * funcType );
		void postvisit( VoidType * voidType );
		void postvisit( BasicType * basicType );
		void postvisit( PointerType * pointerType );
		void postvisit( ArrayType * arrayType );
		void postvisit( ReferenceType * refType );
		void postvisit( StructInstType * structInst );
		void postvisit( UnionInstType * unionInst );
		void postvisit( EnumInstType * enumInst );
		void postvisit( TypeInstType * typeInst );
		void postvisit( TupleType  * tupleType );
		void postvisit( VarArgsType * varArgsType );
		void postvisit( ZeroType * zeroType );
		void postvisit( OneType * oneType );
		void postvisit( GlobalScopeType * globalType );
		void postvisit( QualifiedType * qualType );

	  private:
		void handleQualifiers( Type *type );
		std::string handleGeneric( ReferenceToType * refType );
		void genArray( const Type::Qualifiers &qualifiers, Type *base, Expression *dimension, bool isVarLen, bool isStatic );
	};

  std::string genDemangleType( Type * type, const std::string & baseString ) {
		PassVisitor<GenType> gt( baseString );
		assert( type );
		type->accept( gt );
		return gt.pass.typeString;
  }

	GenType::GenType( const std::string &typeString ) : typeString( typeString ) {}

	// *** BaseSyntaxNode
	void GenType::previsit( BaseSyntaxNode * ) {
		// turn off automatic recursion for all nodes, to allow each visitor to
		// precisely control the order in which its children are visited.
		visit_children = false;
	}

	void GenType::postvisit( BaseSyntaxNode * node ) {
		std::stringstream ss;
		node->print( ss );
		assertf( false, "Unhandled node reached in GenType: %s", ss.str().c_str() );
	}

	void GenType::postvisit( VoidType * voidType ) {
		typeString = "void " + typeString;
		handleQualifiers( voidType );
	}

	void GenType::postvisit( BasicType * basicType ) {
		BasicType::Kind kind = basicType->kind;
		assert( 0 <= kind && kind < BasicType::NUMBER_OF_BASIC_TYPES );
		typeString = std::string( BasicType::typeNames[kind] ) + " " + typeString;
		handleQualifiers( basicType );
	}

	void GenType::genArray( const Type::Qualifiers & qualifiers, Type * base, Expression *dimension, bool isVarLen, bool ) {
		std::ostringstream os;
		if ( typeString != "" ) {
			if ( typeString[ 0 ] == '*' ) {
				os << "(" << typeString << ")";
			} else {
				os << typeString;
			} // if
		} // if
		os << "[";

		if ( qualifiers.is_const ) {
			os << "const ";
		} // if
		if ( qualifiers.is_volatile ) {
			os << "volatile ";
		} // if
		if ( qualifiers.is_restrict ) {
			os << "__restrict ";
		} // if
		if ( qualifiers.is_atomic ) {
			os << "_Atomic ";
		} // if
		if ( dimension != 0 ) {
			// TODO: ???
			// PassVisitor<CodeGenerator> cg( os, pretty, genC, lineMarks );
			// dimension->accept( cg );
		} else if ( isVarLen ) {
			// no dimension expression on a VLA means it came in with the * token
			os << "*";
		} // if
		os << "]";

		typeString = os.str();

		base->accept( *visitor );
	}

	void GenType::postvisit( PointerType * pointerType ) {
		assert( pointerType->base != 0);
		if ( pointerType->get_isStatic() || pointerType->get_isVarLen() || pointerType->dimension ) {
			assert(false);
			genArray( pointerType->get_qualifiers(), pointerType->base, pointerType->dimension, pointerType->get_isVarLen(), pointerType->get_isStatic() );
		} else {
			handleQualifiers( pointerType );
			if ( typeString[ 0 ] == '?' ) {
				typeString = "* " + typeString;
			} else {
				typeString = "*" + typeString;
			} // if
			pointerType->base->accept( *visitor );
		} // if
	}

	void GenType::postvisit( ArrayType * arrayType ) {
		genArray( arrayType->get_qualifiers(), arrayType->base, arrayType->dimension, arrayType->get_isVarLen(), arrayType->get_isStatic() );
	}

	void GenType::postvisit( ReferenceType * refType ) {
		assert( false );
		assert( refType->base != 0);
		handleQualifiers( refType );
		typeString = "&" + typeString;
		refType->base->accept( *visitor );
	}

	void GenType::postvisit( FunctionType * funcType ) {
		std::ostringstream os;

		if ( typeString != "" ) {
			if ( typeString[0] == '*' ) {
				os << "(" << typeString << ")";
			} else {
				os << typeString;
			} // if
		} // if

		/************* parameters ***************/
		const std::list<DeclarationWithType *> &pars = funcType->parameters;

		if ( pars.empty() ) {
			if ( funcType->get_isVarArgs() ) {
				os << "()";
			} else {
				os << "(void)";
			} // if
		} else {
			os << "(" ;

			unsigned int i = 0;
			for (DeclarationWithType * p : pars) {
				os << genDemangleType( p->get_type(), "" );
				if (++i != pars.size()) os << ", ";
			}

			if ( funcType->get_isVarArgs() ) {
				os << ", ...";
			} // if
			os << ")";
		} // if

		typeString = os.str();

		if ( funcType->returnVals.size() == 0 ) {
			typeString += ": void";
		} else {
			typeString += ": " + genDemangleType(funcType->returnVals.front()->get_type(), "");
		} // if

		// add forall
		if( ! funcType->forall.empty() ) {
			std::ostringstream os;
			os << "forall(";
			unsigned int i = 0;
			for ( auto td : funcType->forall ) {
				os << td->typeString() << " " << td->name;
				if (! td->assertions.empty()) {
					os << " | { ";
					unsigned int j = 0;
					for (DeclarationWithType * assert : td->assertions) {
						os << genDemangleType(assert->get_type(), assert->name);
						if (++j != td->assertions.size()) os << ", ";
					}
					os << "}";
				}
				if (++i != funcType->forall.size()) os << ", ";
			}
			os << ")";
			typeString = typeString + " -> " + os.str();
		}
	}

	std::string GenType::handleGeneric( ReferenceToType * refType ) {
		if ( ! refType->parameters.empty() ) {
			std::ostringstream os;
			// TODO: ???
			// PassVisitor<CodeGenerator> cg( os, pretty, genC, lineMarks );
			os << "(";
			// cg.pass.genCommaList( refType->parameters.begin(), refType->parameters.end() );
			os << ") ";
			return os.str();
		}
		return "";
	}

	void GenType::postvisit( StructInstType * structInst )  {
		typeString = "struct " + structInst->name + handleGeneric( structInst ) + " " + typeString;
		handleQualifiers( structInst );
	}

	void GenType::postvisit( UnionInstType * unionInst ) {
		typeString = "union " + unionInst->name + handleGeneric( unionInst ) + " " + typeString;
		handleQualifiers( unionInst );
	}

	void GenType::postvisit( EnumInstType * enumInst ) {
		typeString = "enum " + enumInst->name + " " + typeString;
		handleQualifiers( enumInst );
	}

	void GenType::postvisit( TypeInstType * typeInst ) {
		typeString = typeInst->name + " " + typeString;
		handleQualifiers( typeInst );
	}

	void GenType::postvisit( TupleType * tupleType ) {
		unsigned int i = 0;
		std::ostringstream os;
		os << "[";
		for ( Type * t : *tupleType ) {
			i++;
			os << genDemangleType( t, "" ) << (i == tupleType->size() ? "" : ", ");
		}
		os << "] ";
		typeString = os.str() + typeString;
	}

	void GenType::postvisit( VarArgsType * varArgsType ) {
		typeString = "__builtin_va_list " + typeString;
		handleQualifiers( varArgsType );
	}

	void GenType::postvisit( ZeroType * zeroType ) {
		// ideally these wouldn't hit codegen at all, but should be safe to make them ints
		typeString = "zero_t " + typeString;
		handleQualifiers( zeroType );
	}

	void GenType::postvisit( OneType * oneType ) {
		// ideally these wouldn't hit codegen at all, but should be safe to make them ints
		typeString = "one_t " + typeString;
		handleQualifiers( oneType );
	}

	void GenType::postvisit( GlobalScopeType * globalType ) {
		handleQualifiers( globalType );
	}

	void GenType::postvisit( QualifiedType * qualType ) {
		std::ostringstream os;
		os << genDemangleType( qualType->parent, "" ) << "." << genDemangleType( qualType->child, "" ) << typeString;
		typeString = os.str();
		handleQualifiers( qualType );
	}

	void GenType::handleQualifiers( Type * type ) {
		if ( type->get_const() ) {
			typeString = "const " + typeString;
		} // if
		if ( type->get_volatile() ) {
			typeString = "volatile " + typeString;
		} // if
		if ( type->get_restrict() ) {
			typeString = "__restrict " + typeString;
		} // if
		if ( type->get_atomic() ) {
			typeString = "_Atomic " + typeString;
		} // if
	}
}


namespace SymTab {
	namespace Mangler {
		namespace {
			struct StringView {
			private:
				std::string str;
				size_t idx = 0;
				// typedef Type * (StringView::*parser)(Type::Qualifiers);
				typedef std::function<Type * (Type::Qualifiers)> parser;
				std::vector<std::pair<std::string, parser>> parsers;
			public:
				StringView(const std::string & str);

				bool done() const { return idx >= str.size(); }
				char cur() const { assert(! done()); return str[idx]; }

				bool expect(char ch) { return str[idx++] == ch;	}
				void next(size_t inc = 1) { idx += inc; }

				/// determines if `pref` is a prefix of `str`
				bool isPrefix(const std::string & pref);
				bool extractNumber(size_t & out);
				bool extractName(std::string & out);
				bool stripMangleName(std::string & name);

				Type * parseFunction(Type::Qualifiers tq);
				Type * parseTuple(Type::Qualifiers tq);
				Type * parseVoid(Type::Qualifiers tq);
				Type * parsePointer(Type::Qualifiers tq);
				Type * parseArray(Type::Qualifiers tq);
				Type * parseStruct(Type::Qualifiers tq);
				Type * parseUnion(Type::Qualifiers tq);
				Type * parseEnum(Type::Qualifiers tq);
				Type * parseType(Type::Qualifiers tq);

				Type * parseType();
				bool parse(std::string & name, Type *& type);
			};

			StringView::StringView(const std::string & str) : str(str) {
				// basic types
				for (size_t k = 0; k < BasicType::NUMBER_OF_BASIC_TYPES; ++k) {
					parsers.emplace_back(Encoding::basicTypes[k], [k](Type::Qualifiers tq) {
						PRINT( std::cerr << "basic type: " << k << std::endl; )
						return new BasicType(tq, (BasicType::Kind)k);
					});
				}
				// type variable types
				for (size_t k = 0; k < TypeDecl::NUMBER_OF_KINDS; ++k) {
					static const std::string typeVariableNames[] = { "DT", "DST", "OT", "FT", "TT", "ALT", };
					static_assert(
						sizeof(typeVariableNames)/sizeof(typeVariableNames[0]) == TypeDecl::NUMBER_OF_KINDS,
						"Each type variable kind should have a demangle name prefix"
					);
					parsers.emplace_back(Encoding::typeVariables[k], [k, this](Type::Qualifiers tq) -> TypeInstType * {
						PRINT( std::cerr << "type variable type: " << k << std::endl; )
						size_t N;
						if (! extractNumber(N)) return nullptr;
						return new TypeInstType(tq, toString(typeVariableNames[k], N), (TypeDecl::Kind)k != TypeDecl::Ftype);
					});
				}
				// everything else
				parsers.emplace_back(Encoding::void_t, [this](Type::Qualifiers tq) { return parseVoid(tq); });
				parsers.emplace_back(Encoding::function, [this](Type::Qualifiers tq) { return parseFunction(tq); });
				parsers.emplace_back(Encoding::pointer, [this](Type::Qualifiers tq) { return parsePointer(tq); });
				parsers.emplace_back(Encoding::array, [this](Type::Qualifiers tq) { return parseArray(tq); });
				parsers.emplace_back(Encoding::tuple, [this](Type::Qualifiers tq) { return parseTuple(tq); });
				parsers.emplace_back(Encoding::struct_t, [this](Type::Qualifiers tq) { return parseStruct(tq); });
				parsers.emplace_back(Encoding::union_t, [this](Type::Qualifiers tq) { return parseUnion(tq); });
				parsers.emplace_back(Encoding::enum_t, [this](Type::Qualifiers tq) { return parseEnum(tq); });
				parsers.emplace_back(Encoding::type, [this](Type::Qualifiers tq) { return parseType(tq); });
				parsers.emplace_back(Encoding::zero, [](Type::Qualifiers tq) { return new ZeroType(tq); });
				parsers.emplace_back(Encoding::one, [](Type::Qualifiers tq) { return new OneType(tq); });
			}

			bool StringView::extractNumber(size_t & out) {
				std::stringstream numss;
				if (idx >= str.size()) return false;
				while (isdigit(str[idx])) {
					numss << str[idx];
					++idx;
					if (idx == str.size()) break;
				}
				if (! (numss >> out)) return false;
				PRINT( std::cerr << "extractNumber success: " << out << std::endl; )
				return true;
			}

			bool StringView::extractName(std::string & out) {
				size_t len;
				if (! extractNumber(len)) return false;
				if (idx+len > str.size()) return false;
				out = str.substr(idx, len);
				idx += len;
				PRINT( std::cerr << "extractName success: " << out << std::endl; )
				return true;
			}

			bool StringView::isPrefix(const std::string & pref) {
				// if ( pref.size() > str.size()-idx ) return false;
				// auto its = std::mismatch( pref.begin(), pref.end(), std::next(str.begin(), idx) );
				// if (its.first == pref.end()) {
				// 	idx += pref.size();
				// 	return true;
				// }

				// This update is untested because there are no tests for this code.
				if ( ::isPrefix( str, pref, idx ) ) {
					idx += pref.size();
					return true;
				}
				return false;
			}

			// strips __NAME__cfa__TYPE_N, where N is [0-9]+: returns str is a match is found, returns empty string otherwise
			bool StringView::stripMangleName(std::string & name) {
				PRINT( std::cerr << "====== " << str.size() << " " << str << std::endl; )
				if (str.size() < 2+Encoding::manglePrefix.size()) return false; // +2 for at least _1 suffix
				if ( ! isPrefix(Encoding::manglePrefix) || ! isdigit(str.back() ) ) return false;

				// get name
				if (! extractName(name)) return false;

				// find bounds for type
				PRINT( std::cerr << idx << " " << str.size() << std::endl; )
				PRINT( std::cerr << "[");
				while (isdigit(str.back())) {
					PRINT(std::cerr << ".");
					str.pop_back();
					if (str.size() <= idx) return false;
				}
				PRINT( std::cerr << "]" << std::endl );
				if (str.back() != '_') return false;
				str.pop_back();
				PRINT( std::cerr << str.size() << " " << name << " " << str.substr(idx) << std::endl; )
				return str.size() > idx;
			}

			Type * StringView::parseFunction(Type::Qualifiers tq) {
				PRINT( std::cerr << "function..." << std::endl; )
				if (done()) return nullptr;
				FunctionType * ftype = new FunctionType( tq, false );
				std::unique_ptr<Type> manager(ftype);
				Type * retVal = parseType();
				if (! retVal) return nullptr;
				PRINT( std::cerr << "with return type: " << retVal << std::endl; )
				ftype->returnVals.push_back(ObjectDecl::newObject("", retVal, nullptr));
				if (done() || ! expect('_')) return nullptr;
				while (! done()) {
					PRINT( std::cerr << "got ch: " << cur() << std::endl; )
					if (cur() == '_') return manager.release();
					Type * param = parseType();
					if (! param) return nullptr;
					PRINT( std::cerr << "with parameter : " << param << std::endl; )
					ftype->parameters.push_back(ObjectDecl::newObject("", param, nullptr));
				}
				return nullptr;
			}

			Type * StringView::parseTuple(Type::Qualifiers tq) {
				PRINT( std::cerr << "tuple..." << std::endl; )
				std::list< Type * > types;
				size_t ncomponents;
				if (! extractNumber(ncomponents)) return nullptr;
				for (size_t i = 0; i < ncomponents; ++i) {
					// TODO: delete all on return
					if (done()) return nullptr;
					PRINT( std::cerr << "got ch: " << cur() << std::endl; )
					Type * t = parseType();
					if (! t) return nullptr;
					PRINT( std::cerr << "with type : " << t << std::endl; )
					types.push_back(t);
				}
				return new TupleType( tq, types );
			}

			Type * StringView::parseVoid(Type::Qualifiers tq) {
				return new VoidType( tq );
			}

			Type * StringView::parsePointer(Type::Qualifiers tq) {
				PRINT( std::cerr << "pointer..." << std::endl; )
				Type * t = parseType();
				if (! t) return nullptr;
				return new PointerType( tq, t );
			}

			Type * StringView::parseArray(Type::Qualifiers tq) {
				PRINT( std::cerr << "array..." << std::endl; )
				size_t length;
				if (! extractNumber(length)) return nullptr;
				Type * t = parseType();
				if (! t) return nullptr;
				return new ArrayType( tq, t, new ConstantExpr( Constant::from_ulong(length) ), false, false );
			}

			Type * StringView::parseStruct(Type::Qualifiers tq) {
				PRINT( std::cerr << "struct..." << std::endl; )
				std::string name;
				if (! extractName(name)) return nullptr;
				return new StructInstType(tq, name);
			}

			Type * StringView::parseUnion(Type::Qualifiers tq) {
				PRINT( std::cerr << "union..." << std::endl; )
				std::string name;
				if (! extractName(name)) return nullptr;
				return new UnionInstType(tq, name);
			}

			Type * StringView::parseEnum(Type::Qualifiers tq) {
				PRINT( std::cerr << "enum..." << std::endl; )
				std::string name;
				if (! extractName(name)) return nullptr;
				return new EnumInstType(tq, name);
			}

			Type * StringView::parseType(Type::Qualifiers tq) {
				PRINT( std::cerr << "type..." << std::endl; )
				std::string name;
				if (! extractName(name)) return nullptr;
				PRINT( std::cerr << "typename..." << name << std::endl; )
				return new TypeInstType(tq, name, false);
			}

			Type * StringView::parseType() {
				if (done()) return nullptr;

				std::list<TypeDecl *> forall;
				if (isPrefix(Encoding::forall)) {
					PRINT( std::cerr << "polymorphic with..." << std::endl; )
					size_t dcount, fcount, vcount, acount;
					if (! extractNumber(dcount)) return nullptr;
					PRINT( std::cerr << dcount << " dtypes" << std::endl; )
					if (! expect('_')) return nullptr;
					if (! extractNumber(fcount)) return nullptr;
					PRINT( std::cerr << fcount << " ftypes" << std::endl; )
					if (! expect('_')) return nullptr;
					if (! extractNumber(vcount)) return nullptr;
					PRINT( std::cerr << vcount << " ttypes" << std::endl; )
					if (! expect('_')) return nullptr;
					if (! extractNumber(acount)) return nullptr;
					PRINT( std::cerr << acount << " assertions" << std::endl; )
					if (! expect('_')) return nullptr;
					for (size_t i = 0; i < acount; ++i) {
						// TODO: need to recursively parse assertions, but for now just return nullptr so that
						// demangler does not crash if there are assertions
						return nullptr;
					}
					if (! expect('_')) return nullptr;
				}

				// qualifiers
				Type::Qualifiers tq;
				while (true) {
					auto qual = std::find_if(Encoding::qualifiers.begin(), Encoding::qualifiers.end(), [this](decltype(Encoding::qualifiers)::value_type val) {
						return isPrefix(val.second);
					});
					if (qual == Encoding::qualifiers.end()) break;
					tq |= qual->first;
				}

				// find the correct type parser and use it
				auto iter = std::find_if(parsers.begin(), parsers.end(), [this](std::pair<std::string, parser> & p) {
					return isPrefix(p.first);
				});
				assertf(iter != parsers.end(), "Unhandled type letter: %c at index: %zd", cur(), idx);
				Type * ret = iter->second(tq);
				if (! ret) return nullptr;
				ret->forall = std::move(forall);
				return ret;
			}

			bool StringView::parse(std::string & name, Type *& type) {
				if (! stripMangleName(name)) return false;
				PRINT( std::cerr << "stripped name: " << name << std::endl; )
				Type * t = parseType();
				if (! t) return false;
				type = t;
				return true;
			}

			std::string demangle(const std::string & mangleName) {
				SymTab::Mangler::StringView view(mangleName);
				std::string name;
				Type * type = nullptr;
				if (! view.parse(name, type)) return mangleName;
				std::unique_ptr<Type> manager(type);
				return genDemangleType(type, name);
			}
		} // namespace
	} // namespace Mangler
} // namespace SymTab

extern "C" {
	char * cforall_demangle(const char * mangleName, int option __attribute__((unused))) {
		const std::string & demangleName = SymTab::Mangler::demangle(mangleName);
		return strdup(demangleName.c_str());
	}
}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
