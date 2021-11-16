//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Init.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Last Modified By : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Update Count     : 1
//

#pragma once

#include <deque>
#include <utility>        // for move
#include <vector>

#include "ParseNode.hpp"
#include "Node.hpp"       // for ptr
#include "Visitor.hpp"

// Must be included in *all* AST classes; should be #undef'd at the end of the file
#define MUTATE_FRIEND \
    template<typename node_t> friend node_t * mutate(const node_t * node); \
	template<typename node_t> friend node_t * shallowCopy(const node_t * node);

namespace ast {

class Expr;
class Stmt;

/// List of designator (NameExpr, VariableExpr, and ConstantExpr) expressions that specify an
/// object being initialized
class Designation final : public ParseNode {
public:
	std::deque<ptr<Expr>> designators;

	Designation( const CodeLocation& loc, std::deque<ptr<Expr>>&& ds = {} )
	: ParseNode( loc ), designators( std::move(ds) ) {}

	const Designation* accept( Visitor& v ) const override { return v.visit( this ); }
private:
	Designation* clone() const override { return new Designation{ *this }; }
	MUTATE_FRIEND
};

/// Flag for whether to construct from initialzier
enum ConstructFlag { NoConstruct, MaybeConstruct };

/// Object initializer base class
class Init : public ParseNode {
public:
	ConstructFlag maybeConstructed;

	Init( const CodeLocation & loc, ConstructFlag mc ) : ParseNode( loc ), maybeConstructed( mc ) {}

	const Init * accept( Visitor & v ) const override = 0;
private:
	Init * clone() const override = 0;
	MUTATE_FRIEND
};

/// Initializer for a common object: `int x = 4`
class SingleInit final : public Init {
public:
	/// value to initialize to. Must be compile-time constant.
	ptr<Expr> value;

	SingleInit( const CodeLocation & loc, const Expr * val, ConstructFlag mc = NoConstruct )
	: Init( loc, mc ), value( val ) {}

	const Init * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	SingleInit * clone() const override { return new SingleInit{ *this }; }
	MUTATE_FRIEND
};

/// Initializer recursively composed of a list of initializers.
/// Used to initialize an array or aggregate: `int a[] = { 1, 2, 3 }`
class ListInit final : public Init {
public:
	/// list of initializers
	std::vector<ptr<Init>> initializers;
	/// list of designators; order/length is consistent with initializers
	std::vector<ptr<Designation>> designations;

	ListInit( const CodeLocation & loc, std::vector<ptr<Init>> && is,
		std::vector<ptr<Designation>> && ds = {}, ConstructFlag mc = NoConstruct );

	using iterator = std::vector<ptr<Init>>::iterator;
	using const_iterator = std::vector<ptr<Init>>::const_iterator;
	iterator begin() { return initializers.begin(); }
	iterator end() { return initializers.end(); }
	const_iterator begin() const { return initializers.begin(); }
	const_iterator end() const { return initializers.end(); }
	size_t size() const { return initializers.size(); }

	const Init * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ListInit * clone() const override { return new ListInit{ *this }; }
	MUTATE_FRIEND
};

/// Either a constructor expression or a C-style initializer.
/// Should not be necessary to create manually; instead set `maybeConstructed` true on `SingleInit`
/// or `ListInit` if the object should be constructed.
class ConstructorInit final : public Init {
public:
	ptr<Stmt> ctor;
	ptr<Stmt> dtor;
	/// C-style initializer made up of SingleInit/ListInit nodes to use as a fallback if an
	/// appropriate constructor definition is not found by the resolver.
	ptr<Init> init;

	ConstructorInit( 
		const CodeLocation & loc, const Stmt * ctor, const Stmt * dtor, const Init * init )
	: Init( loc, MaybeConstruct ), ctor( ctor ), dtor( dtor ), init( init ) {}

	const Init * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	ConstructorInit * clone() const override { return new ConstructorInit{ *this }; }
	MUTATE_FRIEND
};

}

#undef MUTATE_FRIEND

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
