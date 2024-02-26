//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Attribute.hpp --
//
// Author           : Aaron B. Moss
// Created On       : Fri May 10 10:30:00 2019
// Last Modified By : Peter A. Buhr
// Created On       : Fri May 10 10:30:00 2019
// Update Count     : 2
//

#pragma once

#include <string>
#include <vector>

#include "Fwd.hpp"
#include "Node.hpp"     // for ptr
#include "Visitor.hpp"

namespace ast {

class Expr;

/// An entry in an attribute list: `__attribute__(( ... ))`
class Attribute final : public Node {
public:
	std::string name;
	std::vector<ptr<Expr>> params;

	Attribute( const std::string & name = "", std::vector<ptr<Expr>> && params = {})
		: name( name ), params( params ) {}
	virtual ~Attribute() = default;

	bool empty() const { return name.empty(); }

	/// strip leading/trailing underscores and lowercase
	std::string normalizedName() const;

	/// true iff this attribute is allowed to appear attached to a function parameter
	bool isValidOnFuncParam() const;

	const Attribute * accept( Visitor & v ) const override { return v.visit( this ); }
private:
	Attribute * clone() const override { return new Attribute{ *this }; }

	/// Must be copied in ALL derived classes
	template<typename node_t>
	friend node_t * mutate(const node_t * node);
	template<typename node_t>
    friend node_t * shallowCopy(const node_t * node);
};

}

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
