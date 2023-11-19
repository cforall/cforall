//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolvMode.h --
//
// Author           : Aaron B. Moss
// Created On       : Mon Jun 11 13:28:00 2018
// Last Modified By : Aaron B. Moss
// Last Modified On : Fri Oct 05 13:46:00 2018
// Update Count     : 2
//

#pragma once

namespace ResolvExpr {

/// Flag set for resolution
struct ResolvMode {
	const bool adjust;			 ///< Adjust array and function types to pointer types? [false]
	const bool prune;            ///< Prune alternatives to min-cost per return type? [true]
	const bool failFast;         ///< Fail on no resulting alternatives? [true]

	constexpr ResolvMode(bool a, bool p, bool ff)
	: adjust(a), prune(p), failFast(ff) {}

	/// Default settings
	constexpr ResolvMode() : adjust(false), prune(true), failFast(true) {}

	/// With adjust flag set; turns array and function types into equivalent pointers
	static constexpr ResolvMode withAdjustment() { return { true, true, true }; }

	/// With adjust flag set but prune unset; pruning ensures there is at least one alternative
	/// per result type
	static constexpr ResolvMode withoutPrune() { return { true, false, true }; }

	/// With adjust and prune flags set but failFast unset; failFast ensures there is at least
	/// one resulting alternative
	static constexpr ResolvMode withoutFailFast() { return { true, true, false }; }

	/// The same mode, but with satisfyAssns turned on; for top-level calls
	ResolvMode atTopLevel() const { return { adjust, true, failFast }; }
};

} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
