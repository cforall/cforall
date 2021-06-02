//
// Cforall Version 1.0.0 Copyright (C) 2015 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// WidenMode.h --
//
// Author           : Aaron B. Moss
// Created On       : Mon Jun 18 11:58:00 2018
// Last Modified By : Aaron B. Moss
// Last Modified On : Mon Jun 18 11:58:00 2018
// Update Count     : 1
//

#pragma once

namespace ResolvExpr {
	struct WidenMode {
		WidenMode( bool first, bool second ): first( first ), second( second ) {}
		
		WidenMode &operator|=( const WidenMode &other ) {
			first |= other.first; second |= other.second; return *this;
		}

		WidenMode &operator&=( const WidenMode &other ) {
			first &= other.first; second &= other.second; return *this;
		}

		WidenMode operator|( const WidenMode &other ) {
			WidenMode newWM( *this ); newWM |= other; return newWM;
		}

		WidenMode operator&( const WidenMode &other ) {
			WidenMode newWM( *this ); newWM &= other; return newWM;
		}
		
		operator bool() { return first && second; }

		bool first : 1, second : 1;
	};

	static inline WidenMode noWiden() { return { false, false }; }
} // namespace ResolvExpr

// Local Variables: //
// tab-width: 4 //
// mode: c++ //
// compile-command: "make install" //
// End: //
