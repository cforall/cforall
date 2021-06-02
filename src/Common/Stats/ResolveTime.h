//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// ResolveTime.h --
//
// Author           : Thierry Delisle
// Created On       : Wed Sep 16 15:45:51 2020
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once

#include "Common/Stats/Base.h"

#if defined( NO_STATISTICS )
	#define NO_RESOLVE_TIME_STATISTICS
#endif

namespace ast {
	class Expr;
}

namespace Stats {
	namespace ResolveTime {
		#if defined(NO_RESOLVE_TIME_STATISTICS)
			void start( const ast::Expr * ) {}
			void stop() {}
		#else
			void start( const ast::Expr * );
			void stop();
		#endif
	};
};