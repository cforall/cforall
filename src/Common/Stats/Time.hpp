//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Time.hpp --
//
// Author           : Thierry Delisle
// Created On       : Fri Mar 01 15:14:11 2019
// Last Modified By : Andrew Beach
// Last Modified On :
// Update Count     :
//

#pragma once

#include "Common/Stats/Base.hpp"

#if defined( NO_STATISTICS )
	#define NO_TIME_STATISTICS
#endif

namespace Stats {
	namespace Time {
#		if defined(NO_TIME_STATISTICS)
			inline void StartGlobal() {}

			inline void StartBlock(const char * const) {}
			inline void StopBlock() {}

			inline void print() {}

			struct BlockGuard {
				BlockGuard(const char * const) {}
				~BlockGuard() {}
			};

			template<typename func_t>
			inline void TimeBlock(const char *, func_t f) {
				f();
			}

			template<typename ret_t = void, typename func_t, typename... arg_t>
			inline ret_t TimeCall(
					const char *, func_t func, arg_t&&... arg) {
				return func(std::forward<arg_t>(arg)...);
			}
#		else
			void StartGlobal();

			void StartBlock(const char * const name);
			void StopBlock();

			void print();

			struct BlockGuard {
				BlockGuard(const char * const name ) { StartBlock(name); }
				~BlockGuard() { StopBlock(); }
			};

			template<typename func_t>
			inline void TimeBlock(const char * name, func_t func) {
				BlockGuard guard(name);
				func();
			}

			template<typename ret_t = void, typename func_t, typename... arg_t>
			inline ret_t TimeCall(
					const char * name, func_t func, arg_t&&... arg) {
				BlockGuard guard(name);
				return func(std::forward<arg_t>(arg)...);
			}
#		endif
	}
}
