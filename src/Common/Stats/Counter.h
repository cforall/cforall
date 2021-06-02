//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Counter.h --
//
// Author           : Thierry Delisle
// Created On       : Thu Feb 28 12::05:10 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#pragma once

#include <cstdint>
#include <iostream>

#include "Common/Stats/Base.h"

#if defined( NO_STATISTICS )
	#define NO_COUNTER_STATISTICS
#endif

namespace Stats {
	namespace Counters {
# 		if defined(NO_COUNTERS_STATISTICS)

			static inline void print() {}

			class CounterGroup {
			public:
			};

			class SimpleCounter {
			public:
				inline void operator++() {}
				inline void operator++(int) {}
				inline void operator+=(size_t) {}
			};

			template<typename T>
			class AverageCounter {
			public:
				inline void push(T value) {}
			};

			template<typename T>
			class MaxCounter {
			public:
				inline void push(T value) {}
			};

			template<typename counter_t>
			counter_t * build(const char * const name) {
				return nullptr;
			}

			template<typename counter_t>
			counter_t * build(const char * const name, Base::Tree<top> * parent) {
					return nullptr;
			}
#		else
			extern bool enabled;

			extern Base::TreeTop top;

			class CounterGroup : public Base::Tree<top> {
			public:
				CounterGroup(const char * const name ) : Base::Tree<top>(name) {}
				CounterGroup(const char * const name, Base::Tree<top> * parent) : Base::Tree<top>(name, parent) {}

				virtual void print(std::ostream & os) override { os << ""; }
			protected:
				virtual ~CounterGroup() = default;
			};

			class SimpleCounter : public Base::Tree<top> {
			public:
				SimpleCounter(const char * const name ) : Base::Tree<top>(name) {}
				SimpleCounter(const char * const name, Base::Tree<top> * parent) : Base::Tree<top>(name, parent) {}

				virtual void print(std::ostream & os) override { os << count; }

				inline void operator++()             { if(!enabled) return; count++;        }
				inline void operator++(int)          { if(!enabled) return; count++;        }
				inline void operator+=(size_t value) { if(!enabled) return; count += value; }

			protected:
				virtual ~SimpleCounter() = default;

			private:
				size_t count = 0;
			};

			template<typename T>
			class AverageCounter : public Base::Tree<top> {
			public:
				AverageCounter(const char * const name ) : Base::Tree<top>(name), sum{} {}
				AverageCounter(const char * const name, Base::Tree<top> * parent) : Base::Tree<top>(name, parent), sum{} {}

				virtual void print(std::ostream & os) { os << sum / count; }

				inline void push(T value) { if(!enabled) return; sum += value; count++; }

			protected:
				virtual ~AverageCounter() = default;

			private:
				T sum;
				size_t count = 1;
			};

			template<typename T>
			class MaxCounter : public Base::Tree<top> {
			public:
				MaxCounter(const char * const name ) : Base::Tree<top>(name), max{} {}
				MaxCounter(const char * const name, Base::Tree<top> * parent) : Base::Tree<top>(name, parent), max{} {}

				virtual void print(std::ostream & os) { os << max; }

				inline void push(T value) { if(!enabled) return; max = std::max(max, value); }

			protected:
				virtual ~MaxCounter() = default;

			private:
				T max;
			};

			template<typename counter_t>
			counter_t * build(const char * const name) {
				if(!enabled) return nullptr;
				return new counter_t(name);
			}

			template<typename counter_t>
			counter_t * build(const char * const name, Base::Tree<top> * parent) {
				if(!enabled) return nullptr;
				return new counter_t(name, parent);
			}
#		endif
	}
}
