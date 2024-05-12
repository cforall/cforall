//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Time.cpp --
//
// Author           : Thierry Delisle
// Created On       : Mon Mar 04 15:16:07 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "Time.hpp"

#include <cassert>
#include <chrono>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <iomanip>
#include <stack>

namespace Stats {
	namespace Time {
#		if !defined(NO_TIME_STATISTICS)
			extern bool enabled;

			Base::TreeTop top;

			typedef  std::chrono::time_point<std::chrono::high_resolution_clock> point_t;
			std::chrono::duration<double> total;

			point_t global_begin;

			int prevl = 0;
			int currl = 0;

			template<typename T>
			static inline std::ostream & operator<<(std::ostream & os, const std::chrono::duration<T> & dd) {
				auto d = std::chrono::duration_cast<std::chrono::milliseconds>(dd);
				auto minutes = std::chrono::duration_cast<std::chrono::minutes>(d);
				auto seconds = std::chrono::duration_cast<std::chrono::seconds>(d % std::chrono::minutes(1));
				auto millis  = std::chrono::duration_cast<std::chrono::milliseconds>(d % std::chrono::seconds(1));

				bool zmin = minutes == minutes.zero();
				bool zsec = seconds == seconds.zero();
				bool zmil = millis  == millis .zero();

				if(!zmin) {
					os << std::setw(4) << minutes.count() << "m";
				} else {
					os << std::string(5, ' ');
				}

				if(!zmin || !zsec) {
					if(!zmin) os << std::setfill('0');
					os << std::setw(2) << seconds.count() << "s";
				} else {
					os << std::string(3, ' ');
				}
				os << std::setfill(' ');

				if(!zmin || !zsec || !zmil) {
					if(!zmin || !zsec) os << std::setfill('0');
					os << std::setw(3) << millis .count();
				} else {
					os << std::string(4, ' ');
				}
				os << std::setfill(' ');

				return os;
			}

			class TimerNode : public Base::Tree<top> {
			public:
				TimerNode(const char * const name )
					: Base::Tree<top>(name)
				{}

				TimerNode(const char * const name, Base::Tree<top> * parent)
					: Base::Tree<top>(name, parent)

				{}

				virtual void print(std::ostream & os) override {
					if(currl > prevl) {
						parents.push(last);
					}
					for(auto lvl = prevl - currl; lvl > 0; lvl--) {
						parents.pop();
					}
					last = end - begin;

					assert(finished);
					std::chrono::duration<double> diff = end - begin;
					os << diff << " | ";
					if(parents.empty()) {
						os << "     N/A | ";
					} else {
						os << std::setw(7) << std::setprecision(0);
						os << size_t(100.0 * diff.count() / parents.top().count()) << "% | ";
					}
					os << std::setw(5) << std::setprecision(0);
					os << size_t(100.0 * diff.count() / total.count()) << "% ";
				}

				void start() {
					begin = std::chrono::high_resolution_clock::now();
				}

				void finish() {
					end = std::chrono::high_resolution_clock::now();
					finished = true;
				}

			protected:
				virtual ~TimerNode() = default;

			private:
				bool finished = false;

				point_t begin;
				point_t end;

				static std::chrono::duration<double> last;
				static std::stack<std::chrono::duration<double>> parents;
			};

			std::stack<TimerNode *> nodes;

			std::chrono::duration<double> TimerNode::last;
			std::stack<std::chrono::duration<double>> TimerNode::parents;

			void StartGlobal() {
				global_begin = std::chrono::high_resolution_clock::now();
			}

			void StartBlock(const char * const name) {
				if(!enabled) return;
				auto node = nodes.empty()
					? new TimerNode(name)
					: new TimerNode(name, nodes.top());

				nodes.push(node);
				node->start();
			}

			void StopBlock() {
				if(!enabled) return;
				nodes.top()->finish();
				nodes.pop();
			}

			void print() {
				if(!top.head) return;
				auto global_end = std::chrono::high_resolution_clock::now();
				total = global_end - global_begin;

				size_t nc = 0;
				Base::ForAll(top, 0, [&](Base::TreeImpl * node, size_t level) {
					nc = std::max(nc, (4 * level) + std::strlen(node->name));
				});

				size_t nct = nc + 37;
				std::cerr << std::string(nct, '=') << std::endl;
				const char * const title = "Timing Results";
				std::cerr << std::string((nct - std::strlen(title)) / 2, ' ');
				std::cerr << title << std::endl;
				std::cerr << std::string(nct, '-') << std::endl;
				std::cerr << "Location";
				std::cerr << std::string(nc - (std::strlen("Location")), ' ');
				std::cerr << " | ";
				std::cerr << "       Time | ";
				std::cerr << "% parent | ";
				std::cerr << "% total |" << std::endl;
				std::cerr << std::string(nct, '-') << std::endl;

				Base::ForAll(top, 0, [&](Base::TreeImpl * node, size_t level) {
					currl = level;
					std::cerr << std::string(level * 4, ' ');
					std::cerr << node->name;
					std::cerr << std::string(nc - ((level * 4) + std::strlen(node->name)), ' ');
					std::cerr << " | ";
					node->print(std::cerr);
					std::cerr << " |";
					std::cerr << '\n';
					prevl = level;
				}, true);

				std::cerr << std::string(nct, '-') << std::endl;
				std::cerr << "Total " << total << std::endl;
				std::cerr << std::string(nct, '-') << std::endl;
			}
#		endif
	}
}
