//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Counter.cpp --
//
// Author           : Thierry Delisle
// Created On       : Thu Feb 28 13::27:10 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include "Counter.hpp"

#include <algorithm>
#include <cstring>
#include <functional>
#include <iomanip>

namespace Stats {
	namespace Counters {
		void print() {
			if(!top.head) return;
			size_t nc = 0;
			Base::ForAll(top, 0, [&](Base::TreeImpl * node, size_t level) {
				nc = std::max(nc, (4 * level) + std::strlen(node->name));
			});

			const char * const title = "Counter Statistic";
			size_t nct = nc + 14;
			std::cerr << std::string(nct, '=') << std::endl;
			std::cerr << std::string((nct - std::strlen(title)) / 2, ' ');
			std::cerr << title << std::endl;
			std::cerr << std::string(nct, '-') << std::endl;


			Base::ForAll(top, 0, [&](Base::TreeImpl * node, size_t level) {
				std::cerr << std::string(level * 4, ' ');
				std::cerr << node->name;
				std::cerr << std::string(nc - ((level * 4) + std::strlen(node->name)), ' ');
				std::cerr << " | ";
				std::cerr << std::setw(9);
				node->print(std::cerr);
				std::cerr << " |";
				std::cerr << '\n';
			}, true);

			std::cerr << std::string(nct, '-') << std::endl;
		}

		Base::TreeTop top;

		extern bool enabled;
	}
}
