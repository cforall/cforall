//
// Cforall Version 1.0.0 Copyright (C) 2019 University of Waterloo
//
// The contents of this file are covered under the licence agreement in the
// file "LICENCE" distributed with Cforall.
//
// Stats.cpp --
//
// Author           : Thierry Delisle
// Created On       : Fri Mar 01 15:45:08 2019
// Last Modified By :
// Last Modified On :
// Update Count     :
//

#include <iostream>
#include <sstream>
#include <string>


namespace Stats {
	namespace Counters {
		bool enabled = false;
		void print();
	}

	namespace Heap {
		bool enabled = false;
		void print();
	}

	namespace Time {
		bool enabled = false;
		void print();
	}

	namespace ResolveTime {
		bool enabled = false;
	}

	struct {
		const char * const opt;
		bool & enabled;
	}
	statistics[] = {
		{ "counters", Counters::enabled },
		{ "heap"    , Heap::enabled },
		{ "time"    , Time::enabled },
		{ "resolve" , ResolveTime::enabled },
	};

	void set_param(std::string & param) {
		if(param == "all") {
			for(auto & stat : statistics) {
				stat.enabled = true;
			}
			return;
		}

		if(param == "none") {
			for(auto & stat : statistics) {
				stat.enabled = false;
			}
			return;
		}

		for(auto & stat : statistics) {
			if(stat.opt == param) {
				stat.enabled = true;
				return;
			}
		}

		std::cerr << "Ignoring unknown statistic " << param << std::endl;
	}

	void parse_params(const char * const params) {
		std::stringstream ss(params);
		while(ss.good()) {
			std::string substr;
			getline( ss, substr, ',' );
			set_param(substr);
		}
	}

	void print() {
		Counters::print();
		Heap::print();
		Time::print();
	}
}
