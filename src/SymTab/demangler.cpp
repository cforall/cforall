#include "Demangle.hpp"
#include <iostream>
#include <fstream>

void demangleAndPrint(const std::string & mangleName) {
	char * demangleName = cforall_demangle(mangleName.c_str(), 0);
	std::cout << mangleName << " => " << demangleName << std::endl;
	free(demangleName);
}

int main(int argc, char * argv[]) {
	char const * fileName = (1 < argc) ? argv[1] : "in-demangle.txt";
	std::ifstream in(fileName);

	std::string line;
	while (std::getline(in, line)) {
		if (line.empty()) {
			std::cout << "=================================" << std::endl;
		} else if (line[0] == '#') {
			continue;
		} else {
			demangleAndPrint(line);
		}
	}
}
