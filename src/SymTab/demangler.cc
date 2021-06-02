#include "Mangler.h"
#include <iostream>
#include <fstream>
using namespace std;

void f(const std::string & mangleName) {
	char * demangleName = cforall_demangle(mangleName.c_str(), 0);
	cout << mangleName << " => " << std::flush << demangleName << endl;
	free(demangleName);
}

int main() {
	ifstream in("in-demangle.txt");
	std::string line;
	while (getline(in, line)) {
		if (line.empty()) { cout << "=================================" << endl; continue; }
		else if (line[0] == '#') continue;
		f(line);
	}
}
