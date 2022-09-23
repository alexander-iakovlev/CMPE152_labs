/**
 * Scanner class.
 */
#ifndef SCANNER_H_
#define SCANNER_H_

#include <string>

#include "InputFile.h"
#include "Token.h"
#include "Source.h"

namespace frontend {

using namespace std;

class Scanner {
private:
	Source *source;
    //InputFile *source;

public:
	Scanner(Source *source) : source(source) {}
    //Scanner(Source sourceFile) { //std::string sourceFileName
        //source = sourceFile;//new InputFile(sourceFileName);
    //}
	Scanner(std::string sourceFileName) {
		source = new Source(sourceFileName);
	}

    Token * nextToken();
};

}

#endif