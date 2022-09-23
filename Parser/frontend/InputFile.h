#ifndef INPUTFILE_H_
#define INPUTFILE_H_

#include <iostream>
#include <fstream>
#include <string>

namespace frontend {

using namespace std;

class InputFile {
private:
    ifstream infile;
    char currentCh;

public:
    static const char EOL = '\n';

    InputFile(string filepath) {
        infile.open(filepath);

        if (infile.fail()) {
            cout << "ERROR: file open failed for " << filepath << endl;
            exit(-1);
        }
        currentCh = nextChar();
    }

    char currentChar() const { return currentCh; }

    char nextChar()
    {
        currentCh = infile.get();

        if (infile.eof()) {
            currentCh = EOF;
        } else if (infile.fail()) {
            exit(-1);
        }

        return currentCh;
    }
};

}

#endif