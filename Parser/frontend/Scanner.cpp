#include <iostream>
#include <fstream>
#include <string>

#include "Scanner.h"

namespace frontend {
using namespace std;
    Token * Scanner::nextToken() {
        char ch = source->currentChar();

        // Skip blanks and other whitespace characters.
        while (isspace(ch)) {
            ch = source->nextChar();
        }

        if (isalpha(ch)) {
            return Token::identifier_token(ch, source);
        } else if (isdigit(ch)) {
            return Token::number_token(ch, source);
        } else if (ch == '\'') {
            return Token::string_token(ch, source);
        }
        return Token::symbol_token(ch, source);
    }
}

void outputSymbolTable(std::ofstream& outfile) {

	std::string infile = "test-in.txt";
	
    frontend::Scanner scanner(infile);
    frontend::Token::initializeTable();
	
	outfile << "\nOutput Symbol Table:\n" << endl;
    
    frontend::Token *t = scanner.nextToken();
    while (t->type != frontend::TokenType::END_OF_FILE) {
        outfile << *t;
        t = scanner.nextToken();
    }

    //outfile.close();
    return;
}