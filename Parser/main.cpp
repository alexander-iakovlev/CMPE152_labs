/**
 * A simple interpreter to demonstrate scanning, parsing, symbol tables,
 * parse trees, and interpreted program execution.
 *
 * (c) 2020 by Ronald Mak
 * Department of Computer Science
 * San Jose State University
 */
#include <string>
#include <iostream>
#include <fstream>

#include "frontend/Source.h"
#include "frontend/Scanner.cpp"
#include "frontend/Parser.cpp"
#include "frontend/Token.cpp"
#include "intermediate/ParseTreePrinter.cpp"
#include "backend/Executor.cpp"

using namespace std;
using namespace frontend;
using namespace intermediate;
using namespace backend;

void testScanner(Source *source);
void testParser(Scanner *scanner, Symtab *symtab, std::ofstream& outfile);
void executeProgram(Parser *parser, Symtab *symtab);

int main(int argc, char *argv[])
{
    /*if (argc != 3)
    {
        cout << "Usage: simple -{scan, parse, execute} sourceFileName" << endl;
        //exit(-1);
    }*/

    Token::initializeTable();
    Parser::initialize();
    //Executor::initialize();

    //string operation      = argv[1];
    //string sourceFileName = argv[1];
	
	std::string infile = "test-in.txt";
	std::string outputfile = "test-out.txt";

    std::ofstream outfile;
    outfile.open(outputfile);

    Source *source = new Source("test-in.txt"); //sourceFileName);
	
	testParser(new Scanner(source), new Symtab(), outfile);
	
	outputSymbolTable(outfile);
	
	outfile.close();

    /*if (operation == "-scan")
    {
        testScanner(source);
    }
    else if (operation == "-parse")
    {
        testParser(new Scanner(source), new Symtab());
    }
    else if (operation == "-execute")
    {
        Symtab *symtab = new Symtab();
        executeProgram(new Parser(new Scanner(source), symtab), symtab);
    }*/

    return 0;
}

/**
 * Test the scanner.
 * @param source the input source.
 */
void testScanner(Source *source)
{
    cout << "Tokens:" << endl << endl;

    Scanner *scanner = new Scanner(source);  // create the scanner

    // Loop to extract and print each token from the source one at a time.
    for (Token *token = scanner->nextToken();
         token->type != END_OF_FILE;
         token = scanner->nextToken())
    {
        printf("%12s : %s\n",
               TOKEN_STRINGS[(int) token->type].c_str(),
               token->text.c_str());
    }
}

/*void outputSymbolTable() {
	for (int i = 0; i < sizeof(TOKEN_STRINGS); i++) {
		cout << TOKEN_STRINGS[i] << endl;
	}
}*/

/**
 * Test the parser.
 * @param scanner the scanner.
 * @param symtab the symbol table.
 */
void testParser(Scanner *scanner, Symtab *symtab, std::ofstream& outfile)
{
    Parser *parser = new Parser(scanner, symtab);  // create the parser
    Node *programNode = parser->parseProgram();    // and parse the program
    int errorCount = parser->getErrorCount();

    if (errorCount == 0)
    {
        outfile << "Parse tree:" << endl << endl;

        ParseTreePrinter *printer = new ParseTreePrinter();
        printer->print(programNode, outfile);
    }
    else
    {
        cout << endl << "There were " << errorCount << " errors." << endl;
    }
}

/**
 * Test the executor.
 * @param parser the parser.
 * @param symtab the symbol table.
 */
void executeProgram(Parser *parser, Symtab *symtab)
{
    Node *programNode = parser->parseProgram();
    int errorCount = parser->getErrorCount();

    if (errorCount == 0)
    {
        Executor *executor = new Executor(symtab);
        executor->visit(programNode);
    }
    else
    {
        cout << endl << "There were " << errorCount << " errors." << endl;
    }
}
