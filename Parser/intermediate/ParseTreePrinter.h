/**
 * Parse tree printer class for a simple interpreter.
 *
 * (c) 2020 by Ronald Mak
 * Department of Computer Science
 * San Jose State University
 */
#ifndef PARSETREEPRINTER_H_
#define PARSETREEPRINTER_H_

#include <string>
#include <vector>
#include <iostream>
#include <fstream>

#include "Node.h"

namespace intermediate {

using namespace std;

class ParseTreePrinter
{
private:
    static const string INDENT_SIZE;

    string indentation;  // indentation of a line
    string line;         // output line

public:
    ParseTreePrinter() : indentation(""), line("") {}

    /**
     * Print a parse tree.
     * @param node the parse tree's root node.
     */
    void print(Node *node, std::ofstream& outfile);

private:
    /**
     * Print a parse tree node's child nodes.
     * @param children the array list of child nodes.
     */
    void printChildren(vector<Node *> children, std::ofstream& outfile);

    /**
     * Print an output line.
     */
    void printLine(std::ofstream& outfile);
};

}  // namespace intermediate

#endif /* PARSETREEPRINTER_H_ */
