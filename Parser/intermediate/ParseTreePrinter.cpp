/**
 * Parse tree printer class for a simple interpreter.
 *
 * (c) 2020 by Ronald Mak
 * Department of Computer Science
 * San Jose State University
 */


#include "Node.h"
#include "ParseTreePrinter.h"

namespace intermediate {

using namespace std;

const string ParseTreePrinter::INDENT_SIZE = "    ";

void ParseTreePrinter::print(Node *node, std::ofstream& outfile)
{
    // Opening tag.
    line += indentation;
    line += "<" + NODE_TYPE_STRINGS[(int) node->type];

    // Attributes.
    if      (node->type == PROGRAM)          line += " " + node->text;
    else if (node->type == VARIABLE)         line += " " + node->text;
    else if (node->type == INTEGER_CONSTANT) line += " " + to_string(node->value.L);
    else if (node->type == REAL_CONSTANT)    line += " " + to_string(node->value.D);
    else if (node->type == STRING_CONSTANT)  line += " '" + node->value.S + "'";
    if (node->lineNumber > 0)                line += " line " + to_string(node->lineNumber);

    // Print the node's children followed by the closing tag.
    vector<Node *> children = node->children;
    if (children.size() > 0)
    {
        line += ">";
        printLine(outfile);

        printChildren(children, outfile);
        line += indentation;
        line += "</" + NODE_TYPE_STRINGS[(int) node->type] + ">";
    }

    // No children: Close off the tag.
    else line += " />";

    printLine(outfile);
}

void ParseTreePrinter::printChildren(vector<Node *> children, std::ofstream& outfile)
{
    string saveIndentation = indentation;
    indentation += INDENT_SIZE;
    for (Node *child : children) print(child, outfile);
    indentation = saveIndentation;
}

void ParseTreePrinter::printLine(std::ofstream& outfile)
{
	
    outfile << line << endl;
    line = "";
}

}  // namespace intermediate

