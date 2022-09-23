/**
 * Parse tree node class for a simple interpreter.
 *
 * (c) 2020 by Ronald Mak
 * Department of Computer Science
 * San Jose State University
 */
#ifndef NODE_H_
#define NODE_H_

#include <string>
#include <vector>

#include "../frontend/Object.h"
#include "SymtabEntry.h"

namespace intermediate {

using namespace std;

enum class NodeType
{ // SHUFFLE ME
  PROGRAM, COMPOUND, ASSIGN, LOOP, TEST, IF, WRITE, WRITELN,
  SWITCH, SELECT_BRANCH, SELECT_CONSTANTS,
  ADD, SUBTRACT, MULTIPLY, DIVIDE, INTEGER_DIVIDE, MODULO, NEGATE,
  AND, OR, EQ, NE, LT, LE, GT, GE, NOT,
  VARIABLE, INTEGER_CONSTANT, REAL_CONSTANT, STRING_CONSTANT, FOR, CASE
};

static const string NODE_TYPE_STRINGS[] =
{
    "PROGRAM", "COMPOUND", "ASSIGN", "LOOP", "TEST", "WRITE", "WRITELN", "SWITCH",
    "SELECT_BRANCH", "SELECT_CONSTANTS", "INTEGER_DIVIDE", "MODULO", "NEGATE", "AND",
    "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "EQ", "LT", "OR", "EQ", "NE", "LT", "LE", "GT", "GE", "NOT",
    "VARIABLE", "INTEGER_CONSTANT", "REAL_CONSTANT", "STRING_CONSTANT", "WHILE", "IF", "FOR", "CASE"
};

constexpr NodeType PROGRAM          = NodeType::PROGRAM;
constexpr NodeType COMPOUND         = NodeType::COMPOUND;
constexpr NodeType ASSIGN           = NodeType::ASSIGN;
constexpr NodeType LOOP             = NodeType::LOOP;
constexpr NodeType TEST             = NodeType::TEST;
constexpr NodeType WRITE            = NodeType::WRITE;
constexpr NodeType WRITELN          = NodeType::WRITELN;
constexpr NodeType ADD              = NodeType::ADD;
constexpr NodeType SUBTRACT         = NodeType::SUBTRACT;
constexpr NodeType MULTIPLY         = NodeType::MULTIPLY;
constexpr NodeType DIVIDE           = NodeType::DIVIDE;
constexpr NodeType EQ               = NodeType::EQ;
constexpr NodeType VARIABLE         = NodeType::VARIABLE;
constexpr NodeType INTEGER_CONSTANT = NodeType::INTEGER_CONSTANT;
constexpr NodeType INTEGER_DIVIDE   = NodeType::INTEGER_DIVIDE;
constexpr NodeType REAL_CONSTANT    = NodeType::REAL_CONSTANT;
constexpr NodeType STRING_CONSTANT  = NodeType::STRING_CONSTANT;
constexpr NodeType SELECT_BRANCH    = NodeType::SELECT_BRANCH;
constexpr NodeType SELECT_CONSTANTS = NodeType::SELECT_CONSTANTS;
constexpr NodeType MODULO           = NodeType::MODULO;
constexpr NodeType NEGATE           = NodeType::NEGATE;
constexpr NodeType AND              = NodeType::AND;
constexpr NodeType OR               = NodeType::OR;
constexpr NodeType NE               = NodeType::NE;
constexpr NodeType LT               = NodeType::LT;
constexpr NodeType LE               = NodeType::LE;
constexpr NodeType GT               = NodeType::GT;
constexpr NodeType GE               = NodeType::GE;
constexpr NodeType NOT              = NodeType::NOT;
constexpr NodeType IF               = NodeType::IF;
constexpr NodeType FOR              = NodeType::FOR;
constexpr NodeType CASE             = NodeType::CASE;
constexpr NodeType SWITCH           = NodeType::SWITCH;

class Node
{
public:
    NodeType type;
    int lineNumber;
    string text;
    SymtabEntry *entry;
    Object value;
    vector<Node *> children;

    /**
     * Constructor
     * @param type node type.
     */
    Node(NodeType type)
        : type(type), lineNumber(0), entry(nullptr) {}

    /**
     * Adopt a child node.
     * @param child the child node.
     */
    void adopt(Node *child) { children.push_back(child); }

// create a new node, and copy the contents over
    Node *copy()
{
    Node *copyNode = new Node(type);
    copyNode->lineNumber = lineNumber;
    copyNode->entry = entry;
    copyNode->value = value;
    copyNode->text = text;

    return copyNode;
}
};

}  // namespace intermediate

#endif /* NODE_H_ */
