/**
 * Parser class for a simple interpreter.
 *
 * (c) 2020 by Ronald Mak
 * Department of Computer Engineering
 * San Jose State University
 */
#include <string>
#include <map>

#include "Token.h"
#include "Parser.h"
#include "../intermediate/Symtab.h"
#include "../intermediate/Node.h"

namespace frontend {

using namespace std;

set<TokenType> Parser::statementStarters;
set<TokenType> Parser::statementFollowers;
set<TokenType> Parser::relationalOperators;
set<TokenType> Parser::simpleExpressionOperators;
set<TokenType> Parser::termOperators;

void Parser::initialize()
{
    // Tokens that can start a statement.
    statementStarters.insert(BEGIN);
    statementStarters.insert(IDENTIFIER);
    statementStarters.insert(REPEAT);
    statementStarters.insert(WHILE);
    statementStarters.insert(FOR);
    statementStarters.insert(TokenType::IF);
    statementStarters.insert(CASE);
    statementStarters.insert(TokenType::WRITE);
    statementStarters.insert(TokenType::WRITELN);

    // Tokens that can immediately follow a statement.
    statementFollowers.insert(SEMICOLON);
    statementFollowers.insert(END);
    statementFollowers.insert(UNTIL);
    statementFollowers.insert(END_OF_FILE);

    relationalOperators.insert(EQUALS);
    relationalOperators.insert(NOT_EQUALS);
    relationalOperators.insert(LESS_THAN);
    relationalOperators.insert(LESS_EQUALS);
    relationalOperators.insert(GREATER_THAN);
    relationalOperators.insert(GREATER_EQUALS);

    simpleExpressionOperators.insert(PLUS_OP);
    simpleExpressionOperators.insert(MINUS_OP);
    simpleExpressionOperators.insert(OR);

    termOperators.insert(MULT_OP);
    termOperators.insert(DIV_OP);
    termOperators.insert(MOD);
    termOperators.insert(TokenType::AND);
}

Node *Parser::parseProgram()
{
    Node *programNode = new Node(NodeType::PROGRAM);

    currentToken = scanner->nextToken();  // first token!

    if (currentToken->type == TokenType::PROGRAM)
    {
        currentToken = scanner->nextToken();  // consume PROGRAM
    }
    else syntaxError("Expecting PROGRAM");

    if (currentToken->type == IDENTIFIER)
    {
        string programName = currentToken->text;
        symtab->enter(programName);
        programNode->text = programName;

        currentToken = scanner->nextToken();  // consume program name
    }
    else syntaxError("Expecting program name");

    if (currentToken->type == SEMICOLON)
    {
        currentToken = scanner->nextToken();  // consume ;
    }
    else syntaxError("Missing ;");

    if (currentToken->type != BEGIN) syntaxError("Expecting BEGIN");

    // The PROGRAM node adopts the COMPOUND tree.
    programNode->adopt(parseCompoundStatement());

    if (currentToken->type == SEMICOLON) syntaxError("Expecting .");
    return programNode;
}

Node *Parser::parseStatement()
{
    Node *stmtNode = nullptr;
    int savedLineNumber = currentToken->lineNumber;
    lineNumber = savedLineNumber;

    switch (currentToken->type)
    {
        case IDENTIFIER : stmtNode = parseAssignmentStatement(); break;
        case BEGIN :      stmtNode = parseCompoundStatement();   break;
        case REPEAT :     stmtNode = parseRepeatStatement();     break;
        case WRITE :      stmtNode = parseWriteStatement();      break;
        case WRITELN :    stmtNode = parseWritelnStatement();    break;
        case WHILE :      stmtNode = whileStatement();           break;
        case FOR :        stmtNode = forStatement();             break;
        case IF :         stmtNode = ifStatement();              break;
        case CASE :       stmtNode = caseStatement();            break;
        case SEMICOLON :  stmtNode = nullptr; break;  // empty statement

        default : syntaxError("Unexpected token");
    }

    if (stmtNode != nullptr) stmtNode->lineNumber = savedLineNumber;
    return stmtNode;
}

Node *Parser::parseAssignmentStatement()
{
    // The current token should now be the left-hand-side variable name.

    Node *assignmentNode = new Node(ASSIGN);

    // Enter the variable name into the symbol table
    // if it isn't already in there.
    string variableName = currentToken->text;
    SymtabEntry *variableId = symtab->lookup(toLowerCase(variableName));
    if (variableId == nullptr) variableId = symtab->enter(variableName);

    // The assignment node adopts the variable node as its first child.
    Node *lhsNode  = new Node(VARIABLE);
    lhsNode->text  = variableName;
    lhsNode->entry = variableId;
    assignmentNode->adopt(lhsNode);

    currentToken = scanner->nextToken();  // consume the LHS variable;

    if (currentToken->type == COLON_EQUALS)
    {
        currentToken = scanner->nextToken();  // consume :=
    }
    else syntaxError("Missing :=");

    // The assignment node adopts the expression node as its second child.
    Node *rhsNode = parseExpression();
    assignmentNode->adopt(rhsNode);

    return assignmentNode;
}

Node *Parser::parseCompoundStatement()
{
    Node *compoundNode = new Node(COMPOUND);
    compoundNode->lineNumber = currentToken->lineNumber;

    currentToken = scanner->nextToken();  // consume BEGIN
    parseStatementList(compoundNode, END);

    if (currentToken->type == END)
    {
        currentToken = scanner->nextToken();  // consume END
    }
    else syntaxError("Expecting END");

    return compoundNode;
}

void Parser::parseStatementList(Node *parentNode, TokenType terminalType)
{
    while (   (currentToken->type != terminalType)
           && (currentToken->type != END_OF_FILE))
    {
        Node *stmtNode = parseStatement();
        if (stmtNode != nullptr) parentNode->adopt(stmtNode);

        // A semicolon separates statements.
        if (currentToken->type == SEMICOLON)
        {
            while (currentToken->type == SEMICOLON)
            {
                currentToken = scanner->nextToken();  // consume ;
            }
        }
        else if (statementStarters.find(currentToken->type) !=
                                                        statementStarters.end())
        {
            syntaxError("Missing ;");
        }
    }
}

Node *Parser::parseRepeatStatement()
{
    // The current token should now be REPEAT.

    // Create a LOOP node.
    Node *loopNode = new Node(LOOP);
    currentToken = scanner->nextToken();  // consume REPEAT

    parseStatementList(loopNode, UNTIL);

    if (currentToken->type == UNTIL)
    {
        // Create a TEST node. It adopts the test expression node.
        Node *testNode = new Node(TEST);
        lineNumber = currentToken->lineNumber;
        testNode->lineNumber = lineNumber;
        currentToken = scanner->nextToken();  // consume UNTIL

        testNode->adopt(parseExpression());

        // The LOOP node adopts the TEST node as its final child.
        loopNode->adopt(testNode);
    }
    else syntaxError("Expecting UNTIL");

    return loopNode;
}

Node *Parser::whileStatement()
{
    // The current token should now be WHILE.
    //according to hw doc, this is similar to repeat loop
    // while loop looks like while (condition) do S; where s is a statement

    // Create a LOOP node, cause this needs to loop until a condition is met.
    Node *loopNode = new Node(LOOP);
    currentToken = scanner->nextToken();  // consume the while, leaving us with (condition) do S
    Node *testNode = new Node(TEST);
    Node *notNode  = new Node(NodeType::NOT);
    // loop adopts the test, test adopts the not because this will loop until it is NOT true
    loopNode->adopt(testNode);
    testNode->adopt(notNode);
    notNode->adopt(parseExpression()); // NOT node adopts the conditin, leaving us with do S

    // catch the DO and consume it, or throw an error
    if (currentToken->type != DO) syntaxError("Expecting DO");
    else
    {
        currentToken = scanner->nextToken();  // consume DO
    }
    // all that should be left is the statement, so the loop adopts it
    loopNode->adopt(parseStatement());
    return loopNode;
}

Node *Parser::forStatement()
{
    // For loops look like for i:= x to y do statement;
    Node *compoundNode = new Node(COMPOUND);
    currentToken = scanner->nextToken();  // consume the FOR, leaving us with i:= x to y do statement;
    Node *assignNode = parseAssignmentStatement();
    compoundNode->adopt(assignNode); // compound node needs to hold on to a control value to compare to the current value
    Node *controlNode = assignNode->children[0]; // child 0 is the first child, or the left tree, should be what value i is assigneed to
    Node *loopNode = new Node(LOOP); // here the compound adopts the loop because a FOR requires looping until a condition is met
    compoundNode->adopt(loopNode);
    Node *testNode = new Node(TEST); // a test node is used to check the condition and determine if we need to loop or not.
    loopNode->adopt(testNode);
    bool upLoop = true; //  we need to check if the for loop is counting up or down, flag here
    if (currentToken->type == TO)
    {
        currentToken = scanner->nextToken();  // consume TO if we count up
    }
    else if (currentToken->type == DOWNTO)
    {
        upLoop = false;
        currentToken = scanner->nextToken();  // consume DOWNTO if we count down
    }
    else syntaxError("Expecting TO or DOWNTO after variable"); // throw an error if conditions are missed
    Node *compareNode = new Node(LT);  //depending on the flag, will we need to check if its less than or greater than?
    if (upLoop) {
     *compareNode = Node(GT);
    }
    testNode->adopt(compareNode);
    compareNode->adopt(controlNode->copy()); // comparenode will copy the value of the control value i
    compareNode->adopt(parseExpression());  // consume the xpression
    if (currentToken->type == DO)
    {
        currentToken = scanner->nextToken();  // consume DO, levaing us with the statement
    }
    else syntaxError("Expecting DO");
    loopNode->adopt(parseStatement()); // loop node adopts the statement that will be looped
    assignNode = new Node(ASSIGN);
    loopNode->adopt(assignNode);
    // loop node needs to increment the variable, otherwise we are in an infinite loop
    assignNode->adopt(controlNode->copy());
    Node *operatorNode = upLoop ? new Node(ADD) : new Node(SUBTRACT); // if we count up we add, otherwise we count down
    assignNode->adopt(operatorNode);
    operatorNode->adopt(controlNode->copy()); // make sure the control node knows whats going on, adopt the control variable to compare
    Node *oneNode = new Node(INTEGER_CONSTANT);
    // increment or decrement depending on what we need to do
    oneNode->value.L = 1;
    oneNode->value.D = 1;
    operatorNode->adopt(oneNode);

    return compoundNode;
}

Node *Parser::ifStatement()
{
    // An if statement looks like if condition then S1 else S2;
    // Create an IF node.
    Node *ifNode = new Node(NodeType::IF);
    currentToken = scanner->nextToken();  // consume IF, leaving us with condition then S1 else S2;
    ifNode->adopt(parseExpression()); // the node adopts and consumes the first condition, leaving us with then s1 else s2;
    if (currentToken->type != THEN) syntaxError("Missing THEN");
    else { currentToken = scanner->nextToken(); } //consume the THEN leaving us with S1 else S2; if there is an else
    ifNode->adopt(parseStatement()); // adopt the S1 as the statement

    // now we must check if there is an ELSE token, which implies there is another statement
    if (currentToken->type == ELSE)
    {
        currentToken = scanner->nextToken();  // consume ELSE
        ifNode->adopt(parseStatement()); // adopt S2
    }

    return ifNode;
}

Node *Parser::caseStatement()
{
    // Create a SWITCH node, since we have to compare and switch accordingly
    Node *switchNode = new Node(SWITCH);
    currentToken = scanner->nextToken();  // consume CASE, the first word
    Node *exprNode = parseExpression();
    switchNode->adopt(exprNode); // adopt the expression tree for the caseStatement
    if (currentToken->type == OF) // check if we have an OF
    {
        currentToken = scanner->nextToken();  // consume OF
    }
    else syntaxError("Expecting OF");

    // this is where it gets funky we gotta check each branchNode
    while (   (currentToken->type == INTEGER)||(currentToken->type == PLUS_OP)||(currentToken->type == MINUS_OP))
    {
        Node *branchNode = new Node(SELECT_BRANCH); // its a branch node for each seperate case
        Node *constantsNode = new Node(SELECT_CONSTANTS); // the constants are the values that we are checking to assign the case
        switchNode->adopt(branchNode);
        branchNode->adopt(constantsNode);

        // looping parse until we hit a : ,  which signifies the end of the case statement
        // statments can have a + or a - (negate)
        // SELECT_CONSTANTS node will adopts each INTEGER_CONSTANT for the comparision.
        do
        {
            bool negate = false;
            if ((currentToken->type == PLUS_OP) || (currentToken->type == MINUS_OP))
            {
                negate = currentToken->type == MINUS_OP;
                currentToken = scanner->nextToken();  // consume whichever symbol
            }

            Node *constantNode = parseIntegerConstant();
            if (negate) constantNode->value = -(constantNode->value.L);

            constantsNode->adopt(constantNode);

            if (currentToken->type == COMMA) { currentToken = scanner->nextToken(); } // consume the comma
        }
         while (currentToken->type != COLON);
        currentToken = scanner->nextToken();  // consume :

        // have the branch adopt the new case
        branchNode->adopt(parseStatement());

        // Consume semicolons and loop again
        if (currentToken->type == SEMICOLON)
        {
            do
            {
                currentToken = scanner->nextToken();  // consume ;
            } while (currentToken->type == SEMICOLON);
        }
    }

    // if we made it here there are no more cases, this should be the end
    if (currentToken->type == END) { currentToken = scanner->nextToken();} //consume the end

    else if (statementStarters.find(currentToken->type) != statementStarters.end())
    {
        syntaxError("Missing END");
    }

    return switchNode;
}

Node *Parser::parseWriteStatement()
{
    // The current token should now be WRITE.

    // Create a WRITE node-> It adopts the variable or string node.
    Node *writeNode = new Node(NodeType::WRITE);
    currentToken = scanner->nextToken();  // consume WRITE

    parseWriteArguments(writeNode);
    if (writeNode->children.size() == 0)
    {
        syntaxError("Invalid WRITE statement");
    }

    return writeNode;
}

Node *Parser::parseWritelnStatement()
{
    // The current token should now be WRITELN.

    // Create a WRITELN node. It adopts the variable or string node.
    Node *writelnNode = new Node(NodeType::WRITELN);
    currentToken = scanner->nextToken();  // consume WRITELN

    if (currentToken->type == LPAREN) parseWriteArguments(writelnNode);
    return writelnNode;
}

void Parser::parseWriteArguments(Node *node)
{
    // The current token should now be (

    bool hasArgument = false;

    if (currentToken->type == LPAREN)
    {
        currentToken = scanner->nextToken();  // consume (
    }
    else syntaxError("Missing left parenthesis");

    if (currentToken->type == IDENTIFIER)
    {
        node->adopt(parseVariable());
        hasArgument = true;
    }
    else if (   (currentToken->type == CHAR)
             || (currentToken->type == STRING))
    {
        node->adopt(parseStringConstant());
        hasArgument = true;
    }
    else syntaxError("Invalid WRITE or WRITELN statement");

    // Look for a field width and a count of decimal places.
    if (hasArgument)
    {
        if (currentToken->type == COLON)
        {
            currentToken = scanner->nextToken();  // consume ,

            if (currentToken->type == INTEGER)
            {
                // Field width
                node->adopt(parseIntegerConstant());

                if (currentToken->type == COLON)
                {
                    currentToken = scanner->nextToken();  // consume ,

                    if (currentToken->type == INTEGER)
                    {
                        // Count of decimal places
                        node->adopt(parseIntegerConstant());
                    }
                    else syntaxError("Invalid count of decimal places");
                }
            }
            else syntaxError("Invalid field width");
        }
    }

    if (currentToken->type == RPAREN)
    {
        currentToken = scanner->nextToken();  // consume )
    }
    else syntaxError("Missing right parenthesis");
}

Node *Parser::parseExpression()
{
    // The current token should now be an identifier or a number
    // or + or -

    // The expression's root node->
    Node *exprNode = parseSimpleExpression();

    // The current token might now be a relational operator.
    if (relationalOperators.find(currentToken->type) != relationalOperators.end())
    {
        TokenType tokenType = currentToken->type;
        Node *opNode = nullptr;

        switch (tokenType)
        {
            case EQUALS :         opNode = new Node(EQ); break;
            case NOT_EQUALS :     opNode = new Node(NE); break;
            case LESS_THAN :      opNode = new Node(LT); break;
            case LESS_EQUALS :    opNode = new Node(LE); break;
            case GREATER_THAN :   opNode = new Node(GT); break;
            case GREATER_EQUALS : opNode = new Node(GE); break;

            default : syntaxError("Unexpected token");
        }

        currentToken = scanner->nextToken();  // consume relational operator

        // The relational operator node adopts the first simple expression
        // node as its first child and the second simple expression node
        // as its second child. Then it becomes the expression's root node.
        if (opNode != nullptr)
        {
            opNode->adopt(exprNode);
            opNode->adopt(parseSimpleExpression());
            exprNode = opNode;
        }
    }

    return exprNode;
}

Node *Parser::parseSimpleExpression()
{
    // The current token should now be an identifier or a number
    // or + or -

    // The simple expression's root node->
    Node *simpExprNode = parseTerm();

    // Keep parsing more terms as long as the current token
    // is a + or - operator.
    while (simpleExpressionOperators.find(currentToken->type) !=
                                                simpleExpressionOperators.end())
    {
        Node *opNode = nullptr;

        switch (currentToken->type)
        {
            case PLUS_OP :  opNode = new Node(ADD);          break;
            case MINUS_OP : opNode = new Node(SUBTRACT);     break;
            case OR :    opNode = new Node(NodeType::OR); break;

            default : syntaxError("Unexpected token");
        }

        currentToken = scanner->nextToken();  // consume the operator

        // The add or subtract node adopts the first term node as its
        // first child and the next term node as its second child.
        // Then it becomes the simple expression's root node.
        opNode->adopt(simpExprNode);
        opNode->adopt(parseTerm());
        simpExprNode = opNode;
    }

    return simpExprNode;
}

Node *Parser::parseTerm()
{
    // The current token should now be an identifier or a number
    // or - or +

    // The term's root node.
    Node *termNode = nullptr;

    if (currentToken->type == PLUS_OP)
    {
        currentToken = scanner->nextToken();  // consume +
        termNode = parseFactor();
    }
    else if (currentToken->type == MINUS_OP)
    {
        currentToken = scanner->nextToken();  // consume -
        termNode = new Node(NEGATE);
        termNode->adopt(parseFactor());
    }
    else termNode = parseFactor();

    // Keep parsing more factors as long as the current token
    // is a * or / operator.
    while (termOperators.find(currentToken->type) != termOperators.end())
    {
        Node *opNode = nullptr;

        switch (currentToken->type)
        {
            case MULT_OP  : opNode = new Node(MULTIPLY);          break;
            case DIV_OP   : opNode = new Node(INTEGER_DIVIDE);    break;
            case MOD   : opNode = new Node(MODULO);            break;
            case AND   : opNode = new Node(NodeType::AND); break;

            default : syntaxError("Unexpected token");
        }

        currentToken = scanner->nextToken();  // consume the operator

        // The multiply or divide node adopts the first factor node as its
        // as its first child and the next factor node as its second child.
        // Then it becomes the term's root node.
        opNode->adopt(termNode);
        opNode->adopt(parseFactor());
        termNode = opNode;
    }

    return termNode;
}

Node *Parser::parseFactor()
{
    // The current token should now be an identifier or a number or (

    if      (currentToken->type == IDENTIFIER) return parseVariable();
    else if (currentToken->type == INTEGER)    return parseIntegerConstant();
    else if (currentToken->type == REAL)       return parseRealConstant();

    else if (currentToken->type == LPAREN)
    {
        currentToken = scanner->nextToken();  // consume (
        Node *exprNode = parseExpression();

        if (currentToken->type == RPAREN)
        {
            currentToken = scanner->nextToken();  // consume )
        }
        else syntaxError("Expecting )");

        return exprNode;
    }

    else if (currentToken->type == TokenType::NOT)
    {
        Node *notNode = new Node(NodeType::NOT);
        currentToken = scanner->nextToken();  // consume NOT

        notNode->adopt(parseFactor());
        return notNode;
    }

    else syntaxError("Unexpected token");
    return nullptr;
}

Node *Parser::parseVariable()
{
    // The current token should now be an identifier.

    // Has the variable been "declared"?
    string variableName = currentToken->text;
    SymtabEntry *variableId = symtab->lookup(toLowerCase(variableName));
    if (variableId == nullptr) semanticError("Undeclared identifier");

    Node *node  = new Node(VARIABLE);
    node->text  = variableName;
    node->entry = variableId;

    currentToken = scanner->nextToken();  // consume the identifier
    return node;
}

Node *Parser::parseIntegerConstant()
{
    // The current token should now be a number.

    Node *integerNode = new Node(INTEGER_CONSTANT);
    //integerNode->value = currentToken->value;

    currentToken = scanner->nextToken();  // consume the number
    return integerNode;
}

Node *Parser::parseRealConstant()
{
    // The current token should now be a number.

    Node *realNode = new Node(REAL_CONSTANT);
    //realNode->value = currentToken->value;

    currentToken = scanner->nextToken();  // consume the number
    return realNode;
}

Node *Parser::parseStringConstant()
{
    // The current token should now be string.

    Node *stringNode = new Node(STRING_CONSTANT);
    //stringNode->value = currentToken->value;

    currentToken = scanner->nextToken();  // consume the string
    return stringNode;
}

void Parser::syntaxError(string message)
{
    printf("SYNTAX ERROR at line %d: %s at '%s'\n",
           lineNumber, message.c_str(), currentToken->text.c_str());
    errorCount++;

    // Recover by skipping the rest of the statement.
    // Skip to a statement follower token.
    while (statementFollowers.find(currentToken->type) ==
                                                    statementFollowers.end())
    {
        currentToken = scanner->nextToken();
    }
}

void Parser::semanticError(string message)
{
    printf("SEMANTIC ERROR at line %d: %s at '%s'\n",
           lineNumber, message.c_str(), currentToken->text.c_str());
    errorCount++;
}

}  // namespace frontend
