#ifndef TOKEN_H_
#define TOKEN_H_

#include <string>
#include <map>

#include "Object.h"
#include "InputFile.h"
#include "Source.h"

namespace frontend {

using namespace std;

enum TokenType
{
    AND = 0, ARRAY, ASM,
    BEGIN, BREAK,
    CASE, CONST, CONSTRUCTOR, CONTINUE,
    DESTRUCTOR, DIV, DO, DOWNTO,
    ELSE, END,
    FALSE, FILE, FOR, FUNCTION, GOTO,
    IF, IMPLEMENTATION, IN, INLINE, INTERFACE,
    LABEL,MOD, NIL, NOT,
    OBJECT, OF, ON, OPERATOR, OR,
    PACKED, PROCEDURE, PROGRAM,
    RECORD, REPEAT,
    SET, SHL, SHR, STRING,
    THEN, TO, TRUE, TYPE,
    UNIT, UNTIL, USES,
    VAR, WHILE, WITH, WRITE, WRITELN, XOR,
    END_OF_FILE, ERROR,
    INTEGER,
    REAL,
    IDENTIFIER,
    PLUS_OP, MINUS_OP, MULT_OP, DIV_OP, 
    COLON_EQUALS, EQUALS, NOT_EQUALS,
    LESS_EQUALS, GREATER_EQUALS,
    LESS_THAN, GREATER_THAN,
    PLUS_EQUALS, MINUS_EQUALS, MULT_EQUALS, DIV_EQUALS,
    CARAT, SEMICOLON, COLON, COMMA,
    LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE,
    LCOMMENT, RCOMMENT,
    PERIOD, PERIOD_PERIOD, CHAR
};

static const string TOKEN_STRINGS[] = {
    "AND", "ARRAY", "ASM", "BEGIN", "BREAK", "CASE", "CONST", "CONSTRUCTOR", "CONTINUE",
    "DESTRUCTOR", "DIV", "DO", "DOWNTO", "ELSE", "END", "FALSE", "FILE", "FOR", "FUNCTION", "GOTO",
    "IF", "IMPLEMENTATION", "IN", "INLINE", "INTERFACE", "LABEL","MOD", "NIL", "NOT",
    "OBJECT", "OF", "ON", "OPERATOR", "OR", "PACKED", "PROCEDURE", "PROGRAM",
    "RECORD", "REPEAT", "SET", "SHL", "SHR", "STRING", "THEN", "TO", "TRUE", "TYPE",
    "UNIT", "UNTIL", "USES", "VAR", "WHILE", "WITH", "WRITE", "WRITELN", "XOR",
    "END_OF_FILE", "ERROR", "INTEGER", "REAL", "IDENTIFIER",
    "PLUS_OP", "MINUS_OP", "MULT_OP", "DIV_OP", "COLON_EQUALS", "EQUALS", "NOT_EQUALS",
    "LESS_EQUALS", "GREATER_EQUALS", "LESS_THAN", "GREATER_THAN",
    "PLUS_EQUALS", "MINUS_EQUALS", "MULT_EQUALS", "DIV_EQUALS", "CARAT", "SEMICOLON", "COLON", "COMMA",
    "LPAREN", "RPAREN", "LBRACKET", "RBRACKET", "LBRACE", "RBRACE", "LCOMMENT", "RCOMMENT", "PERIOD", "PERIOD_PERIOD", "CHAR"
};

class Token {
private:
    static map<string, TokenType> transitionTable;

public:
    TokenType type;
    int lineNumber;  // source line number of the token
    string text;
    Object value;    // the value (if any) of the token

    static void initializeTable();

    Token(char firstChar) : type(ERROR), lineNumber(0), text("")
    {
        text += firstChar;
    }

    static Token *identifier_token(char firstChar, Source *source);

    static Token *number_token(char firstChar, Source *source);

    static Token *string_token(char firstChar, Source *source);

    static Token *symbol_token(char firstChar, Source *source);

    static void error(Token *token, string message);

    friend ostream& operator<<(ostream& os, const Token& token);
};

}  // namespace frontend

#endif /* TOKEN_H_ */