#include <string>
#include <ctype.h>
#include <algorithm>
#include <map>

#include "Token.h"

namespace frontend {

using namespace std;

inline string toUpperCase(string str)
{
    string upper_case(str);
    transform(upper_case.begin(), upper_case.end(),
              upper_case.begin(), ::toupper);
    return upper_case;
}

map<string, TokenType> Token::transitionTable;

void Token::initializeTable() {
    transitionTable["AND"] = TokenType::AND;
    transitionTable["ARRAY"] = TokenType::ARRAY;
    transitionTable["ASM"] = TokenType::ASM;
    transitionTable["BEGIN"] = TokenType::BEGIN;
    transitionTable["BREAK"] = TokenType::BREAK;
    transitionTable["CASE"] = TokenType::CASE;
    transitionTable["CONST"] = TokenType::CONST;
    transitionTable["CONSTRUCTOR"] = TokenType::CONSTRUCTOR;
    transitionTable["CONTINUE"] = TokenType::CONTINUE;
    transitionTable["DESTRUCTOR"] = TokenType::DESTRUCTOR;
    transitionTable["DIV"] = TokenType::DIV;
    transitionTable["DO"] = TokenType::DO;
    transitionTable["DOWNTO"] = TokenType::DOWNTO;
    transitionTable["ELSE"] = TokenType::ELSE;
    transitionTable["END"] = TokenType::END;
    transitionTable["FALSE"] = TokenType::FALSE;
    transitionTable["FILE"] = TokenType::FILE;
    transitionTable["FOR"] = TokenType::FOR;
    transitionTable["FUNCTION"] = TokenType::FUNCTION;
    transitionTable["GOTO"] = TokenType::GOTO;
    transitionTable["IF"] = TokenType::IF;
    transitionTable["IMPLEMENTATION"] = TokenType::IMPLEMENTATION;
    transitionTable["IN"] = TokenType::IN;
    transitionTable["INLINE"] = TokenType::INLINE;
    transitionTable["INTERFACE"] = TokenType::INTERFACE;
    transitionTable["LABEL"] = TokenType::LABEL;
    transitionTable["MOD"] = TokenType::MOD;
    transitionTable["NIL"] = TokenType::NIL;
    transitionTable["NOT"] = TokenType::NOT;
    transitionTable["OBJECT"] = TokenType::OBJECT;
    transitionTable["OF"] = TokenType::OF;
    transitionTable["ON"] = TokenType::ON;
    transitionTable["OPERATOR"] = TokenType::OPERATOR;
    transitionTable["OR"] = TokenType::OR;
    transitionTable["PACKED"] = TokenType::PACKED;
    transitionTable["PROCEDURE"] = TokenType::PROCEDURE;
    transitionTable["PROGRAM"] = TokenType::PROGRAM;
    transitionTable["RECORD"] = TokenType::RECORD;
    transitionTable["REPEAT"] = TokenType::REPEAT;
    transitionTable["SET"] = TokenType::SET;
    transitionTable["SHL"] = TokenType::SHL;
    transitionTable["SHR"] = TokenType::SHR;
    transitionTable["STRING"] = TokenType::STRING;
    transitionTable["THEN"] = TokenType::THEN;
    transitionTable["TO"] = TokenType::TO;
    transitionTable["TRUE"] = TokenType::TRUE;
    transitionTable["TYPE"] = TokenType::TYPE;
    transitionTable["UNIT"] = TokenType::UNIT;
    transitionTable["UNTIL"] = TokenType::UNTIL;
    transitionTable["USED"] = TokenType::USES;
    transitionTable["VAR"] = TokenType::VAR;
    transitionTable["WHILE"] = TokenType::WHILE;
    transitionTable["WITH"] = TokenType::WITH;
	transitionTable["WRITE"] = TokenType::WRITE;
	transitionTable["WRITELN"] = TokenType::WRITELN;
    transitionTable["XOR"] = TokenType::XOR;
    // transitionTable[""] = INTEGER
    // transitionTable[""] = REAL
    // transitionTable[""] = IDENTIFIER
    transitionTable["+"] = TokenType::PLUS_OP;
    transitionTable["+="] = TokenType::PLUS_EQUALS; 
    transitionTable["-"] = TokenType::MINUS_OP;
    transitionTable["-="] = TokenType::MINUS_EQUALS;
    transitionTable["*"] = TokenType::MULT_OP;
    transitionTable["*="] = TokenType::MULT_EQUALS;
    transitionTable["/"] = TokenType::DIV_OP;
    transitionTable["/="] = TokenType::DIV_EQUALS;
    transitionTable[":"] = TokenType::COLON;
    transitionTable[":="] = TokenType::COLON_EQUALS; 
    transitionTable["="] = TokenType::EQUALS;
    transitionTable["<>"] = TokenType::NOT_EQUALS;
    transitionTable["<="] = TokenType::LESS_EQUALS;
    transitionTable[">="] = TokenType::GREATER_EQUALS;
    transitionTable["<"] = TokenType::LESS_THAN; 
    transitionTable[">"] = TokenType::GREATER_THAN;
    transitionTable["^"] = TokenType::CARAT; 
    transitionTable[";"] = TokenType::SEMICOLON; 
    transitionTable[","] = TokenType::COMMA;
    transitionTable["("] = TokenType::LPAREN; 
    transitionTable[")"] = TokenType::RPAREN; 
    transitionTable["["] = TokenType::LBRACKET; 
    transitionTable["]"] = TokenType::RBRACKET;
    transitionTable["{"] = TokenType::LBRACE;
    transitionTable["}"] = TokenType::RBRACE;
    transitionTable["(*"] = TokenType::LCOMMENT; 
    transitionTable["*)"] = TokenType::RCOMMENT;
    transitionTable["."] = TokenType::PERIOD;
    transitionTable[".."] = TokenType::PERIOD_PERIOD;
}

Token * Token::identifier_token(char firstChar, Source *source) {
    Token *token = new Token(firstChar);

    for (char ch = source->nextChar(); isalnum(ch); ch = source->nextChar()) {
        token->text += ch;
    }
    
    string upper = toUpperCase(token->text);
    if (Token::transitionTable.find(upper) != Token::transitionTable.end()) {
        token->type = Token::transitionTable[upper];
    }
    else {
        token->type = TokenType::IDENTIFIER;
    }

    return token;
}

Token * Token::number_token(char firstChar, Source *source) {
    Token *token = new Token(firstChar);
    int decCounter = 0;

    for (char ch = source->nextChar(); isdigit(ch) || (ch == '.'); ch = source->nextChar()) {
        if (ch == '.') {
            decCounter++;
        }
        token->text += ch;
    }

    if (decCounter == 0) {
        token->type = TokenType::INTEGER;
    } else if (decCounter == 1) {
        token->type    = TokenType::REAL;
    } else {
        error(token, "Invalid number");
    }
    return token;
}

Token *Token::string_token(char firstChar, Source *source) {
    Token *token = new Token(firstChar);  // the leading '

    for (char ch = source->nextChar(); ch != '\''; ch = source->nextChar()) {
        token->text += ch;
    }

    token->text += '\'';
    source->nextChar();
    if (token->text.length() == 3) {
        token->type = TokenType::CHAR;
    } else {
        token->type = TokenType::STRING;
    }

    return token;
}

Token * Token::symbol_token(char firstChar, Source *source) {
    Token *token = new Token(firstChar);

    if (Token::transitionTable.find(token->text) != Token::transitionTable.end()) {
        string next_ch = std::string(1, source->nextChar());
        string double_lookahead = token->text + next_ch;
        if (Token::transitionTable.find(double_lookahead) != Token::transitionTable.end()) {
            token->text = double_lookahead;
            token->type = Token::transitionTable[double_lookahead];
        } else {
            token->type = Token::transitionTable[token->text];
            return token;
        }
    } else {
        if (firstChar == EOF) {
            token->type = TokenType::END_OF_FILE;
        } else {
            error(token, "Invalid Token");
            token->type = TokenType::ERROR;
        }
    }

    source->nextChar();
    return token;
}

void Token::error(Token *token, string message)
{   
    token->type = TokenType::ERROR;
    token->text = message + " at " + token->text.c_str();
}

ostream& operator<<(ostream& os, const Token& token) {
    os << TOKEN_STRINGS[token.type] << " : " << token.text << endl;
    return os;
}

}
