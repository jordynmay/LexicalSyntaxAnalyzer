%{
#include <stdio.h>
#include <stack>
#include "SymbolTable.h"

#define ARITHMETIC_OP -10
#define LOGICAL_OP -11
#define RELATIONAL_OP -12

#define UNDEFINED_IDENT "Undefined identifier"
#define MULTPLY_DEFINED_IDENT "Multiply defined identifier"
#define ARG1_MUST_BE_INT "Arg 1 must be integer"
#define ARG2_MUST_BE_INT "Arg 2 must be integer"
#define ARG1_MUST_BE_LIST "Arg 1 must be list"
#define ARG2_MUST_BE_LIST "Arg 2 must be list"
#define ARG1_MUST_BE_FNCT "Arg 1 must be function"
#define ARG2_MUST_BE_FNCT "Arg 2 must be function"
#define ARG1_MUST_BE_INT_FLOAT_BOOL "Arg 1 must be integer or float or bool"
#define ARG2_MUST_BE_INT_FLOAT_BOOL "Arg 2 must be integer or float or bool"
#define ARG1_CANNOT_BE_FNCT "Arg 1 cannot be function"
#define ARG2_CANNOT_BE_FNCT "Arg 2 cannot be function"
#define ARG1_CANNOT_BE_LIST "Arg 1 cannot be list"
#define ARG2_CANNOT_BE_LIST "Arg 2 cannot be list"
#define ARG1_CANNOT_BE_FNCT_NULL "Arg 1 cannot be function or null"
#define ARG2_CANNOT_BE_FNCT_NULL "Arg 2 cannot be function or null"
#define ARG1_CANNOT_BE_FNCT_NULL_LIST_STR "Arg 1 cannot be function or null or list or string"
#define ARG2_CANNOT_BE_FNCT_NULL_LIST_STR "Arg 2 cannot be function or null or list or string"
#define TOO_MANY_PARAMS "Too many parameters in function call"
#define TOO_FEW_PARAMS "Too few parameters in function call"

// Used for enabling/disabling print statements easily
// Helpful for error detection
bool PRINT_RULE = 0;
bool PRINT_TOKEN = 0;
bool PRINT_SCOPE = 0;
bool PRINT_ADD = 0;

int numLines = 1; 
stack<SYMBOL_TABLE> scopeStack; // Global stack

void printRule(const char *, const char *);
int yyerror(const char *s);
void printTokenInfo(const char* tokenType, const char* lexeme);
void beginScope();
void endScope();
TYPE_INFO findEntryInAnyScope(const string theName);

extern "C" 
{
    int yyparse(void);
    int yylex(void);
    int yywrap() { return 1; }
}

%}

/* Used to associate an identifier's name with an identifier token */
%union
{
  char* text;
  TYPE_INFO typeInfo;
};

/* Token declarations */
%token  T_IDENT T_INTCONST T_UNKNOWN
%token  T_ADD T_SUB T_MULT T_DIV T_QUIT
%token  T_FLOATCONST T_STRCONST
%token  T_IF T_WHILE T_FOR T_IN
%token  T_FUNCTION T_TRUE T_FALSE
%token  T_PRINT T_CAT T_READ
%token  T_LIST T_COMMENT
%token  T_SEMICOLON T_COMMA T_LPAREN
%token  T_LBRACKET T_RBRACKET T_LBRACE T_RBRACE
%token  T_MOD T_POW
%token  T_LT T_LE T_GT T_GE T_EQ T_NE
%token  T_NOT T_AND T_OR T_ASSIGN

%type <text> T_IDENT // Associates T_IDENT token with the char* type
%type <typeInfo> N_CONST N_EXPR_LIST N_EXPR N_IF_EXPR N_WHILE_EXPR
%type <typeInfo> N_FOR_EXPR N_COMPOUND_EXPR N_ARITHLOGIC_EXPR
%type <typeInfo> N_ASSIGNMENT_EXPR N_OUTPUT_EXPR N_INPUT_EXPR
%type <typeInfo> N_LIST_EXPR N_FUNCTION_DEF N_FUNCTION_CALL
%type <typeInfo> N_QUIT_EXPR N_PARAM_LIST N_PARAMS
%type <typeInfo> N_VAR N_ENTIRE_VAR N_SINGLE_ELEMENT
%type <typeInfo> N_SIMPLE_ARITHLOGIC N_ADD_OP_LIST
%type <typeInfo> N_TERM N_FACTOR N_MULT_OP_LIST
%type <typeInfo> N_REL_OP N_INDEX
%type <integer> N_ADD_OP N_MULT_OP

%nonassoc   T_RPAREN
%nonassoc   T_ELSE

/* Starting point */
%start		N_START

/* Translation rules */
%%
N_START		: N_EXPR
			{
			printRule("START", "EXPR");
      endScope();
			printf("\n---- Completed parsing ----\n\n");
			}
			;
N_EXPR      : N_IF_EXPR
            {
            printRule("EXPR", "IF_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_WHILE_EXPR
            {
            printRule("EXPR", "WHILE_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_FOR_EXPR
            {
            printRule("EXPR", "FOR_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_COMPOUND_EXPR
            {
            printRule("EXPR", "COMPOUND_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_ARITHLOGIC_EXPR
            {
            printRule("EXPR", "ARITHLOGIC_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_ASSIGNMENT_EXPR
            {
            printRule("EXPR", "ASSIGNMENT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_OUTPUT_EXPR
            {
            printRule("EXPR", "OUTPUT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_INPUT_EXPR
            {
            printRule("EXPR", "INPUT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_LIST_EXPR
            {
            printRule("EXPR", "LIST_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_FUNCTION_DEF
            {
            printRule("EXPR", "FUNCTION_DEF");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_FUNCTION_CALL
            {
            printRule("EXPR", "FUNCTION_CALL");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_QUIT_EXPR
            {
            printRule("EXPR", "QUIT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            ;
N_CONST     : T_INTCONST
            {
            printRule("CONST", "INTCONST");
            $$.type = INT;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            | T_STRCONST
            {
            printRule("CONST", "STRCONST");
            $$.type = STR;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            | T_FLOATCONST
            {
            printRule("CONST", "FLOATCONST");
            $$.type = FLOAT;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            | T_TRUE
            {
            printRule("CONST", "TRUE");
            $$.type = BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            | T_FALSE
            {
            printRule("CONST", "FALSE");
            $$.type = BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            ;
N_COMPOUND_EXPR : T_LBRACE N_EXPR N_EXPR_LIST T_RBRACE
            {
            printRule("COMPOUND_EXPR", "{ EXPR EXPR_LIST }");
            if($3.type == GOES_TO_EPSILON)
            {
              $$.type = $2.type;
              $$.numParams = $2.numParams;
              $$.returnType = $2.returnType;
            }
            else
            {
              $$.type = $3.type;
              $$.numParams = $3.numParams;
              $$.returnType = $3.returnType;
            }
            }
            ;
N_EXPR_LIST : T_SEMICOLON N_EXPR N_EXPR_LIST
            {
            printRule("EXPR_LIST", "; EXPR EXPR_LIST");
            if($3.type == GOES_TO_EPSILON)
            {
              $$.type = $2.type;
              $$.numParams = $2.numParams;
              $$.returnType = $2.returnType;
            }
            else
            {
              $$.type = $3.type;
              $$.numParams = $3.numParams;
              $$.returnType = $3.returnType;
            }
            }
            |
            {
            printRule("EXPR_LIST", "epsilon");
            $$.type = GOES_TO_EPSILON;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            ;
N_IF_EXPR   : N_COND_IF T_RPAREN N_THEN_EXPR
            {
            printRule("IF_EXPR", "COND_IF ) THEN_EXPR");
            }
            | N_COND_IF T_RPAREN N_THEN_EXPR T_ELSE N_EXPR
            {
            printRule("IF_EXPR", "COND_IF ) THEN_EXPR ELSE EXPR");
            }
            ;
N_COND_IF   : T_IF T_LPAREN N_EXPR
            {
            printRule("COND_IF", "IF ( EXPR");
            }
            ;
N_THEN_EXPR : N_EXPR
            {
            printRule("THEN_EXPR", "EXPR");
            }
            ;
N_WHILE_EXPR    : T_WHILE T_LPAREN N_EXPR T_RPAREN N_EXPR
            {
            printRule("WHILE_EXPR", "WHILE ( EXPR ) EXPR");
            if($5.type == FUNCTION || $5.type == LIST
              || $5.type == NULL || $5.type == STR)
            {
              yyerror(ARG2_CANNOT_BE_FNCT_NULL_LIST_STR);
            }
            else
            {
              $$.type = $5.type;
            }
            }
            ;
N_FOR_EXPR  : T_FOR T_LPAREN T_IDENT
            {
            printRule("FOR_EXPR", "FOR ( IDENT IN EXPR ) EXPR");
            string lexeme = string($3);
            TYPE_INFO temp;
            temp.type = UNDEFINED;
            temp.numParams = NOT_APPLICABLE;
            temp.returnType = NOT_APPLICABLE;
            bool added = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
            if(added && PRINT_ADD)
            {
            printf("___Adding %s to symbol table\n", $3);
            }
            }
            T_IN N_EXPR T_RPAREN N_EXPR
            {
            }
            ;
N_LIST_EXPR : T_LIST T_LPAREN N_CONST_LIST T_RPAREN
            {
            printRule("LIST_EXPR", "LIST ( CONST_LIST )");
            $$.type = LIST;
            }
            ;
N_CONST_LIST    : N_CONST T_COMMA N_CONST_LIST
            {
            printRule("CONST_LIST", "CONST, CONST_LIST");
            }
            | N_CONST
            {
            printRule("CONST_LIST", "CONST");
            }
            ;
N_ASSIGNMENT_EXPR    : T_IDENT N_INDEX
            {
            printRule("ASSIGNMENT_EXPR", "IDENT INDEX = EXPR");
        
            if($2.type != GOES_TO_EPSILON)
            {
              TYPE_INFO temp = findEntryInAnyScope(string($1));
              if(temp.type == UNDEFINED)
              {
                // Cannot index into a list that is not yet in the ST
                yyerror(UNDEFINED_IDENT);
              }
              else if(temp.type != LIST)
              {
                // Cannot index into a variable that is not a list
                yyerror(ARG1_MUST_BE_LIST);
              }
            }

            string lexeme = string($1);
            TYPE_INFO temp;
            temp.type = $1.type;
            temp.numParams = NOT_APPLICABLE;
            temp.returnType = NOT_APPLICABLE;
            bool added = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
            if(added && PRINT_ADD)
            {
            printf("___Adding %s to symbol table\n", $1);
            }
            }
            T_ASSIGN N_EXPR
            {
              if($1.type == LIST && $5.type == LIST)
              {
                // No lists of lists allowed
                yyerror(ARG2_CANNOT_BE_LIST);
              }
              $$.type = $5.type;
            }
            ;
N_INDEX     : T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
            {
            printRule("INDEX", "[[ EXPR ]]");
            }
            |
            {
            printRule("INDEX", "epsilon");
            $$.type = GOES_TO_EPSILON;
            }
            ;
N_QUIT_EXPR : T_QUIT T_LPAREN T_RPAREN
            {
            printRule("QUIT_EXPR", "QUIT()");
            $$.type = NULL;
            exit(1);
            }
            ;
N_OUTPUT_EXPR   : T_PRINT T_LPAREN N_EXPR T_RPAREN
            {
            printRule("OUTPUT_EXPR", "PRINT ( EXPR )");
            if($3.type == FUNCTION || $3.type == NULL)
            {
              yyerror(ARG1_CANNOT_BE_FNCT_NULL);
            }
            $$.type = $3.type;
            }
            | T_CAT T_LPAREN N_EXPR T_RPAREN
            {
            printRule("OUTPUT_EXPR", "CAT ( EXPR )");
            $$.type = NULL;
            }
            ;
N_INPUT_EXPR    : T_READ T_LPAREN T_RPAREN
            {
            printRule("INPUT_EXPR", "READ ( )");
            }
            ;
N_FUNCTION_DEF  : T_FUNCTION
            {
              beginScope();
            }
            T_LPAREN N_PARAM_LIST T_RPAREN N_COMPOUND_EXPR
            {
            printRule("FUNCTION_DEF", "FUNCTION ( PARAM_LIST ) COMPOUND_EXPR");
            $$.type = FUNCTION;
            endScope();
            }
            ;
N_PARAM_LIST    : N_PARAMS
            {
            printRule("PARAM_LIST", "PARAMS");
            }
            | N_NO_PARAMS
            {
            printRule("PARAM_LIST", "NO_PARAMS");
            }
            ;
N_NO_PARAMS :
            {
            printRule("NO_PARAMS", "epsilon");
            }
            ;
N_PARAMS    : T_IDENT
            {
            printRule("PARAMS", "IDENT");
            string lexeme = string($1);
            TYPE_INFO temp;
            temp.type = INT;
            temp.numParams = NOT_APPLICABLE;
            temp.returnType = NOT_APPLICABLE;
            if(PRINT_ADD)
            {
              printf("___Adding %s to symbol table\n", $1);
            }
            bool success =  scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
            if(!success)
            {
              yyerror(MULTPLY_DEFINED_IDENT);
              exit(1);
            }
            }
            | T_IDENT T_COMMA N_PARAMS
            {
            printRule("PARAMS", "IDENT, PARAMS");
            string lexeme = string($1);
            TYPE_INFO temp;
            temp.type = INT;
            temp.numParams = NOT_APPLICABLE;
            temp.returnType = NOT_APPLICABLE;
            if(PRINT_ADD)
            {
              printf("___Adding %s to symbol table\n", $1);
            }
            bool success =  scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
            if(!success)
            {
              yyerror(MULTPLY_DEFINED_IDENT);
              exit(1);
            }
            }
            ;
N_FUNCTION_CALL : T_IDENT
            {
            }
            T_LPAREN N_ARG_LIST T_RPAREN
            {
            printRule("FUNCTION_CALL", "IDENT ( ARG_LIST )");
            string lexeme = string($1);
            TYPE_INFO temp = findEntryInAnyScope(lexeme);
            if(temp.type == UNDEFINED)
            {
              yyerror(UNDEFINED_IDENT);
              exit(1);
            }
            }
            ;
N_ARG_LIST  : N_ARGS
            {
            printRule("ARG_LIST", "ARGS");
            }
            | N_NO_ARGS
            {
            printRule("ARG_LIST", "NO_ARGS");
            }
            ;
N_NO_ARGS   :
            {
            printRule("NO_ARGS", "epsilon");
            }
            ;
N_ARGS      : N_EXPR
            {
            printRule("ARGS", "EXPR");
            }
            | N_EXPR T_COMMA N_ARGS
            {
            printRule("ARGS", "EXPR, ARGS");
            }
            ;
N_ARITHLOGIC_EXPR   : N_SIMPLE_ARITHLOGIC
            {
            printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC");
            }
            | N_SIMPLE_ARITHLOGIC N_REL_OP N_SIMPLE_ARITHLOGIC
            {
            printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC REL_OP SIMPLE_ARITHLOGIC");
            $$.type = BOOL;
            }
            ;
N_SIMPLE_ARITHLOGIC : N_TERM N_ADD_OP_LIST
            {
            printRule("SIMPLE_ARITHLOGIC", "TERM ADD_OP_LIST");
            if($2.type == NOT_APPLICABLE)
            {
              $$.type = $1.type;
            }
            else
            {
              // check arg1 and arg2
              if($2.type == BOOL)
              {
                $$.type = BOOL;
              }
              else if($1.type == FLOAT || $2.type == FLOAT)
              {
                $$.type = FLOAT;
              }
              else if($1.type == INT && $2.type == INT)
              {
                $$.type = INT;
              }
            }
            }
            ;
N_ADD_OP_LIST   : N_ADD_OP N_TERM N_ADD_OP_LIST
            {
            printRule("ADD_OP_LIST", "ADD_OP TERM ADD_OP_LIST");
            if($1 == ARITHMETIC_OP)
            {
              //check arg1 and arg2
              if($3.type == NOT_APPLICABLE)
              {
                $$.type = $2.type;
              }
              else
              {
                if($3.type == BOOL)
                {
                  $$.type = BOOL;
                }
                else if($2.type == FLOAT || $3.type == FLOAT)
                {
                  $$.type = FLOAT;
                }
                else if($2.type == INT && $3.type == INT)
                {
                  $$.type = INT;
                }
              }
            }
            else if($1 == LOGICAL_OP)
            {
              $$.type = BOOL;
            }
            }
            |
            {
            printRule("ADD_OP_LIST", "epsilon");
            $$.type = NOT_APPLICABLE;
            }
            ;
N_TERM      : N_FACTOR N_MULT_OP_LIST
            {
            printRule("TERM", "FACTOR MULT_OP_LIST");
            if($2.type == NOT_APPLICABLE)
            {
              $$.type = $1.type;
            }
            else
            {
              //check arg1 and arg2
              if($2.type == BOOL)
              {
                $$.type = BOOL;
              }
              else if($1.type == FLOAT || $2.type == FLOAT)
              {
                $$.type = FLOAT;
              }
              else if($1.type == INT && $2.type == INT)
              {
                $$.type = INT;
              }
            }
            }
            ;
N_MULT_OP_LIST  : N_MULT_OP N_FACTOR N_MULT_OP_LIST
            {
            printRule("MULT_OP_LIST", "MULT_OP FACTOR MULT_OP_LIST");
            if($1 == ARITHMETIC_OP)
            {
              //check arg 1 and arg 2
              if($3.type == NOT_APPLICABLE)
              {
                $$.type = $2.type;
              }
              else
              {
                if($3.type == BOOL)
                {
                  $$.type = BOOL;
                }
                else if($2.type == FLOAT || $3.type == FLOAT)
                {
                  $$.type = FLOAT;
                }
                else if($2.type == INT && $3.type == INT)
                {
                  $$.type = INT;
                }
              }
            }
            else if($1 == LOGICAL_OP)
            {
              $$.type = BOOL;
            }
            }
            |
            {
            printRule("MULT_OP_LIST", "epsilon");
            $$.type = NOT_APPLICABLE;
            }
            ;
N_FACTOR    : N_VAR
            {
            printRule("FACTOR", "VAR");
            $$.type = $1.type;
            }
            | N_CONST
            {
            printRule("FACTOR", "CONST");
            $$.type = $1.type;
            }
            | T_LPAREN N_EXPR T_RPAREN
            {
            printRule("FACTOR", "( EXPR )");
            $$.type = $2.type;
            }
            | T_NOT N_FACTOR
            {
            printRule("FACTOR", "! FACTOR");
            $$.type = $2.type;
            }
            ;
N_ADD_OP    : T_ADD
            {
            printRule("ADD_OP", "+");
            $$ = ARITHMETIC_OP;
            }
            | T_SUB
            {
            printRule("ADD_OP", "-");
            $$ = ARITHMETIC_OP;
            }
            | T_OR
            {
            printRule("ADD_OP", "|");
            $$ = LOGICAL_OP;
            }
            ;
N_MULT_OP   : T_MULT
            {
            printRule("MULT_OP", "*");
            $$ = ARITHMETIC_OP;
            }
            | T_DIV
            {
            printRule("MULT_OP", "/");
            $$ = ARITHMETIC_OP;
            }
            | T_AND
            {
            printRule("MULT_OP", "&");
            $$ = LOGICAL_OP;
            }
            | T_MOD
            {
            printRule("MULT_OP", "%%");
            $$ = ARITHMETIC_OP;
            }
            | T_POW
            {
            printRule("MULT_OP", "^");
            $$ = ARITHMETIC_OP;
            }
            ;
N_REL_OP    : T_LT
            {
            printRule("REL_OP", "<");
            $$ = RELATIONAL_OP;
            }
            | T_GT
            {
            printRule("REL_OP", ">");
            $$ = RELATIONAL_OP;
            }
            | T_LE
            {
            printRule("REL_OP", "<=");
            $$ = RELATIONAL_OP;
            }
            | T_GE
            {
            printRule("REL_OP", ">=");
            $$ = RELATIONAL_OP;
            }
            | T_EQ
            {
            printRule("REL_OP", "==");
            $$ = RELATIONAL_OP;
            }
            | T_NE
            {
            printRule("REL_OP", "!=");
            $$ = RELATIONAL_OP;
            }
            ;
N_VAR       : N_ENTIRE_VAR
            {
            printRule("VAR", "ENTIRE_VAR");
            $$.type = $1.type;
            }
            | N_SINGLE_ELEMENT
            {
            printRule("VAR", "SINGLE_ELEMENT");
            $$.type = $1.type;
            }
            ;
N_SINGLE_ELEMENT    : T_IDENT
            T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
            {
            printRule("SINGLE_ELEMENT", "IDENT [[ EXPR ]]");
            string lexeme = string($1);
            TYPE_INFO temp = findEntryInAnyScope(lexeme);
            if(temp.type == UNDEFINED)
            {
              yyerror(UNDEFINED_IDENT);
              exit(1);
            }
            else if(temp.type != LIST)
            {
              // Cannot index into a variable that is not a list
              yyerror(ARG1_MUST_BE_LIST);
            }
            else
            {
              $$.type == INT_OR_STR_OR_FLOAT_OR_BOOL;
            }
            }
            ;
N_ENTIRE_VAR    : T_IDENT
            {
            printRule("ENTIRE_VAR", "IDENT");
            string lexeme = string($1);
            TYPE_INFO temp = findEntryInAnyScope(lexeme);
            if(temp.type == UNDEFINED)
            {
              yyerror(UNDEFINED_IDENT);
              exit(1);
            }
            $$.type = $1.type;
            }
            ;


%%

#include "lex.yy.c"
extern FILE *yyin;

void printRule(const char *lhs, const char *rhs) 
{
  if(PRINT_RULE)
  {
    printf("%s -> %s\n", lhs, rhs);
  }
  return;
}

int yyerror(const char *s) 
{
  printf("Line %d: ", numLines);
  printf("%s\n", s);
  exit(1);
}

// Print out token's type and lexeme
void printTokenInfo(const char* tokenType, const char* lexeme) 
{
  if(PRINT_TOKEN)
  {
    printf("TOKEN: %-20s LEXEME: %s\n", tokenType, lexeme);
  }
  return;
}

// Called each time we enter a new scope
void beginScope()
{
  scopeStack.push(SYMBOL_TABLE());
  if(PRINT_SCOPE)
  {
    printf("\n___Entering new scope...\n\n");
  }
  return;
}

// Called each time we exit a scope
void endScope()
{
  scopeStack.pop();
  if(PRINT_SCOPE)
  {
    printf("\n___Exiting scope...\n\n");
  }
  return;
}

// Looks through all symbol tables in the global stack
TYPE_INFO findEntryInAnyScope(const string theName)
{
  if(scopeStack.empty()) return(UNDEFINED);
  TYPE_INFO temp = scopeStack.top().findEntry(theName);
  if(temp.type != UNDEFINED)
  {
    return(temp);
  }
  else
  {
    SYMBOL_TABLE symbolTable = scopeStack.top();
    scopeStack.pop();
    temp = findEntryInAnyScope(theName);
    scopeStack.push(symbolTable);
    return(temp);
  }
}

int main() 
{
  beginScope();
  do 
  {
	yyparse();
  } while (!feof(yyin));

  //printf("%d lines processed\n", numLines);
  return(0);
}
