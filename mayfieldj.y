%{
#include <stdio.h>
#include <stack>
#include "SymbolTable.h"

// Used to distinguish whether an operator is arithmetic, logical, or relational
// Helps to determine what type the operands should be
#define ARITHMETIC_OP 100
#define LOGICAL_OP 101
#define RELATIONAL_OP 102

#define MUST_BE_INT 200
#define MUST_BE_LIST 201
#define MUST_BE_FNCT 202
#define MUST_BE_INT_FLOAT_BOOL 203
#define CANNOT_BE_FNCT 204
#define CANNOT_BE_LIST 205
#define CANNOT_BE_FNCT_NULL 206
#define CANNOT_BE_FNCT_NULL_LIST 207
#define CANNOT_BE_FNCT_NULL_LIST_STR 208
#define TOO_MANY_PARAMS 209
#define TOO_FEW_PARAMS 210
#define PARAMS_MUST_BE_INT 211
#define UNDEFINED_IDENT 212
#define MULTIPLY_DEFINED_IDENT 213
#define DEFAULT_ERROR 214

const int NUM_ERR_MESSAGES = 15;
int numArgs = 0;

// Error message strings for each supported error
const string ERR_MSG[NUM_ERR_MESSAGES] = {
  "must be integer",
  "must be list",
  "must be function",
  "must be integer or float or bool",
  "cannot be function",
  "cannot be list",
  "cannot be function or null",
  "cannot be function or null or list",
  "cannot be function or null or list or string",
  "Too many parameters in function call",
  "Too few parameters in function call",
  "Function parameters must be integer",
  "Undefined identifier",
  "Multiply defined identifier",
  "<undefined error>"
};

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
void semanticError(const int argNum, const int errNum);
bool isIntCompatible(const int typeval);
bool isFloatCompatible(const int typeval);
bool isBoolCompatible(const int typeval);
bool isStrCompatible(const int typeval);
bool isListCompatible(const int typeval);
bool isIntOrFloatOrBoolCompatible(const int typeval);
bool isIntOrStrOrFloatOrBoolCompatible(const int typeval);

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
  int number;
  bool flag = false;
  int num;
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
%type <typeInfo> N_INDEX N_COND_IF N_THEN_EXPR
%type <number> N_ADD_OP N_MULT_OP N_REL_OP

%nonassoc   T_RPAREN
%nonassoc   T_ELSE

/* Starting point */
%start		N_START

/* Translation rules */
%%
N_START		: N_EXPR
			{
			printRule("START", "EXPR");
      printf("EXPR type is: ");
      switch($1.type) //!!! add new types to the cases
      {
        case(NULL_TYPE):
          printf("NULL");
          break;
        case(INT):
          printf("INT");
          break;
        case(STR):
          printf("STR");
          break;
        case(BOOL):
          printf("BOOL");
          break;
        case(FLOAT):
          printf("FLOAT");
          break;
        case(LIST):
          printf("LIST");
          break;
        case(FUNCTION):
          printf("FUNCTION");
          break;
        case(INT_OR_STR_OR_FLOAT_OR_BOOL):
          printf("INT_OR_STR_OR_FLOAT_OR_BOOL");
          break;
        case(UNDEFINED):
          printf("UNDEFINED");
          break;
        case(NOT_APPLICABLE):
          printf("NOT_APPLICABLE");
          break;
        case(GOES_TO_EPSILON):
          printf("GOES_TO_EPSILON");
          break;
        default:
          printf("DEFAULT");
          break;
      }
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
            if($3.type == FUNCTION)
            {
              semanticError(2, CANNOT_BE_FNCT);
            }
            $$.type = $3.type;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;
            $$.isParam = $3.isParam;
            }
            | N_COND_IF T_RPAREN N_THEN_EXPR T_ELSE N_EXPR
            {
            printRule("IF_EXPR", "COND_IF ) THEN_EXPR ELSE EXPR");
            if($3.type == FUNCTION)
            {
              semanticError(2, CANNOT_BE_FNCT);
            }
            //printf("else type is: %d\n", $5.type);!!!
            if($5.type == FUNCTION)
            {
              semanticError(3, CANNOT_BE_FNCT);
            }
            //!!! assign type based on an or type combo
            //ASK DR. LEOPOLD
            $$.type = $3.type;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;
            $$.isParam = $3.isParam;
            }
            ;
N_COND_IF   : T_IF T_LPAREN N_EXPR
            {
            printRule("COND_IF", "IF ( EXPR");
            $$.type = $3.type;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;

            if($$.type == FUNCTION || $$.type == LIST 
            || $$.type == NULL_TYPE || $$.type == STR)
            {
              semanticError(1, CANNOT_BE_FNCT_NULL_LIST_STR); //!!!okay to check here?
            }
            }
            ;
N_THEN_EXPR : N_EXPR
            {
            printRule("THEN_EXPR", "EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            ;
N_WHILE_EXPR    : T_WHILE T_LPAREN N_EXPR
            {
            if($3.type == FUNCTION || $3.type == LIST
              || $3.type == NULL_TYPE || $3.type == STR)
            {
              semanticError(1, CANNOT_BE_FNCT_NULL_LIST_STR);
            }
            }
            T_RPAREN N_EXPR
            {
            printRule("WHILE_EXPR", "WHILE ( EXPR ) EXPR");
            $$.type = $6.type;
            }
            ;
N_FOR_EXPR  : T_FOR T_LPAREN T_IDENT
            {
            printRule("FOR_EXPR", "FOR ( IDENT IN EXPR ) EXPR");
            string lexeme = string($3);
            TYPE_INFO temp = findEntryInAnyScope(lexeme);
            if(temp.type != UNDEFINED)
            {
              if(!isIntOrStrOrFloatOrBoolCompatible(temp.type))
              {
                semanticError(1, CANNOT_BE_FNCT_NULL_LIST); //!!!change
              }
            }
            else
            {
              TYPE_INFO add_this = {INT_OR_STR_OR_FLOAT_OR_BOOL, NOT_APPLICABLE, NOT_APPLICABLE};
              bool added = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, add_this));
              if(added && PRINT_ADD)
              {
              printf("___Adding %s to symbol table\n", $3);
              }
            }
            }
            T_IN N_EXPR
            {
            if($6.type != LIST)
            {
              semanticError(2, MUST_BE_LIST);
            }
            }
            T_RPAREN N_EXPR
            {
            $$.type = $9.type;
            $$.numParams = $9.numParams;
            $$.returnType = $9.returnType;
            }
            ;
N_LIST_EXPR : T_LIST T_LPAREN N_CONST_LIST T_RPAREN
            {
            printRule("LIST_EXPR", "LIST ( CONST_LIST )");
            $$.type = LIST;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
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
            string lexeme = string($1);
            TYPE_INFO temp = scopeStack.top().findEntry(lexeme);
            if(temp.type == UNDEFINED)
            {
              if(PRINT_ADD)
              {
                printf("___Adding %s to symbol table\n", $1);
              }
              TYPE_INFO temp2 = {NOT_APPLICABLE, NOT_APPLICABLE, NOT_APPLICABLE, false};
              bool added = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp2));
              $<flag>$ = false;
            }
            else
            {
              $<flag>$ = true;
            }
            }
            T_ASSIGN N_EXPR
            {
            string lexeme = string($1);
            TYPE_INFO temp = scopeStack.top().findEntry(lexeme);

            if($2.type != GOES_TO_EPSILON && !isListCompatible(temp.type))
            {
              // Cannot index into a variable that is not a list
              semanticError(1, MUST_BE_LIST);
            }
            if($<flag>3)
            {
              if(temp.isParam && !isIntCompatible($5.type))
              {
                // Parameters must be integers
                semanticError(1, MUST_BE_INT);
              }
              TYPE_INFO temp2 = {$5.type, $5.numParams, $5.returnType, false};
              scopeStack.top().modifyEntry(SYMBOL_TABLE_ENTRY(lexeme, temp2));
            }
            else
            {
              TYPE_INFO temp2 = {$5.type, $5.numParams, $5.returnType, false};
              scopeStack.top().modifyEntry(SYMBOL_TABLE_ENTRY(lexeme, temp2));
            }
            if($2.type != GOES_TO_EPSILON && $5.type == LIST)
            {
              // No lists of lists allowed
              semanticError(1, CANNOT_BE_LIST);
            }
            $$.type = $5.type;
            $$.numParams = $5.numParams;
            $$.returnType = $5.returnType;
            $$.isParam = $5.isParam;
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
            $$.type = NULL_TYPE;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            exit(1);
            }
            ;
N_OUTPUT_EXPR   : T_PRINT T_LPAREN N_EXPR T_RPAREN
            {
            printRule("OUTPUT_EXPR", "PRINT ( EXPR )");
            if($3.type == FUNCTION || $3.type == NULL_TYPE)
            {
              semanticError(1, CANNOT_BE_FNCT_NULL);
            }
            $$.type = $3.type;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;
            }
            | T_CAT T_LPAREN N_EXPR T_RPAREN
            {
            printRule("OUTPUT_EXPR", "CAT ( EXPR )");
            if($3.type == FUNCTION || $3.type == NULL_TYPE)
            {
              semanticError(1, CANNOT_BE_FNCT_NULL);
            }
            $$.type = NULL_TYPE;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;
            }
            ;
N_INPUT_EXPR    : T_READ T_LPAREN T_RPAREN
            {
            printRule("INPUT_EXPR", "READ ( )");
            $$.type = INT_OR_STR_OR_FLOAT;
            }
            ;
N_FUNCTION_DEF  : T_FUNCTION
            {
              printRule("FUNCTION_DEF", "FUNCTION ( PARAM_LIST ) COMPOUND_EXPR");
              beginScope();
            }
            T_LPAREN N_PARAM_LIST
            {
              $<num>$ = scopeStack.top().getNumParams();
            }
            T_RPAREN N_COMPOUND_EXPR
            {
            endScope();
            if($7.type == FUNCTION)
            {
              //yyerror("Arg 2 cannot be function");
              semanticError(2, CANNOT_BE_FNCT);
            }
            $$.type = FUNCTION;
            $$.numParams = $<num>5; //!!! should be length of param list
            $$.returnType = $7.type;
            $$.isParam = false;
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
            TYPE_INFO temp = {INT, NOT_APPLICABLE, NOT_APPLICABLE, true};
            if(PRINT_ADD)
            {
              printf("___Adding %s to symbol table\n", $1);
            }
            bool success =  scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
            if(!success)
            {
              semanticError(-1, MULTIPLY_DEFINED_IDENT);
              exit(1);
            }
            }
            | T_IDENT T_COMMA N_PARAMS
            {
            printRule("PARAMS", "IDENT, PARAMS");
            string lexeme = string($1);
            TYPE_INFO temp = {INT, NOT_APPLICABLE, NOT_APPLICABLE, true};
            if(PRINT_ADD)
            {
              printf("___Adding %s to symbol table\n", $1);
            }
            bool success =  scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
            if(!success)
            {
              semanticError(-1, MULTIPLY_DEFINED_IDENT);
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
              semanticError(-1, UNDEFINED_IDENT);
              exit(1);
            }
            else if(temp.type != FUNCTION)
            {
              semanticError(1, MUST_BE_FNCT);
            }
            // Check to make sure there are the correct number of parameters!!!!!
            if(numArgs > temp.numParams)
            {
              semanticError(-1, TOO_MANY_PARAMS);
            }
            else if(numArgs < temp.numParams)
            {
              semanticError(-1, TOO_FEW_PARAMS);
            }
            $$.type = temp.returnType;
            $$.numParams = UNDEFINED;
            $$.returnType = UNDEFINED;
            numArgs = 0;
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
            numArgs++;
            if(!isIntCompatible($1.type))
            {//pass ctr as arg num!!!
              semanticError(-1, PARAMS_MUST_BE_INT);
              //semanticError(1, MUST_BE_INT); //!!! must change arg num
            }
            }
            | N_EXPR T_COMMA N_ARGS
            {
            printRule("ARGS", "EXPR, ARGS");
            numArgs++;
            if(!isIntCompatible($1.type))
            {
              semanticError(-1, PARAMS_MUST_BE_INT);
              //semanticError(1, MUST_BE_INT); //!!! change to arg num
            }
            }
            ;
N_ARITHLOGIC_EXPR   : N_SIMPLE_ARITHLOGIC
            {
            printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_SIMPLE_ARITHLOGIC N_REL_OP N_SIMPLE_ARITHLOGIC
            {
            printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC REL_OP SIMPLE_ARITHLOGIC");
            // Both arguments must be int, float, or bool compatible
            if(!isIntOrFloatOrBoolCompatible($1.type))
            {
              semanticError(1, MUST_BE_INT_FLOAT_BOOL);
            }
            else if(!isIntOrFloatOrBoolCompatible($3.type))
            {
              semanticError(2, MUST_BE_INT_FLOAT_BOOL);
            }
            // Resulting type is a bool because a relational operator was used
            $$.type = BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            ;
N_SIMPLE_ARITHLOGIC : N_TERM N_ADD_OP_LIST
            {
            printRule("SIMPLE_ARITHLOGIC", "TERM ADD_OP_LIST");
            if($2.type == NOT_APPLICABLE)
            {
              $$.type = $1.type;
              $$.numParams = $1.numParams;
              $$.returnType = $1.returnType;
            }
            else
            {
              // Both arguments must be int, float, or bool compatible
              if(!isIntOrFloatOrBoolCompatible($1.type))
              {
                semanticError(1, MUST_BE_INT_FLOAT_BOOL);
              }
              if(!isIntOrFloatOrBoolCompatible($2.type))
              {
                semanticError(2, MUST_BE_INT_FLOAT_BOOL);
              }
              // If both operands are bool compatible, resulting type is bool
              if(isBoolCompatible($1.type) && isBoolCompatible($2.type))
              {
                $$.type = BOOL;
              }
              // If both operands are int compatible, resulting type is int
              else if(isIntCompatible($1.type) && isIntCompatible($2.type))
              {
                $$.type = INT;
              }
              // If one operand is float compatible, resulting type is float
              else if(isFloatCompatible($1.type) || isFloatCompatible($2.type))
              {
                $$.type = FLOAT;
              }
              else
              {
                $$.type = $1.type;
              }
              $$.numParams = $1.numParams;
              $$.returnType = $1.returnType;
            }
            }
            ;
N_ADD_OP_LIST   : N_ADD_OP N_TERM N_ADD_OP_LIST
            {
            printRule("ADD_OP_LIST", "ADD_OP TERM ADD_OP_LIST");
            // Argument must be int, float, or bool compatible
            if(!isIntOrFloatOrBoolCompatible($2.type))
            {
              semanticError(2, MUST_BE_INT_FLOAT_BOOL);
            }
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            // If an arithmetic operator is used, resulting type is int or float
            if($1 == ARITHMETIC_OP)
            {
              if($3.type == NOT_APPLICABLE)
              {
                $$.type = $2.type;
              }
              else
              {
                // If both operands are int compatible, resulting type is int
                if(isIntCompatible($2.type) && isIntCompatible($3.type))
                {
                  $$.type = INT;
                }
                // If one operand is float compatible, resulting type is float
                else if(isFloatCompatible($2.type) || isFloatCompatible($3.type))
                {
                  $$.type = FLOAT;
                }
                else
                {
                  $$.type = $2.type;
                }
              }
            }
            // If a logical operator is used, resulting type is bool
            else if($1 == LOGICAL_OP)
            {
              $$.type = BOOL;
            }
            }
            | /* epsilon */
            {
            printRule("ADD_OP_LIST", "epsilon");
            $$.type = NOT_APPLICABLE;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            ;
N_TERM      : N_FACTOR N_MULT_OP_LIST
            {
            printRule("TERM", "FACTOR MULT_OP_LIST");
            if($2.type == NOT_APPLICABLE)
            {
              $$.type = $1.type;
              $$.numParams = $1.numParams;
              $$.returnType = $1.returnType;
            }
            else
            {
              // Both arguments must be int, float, or bool compatible
              if(!isIntOrFloatOrBoolCompatible($1.type))
              {
                semanticError(1, MUST_BE_INT_FLOAT_BOOL);
              }
              if(!isIntOrFloatOrBoolCompatible($2.type))
              {
                semanticError(2, MUST_BE_INT_FLOAT_BOOL);
              }
              // If both operands are bool compatible, resulting type is bool
              if(isBoolCompatible($1.type) && isBoolCompatible($2.type))
              {
                $$.type = BOOL;
              }
              // If both operands are int compatible, resulting type is int
              else if(isIntCompatible($1.type) && isIntCompatible($2.type))
              {
                $$.type = INT;
              }
              // If one operand is float compatible, resulting type is float
              else if(isFloatCompatible($1.type) || isFloatCompatible($2.type))
              {
                $$.type = FLOAT;
              }
              else
              {
                $$.type = $1.type;
              }
              $$.numParams = NOT_APPLICABLE;
              $$.returnType = NOT_APPLICABLE;
            }
            }
            ;
N_MULT_OP_LIST  : N_MULT_OP N_FACTOR N_MULT_OP_LIST
            {
            printRule("MULT_OP_LIST", "MULT_OP FACTOR MULT_OP_LIST");
            // Argument must be int, float, or bool compatible
            if(!isIntOrFloatOrBoolCompatible($2.type))
            {
              semanticError(2, MUST_BE_INT_FLOAT_BOOL);
            }
            if($3.type == NOT_APPLICABLE)
            {
              $$.type = $2.type;
            }
            else
            {
            // If an arithmetic operator is used, resulting type is int or float
            if($1 == ARITHMETIC_OP)
            {
                // If both operands are int compatible, resulting type is int
                if(isIntCompatible($2.type) && isIntCompatible($3.type))
                {
                  $$.type = INT;
                }
                // If one operand is float compatible, resulting type is float
                else if(isFloatCompatible($2.type) || isFloatCompatible($3.type))
                {
                  $$.type = FLOAT;
                }
                else
                {
                  $$.type = $2.type;
                }
              }
            // If a logical operator is used, resulting type is bool
            else if($1 == LOGICAL_OP)
            {
              $$.type = BOOL;
            }
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            }
            | /* epsilon */
            {
            printRule("MULT_OP_LIST", "epsilon");
            $$.type = NOT_APPLICABLE;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            ;
N_FACTOR    : N_VAR
            {
            printRule("FACTOR", "VAR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_CONST
            {
            printRule("FACTOR", "CONST");
            $$.type = $1.type;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            | T_LPAREN N_EXPR T_RPAREN
            {
            printRule("FACTOR", "( EXPR )");
            $$.type = $2.type;
            $$.numParams = $2.numParams;
            $$.returnType = $2.returnType;
            }
            | T_NOT N_FACTOR
            {
            printRule("FACTOR", "! FACTOR");
            $$.type = $2.type;
            $$.numParams = $2.numParams;
            $$.returnType = $2.returnType;
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
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            | N_SINGLE_ELEMENT
            {
            printRule("VAR", "SINGLE_ELEMENT");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            }
            ;
N_SINGLE_ELEMENT    : T_IDENT
            T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
            {
            printRule("SINGLE_ELEMENT", "IDENT [[ EXPR ]]");
            string lexeme = string($1);
            TYPE_INFO temp = findEntryInAnyScope(lexeme);
            // T_IDENT must already exist in some ST
            if(temp.type == UNDEFINED)
            {
              semanticError(-1, UNDEFINED_IDENT);
              exit(1);
            }
            else if(temp.type != LIST)
            {
              // Cannot index into a variable that is not a list
              semanticError(1, MUST_BE_LIST);
            }
            $$.type = INT_OR_STR_OR_FLOAT_OR_BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            }
            ;
N_ENTIRE_VAR    : T_IDENT
            {
            printRule("ENTIRE_VAR", "IDENT");
            string lexeme = string($1);
            TYPE_INFO temp = findEntryInAnyScope(lexeme);
            // T_IDENT must already exist in some ST
            if(temp.type == UNDEFINED)
            {
              semanticError(-1, UNDEFINED_IDENT);
              exit(1);
            }
            $$.type = temp.type;
            $$.numParams = temp.numParams;
            $$.returnType = temp.returnType;
            $$.isParam = temp.isParam;
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
  TYPE_INFO temp2;
  temp2.type = UNDEFINED;
  temp2.numParams = NOT_APPLICABLE;
  temp2.returnType = NOT_APPLICABLE;
  temp2.isParam = false;
  if(scopeStack.empty()) return(temp2);
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

void semanticError(const int argNum, const int errNum)
{
  string errorMsg;
  int errNo = errNum;

  if((errNum < 0) || (errNum > NUM_ERR_MESSAGES+200-1))
  {
    errNo = DEFAULT_ERROR;
  }
  if(argNum > 0)
  {
    errorMsg = "Arg " + to_string(argNum) + " ";
  }
  else
  {
    errorMsg = "";
  }
  errorMsg += ERR_MSG[errNo-200];
  yyerror(errorMsg.c_str());
}

bool isIntCompatible(const int typeval)
{
  if(typeval == INT || typeval == BOOL || typeval == INT_OR_STR_OR_FLOAT_OR_BOOL
    || typeval == INT_OR_STR || typeval == INT_OR_BOOL || typeval == INT_OR_FLOAT
    || typeval == STR_OR_BOOL || typeval == BOOL_OR_FLOAT || typeval == LIST_OR_INT
    || typeval == LIST_OR_BOOL || typeval == INT_OR_STR_OR_BOOL
    || typeval == INT_OR_STR_OR_FLOAT || typeval == INT_OR_BOOL_OR_FLOAT
    || typeval == STR_OR_BOOL_OR_FLOAT || typeval == LIST_OR_INT_OR_STR
    || typeval == LIST_OR_INT_OR_BOOL || typeval == LIST_OR_INT_OR_FLOAT
    || typeval == LIST_OR_STR_OR_BOOL || typeval == LIST_OR_BOOL_OR_FLOAT
    || typeval == LIST_OR_FLOAT_OR_BOOL_OR_STR || typeval == LIST_OR_BOOL_OR_STR_OR_INT
    || typeval == LIST_OR_FLOAT_OR_STR_OR_INT || typeval == INT_OR_BOOL_OR_FLOAT_OR_LIST
    || typeval == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST)
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool isFloatCompatible(const int typeval)
{
  if(typeval == FLOAT || typeval == INT_OR_STR_OR_FLOAT_OR_BOOL || typeval == INT_OR_FLOAT
    || typeval == STR_OR_FLOAT || typeval == BOOL_OR_FLOAT || typeval == LIST_OR_FLOAT
    || typeval == INT_OR_STR_OR_FLOAT || typeval == INT_OR_BOOL_OR_FLOAT 
    || typeval == STR_OR_BOOL_OR_FLOAT || typeval == LIST_OR_INT_OR_FLOAT
    || typeval == LIST_OR_STR_OR_FLOAT || typeval == LIST_OR_BOOL_OR_FLOAT
    || typeval == LIST_OR_FLOAT_OR_BOOL_OR_STR || typeval == LIST_OR_FLOAT_OR_STR_OR_INT 
    || typeval == INT_OR_BOOL_OR_FLOAT_OR_LIST || typeval == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST)
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool isBoolCompatible(const int typeval)
{
  if(typeval == BOOL || typeval == INT_OR_STR_OR_FLOAT_OR_BOOL || typeval == INT_OR_BOOL
    || typeval == STR_OR_BOOL || typeval == BOOL_OR_FLOAT || typeval == LIST_OR_BOOL
    || typeval == INT_OR_STR_OR_BOOL || typeval == INT_OR_BOOL_OR_FLOAT
    || typeval == STR_OR_BOOL_OR_FLOAT || typeval == LIST_OR_INT_OR_BOOL
    || typeval == LIST_OR_STR_OR_BOOL || typeval == LIST_OR_BOOL_OR_FLOAT
    || typeval == LIST_OR_FLOAT_OR_BOOL_OR_STR || typeval == LIST_OR_BOOL_OR_STR_OR_INT
    || typeval == INT_OR_BOOL_OR_FLOAT_OR_LIST || typeval == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST)
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool isStrCompatible(const int typeval)
{
  if(typeval == STR || typeval == INT_OR_STR_OR_FLOAT_OR_BOOL || typeval == INT_OR_STR
    || typeval == STR_OR_BOOL || typeval == STR_OR_FLOAT || typeval == LIST_OR_STR
    || typeval == INT_OR_STR_OR_BOOL || typeval == INT_OR_STR_OR_FLOAT
    || typeval == STR_OR_BOOL_OR_FLOAT || typeval == LIST_OR_INT_OR_STR
    || typeval == LIST_OR_STR_OR_BOOL || typeval == LIST_OR_FLOAT_OR_BOOL_OR_STR
    || typeval == LIST_OR_BOOL_OR_STR_OR_INT || typeval == LIST_OR_FLOAT_OR_STR_OR_INT
    || typeval == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST)
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool isListCompatible(const int typeval)
{
  if(typeval == LIST || typeval == LIST_OR_INT || typeval == LIST_OR_STR
    || typeval == LIST_OR_BOOL || typeval == LIST_OR_FLOAT || typeval == LIST_OR_INT_OR_STR
    || typeval == LIST_OR_INT_OR_BOOL || typeval == LIST_OR_INT_OR_FLOAT
    || typeval == LIST_OR_STR_OR_BOOL || typeval == LIST_OR_STR_OR_FLOAT
    || typeval == LIST_OR_BOOL_OR_FLOAT || typeval == LIST_OR_FLOAT_OR_BOOL_OR_STR
    || typeval == LIST_OR_BOOL_OR_STR_OR_INT || typeval == LIST_OR_FLOAT_OR_STR_OR_INT
    || typeval == INT_OR_BOOL_OR_FLOAT_OR_LIST || typeval == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST)
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool isIntOrFloatOrBoolCompatible(const int typeval)
{
  if(isIntCompatible(typeval) || isFloatCompatible(typeval) || isBoolCompatible(typeval))
  {
    return true;
  }
  else
  {
    return false;
  }
}

bool isIntOrStrOrFloatOrBoolCompatible(const int typeval)
{
  if(isIntOrFloatOrBoolCompatible(typeval) || isStrCompatible(typeval))
  {
    return true;
  }
  else
  {
    return false;
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
