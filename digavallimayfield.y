%{
#include <stdio.h>
#include <stack>
#include <ctype.h>
#include <typeinfo>
#include "SymbolTable.h"
#include <iostream>
#include <stdlib.h>
#include <cstring>
#include <math.h>

// Used to distinguish whether an operator is arithmetic, logical, or relational
// Helps to determine what type the operands should be
#define ARITHMETIC_OP 101
#define LOGICAL_OP 102
#define RELATIONAL_OP 103

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
#define SUBSCRIPT_OOB 214
#define ATTEMPT_DIV_BY_0 215
#define DEFAULT_ERROR 216

const int NUM_ERR_MESSAGES = 17;
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
  "Subscript out of bounds",
  "Attempted division by zero",
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
bool relOpCompare(float valOne, float valTwo, string relOper);
string convertToString(char* a, int size);
void printListFnct(vector<LIST_ENTRY>* listEntries);
bool shouldBeBool(char oper[256]);
template<typename T, typename U>
float ArithFnct(T t1, U t2, char oper[256]);
float MiddleFnct(TYPE_INFO t1, TYPE_INFO t2, char oper[256]);

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
  bool flag = false;
  bool boolean_val;
  int num;
  float float_num;
  TYPE_INFO typeInfo;
  ARITHLOGREL_OP arithlogrelOp;
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
%type <typeInfo> N_QUIT_EXPR N_PARAM_LIST N_PARAMS N_CONST_LIST
%type <typeInfo> N_VAR N_ENTIRE_VAR N_SINGLE_ELEMENT
%type <typeInfo> N_SIMPLE_ARITHLOGIC N_ADD_OP_LIST
%type <typeInfo> N_TERM N_FACTOR N_MULT_OP_LIST
%type <typeInfo> N_INDEX N_COND_IF N_THEN_EXPR
%type <arithlogrelOp> N_ADD_OP N_MULT_OP N_REL_OP
%type <num> T_INTCONST
%type <text> T_STRCONST
%type <float_num> T_FLOATCONST

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
			printf("\n---- Completed parsing ----\n");
      printf("\nValue of the expression is: ");
      // Outputs value of the last expression in the input file
      switch($1.type)
      {
        case(INT):
          cout << $1.int_val;
          break;
        case(STR):
          cout << $1.str_val;
          break;
        case(BOOL):
          if($1.bool_val)
            cout << "TRUE";
          else
            cout << "FALSE";
          break;
        case(FLOAT):
          printf("%.2f", $1.float_val);
          break;
        case(NULL_TYPE):
          cout << "NULL";
          break;
        case(LIST):
          printListFnct($1.list_val);
          break;
        case(NOT_APPLICABLE):
          cout << "NOT_APPLICABLE";
          break;
        case(GOES_TO_EPSILON):
          cout << "GOES_TO_EPSILON";
          break;
        case(UNDEFINED):
          cout << "UNDEFINED";
          break;
        default:
          cout << "DEFAULT";
          break;
			}
      }
			;
N_EXPR      : N_IF_EXPR
            {
            printRule("EXPR", "IF_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_WHILE_EXPR
            {
            printRule("EXPR", "WHILE_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_FOR_EXPR
            {
            printRule("EXPR", "FOR_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_COMPOUND_EXPR
            {
            printRule("EXPR", "COMPOUND_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_ARITHLOGIC_EXPR
            {
            printRule("EXPR", "ARITHLOGIC_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_ASSIGNMENT_EXPR
            {
            printRule("EXPR", "ASSIGNMENT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_OUTPUT_EXPR
            {
            printRule("EXPR", "OUTPUT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_INPUT_EXPR
            {
            printRule("EXPR", "INPUT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_LIST_EXPR
            {
            printRule("EXPR", "LIST_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_FUNCTION_DEF
            {
            printRule("EXPR", "FUNCTION_DEF");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_FUNCTION_CALL
            {
            printRule("EXPR", "FUNCTION_CALL");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_QUIT_EXPR
            {
            printRule("EXPR", "QUIT_EXPR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            ;
N_CONST     : T_INTCONST
            {
            printRule("CONST", "INTCONST");
            $$.type = INT;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = 0;
            $$.int_val = $1;
            strcpy($$.str_val, "");
            $$.bool_val = false;
            $$.float_val = 0;
            }
            | T_STRCONST
            {
            printRule("CONST", "STRCONST");
            $$.type = STR;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = 0;
            $$.int_val = 0;
            strcpy($$.str_val, string($1).c_str());
            $$.bool_val = false;
            $$.float_val = 0;
            }
            | T_FLOATCONST
            {
            printRule("CONST", "FLOATCONST");
            $$.type = FLOAT;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = 0;
            $$.int_val = 0;
            strcpy($$.str_val, "");
            $$.bool_val = false;
            $$.float_val = $1;
            }
            | T_TRUE
            {
            printRule("CONST", "TRUE");
            $$.type = BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = 0;
            $$.int_val = 0;
            strcpy($$.str_val, "");
            $$.bool_val = true;
            $$.float_val = 0;
            }
            | T_FALSE
            {
            printRule("CONST", "FALSE");
            $$.type = BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = 0;
            $$.int_val = 0;
            strcpy($$.str_val, "");
            $$.bool_val = false;
            $$.float_val = 0;
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
              $$.null_val = $2.null_val;
              $$.int_val = $2.int_val;
              strcpy($$.str_val, $2.str_val);
              $$.bool_val = $2.bool_val;
              $$.float_val = $2.float_val;
              $$.list_val = $2.list_val;
            }
            else
            {
              $$.type = $3.type;
              $$.numParams = $3.numParams;
              $$.returnType = $3.returnType;
              $$.null_val = $3.null_val;
              $$.int_val = $3.int_val;
              strcpy($$.str_val, $3.str_val);
              $$.bool_val = $3.bool_val;
              $$.float_val = $3.float_val;
              $$.list_val = $3.list_val;
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
              $$.null_val = $2.null_val;
              $$.int_val = $2.int_val;
              strcpy($$.str_val, $2.str_val);
              $$.bool_val = $2.bool_val;
              $$.float_val = $2.float_val;
              $$.list_val = $2.list_val;
            }
            else
            {
              $$.type = $3.type;
              $$.numParams = $3.numParams;
              $$.returnType = $3.returnType;
              $$.null_val = $3.null_val;
              $$.int_val = $3.int_val;
              strcpy($$.str_val, $3.str_val);
              $$.bool_val = $3.bool_val;
              $$.float_val = $3.float_val;
              $$.list_val = $3.list_val;
            }
            }
            | /* epsilon */
            {
            printRule("EXPR_LIST", "epsilon");
            $$.type = GOES_TO_EPSILON;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.isParam = false;
            }
            ;
N_IF_EXPR   : N_COND_IF T_RPAREN N_THEN_EXPR
            {
            printRule("IF_EXPR", "COND_IF ) THEN_EXPR");
            // The then expression cannot be a function
            if($3.type == FUNCTION)
            {
              semanticError(2, CANNOT_BE_FNCT);
            }
            bool if_value = false;
            if($1.type == INT)
            {
              if_value = ($1.int_val == 0 ? false : true);
            }
            else if($1.type == FLOAT)
            {
              if_value = ($1.float_val == 0 ? false : true);
            }
            else if($1.type == BOOL)
            {
              if_value = ($1.bool_val == 0 ? false : true);
            }
            // If the conditional is true, set according to then expr
            if(if_value)
            {
              $$.type = $3.type;
              $$.null_val = $3.null_val;
              $$.int_val = $3.int_val;
              strcpy($$.str_val, $3.str_val);
              $$.bool_val = $3.bool_val;
              $$.float_val = $3.float_val;
              $$.list_val = $3.list_val;
            }
            // Else, set everything to null since there is no else statement
            else
            {
              $$.type = NULL_TYPE;
              $$.null_val = 1;
              $$.int_val = 0;
              strcpy($$.str_val, "");
              $$.bool_val = false;
              $$.float_val = 0;
            }
            }
            | N_COND_IF T_RPAREN N_THEN_EXPR T_ELSE N_EXPR
            {
            printRule("IF_EXPR", "COND_IF ) THEN_EXPR ELSE EXPR");
            // The then or else expressions cannot be functions
            if($3.type == FUNCTION)
            {
              semanticError(2, CANNOT_BE_FNCT);
            }
            if($5.type == FUNCTION)
            {
              semanticError(3, CANNOT_BE_FNCT);
            }
            bool if_value = false;
            if($1.type == INT)
            {
              if_value = ($1.int_val == 0 ? false : true);
            }
            else if($1.type == FLOAT)
            {
              if_value = ($1.float_val == 0 ? false : true);
            }
            else if($1.type == BOOL)
            {
              if_value = ($1.bool_val == 0 ? false : true);
            }
            // If the conditional is true, set according to then expr
            if(if_value)
            {
              $$.type = $3.type;
              $$.null_val = $3.null_val;
              $$.int_val = $3.int_val;
              strcpy($$.str_val, $3.str_val);
              $$.bool_val = $3.bool_val;
              $$.float_val = $3.float_val;
              $$.list_val = $3.list_val;
            }
            // Else, set according to the else expr
            else
            {
              $$.type = $5.type;
              $$.null_val = $5.null_val;
              $$.int_val = $5.int_val;
              strcpy($$.str_val, $5.str_val);
              $$.bool_val = $5.bool_val;
              $$.float_val = $5.float_val;
              $$.list_val = $5.list_val;
            }
            }
            ;
N_COND_IF   : T_IF T_LPAREN N_EXPR
            {
            printRule("COND_IF", "IF ( EXPR");
            $$.type = $3.type;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;

            $$.null_val = $3.null_val;
            $$.int_val = $3.int_val;
            strcpy($$.str_val, $3.str_val);
            $$.bool_val = $3.bool_val;
            $$.float_val = $3.float_val;
            $$.list_val = $3.list_val;

            if($$.type == FUNCTION || $$.type == LIST 
            || $$.type == NULL_TYPE || $$.type == STR)
            {
              semanticError(1, CANNOT_BE_FNCT_NULL_LIST_STR);
            }
            }
            ;
N_THEN_EXPR : N_EXPR
            {
            printRule("THEN_EXPR", "EXPR");
            if($1.type == FUNCTION)
              semanticError(2, CANNOT_BE_FNCT);
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
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
            $$.numParams = $6.numParams;
            $$.returnType = $6.returnType;
            $$.isParam = $6.isParam;
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
                semanticError(1, CANNOT_BE_FNCT_NULL_LIST);
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
            $$.isParam = false;
            $$.list_val = $3.list_val;
            }
            ;
N_CONST_LIST    : N_CONST T_COMMA N_CONST_LIST
            {
            printRule("CONST_LIST", "CONST, CONST_LIST");
            LIST_ENTRY push_this;
            push_this.type = $1.type;
            push_this.int_val = $1.int_val;
            push_this.str_val = $1.str_val;
            push_this.float_val = $1.float_val;
            push_this.bool_val = $1.bool_val;
            $$.list_val = new vector<LIST_ENTRY>();
            (*$$.list_val).push_back(push_this);

            // If there are more elements in list
            if($3.type != GOES_TO_EPSILON)
            {
            (*$$.list_val).insert($$.list_val->end(), $3.list_val->begin(),
              $3.list_val->end());
            }
            }
            | N_CONST
            {
            printRule("CONST_LIST", "CONST");
            LIST_ENTRY push_this;
            push_this.type = $1.type;
            push_this.int_val = $1.int_val;
            push_this.str_val = $1.str_val;
            push_this.float_val = $1.float_val;
            push_this.bool_val = $1.bool_val;
            $$.list_val = new vector<LIST_ENTRY>();
            (*$$.list_val).push_back(push_this);
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
            bool checkBounds = false;
            int listSize = 0;
            // Store list size if temp is a list
            if(temp.type == LIST)
            {
              checkBounds = true;
              listSize = temp.list_val->size();
            }
            if($2.type != GOES_TO_EPSILON && !isListCompatible(temp.type)) //!!!
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
            }
            if($2.type != GOES_TO_EPSILON && isListCompatible($5.type))//!!!
            {
              // No lists of lists allowed
              semanticError(1, CANNOT_BE_LIST);
            }
            temp.type = $5.type;
            temp.numParams = $5.numParams;
            temp.returnType = $5.returnType;
            temp.isParam = $5.isParam;
            temp.null_val = $5.null_val;
            temp.int_val = $5.int_val;
            strcpy(temp.str_val, $5.str_val);
            temp.bool_val = $5.bool_val;
            temp.float_val = $5.float_val;
            temp.list_val = $5.list_val;

            // If no indexing, just plain variable
            if($2.type == GOES_TO_EPSILON)
            {
              scopeStack.top().modifyEntry(SYMBOL_TABLE_ENTRY(lexeme, temp));
              TYPE_INFO test = findEntryInAnyScope(lexeme);
            }
            // Else if indexing and T_IDENT is a list
            else
            {
              int idx = 1;
              switch($2.type)
              {
                case(NULL_TYPE):
                  idx = $2.null_val;
                  break;
                case(INT):
                  idx = $2.int_val;
                  break;
                case(STR):
                  idx = stoi($2.str_val);
                  break;
                case(BOOL):
                  idx = static_cast<int>($2.bool_val);
                  break;
                case(FLOAT):
                  idx = static_cast<int>($2.float_val);
                  break;
              }
              // Must check the bounds to ensure proper indexing
              if(checkBounds)
              {
                if(idx < 1 || idx > listSize)
                {
                  semanticError(-1, SUBSCRIPT_OOB);
                }
              }
              scopeStack.top().modifyListEntry(SYMBOL_TABLE_ENTRY(lexeme, temp), idx-1);
            }
            $$.type = $5.type;
            $$.numParams = $5.numParams;
            $$.returnType = $5.returnType;
            $$.isParam = $5.isParam;
            $$.null_val = $5.null_val;
            $$.int_val = $5.int_val;
            strcpy($$.str_val, $5.str_val);
            $$.bool_val = $5.bool_val;
            $$.float_val = $5.float_val;
            $$.list_val = $5.list_val;
            }
            ;
N_INDEX     : T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
            {
            printRule("INDEX", "[[ EXPR ]]");

            if($3.type != INT)
            {
              semanticError(1, MUST_BE_INT);
            }

            $$.type = $3.type;
            $$.null_val = $3.null_val;
            $$.int_val = $3.int_val;
            strcpy($$.str_val, $3.str_val);
            $$.bool_val = $3.bool_val;
            $$.float_val = $3.float_val;
            }
            | /* epsilon */
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
            $$.null_val = 1;
            $$.int_val = 0;
            strcpy($$.str_val, "");
            $$.bool_val = false;
            $$.float_val = 0;
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
            // Switch case print for different types
            switch($3.type)
            {
              case(INT):
              {
                printf("%d\n", $3.int_val);
                break;
              }
              case(STR):
              {
                printf("%s\n", $3.str_val);
                break;
              }
              case(BOOL):
              {
                printf($3.bool_val ? "TRUE\n" : "FALSE\n");
                break;
              }
              case(FLOAT):
              {
                printf("%.2f\n", $3.float_val);
                break;
              }
              case(LIST):
              {
                printListFnct($3.list_val);
                break;
              }
            }
            // PRINT takes on the type of whatever it prints
            $$.type = $3.type;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;
            $$.null_val = $3.null_val;
            $$.int_val = $3.int_val;
            strcpy($$.str_val, $3.str_val);
            $$.bool_val = $3.bool_val;
            $$.float_val = $3.float_val;
            $$.list_val = $3.list_val;
            }
            | T_CAT T_LPAREN N_EXPR T_RPAREN
            {
            printRule("OUTPUT_EXPR", "CAT ( EXPR )");
            if($3.type == FUNCTION || $3.type == NULL_TYPE)
            {
              semanticError(1, CANNOT_BE_FNCT_NULL);
            }
            // Switch case print for different types
            switch($3.type)
            {
              case(INT):
              {
                printf("%d\n", $3.int_val);
                break;
              }
              case(STR):
              {
                printf("%s\n", $3.str_val);
                break;
              }
              case(BOOL):
              {
                printf($3.bool_val ? "TRUE\n" : "FALSE\n");
                break;
              }
              case(FLOAT):
              {
                printf("%.2f\n", $3.float_val);
                break;
              }
              case(LIST):
              {
                printListFnct($3.list_val);
                break;
              }
            }
            // CAT returns null
            $$.type = NULL_TYPE;
            $$.numParams = $3.numParams;
            $$.returnType = $3.returnType;
            $$.null_val = 1;
            $$.int_val = 0;
            strcpy($$.str_val, "");
            $$.bool_val = false;
            $$.float_val = 0;
            }
            ;
N_INPUT_EXPR    : T_READ T_LPAREN T_RPAREN
            {
            printRule("INPUT_EXPR", "READ ( )");
            string read_in;
            getline(cin, read_in); // Reads in the input
            // If there are no signs or decimal points, the value is a string
            if(read_in[0] != '+' && read_in[0] != '-' && !isdigit(read_in[0]))
            {
                $$.type = STR;
                $$.null_val = 0;
                $$.int_val = 0;
                strcpy($$.str_val, read_in.c_str());
                $$.bool_val = false;
                $$.float_val = 0;
            }
            // Else if there is a decimal point, it is a float
            else if(read_in.find('.') != std::string::npos)
            {
                $$.type = FLOAT;
                $$.null_val = 0;
                $$.int_val = 0;
                strcpy($$.str_val, "");
                $$.bool_val = false;
                $$.float_val = stof(read_in);
            }
            // Else if there is no decimal point, it is an integer
            else
            {
                $$.type = INT;
                $$.null_val = 0;
                $$.int_val = stoi(read_in);
                strcpy($$.str_val, "");
                $$.bool_val = false;
                $$.float_val = 0;
            }
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.isParam = false;
            }
            ;
N_FUNCTION_DEF  : T_FUNCTION
            {
              printRule("FUNCTION_DEF", "FUNCTION ( PARAM_LIST ) COMPOUND_EXPR");
              beginScope();
            }
            T_LPAREN N_PARAM_LIST
            {
              // Number of entries in N_PARAM_LIST aka numParams for the function
              $<num>$ = scopeStack.top().getNumParams();
            }
            T_RPAREN N_COMPOUND_EXPR
            {
            endScope();
            if($7.type == FUNCTION)
            {
              semanticError(2, CANNOT_BE_FNCT);
            }
            $$.type = FUNCTION;
            $$.numParams = $<num>5;
            $$.returnType = $7.type;
            $$.isParam = false;
            $<num>5 = 0;
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
N_NO_PARAMS : /* epsilon */
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
              // Cannot pass the same parameter twice in a function
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
              // Cannot pass the same parameter twice in a function
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
            // Check to make sure there are the correct number of parameters
            if(numArgs > temp.numParams)
            {
              semanticError(-1, TOO_MANY_PARAMS);
            }
            else if(numArgs < temp.numParams)
            {
              semanticError(-1, TOO_FEW_PARAMS);
            }
            // If a recursive call occurs
            if(temp.returnType == NOT_APPLICABLE || temp.returnType == UNDEFINED)
            {
              semanticError(-1, UNDEFINED_IDENT);
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
N_NO_ARGS   : /* epsilon */
            {
            printRule("NO_ARGS", "epsilon");
            }
            ;
N_ARGS      : N_EXPR
            {
            printRule("ARGS", "EXPR");
            numArgs++;
            if(!isIntCompatible($1.type))
            {
              semanticError(-1, PARAMS_MUST_BE_INT);
            }
            }
            | N_EXPR T_COMMA N_ARGS
            {
            printRule("ARGS", "EXPR, ARGS");
            numArgs++;
            if(!isIntCompatible($1.type))
            {
              semanticError(-1, PARAMS_MUST_BE_INT);
            }
            }
            ;
N_ARITHLOGIC_EXPR   : N_SIMPLE_ARITHLOGIC
            {
            printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_SIMPLE_ARITHLOGIC N_REL_OP N_SIMPLE_ARITHLOGIC
            {
            printRule("ARITHLOGIC_EXPR", "SIMPLE_ARITHLOGIC REL_OP SIMPLE_ARITHLOGIC");
            if($1.type != INT && $1.type != BOOL && $1.type != FLOAT)
            {
              semanticError(1, MUST_BE_INT);
            }
            else if($3.type != INT && $3.type != BOOL && $1.type != FLOAT)
            {
              semanticError(2, MUST_BE_INT);
            }
            float val_one = 0;
            // Sets value for the first simple arithlogic
            switch($1.type)
            {
              case(INT):
              {
                val_one = static_cast<float>($1.int_val);
                break;
              }
              case(BOOL):
              {
                val_one = static_cast<float>($1.bool_val);
                break;
              }
              case(FLOAT):
                val_one = $1.float_val;
            }
            float val_two = 0;
            // Sets value for the second simple arithlogic
            switch($3.type)
            {
              case(INT):
              {
                val_two = static_cast<float>($3.int_val);
                break;
              }
              case(BOOL):
              {
                val_two = static_cast<float>($3.bool_val);
                break;
              }
              case(FLOAT):
                val_two = $3.float_val;
            }
            string rel_oper = $2.op_str;
            bool final_val = relOpCompare(val_one, val_two, rel_oper);
            // Resulting type is a bool because a relational operator was used
            $$.type = BOOL;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = 0;
            $$.int_val = 0;
            strcpy($$.str_val, "");
            $$.bool_val = final_val;
            $$.float_val = 0;
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
              $$.int_val = $1.int_val;
              $$.float_val = $1.float_val;
              $$.bool_val = $1.bool_val;
              $$.list_val = $1.list_val;
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
              if(shouldBeBool($2.str_val))
              {
                $$.type = BOOL;
                $$.bool_val = static_cast<bool>(MiddleFnct($1, $2, $2.str_val));
              }
              else
              {
                if(isIntCompatible($1.type) && isIntCompatible($2.type))
                {
                  $$.type = INT;
                  $$.int_val = static_cast<int>(MiddleFnct($1, $2, $2.str_val));

                }
                // If one operand is float compatible, resulting type is float
                else if(isFloatCompatible($1.type) || isFloatCompatible($2.type))
                {
                  $$.type = FLOAT;
                  $$.float_val = MiddleFnct($1, $2, $2.str_val);
                }
                else
                {
                  $$.type = $1.type;
                  $$.int_val = 0;
                  $$.float_val = 0;
                  $$.bool_val = false;
                }
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
            strcpy($$.str_val, $1.op_str);
            // If no other add_op_list
            if($3.type == NOT_APPLICABLE)
            {
              $$.type = $2.type;
              $$.int_val = $2.int_val;
              $$.float_val = $2.float_val;
              $$.bool_val = $2.bool_val;
            }
            else
            {
              if(shouldBeBool($3.str_val))
              {
                $$.type = BOOL;
                $$.bool_val = static_cast<bool>(MiddleFnct($2, $3, $3.str_val));
              }
              else
              {
                if(isIntCompatible($2.type) && isIntCompatible($3.type))
                {
                  $$.type = INT;
                  $$.int_val = static_cast<int>(MiddleFnct($2, $3, $3.str_val));

                }
                // If one operand is float compatible, resulting type is float
                else if(isFloatCompatible($2.type) || isFloatCompatible($3.type))
                {
                  $$.type = FLOAT;
                  $$.float_val = MiddleFnct($2, $3, $3.str_val);
                }
                else
                {
                  $$.type = $2.type;
                  $$.int_val = 0;
                  $$.float_val = 0;
                  $$.bool_val = false;
                }
              }
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
              $$.int_val = $1.int_val;
              $$.float_val = $1.float_val;
              $$.bool_val = $1.bool_val;
              $$.list_val = $1.list_val;
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
              if(shouldBeBool($2.str_val))
              {
                $$.type = BOOL;
                $$.bool_val = static_cast<bool>(MiddleFnct($1, $2, $2.str_val));
              }
              else
              {
                if(isIntCompatible($1.type) && isIntCompatible($2.type))
                {
                  $$.type = INT;
                  $$.int_val = static_cast<int>(MiddleFnct($1, $2, $2.str_val));

                }
                // If one operand is float compatible, resulting type is float
                else if(isFloatCompatible($1.type) || isFloatCompatible($2.type))
                {
                  $$.type = FLOAT;
                  $$.float_val = MiddleFnct($1, $2, $2.str_val);
                }
                else
                {
                  $$.type = $1.type;
                  $$.int_val = 0;
                  $$.float_val = 0;
                  $$.bool_val = false;
                }
              }
              $$.numParams = $1.numParams;
              $$.returnType = $1.returnType;
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
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            strcpy($$.str_val, $1.op_str);
            // If no other mult_op_list
            if($3.type == NOT_APPLICABLE)
            {
              $$.type = $2.type;
              $$.int_val = $2.int_val;
              $$.float_val = $2.float_val;
              $$.bool_val = $2.bool_val;
            }
            else
            {
              if(shouldBeBool($3.str_val))
              {
                $$.type = BOOL;
                $$.bool_val = static_cast<bool>(MiddleFnct($2, $3, $3.str_val));
              }
              else
              {
                if(isIntCompatible($2.type) && isIntCompatible($3.type))
                {
                  $$.type = INT;
                  $$.int_val = static_cast<int>(MiddleFnct($2, $3, $3.str_val));

                }
                // If one operand is float compatible, resulting type is float
                else if(isFloatCompatible($2.type) || isFloatCompatible($3.type))
                {
                  $$.type = FLOAT;
                  $$.float_val = MiddleFnct($2, $3, $3.str_val);
                }
                else
                {
                  $$.type = $2.type;
                  $$.int_val = 0;
                  $$.float_val = 0;
                  $$.bool_val = false;
                }
              }
            }
            }
            | /* epsilon */
            {
            printRule("MULT_OP_LIST", "epsilon");
            $$.type = NOT_APPLICABLE;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.isParam = false;
            }
            ;
N_FACTOR    : N_VAR
            {
            printRule("FACTOR", "VAR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_CONST
            {
            printRule("FACTOR", "CONST");
            $$.type = $1.type;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | T_LPAREN N_EXPR T_RPAREN
            {
            printRule("FACTOR", "( EXPR )");
            $$.type = $2.type;
            $$.numParams = $2.numParams;
            $$.returnType = $2.returnType;
            $$.null_val = $2.null_val;
            $$.int_val = $2.int_val;
            strcpy($$.str_val, $2.str_val);
            $$.bool_val = $2.bool_val;
            $$.float_val = $2.float_val;
            $$.list_val = $2.list_val;
            }
            | T_NOT N_FACTOR
            {
            printRule("FACTOR", "! FACTOR");
            $$.type = BOOL;
            $$.numParams = $2.numParams;
            $$.returnType = $2.returnType;
            $$.null_val = !($2.null_val);
            if($2.type == INT)
              $$.bool_val = !($2.int_val);
            else if($2.type == FLOAT)
              $$.bool_val = !($2.float_val);
            else if($2.type == BOOL)
              $$.bool_val = !($2.bool_val);
            else if($2.type == STR)
              strcpy($$.str_val, "");
            else
              $$.bool_val = false;
            }
            ;
N_ADD_OP    : T_ADD
            {
            printRule("ADD_OP", "+");
            $$.number = ARITHMETIC_OP;
            strcpy($$.op_str, "+");
            }
            | T_SUB
            {
            printRule("ADD_OP", "-");
            $$.number = ARITHMETIC_OP;
            strcpy($$.op_str, "-");
            }
            | T_OR
            {
            printRule("ADD_OP", "|");
            $$.number = LOGICAL_OP;
            strcpy($$.op_str, "|");
            }
            ;
N_MULT_OP   : T_MULT
            {
            printRule("MULT_OP", "*");
            $$.number = ARITHMETIC_OP;
            strcpy($$.op_str, "*");
            }
            | T_DIV
            {
            printRule("MULT_OP", "/");
            $$.number = ARITHMETIC_OP;
            strcpy($$.op_str, "/");
            }
            | T_AND
            {
            printRule("MULT_OP", "&");
            $$.number = LOGICAL_OP;
            strcpy($$.op_str, "&");
            }
            | T_MOD
            {
            printRule("MULT_OP", "\%\%");
            $$.number = ARITHMETIC_OP;
            strcpy($$.op_str, "%%");
            }
            | T_POW
            {
            printRule("MULT_OP", "^");
            $$.number = ARITHMETIC_OP;
            strcpy($$.op_str, "^");
            }
            ;
N_REL_OP    : T_LT
            {
            printRule("REL_OP", "<");
            $$.number = RELATIONAL_OP;
            strcpy($$.op_str, "<");
            }
            | T_GT
            {
            printRule("REL_OP", ">");
            $$.number = RELATIONAL_OP;
            strcpy($$.op_str, ">");
            }
            | T_LE
            {
            printRule("REL_OP", "<=");
            $$.number = RELATIONAL_OP;
            strcpy($$.op_str, "<=");
            }
            | T_GE
            {
            printRule("REL_OP", ">=");
            $$.number = RELATIONAL_OP;
            strcpy($$.op_str, ">=");
            }
            | T_EQ
            {
            printRule("REL_OP", "==");
            $$.number = RELATIONAL_OP;
            strcpy($$.op_str, "==");
            }
            | T_NE
            {
            printRule("REL_OP", "!=");
            $$.number = RELATIONAL_OP;
            strcpy($$.op_str, "!=");
            }
            ;
N_VAR       : N_ENTIRE_VAR
            {
            printRule("VAR", "ENTIRE_VAR");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
            }
            | N_SINGLE_ELEMENT
            {
            printRule("VAR", "SINGLE_ELEMENT");
            $$.type = $1.type;
            $$.numParams = $1.numParams;
            $$.returnType = $1.returnType;
            $$.null_val = $1.null_val;
            $$.int_val = $1.int_val;
            strcpy($$.str_val, $1.str_val);
            $$.bool_val = $1.bool_val;
            $$.float_val = $1.float_val;
            $$.list_val = $1.list_val;
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
            else if(!isListCompatible(temp.type))
            {
              // Cannot index into a variable that is not a list
              semanticError(1, MUST_BE_LIST);
            }
            int idx = 1;
            switch($4.type)
            {
              case(NULL_TYPE):
                idx = $4.null_val;
                break;
              case(INT):
                idx = $4.int_val;
                break;
              case(STR):
              {
                string tempo = $4.str_val;
                idx = stoi(tempo);
                break;
              }
              case(BOOL):
                idx = static_cast<int>($4.bool_val);
                break;
              case(FLOAT):
                idx = static_cast<int>($4.float_val);
                break;
            }
            if(idx > temp.list_val->size() || idx < 1)
            {
              semanticError(-1,SUBSCRIPT_OOB);
            }
            vector<LIST_ENTRY>& vecRef = *temp.list_val;
            $$.type = vecRef[idx-1].type;
            $$.numParams = NOT_APPLICABLE;
            $$.returnType = NOT_APPLICABLE;
            $$.int_val = vecRef[idx-1].int_val;
            strcpy($$.str_val, vecRef[idx-1].str_val.c_str());
            $$.bool_val = vecRef[idx-1].bool_val;
            $$.float_val = vecRef[idx-1].float_val;
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
            $$.null_val = temp.null_val;
            $$.int_val = temp.int_val;
            strcpy($$.str_val, temp.str_val);
            $$.bool_val = temp.bool_val;
            $$.float_val = temp.float_val;
            $$.list_val = temp.list_val;
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

// Evaluates a boolean expression given a relational operator
bool relOpCompare(float valOne, float valTwo, string relOper)
{
  if(relOper == "<")
    return(valOne < valTwo);
  else if(relOper == ">")
    return(valOne > valTwo);
  else if(relOper == "<=")
    return(valOne <= valTwo);
  else if(relOper == ">=")
    return(valOne >= valTwo);
  else if(relOper == "==")
    return(valOne == valTwo);
  else if(relOper == "!=")
    return(valOne != valTwo);
  else
    return false;
}

// Converts char* to type string
string convertToString(char* a, int size)
{
  int i;
  string s = "";
  for(i=0; i<size; i++)
  {
    s = s+a[i];
  }
  return s;
}

// Prints all entries in a list
void printListFnct(vector<LIST_ENTRY>* listEntries)
{
  int listSize = (listEntries->size()) - 1;
  vector<LIST_ENTRY>& vecRef = *listEntries;
  cout << "( ";
  for(int i=0; i<=listSize; i++)
  {
    if(vecRef[i].type == INT)
      cout << vecRef[i].int_val;
    else if(vecRef[i].type == STR)
      cout << vecRef[i].str_val;
    else if(vecRef[i].type == BOOL)
      printf(vecRef[i].bool_val ? "TRUE" : "FALSE");
    else if(vecRef[i].type == FLOAT)
      printf("%.2f", vecRef[i].float_val);

    if(i != listEntries->size()-1)
      cout << " ";  
  }
  cout << " )" << endl;
}

// Returns if the resulting type of the larger expression should be boolean
//    Which is true if a relational or logical operator is used
bool shouldBeBool(char oper[256])
{
  if(strcmp(oper, "|") == 0 || strcmp(oper, "&") == 0 ||
    strcmp(oper, "<") == 0 || strcmp(oper, ">") == 0 ||
    strcmp(oper, "<=") == 0 || strcmp(oper, ">=") == 0 ||
    strcmp(oper, "==") == 0 || strcmp(oper, "!=") == 0)
  {
    return true;
  }
  else
    return false;
}

// Takes an operator and two variables of type int, float, or bool
// Computes the arithmetic result of the operation
template<typename T, typename U>
float ArithFnct(T t1, U t2, char oper[256])
{
  if(strcmp(oper, "+") == 0)
    return(t1+t2);
  else if(strcmp(oper, "-") == 0)
  {
    return(t1-t2);
  }
  else if(strcmp(oper, "*") == 0)
    return(t1*t2);
  else if(strcmp(oper, "/") == 0)
  {
    if(t2 == 0)
    {
      semanticError(-1, ATTEMPT_DIV_BY_0);
    }
    return(t1/t2);
  }
  else if(strcmp(oper, "%%") == 0)
    return(fmod(t1,t2));
  else if(strcmp(oper, "^") == 0)
    return(pow(t1, t2));
  else if(strcmp(oper, "|") == 0)
  {
    return(t1||t2);
  }
  else if(strcmp(oper, "&") == 0)
  {
    return(t1&&t2);
  }
  else if(strcmp(oper, "<") == 0)
    return(t1<t2);
  else if(strcmp(oper, ">") == 0)
    return(t1>t2);
  else if(strcmp(oper, "<=") == 0)
    return(t1<=t2);
  else if(strcmp(oper, ">=") == 0)
    return(t1>=t2);
  else if(strcmp(oper, "==") == 0)
    return(t1==t2);
  else if(strcmp(oper, "!=") == 0)
    return(t1!=t2); 
  return 0;
}

float MiddleFnct(TYPE_INFO t1, TYPE_INFO t2, char oper[256])
{
  float ret_val;

  if(t1.type == INT && t2.type == INT)
  {
    ret_val = ArithFnct(t1.int_val, t2.int_val, oper);
  }
  else if(t1.type == INT && t2.type == FLOAT)
    ret_val = ArithFnct(t1.int_val, t2.float_val, oper);
  else if(t1.type == INT && t2.type == BOOL)
    ret_val = ArithFnct(t1.int_val, t2.bool_val, oper);
  else if(t1.type == FLOAT && t2.type == INT)
    ret_val = ArithFnct(t1.float_val, t2.int_val, oper);
  else if(t1.type == FLOAT && t2.type == FLOAT)
    ret_val = ArithFnct(t1.float_val, t2.float_val, oper);
  else if(t1.type == FLOAT && t2.type == BOOL)
    ret_val = ArithFnct(t1.float_val, t2.bool_val, oper);
  else if(t1.type == BOOL && t2.type == INT)
    ret_val = ArithFnct(t1.bool_val, t2.int_val, oper);
  else if(t1.type == BOOL && t2.type == FLOAT)
    ret_val = ArithFnct(t1.bool_val, t2.float_val, oper);
  else if(t1.type == BOOL && t2.type == BOOL)
    ret_val = ArithFnct(t1.bool_val, t2.bool_val, oper);

  return(ret_val);
}

int main(int argc, char** argv) 
{
  beginScope();
  if(argc < 2)
  {
      printf("You must specify a file in the command line!\n");
      exit(1);
  }
  yyin = fopen(argv[1], "r");
  do 
  {
	yyparse();
  } while (!feof(yyin));

  //printf("%d lines processed\n", numLines);
  return(0);
}
