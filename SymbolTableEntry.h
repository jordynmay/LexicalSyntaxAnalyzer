#ifndef SYMBOL_TABLE_ENTRY_H
#define SYMBOL_TABLE_ENTRY_H

#include <string>
using namespace std;

// Type codes
#define NOT_APPLICABLE -3
#define GOES_TO_EPSILON -2
#define UNDEFINED -1
#define NULL_TYPE 0
#define INT 1
#define STR 2
#define BOOL 3
#define FLOAT 4
#define LIST 5
#define FUNCTION 6
#define INT_OR_STR 7
#define INT_OR_BOOL 8
#define INT_OR_FLOAT 9
#define STR_OR_BOOL 10
#define STR_OR_FLOAT 11
#define BOOL_OR_FLOAT 12
#define LIST_OR_INT 13
#define LIST_OR_STR 14
#define LIST_OR_BOOL 15
#define LIST_OR_FLOAT 16
#define INT_OR_STR_OR_BOOL 17
#define INT_OR_STR_OR_FLOAT 18
#define INT_OR_BOOL_OR_FLOAT 19
#define STR_OR_BOOL_OR_FLOAT 20
#define LIST_OR_INT_OR_STR 21
#define LIST_OR_INT_OR_BOOL 22
#define LIST_OR_INT_OR_FLOAT 23
#define LIST_OR_STR_OR_BOOL 24
#define LIST_OR_STR_OR_FLOAT 25
#define LIST_OR_BOOL_OR_FLOAT 26
#define INT_OR_STR_OR_FLOAT_OR_BOOL 27
#define LIST_OR_FLOAT_OR_BOOL_OR_STR 28
#define LIST_OR_BOOL_OR_STR_OR_INT 29
#define LIST_OR_FLOAT_OR_STR_OR_INT 30
#define INT_OR_BOOL_OR_FLOAT_OR_LIST 31
#define INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST 32


typedef struct
{
  int type; // One of the type codes

  // Only applicable if type is FUNCTION
  int numParams;
  int returnType;

  int isParam;
} TYPE_INFO;


class SYMBOL_TABLE_ENTRY
{
private:
  // Member variables
  string name;
  TYPE_INFO typeInfo;

public:
  // Constructors
  SYMBOL_TABLE_ENTRY( )
  {
    name = "";
    typeInfo.type = UNDEFINED;
    typeInfo.numParams = UNDEFINED;
    typeInfo.returnType = UNDEFINED;
    typeInfo.isParam = false;
  }

  SYMBOL_TABLE_ENTRY(const string theName, const TYPE_INFO theType)
  {
    name = theName;
    typeInfo.type = theType.type;
    typeInfo.numParams = theType.numParams;
    typeInfo.returnType = theType.returnType;
    typeInfo.isParam = theType.isParam;
  }

  // Accessors
  string getName() const { return name; }
  TYPE_INFO getTypeInfo() const { return typeInfo; }
};

#endif  // SYMBOL_TABLE_ENTRY_H
