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
#define INT_OR_STR_OR_FLOAT_OR_BOOL 7


typedef struct
{
  int type; // One of the type codes

  // Only applicable if type is FUNCTION
  int numParams;
  int returnType;
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
  }

  SYMBOL_TABLE_ENTRY(const string theName, const TYPE_INFO theType)
  {
    name = theName;
    typeInfo.type = theType.type;
    typeInfo.numParams = theType.numParams;
    typeInfo.returnType = theType.returnType;
  }

  // Accessors
  string getName() const { return name; }
  TYPE_INFO getTypeInfo() const { return typeInfo; }
};

#endif  // SYMBOL_TABLE_ENTRY_H
