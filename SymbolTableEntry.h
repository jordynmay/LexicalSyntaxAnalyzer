#ifndef SYMBOL_TABLE_ENTRY_H
#define SYMBOL_TABLE_ENTRY_H

#include <string>
using namespace std;

// Type codes
const int NOT_APPLICABLE = -3;
const int GOES_TO_EPSILON = -2;
const int UNDEFINED = -1;
const int NULL_TYPE = 0b000000;
const int INT = 0b100000;
const int STR = 0b010000;
const int BOOL = 0b001000;
const int FLOAT = 0b000100;
const int LIST = 0b000010;
const int FUNCTION = 0b000001;
const int INT_OR_STR = 0b110000;
const int INT_OR_BOOL = 0b101000;
const int INT_OR_FLOAT = 0b100100;
const int STR_OR_BOOL = 0b011000;
const int STR_OR_FLOAT = 0b010100;
const int BOOL_OR_FLOAT = 0b001100;
const int LIST_OR_INT = 0b100010;
const int LIST_OR_STR = 0b010010;
const int LIST_OR_BOOL = 0b001010;
const int LIST_OR_FLOAT = 0b000110;
const int INT_OR_STR_OR_BOOL = 0b111000;
const int INT_OR_STR_OR_FLOAT = 0b110100;
const int INT_OR_BOOL_OR_FLOAT = 0b101100;
const int STR_OR_BOOL_OR_FLOAT = 0b011100;
const int LIST_OR_INT_OR_STR = 0b110010;
const int LIST_OR_INT_OR_BOOL = 0b101010;
const int LIST_OR_INT_OR_FLOAT = 0b100110;
const int LIST_OR_STR_OR_BOOL = 0b011010;
const int LIST_OR_STR_OR_FLOAT = 0b010110;
const int LIST_OR_BOOL_OR_FLOAT = 0b001110;
const int INT_OR_STR_OR_FLOAT_OR_BOOL = 0b111100;
const int LIST_OR_FLOAT_OR_BOOL_OR_STR = 0b011110;
const int LIST_OR_BOOL_OR_STR_OR_INT = 0b111010;
const int LIST_OR_FLOAT_OR_STR_OR_INT = 0b110110;
const int INT_OR_BOOL_OR_FLOAT_OR_LIST = 0b101110;
const int INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST = 0b111110;


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
