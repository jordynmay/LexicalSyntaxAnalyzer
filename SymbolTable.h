#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <iostream>
#include <map>
#include <string>
#include "SymbolTableEntry.h"
using namespace std;


class SYMBOL_TABLE
{
private:
  std::map<string, SYMBOL_TABLE_ENTRY> hashTable;

public:
  //Constructor
  SYMBOL_TABLE( ) { }

  // Add SYMBOL_TABLE_ENTRY x to this symbol table.
  // If successful, return true; otherwise, return false.
  bool addEntry(SYMBOL_TABLE_ENTRY x)
  {
    // Make sure there isn't already an entry with the same name
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(x.getName())) == hashTable.end())
    {
      hashTable.insert(make_pair(x.getName(), x));
      return(true);
    }
    else return(false);
  }

  // Modifies a variable in the stack
  void modifyEntry(SYMBOL_TABLE_ENTRY x)
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    // If ident is not found in the stack
    if ((itr = hashTable.find(x.getName())) == hashTable.end())
    {
      return;
    }
    else
    {
      // Erase the old element
      hashTable.erase(itr);
      // Insert the modified element
      hashTable.insert(make_pair(x.getName(), x));
      return;
    }
  }

  // Modifies a specific element in a list variable in the stack
  void modifyListEntry(SYMBOL_TABLE_ENTRY x, const int idx)
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    // If ident is not found in the stack
    if ((itr = hashTable.find(x.getName())) == hashTable.end())
    {
      return;
    }
    else
    {
      // Finds the element in the symbol table with the list containing the elem to modify
      TYPE_INFO list_to_modify = findEntry(x.getName());
      // What we want the modified element to be
      TYPE_INFO elem_to_insert = x.getTypeInfo();
      int listSize = list_to_modify.list_val->size();

      if(elem_to_insert.type == INT)
      {
        list_to_modify.list_val->operator[](idx).type = INT;
        list_to_modify.list_val->operator[](idx).int_val = elem_to_insert.int_val;
      }
      else if(elem_to_insert.type == STR)
      {
        list_to_modify.list_val->operator[](idx).type = STR;
        list_to_modify.list_val->operator[](idx).str_val = elem_to_insert.str_val;
      }
      else if(elem_to_insert.type == FLOAT)
      {
        list_to_modify.list_val->operator[](idx).type = FLOAT;
        list_to_modify.list_val->operator[](idx).float_val = elem_to_insert.float_val;
      }
      else if(elem_to_insert.type == BOOL)
      {
        list_to_modify.list_val->operator[](idx).type = BOOL;
        list_to_modify.list_val->operator[](idx).bool_val = elem_to_insert.bool_val;
      }

      // Erase the old list
      hashTable.erase(itr);
      // Insert the modified list
      hashTable.insert(make_pair(x.getName(), SYMBOL_TABLE_ENTRY(x.getName(), list_to_modify)));
      return;
    }
  }

  // If a SYMBOL_TABLE_ENTRY with name theName is
  // found in this symbol table, then return true;
  // otherwise, return false.
  TYPE_INFO findEntry(string theName)
  {
    TYPE_INFO temp = {UNDEFINED, 0, 0};
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(theName)) == hashTable.end())
      return(temp);
    else return(itr->second.getTypeInfo());
  }

  int getNumParams()
  {
    return(hashTable.size());
  }

};

#endif  // SYMBOL_TABLE_H
