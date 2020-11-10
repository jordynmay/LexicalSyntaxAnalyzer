#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

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

  void modifyEntry(SYMBOL_TABLE_ENTRY x)
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(x.getName())) == hashTable.end())
    {
      return;
    }
    else
    {
      hashTable.erase(itr);
      hashTable.insert(make_pair(x.getName(), x));
      return;
    }
  }

  void modifyListEntry(SYMBOL_TABLE_ENTRY x, const int idx)
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(x.getName())) == hashTable.end())
    {
      return;
    }
    else
    {
      // We have itr at the symbol table entry we want to modify
      // Itr points to a list
      // We want to modify element [idx] of that list
      // itr->second->typeInfo??
      // How to access the [idx] element?
      // Want to replace idx's symbol table entry with x
      //      which was updated in the .y file via T_IDENT N_INDEX = EXPR
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
