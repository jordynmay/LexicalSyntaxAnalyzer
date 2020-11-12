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
      //cerr << "in else of modify" << endl;
      //TYPE_INFO elem_to_insert = itr->second.getTypeInfo();
      TYPE_INFO list_to_modify = findEntry(x.getName());
      TYPE_INFO elem_to_insert = x.getTypeInfo();
      //LIST_ENTRY elem_to_modify = list_to_modify.list_val[idx-1];
      //vector<LIST_ENTRY>* temp_list = new vector<LIST_ENTRY>();
      //temp_list = list_to_modify.list_val;
      //vector<LIST_ENTRY>& temp_list = *list_to_modify.list_val;
      //vector<LIST_ENTRY> temp_list;
      //temp_list.clear();

      //
      //cerr << "***temp elem type: " << temp_list[idx].type << endl;
      //cerr << "***temp elem before modify: " << temp_list[idx].int_val << endl;

      //cerr << "idx is: " << idx << endl;
      //cerr << "list name: " << x.getName() << endl;
      int listSize = list_to_modify.list_val->size();

      //cerr << "list size: " << listSize << endl;
      //cerr << "list type: " << list_to_modify.type << endl;
      //cerr << "elem type: " << list_to_modify.list_val->operator[](idx).type << endl;
      //cerr << "elem before modify: " << list_to_modify.list_val->operator[](idx).int_val << endl;
      //cerr << "elem after modify: " << list_to_modify.list_val->operator[](idx).int_val << endl;
      
       //cerr << "\tvals for list: " << endl;

      //cerr << "index is: " << idx << endl << endl;

      if(elem_to_insert.type == INT)
      {
        //cerr << "in int assign" <<endl;
        list_to_modify.list_val->operator[](idx).type = INT;
        list_to_modify.list_val->operator[](idx).int_val = elem_to_insert.int_val;
      }
      else if(elem_to_insert.type == STR)
      {
        list_to_modify.list_val->operator[](idx).type = STR;
        //cerr << "in str assign" << endl;
        //cerr << "val was: " << list_to_modify.list_val->operator[](idx).bool_val << endl;
        list_to_modify.list_val->operator[](idx).str_val = elem_to_insert.str_val;
        //strcpy(list_to_modify.list_val->operator[](idx).str_val, elem_to_insert.str_val);
        //temp_list[idx].str_val = elem_to_insert.str_val;
        //cerr << "val is: " << list_to_modify.list_val->operator[](idx).str_val << endl;
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



      
      /*for(int i=0; i < listSize; i++)
      {
        //cerr << "for i: " << i << endl;
        temp_list.push_back(list_to_modify.list_val->operator[](i));
      }
      //cerr << "elem before modify in temp: " << temp_list[idx].int_val << endl;
      //cerr << "type to insert: " << elem_to_insert.type << endl;
      if(elem_to_insert.type == INT)
      {
        //cerr << "in int assign" <<endl;
        temp_list[idx].type = INT;
        temp_list[idx].int_val = elem_to_insert.int_val;
      }
      else if(elem_to_insert.type == STR)
      {
        temp_list[idx].type = STR;
        strcpy(temp_list[idx].str_val, elem_to_insert.str_val);
        //temp_list[idx].str_val = elem_to_insert.str_val;
      }
      else if(elem_to_insert.type == FLOAT)
      {
        temp_list[idx].type = FLOAT;
        temp_list[idx].float_val = elem_to_insert.float_val;
      }
      else if(elem_to_insert.type == BOOL)
      {
        temp_list[idx].type = BOOL;
        temp_list[idx].bool_val = elem_to_insert.bool_val;
      }
      //cerr << "elem after modify: " << temp_list[idx].int_val << endl;
      //cerr << "after" <<endl;

      //int sizeOfList = list_to_modify.list_val->size();
      //cerr << "after size assign" <<endl;
      //cerr << "size of list: " << sizeOfList << endl;
      list_to_modify.list_val->clear();
      for(int i=0; i < listSize; i++)
      {
        list_to_modify.list_val->push_back(temp_list[i]);
      }*/




      //cerr << "after for" << endl;
      //TYPE_INFO temp_typeInfo = {LIST, UNDEFINED, UNDEFINED, false};
      //temp_typeInfo.list_val = (temp_list);


      //vector<LIST_ENTRY>& temp_list = *temp.list_val;
      
      //temp2[idx] = 

      //insert_this = itr->second.typeInfo.list_val;
      // We have itr at the symbol table entry we want to modify
      // Itr points to a list
      // We want to modify element [idx] of that list
      // itr->second->typeInfo??
      // How to access the [idx] element?
      // Want to replace idx's symbol table entry with x
      //      which was updated in the .y file via T_IDENT N_INDEX = EXPR
      hashTable.erase(itr);
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
