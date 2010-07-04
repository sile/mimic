#ifndef TRIE_DOUBLE_ARRAY_H
#define TRIE_DOUBLE_ARRAY_H

#include <cstdio>
#include "char_stream.h"
#include <string>

class DoubleArray {
  struct Node {
    int base;
    int chck;
  };
  
public:
  DoubleArray(const char* filepath) {
    FILE* f = fopen(filepath, "rb");
    
    unsigned node_size;
    fread(&node_size, sizeof(unsigned), 1, f);
    nodes = new Node[node_size];
    
    fread(nodes, sizeof(Node), node_size, f);
    
    fread(ctype_score_matrix, sizeof(int), 5*5, f);
    fclose(f);
  }
  ~DoubleArray() {
    delete [] nodes;
  }

  template<class Callback>
  void each_common_prefix(const char* key, unsigned start, Callback& fn) const {
    int node_index=0;
    CharStream in(key+start);
    for(unsigned offset=start;; offset++) {
      int terminal_index = nodes[node_index].base + '\a';
      if(nodes[terminal_index].chck == node_index) {
	for(unsigned type=0; type <= 5; type++) {
	  int last_index = nodes[terminal_index].base + type+1;
	  if(nodes[last_index].chck == terminal_index)
	    fn(key, start, offset, nodes[last_index].base, type);
	}
      }
      
      if(in.peek()=='\0')
	break;
      
      int next_index = nodes[node_index].base + in.read();
      if(nodes[next_index].chck == node_index)
        node_index = next_index;
      else
	break;
    }    
  }

  int char_type_link_score(char left_type, char right_type) const {
    return ctype_score_matrix[left_type][right_type];
  }
private:
  Node* nodes;
  int ctype_score_matrix[5][5];
};

#endif
