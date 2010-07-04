#ifndef TRIE_BUILDER_H
#define TRIE_BUILDER_H

#include <cstdio>
#include "node_allocator.h"
#include "char_stream.h"
#include "char_stream_vector.h"

template <class StreamVector>
class Builder {
  struct Node {
    int base;
    int chck;
  };

public:
  Builder(StreamVector& sv)
    : sv(sv), node_size(sv.size()*15) {
    nodes = new Node[node_size];
    for(int i=0; i < node_size; i++)
      nodes[i].chck = -1; 
  }

  ~Builder() {
    delete [] nodes;
  }
  
  Builder<StreamVector>& build() {
    build_impl(0, sv.size(), 0);
    return *this;
  }

  void save(const char* filepath, const void* optional_data, unsigned optional_data_size) {
    FILE* f = fopen(filepath, "wb");
    
    if(node_size > 0xFF)
      while(nodes[node_size-0xFF].chck==-1)
        node_size--;
    
    fwrite(&node_size, sizeof(unsigned), 1, f);
    fwrite(nodes, sizeof(Node), node_size, f);

    fwrite(optional_data, sizeof(char), optional_data_size, f);
    fclose(f);
  }

private:
  void build_impl(std::size_t beg, std::size_t end, int root_node) {
    if(end-beg == 1) {
      // 末端: "\a#{score}"
      for(; sv[beg].peek() != '\0'; sv[beg].read())
        root_node = set_node(root_node, alloc.allocate(sv[beg].peek()), sv[beg].peek());
      nodes[root_node].base = sv[beg].score;  // 末端のノードにはスコアをセット
      return;
    }

    std::vector<unsigned char> children;
    std::vector<std::size_t>   ranges;
    do {
      children.push_back(sv[beg].peek());
      ranges.push_back(beg);
      beg = end_of_same_node(sv, beg, end);
    } while (beg != end);
    ranges.push_back(end);

    int base_node = alloc.allocate(children);
    for(std::size_t i=0; i < children.size(); i++)
      build_impl(ranges[i], ranges[i+1], set_node(root_node, base_node, children[i]));
  }

  int set_node(int node, int base_node, unsigned char child) {
    int next   = base_node + child;
    nodes[node].base = base_node;
    nodes[next].chck = node;
    return next;
  }

  unsigned end_of_same_node(StreamVector& sv, std::size_t beg, std::size_t end) {
    unsigned char ch = sv[beg].read();
    std::size_t cur  = beg+1;
    for(; cur < end && ch == sv[cur].peek(); cur++)
      sv[cur].read();
    return cur;
  }

private:
  StreamVector& sv;
  int node_size;
  Node* nodes;
  NodeAllocator alloc;
};

#endif
