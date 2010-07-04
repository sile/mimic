#ifndef TRIE_CHAR_STREAM_VECTOR_H
#define TRIE_CHAR_STREAM_VECTOR_H

#include <vector>
#include <cstdio>
#include <cstring>
#include "char_stream.h"

class CharStreamVector {
public:
  CharStreamVector(const char* filepath) 
    : buf(NULL), valid(false) {
    FILE* f;
    if((f=fopen(filepath,"rb"))==NULL)
      return;

    fseek(f,0,SEEK_END);
    long file_size = ftell(f);
    fseek(f,0,SEEK_SET);
    
    if(file_size != -1) {
      buf = new char[file_size+1];
      fread(buf, sizeof(char), file_size, f);
      buf[file_size]='\0';
      
      if(buf[file_size-1]=='\n')
        buf[file_size-1]='\0';

      init(buf);
      
      valid=true;
    }
    fclose(f);
  }
  ~CharStreamVector() {
    delete [] buf;
  }

  CharStream& operator[] (unsigned index) { return words[index]; }
  unsigned size() const { return words.size(); }
  operator bool() const { return valid; }

private:
  void init(char* cur) {
    words.push_back(CharStream(cur));  
    for(cur=strchr(cur,'\n'); cur; cur=strchr(cur,'\n')) {
      *cur = '\0';                     
      cur++;
      words.push_back(CharStream(cur));
    }
  }

private:
  std::vector<CharStream> words;
  char* buf;
  bool valid;
};

#endif
