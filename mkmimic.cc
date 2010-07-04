#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <cstdlib>
#include <cstring>
#include "trie/builder.h"
#include "trie/char_stream.h"
#include "trie/char_stream_vector.h"

struct Feature {
  Feature(char type, const char* word_beg, const char* word_end, int score) 
    : word(word_beg, word_end), score(score) {
    word += "\a";
    word += type+1;
    cur = word.c_str();
  }

  std::string word;
  int         score;
  const char* cur;

  bool operator<(const Feature& f) const {
    return word < f.word;
  }

  unsigned char read() { return *cur++; }     
  unsigned char peek() const { return *cur; }   
};

typedef std::vector<Feature> Features;

int main(int argc, char** argv) {
  if(argc != 3) {
    std::cerr << "Usage: mkmimic <index> <model-file>" << std::endl;
    return 1;
  }
  
  CharStreamVector csv(argv[2]);
  if(!csv) {
    std::cerr << "Can't open file: " << argv[2] << std::endl;
    return 1;
  }

  //
  char atoi_buf[3] = {0};
  int ctype_score_matrix[5][5] = {0};
  Features features;
  
  // 
  for(unsigned i=0; i < csv.size(); i++) {
    const char* line=csv[i].rest();
    const char* tab = strchr(line+2, '\t');

    atoi_buf[0] = line[0];
    atoi_buf[1] = line[1];
    
    const char  type     = atoi(atoi_buf);
    const char* word_beg = line+2;
    const char* word_end = tab;
    const int   score    = atoi(tab+1);
    
    if(word_beg==word_end) {
      int left = type%5;
      int right= type/5;
      ctype_score_matrix[left][right] = score;
    } else {
      features.push_back(Feature(type,word_beg,word_end,score));
    }
  }
  
  //
  std::sort(features.begin(), features.end());

  //
  Builder<Features>(features).build().save(argv[1], ctype_score_matrix, sizeof(int)*5*5);
  
  return 0;
}
