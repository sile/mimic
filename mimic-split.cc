#include <iostream>
#include <vector>
#include "trie/double_array.h"
#include "trie/char_stream_vector.h"

const unsigned utf8_len_table[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1
};

bool is_hiragana(const char* s) {
  unsigned char c1 = s[0];
  unsigned char c2 = s[1];
  unsigned char c3 = s[2];
  
  if (c1 == 0xE3) {
    if (c2 == 0x81) {
      if (c3 >= 0x81)
	return true;
    } else if (c2 == 0x82) {
      if (c3 <= 0x9F)
	return true;
    }
  }
  return false;
}

bool is_katakana(const char* s) {
  unsigned char c1 = s[0];
  unsigned char c2 = s[1];
  unsigned char c3 = s[2];
  
  if (c1 == 0xE3) {
    if (c2 == 0x82) {
      if (c3 >= 0xA1)
	return true;
    } else if (c2 >= 0x83 && c2 <= 0x86) {
      return true;
    } else if (c2 == 0x87) {
      if (c3 <= 0xBF)
	return true;
    }
  }
  return false;
}

char char_type(const char*& s) {
  if(s[0] & 0x80) {
    if(utf8_len_table[static_cast<unsigned char>(s[0])]!=3) {
      s += utf8_len_table[s[0]];
      return 4; // default;
    }
    
    s += 3;
    if(is_hiragana(s-3))
      return 1; // hiragana
    if(is_katakana(s-3))
      return 2; // katakana
    return 4;
  } else {
    char c=*(s++);
    if('0' <= c && c <= '9') 
      return 0; // numeric
    
    switch(c) {
    case ',': case '.': case '!': case '?':
      return 3; // symbol
    default:
      return 4; // default
    }
  }
}

// for utf8
const char* next(const char* s) {
  for(unsigned char c=*(++s); c != '\0' && c & 0x80 && !(c & 0x40); c=*(++s));
  return s;
}

// for utf8
const char* prev(const char* s) {
  for(unsigned char c=*(--s); c & 0x80 && !(c & 0x40); c=*(--s));
  return s;
}

struct Callback {
  Callback(std::vector<int>& cut_score)
    : cut_score(cut_score) {}

  void operator()(const char* key, unsigned start, unsigned end, int score, unsigned type) const {
    //std::cout << type << "#" << std::string(key+start, key+end) << " => " << score << std::endl;
    unsigned pos=0;
    
    switch(type) {
    case 0:
      pos = next(key+end)-key;
      break;
    case 1:  
      pos = end;
      break; 
    case 2:
      pos = start;
      break;
    case 3:
      pos = prev(key+start)-key;
      break;
    default:
      pos = prev(key+end)-key;
    }

    /*
    if(pos > 0 && pos < cut_score.size())
      std::cout <<type<<"#"<<start<<'~'<<end<< " => " << pos <<'/'<< cut_score.size() << " = " << score << std::endl;
    */

    if(pos < cut_score.size())
      cut_score[pos] += score;
  }
  
  std::vector<int>& cut_score;
};

int main(int argc, char** argv) {
  if(argc != 3) {
    std::cerr << "Usage: mimic-split <index> <text-file>" << std::endl;
    return 1;
  }

  DoubleArray da(argv[1]);
  CharStreamVector csv(argv[2]);
  
  for(unsigned i=0; i < csv.size(); i++) {
    const char* line  = csv[i].rest();
    unsigned line_len = strlen(line);
    
    std::vector<int> cut_score(line_len);
    Callback fn(cut_score); 
    
    //
    if(line_len != 0) {
      const char* line2 = line;
      char prev_type = char_type(line2);
      while(line < line2 && line2 < line+line_len) {
	//	std::cerr << (int)line2 << std::endl;
	unsigned pos = line2-line;
	char type = char_type(line2);
	cut_score[pos] = da.char_type_link_score(prev_type, type);
	prev_type = type;
      }
    }

    /*
    for(unsigned pos=1; pos < line_len; pos++) {
      std::cout << " " << pos << "#" << cut_score[pos] << std::endl;
    }
    */

    //
    for(unsigned pos=0; pos < line_len; pos++)
      da.each_common_prefix(line, pos, fn);

    unsigned prev=0;
    for(unsigned pos=1; pos < line_len; pos++) {
      if(cut_score[pos] > 0) {
	std::cout << std::string(line+prev, line+pos) << " ";
	prev=pos;
      }
      //std::cout << " " << pos << "#" << cut_score[pos] << std::endl;
    }
    std::cout << std::string(line+prev) << std::endl;
  }

  return 0;
}
