#ifndef TRIE_CHAR_STREAM_H
#define TRIE_CHAR_STREAM_H

// 文字列を文字ストリームとして扱うためのクラス
class CharStream {
public:
  CharStream(const char* str) : cur(str) {}
  unsigned char read() { return *cur++; }     
  unsigned char prev() const { return cur[-1]; }
  unsigned char peek() const { return *cur; } 
  const char*   rest() const { return cur; }
private:
  const char* cur;
};

#endif

