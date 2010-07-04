require 'MeCab'

mecab = MeCab::Tagger.new("-Owakati")

open(ARGV[0]).each do |line|
  puts mecab.parse(line.strip).gsub(/ /,"\n")+"\n"
end
