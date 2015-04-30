#!/usr/bin/env python
# coding: utf-8
import subprocess
import json

def get_word(line):
  word, _ = line.split(' ', 1)
  return word[1:]

def get_rhymes(word):
  try:
    rhymes = subprocess.check_output(['rhyme', word]).strip()
    return rhymes.split(' ') if rhymes else []
  except Exception as e:
    print '***', word
  return []

if __name__ == '__main__':
  rhymes = {}
  with open('wordsScraped.txt', 'rb') as fin:
    for line in fin:
      word = get_word(line)
      rhymes[word] = get_rhymes(word)
      print word
  with open('output.json', 'wb') as fout:
    json.dump(rhymes, fout, indent=2)
  print 'Done.'

