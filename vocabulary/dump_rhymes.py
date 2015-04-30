#!/usr/bin/env python
# coding: utf-8
import json
import subprocess
DEBUG = True


def get_rhymes(word):
  """Call the `rhyme` program with a given word as the argument, returning
  a list of the resulting words.
  """
  try:
    rhymes = subprocess.check_output(['rhyme', word]).strip()
    return rhymes.split(' ') if rhymes else []
  except Exception as e:
    print '***', word
  return []


def main():
  """Generate JSON file with a mapping of `word: [words that rhyme with word]`
  """
  wordlist = []
  with open('words.txt', 'rb') as fin:
    wordlist = fin.readline().split()
  num_words = len(wordlist)

  rhymes = {}
  for i, word in enumerate(wordlist):
    rhymes[word] = get_rhymes(word)
    if DEBUG:
      print '{}/{}: {}'.format(i + 1, num_words, word)

  with open('rhymes.json', 'wb') as fout:
    json.dump(rhymes, fout, indent=2)

  print 'Done.'


if __name__ == '__main__':
  main()
