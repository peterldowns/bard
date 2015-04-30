#!/usr/bin/env python
# coding: utf-8
import json

def encode_str(s):
  def clean(word):
    if '(' in word or ')' in word or '.' in word:
      return ''
    return ''.join(c.encode('ascii', 'replace') for c in word)
  return clean(s)

def to_scm(word, data):
  return '({} ({}) {} ({}) ({}) ({}))'.format(
      word,
      ' '.join(data['pos']),
      data['num_syllables'],
      ' '.join(map(encode_str, data['synonyms'])),
      ' '.join(map(encode_str, data['antonyms'])),
      ' '.join(map(encode_str, data['rhymes'])))

if __name__ == '__main__':
  data = []
  with open('scrape.scm', 'w') as fout:
    with open('scrape.json', 'r') as fin:
      json_data = json.load(fin)
      for word, data in json_data.iteritems():
        fout.write(to_scm(word, data) + '\n')
  print 'Done.'


