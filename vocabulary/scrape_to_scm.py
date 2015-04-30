#!/usr/bin/env python
# coding: utf-8
import json
import re

bad_chars = re.compile(r'\(|\)|\'|\.|[0-9]')


def encode_str(s):
  """Clean and ascii-encode a string for uise with scheme."""
  # Any unicode characters get replaced with their ascii equivalent if
  # possible, or '?'.
  ascii_s = s.encode('ascii', 'replace')
  # There are some words that look like "foo(9)", which we ignore.
  return bad_chars.sub('', ascii_s)


def to_scm(word, data):
  return '({} ({}) {} ({}) ({}) ({}))'.format(
      word,
      ' '.join(data['pos']),
      data['num_syllables'],
      ' '.join(map(encode_str, data['synonyms'])),
      ' '.join(map(encode_str, data['antonyms'])),
      ' '.join(map(encode_str, data['rhymes'])))


def main():
  """Write a scheme-loadable version of our scraped dictionary."""
  data = []
  with open('scrape.scm', 'w') as fout:
    with open('scrape.json', 'r') as fin:
      json_data = json.load(fin)
      for word, data in json_data.iteritems():
        fout.write(to_scm(word, data) + '\n')
  print 'Done.'


if __name__ == '__main__':
  main()
