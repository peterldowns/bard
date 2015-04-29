#!/usr/bin/env python
# coding: utf-8
import re


bad_chars = re.compile(r'\\[a-zA-Z0-9\\]+')
hyphens = re.compile(r'(?<=\w)(\(|\))(?=\w)')
parens = re.compile(r'\(\ ')

def fix_line(line):
  line = bad_chars.sub('', line)
  line = hyphens.sub('-', line)
  line = parens.sub('(', line).strip()
  return line + '\n'


if __name__ == '__main__':
  with open('wordsScrapped.txt', 'r') as fin:
    with open('wordsScrapped.txt.clean', 'w') as fout:
      fout.writelines(fix_line(line) for line in fin)
  print 'Done.'
