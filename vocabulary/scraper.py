#!/usr/bin/env python
# coding: utf-8
from bs4 import BeautifulSoup
import json
import re
import requests

baseURL = "http://dictionary.reference.com/browse/"


def clean_pos(pos_str):
  return re.sub(r'\s*\(.*\)\s*', '', pos_str).split(' ')


def clean(inputStr):
  temp = re.sub("[\d.]|;", "", inputStr).strip()
  return [y for y in [x.strip() for x in
                      temp.replace("See", "").split(",")]
          if " " not in y]


def scrape(word):
  try:
    req = requests.get(baseURL+word)
    data = req.text
    soup = BeautifulSoup(data)
    text = soup.select('.me')[0]['data-syllable'].encode('utf8')
    synonyms, antonyms = [], []
    syllables = ''.join([i if ord(i) < 128 else ' ' for i in text]).split()

    if soup.select('.luna-data-header span'):
      pos = soup.select('.luna-data-header span')[0].string.encode('utf8')
    else:
      return None
    if soup.select('.tail-type-synonyms .tail-elements'):
      synonyms = clean(
          soup.select(
            '.tail-type-synonyms .tail-elements')[0].text.encode("utf-8"))
    if soup.select('.tail-type-antonyms .tail-elements'):
      antonyms = clean(
          soup.select(
            '.tail-type-antonyms .tail-elements')[0].text.encode("utf-8"))
    return [word, pos, len(syllables), synonyms, antonyms]
  except Exception as e:
    return None


def write_data(json_data):
  print 'Writing JSON data.'
  with open('scrape.json', 'w') as fout:
    json.dump(json_data, fout, indent=2)


def main():
  wordlist = []
  with open('words.txt', 'r') as fin:
    wordlist = fin.readline().split()

  rhymes = {}
  with open('rhymes.json', 'r') as fin:
    rhymes = json.load(fin)

  num_words = len(wordlist)
  json_data = {}
  for i, word in enumerate(wordlist):
    print 'Scraping {}/{} {}'.format(i + 1, num_words, word)
    scrape_data = scrape(word)
    if not scrape_data:
      print '... failed!'
      continue

    word, pos, num_syllables, synonyms, antonyms = scrape_data
    rhyming_words = rhymes.get(word, [])
    pos = clean_pos(pos)
    json_data[word] = {
        'pos': pos,
        'num_syllables': num_syllables,
        'synonyms': synonyms,
        'antonyms': antonyms,
        'rhymes': rhyming_words,
      }

    # Checkpoint our progress in case something bad happens (this scrape takes
    # a while).
    if i % 1000 == 0:
      write_data(json_data)

  write_data(json_data)
  print 'Done.'


if __name__ == '__main__':
  main()
