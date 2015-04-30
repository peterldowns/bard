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
	return [y for y in [x.strip() for x in temp.replace("See", "").split(",")] if " " not in y]

def scrape(word):
	req = requests.get(baseURL+word)
	data = req.text
	soup = BeautifulSoup(data)
	try:
		text = soup.select('.me')[0]['data-syllable'].encode('utf8')
		synonyms, antonyms = [], []
		syllables = ''.join([i if ord(i) < 128 else ' ' for i in text]).split()

		if soup.select('.luna-data-header span'):
			pos = soup.select('.luna-data-header span')[0].string.encode('utf8')
		else:
			return ""
		if soup.select('.tail-type-synonyms .tail-elements'):
			synonyms = clean(soup.select('.tail-type-synonyms .tail-elements')[0].text.encode("utf-8"))
		if soup.select('.tail-type-antonyms .tail-elements'):
			antonyms = clean(soup.select('.tail-type-antonyms .tail-elements')[0].text.encode("utf-8"))
		return [word, pos, len(syllables), synonyms, antonyms]
	except (IndexError, KeyError):
		return ""

if __name__ == '__main__':
  wordlist = []
  with open('words.txt', 'r') as fin:
    wordlist = fin.readline().split()

  rhymes = {}
  with open('rhymes.json', 'r') as fin:
    rhymes = json.load(fin)

  num_words = len(wordlist)
  json_data = {}
  str_data = []
  for i, word in enumerate(wordlist):
    print 'Scraping {}/{} {}'.format(i + 1, num_words, word)
    scrape_data = scrape(word)

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
    str_data.append([word, pos, num_syllables, synonyms, antonyms, rhyming_words])

  print 'Writing text data.'
  with open('scrape.txt', 'w') as fout:
    for data in str_data:
      fout.write(str(data) + '\n')

  print 'Writing JSON data.'
  with open('scrape.json', 'w') as fout:
    json.dump(json_data, fout, indent=2)

  print 'Done.'
